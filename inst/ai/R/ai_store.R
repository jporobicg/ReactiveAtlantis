## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis: local SQLite + FTS5 store    ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ra_ai_store_open <- function(index_dir = ra_ai_config()$index_dir, create = TRUE) {
  if (!ra_ai_need_pkg("DBI") || !ra_ai_need_pkg("RSQLite")) {
    stop("RSQLite and DBI are required to build or read the Ask Atlantis index.",
         call. = FALSE)
  }
  if (isTRUE(create) && !dir.exists(index_dir)) {
    dir.create(index_dir, recursive = TRUE, showWarnings = FALSE)
  }
  dbfile <- ra_ai_db_file(index_dir)
  if (!isTRUE(create) && !file.exists(dbfile)) {
    stop("No Ask Atlantis index at ", dbfile,
         ". Run index_ask_atlantis() first.", call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  ra_ai_store_init(con)
  con
}

ra_ai_store_init <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS meta (
      key TEXT PRIMARY KEY,
      value TEXT
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS sources (
      source_id TEXT PRIMARY KEY,
      source_type TEXT NOT NULL,
      path TEXT NOT NULL,
      title TEXT,
      version TEXT,
      file_hash TEXT,
      mtime REAL,
      indexed_at TEXT,
      status TEXT,
      error TEXT,
      n_chunks INTEGER DEFAULT 0
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS chunks (
      chunk_id TEXT PRIMARY KEY,
      source_id TEXT NOT NULL,
      source_type TEXT NOT NULL,
      path TEXT,
      title TEXT,
      section TEXT,
      loc_start INTEGER,
      loc_end INTEGER,
      loc_kind TEXT,
      version TEXT,
      text TEXT NOT NULL,
      FOREIGN KEY (source_id) REFERENCES sources(source_id)
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS embeddings (
      chunk_id TEXT PRIMARY KEY,
      dim INTEGER,
      vector BLOB,
      FOREIGN KEY (chunk_id) REFERENCES chunks(chunk_id)
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ingest_log (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      ts TEXT,
      source_id TEXT,
      level TEXT,
      message TEXT
    )
  ")
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS ignored_paths (
      path TEXT PRIMARY KEY,
      reason TEXT,
      ts TEXT
    )
  ")
  fts_exists <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM sqlite_master WHERE type = 'table' AND name = 'chunks_fts'"
  )$n
  if (fts_exists < 1) {
    tryCatch(
      DBI::dbExecute(con, "
        CREATE VIRTUAL TABLE chunks_fts USING fts5(
          chunk_id UNINDEXED,
          text,
          section,
          source_type,
          tokenize = 'unicode61'
        )
      "),
      error = function(e) {
        stop("SQLite FTS5 is required for Ask Atlantis lexical search: ",
             e$message, call. = FALSE)
      }
    )
  }
  invisible(con)
}

ra_ai_store_log <- function(con, message, source_id = NA_character_,
                            level = "info") {
  DBI::dbExecute(
    con,
    "INSERT INTO ingest_log (ts, source_id, level, message) VALUES (?, ?, ?, ?)",
    params = list(
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      source_id, level, as.character(message)[1]
    )
  )
  invisible(NULL)
}

ra_ai_store_meta_set <- function(con, key, value) {
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)",
    params = list(as.character(key)[1], as.character(value)[1])
  )
}

ra_ai_store_meta_get <- function(con, key, default = NA_character_) {
  row <- DBI::dbGetQuery(
    con, "SELECT value FROM meta WHERE key = ?",
    params = list(as.character(key)[1])
  )
  if (nrow(row) == 0) return(default)
  row$value[1]
}

ra_ai_delete_source_chunks <- function(con, source_id) {
  ids <- DBI::dbGetQuery(
    con, "SELECT chunk_id FROM chunks WHERE source_id = ?",
    params = list(source_id)
  )$chunk_id
  if (length(ids) > 0) {
    for (id in ids) {
      DBI::dbExecute(con, "DELETE FROM embeddings WHERE chunk_id = ?",
                     params = list(id))
      DBI::dbExecute(con, "DELETE FROM chunks_fts WHERE chunk_id = ?",
                     params = list(id))
    }
  }
  DBI::dbExecute(con, "DELETE FROM chunks WHERE source_id = ?",
                 params = list(source_id))
  invisible(ids)
}

ra_ai_upsert_source <- function(con, rec) {
  DBI::dbExecute(
    con,
    paste(
      "INSERT OR REPLACE INTO sources",
      "(source_id, source_type, path, title, version, file_hash, mtime,",
      " indexed_at, status, error, n_chunks)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      rec$source_id, rec$source_type, rec$path, rec$title, rec$version,
      rec$file_hash, rec$mtime, rec$indexed_at, rec$status,
      rec$error %||% NA_character_, rec$n_chunks %||% 0L
    )
  )
}

ra_ai_insert_chunk <- function(con, rec) {
  DBI::dbExecute(
    con,
    paste(
      "INSERT OR REPLACE INTO chunks",
      "(chunk_id, source_id, source_type, path, title, section, loc_start,",
      " loc_end, loc_kind, version, text)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      rec$chunk_id, rec$source_id, rec$source_type, rec$path, rec$title,
      rec$section, rec$loc_start, rec$loc_end, rec$loc_kind, rec$version,
      rec$text
    )
  )
  DBI::dbExecute(
    con,
    "INSERT INTO chunks_fts (chunk_id, text, section, source_type) VALUES (?, ?, ?, ?)",
    params = list(rec$chunk_id, rec$text, rec$section %||% "", rec$source_type)
  )
}

ra_ai_pack_vector <- function(x) {
  writeBin(as.numeric(x), raw(), size = 8L, endian = "little")
}

ra_ai_unpack_vector <- function(blob) {
  if (is.null(blob) || length(blob) == 0) return(numeric())
  readBin(blob, what = "numeric", n = length(blob) / 8L, size = 8L,
          endian = "little")
}

ra_ai_insert_embedding <- function(con, chunk_id, vec) {
  vec <- as.numeric(vec)
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO embeddings (chunk_id, dim, vector) VALUES (?, ?, ?)",
    params = list(chunk_id, length(vec), list(ra_ai_pack_vector(vec)))
  )
}

ra_ai_file_hash <- function(path) {
  unname(tools::md5sum(path))
}

ra_ai_chunk_id <- function(source_id, section, loc_start, text) {
  raw <- paste(source_id, section, loc_start, nchar(text),
               substring(text, 1, 80), sep = "|")
  paste0("chk_", substr(digest_fallback(raw), 1, 16))
}

digest_fallback <- function(x) {
  tmp <- tempfile()
  writeLines(x, tmp, useBytes = TRUE)
  h <- unname(tools::md5sum(tmp))
  unlink(tmp)
  h
}

ra_ai_list_sources <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM sources ORDER BY source_type, source_id")
}

ra_ai_list_chunks <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT chunk_id, source_id, source_type, path, title, section, loc_start, loc_end, loc_kind, version FROM chunks"
  )
}

ra_ai_get_chunks_by_id <- function(con, ids) {
  ids <- unique(as.character(ids))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) {
    return(data.frame())
  }
  qs <- paste(rep("?", length(ids)), collapse = ",")
  DBI::dbGetQuery(
    con,
    paste("SELECT * FROM chunks WHERE chunk_id IN (", qs, ")"),
    params = as.list(ids)
  )
}

ra_ai_source_exists <- function(con, source_id) {
  n <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM sources WHERE source_id = ?",
    params = list(source_id)
  )$n
  n > 0
}
