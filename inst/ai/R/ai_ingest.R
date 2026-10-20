## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis: corpus ingest and chunking   ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ra_ai_read_json <- function(path) {
  if (!ra_ai_need_pkg("jsonlite")) {
    stop("jsonlite is required to read the sources manifest.", call. = FALSE)
  }
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

##' Load and expand the sources manifest.
ra_ai_load_manifest <- function(path = ra_ai_config()$sources_config,
                                paths = ra_ai_paths()) {
  if (!file.exists(path)) {
    stop("Sources manifest not found: ", path, call. = FALSE)
  }
  man <- ra_ai_read_json(path)
  if (is.null(man$sources) || length(man$sources) == 0) {
    stop("Sources manifest has no 'sources' entries: ", path, call. = FALSE)
  }
  man$exclude_globs <- as.character(man$exclude_globs %||% character())
  man$max_file_bytes <- suppressWarnings(as.numeric(man$max_file_bytes %||% 5e5))
  if (!is.finite(man$max_file_bytes)) man$max_file_bytes <- 5e5
  for (i in seq_along(man$sources)) {
    src <- man$sources[[i]]
    src$path <- ra_ai_expand_path(src$path, paths)
    src$id <- as.character(src$id %||% paste0("src", i))[1]
    src$type <- as.character(src$type %||% "manual")[1]
    src$version <- as.character(src$version %||% NA_character_)[1]
    src$include <- as.character(src$include %||% c("**/*.md", "**/*.txt",
                                                   "**/*.R", "**/*.c", "**/*.h"))
    man$sources[[i]] <- src
  }
  man
}

ra_ai_glob_to_rx <- function(glob) {
  glob <- as.character(glob)[1]
  glob <- gsub("\\", "/", glob, fixed = TRUE)
  glob <- gsub(".", "\\.", glob, fixed = TRUE)
  glob <- gsub("**", "\001", glob, fixed = TRUE)
  glob <- gsub("*", "[^/]*", glob, fixed = TRUE)
  glob <- gsub("\001", ".*", glob, fixed = TRUE)
  glob <- gsub("?", ".", glob, fixed = TRUE)
  paste0("^", glob, "$")
}

ra_ai_path_matches <- function(path, globs) {
  if (length(globs) == 0) return(FALSE)
  p <- gsub("\\", "/", path, fixed = TRUE)
  bn <- basename(p)
  for (g in globs) {
    rx <- ra_ai_glob_to_rx(g)
    if (grepl(rx, p) || grepl(rx, bn)) return(TRUE)
    ## Allow matching a suffix of the path against **/ patterns
    if (startsWith(g, "**/") && grepl(ra_ai_glob_to_rx(sub("^\\*\\*/", "", g)), bn)) {
      return(TRUE)
    }
  }
  FALSE
}

ra_ai_list_source_files <- function(src, exclude_globs, max_file_bytes,
                                    ignored = NULL) {
  path <- src$path
  include <- src$include
  files <- character()
  if (is.na(path) || !nzchar(path)) {
    return(list(files = character(), ignored = ignored))
  }
  if (file.exists(path) && !dir.exists(path)) {
    files <- path
  } else if (dir.exists(path)) {
    all_files <- list.files(path, recursive = TRUE, full.names = TRUE,
                            include.dirs = FALSE)
    keep <- vapply(all_files, function(f) {
      rel <- substring(f, nchar(path) + 2L)
      rel <- gsub("\\", "/", rel, fixed = TRUE)
      any(vapply(include, function(g) {
        ra_ai_path_matches(rel, g) || ra_ai_path_matches(basename(rel), g)
      }, logical(1)))
    }, logical(1))
    files <- all_files[keep]
  }
  kept <- character()
  for (f in files) {
    reason <- NULL
    if (ra_ai_path_matches(f, exclude_globs) ||
        ra_ai_path_matches(basename(f), exclude_globs)) {
      reason <- "excluded by glob"
    } else {
      sz <- suppressWarnings(file.size(f))
      if (is.finite(sz) && sz > max_file_bytes) {
        reason <- paste("file larger than", max_file_bytes, "bytes")
      }
    }
    if (is.null(reason)) {
      kept <- c(kept, f)
    } else if (!is.null(ignored)) {
      ignored[[length(ignored) + 1]] <- list(path = f, reason = reason)
    }
  }
  list(files = kept, ignored = ignored)
}

##' Split text into heading-aware chunks.
ra_ai_chunk_text <- function(lines, path, source_id, source_type, version,
                             title = basename(path), loc_kind = "line") {
  lines <- as.character(lines)
  if (length(lines) == 0) return(list())
  heading_rx <- "^[[:space:]]{0,3}(#{1,6}[[:space:]].+|\\d+\\.[[:space:]].+|Case:[[:space:]].+)"
  starts <- c(1L, which(grepl(heading_rx, lines)))
  starts <- unique(starts)
  ends <- c(starts[-1] - 1L, length(lines))
  chunks <- list()
  for (i in seq_along(starts)) {
    block <- lines[starts[i]:ends[i]]
    section <- trimws(sub("^[# ]+", "", block[1]))
    if (!nzchar(section)) section <- title
    text <- paste(block, collapse = "\n")
    text <- trimws(text)
    if (nchar(text) < 20) next
    ## Bound very long sections
    parts <- ra_ai_split_long(text, max_chars = 1800)
    for (j in seq_along(parts)) {
      loc_s <- starts[i]
      loc_e <- ends[i]
      sec <- if (length(parts) == 1) section else paste0(section, " (", j, ")")
      chunks[[length(chunks) + 1]] <- list(
        chunk_id = ra_ai_chunk_id(source_id, sec, loc_s + j, parts[[j]]),
        source_id = source_id,
        source_type = source_type,
        path = path,
        title = title,
        section = sec,
        loc_start = loc_s,
        loc_end = loc_e,
        loc_kind = loc_kind,
        version = version,
        text = parts[[j]]
      )
    }
  }
  chunks
}

ra_ai_split_long <- function(text, max_chars = 1800) {
  if (nchar(text) <= max_chars) return(list(text))
  paras <- unlist(strsplit(text, "\n\n+", perl = TRUE))
  out <- list()
  buf <- ""
  for (p in paras) {
    if (nchar(buf) + nchar(p) + 2 > max_chars && nzchar(buf)) {
      out[[length(out) + 1]] <- buf
      buf <- p
    } else {
      buf <- if (nzchar(buf)) paste(buf, p, sep = "\n\n") else p
    }
  }
  if (nzchar(buf)) out[[length(out) + 1]] <- buf
  out
}

##' Split code by function-like boundaries, keeping file location.
ra_ai_chunk_code <- function(lines, path, source_id, source_type, version,
                             title = basename(path)) {
  lines <- as.character(lines)
  if (length(lines) == 0) return(list())
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("r")) {
    starts <- which(grepl("^[A-Za-z.][A-Za-z0-9._]*[[:space:]]*(<-|=)[[:space:]]*function\\s*\\(", lines))
    ## Also keep roxygen blocks attached to the following function
    roxy <- which(grepl("^##'|^#'", lines))
  } else {
    starts <- which(grepl(
      "^[A-Za-z_][A-Za-z0-9_[:space:]\\*]*\\([^;]*\\)[[:space:]]*\\{?[[:space:]]*$",
      lines
    ))
    roxy <- integer()
  }
  if (length(starts) == 0) {
    return(ra_ai_chunk_text(lines, path, source_id, source_type, version,
                            title = title, loc_kind = "line"))
  }
  ## Pull a short header comment into the first chunk
  file_head <- if (starts[1] > 1) paste(lines[1:max(1, min(starts[1] - 1, 40))],
                                        collapse = "\n") else ""
  starts <- unique(c(1L, starts))
  ends <- c(starts[-1] - 1L, length(lines))
  chunks <- list()
  for (i in seq_along(starts)) {
    loc_s <- starts[i]
    loc_e <- ends[i]
    block <- lines[loc_s:loc_e]
    if (length(block) > 180) block <- block[1:180]
    text <- trimws(paste(block, collapse = "\n"))
    if (nchar(text) < 20) next
    first <- trimws(block[1])
    section <- substring(first, 1, 80)
    chunks[[length(chunks) + 1]] <- list(
      chunk_id = ra_ai_chunk_id(source_id, section, loc_s, text),
      source_id = source_id,
      source_type = source_type,
      path = path,
      title = title,
      section = section,
      loc_start = loc_s,
      loc_end = loc_e,
      loc_kind = "line",
      version = version,
      text = text
    )
  }
  if (nzchar(trimws(file_head)) && nchar(file_head) >= 20 &&
      !any(vapply(chunks, function(c) identical(c$loc_start, 1L), logical(1)))) {
    chunks <- c(list(list(
      chunk_id = ra_ai_chunk_id(source_id, paste("header", title), 1L, file_head),
      source_id = source_id,
      source_type = source_type,
      path = path,
      title = title,
      section = paste("File header:", title),
      loc_start = 1L,
      loc_end = max(1L, starts[1] - 1L),
      loc_kind = "line",
      version = version,
      text = file_head
    )), chunks)
  }
  chunks
}

ra_ai_read_pdf_pages <- function(path) {
  if (!ra_ai_need_pkg("pdftools")) {
    return(list(
      pages = character(),
      error = "pdftools is not installed; PDF was not extracted"
    ))
  }
  pages <- tryCatch(pdftools::pdf_text(path), error = function(e) e)
  if (inherits(pages, "error")) {
    return(list(pages = character(), error = pages$message))
  }
  list(pages = as.character(pages), error = NULL)
}

ra_ai_chunk_pdf <- function(path, source_id, source_type, version,
                            title = basename(path)) {
  extracted <- ra_ai_read_pdf_pages(path)
  if (!is.null(extracted$error)) {
    return(list(chunks = list(), error = extracted$error,
                unreadable = path))
  }
  chunks <- list()
  unreadable <- integer()
  for (i in seq_along(extracted$pages)) {
    txt <- extracted$pages[[i]]
    ## Page-number-only or scanned pages with no extractable text.
    if (nchar(trimws(txt)) < 20) {
      unreadable <- c(unreadable, i)
      next
    }
    lines <- unlist(strsplit(txt, "\n", fixed = TRUE))
    page_chunks <- ra_ai_chunk_text(
      lines, path, source_id, source_type, version,
      title = paste0(title, " p.", i), loc_kind = "page"
    )
    for (j in seq_along(page_chunks)) {
      page_chunks[[j]]$loc_start <- i
      page_chunks[[j]]$loc_end <- i
      page_chunks[[j]]$loc_kind <- "page"
    }
    chunks <- c(chunks, page_chunks)
  }
  err <- NULL
  if (length(unreadable) > 0) {
    err <- paste("Unreadable or empty PDF pages:", paste(unreadable, collapse = ", "))
  }
  list(chunks = chunks, error = err, unreadable = unreadable)
}

ra_ai_ingest_file <- function(path, source_id, source_type, version) {
  ext <- tolower(tools::file_ext(path))
  title <- basename(path)
  if (ext == "pdf") {
    return(ra_ai_chunk_pdf(path, source_id, source_type, version, title))
  }
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) NULL)
  if (is.null(lines)) {
    return(list(chunks = list(), error = paste("Could not read", path)))
  }
  ## Drop embedded NULs from mixed encodings without putting a NUL in this file.
  lines <- vapply(lines, function(line) {
    raw <- charToRaw(enc2utf8(as.character(line)[1]))
    rawToChar(raw[raw != as.raw(0L)])
  }, character(1), USE.NAMES = FALSE)
  if (ext %in% c("r", "c", "h", "cpp", "hpp", "cc")) {
    chunks <- ra_ai_chunk_code(lines, path, source_id, source_type, version, title)
  } else {
    chunks <- ra_ai_chunk_text(lines, path, source_id, source_type, version, title)
  }
  list(chunks = chunks, error = NULL)
}

##' Always inject one chunk per existing Shiny tab from the nav catalog.
ra_ai_nav_catalog_chunks <- function() {
  catalog <- ra_ai_nav_catalog()
  lapply(names(catalog), function(id) {
    entry <- catalog[[id]]
    text <- paste(
      "ReactiveAtlantis tool:", entry$title,
      "\nNavigation target id:", entry$id,
      "\nOpen the", entry$title, "tab in the top navigation.",
      "\n", entry$summary
    )
    list(
      chunk_id = paste0("nav_", id),
      source_id = "ra-nav-catalog",
      source_type = "reactive_code",
      path = "inst/shiny/app.R",
      title = "ReactiveAtlantis navigation catalog",
      section = entry$title,
      loc_start = NA_integer_,
      loc_end = NA_integer_,
      loc_kind = "nav",
      version = "package",
      text = text
    )
  })
}

ra_ai_source_unchanged <- function(con, source_id, file_hash, mtime) {
  row <- DBI::dbGetQuery(
    con,
    "SELECT file_hash, mtime, status FROM sources WHERE source_id = ?",
    params = list(source_id)
  )
  if (nrow(row) == 0) return(FALSE)
  if (!identical(row$status[1], "ok")) return(FALSE)
  identical(row$file_hash[1], file_hash) &&
    isTRUE(abs(as.numeric(row$mtime[1]) - as.numeric(mtime)) < 1e-6)
}

##' Build or incrementally update the local index.
##'
##' @param sources_config Path to the JSON manifest
##' @param index_dir Directory for the SQLite file
##' @param embed Whether to request Ollama embeddings
##' @param config Runtime config
##' @return Invisible list of ingest diagnostics
ra_ai_index <- function(sources_config = NULL, index_dir = NULL,
                        embed = TRUE, config = ra_ai_config()) {
  if (is.null(sources_config)) sources_config <- config$sources_config
  if (is.null(index_dir)) index_dir <- config$index_dir
  man <- ra_ai_load_manifest(sources_config)
  con <- ra_ai_store_open(index_dir, create = TRUE)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  ra_ai_store_meta_set(con, "manifest", sources_config)
  ra_ai_store_meta_set(con, "indexed_at",
                       format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))

  ignored <- list()
  failures <- list()
  n_new <- 0L
  n_skip <- 0L
  all_new_ids <- character()

  DBI::dbExecute(con, "DELETE FROM ignored_paths")

  ## Nav catalog is always rebuilt from the running code, not from files.
  ra_ai_delete_source_chunks(con, "ra-nav-catalog")
  ra_ai_upsert_source(con, list(
    source_id = "ra-nav-catalog",
    source_type = "reactive_code",
    path = "inst/shiny/app.R",
    title = "ReactiveAtlantis navigation catalog",
    version = "package",
    file_hash = "nav-catalog",
    mtime = as.numeric(Sys.time()),
    indexed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = "ok",
    error = NA_character_,
    n_chunks = length(ra_ai_nav_catalog())
  ))
  for (ch in ra_ai_nav_catalog_chunks()) {
    ra_ai_insert_chunk(con, ch)
    all_new_ids <- c(all_new_ids, ch$chunk_id)
  }

  for (src in man$sources) {
    listed <- ra_ai_list_source_files(src, man$exclude_globs, man$max_file_bytes,
                                      ignored)
    ignored <- listed$ignored
    files <- listed$files
    if (length(files) == 0) {
      msg <- paste("No files found for source", src$id, "at", src$path)
      ra_ai_store_log(con, msg, src$id, "warn")
      if (!file.exists(src$path) && !dir.exists(src$path)) {
        failures[[length(failures) + 1]] <- list(source_id = src$id,
                                                 error = msg)
        ra_ai_upsert_source(con, list(
          source_id = src$id, source_type = src$type, path = src$path,
          title = src$id, version = src$version, file_hash = NA_character_,
          mtime = NA_real_,
          indexed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          status = "error", error = msg, n_chunks = 0L
        ))
      }
      next
    }
    for (f in files) {
      sid <- paste(src$id, digest_fallback(normalizePath(f, winslash = "/",
                                                         mustWork = FALSE)),
                   sep = ":")
      h <- ra_ai_file_hash(f)
      mt <- as.numeric(file.info(f)$mtime)
      if (ra_ai_source_unchanged(con, sid, h, mt)) {
        n_skip <- n_skip + 1L
        next
      }
      ra_ai_delete_source_chunks(con, sid)
      ingested <- ra_ai_ingest_file(f, sid, src$type, src$version)
      status <- "ok"
      err <- ingested$error
      if (!is.null(err) && length(ingested$chunks) == 0) status <- "error"
      if (!is.null(err)) {
        ra_ai_store_log(con, err, sid, if (status == "error") "error" else "warn")
        if (status == "error") {
          failures[[length(failures) + 1]] <- list(source_id = sid, error = err)
        }
      }
      n_ch <- length(ingested$chunks)
      ra_ai_upsert_source(con, list(
        source_id = sid, source_type = src$type, path = f,
        title = basename(f), version = src$version, file_hash = h, mtime = mt,
        indexed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        status = status, error = err %||% NA_character_, n_chunks = n_ch
      ))
      for (ch in ingested$chunks) {
        ra_ai_insert_chunk(con, ch)
        all_new_ids <- c(all_new_ids, ch$chunk_id)
      }
      n_new <- n_new + 1L
    }
  }

  if (length(ignored) > 0) {
    for (ig in ignored) {
      DBI::dbExecute(
        con,
        "INSERT OR REPLACE INTO ignored_paths (path, reason, ts) VALUES (?, ?, ?)",
        params = list(
          ig$path, ig$reason,
          format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        )
      )
    }
  }

  embed_n <- 0L
  embed_err <- NULL
  if (isTRUE(embed) && length(all_new_ids) > 0) {
    embed_res <- tryCatch(
      ra_ai_embed_chunks(con, all_new_ids, config),
      error = function(e) e
    )
    if (inherits(embed_res, "error")) {
      embed_err <- embed_res$message
      ra_ai_store_log(con, paste("Embedding skipped:", embed_err),
                      NA_character_, "warn")
    } else {
      embed_n <- embed_res
    }
  }

  diag <- list(
    index_dir = index_dir,
    manifest = sources_config,
    files_indexed = n_new,
    files_skipped_unchanged = n_skip,
    chunks_embedded = embed_n,
    embed_error = embed_err,
    failures = failures,
    ignored = ignored
  )
  invisible(diag)
}

ra_ai_embed_chunks <- function(con, chunk_ids, config) {
  rows <- ra_ai_get_chunks_by_id(con, chunk_ids)
  if (nrow(rows) == 0) return(0L)
  n <- 0L
  batch <- 8L
  idx <- seq_len(nrow(rows))
  for (from in seq(1L, length(idx), by = batch)) {
    take <- idx[from:min(from + batch - 1L, length(idx))]
    texts <- rows$text[take]
    vecs <- ra_ollama_embed(texts, config)
    if (length(vecs) != length(take)) {
      stop("Embedding count did not match batch size.", call. = FALSE)
    }
    for (j in seq_along(take)) {
      ra_ai_insert_embedding(con, rows$chunk_id[take[j]], vecs[[j]])
      n <- n + 1L
    }
  }
  n
}

##' Index health: sources, versions, chunk counts, ignored paths, failures.
ra_ai_diagnose <- function(index_dir = NULL, config = ra_ai_config()) {
  if (is.null(index_dir)) index_dir <- config$index_dir
  dbfile <- ra_ai_db_file(index_dir)
  if (!file.exists(dbfile)) {
    return(list(
      ok = FALSE,
      index_dir = index_dir,
      message = "No index has been built yet."
    ))
  }
  con <- ra_ai_store_open(index_dir, create = FALSE)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  sources <- ra_ai_list_sources(con)
  n_chunks <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM chunks")$n
  n_embed <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM embeddings")$n
  ignored <- DBI::dbGetQuery(con, "SELECT * FROM ignored_paths")
  log <- DBI::dbGetQuery(
    con,
    "SELECT * FROM ingest_log ORDER BY id DESC LIMIT 50"
  )
  failures <- sources[sources$status != "ok", , drop = FALSE]
  list(
    ok = TRUE,
    index_dir = index_dir,
    dbfile = dbfile,
    n_sources = nrow(sources),
    n_chunks = n_chunks,
    n_embeddings = n_embed,
    sources = sources,
    ignored = ignored,
    failures = failures,
    recent_log = log,
    indexed_at = ra_ai_store_meta_get(con, "indexed_at"),
    manifest = ra_ai_store_meta_get(con, "manifest")
  )
}

ra_ai_print_diagnose <- function(diag = ra_ai_diagnose()) {
  if (!isTRUE(diag$ok)) {
    message(diag$message)
    return(invisible(diag))
  }
  message("Ask Atlantis index: ", diag$dbfile)
  message("Indexed at: ", diag$indexed_at)
  message("Sources: ", diag$n_sources, "  chunks: ", diag$n_chunks,
          "  embeddings: ", diag$n_embeddings)
  if (!is.null(diag$sources) && nrow(diag$sources) > 0) {
    by <- tapply(diag$sources$n_chunks, diag$sources$source_type, sum, na.rm = TRUE)
    for (nm in names(by)) {
      message("  ", nm, ": ", by[[nm]], " chunks")
    }
  }
  if (!is.null(diag$failures) && nrow(diag$failures) > 0) {
    message("Failures:")
    for (i in seq_len(nrow(diag$failures))) {
      message("  ", diag$failures$source_id[i], ": ", diag$failures$error[i])
    }
  }
  if (!is.null(diag$ignored) && nrow(diag$ignored) > 0) {
    message("Ignored paths: ", nrow(diag$ignored))
  }
  if (!is.null(diag$recent_log) && nrow(diag$recent_log) > 0) {
    notes <- diag$recent_log
    notes <- notes[notes$level %in% c("warn", "error"), , drop = FALSE]
    if (nrow(notes) > 0) {
      message("Recent ingest notes:")
      nshow <- min(nrow(notes), 20L)
      for (i in seq_len(nshow)) {
        message("  [", notes$level[i], "] ", notes$message[i])
      }
    }
  }
  invisible(diag)
}
