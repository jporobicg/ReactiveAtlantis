## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis: hybrid FTS + embedding search ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ra_ai_stopwords <- function() {
  c("the", "and", "for", "what", "does", "this", "that", "with", "from",
    "how", "where", "when", "should", "would", "could", "about", "into",
    "your", "have", "been", "will", "which", "there", "their", "them",
    "then", "than", "some", "just", "like", "over", "also", "only")
}

ra_ai_tokens <- function(text) {
  toks <- unlist(strsplit(tolower(as.character(text)[1]), "[^a-z0-9_]+"))
  toks <- toks[nchar(toks) >= 2]
  setdiff(toks, ra_ai_stopwords())
}

ra_ai_fts_query <- function(text) {
  toks <- ra_ai_tokens(text)
  if (length(toks) == 0) return(NULL)
  toks <- unique(toks)
  if (length(toks) > 12) toks <- toks[1:12]
  toks <- gsub('"', "", toks, fixed = TRUE)
  paste(sprintf('"%s"', toks), collapse = " OR ")
}

ra_ai_source_weight <- function(source_type) {
  switch(as.character(source_type)[1],
         atlantis_code = 1.05,
         manual = 1.00,
         reactive_code = 0.95,
         calibration = 0.70,
         0.80)
}

ra_ai_cosine <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  if (length(a) == 0 || length(b) == 0 || length(a) != length(b)) return(0)
  na <- sqrt(sum(a * a))
  nb <- sqrt(sum(b * b))
  if (!is.finite(na) || !is.finite(nb) || na == 0 || nb == 0) return(0)
  sum(a * b) / (na * nb)
}

ra_ai_fts_search <- function(con, question, limit = 20L) {
  q <- ra_ai_fts_query(question)
  if (is.null(q)) return(data.frame())
  sql <- paste(
    "SELECT chunk_id, bm25(chunks_fts) AS rank",
    "FROM chunks_fts WHERE chunks_fts MATCH ?",
    "ORDER BY rank LIMIT ?"
  )
  hits <- tryCatch(
    DBI::dbGetQuery(con, sql, params = list(q, as.integer(limit))),
    error = function(e) data.frame()
  )
  hits
}

ra_ai_embed_search <- function(con, question, config, limit = 20L) {
  n_emb <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM embeddings")$n
  if (n_emb < 1) return(data.frame())
  qvec <- tryCatch(ra_ollama_embed(question, config)[[1]],
                   error = function(e) NULL)
  if (is.null(qvec)) return(data.frame())
  emb <- DBI::dbGetQuery(con, "SELECT chunk_id, vector FROM embeddings")
  if (nrow(emb) == 0) return(data.frame())
  scores <- vapply(seq_len(nrow(emb)), function(i) {
    ra_ai_cosine(qvec, ra_ai_unpack_vector(emb$vector[[i]]))
  }, numeric(1))
  ord <- order(scores, decreasing = TRUE)
  take <- ord[seq_len(min(limit, length(ord)))]
  data.frame(
    chunk_id = emb$chunk_id[take],
    embed_score = scores[take],
    stringsAsFactors = FALSE
  )
}

##' Retrieve a small set of passages with provenance.
ra_ai_retrieve <- function(question, index_dir = NULL, config = ra_ai_config(),
                           k = NULL) {
  if (is.null(index_dir)) index_dir <- config$index_dir
  if (is.null(k)) k <- config$max_chunks
  dbfile <- ra_ai_db_file(index_dir)
  if (!file.exists(dbfile)) {
    return(list(
      chunks = data.frame(),
      message = "No local index is available. Run index_ask_atlantis() first."
    ))
  }
  con <- ra_ai_store_open(index_dir, create = FALSE)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  fts <- ra_ai_fts_search(con, question, limit = 24L)
  emb <- ra_ai_embed_search(con, question, config, limit = 24L)

  ids <- unique(c(as.character(fts$chunk_id), as.character(emb$chunk_id)))
  if (length(ids) == 0) {
    ## Last resort: token LIKE search so parameter names still hit
    toks <- ra_ai_tokens(question)
    if (length(toks) > 0) {
      like <- paste0("%", toks[1], "%")
      ids <- DBI::dbGetQuery(
        con,
        "SELECT chunk_id FROM chunks WHERE lower(text) LIKE lower(?) LIMIT 12",
        params = list(like)
      )$chunk_id
    }
  }
  if (length(ids) == 0) {
    return(list(chunks = data.frame(), message = "No matching passages."))
  }
  rows <- ra_ai_get_chunks_by_id(con, ids)
  if (nrow(rows) == 0) {
    return(list(chunks = data.frame(), message = "No matching passages."))
  }

  fts_map <- if (nrow(fts) > 0) {
    stats::setNames(as.numeric(fts$rank), fts$chunk_id)
  } else numeric()
  emb_map <- if (nrow(emb) > 0) {
    stats::setNames(as.numeric(emb$embed_score), emb$chunk_id)
  } else numeric()

  q_toks <- ra_ai_tokens(question)
  scores <- vapply(seq_len(nrow(rows)), function(i) {
    id <- rows$chunk_id[i]
    ## FTS5 bm25: smaller (more negative) is a better match.
    fts_s <- if (id %in% names(fts_map)) 1 / (1 + exp(fts_map[[id]])) else 0
    emb_s <- if (id %in% names(emb_map)) emb_map[[id]] else 0
    txt <- tolower(rows$text[i])
    sec <- tolower(as.character(rows$section[i])[1])
    exact <- sum(vapply(q_toks, function(t) grepl(t, txt, fixed = TRUE),
                        logical(1)))
    exact_boost <- min(0.4, 0.06 * exact)
    if (nzchar(sec) && sec %in% q_toks) exact_boost <- exact_boost + 0.35
    fts_w <- if (any(emb_s > 0) || length(emb_map) > 0) 0.45 else 0.75
    emb_w <- 1 - fts_w
    w <- ra_ai_source_weight(rows$source_type[i])
    if (grepl("calibrat", tolower(question)) &&
        identical(as.character(rows$source_type[i])[1], "calibration")) {
      w <- w * 1.8
    }
    w * (fts_w * fts_s + emb_w * emb_s + exact_boost)
  }, numeric(1))
  rows$score <- scores
  rows <- rows[order(rows$score, decreasing = TRUE), , drop = FALSE]
  if (nrow(rows) > k) rows <- rows[seq_len(k), , drop = FALSE]
  list(chunks = rows, message = NULL)
}

ra_ai_format_passages <- function(chunks, max_chars = 1200L) {
  if (is.null(chunks) || nrow(chunks) == 0) return("")
  parts <- vapply(seq_len(nrow(chunks)), function(i) {
    loc <- ra_ai_format_location(chunks[i, ])
    body <- chunks$text[i]
    if (nchar(body) > max_chars) body <- paste0(substring(body, 1, max_chars), "…")
    paste0(
      "[", i, "] source_id=", chunks$source_id[i],
      " chunk_id=", chunks$chunk_id[i],
      " type=", chunks$source_type[i],
      " version=", chunks$version[i], "\n",
      "title=", chunks$title[i], " section=", chunks$section[i], " ", loc, "\n",
      body
    )
  }, character(1))
  paste(parts, collapse = "\n\n----\n\n")
}

ra_ai_format_location <- function(row) {
  kind <- as.character(row$loc_kind %||% "")[1]
  a <- row$loc_start
  b <- row$loc_end
  if (identical(kind, "page") && !is.na(a)) {
    if (!is.na(b) && b != a) return(paste0("pages ", a, "-", b))
    return(paste0("page ", a))
  }
  if (identical(kind, "line") && !is.na(a)) {
    if (!is.na(b)) return(paste0("lines ", a, "-", b))
    return(paste0("line ", a))
  }
  if (identical(kind, "nav")) return("app navigation")
  ""
}

ra_ai_citation_from_row <- function(row) {
  list(
    chunk_id = as.character(row$chunk_id)[1],
    source_id = as.character(row$source_id)[1],
    source_type = as.character(row$source_type)[1],
    path = as.character(row$path)[1],
    title = as.character(row$title)[1],
    section = as.character(row$section)[1],
    version = as.character(row$version)[1],
    location = ra_ai_format_location(row)
  )
}
