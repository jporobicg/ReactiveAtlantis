## Shared Ask Atlantis test helpers.

source_ask_atlantis <- function() {
  root <- ra_pkg_root()
  source(file.path(root, "inst", "ai", "load.R"), local = FALSE)
  source(file.path(root, "inst", "shiny", "modules", "assistant_module.R"),
         local = FALSE)
  root
}

ra_ai_test_transport <- function(method, url, path, body = NULL,
                                 timeout_s = 30) {
  if (identical(path, "/api/tags")) {
    return(list(
      ok = TRUE, status = 200L,
      body = list(models = list(
        list(name = "qwen3.5:4b"),
        list(name = "nomic-embed-text")
      )),
      raw = "{}"
    ))
  }
  if (path %in% c("/api/embed", "/api/embeddings")) {
    texts <- body$input
    if (is.null(texts)) texts <- body$prompt
    if (is.null(texts)) texts <- ""
    if (!is.list(texts)) texts <- as.list(texts)
    emb <- lapply(texts, function(txt) {
      chars <- utf8ToInt(substring(paste(txt, collapse = " "), 1, 32))
      if (length(chars) < 8) chars <- c(chars, rep(1, 8 - length(chars)))
      as.numeric(chars[1:8]) / 1000
    })
    body_out <- if (identical(path, "/api/embeddings")) {
      list(embedding = emb[[1]])
    } else {
      list(embeddings = emb)
    }
    return(list(ok = TRUE, status = 200L, body = body_out, raw = "{}"))
  }
  if (identical(path, "/api/chat")) {
    user <- ""
    hist_echo <- ""
    if (!is.null(body$messages) && length(body$messages) > 0) {
      last <- body$messages[[length(body$messages)]]
      user <- as.character(last$content %||% "")[1]
      hist_echo <- paste(vapply(body$messages, function(m) {
        as.character(m$content %||% "")[1]
      }, character(1)), collapse = "\n")
    }
    ids <- unique(unlist(regmatches(
      user, gregexpr("chunk_id=chk_[a-z0-9]+|chunk_id=nav_[a-z0-9]+", user)
    )))
    ids <- sub("chunk_id=", "", ids)
    sids <- unique(unlist(regmatches(
      user, gregexpr("source_id=[^ \\n]+", user)
    )))
    sids <- sub("source_id=", "", sids)
    qtxt <- sub("(?s).*User question:\\n", "", user, perl = TRUE)
    citations <- lapply(seq_along(ids), function(i) {
      list(chunk_id = ids[[i]], source_id = if (i <= length(sids)) sids[[i]] else "")
    })
    kinds <- list("documented_fact")
    if (grepl("calibrat", qtxt, ignore.case = TRUE)) {
      kinds <- list("calibration_example")
    }
    if (grepl("decline|hypothes|why can", qtxt, ignore.case = TRUE)) {
      kinds <- list("hypothesis")
    }
    if (grepl("fixture_consume|clearance|gape_overlap", qtxt, ignore.case = TRUE) ||
        grepl("consumption function", qtxt, ignore.case = TRUE)) {
      kinds <- list("code_verified")
    }
    nav <- list()
    if (grepl("predation", qtxt, ignore.case = TRUE)) {
      nav <- c(nav, list(list(id = "predation")))
    }
    if (grepl("recruit", qtxt, ignore.case = TRUE)) {
      nav <- c(nav, list(list(id = "recruitment")))
    }
    if (grepl("feeding|pPREY|KLP", qtxt, ignore.case = TRUE)) {
      nav <- c(nav, list(list(id = "feeding")))
    }
    if (grepl("SuperCalibrator", qtxt, ignore.case = TRUE)) {
      nav <- c(nav, list(list(id = "SuperCalibrator")))
    }
    answer <- paste(
      "Based on the retrieved local passages:",
      substring(qtxt, 1, 280),
      "HIST:", substring(hist_echo, 1, 4000)
    )
    if (grepl("calibrat", qtxt, ignore.case = TRUE)) {
      answer <- paste(
        answer,
        "In one calibration example this was observed;",
        "it is not a universal parameter rule."
      )
    }
    payload <- jsonlite::toJSON(
      list(answer = answer, claim_kinds = kinds,
           citations = citations, nav_targets = nav),
      auto_unbox = TRUE
    )
    return(list(
      ok = TRUE, status = 200L,
      body = list(message = list(role = "assistant", content = payload)),
      raw = as.character(payload)
    ))
  }
  list(ok = FALSE, status = 404L, body = NULL, raw = "unknown mock path")
}

ra_ai_unreachable_transport <- function(method, url, path, body = NULL,
                                        timeout_s = 30) {
  stop("Ollama is not running (mock)")
}

with_mock_ollama <- function(code, transport = ra_ai_test_transport) {
  old <- getOption("reactiveatlantis.ollama_transport")
  on.exit(options(reactiveatlantis.ollama_transport = old), add = TRUE)
  options(reactiveatlantis.ollama_transport = transport)
  force(code)
}

build_fixture_index <- function(embed = FALSE) {
  root <- source_ask_atlantis()
  idx <- tempfile("ra-ai-idx-")
  dir.create(idx)
  cfg <- ra_ai_config()
  cfg$index_dir <- idx
  man <- file.path(root, "inst", "ai", "sources.json")
  if (isTRUE(embed)) {
    with_mock_ollama({
      ra_ai_index(sources_config = man, index_dir = idx, embed = TRUE, config = cfg)
    })
  } else {
    ra_ai_index(sources_config = man, index_dir = idx, embed = FALSE, config = cfg)
  }
  cfg
}
