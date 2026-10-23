## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis: local Ollama HTTP client     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## All calls go through ra_ollama_request() so tests can mock the HTTP layer.
## There is no cloud fallback.

ra_ollama_normalize_url <- function(url) {
  url <- trimws(as.character(url)[1])
  url <- sub("/+$", "", url)
  url
}

##' Low-level POST/GET. Replace with options(reactiveatlantis.ollama_transport).
ra_ollama_request <- function(method, url, path, body = NULL, timeout_s = 30) {
  transport <- getOption("reactiveatlantis.ollama_transport", default = NULL)
  if (is.function(transport)) {
    return(transport(method = method, url = url, path = path, body = body,
                     timeout_s = timeout_s))
  }
  if (!ra_ai_need_pkg("httr2")) {
    stop("The httr2 package is required to talk to Ollama.", call. = FALSE)
  }
  full <- paste0(ra_ollama_normalize_url(url), path)
  req <- httr2::request(full)
  req <- httr2::req_timeout(req, timeout_s)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req <- httr2::req_headers(req, "Content-Type" = "application/json")
  if (identical(toupper(method), "GET")) {
    req <- httr2::req_method(req, "GET")
  } else {
    req <- httr2::req_method(req, "POST")
    if (!is.null(body)) {
      payload <- if (ra_ai_need_pkg("jsonlite")) {
        jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
      } else {
        stop("jsonlite is required to encode Ollama requests.", call. = FALSE)
      }
      req <- httr2::req_body_raw(req, payload, type = "application/json")
    }
  }
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  txt <- httr2::resp_body_string(resp)
  parsed <- NULL
  if (nzchar(txt) && ra_ai_need_pkg("jsonlite")) {
    parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                       error = function(e) NULL)
  }
  list(
    ok = status >= 200 && status < 300,
    status = status,
    body = parsed,
    raw = txt
  )
}

##' Probe Ollama and the configured models. Never throws.
ra_ollama_status <- function(config = ra_ai_config()) {
  out <- list(
    reachable = FALSE,
    models = character(),
    has_chat_model = FALSE,
    has_embed_model = FALSE,
    message = NULL,
    detail = NULL
  )
  res <- tryCatch(
    ra_ollama_request("GET", config$ollama_url, "/api/tags",
                      timeout_s = config$health_timeout_s),
    error = function(e) {
      list(ok = FALSE, status = NA_integer_, body = NULL, raw = e$message)
    }
  )
  if (!isTRUE(res$ok)) {
    out$message <- "Ollama is not running"
    out$detail <- paste(
      "Start Ollama locally, then pull the configured models.",
      "The assistant never sends text to a cloud service."
    )
    return(out)
  }
  out$reachable <- TRUE
  models <- character()
  if (!is.null(res$body$models)) {
    models <- vapply(res$body$models, function(m) {
      as.character(m$name %||% m$model %||% "")[1]
    }, character(1))
  }
  models <- models[nzchar(models)]
  out$models <- models
  out$has_chat_model <- ra_ollama_model_present(config$model, models)
  out$has_embed_model <- ra_ollama_model_present(config$embed_model, models)
  if (!out$has_chat_model) {
    out$message <- paste0("Chat model '", config$model, "' is not installed")
    out$detail <- paste0(
      "Run: ollama pull ", config$model,
      ". A smaller model such as qwen3.5:2b is also fine."
    )
    return(out)
  }
  if (!out$has_embed_model) {
    out$message <- paste0("Embedding model '", config$embed_model, "' is not installed")
    out$detail <- paste0(
      "Run: ollama pull ", config$embed_model,
      ". Indexing can still use full-text search without embeddings."
    )
    ## Chat can still run; retrieval falls back to FTS.
    out$chat_ready <- TRUE
    return(out)
  }
  out$message <- "Ready (local)"
  out$chat_ready <- TRUE
  out
}

ra_ollama_model_present <- function(want, have) {
  if (length(have) == 0 || !nzchar(want)) return(FALSE)
  want <- tolower(want)
  have <- tolower(have)
  ## Exact tag, or the same tag with a trailing digest suffix.
  any(have == want | startsWith(have, paste0(want, "-")))
}

ra_ollama_chat <- function(messages, config = ra_ai_config()) {
  body <- list(
    model = config$model,
    messages = messages,
    stream = FALSE,
    options = list(temperature = config$temperature)
  )
  res <- ra_ollama_request(
    "POST", config$ollama_url, "/api/chat",
    body = body, timeout_s = config$timeout_s
  )
  if (!isTRUE(res$ok)) {
    stop(ra_ollama_error_message(res, "chat"), call. = FALSE)
  }
  content <- NULL
  if (!is.null(res$body$message$content)) {
    content <- as.character(res$body$message$content)[1]
  } else if (!is.null(res$body$content)) {
    content <- as.character(res$body$content)[1]
  }
  if (is.null(content) || !nzchar(content)) {
    stop("Ollama returned an empty chat response.", call. = FALSE)
  }
  content
}

ra_ollama_embed <- function(texts, config = ra_ai_config()) {
  texts <- as.character(texts)
  if (length(texts) == 0) return(list())
  body <- list(model = config$embed_model, input = texts)
  res <- tryCatch(
    ra_ollama_request(
      "POST", config$ollama_url, "/api/embed",
      body = body, timeout_s = min(config$timeout_s, 60)
    ),
    error = function(e) list(ok = FALSE, body = NULL, raw = e$message, status = NA)
  )
  vecs <- ra_ollama_parse_embeddings(res)
  if (!is.null(vecs)) return(vecs)

  ## Older Ollama builds used /api/embeddings with "prompt".
  vecs <- vector("list", length(texts))
  for (i in seq_along(texts)) {
    one <- ra_ollama_request(
      "POST", config$ollama_url, "/api/embeddings",
      body = list(model = config$embed_model, prompt = texts[[i]]),
      timeout_s = min(config$timeout_s, 60)
    )
    parsed <- ra_ollama_parse_embeddings(one)
    if (is.null(parsed) || length(parsed) < 1) {
      stop(ra_ollama_error_message(one, "embed"), call. = FALSE)
    }
    vecs[[i]] <- parsed[[1]]
  }
  vecs
}

ra_ollama_parse_embeddings <- function(res) {
  if (!isTRUE(res$ok) || is.null(res$body)) return(NULL)
  if (!is.null(res$body$embeddings)) {
    return(lapply(res$body$embeddings, function(v) as.numeric(unlist(v))))
  }
  if (!is.null(res$body$embedding)) {
    return(list(as.numeric(unlist(res$body$embedding))))
  }
  NULL
}

ra_ollama_error_message <- function(res, kind) {
  if (!is.null(res$body$error)) {
    return(paste("Ollama", kind, "error:", as.character(res$body$error)[1]))
  }
  if (!is.null(res$raw) && nzchar(as.character(res$raw)[1])) {
    return(paste("Ollama", kind, "failed:", substr(as.character(res$raw)[1], 1, 200)))
  }
  paste("Ollama", kind, "request failed.")
}

##' Human-readable panel state: ready, offline, model_missing, error, disabled.
ra_ai_panel_state <- function(config = ra_ai_config()) {
  if (!isTRUE(config$enabled)) {
    return(list(code = "disabled", label = "Disabled",
                detail = "Set RA_AI_ENABLED=true to show the Ask Atlantis drawer."))
  }
  st <- tryCatch(ra_ollama_status(config), error = function(e) {
    list(reachable = FALSE, has_chat_model = FALSE,
         message = "Could not check Ollama", detail = e$message)
  })
  if (!isTRUE(st$reachable)) {
    return(list(code = "offline", label = "Offline",
                detail = st$detail %||% st$message))
  }
  if (!isTRUE(st$has_chat_model)) {
    return(list(code = "model_missing", label = "Model missing",
                detail = st$detail %||% st$message))
  }
  list(code = "ready", label = st$message %||% "Ready (local)",
       detail = "Answers stay on this machine. They are guidance, not diagnostics.")
}
