## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis: configuration and paths      ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' Locate the ReactiveAtlantis package root (source tree or installed copy).
ra_ai_find_root <- function() {
  env <- Sys.getenv("RA_PKG_ROOT", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }

  inst_pkg <- tryCatch(system.file(package = "ReactiveAtlantis"),
                       error = function(e) "")
  if (nzchar(inst_pkg) && file.exists(file.path(inst_pkg, "DESCRIPTION"))) {
    return(normalizePath(inst_pkg, winslash = "/", mustWork = FALSE))
  }

  cand <- c(
    getwd(),
    file.path(getwd(), ".."),
    file.path(getwd(), "..", ".."),
    file.path(getwd(), "..", "..", "..")
  )
  for (p in cand) {
    if (!dir.exists(p)) next
    if (file.exists(file.path(p, "DESCRIPTION")) &&
        (file.exists(file.path(p, "inst", "shiny", "app.R")) ||
         file.exists(file.path(p, "shiny", "app.R")))) {
      return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

##' Shiny app, AI helper, and R source directories for this checkout or install.
ra_ai_paths <- function(root = ra_ai_find_root()) {
  shiny_dir <- if (dir.exists(file.path(root, "inst", "shiny"))) {
    file.path(root, "inst", "shiny")
  } else {
    file.path(root, "shiny")
  }
  ai_dir <- if (dir.exists(file.path(root, "inst", "ai"))) {
    file.path(root, "inst", "ai")
  } else {
    file.path(root, "ai")
  }
  list(
    root = root,
    shiny = shiny_dir,
    ai = ai_dir,
    r_src = file.path(root, "R")
  )
}

ra_ai_truthy <- function(x) {
  x <- tolower(trimws(as.character(x)[1]))
  x %in% c("true", "1", "yes", "on")
}

##' Whether the Ask Atlantis panel should appear. Default is off.
ra_ai_enabled <- function() {
  ra_ai_truthy(Sys.getenv("RA_AI_ENABLED", unset = "false"))
}

ra_ai_default_index_dir <- function() {
  cache <- tryCatch(
    tools::R_user_dir("ReactiveAtlantis", which = "cache"),
    error = function(e) file.path(tempdir(), "ReactiveAtlantis-cache")
  )
  file.path(cache, "ask-atlantis")
}

ra_ai_default_sources_config <- function() {
  paths <- ra_ai_paths()
  file.path(paths$ai, "sources.json")
}

##' Local directory for downloaded CSIRO Atlantis manuals (not in git).
ra_ai_default_corpus_dir <- function() {
  env <- Sys.getenv("RA_AI_CORPUS_DIR", unset = "")
  if (nzchar(env)) {
    return(normalizePath(env, winslash = "/", mustWork = FALSE))
  }
  data <- tryCatch(
    tools::R_user_dir("ReactiveAtlantis", which = "data"),
    error = function(e) file.path(tempdir(), "ReactiveAtlantis-data")
  )
  file.path(data, "atlantis-manuals")
}

##' Expand ${RA_PKG_ROOT}, ${RA_SHINY_DIR}, ${RA_AI_DIR}, ${RA_R_DIR},
##' ${RA_AI_CORPUS_DIR} in a path.
ra_ai_expand_path <- function(path, paths = ra_ai_paths()) {
  path <- as.character(path)[1]
  if (is.na(path) || !nzchar(path)) return(path)
  path <- gsub("${RA_PKG_ROOT}", paths$root, path, fixed = TRUE)
  path <- gsub("${RA_SHINY_DIR}", paths$shiny, path, fixed = TRUE)
  path <- gsub("${RA_AI_DIR}", paths$ai, path, fixed = TRUE)
  path <- gsub("${RA_R_DIR}", paths$r_src, path, fixed = TRUE)
  path <- gsub("${RA_AI_CORPUS_DIR}", ra_ai_default_corpus_dir(),
               path, fixed = TRUE)
  path
}

##' Runtime configuration. Environment variables override defaults.
ra_ai_config <- function() {
  index_dir <- Sys.getenv("RA_AI_INDEX_DIR", unset = "")
  if (!nzchar(index_dir)) index_dir <- ra_ai_default_index_dir()
  sources_config <- Sys.getenv("RA_AI_SOURCES_CONFIG", unset = "")
  if (!nzchar(sources_config)) sources_config <- ra_ai_default_sources_config()

  temp <- suppressWarnings(as.numeric(Sys.getenv("RA_OLLAMA_TEMPERATURE", "0.2")))
  if (!is.finite(temp)) temp <- 0.2
  timeout_s <- suppressWarnings(as.numeric(Sys.getenv("RA_OLLAMA_TIMEOUT_S", "90")))
  if (!is.finite(timeout_s) || timeout_s <= 0) timeout_s <- 90

  list(
    enabled = ra_ai_enabled(),
    ollama_url = Sys.getenv("RA_OLLAMA_URL", unset = "http://127.0.0.1:11434"),
    model = Sys.getenv("RA_OLLAMA_MODEL", unset = "qwen3.5:4b"),
    embed_model = Sys.getenv("RA_OLLAMA_EMBED_MODEL", unset = "nomic-embed-text"),
    index_dir = index_dir,
    sources_config = sources_config,
    temperature = temp,
    timeout_s = timeout_s,
    health_timeout_s = 2,
    max_history = 12L,
    max_chunks = 6L,
    max_chunk_chars = 1200L
  )
}

ra_ai_need_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

ra_ai_db_file <- function(index_dir = ra_ai_config()$index_dir) {
  file.path(index_dir, "ask-atlantis.sqlite")
}

##' Locate a file under inst/ai, preferring this checkout over an older install.
ra_ai_ai_file <- function(...) {
  rel <- file.path(...)
  cands <- c(
    file.path(ra_ai_paths()$ai, rel),
    file.path(getwd(), "inst", "ai", rel),
    file.path(getwd(), "ai", rel),
    file.path(getwd(), "..", "inst", "ai", rel),
    file.path(getwd(), "..", "..", "inst", "ai", rel)
  )
  env <- Sys.getenv("RA_PKG_ROOT", unset = "")
  if (nzchar(env)) {
    cands <- c(cands, file.path(env, "inst", "ai", rel), file.path(env, "ai", rel))
  }
  hit <- cands[file.exists(cands)]
  if (length(hit) > 0) return(hit[1])
  file.path(ra_ai_paths()$ai, rel)
}
