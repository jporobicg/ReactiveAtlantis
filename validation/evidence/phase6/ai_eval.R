`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
## Phase 6: run the independent benchmark through the Guide's own ra_ai_answer() with the live local model.
args <- commandArgs(TRUE); qfile <- args[1]; out <- args[2]
lib <- "~/whacamole_nesp/ra_audit_data/lib_pr4"; .libPaths(c(path.expand(lib), .libPaths()))
Sys.setenv(RA_AI_ENABLED = "true", RA_AI_INDEX_DIR = path.expand("~/whacamole_nesp/ra_audit_data/ai/index"),
           RA_AI_CORPUS_DIR = path.expand("~/whacamole_nesp/ra_audit_data/ai/corpus"),
           RA_AI_SOURCES_CONFIG = system.file("ai", "sources.atlantis-manuals.json", package = "ReactiveAtlantis"))
env <- new.env(); assign(".ra_ai_load_dir", system.file("ai", package = "ReactiveAtlantis"), envir = env)
sys.source(file.path(system.file("ai", package = "ReactiveAtlantis"), "load.R"), envir = env)
if (identical(Sys.getenv("AUDIT_THINK_FALSE"), "1")) {   # AUDIT WORKAROUND (not shipped behaviour): disable model reasoning
  options(reactiveatlantis.ollama_transport = function(method, url, path, body = NULL, timeout_s = 30) {
    if (identical(path, "/api/chat") && !is.null(body)) body$think <- FALSE
    op <- options(reactiveatlantis.ollama_transport = NULL); on.exit(options(op))
    env$ra_ollama_request(method, url, path, body, timeout_s) })
  cat("AUDIT WORKAROUND: think=false injected via reactiveatlantis.ollama_transport\n") }
cfg <- env$ra_ai_config(); cat("model:", cfg$model %||% cfg$chat_model %||% "?", "\n")
Q <- jsonlite::fromJSON(qfile)$questions
res <- list()
for (i in seq_len(nrow(Q))) {
  q <- Q[i, ]; t0 <- Sys.time()
  a <- tryCatch(env$ra_ai_answer(q$q, config = cfg), error = function(e) list(answer = paste("ERROR:", conditionMessage(e))))
  sec <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  txt <- paste(unlist(a$answer), collapse = " ")
  cites <- paste(vapply(a$citations %||% list(), function(x) paste(x$title %||% x$id %||% "", x$page %||% x$section %||% ""), ""), collapse = " | ")
  nav <- paste(vapply(a$nav_targets %||% list(), function(x) paste(x$title %||% x$tab %||% x$id %||% ""), ""), collapse = ", ")
  must <- unlist(q$must); mustnot <- unlist(q$must_not)
  hit <- vapply(must, function(p) grepl(p, txt, ignore.case = TRUE, perl = TRUE), TRUE)
  bad <- if (length(mustnot)) vapply(mustnot, function(p) grepl(p, txt, ignore.case = TRUE, perl = TRUE), TRUE) else logical()
  res[[i]] <- data.frame(id = q$id, cat = q$cat, seconds = sec, must_hit = sum(hit), must_n = length(must), must_not_hit = sum(bad),
                         pass = all(hit) && !any(bad), mode = paste(a$mode %||% a$kind %||% a$status %||% "", collapse = ""),
                         n_citations = length(a$citations %||% list()), nav = nav, citations = substr(cites, 1, 300),
                         answer = substr(gsub("\\s+", " ", txt), 1, 1500), stringsAsFactors = FALSE)
  cat(sprintf("%s %-14s %5.1fs pass=%s must %d/%d mustnot %d cites %d\n", q$id, q$cat, sec, res[[i]]$pass, sum(hit), length(must), sum(bad), res[[i]]$n_citations))
  write.csv(do.call(rbind, res), out, row.names = FALSE)
}
