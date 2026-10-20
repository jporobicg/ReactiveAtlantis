#!/usr/bin/env Rscript
## Retrieval-only evaluation against the fixture corpus (no Ollama required).
## With a live model, answers can be scored by hand using questions.json.

args <- commandArgs(trailingOnly = TRUE)
use_model <- "--model" %in% args

root_cands <- c(getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", ".."))
load_r <- NULL
for (p in root_cands) {
  f <- file.path(p, "inst", "ai", "load.R")
  if (file.exists(f)) {
    load_r <- f
    qfile <- file.path(p, "inst", "ai", "evaluation", "questions.json")
    break
  }
}
if (is.null(load_r)) stop("Run from the ReactiveAtlantis package root.")
source(load_r, local = TRUE)
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to run the evaluation set.")
}

cfg <- ra_ai_config()
idx <- tempfile("ra-ai-eval-")
dir.create(idx)
diag <- ra_ai_index(index_dir = idx, embed = FALSE)
questions <- jsonlite::fromJSON(qfile, simplifyVector = FALSE)$questions

rows <- lapply(questions, function(q) {
  scope <- ra_ai_scope_check(q$question)
  retrieved <- ra_ai_retrieve(q$question, index_dir = idx, config = cfg)
  chunks <- retrieved$chunks
  types <- if (nrow(chunks)) unique(as.character(chunks$source_type)) else character()
  nav <- ra_ai_validate_nav(c(
    ra_ai_suggest_nav(q$question),
    list(list(id = "SuperCalibrator", title = "SuperCalibrator"))
  ))
  nav_ids <- vapply(nav, function(x) x$id, character(1))
  ans <- NULL
  if (isTRUE(use_model)) {
    ans <- ra_ai_answer(q$question, config = modifyList(cfg, list(index_dir = idx)))
  }
  expect_types <- unlist(q$expect$source_types)
  type_hit <- if (length(expect_types) == 0) NA else any(expect_types %in% types)
  expect_nav <- unlist(q$expect$nav_ids)
  nav_hit <- if (length(expect_nav) == 0) NA else any(expect_nav %in% nav_ids)
  data.frame(
    id = q$id,
    kind = q$kind,
    scope_ok = isTRUE(scope$ok),
    scope_reason = scope$reason,
    n_chunks = nrow(chunks),
    source_types = paste(types, collapse = ","),
    type_hit = type_hit,
    nav_ids = paste(nav_ids, collapse = ","),
    nav_hit = nav_hit,
    invented_nav_rejected = !"SuperCalibrator" %in% nav_ids,
    model_reason = if (is.null(ans)) NA_character_ else ans$reason,
    stringsAsFactors = FALSE
  )
})
tab <- do.call(rbind, rows)
print(tab, row.names = FALSE)
message("Retrieval eval against fixture corpus. Live Ollama used: ", use_model)
invisible(tab)
