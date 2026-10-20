#!/usr/bin/env Rscript
## Retrieval evaluation.
## Default: fixture corpus (no Ollama required).
## With --sources PATH, index that manifest instead (CSIRO manuals, etc.).
##
##   Rscript inst/ai/evaluation/run_eval.R
##   Rscript inst/ai/evaluation/run_eval.R --sources inst/ai/sources.atlantis-manuals.json
##   Rscript inst/ai/evaluation/run_eval.R --model

args <- commandArgs(trailingOnly = TRUE)
use_model <- "--model" %in% args

sources_arg <- NULL
index_arg <- NULL
i <- match("--sources", args)
if (!is.na(i) && i < length(args) && !startsWith(args[i + 1], "--")) {
  sources_arg <- args[i + 1]
}
i <- match("--index-dir", args)
if (!is.na(i) && i < length(args) && !startsWith(args[i + 1], "--")) {
  index_arg <- args[i + 1]
}
for (a in args) {
  if (startsWith(a, "--sources=")) sources_arg <- sub("^--sources=", "", a)
  if (startsWith(a, "--index-dir=")) index_arg <- sub("^--index-dir=", "", a)
}

root_cands <- c(getwd(), file.path(getwd(), ".."), file.path(getwd(), "..", ".."))
load_r <- NULL
pkg_root <- getwd()
for (p in root_cands) {
  f <- file.path(p, "inst", "ai", "load.R")
  if (file.exists(f)) {
    load_r <- f
    pkg_root <- p
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
if (is.null(sources_arg) || !nzchar(sources_arg)) {
  sources_arg <- file.path(pkg_root, "inst", "ai", "sources.json")
}
if (!file.exists(sources_arg)) {
  alt <- file.path(pkg_root, sources_arg)
  if (file.exists(alt)) sources_arg <- alt
}
if (is.null(index_arg) || !nzchar(index_arg)) {
  index_arg <- tempfile("ra-ai-eval-")
  dir.create(index_arg)
}

diag <- ra_ai_index(sources_config = sources_arg, index_dir = index_arg,
                    embed = FALSE)
questions <- jsonlite::fromJSON(qfile, simplifyVector = FALSE)$questions

rows <- lapply(questions, function(q) {
  scope <- ra_ai_scope_check(q$question)
  retrieved <- ra_ai_retrieve(q$question, index_dir = index_arg, config = cfg)
  chunks <- retrieved$chunks
  types <- if (nrow(chunks)) unique(as.character(chunks$source_type)) else character()
  nav <- ra_ai_validate_nav(c(
    ra_ai_suggest_nav(q$question),
    list(list(id = "SuperCalibrator", title = "SuperCalibrator"))
  ))
  nav_ids <- vapply(nav, function(x) x$id, character(1))
  ans <- NULL
  if (isTRUE(use_model)) {
    ans <- ra_ai_answer(q$question, config = modifyList(cfg, list(index_dir = index_arg)))
  }
  expect_types <- unlist(q$expect$source_types)
  type_hit <- if (length(expect_types) == 0) NA else any(expect_types %in% types)
  expect_nav <- unlist(q$expect$nav_ids)
  nav_hit <- if (length(expect_nav) == 0) NA else any(expect_nav %in% nav_ids)
  real_manual <- FALSE
  top_is_real_manual <- FALSE
  top_path <- NA_character_
  top_section <- NA_character_
  if (nrow(chunks) > 0) {
    is_csiro <- mapply(
      ra_ai_is_csiro_manual_chunk,
      chunks$path, chunks$source_id, chunks$version
    )
    real_manual <- any(is_csiro)
    top_is_real_manual <- isTRUE(is_csiro[[1]])
    top_path <- basename(as.character(chunks$path[1]))
    top_section <- as.character(chunks$section[1])
  }
  data.frame(
    id = q$id,
    kind = q$kind,
    scope_ok = isTRUE(scope$ok),
    scope_reason = scope$reason,
    n_chunks = nrow(chunks),
    source_types = paste(types, collapse = ","),
    type_hit = type_hit,
    real_manual = real_manual,
    top_is_real_manual = top_is_real_manual,
    top_path = top_path,
    top_section = top_section,
    nav_ids = paste(nav_ids, collapse = ","),
    nav_hit = nav_hit,
    invented_nav_rejected = !"SuperCalibrator" %in% nav_ids,
    model_reason = if (is.null(ans)) NA_character_ else ans$reason,
    stringsAsFactors = FALSE
  )
})
tab <- do.call(rbind, rows)
print(tab, row.names = FALSE)
hit_ids <- as.character(tab$id[tab$real_manual])
top_ids <- as.character(tab$id[tab$top_is_real_manual])
message("Manifest: ", sources_arg)
message("Index: ", index_arg)
message("Files indexed: ", diag$files_indexed, "  failures: ", length(diag$failures))
message("Questions retrieving CSIRO manual passages (any of top-k): ",
        paste(hit_ids, collapse = ", "))
message("Questions whose top hit is a CSIRO manual: ",
        paste(top_ids, collapse = ", "))
message("Count any/top: ", sum(tab$real_manual), "/", sum(tab$top_is_real_manual),
        " of ", nrow(tab))
message("Live Ollama used: ", use_model)
invisible(tab)
