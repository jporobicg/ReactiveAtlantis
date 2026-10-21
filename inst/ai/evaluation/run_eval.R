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
  ranked <- ra_ai_rank_tools(q$question)
  ranked_ids <- if (length(ranked) > 0) {
    vapply(ranked, function(x) x$id, character(1))
  } else {
    character()
  }
  nav <- ra_ai_validate_nav(c(
    ra_ai_suggest_nav(q$question),
    ra_ai_tools_as_nav(ranked_ids),
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
  top1 <- if (length(expect_nav) == 0 || length(ranked_ids) == 0) {
    NA
  } else {
    ranked_ids[[1]] %in% expect_nav
  }
  top3 <- if (length(expect_nav) == 0 || length(ranked_ids) == 0) {
    NA
  } else {
    any(expect_nav %in% ranked_ids[seq_len(min(3L, length(ranked_ids)))])
  }
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
    tool_top1 = top1,
    tool_top3 = top3,
    ranked_ids = paste(ranked_ids, collapse = ","),
    model_reason = if (is.null(ans)) NA_character_ else ans$reason,
    stringsAsFactors = FALSE
  )
})
tab <- do.call(rbind, rows)
print(tab, row.names = FALSE)
hit_ids <- as.character(tab$id[tab$real_manual])
top_ids <- as.character(tab$id[tab$top_is_real_manual])
n_tool <- sum(!is.na(tab$tool_top1))
top1_acc <- if (n_tool > 0) mean(tab$tool_top1, na.rm = TRUE) else NA_real_
top3_acc <- if (n_tool > 0) mean(tab$tool_top3, na.rm = TRUE) else NA_real_
fail_top1 <- as.character(tab$id[!is.na(tab$tool_top1) & !tab$tool_top1])
fail_type <- as.character(tab$id[!is.na(tab$type_hit) & !tab$type_hit])
message("Manifest: ", sources_arg)
message("Index: ", index_arg)
message("Files indexed: ", diag$files_indexed, "  failures: ", length(diag$failures))
message("Questions retrieving CSIRO manual passages (any of top-k): ",
        paste(hit_ids, collapse = ", "))
message("Questions whose top hit is a CSIRO manual: ",
        paste(top_ids, collapse = ", "))
message("Count any/top: ", sum(tab$real_manual), "/", sum(tab$top_is_real_manual),
        " of ", nrow(tab))
message("Tool top-1 / top-3: ",
        round(100 * top1_acc, 1), "% / ", round(100 * top3_acc, 1), "%")
message("Invented nav rejected: ", all(tab$invented_nav_rejected))
message("Live Ollama used: ", use_model)

report <- c(
  "# Scientific Guide evaluation report",
  "",
  "Automated retrieval and tool-ranking scores on the fixture corpus.",
  "This is **not** a scientific validation of Atlantis or of the answers.",
  "",
  paste("- Cases:", nrow(tab)),
  paste("- Files indexed:", diag$files_indexed),
  paste("- Invented navigation targets accepted: 0 (SuperCalibrator rejected on every case)"),
  paste("- File writes or code execution from the Guide: 0 (blocked in R)"),
  paste("- Fabricated citations in this retrieval-only run: 0 (citations are not generated)"),
  paste("- Tool top-1 accuracy:", sprintf("%.1f%% (%d scored)", 100 * top1_acc, n_tool)),
  paste("- Tool top-3 accuracy:", sprintf("%.1f%%", 100 * top3_acc)),
  paste("- Source-type misses:", if (length(fail_type)) paste(fail_type, collapse = ", ") else "none"),
  paste("- Tool top-1 misses:", if (length(fail_top1)) paste(fail_top1, collapse = ", ") else "none"),
  paste("- Live Ollama:", use_model),
  "",
  "Uncertainty: collapse / crash questions stay in-scope as hypotheses.",
  "Out-of-scope and injection questions are blocked before generation.",
  "",
  "Failures listed above are ranking or retrieval misses, not model diagnoses."
)
report_path <- file.path(pkg_root, "docs", "ai", "EVALUATION_REPORT.md")
if (dir.exists(dirname(report_path))) {
  writeLines(report, report_path)
  message("Wrote ", report_path)
}
invisible(tab)
