#!/usr/bin/env Rscript
## Rebuild or diagnose the Ask Atlantis local index.
##
##   Rscript inst/ai/index.R
##   Rscript inst/ai/index.R --diagnose
##   Rscript inst/ai/index.R --no-embed

args <- commandArgs(trailingOnly = TRUE)
diagnose_only <- "--diagnose" %in% args || "-d" %in% args
no_embed <- "--no-embed" %in% args

find_load <- function() {
  cand <- c(
    file.path(getwd(), "inst", "ai", "load.R"),
    file.path(dirname(normalizePath(sys.frame(1)$ofile %||% ".")), "load.R")
  )
  pkg <- tryCatch(system.file("ai", "load.R", package = "ReactiveAtlantis"),
                  error = function(e) "")
  if (nzchar(pkg)) cand <- c(pkg, cand)
  hit <- cand[file.exists(cand)]
  if (length(hit) == 0) {
    stop("Cannot find inst/ai/load.R. Run from the package root or install ReactiveAtlantis.")
  }
  hit[1]
}

`%||%` <- function(a, b) if (is.null(a)) b else a
source(find_load(), local = FALSE)

if (isTRUE(diagnose_only)) {
  ra_ai_print_diagnose(ra_ai_diagnose())
} else {
  diag <- ra_ai_index(embed = !isTRUE(no_embed))
  ra_ai_print_diagnose(ra_ai_diagnose(diag$index_dir))
}
