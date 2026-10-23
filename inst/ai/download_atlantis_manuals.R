#!/usr/bin/env Rscript
## Download CSIRO Atlantis user-guide PDFs into a local corpus directory.
##
## The PDFs are CSIRO copyright and are NOT redistributed with this package.
## See inst/ai/atlantis-manuals.json for URLs and the copyright note.
##
##   Rscript inst/ai/download_atlantis_manuals.R
##   Rscript inst/ai/download_atlantis_manuals.R --dest /path/to/corpus
##   Rscript inst/ai/download_atlantis_manuals.R --force

args <- commandArgs(trailingOnly = TRUE)
overwrite <- "--force" %in% args || "--overwrite" %in% args

dest <- NULL
i <- match("--dest", args)
if (!is.na(i) && i < length(args) && !startsWith(args[i + 1], "--")) {
  dest <- args[i + 1]
}
for (a in args) {
  if (startsWith(a, "--dest=")) dest <- sub("^--dest=", "", a)
}

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

res <- ra_ai_download_atlantis_manuals(
  dest_dir = dest,
  overwrite = overwrite,
  verbose = TRUE
)
print(res$files, row.names = FALSE)
if (!isTRUE(res$ok)) {
  stop("One or more Atlantis manuals could not be downloaded.", call. = FALSE)
}
message("Corpus directory: ", res$dest_dir)
message("Index with:")
message("  Rscript inst/ai/index.R --no-embed")
message("after setting RA_AI_SOURCES_CONFIG to inst/ai/sources.atlantis-manuals.json")
