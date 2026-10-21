## Source Ask Atlantis helpers. Used by the Shiny app and exported R wrappers.
##
## Callers must source this file into an *unlocked* environment. The
## installed package namespace is locked and cannot receive new bindings.

if (!exists(".ra_ai_load_dir", inherits = FALSE) ||
    !dir.exists(file.path(.ra_ai_load_dir, "R"))) {
  .ra_ai_load_dir <- local({
    for (i in sys.nframe():1) {
      ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
      if (!is.null(ofile) && nzchar(ofile) &&
          identical(basename(ofile), "load.R")) {
        return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
      }
    }
    pkg <- tryCatch(system.file("ai", package = "ReactiveAtlantis"),
                    error = function(e) "")
    if (nzchar(pkg) && file.exists(file.path(pkg, "R", "ai_config.R"))) {
      return(normalizePath(pkg, winslash = "/", mustWork = FALSE))
    }
    cand <- c(file.path(getwd(), "inst", "ai"), file.path(getwd(), "ai"))
    hit <- cand[file.exists(file.path(cand, "R", "ai_config.R"))]
    if (length(hit) > 0) {
      return(normalizePath(hit[1], winslash = "/", mustWork = FALSE))
    }
    getwd()
  })
}

.ra_ai_files <- file.path(.ra_ai_load_dir, "R", c(
  "ai_config.R",
  "ai_nav.R",
  "ai_cases.R",
  "ai_tools.R",
  "ai_context.R",
  "ai_ollama.R",
  "ai_store.R",
  "ai_ingest.R",
  "ai_retrieve.R",
  "ai_prompt.R",
  "ai_answer.R",
  "ai_download.R"
))
for (.ra_ai_f in .ra_ai_files) {
  if (file.exists(.ra_ai_f)) source(.ra_ai_f, local = TRUE)
}
rm(.ra_ai_load_dir, .ra_ai_files, .ra_ai_f)
