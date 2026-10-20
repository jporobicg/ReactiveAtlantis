## Source Ask Atlantis helpers. Used by the Shiny app and exported R wrappers.

.ra_ai_load_dir <- local({
  ofile <- NULL
  for (i in sys.nframe():1) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile) && nzchar(ofile)) {
      return(dirname(normalizePath(ofile, winslash = "/", mustWork = FALSE)))
    }
  }
  getwd()
})

.ra_ai_files <- file.path(.ra_ai_load_dir, "R", c(
  "ai_config.R",
  "ai_nav.R",
  "ai_ollama.R",
  "ai_store.R",
  "ai_ingest.R",
  "ai_retrieve.R",
  "ai_prompt.R",
  "ai_answer.R"
))
for (.ra_ai_f in .ra_ai_files) {
  if (file.exists(.ra_ai_f)) source(.ra_ai_f, local = TRUE)
}
rm(.ra_ai_load_dir, .ra_ai_files, .ra_ai_f)
