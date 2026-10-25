## Launch ReactiveAtlantis three documented ways on one NESP run; record resolved files and timings.
args <- commandArgs(TRUE); run <- args[1]; out_dir <- args[2]
lib <- "~/whacamole_nesp/ra_audit_data/lib_pr4"
.libPaths(c(lib, .libPaths())); Sys.setenv(NOT_CRAN = "true", R_LIBS = paste(.libPaths(), collapse = ":"))
library(shinytest2); dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
mk <- function(name, opts) {
  d <- file.path(tempdir(), name); dir.create(d)
  writeLines(c(sprintf(".libPaths(c('%s', .libPaths()))", path.expand(lib)),
               sprintf("options(%s)", opts),
               "shiny::shinyAppDir(system.file('shiny', package = 'ReactiveAtlantis'))"),
             file.path(d, "app.R")); d
}
modes <- list(
  pair    = sprintf("reactiveatlantis.config_dir='%s', reactiveatlantis.output_dir='%s'", run, file.path(run, "outputs")),
  project = sprintf("reactiveatlantis.project_dir='%s'", run),
  project_out = sprintf("reactiveatlantis.project_dir='%s'", file.path(run, "outputs")),
  blank   = "reactiveatlantis.dummy=1")
txt <- function(app, id) tryCatch(rvest::html_text2(rvest::read_html(app$get_html(id))), error = function(e) paste("ERR", conditionMessage(e)))
for (m in names(modes)) {
  t0 <- Sys.time()
  app <- AppDriver$new(mk(m, modes[[m]]), name = m, width = 1600, height = 1000, load_timeout = 120000, timeout = 600000)
  app$wait_for_idle(timeout = 600000); t_start <- difftime(Sys.time(), t0, units = "secs")
  if (m == "blank") {
    app$set_inputs(`project-run1_config_dir` = run, `project-run1_output_dir` = file.path(run, "outputs"))
  }
  t1 <- Sys.time(); app$click("project-load_project"); app$wait_for_idle(timeout = 600000)
  t_load <- difftime(Sys.time(), t1, units = "secs")
  cat("\n=====", m, "| start", round(t_start, 1), "s | load", round(t_load, 1), "s\n")
  cat("-- files:\n", txt(app, "#project-file_checklist"), "\n-- tools:\n", txt(app, "#project-tool_checklist"), "\n")
  app$get_screenshot(file.path(out_dir, paste0("launch_", m, ".png")))
  lg <- as.data.frame(app$get_logs()); bad <- lg[grepl("rror|arning", lg$message), "message"]
  cat("-- log errors/warnings:", length(bad), "\n"); if (length(bad)) cat(substr(unique(bad), 1, 200), sep = "\n")
  app$stop()
}
