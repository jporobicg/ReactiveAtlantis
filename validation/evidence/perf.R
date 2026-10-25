## Phase 7: timing, memory (RSS) and open file handles of the Shiny process over repeated interactions on NESP c3.
args <- commandArgs(TRUE); run <- args[1]; run2 <- args[2]; out <- args[3]
lib <- "~/whacamole_nesp/ra_audit_data/lib_pr4"; .libPaths(c(lib, .libPaths())); Sys.setenv(NOT_CRAN = "true")
library(shinytest2)
d <- file.path(tempdir(), "ra_app"); dir.create(d)
writeLines(c(sprintf(".libPaths(c('%s', .libPaths()))", path.expand(lib)),
  sprintf("options(reactiveatlantis.config_dir='%s', reactiveatlantis.output_dir='%s')", run, file.path(run, "outputs")),
  "shiny::shinyAppDir(system.file('shiny', package = 'ReactiveAtlantis'))"), file.path(d, "app.R"))
app <- AppDriver$new(d, width = 1600, height = 1200, load_timeout = 180000, timeout = 900000)
pid <- app$.__enclos_env__$private$shiny_process$get_pid()
rss <- function() as.numeric(sub("VmRSS:\\s+([0-9]+).*", "\\1", grep("^VmRSS", readLines(sprintf("/proc/%d/status", pid)), value = TRUE))) / 1024
nfd <- function() length(list.files(sprintf("/proc/%d/fd", pid)))
ncfd <- function() sum(grepl("\\.nc$", Sys.readlink(list.files(sprintf("/proc/%d/fd", pid), full.names = TRUE))))
log <- data.frame(); step <- function(lab, expr) { t0 <- Sys.time(); force(expr); app$wait_for_idle(timeout = 900000)
  r <- data.frame(step = lab, seconds = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), rss_mb = round(rss()), fds = nfd(), nc_fds = ncfd())
  log <<- rbind(log, r); print(r); write.csv(log, out, row.names = FALSE) }
step("app idle", NULL)
step("load c3", app$click("project-load_project"))
step("Spatial tab", app$set_inputs(ra_page = "Spatial", wait_ = FALSE))
grps <- c("FTM", "FTS", "FPL", "SHW", "FMN", "FMM", "ZL", "PL", "SQG", "FBL")
for (k in 1:3) for (g in grps) step(paste0("spatial fg ", g, " pass", k), app$set_inputs(`spatial-fg` = g, wait_ = FALSE))
for (k in 1:10) step(paste0("time slider ", k * 6), app$set_inputs(`spatial-time_i` = k * 6, wait_ = FALSE))
step("Compare tab", app$set_inputs(ra_page = "Compare", wait_ = FALSE))
step("Feeding pressure", { app$set_inputs(ra_page = "Feeding", wait_ = FALSE); app$wait_for_idle(timeout = 900000); app$set_inputs(`feeding-view_type` = "pressure", wait_ = FALSE) })
step("Feeding pressure again (cache?)", { app$set_inputs(`feeding-view_type` = "matrix", wait_ = FALSE); app$wait_for_idle(timeout = 900000); app$set_inputs(`feeding-view_type` = "pressure", wait_ = FALSE) })
for (k in 1:3) step(paste0("reload c3 #", k), app$click("project-load_project"))
step("switch to run c1 (same folders input)", { app$set_inputs(`project-run1_config_dir` = run2, `project-run1_output_dir` = file.path(run2, "outputs"), wait_ = FALSE); app$wait_for_idle(timeout = 60000); app$click("project-load_project") })
step("Spatial after switch", app$set_inputs(ra_page = "Spatial", wait_ = FALSE))
app$stop()
