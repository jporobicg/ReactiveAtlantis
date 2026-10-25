## Phase 6 setup: download CSIRO manuals and build the Guide index with the package's own functions (paths kept in ra_audit_data)
lib <- "~/whacamole_nesp/ra_audit_data/lib_pr4"; .libPaths(c(path.expand(lib), .libPaths()))
Sys.setenv(RA_AI_ENABLED = "true", RA_AI_CORPUS_DIR = path.expand("~/whacamole_nesp/ra_audit_data/ai/corpus"),
           RA_AI_INDEX_DIR = path.expand("~/whacamole_nesp/ra_audit_data/ai/index"),
           RA_AI_SOURCES_CONFIG = system.file("ai", "sources.atlantis-manuals.json", package = "ReactiveAtlantis"))
library(ReactiveAtlantis)
t0 <- Sys.time(); r1 <- try(download_atlantis_manuals()); cat("download:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n"); print(r1)
r1b <- try(download_atlantis_html_manual()); print(r1b)
print(list.files(Sys.getenv("RA_AI_CORPUS_DIR"), recursive = TRUE))
t0 <- Sys.time(); r2 <- try(index_ask_atlantis()); cat("index:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n"); print(r2)
print(try(diagnose_ask_atlantis_index()))
