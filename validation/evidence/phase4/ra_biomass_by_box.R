## RA biomass_by_box for every group of a run -> CSV (Time index 1-based, Box 0-based, tonnes)
args <- commandArgs(TRUE); run <- args[1]; out <- args[2]
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
grp <- read.csv(file.path(run, "NESP_groups.csv")); names(grp) <- tolower(names(grp)); grp <- grp[grp$isturnedon == 1, ]
nc <- nc_handle(file.path(run, "outputs", "output.nc"))
nt <- nc$dim$t$len; keep <- c(1, nt %/% 2 + 1, nt)
res <- do.call(rbind, lapply(grp$code, function(code) {
  d <- tryCatch(biomass_by_box(nc, grp, code), error = function(e) {message(code, ": ", conditionMessage(e)); NULL})
  if (is.null(d) || !nrow(d)) return(NULL); d[d$Time %in% keep, ]
}))
write.csv(res, out, row.names = FALSE); cat("rows", nrow(res), "groups", length(unique(res$Code)), "\n")
