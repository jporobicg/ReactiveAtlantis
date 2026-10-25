## D16: RA Consumption tab total (consumed_tonnes) vs realised intake = sum(Nums * Growth) / E from Atlantis output
args <- commandArgs(TRUE); run <- args[1]
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
grp <- read.csv(file.path(run, "NESP_groups.csv")); names(grp) <- tolower(names(grp))
prm <- readLines(file.path(run, "NESP_bio.prm"), warn = FALSE)
np <- nc_handle(file.path(run, "outputs", "outputPROD.nc")); no <- nc_handle(file.path(run, "outputs", "output.nc"))
for (code in c("FTM", "FTS", "FPL", "SHW")) {
  g <- grp[grp$code == code, ]; E <- as.numeric(strsplit(trimws(grep(paste0("^E_", code, "\\b"), prm, value = TRUE)), "[[:space:]]+")[[1]][2])
  ra <- consumed_tonnes(np, no, grp, code, prm = prm)$tonnes
  real <- sapply(c(2, 32, 62), function(ti) { s <- 0
    for (k in seq_len(g$numcohorts)) {
      N <- colSums(ncdf4::ncvar_get(no, paste0(g$name, k, "_Nums"), start = c(1, 1, ti), count = c(-1, -1, 1)))
      G <- ncdf4::ncvar_get(np, paste0(g$name, k, "_Growth"), start = c(1, ti), count = c(-1, 1)); s <- s + sum(N * G) }
    s / E * 5.7 * 2e-8 })
  cat(sprintf("%s E=%.2f | RA consumed_tonnes t=2,32,62: %s | realised Sum(N*Growth)/E t/day: %s | ratio %s\n", code, E,
      paste(signif(ra[c(2, 32, 62)], 3), collapse = ", "), paste(signif(real, 3), collapse = ", "), paste(signif(ra[c(2, 32, 62)] / real, 3), collapse = ", ")))
}
