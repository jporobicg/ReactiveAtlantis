## D15: RA DietCheck pre-aggregation on NESP: per-predator diet proportions after summing cohorts
args <- commandArgs(TRUE); run <- args[1]
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
raw <- data.table::fread(file.path(run, "outputs", "outputDietCheck.txt"))
cat("raw: rows", nrow(raw), "; per (predator,cohort,time) proportions sum to:", paste(signif(range(rowSums(raw[, -(1:5)])[rowSums(raw[, -(1:5)]) > 0]), 4), collapse = " - "), "\n")
agg <- preaggregate_diet(raw)$diet_time
prey <- setdiff(names(agg), c("Time", "Predator", "Stock"))
s <- agg[, .(sum_prop = rowSums(.SD)), .SDcols = prey, by = .(Time, Predator)]
for (p in c("FTM", "FTS", "FPL", "SHW", "ZL")) {
  x <- s[Predator == p & Time == 930]
  sub <- raw[Predator == p & Time == 930]; ncoh <- sum(rowSums(sub[, -(1:5)]) > 0)
  cat(sprintf("%s t=930: RA diet proportions summed over prey = %.3f (cohorts with diet: %d)\n", p, x$sum_prop, ncoh))
}
