## Schoener's D on controlled distributions (synthetic, hand-computed expectations)
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
mk <- function(v, t = 1L) data.frame(Time = t, Box = seq_along(v) - 1L, Value = v)
D <- function(a, b) spatial_overlap_index(mk(a), mk(b))$by_time$Schoener
cases <- list(
  "complete overlap, same shares (D=1)"          = list(c(1, 2, 3), c(10, 20, 30), 1),
  "no overlap (D=0)"                              = list(c(5, 0, 0), c(0, 3, 4), 0),
  "partial: p=(.5,.5,0) q=(0,.5,.5) (D=.5)"       = list(c(1, 1, 0), c(0, 1, 1), 0.5),
  "scale invariance: x1000 prey (D=.5)"           = list(c(1, 1, 0), c(0, 1000, 1000), 0.5),
  "predator zero everywhere (D undefined -> ?)"   = list(c(0, 0, 0), c(1, 2, 3), NA),
  "unequal shares p=(.9,.1) q=(.1,.9) (D=.2)"     = list(c(9, 1), c(1, 9), 0.2))
for (nm in names(cases)) { c <- cases[[nm]]; got <- D(c[[1]], c[[2]])
  cat(sprintf("%-46s expected %-4s got %-8s %s\n", nm, format(c[[3]]), format(signif(got, 6)),
      if (is.na(c[[3]])) "(check: undefined case)" else if (isTRUE(abs(got - c[[3]]) < 1e-12)) "PASS" else "FAIL")) }
## temporal: two time steps independent
two <- spatial_overlap_index(rbind(mk(c(1,1,0),1L), mk(c(1,0,0),2L)), rbind(mk(c(0,1,1),1L), mk(c(1,0,0),2L)))$by_time
cat("two time steps: D(t1)=", two$Schoener[1], " expected 0.5; D(t2)=", two$Schoener[2], " expected 1\n")
## local index scale dependence (documented caveat)
loc <- spatial_overlap_index(mk(c(1, 1)), mk(c(1000, 1)))$by_box$Value
cat("local index for pred (1,1) vs prey (1000,1): box0 =", signif(loc[1], 3), " box1 =", loc[2], " (shares: pred .5/.5, prey .999/.001)\n")
