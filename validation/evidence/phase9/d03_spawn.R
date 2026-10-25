## D03: RA spawn_from_nc vs Atlantis Ecology_Age_Structured_Spawn (atdemography.c:571-592), NESP FTM/FTB
args <- commandArgs(TRUE); run <- args[1]
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
grp <- read.csv(file.path(run, "NESP_groups.csv")); names(grp) <- tolower(names(grp))
prm <- readLines(file.path(run, "NESP_bio.prm"), warn = FALSE); nc <- nc_handle(file.path(run, "outputs", "output.nc"))
rp <- build_rec_params(grp, prm)
at_spawn <- function(SN, RN, FSP, KSPA, XRS) {          # per individual, Atlantis non-repcost branch
  WforS <- (1 + XRS) * SN; Wgt <- SN + RN
  s <- FSP * WforS - KSPA
  s <- s - ifelse(Wgt < WforS, WforS - Wgt, 0)
  s[s < 0] <- 0
  big <- s > RN; s[big] <- RN[big]                      # sanity cap (RN_buf assumed 1)
  s
}
for (code in c("FTM", "FTB")) {
  r <- rp$rec[rp$rec$FG == code, ]; fspb <- rp$fspb[[code]]
  ra <- spawn_from_nc(nc, grp, r, code, fspb)
  g <- grp[grp$code == code, ]; time <- nc_time_days(nc); idx <- spawn_times_idx(time, r$Time.sp)
  ref <- sapply(idx, function(ti) { tot <- 0
    for (coh in seq_len(g$numcohorts)) {
      N <- as.numeric(nc_get_at_t(nc, paste0(g$name, coh, "_Nums"), ti)); RN <- as.numeric(nc_get_at_t(nc, paste0(g$name, coh, "_ResN"), ti))
      SN <- as.numeric(nc_get_at_t(nc, paste0(g$name, coh, "_StructN"), ti)); k <- N > 1e-16
      tot <- tot + sum(fspb[coh] * at_spawn(SN[k], RN[k], r$FSP, r$KSPA, r$XRS) * N[k]) }
    tot })
  cat(sprintf("\n%s  FSP %.3f KSPA %.1f X_RS %.2f; spawn day %s\n", code, r$FSP, r$KSPA, r$XRS, paste(round(time[idx]), collapse = ",")))
  print(data.frame(day = round(ra$time), RA_spawn_mgN = signif(ra$spawn, 4), Atlantis_ref_mgN = signif(ref, 4), RA_over_ref = signif(ra$spawn / ref, 4)))
}
