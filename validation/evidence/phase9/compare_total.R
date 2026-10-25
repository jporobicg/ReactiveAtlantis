## Compare tab "total" biomass (nitro.weight with inf_box from boxes.prop) vs BiomIndx and the independent reference
args <- commandArgs(TRUE); run <- args[1]; ref_csv <- args[2]
.libPaths(c(path.expand("~/whacamole_nesp/ra_audit_data/lib_pr4"), .libPaths()))
source(path.expand("~/whacamole_nesp/ra_audit_data/scripts/ra_harness.R")); ra_source_modules()
grp <- read.csv(file.path(run, "NESP_groups.csv")); names(grp) <- tolower(names(grp)); grp <- grp[grp$isturnedon == 1, ]
dat <- list(paths = list(output_nc = file.path(run, "outputs", "output.nc"), bgm = file.path(run, "SEAP_NESP_final_xy_fixed.bgm")))
dat <- ensure_inf_box(dat); ib <- dat$inf_box
cat("inf_box: dummy?", identical(ib, dummy_inf_box(147, 7)), "; Vol dims", paste(dim(ib$Vol), collapse = "x"),
    "; boundary boxes (Depth_Bound==0):", paste(which(ib$info$Depth_Bound == 0) - 1, collapse = ","), "\n")
## volume check vs NetCDF volume at t0
v_nc <- ncdf4::ncvar_get(dat$nc_out, "volume", start = c(1, 1, 1), count = c(-1, -1, 1))
cat("box-total volume RA/NetCDF: median", signif(median(colSums(ib$Vol, na.rm = TRUE) / colSums(v_nc)), 4),
    "range", paste(signif(range(colSums(ib$Vol, na.rm = TRUE) / colSums(v_nc), na.rm = TRUE), 4), collapse = " - "), "\n")
ref <- read.csv(ref_csv); bi <- read.table(file.path(run, "outputs", "outputBiomIndx.txt"), header = TRUE)
out <- do.call(rbind, lapply(c("FTM","FTS","FPL","SHW","FMN","ZL","ZS","PL","BO","MA","SG","BFF"), function(code) {
  r <- tryCatch(nitro.weight(dat$nc_out, grp, code, By = "Total", box.info = ib, mg2t = 2e-8, x.cn = 5.7), error = function(e) NULL)
  b <- if (is.null(r)) NA else { x <- if (is.list(r)) (r$Bio %||% r$biomass %||% r[[1]]) else r; as.numeric(x)[1] }
  rr <- ref[ref$code == code & ref$t == 0, ]
  data.frame(code = code, kind = rr$kind[1], RA_compare_t0 = signif(b, 4), ref_t0 = signif(rr$ref_t[1], 4), BiomIndx_t0 = signif(rr$biomindx_t[1], 4), RA_over_ref = signif(b / rr$ref_t[1], 4))
}))
print(out, row.names = FALSE)
