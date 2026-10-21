## Shared synthetic Atlantis run used by unit tests and shinytest2 e2e.

ra_pkg_root <- function() {
  cand <- c(getwd(), file.path(getwd(), "..", ".."), file.path(getwd(), ".."))
  for (p in cand) {
    if (file.exists(file.path(p, "inst", "shiny", "app.R"))) {
      return(normalizePath(p))
    }
  }
  stop("Cannot find package root")
}

ra_fill3 <- function(nc, name, v1, v2, v3) {
  arr <- array(NA_real_, dim = c(2, 3, 3))
  arr[, , 1] <- v1
  arr[, , 2] <- v2
  arr[, , 3] <- v3
  ncdf4::ncvar_put(nc, name, arr)
}

write_synthetic_biology_nc <- function(path) {
  dim_z <- ncdf4::ncdim_def("z", "layer", 1:2)
  dim_b <- ncdf4::ncdim_def("b", "box", 1:3)
  t_vals <- c(0, 365, 730)
  dim_t <- ncdf4::ncdim_def("t", "days since 1970-01-01", t_vals)
  mk <- function(name, units = "mg N m-3") {
    ncdf4::ncvar_def(name, units, list(dim_z, dim_b, dim_t), -999, prec = "float")
  }
  vars <- list(
    mk("cod1_StructN"), mk("cod1_ResN"), mk("cod1_Nums", "individuals"),
    mk("cod2_StructN"), mk("cod2_ResN"), mk("cod2_Nums", "individuals"),
    mk("diatoms_N"), mk("Temp", "degC"), mk("salt", "PSU"),
    mk("NO3"), mk("NH3"), mk("Chl_a"), mk("Denitrificatio"),
    mk("Light", "W m-2"), mk("eddy", "1"),
    mk("volume", "m3"),
    ncdf4::ncvar_def("nominal_dz", "m", list(dim_z, dim_b), -999, prec = "float")
  )
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "days since 1970-01-01")
  ra_fill3(nc, "cod1_StructN", 10, 15, 20)
  ra_fill3(nc, "cod1_ResN", 5, 5, 5)
  ra_fill3(nc, "cod1_Nums", 100, 100, 100)
  ra_fill3(nc, "cod2_StructN", 8, 8, 8)
  ra_fill3(nc, "cod2_ResN", 4, 4, 4)
  ra_fill3(nc, "cod2_Nums", 50, 50, 50)
  ra_fill3(nc, "diatoms_N", 2, 2, 2)
  ra_fill3(nc, "Temp", 8, 10, 12)
  ra_fill3(nc, "salt", 34, 34, 34)
  ra_fill3(nc, "NO3", 1, 1.1, 1.2)
  ra_fill3(nc, "NH3", 0.2, 0.2, 0.2)
  ra_fill3(nc, "Chl_a", 0.5, 0.6, 0.4)
  ra_fill3(nc, "Denitrificatio", 0.01, 0.01, 0.01)
  ra_fill3(nc, "Light", 40, 50, 45)
  ra_fill3(nc, "eddy", 1, 1, 1)
  ra_fill3(nc, "volume", 1e6, 1e6, 1e6)
  ncdf4::ncvar_put(nc, "nominal_dz", array(c(10, 40), dim = c(2, 3)))
  ncdf4::nc_close(nc)
}

write_synthetic_prod_nc <- function(path) {
  dim_z <- ncdf4::ncdim_def("z", "layer", 1:2)
  dim_b <- ncdf4::ncdim_def("b", "box", 1:3)
  t_vals <- c(0, 365, 730)
  dim_t <- ncdf4::ncdim_def("t", "days since 1970-01-01", t_vals)
  mk <- function(name, units = "mg N m-3 d-1") {
    ncdf4::ncvar_def(name, units, list(dim_z, dim_b, dim_t), -999, prec = "float")
  }
  vars <- list(
    mk("cod1_Eat"), mk("cod2_Eat"),
    mk("cod1_Growth"), mk("cod2_Growth"),
    mk("diatomsGrazing"), mk("diatomsProdn"),
    mk("volume", "m3"),
    ncdf4::ncvar_def("nominal_dz", "m", list(dim_z, dim_b), -999, prec = "float")
  )
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "days since 1970-01-01")
  ra_fill3(nc, "cod1_Eat", 1, 1, 1)
  ra_fill3(nc, "cod2_Eat", 0.5, 0.5, 0.5)
  ra_fill3(nc, "cod1_Growth", 0.2, 0.2, 0.2)
  ra_fill3(nc, "cod2_Growth", 0.1, 0.1, 0.1)
  ra_fill3(nc, "diatomsGrazing", 0.4, 0.4, 0.4)
  ra_fill3(nc, "diatomsProdn", 0.3, 0.3, 0.3)
  ra_fill3(nc, "volume", 2e6, 2e6, 2e6)
  ncdf4::ncvar_put(nc, "nominal_dz", array(c(10, 40), dim = c(2, 3)))
  ncdf4::nc_close(nc)
}

write_synthetic_catch_nc <- function(path) {
  dim_b <- ncdf4::ncdim_def("b", "count", 1:3)
  t_vals <- (0:2) * 86400
  dim_t <- ncdf4::ncdim_def("t", "seconds since 1970-01-01", t_vals)
  mk <- function(name) {
    ncdf4::ncvar_def(name, "tons", list(dim_b, dim_t), -999, prec = "float")
  }
  vars <- list(
    mk("COD_Catch_FC1"), mk("COD_Catch_FC12"),
    mk("cod1_Catch"), mk("cod1_Num"), mk("cod2_Catch"), mk("cod2_Num")
  )
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "seconds since 1970-01-01")
  put <- function(name, val) ncdf4::ncvar_put(nc, name, array(val, dim = c(3, 3)))
  put("COD_Catch_FC1", 10)
  put("COD_Catch_FC12", 2)
  put("cod1_Catch", 4)
  put("cod1_Num", 400)
  put("cod2_Catch", 3)
  put("cod2_Num", 300)
  ncdf4::nc_close(nc)
}

##' Larger NetCDF for cache/timing tests (still small enough for CI)
write_timing_biology_nc <- function(path, n_z = 7, n_b = 30, n_t = 40,
                                    n_grp = 4, n_coh = 5) {
  dim_z <- ncdf4::ncdim_def("z", "layer", seq_len(n_z))
  dim_b <- ncdf4::ncdim_def("b", "box", seq_len(n_b))
  t_vals <- (seq_len(n_t) - 1) * 365
  dim_t <- ncdf4::ncdim_def("t", "days since 1965-01-01", t_vals)
  mk <- function(name, units = "mg N m-3") {
    ncdf4::ncvar_def(name, units, list(dim_z, dim_b, dim_t), -999, prec = "float")
  }
  vars <- list(mk("volume", "m3"), mk("Temp", "degC"), mk("Light", "W m-2"))
  grp_names <- sprintf("g%02d", seq_len(n_grp))
  for (nm in grp_names) {
    for (coh in seq_len(n_coh)) {
      vars <- c(vars, list(
        mk(paste0(nm, coh, "_StructN")),
        mk(paste0(nm, coh, "_ResN")),
        mk(paste0(nm, coh, "_Nums"), "individuals")
      ))
    }
  }
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "days since 1965-01-01")
  fill <- array(1, dim = c(n_z, n_b, n_t))
  fill[, , n_t] <- 1.2
  ncdf4::ncvar_put(nc, "volume", array(1e6, dim = c(n_z, n_b, n_t)))
  ncdf4::ncvar_put(nc, "Temp", fill)
  ncdf4::ncvar_put(nc, "Light", fill * 40)
  for (nm in grp_names) {
    for (coh in seq_len(n_coh)) {
      ncdf4::ncvar_put(nc, paste0(nm, coh, "_StructN"), fill * (10 + coh))
      ncdf4::ncvar_put(nc, paste0(nm, coh, "_ResN"), fill * 5)
      ncdf4::ncvar_put(nc, paste0(nm, coh, "_Nums"), fill * 80)
    }
  }
  ncdf4::nc_close(nc)
  invisible(path)
}

##' Write a complete tiny Atlantis output folder for browser tests
write_synthetic_atlantis_run <- function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  write.csv(
    data.frame(
      Code = c("COD", "PHY"),
      Name = c("cod", "diatoms"),
      LongName = c("Cod", "Phytoplankton"),
      NumCohorts = c(2, 1),
      IsImpacted = c(1, 0),
      IsTurnedOn = c(1, 1),
      IsPredator = c(1, 0),
      GroupType = c("FISH", "LG_PHY"),
      stringsAsFactors = FALSE
    ),
    file.path(dir, "groups.csv"), row.names = FALSE
  )
  write.csv(
    data.frame(Index = c(1, 12), Code = c("DEM", "PEL"),
               Name = c("demersal", "pelagic"), stringsAsFactors = FALSE),
    file.path(dir, "fisheries.csv"), row.names = FALSE
  )
  write_synthetic_biology_nc(file.path(dir, "output.nc"))
  write_synthetic_biology_nc(file.path(dir, "init.nc"))
  write_synthetic_prod_nc(file.path(dir, "outputPROD.nc"))
  write_synthetic_catch_nc(file.path(dir, "outputCATCH.nc"))
  writeLines(c(
    "nbox 3",
    "box0.vert 4", "0 0", "1 0", "1 1", "0 1",
    "box0.ibox 1 2 -1", "box0.botz -50", "box0.area 1",
    "box1.vert 4", "1 0", "2 0", "2 1", "1 1",
    "box1.ibox 0 2", "box1.botz -20", "box1.area 1",
    "box2.vert 4", "2 0", "3 0", "3 1", "2 1",
    "box2.ibox 0 1", "box2.botz 0", "box2.area 1"
  ), file.path(dir, "geo.bgm"))
  utils::write.table(
    data.frame(Time = c(0, 365, 730), COD = c(100, 80, 70), PHY = c(50, 55, 52)),
    file.path(dir, "BiomIndx.txt"), row.names = FALSE, quote = FALSE
  )
  diet <- data.frame(
    Time = rep(c(0, 365, 730), each = 2),
    Predator = "COD",
    Cohort = rep(1:2, times = 3),
    Stock = 0,
    COD = c(0.2, 0.1, 0.25, 0.15, 0.3, 0.2),
    PHY = c(0.8, 0.9, 0.75, 0.85, 0.7, 0.8)
  )
  utils::write.table(diet, file.path(dir, "DietCheck.txt"), row.names = FALSE, quote = FALSE)
  utils::write.table(
    data.frame(Time = c(0, 365, 730), COD.0 = c(20, 22, 18)),
    file.path(dir, "YOY.txt"), row.names = FALSE, quote = FALSE
  )
  utils::write.table(
    data.frame(Time = c(0, 365, 730), COD = c(80, 70, 60)),
    file.path(dir, "SSB.txt"), row.names = FALSE, quote = FALSE
  )
  mort <- data.frame(
    Time = c(0, 365, 730),
    COD.0.S0.F = 0.1, COD.1.S0.F = 0.12,
    COD.0.S0.M1 = 0.2, COD.1.S0.M1 = 0.22,
    COD.0.S0.M2 = 0.3, COD.1.S0.M2 = 0.28
  )
  utils::write.table(mort, file.path(dir, "SpecificMort.txt"), row.names = FALSE, quote = FALSE)
  utils::write.table(mort, file.path(dir, "SpecificPredMort.txt"), row.names = FALSE, quote = FALSE)
  writeLines(c(
    "flagnut 0",
    "KN_PHY 0.5",
    "KS_PHY 0.1",
    "KI_PHY 20",
    "eddy_scale 1",
    "flagrecruitCOD 3",
    "BHalpha_COD 7.3e9",
    "BHbeta_COD 4.5e11",
    "COD_Time_Spawn 0",
    "KSPA_COD 0",
    "FSP_COD 1",
    "KWSR_COD 0.07",
    "KWRR_COD 0.5",
    "X_CN 5.7",
    "X_RS 2.65",
    "FSPB_COD 2",
    "0 1",
    "KLP_COD 0.5",
    "KUP_COD 2.0",
    "pPREY1COD1 2",
    "0.1 0.8",
    "pPREY2COD1 2",
    "0.0 0.2"
  ), file.path(dir, "bio.prm"))
  writeLines(c("tinc 1", "toutinc 365", "flagrecruit 1"), file.path(dir, "run.prm"))
  write.csv(
    data.frame(
      Code = c("COD", "COD"),
      Year = c(1970, 1971),
      Value = c(100, 80),
      Lower = c(80, 60),
      Upper = c(120, 100),
      Type = c("Biomass", "Recruitment"),
      stringsAsFactors = FALSE
    ),
    file.path(dir, "observations.csv"), row.names = FALSE
  )
  invisible(dir)
}

##' Split a mixed Atlantis folder into config/ and output/ subfolders
split_atlantis_pair <- function(src, dest = NULL) {
  src <- normalizePath(src, winslash = "/", mustWork = TRUE)
  if (is.null(dest)) dest <- tempfile("ra_pair_")
  cfg <- file.path(dest, "config")
  out <- file.path(dest, "output")
  dir.create(cfg, recursive = TRUE, showWarnings = FALSE)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src, full.names = TRUE, recursive = FALSE)
  files <- files[!dir.exists(files)]
  is_config <- function(bn) {
    grepl("\\.(prm|bgm)$", bn, ignore.case = TRUE) ||
      grepl("group|fisher|init|_ini|observation", bn, ignore.case = TRUE) ||
      (grepl("\\.csv$", bn, ignore.case = TRUE) &&
         grepl("catch|obs", bn, ignore.case = TRUE))
  }
  is_output <- function(bn) {
    grepl("BiomIndx|DietCheck|SSB\\.txt|YOY\\.txt|SpecificMort|SpecificPred",
          bn, ignore.case = TRUE) ||
      grepl("CATCH|PROD", bn, ignore.case = TRUE) ||
      (grepl("\\.nc$", bn, ignore.case = TRUE) &&
         !grepl("init|_ini", bn, ignore.case = TRUE))
  }
  for (p in files) {
    bn <- basename(p)
    if (is_config(bn)) {
      file.copy(p, file.path(cfg, bn), overwrite = TRUE)
    }
    if (is_output(bn)) {
      file.copy(p, file.path(out, bn), overwrite = TRUE)
    }
  }
  list(root = dest, config_dir = cfg, output_dir = out)
}

##' Write a synthetic run already split into config/ and output/
write_synthetic_atlantis_pair <- function(dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  mixed <- file.path(dir, "_mixed")
  write_synthetic_atlantis_run(mixed)
  pair <- split_atlantis_pair(mixed, dir)
  unlink(mixed, recursive = TRUE)
  pair
}

##' NESP-style pair: config (groups/prm/init/bgm) separate from output tables
write_nesp_style_pair <- function(dir) {
  write_synthetic_atlantis_pair(dir)
}
