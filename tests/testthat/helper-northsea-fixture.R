## Build a trimmed North Sea fixture with on-disk names from ns_small.

northsea_fixture_dir <- function() {
  file.path(ra_pkg_root(), "tests", "testthat", "fixtures", "northsea")
}

northsea_source_dir <- function() {
  cand <- c(
    "/tmp/ns_small",
    file.path(Sys.getenv("HOME"), "ns_small"),
    getOption("reactiveatlantis.ns_small", "")
  )
  cand <- cand[nzchar(cand) & dir.exists(cand)]
  if (length(cand) == 0) return(NA_character_)
  cand[1]
}

trim_space_table <- function(src, dest, n_rows = 50L) {
  n_keep <- n_rows + 1L
  lines <- readLines(src, n = n_keep, warn = FALSE)
  writeLines(lines, dest)
  dest
}

write_northsea_biology_nc <- function(path, n_coh = 14L) {
  dim_z <- ncdf4::ncdim_def("z", "layer", 1:2)
  dim_b <- ncdf4::ncdim_def("b", "box", 1:3)
  t_vals <- c(0, 365, 730)
  dim_t <- ncdf4::ncdim_def("t", "days since 1970-01-01", t_vals)
  mk <- function(name, units = "mg N m-3") {
    ncdf4::ncvar_def(name, units, list(dim_z, dim_b, dim_t), -999, prec = "float")
  }
  vars <- list(
    mk("Temp", "degC"), mk("salt", "PSU"), mk("NO3"), mk("NH3"),
    mk("Si"), mk("Chl_a"), mk("Light", "W m-2"), mk("eddy", "1"),
    mk("volume", "m3"), mk("diatoms_N"), mk("other_phytoplankton_N"),
    ncdf4::ncvar_def("nominal_dz", "m", list(dim_z, dim_b), -999, prec = "float")
  )
  for (i in seq_len(n_coh)) {
    vars <- c(vars, list(
      mk(paste0("cod", i, "_StructN")),
      mk(paste0("cod", i, "_ResN")),
      mk(paste0("cod", i, "_Nums"), "individuals")
    ))
  }
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "days since 1970-01-01")
  ra_fill3(nc, "Temp", 8, 10, 12)
  ra_fill3(nc, "salt", 34, 34, 34)
  ra_fill3(nc, "NO3", 1, 1.1, 1.2)
  ra_fill3(nc, "NH3", 0.2, 0.2, 0.2)
  ra_fill3(nc, "Si", 2, 2, 2)
  ra_fill3(nc, "Chl_a", 0.5, 0.6, 0.4)
  ra_fill3(nc, "Light", 40, 50, 45)
  ra_fill3(nc, "eddy", 1, 1, 1)
  ra_fill3(nc, "volume", 1e6, 1e6, 1e6)
  ra_fill3(nc, "diatoms_N", 2, 2.1, 1.9)
  ra_fill3(nc, "other_phytoplankton_N", 1, 1, 1)
  ncdf4::ncvar_put(nc, "nominal_dz", array(c(10, 40), dim = c(2, 3)))
  for (i in seq_len(n_coh)) {
    ra_fill3(nc, paste0("cod", i, "_StructN"), 10 + i, 12 + i, 14 + i)
    ra_fill3(nc, paste0("cod", i, "_ResN"), 5, 5, 5)
    ra_fill3(nc, paste0("cod", i, "_Nums"), 80, 70, 60)
  }
  ncdf4::nc_close(nc)
}

write_northsea_prod_nc <- function(path, n_coh = 14L) {
  dim_z <- ncdf4::ncdim_def("z", "layer", 1:2)
  dim_b <- ncdf4::ncdim_def("b", "box", 1:3)
  t_vals <- c(0, 365, 730)
  dim_t <- ncdf4::ncdim_def("t", "days since 1970-01-01", t_vals)
  mk <- function(name, units = "mg N m-3 d-1") {
    ncdf4::ncvar_def(name, units, list(dim_z, dim_b, dim_t), -999, prec = "float")
  }
  vars <- list(
    mk("diatomsGrazing"), mk("diatomsProdn"),
    mk("volume", "m3"),
    ncdf4::ncvar_def("nominal_dz", "m", list(dim_z, dim_b), -999, prec = "float")
  )
  for (i in seq_len(n_coh)) {
    vars <- c(vars, list(mk(paste0("cod", i, "_Eat")), mk(paste0("cod", i, "_Growth"))))
  }
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "days since 1970-01-01")
  ra_fill3(nc, "diatomsGrazing", 0.4, 0.4, 0.4)
  ra_fill3(nc, "diatomsProdn", 0.3, 0.3, 0.3)
  ra_fill3(nc, "volume", 2e6, 2e6, 2e6)
  ncdf4::ncvar_put(nc, "nominal_dz", array(c(10, 40), dim = c(2, 3)))
  for (i in seq_len(n_coh)) {
    ra_fill3(nc, paste0("cod", i, "_Eat"), 1, 1, 1)
    ra_fill3(nc, paste0("cod", i, "_Growth"), 0.2, 0.2, 0.2)
  }
  ncdf4::nc_close(nc)
}

write_northsea_catch_nc <- function(path) {
  dim_b <- ncdf4::ncdim_def("b", "count", 1:3)
  t_vals <- (0:2) * 86400
  dim_t <- ncdf4::ncdim_def("t", "seconds since 1970-01-01", t_vals)
  mk <- function(name) {
    ncdf4::ncvar_def(name, "tons", list(dim_b, dim_t), -999, prec = "float")
  }
  vars <- list(
    mk("COD_Catch_FC1"), mk("cod1_Catch"), mk("cod1_Num"),
    mk("cod2_Catch"), mk("cod2_Num")
  )
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "seconds since 1970-01-01")
  put <- function(name, val) ncdf4::ncvar_put(nc, name, array(val, dim = c(3, 3)))
  put("COD_Catch_FC1", 10)
  put("cod1_Catch", 4)
  put("cod1_Num", 400)
  put("cod2_Catch", 3)
  put("cod2_Num", 300)
  ncdf4::nc_close(nc)
}

##' Copy trimmed real North Sea names into dest (default packaged fixture)
write_northsea_fixture <- function(dest = northsea_fixture_dir(),
                                   src = northsea_source_dir()) {
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  if (is.na(src) || !dir.exists(src)) {
    stop("North Sea source folder not found (expected /tmp/ns_small).")
  }
  copy_as <- function(name) {
    file.copy(file.path(src, name), file.path(dest, name), overwrite = TRUE)
  }
  copy_as("functionalGroups.csv")
  copy_as("NorthSeaFisheries.csv")
  copy_as("NorthSea.bgm")
  copy_as("059_C_PAD_T15_x10.prm")
  copy_as("NorthSea_run_fishing_F.prm")
  copy_as("NorthSea_force_fish.prm")
  trim_space_table(file.path(src, "outputNorthSeaBiomIndx.txt"),
                   file.path(dest, "outputNorthSeaBiomIndx.txt"), 50L)
  trim_space_table(file.path(src, "outputNorthSeaSSB.txt"),
                   file.path(dest, "outputNorthSeaSSB.txt"), 50L)
  trim_space_table(file.path(src, "outputNorthSeaYOY.txt"),
                   file.path(dest, "outputNorthSeaYOY.txt"), 50L)
  trim_space_table(file.path(src, "outputNorthSeaSpecificMort.txt"),
                   file.path(dest, "outputNorthSeaSpecificMort.txt"), 12L)
  diet <- data.frame(
    Time = rep(c(0, 365, 730), each = 2),
    Predator = "COD",
    Cohort = rep(1:2, times = 3),
    Stock = 0,
    COD = c(0.2, 0.1, 0.25, 0.15, 0.3, 0.2),
    HAD = c(0.3, 0.4, 0.25, 0.35, 0.2, 0.3),
    PHD = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  utils::write.table(diet, file.path(dest, "outputNorthSeaDietCheck.txt"),
                     row.names = FALSE, quote = FALSE)
  pred <- data.frame(
    Time = c(0, 365, 730),
    stringsAsFactors = FALSE
  )
  for (i in 0:13) {
    pred[[paste0("COD-", i, "-S0-M1")]] <- 0.2
    pred[[paste0("COD-", i, "-S0-M2")]] <- 0.3
    pred[[paste0("COD-", i, "-S0-F")]] <- 0.1
  }
  utils::write.table(pred, file.path(dest, "outputNorthSeaSpecificPredMort.txt"),
                     row.names = FALSE, quote = FALSE)
  write_northsea_biology_nc(file.path(dest, "outputNorthSea.nc"))
  write_northsea_biology_nc(file.path(dest, "init_NorthSea.nc"))
  write_northsea_prod_nc(file.path(dest, "outputNorthSeaPROD.nc"))
  write_northsea_catch_nc(file.path(dest, "outputNorthSeaCATCH.nc"))
  dest
}

##' Full-folder lookalike: duplicate BGM, extra MortPerPred / PredPropCheck, copy nc
write_messy_northsea_fixture <- function(dest) {
  src <- northsea_fixture_dir()
  if (!dir.exists(src) || !file.exists(file.path(src, "functionalGroups.csv"))) {
    stop("Packaged North Sea fixture is missing.")
  }
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  for (f in list.files(src, full.names = TRUE)) {
    file.copy(f, file.path(dest, basename(f)), overwrite = TRUE)
  }
  file.copy(file.path(dest, "NorthSea.bgm"),
            file.path(dest, "NorthSea (1).bgm"), overwrite = TRUE)
  if (file.exists(file.path(dest, "outputNorthSea.nc"))) {
    file.copy(file.path(dest, "outputNorthSea.nc"),
              file.path(dest, "outputNorthSea (1).nc"), overwrite = TRUE)
  }
  writeLines(c("Time Pred COD", "0 X 0.1"),
             file.path(dest, "outputNorthSeaMortPerPred.txt"))
  writeLines(c("Time Pred Prop", "0 COD 0.2"),
             file.path(dest, "outputNorthSeaPredPropCheck.txt"))
  dest
}
