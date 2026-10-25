## Expected predation pressure from Atlantis feeding equations.

find_pkg_root <- function() ra_pkg_root()

root <- find_pkg_root()
mod <- file.path(root, "inst", "shiny", "modules")

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(ncdf4)
})

source(file.path(mod, "theme.R"), local = TRUE)
source(file.path(mod, "ui_helpers.R"), local = TRUE)
source(file.path(mod, "utils.R"), local = TRUE)
source(file.path(mod, "project_store.R"), local = TRUE)
source(file.path(mod, "nc_cache.R"), local = TRUE)
source(file.path(mod, "analysis_helpers.R"), local = TRUE)
source(file.path(mod, "process_diagnostics.R"), local = TRUE)
source(file.path(mod, "feeding_module.R"), local = TRUE)
source(file.path(mod, "predation_pressure.R"), local = TRUE)

test_that("Holling Type II matches the User Guide / Murray-Parslow form", {
  ## I = C * A / (1 + C/mum * E * A); Gr = I * pred_scale
  C <- 0.1
  mum <- 0.05
  E <- 1
  A <- 2
  pred_scale <- 1000
  got <- holling_ingest_mgn_day(C, mum, A, E * A, pred_scale, predcase = 0)
  expect_equal(got, (C * A / (1 + C * E * A / mum)) * pred_scale)
})

test_that("Holling Type I is min(C A, mum/E)", {
  got <- holling_ingest_mgn_day(0.1, 0.05, 10, 1 * 10, 1, predcase = 1)
  expect_equal(got, 0.05)
  got_low <- holling_ingest_mgn_day(0.1, 0.05, 0.2, 1 * 0.2, 1, predcase = 1)
  expect_equal(got_low, 0.1 * 0.2)
})

test_that("Holling Type III squares availability", {
  type2 <- holling_ingest_mgn_day(0.1, 0.05, 2, 2, 1000, predcase = 0)
  type3 <- holling_ingest_mgn_day(0.1, 0.05, 2, 2, 1000, predcase = 2)
  expect_true(type3 != type2)
  A2 <- 4
  expect_equal(type3, (0.1 * A2 / (1 + 0.1 * 1 * A2 / 0.05)) * 1000)
})

test_that("zero gape or zero clearance gives zero ingestion", {
  expect_equal(holling_ingest_mgn_day(0, 0.05, 2, 2, 1000, 0), 0)
  expect_equal(gape_size_scalar(10, 1, 0.5, 2), 0)
  expect_equal(gape_size_scalar(10, 8, 0.5, 2), 1)
})

test_that("prm tokens read C/mum vectors and E scalars", {
  prm <- c(
    "flagfishrates 1",
    "C_COD 2",
    "0.02 0.04",
    "mum_COD 2",
    "0.05 0.08",
    "E_COD 0.5 comment",
    "EPlant_COD 0.01",
    "predcase_COD 0 0",
    "KLP_COD 0.5",
    "KUP_COD 2.0",
    "COD_age_mat 2"
  )
  expect_equal(read_prm_token(prm, "C_COD"), c(0.02, 0.04))
  expect_equal(read_prm_token(prm, "mum_COD"), c(0.05, 0.08))
  expect_equal(read_prm_scalar(prm, "E_COD"), 0.5)
  expect_equal(read_prm_scalar(prm, "predcase_COD"), 0)
  expect_equal(read_prm_scalar(prm, "flagfishrates"), 1)
})

test_that("pressure matrix keeps the pPREY layout and reports t/day and fraction", {
  tmp <- tempfile("press_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  write_synthetic_atlantis_run(tmp)
  grp <- utils::read.csv(file.path(tmp, "groups.csv"), stringsAsFactors = FALSE)
  names(grp) <- tolower(names(grp))
  prm <- readLines(file.path(tmp, "bio.prm"))
  ava <- read_pprey_matrix(prm)
  colnames(ava) <- as.character(grp$code)
  nc <- ncdf4::nc_open(file.path(tmp, "init.nc"))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  press <- expected_predation_pressure(ava, grp, prm, nc, box = NULL)
  expect_equal(dim(press$consumption_t_day), dim(ava))
  expect_equal(rownames(press$prey_loss_frac_day), rownames(ava))
  expect_equal(colnames(press$prey_loss_frac_day), colnames(ava))
  expect_true(all(is.finite(press$prey_loss_frac_day)))
  expect_true(all(press$consumption_t_day >= 0))
  ## Juvenile COD eating PHY (column 2): pPREY > 0
  expect_true(press$consumption_t_day["pPREY1COD1", "PHY"] > 0)
  expect_true(press$prey_loss_frac_day["pPREY1COD1", "PHY"] > 0)
  ## Adult prey row with pPREY = 0 on COD stays zero
  expect_equal(unname(press$consumption_t_day["pPREY2COD1", "COD"]), 0)
  one <- press$boxes_used
  expect_true(length(one) >= 1)
  box0 <- expected_predation_pressure(ava, grp, prm, nc, box = 0)
  expect_equal(box0$boxes_used, 0)
  expect_true(all(box0$consumption_t_day <= press$consumption_t_day + 1e-12))
})

test_that("hover and flags stay on the full matrix", {
  ava <- matrix(
    c(0.1, 0.8, 0, 0.2),
    nrow = 2, byrow = TRUE,
    dimnames = list(c("pPREY1COD1", "pPREY2COD1"), c("COD", "PHY"))
  )
  press <- list(
    consumption_t_day = ava * 2,
    prey_loss_frac_day = ava * 0.01,
    prey_loss_frac_year = ava * 3.65,
    gape = ava * 0 + 1,
    cells = data.frame(
      Predator_row = "pPREY1COD1", Pred_code = "COD", Pred_stage = "Juvenile",
      Prey = "PHY", Prey_stage = "Juvenile", pPREY = 0.8, Gape = 1,
      predcase = 0L, Consumption_t_day = 1.6, Prey_loss_frac_day = 0.008,
      Prey_loss_frac_year = 2.92, Prey_tonnes = 10, Flag = "OK",
      stringsAsFactors = FALSE
    )
  )
  hover <- pressure_hover_text(ava, press, NULL)
  expect_match(hover["pPREY1COD1", "PHY"], "Expected pressure")
  expect_match(hover["pPREY1COD1", "PHY"], "t / day")
  expect_equal(dim(hover), dim(ava))
  expect_equal(pressure_metric_matrix(press, "t_day")[1, 2], 1.6)
})

test_that("pair-by-box sums to the system-wide Holling cell", {
  tmp <- tempfile("press_box_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  write_synthetic_atlantis_run(tmp)
  grp <- utils::read.csv(file.path(tmp, "groups.csv"), stringsAsFactors = FALSE)
  names(grp) <- tolower(names(grp))
  prm <- readLines(file.path(tmp, "bio.prm"))
  ava <- read_pprey_matrix(prm)
  colnames(ava) <- as.character(grp$code)
  nc <- ncdf4::nc_open(file.path(tmp, "init.nc"))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  press <- expected_predation_pressure(ava, grp, prm, nc, box = NULL)
  by_box <- pressure_pair_by_box(
    ava, grp, prm, nc, "COD", "PHY",
    pred_stage = 1, prey_stage = 1, time_i = 1L,
    stocks = press$stocks
  )
  expect_equal(nrow(by_box), press$n_box)
  expect_equal(
    sum(by_box$Consumption_t_day),
    unname(press$consumption_t_day["pPREY1COD1", "PHY"]),
    tolerance = 1e-10
  )
  expect_true(all(by_box$Box == seq_len(nrow(by_box)) - 1L))
})

test_that("output time step changes expected consumption", {
  tmp <- tempfile("press_t_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  write_synthetic_atlantis_run(tmp)
  grp <- utils::read.csv(file.path(tmp, "groups.csv"), stringsAsFactors = FALSE)
  names(grp) <- tolower(names(grp))
  prm <- readLines(file.path(tmp, "bio.prm"))
  ava <- read_pprey_matrix(prm)
  colnames(ava) <- as.character(grp$code)
  nc <- ncdf4::nc_open(file.path(tmp, "output.nc"))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_equal(nc_n_times(nc), 3L)
  p1 <- expected_predation_pressure(ava, grp, prm, nc, time_i = 1L)
  p3 <- expected_predation_pressure(ava, grp, prm, nc, time_i = 3L)
  expect_true(p3$consumption_t_day["pPREY1COD1", "PHY"] !=
                p1$consumption_t_day["pPREY1COD1", "PHY"])
})

test_that("realised compare and production flags use DietCheck and SpecificPredMort", {
  tmp <- tempfile("press_real_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  write_synthetic_atlantis_run(tmp)
  grp <- utils::read.csv(file.path(tmp, "groups.csv"), stringsAsFactors = FALSE)
  names(grp) <- tolower(names(grp))
  prm <- readLines(file.path(tmp, "bio.prm"))
  ava <- read_pprey_matrix(prm)
  colnames(ava) <- as.character(grp$code)
  nc <- ncdf4::nc_open(file.path(tmp, "init.nc"))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  press <- expected_predation_pressure(ava, grp, prm, nc)
  diet <- utils::read.table(file.path(tmp, "DietCheck.txt"), header = TRUE)
  pred_mort <- utils::read.table(file.path(tmp, "SpecificPredMort.txt"),
                                 header = TRUE)
  nc_prod <- ncdf4::nc_open(file.path(tmp, "outputPROD.nc"))
  on.exit(ncdf4::nc_close(nc_prod), add = TRUE)
  prod <- prey_production_t_day(nc_prod, nc, grp, "PHY", time_i = 1L)
  expect_true(is.finite(prod) && prod > 0)
  cmp <- pressure_vs_realised(press, diet, pred_mort, "COD", "PHY", prod)
  expect_true("DietCheck mean proportion" %in% cmp$Source)
  expect_true("SpecificPredMort mean (fraction per output period)" %in% cmp$Source)
  expect_true("Atlantis realised intake of predator (t/day)" %in% cmp$Source)
  expect_true(is.finite(cmp$Value[cmp$Source == "DietCheck mean proportion"]))
  expect_true(is.finite(cmp$Value[cmp$Source == "SpecificPredMort mean (fraction per output period)"]))
  expect_equal(cmp$Value[cmp$Source == "Prey production (t/day)"], prod)
  flagged <- pressure_flag_vs_production(press$cells, "PHY", 0)
  expect_true(any(flagged$Flag == "loss > prey production / day"))
  poly <- read_bgm_polygons(file.path(tmp, "geo.bgm"))
  by_box <- pressure_pair_by_box(
    ava, grp, prm, nc, "COD", "PHY", pred_stage = 1, prey_stage = 1,
    stocks = press$stocks
  )
  mapped <- pressure_map_polygons(poly, by_box, "frac_day")
  expect_true("Value" %in% names(mapped))
  expect_true(any(is.finite(mapped$Value)))
})
