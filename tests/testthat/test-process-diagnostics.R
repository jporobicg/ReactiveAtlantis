## Mortality by predator, recruits vs SSB, realised diet, spatial overlap.

find_pkg_root <- function() ra_pkg_root()

root <- find_pkg_root()
mod <- file.path(root, "inst", "shiny", "modules")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
})

source(file.path(mod, "theme.R"), local = TRUE)
source(file.path(mod, "ui_helpers.R"), local = TRUE)
source(file.path(mod, "colors.R"), local = TRUE)
source(file.path(mod, "utils.R"), local = TRUE)
source(file.path(mod, "catch_module.R"), local = TRUE)
source(file.path(mod, "project_store.R"), local = TRUE)
source(file.path(mod, "analysis_helpers.R"), local = TRUE)
source(file.path(mod, "process_diagnostics.R"), local = TRUE)
source(file.path(mod, "predation_pressure.R"), local = TRUE)
source(file.path(mod, "mortality_module.R"), local = TRUE)
source(file.path(mod, "recruitment_module.R"), local = TRUE)
source(file.path(mod, "feeding_module.R"), local = TRUE)
source(file.path(mod, "spatial_module.R"), local = TRUE)
source(file.path(mod, "predation_module.R"), local = TRUE)
source(file.path(mod, "nc_cache.R"), local = TRUE)

test_that("SpecificPredMort rows are prey and columns are predators (Atlantis layout)", {
  ## Regression for D01. Atlantis writes Time, Group, Cohort, Stock, then one
  ## column per predator; Group/Cohort are the prey (atbiolmessage.c,
  ## calcTrackedPredMort[prey][cohort][stock][pred]). Shape of the NESP case:
  ## southern bluefin tuna (FTM) are eaten by white sharks (SHW) and a little
  ## by FVL; FTM itself eats sardine (FPS).
  pred <- data.frame(
    Time = c(0, 0, 0, 365, 365, 365),
    Group = c("FTM", "FTM", "FPS", "FTM", "FTM", "FPS"),
    Cohort = c(0, 1, 0, 0, 1, 0),
    Stock = 0,
    SHW = c(0.30, 0.10, 0, 0.32, 0.12, 0),
    FVL = c(0.01, 0, 0, 0.02, 0, 0),
    FTM = c(0, 0, 0.40, 0, 0, 0.45),
    stringsAsFactors = FALSE
  )
  on_ftm <- parse_pred_mort_by_predator(pred, "FTM")
  on_ftm <- on_ftm[on_ftm$Value > 0, ]
  expect_setequal(unique(on_ftm$Predator), c("SHW", "FVL"))
  expect_equal(unique(on_ftm$Prey), "FTM")
  expect_equal(sum(on_ftm$Value[on_ftm$Predator == "SHW"]), 0.30 + 0.10 + 0.32 + 0.12)
  expect_equal(sort(unique(on_ftm$Age)), c("Age01", "Age02"))
  on_fps <- parse_pred_mort_by_predator(pred, "FPS")
  on_fps <- on_fps[on_fps$Value > 0, ]
  expect_equal(unique(on_fps$Predator), "FTM")
  expect_equal(sum(on_fps$Value), 0.85)
  only_shw <- parse_pred_mort_by_predator(pred, "FTM", pred_code = "SHW")
  expect_equal(unique(only_shw$Predator), "SHW")
  none <- parse_pred_mort_by_predator(pred, "HAD")
  expect_equal(nrow(none), 0)
  expect_true(grepl("No predation", attr(none, "reason")))
})

test_that("SpecificMort-style PredMort explains why predators cannot be split", {
  wide <- data.frame(Time = 1:2, COD.0.S0.M2 = 0.3, COD.0.S0.F = 0.1)
  out <- parse_pred_mort_by_predator(wide, "COD")
  expect_equal(nrow(out), 0)
  expect_true(grepl("SpecificMort-style", attr(out, "reason"), fixed = TRUE))
  st <- mortality_file_status(
    list(specific_mort = NA, specific_pred_mort = tempfile(), mort = NA),
    list(pred_mort = wide)
  )
  expect_true(st$present[st$role == "specific_pred_mort"])
  expect_true(grepl("SpecificMort-style", st$note[st$role == "specific_pred_mort"]))
  expect_true(grepl("Mort.txt", mortality_missing_message(st, "mort")))
})

test_that("Mort.txt splits relative F and M", {
  mort <- data.frame(Time = c(0, 365), COD.M = c(0.4, 0.42), COD.F = c(0.1, 0.11))
  long <- parse_mort_txt(mort, "COD")
  expect_equal(nrow(long), 4)
  expect_equal(sort(unique(long$Source)), c("Fishing", "Natural"))
  miss <- parse_mort_txt(mort, "HAD")
  expect_equal(nrow(miss), 0)
})

test_that("YOY and SSB join for recruits vs spawning stock", {
  yoy <- data.frame(Time = c(0, 365, 730), COD.0 = c(20, 22, 18))
  ssb <- data.frame(Time = c(0, 365, 730), COD = c(80, 70, 60))
  df <- join_recruits_ssb(yoy, ssb, "COD")
  expect_equal(nrow(df), 3)
  expect_equal(df$Recruits, c(20, 22, 18))
  expect_equal(df$SSB, c(80, 70, 60))
  empty <- join_recruits_ssb(NULL, ssb, "COD")
  expect_equal(nrow(empty), 0)
  expect_true(grepl("YOY.txt", attr(empty, "reason")))
})

test_that("DietCheck and consumed tonnes follow the selected pair", {
  diet <- data.frame(
    Time = c(0, 365, 730),
    Predator = "COD",
    PHY = c(0.8, 0.7, 0.6),
    COD = c(0.2, 0.3, 0.4)
  )
  prop <- dietcheck_pair_series(diet, "COD", "PHY")
  expect_equal(prop$Proportion, c(0.8, 0.7, 0.6))
  cons <- consumed_pair_series(c(10, 20, 30), diet, "COD", "PHY")
  expect_equal(cons$Tonnes, c(8, 14, 18))
  miss <- dietcheck_pair_series(diet, "HAD", "PHY")
  expect_equal(nrow(miss), 0)
})

test_that("consumption uses Atlantis intake Nums x Growth / E per cohort (D16, D15)", {
  ## Two cohorts, two boxes, two times. Hand-computed reference.
  ## Cohort 1: Nums box0 = 10, box1 = 30; Growth 2 and 4 mg N/ind/day
  ## Cohort 2: Nums box0 = 5,  box1 = 0;  Growth 10 and 10
  ## E = 0.5 -> intake mg N/day: c1 (10*2 + 30*4)/0.5 = 280; c2 (5*10)/0.5 = 100
  tmp <- tempfile("intake_"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dz <- ncdf4::ncdim_def("z", "", 1:1); db <- ncdf4::ncdim_def("b", "", 1:2)
  dt <- ncdf4::ncdim_def("t", "seconds since 2000-01-01", c(0, 86400 * 30))
  vo <- lapply(1:2, function(c) ncdf4::ncvar_def(paste0("Fish", c, "_Nums"), "", list(dz, db, dt)))
  nco <- ncdf4::nc_create(file.path(tmp, "out.nc"), vo)
  ncdf4::ncvar_put(nco, "Fish1_Nums", array(c(10, 30, 10, 30), c(1, 2, 2)))
  ncdf4::ncvar_put(nco, "Fish2_Nums", array(c(5, 0, 5, 0), c(1, 2, 2)))
  ncdf4::nc_close(nco)
  vp <- lapply(1:2, function(c) ncdf4::ncvar_def(paste0("Fish", c, "_Growth"), "mg N d-1", list(db, dt)))
  ncp <- ncdf4::nc_create(file.path(tmp, "prod.nc"), vp)
  ncdf4::ncvar_put(ncp, "Fish1_Growth", matrix(c(2, 4, 2, 4), 2))
  ncdf4::ncvar_put(ncp, "Fish2_Growth", matrix(c(10, 10, 10, 10), 2))
  ncdf4::nc_close(ncp)
  nco <- ncdf4::nc_open(file.path(tmp, "out.nc")); ncp <- ncdf4::nc_open(file.path(tmp, "prod.nc"))
  on.exit({ ncdf4::nc_close(nco); ncdf4::nc_close(ncp) }, add = TRUE)
  grp <- data.frame(code = "FSH", name = "Fish", numcohorts = 2, grouptype = "FISH")
  k <- 5.7 * 0.00000002
  coh <- realised_intake_by_cohort(ncp, nco, grp, "E_FSH 0.5", "FSH")
  expect_equal(coh$Time, c(0, 30, 0, 30))
  expect_equal(coh$Intake[coh$Cohort == 0], c(280, 280) * k)
  expect_equal(coh$Intake[coh$Cohort == 1], c(100, 100) * k)
  tot <- consumed_tonnes(ncp, nco, grp, "FSH", prm = "E_FSH 0.5")
  expect_equal(tot$tonnes, c(380, 380) * k)
  ## Per-cohort diet: cohort 0 eats 100 % ZOO; cohort 1 eats 50 % ZOO, 50 % SQD.
  ## Intake-weighted: ZOO = 280 + 50 = 330, SQD = 50 (not 1.5 x 380 from summing).
  diet_full <- data.frame(Time = c(0, 0, 30, 30), Predator = "FSH", Stock = 0,
                          Cohort = c(0, 1, 0, 1), ZOO = c(1, 0.5, 1, 0.5), SQD = c(0, 0.5, 0, 0.5))
  by <- consumption_by_prey_cohort(coh, diet_full, "FSH")
  expect_equal(by$Tonnes[by$Prey == "ZOO" & by$Time == 0], 330 * k)
  expect_equal(by$Tonnes[by$Prey == "SQD" & by$Time == 30], 50 * k)
  expect_equal(sum(by$Tonnes[by$Time == 0]), 380 * k)
})

test_that("predator diet shares sum to 1 after cohort aggregation (D15)", {
  raw <- data.frame(Time = 30, Predator = "FSH", Cohort = c(0, 1, 2), Stock = 0, Updated = 0,
                    ZOO = c(1, 0.5, 0), SQD = c(0, 0.5, 0))
  agg <- preaggregate_diet(raw)$diet_time
  expect_equal(agg$ZOO + agg$SQD, 1)
  expect_equal(agg$ZOO, 0.75)
  expect_false("Updated" %in% names(agg))
})

test_that("health flags: exact thresholds and undefined initial values (D22)", {
  first <- c(A = 100, B = 100, C = 100, D = 100, E = 0, F = 0, G = NA, I = Inf, J = 100, K = -5)
  last  <- c(A = 1,   B = 0.999, C = 1000, D = 1000.001, E = 0, F = 5, G = 50, I = 10, J = 0, K = 5)
  bi <- rbind(data.frame(Time = 0, t(first)), data.frame(Time = 365, t(last)))
  grp <- data.frame(code = names(first), longname = names(first), grouptype = "FISH")
  h <- biomass_health(bi, grp)
  f <- setNames(h$Flag, h$Code)
  expect_equal(f[["A"]], "OK")                                   # exactly 1 %
  expect_equal(f[["B"]], "Collapsed (< 1% of initial)")
  expect_equal(f[["C"]], "OK")                                   # exactly 10x
  expect_equal(f[["D"]], "Increased (> 10\u00d7 initial)")
  expect_equal(f[["J"]], "Collapsed (< 1% of initial)")
  for (k in c("E", "F", "G", "I", "K")) expect_true(startsWith(f[[k]], "Undefined"), info = k)
  expect_true(all(is.na(h$Ratio[h$Code %in% c("E", "F", "G", "I", "K")])))
})

test_that("individual spawn follows Atlantis (deficit subtracted, cap at RN) (D03)", {
  ## Ecology_Age_Structured_Spawn, atdemography.c:571-592. X_RS 2.65 so
  ## WforS = 3.65 SN. Hand values with SN = 10 (WforS = 36.5):
  ## well fed RN 30: 0.5*36.5 - 1 = 17.25 (no deficit)
  ## thin RN 20 (weight 30, deficit 6.5): 17.25 - 6.5 = 10.75
  ## very thin RN 5 (deficit 21.5): below 0 -> 0
  ## cap: FSP 1, KSPA 0, RN 2 (deficit 24.5): 36.5 - 24.5 = 12 > RN -> 2
  sp <- atlantis_ind_spawn(SN = c(10, 10, 10), RN = c(30, 20, 5), FSP = 0.5, KSPA = 1, X_RS = 2.65)
  expect_equal(sp, c(17.25, 10.75, 0))
  expect_equal(atlantis_ind_spawn(10, 2, 1, 0, 2.65), 2)
})

test_that("spatial overlap is Schoener D and local 0-1 per box", {
  pred <- data.frame(Time = c(1, 1, 2, 2), Box = c(0, 1, 0, 1),
                     Biomass = c(10, 0, 5, 5))
  prey <- data.frame(Time = c(1, 1, 2, 2), Box = c(0, 1, 0, 1),
                     Biomass = c(10, 0, 0, 10))
  ov <- spatial_overlap_index(pred, prey)
  t1 <- ov$by_box[ov$by_box$Time == 1, ]
  expect_equal(t1$Value[t1$Box == 0], 1)
  expect_equal(t1$Value[t1$Box == 1], 0)
  expect_equal(ov$by_time$Schoener[ov$by_time$Time == 1], 1)
  expect_equal(ov$by_time$Schoener[ov$by_time$Time == 2], 0.5)
  none <- spatial_overlap_index(NULL, prey)
  expect_equal(nrow(none$by_box), 0)
})

test_that("North Sea / synthetic fixtures load the new mortality tables", {
  run_dir <- tempfile("diag_syn_")
  dir.create(run_dir)
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
  write_synthetic_atlantis_run(run_dir)
  found <- detect_project_files(run_dir)
  expect_true(path_ok(found$mort))
  expect_true(path_ok(found$specific_pred_mort))
  loaded <- load_project(found)
  expect_true(pred_mort_is_trunk(loaded$pred_mort))
  long <- parse_pred_mort_by_predator(loaded$pred_mort, "PHY")
  expect_gt(nrow(long), 0)
  rec <- join_recruits_ssb(loaded$yoy, loaded$ssb, "COD")
  expect_gt(nrow(rec), 0)
  fx <- northsea_fixture_dir()
  skip_if_not(dir.exists(fx) && file.exists(file.path(fx, "functionalGroups.csv")),
              "North Sea fixture not packaged")
  ns <- detect_project_files(fx)
  expect_true(path_ok(ns$specific_mort))
  expect_true(path_ok(ns$mort) || file.exists(file.path(fx, "outputNorthSeaMort.txt")))
})

test_that("module UIs expose the new views and plot ids", {
  mort_ui <- paste(as.character(mortality_ui("mortality")), collapse = " ")
  expect_true(grepl("Predation by predator", mort_ui))
  expect_true(grepl("Mort.txt", mort_ui, fixed = TRUE))
  rec_ui <- paste(as.character(recruitment_ui("recruitment")), collapse = " ")
  expect_true(grepl("Recruits vs SSB", rec_ui))
  expect_true(grepl("plot_sr_series", rec_ui) || grepl("sr_series", rec_ui))
  feed_ui <- paste(as.character(feeding_ui("feeding")), collapse = " ")
  expect_true(grepl("diet_prop_plot", feed_ui))
  expect_true(grepl("consumed_plot", feed_ui))
  spat_ui <- paste(as.character(spatial_ui("spatial")), collapse = " ")
  expect_true(grepl("Predator-prey overlap", spat_ui))
  expect_true(grepl("spatial-prey", spat_ui))
})

test_that("Scientific Guide registry lists the new views", {
  tools <- jsonlite::fromJSON(file.path(root, "inst", "ai", "tools.json"),
                              simplifyVector = FALSE)
  ids <- vapply(tools$tools, function(t) t$tool_id, character(1))
  mort <- tools$tools[[match("mortality", ids)]]
  expect_true(all(c("source", "predator", "mort") %in% unlist(mort$views)))
  rec <- tools$tools[[match("recruitment", ids)]]
  expect_true("sr" %in% unlist(rec$views))
  spat <- tools$tools[[match("spatial", ids)]]
  expect_true("overlap" %in% unlist(spat$views))
  source(file.path(root, "inst", "ai", "R", "ai_nav.R"), local = TRUE)
  nav <- ra_ai_nav_catalog()
  expect_true(grepl("overlap", nav$spatial$summary, ignore.case = TRUE))
  expect_true(grepl("SpecificPredMort", nav$mortality$summary, fixed = TRUE))
  expect_true(grepl("DietCheck", nav$feeding$summary, fixed = TRUE))
  expect_true("overlap" %in% nav$spatial$keywords)
})

test_that("stock-recruit pairs use the spawning before each recruitment (N7)", {
  ## Output every 30 days; spawning (SSB change) at day 330 and 690,
  ## recruitment (YOY change) at day 180 (initial pulse, before any spawning),
  ## 540 and 900. SSB is 0 until day 330.
  t <- seq(0, 960, by = 30)
  ssb <- data.frame(Time = t, FTM = ifelse(t < 330, 0, ifelse(t < 690, 100, 80)))
  yoy <- data.frame(Time = t, `FTM-0` = ifelse(t < 180, 5, ifelse(t < 540, 50, ifelse(t < 900, 20, 30))),
                    check.names = FALSE)
  pr <- recruit_ssb_pairs(yoy, ssb, "FTM")
  expect_equal(pr$Time, c(540, 900))
  expect_equal(pr$Recruits, c(20, 30))
  expect_equal(pr$SSB, c(100, 80))
  expect_equal(pr$SSB_time, c(330, 690))
})
