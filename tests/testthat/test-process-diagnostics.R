## Mortality by predator, recruits vs SSB, realised diet, spatial overlap.

find_pkg_root <- function() {
  cand <- c(getwd(), file.path(getwd(), "..", ".."), file.path(getwd(), ".."))
  for (p in cand) {
    if (file.exists(file.path(p, "inst", "shiny", "modules", "analysis_helpers.R"))) {
      return(normalizePath(p))
    }
  }
  stop("Cannot find package root")
}

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

test_that("trunk SpecificPredMort parses predation by predator", {
  pred <- data.frame(
    Time = c(0, 0, 365, 365),
    Group = "COD",
    Cohort = c(0, 1, 0, 1),
    Stock = 0,
    COD = c(0.05, 0.02, 0.06, 0.03),
    PHY = c(0.25, 0.26, 0.22, 0.24),
    stringsAsFactors = FALSE
  )
  long <- parse_pred_mort_by_predator(pred, "PHY")
  expect_equal(nrow(long), 4)
  expect_equal(unique(long$Predator), "COD")
  expect_equal(unique(long$Prey), "PHY")
  expect_equal(sort(unique(long$Age)), c("Age01", "Age02"))
  expect_equal(sum(long$Value), sum(pred$PHY))
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
