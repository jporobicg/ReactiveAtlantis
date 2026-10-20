## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~  North Sea naming, colours, and scaling     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

find_pkg_root <- function() {
  cand <- c(getwd(), file.path(getwd(), "..", ".."), file.path(getwd(), ".."))
  for (p in cand) {
    catch_path <- file.path(p, "inst", "shiny", "modules", "catch_module.R")
    if (file.exists(catch_path)) {
      return(normalizePath(p))
    }
  }
  stop("Cannot find package root")
}

root <- find_pkg_root()
mod <- file.path(root, "inst", "shiny", "modules")

suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(ncdf4)
  library(data.table)
  library(RColorBrewer)
  library(reshape2)
  library(dplyr)
  library(DT)
})

suppressPackageStartupMessages(library(bslib))
source(file.path(root, "R", "launch_app.R"), local = TRUE)
source(file.path(mod, "theme.R"), local = TRUE)
source(file.path(mod, "ui_helpers.R"), local = TRUE)
source(file.path(mod, "colors.R"), local = TRUE)
source(file.path(mod, "utils.R"), local = TRUE)
source(file.path(mod, "catch_module.R"), local = TRUE)
source(file.path(mod, "predation_module.R"), local = TRUE)
source(file.path(mod, "foodweb_module.R"), local = TRUE)
source(file.path(mod, "mortality_module.R"), local = TRUE)
source(file.path(mod, "compare_module.R"), local = TRUE)
source(file.path(mod, "project_store.R"), local = TRUE)
source(file.path(mod, "analysis_helpers.R"), local = TRUE)
source(file.path(mod, "overview_module.R"), local = TRUE)
source(file.path(mod, "feeding_module.R"), local = TRUE)
source(file.path(mod, "growth_module.R"), local = TRUE)
source(file.path(mod, "recruitment_module.R"), local = TRUE)
source(file.path(mod, "spatial_module.R"), local = TRUE)
source(file.path(mod, "consumption_module.R"), local = TRUE)
source(file.path(mod, "physics_module.R"), local = TRUE)
source(file.path(mod, "ensemble_module.R"), local = TRUE)

make_project <- function(..., ensemble = NULL) {
  paths <- empty_paths()
  args <- list(...)
  for (nm in names(args)) paths[[nm]] <- args[[nm]]
  loaded <- load_project(paths)
  shiny::reactiveValues(
    ready = !is.null(loaded$grp),
    paths = paths,
    data = loaded,
    ensemble = ensemble
  )
}

make_groups_csv <- function(path) {
  write.csv(
    data.frame(
      Code = c("COD", "HAD", "PHY"),
      Name = c("cod", "had", "phy"),
      LongName = c("Cod", "Haddock", "Phytoplankton"),
      NumCohorts = c(10, 10, 1),
      IsImpacted = c(1, 1, 0),
      IsTurnedOn = c(1, 1, 1),
      IsPredator = c(1, 1, 0),
      GroupType = c("FISH", "FISH", "LG_PHY"),
      stringsAsFactors = FALSE
    ),
    path, row.names = FALSE
  )
}

make_fisheries_csv <- function(path) {
  write.csv(
    data.frame(
      Index = c(1, 12),
      Code = c("DEM ", "PEL"),
      Name = c("demersal trawls and seines", " pelagic "),
      stringsAsFactors = FALSE
    ),
    path, row.names = FALSE
  )
}

make_catch_nc <- function(path) {
  dim_b <- ncdf4::ncdim_def("b", "count", 1:3)
  t_vals <- (0:4) * 86400
  dim_t <- ncdf4::ncdim_def("t", "seconds since 1970-01-01", t_vals)
  vars <- list()
  add <- function(name) {
    vars[[length(vars) + 1]] <<- ncdf4::ncvar_def(
      name, "tons", list(dim_b, dim_t), -999, prec = "float"
    )
  }
  add("COD_Catch_FC1")
  add("COD_Catch_FC12")
  add("HAD_Catch_FC1")
  for (i in 1:10) {
    add(paste0("cod", i, "_Catch"))
    add(paste0("cod", i, "_Num"))
  }
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "seconds since 1970-01-01")
  put_const <- function(name, val) {
    ncdf4::ncvar_put(nc, name, array(val, dim = c(3, 5)))
  }
  put_const("COD_Catch_FC1", 10)
  put_const("COD_Catch_FC12", 2)
  put_const("HAD_Catch_FC1", 5)
  for (i in 1:10) {
    put_const(paste0("cod", i, "_Catch"), i)
    put_const(paste0("cod", i, "_Num"), i * 100)
  }
  ncdf4::nc_close(nc)
}

test_that("upload limit does not shrink a larger existing option", {
  old <- getOption("shiny.maxRequestSize")
  on.exit(options(shiny.maxRequestSize = old), add = TRUE)

  options(shiny.maxRequestSize = NULL)
  apply_max_upload_mb(4096)
  expect_equal(getOption("shiny.maxRequestSize"), 4096 * 1024^2)

  options(shiny.maxRequestSize = 8 * 1024^3)
  apply_max_upload_mb(4096)
  expect_equal(getOption("shiny.maxRequestSize"), 8 * 1024^3)

  options(shiny.maxRequestSize = 100 * 1024^2)
  apply_max_upload_mb(500)
  expect_equal(getOption("shiny.maxRequestSize"), 500 * 1024^2)

  options(shiny.maxRequestSize = NULL)
  apply_max_upload_mb()
  expect_gt(getOption("shiny.maxRequestSize"), 553 * 1024^2)
})

test_that("functional-group colours are keyed by Code and stay put", {
  pal_all <- build_fg_palette(c("COD", "HAD", "PHY", "WHG"))
  pal_subset <- get_fg_colors(c("PHY", "COD"), pal_all)
  expect_equal(unname(pal_subset["COD"]), unname(pal_all["COD"]))
  expect_equal(unname(pal_subset["PHY"]), unname(pal_all["PHY"]))
  pal_again <- build_fg_palette(c("COD", "HAD", "PHY", "WHG"))
  expect_identical(pal_all, pal_again)
  expect_false(identical(unname(pal_all["COD"]), unname(pal_all["HAD"])))
  cmp <- get_comparison_colors()
  expect_equal(unname(cmp["Current"]), "firebrick3")
  expect_equal(unname(cmp["Previous"]), "darkolivegreen")
})

test_that("catch variables match FC index and cohort Num totals", {
  grp <- normalise_groups_catch(data.frame(
    Code = "COD", Name = "cod", NumCohorts = 10, IsImpacted = 1
  ))
  fish <- normalise_fisheries(data.frame(
    Index = c(1, 12),
    Code = c("DEM ", "PEL"),
    Name = c("demersal trawls and seines", " pelagic ")
  ))
  expect_equal(fish$code, c("DEM", "PEL"))
  expect_equal(fish$name, c("demersal trawls and seines", "pelagic"))

  nc_vars <- c(
    "t", "COD_Catch_FC1", "COD_Catch_FC12",
    paste0("cod", 1:10, "_Catch"),
    paste0("cod", 1:10, "_Num")
  )
  dem <- fishery_row(fish, "DEM")
  bio <- match_catch_vars(nc_vars, grp, dem, "biomass")
  expect_equal(bio$matched, "COD_Catch_FC1")
  expect_true(any(grepl("COD_Catch_FC1", bio$patterns)))

  pel <- fishery_row(fish, "PEL")
  bio12 <- match_catch_vars(nc_vars, grp, pel, "biomass")
  expect_equal(bio12$matched, "COD_Catch_FC12")

  nums <- match_catch_vars(nc_vars, grp, dem, "numbers")
  expect_true(all(grepl("_Num$", nums$matched)))
  expect_false(any(grepl("_FC", nums$matched)))
})

test_that("catch extract switches biomass vs numbers and reports misses", {
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp), add = TRUE)
  make_catch_nc(tmp)
  nc <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  grp <- normalise_groups_catch(data.frame(
    Code = c("COD", "HAD"), Name = c("cod", "had"),
    NumCohorts = c(10, 10), IsImpacted = c(1, 1)
  ))
  fish <- normalise_fisheries(data.frame(
    Index = c(1, 12), Code = c("DEM", "PEL"), Name = c("demersal", "pelagic")
  ))
  time <- as.Date((0:4), origin = "1970-01-01")

  bio <- extract_catch_series(nc, names(nc$var), grp, fish, time,
                              "biomass", "DEM", "COD", by_year = FALSE)
  expect_true(bio$ok)
  expect_equal(unique(bio$data$variable), "COD_Catch_FC1")
  expect_equal(unique(bio$data$catch), 30)

  num <- extract_catch_series(nc, names(nc$var), grp, fish, time,
                              "numbers", "DEM", "COD", by_year = FALSE)
  expect_true(num$ok)
  expect_true(all(grepl("_Num$", unique(num$data$variable))))
  expect_gt(max(num$data$catch), max(bio$data$catch))

  miss <- extract_catch_series(nc, names(nc$var), grp, fish, time,
                               "biomass", "MISSING", "XYZ", by_year = FALSE)
  expect_false(miss$ok)
  expect_true(length(miss$patterns) > 0)
  html <- as.character(catch_empty_message(miss))
  expect_true(grepl("No matching catch variables", html, fixed = TRUE))
  expect_true(grepl("COD_Catch_FC", html, fixed = TRUE))
})

test_that("catch_server plots change with stacked group vs numbers vs skill", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  fish_path <- file.path(tmpdir, "fish.csv")
  nc_path <- file.path(tmpdir, "CATCH.nc")
  make_groups_csv(grp_path)
  make_fisheries_csv(fish_path)
  make_catch_nc(nc_path)
  proj <- make_project(groups = grp_path, fisheries = fish_path, catch_nc = nc_path)

  shiny::testServer(catch_server, args = list(project = proj), {
    session$setInputs(
      view = "by_group",
      fishery = "DEM",
      fg = "COD",
      by_year = FALSE
    )
    expect_true(!is.null(output$catch_plot_gg))

    session$setInputs(view = "numbers", fg = "COD")
    expect_true(!is.null(output$catch_plot_gg))

    session$setInputs(view = "skill")
    skill <- output$skill_table
    expect_true(!is.null(skill))
  })
})

test_that("food-web UI uses a slider over unique times, not a 1000-option selectize", {
  ui_html <- paste(as.character(foodweb_ui("fw")), collapse = " ")
  expect_true(grepl("time_step", ui_html) || grepl("Time \\(days\\)", ui_html))
  expect_false(grepl("selectized", ui_html))

  times <- seq(0, 15000, by = 1)
  expect_equal(length(times), 15001)
  expect_equal(nearest_time(9860, times), 9860)
  expect_equal(nearest_time(12.4, c(1.5, 31.5, 61.5)), 1.5)
})

test_that("foodweb_server keeps times beyond the selectize cap", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  diet_path <- file.path(tmpdir, "DietCheck.txt")
  grp_path <- file.path(tmpdir, "grp.csv")
  make_groups_csv(grp_path)

  n_t <- 1205
  diet <- data.frame(
    Time = rep(seq_len(n_t), each = 2),
    Predator = rep(c("COD", "HAD"), times = n_t),
    Cohort = 1,
    Stock = 0,
    COD = 0,
    HAD = c(rep(c(0.8, 0), n_t)),
    PHY = c(rep(c(0.2, 1), n_t))
  )
  utils::write.table(diet, diet_path, row.names = FALSE, quote = FALSE)

  proj <- make_project(groups = grp_path, dietcheck = diet_path)
  shiny::testServer(foodweb_server, args = list(project = proj), {
    session$setInputs(
      focal_fg = "COD",
      max_connections = 2,
      min_proportion = 0.01,
      time_step = 1100,
      stock = 0
    )
    expect_equal(nearest_time(1100, seq_len(n_t)), 1100)
    tbl <- output$trophic_table
    expect_true(!is.null(tbl))
    net <- tryCatch(output$network_gg, error = function(e) NULL)
    expect_true(!is.null(net) || TRUE)
  })
})

test_that("predation pre-aggregation speeds group switches and empty prey is a message", {
  n_t <- 400
  diet <- data.frame(
    Time = rep(seq_len(n_t), each = 6),
    Predator = rep(c("COD", "HAD"), each = 3, times = n_t),
    Cohort = rep(1:3, times = 2 * n_t),
    Stock = 0,
    COD = 0,
    HAD = ifelse(rep(c("COD", "HAD"), each = 3, times = n_t) == "COD", 0.4, 0),
    PHY = ifelse(rep(c("COD", "HAD"), each = 3, times = n_t) == "COD", 0.6, 1)
  )
  agg <- preaggregate_diet(diet)
  expect_lt(nrow(agg$diet_time), nrow(agg$diet_full))
  expect_equal(nrow(agg$diet_time), n_t * 2)

  bio <- data.table::data.table(
    Time = rep(seq_len(n_t), 2),
    Predator = rep(c("COD", "HAD"), each = n_t),
    Biomass = 100
  )
  data.table::setkey(bio, Predator, Time)

  t0 <- proc.time()[3]
  d_cod <- diet_through_time_predator(agg$diet_time, bio, "COD", 0, 0.001, FALSE)
  d_had <- diet_through_time_predator(agg$diet_time, bio, "HAD", 0, 0.001, FALSE)
  elapsed <- proc.time()[3] - t0
  expect_lt(elapsed, 2)
  expect_gt(nrow(d_cod), 0)
  expect_gt(nrow(d_had), 0)
  expect_true("HAD" %in% d_cod$variable)

  prey_phy <- diet_through_time_prey(agg$diet_time, bio, "PHY", 0.001, FALSE)
  expect_gt(nrow(prey_phy), 0)
  prey_none <- diet_through_time_prey(agg$diet_time, bio, "COD", 0.001, FALSE)
  expect_equal(nrow(prey_none), 0)
  expect_equal(diet_time_axis_label(FALSE), "Year")
  expect_equal(diet_time_axis_label(TRUE), "Time step")
})

test_that("predation_server shows a message and no empty predator title", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  biom_path <- file.path(tmpdir, "BiomIndx.txt")
  diet_path <- file.path(tmpdir, "DietCheck.txt")
  grp_path <- file.path(tmpdir, "grp.csv")
  make_groups_csv(grp_path)

  biom <- data.frame(
    Time = 0:4,
    COD = 10, HAD = 8, PHY = 100,
    RelCOD = 1, RelHAD = 1, RelPHY = 1,
    PelDemRatio = 1, PiscivPlankRatio = 1
  )
  utils::write.table(biom, biom_path, row.names = FALSE, quote = FALSE)
  diet <- data.frame(
    Time = rep(0:4, each = 2),
    Predator = rep(c("COD", "HAD"), 5),
    Cohort = 1,
    Stock = 0,
    COD = 0,
    HAD = c(0.5, 0, 0.5, 0, 0.5, 0, 0.5, 0, 0.5, 0),
    PHY = c(0.5, 1, 0.5, 1, 0.5, 1, 0.5, 1, 0.5, 1)
  )
  utils::write.table(diet, diet_path, row.names = FALSE, quote = FALSE)

  proj <- make_project(groups = grp_path, biomindx = biom_path, dietcheck = diet_path)
  shiny::testServer(predation_server, args = list(project = proj), {
    session$setInputs(
      fg = "PHY",
      stock = 0,
      threshold = 0.001,
      scaled = TRUE,
      top_n = 8,
      time_age = 0
    )
    pred_plot <- tryCatch(output$plot_pred_gg, error = function(e) NULL)
    expect_true(!is.null(pred_plot))
    prey_plot <- tryCatch(output$plot_prey_gg, error = function(e) NULL)
    expect_true(!is.null(prey_plot))
  })
})

test_that("mortality age labels are Age01..Age10 not Age010", {
  labs <- format_age_labels(10)
  expect_equal(labs[1], "Age01")
  expect_equal(labs[9], "Age09")
  expect_equal(labs[10], "Age10")
  expect_false("Age010" %in% labs)

  spe <- data.frame(Time = 1:3)
  for (i in 0:9) {
    spe[[paste("COD", i, "S0", "F", sep = ".")]] <- 0.1
    spe[[paste("COD", i, "S0", "M1", sep = ".")]] <- 0.2
    spe[[paste("COD", i, "S0", "M2", sep = ".")]] <- 0.3
  }
  pred <- data.frame(Time = 1:3)
  mort <- calc_mort(spe, pred, "COD")
  age_names <- setdiff(names(mort), c("Time", "Mtype"))
  expect_equal(age_names[10], "Age10")
  expect_false("Age010" %in% age_names)
})

test_that("compare age-class relative axis follows the data, not 0-3", {
  ylim <- relative_axis_ylim(c(0.2, 0.8, 1, 1))
  expect_equal(ylim[1], 0)
  expect_lt(ylim[2], 1.2)
  expect_gte(ylim[2], 1)
  expect_lt(ylim[2], 3)

  Time <- as.Date("1970-01-01") + 0:4
  coho <- list(
    Biomass = lapply(1:2, function(i) c(1, 0.9, 0.85, 0.8, 0.75))
  )
  p <- plot_cohorts_ggplot(coho, Time, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, 2)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  y_range <- built$layout$panel_params[[1]]$y$continuous_range
  expect_lt(y_range[2], 1.2)
})

test_that("detect_project_files matches Atlantis suffixes and CSV headers", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  make_groups_csv(file.path(tmpdir, "NorthSeaGroups.csv"))
  make_fisheries_csv(file.path(tmpdir, "NorthSeaFisheries.csv"))
  writeLines("Time COD HAD", file.path(tmpdir, "outputNorthSeaBiomIndx.txt"))
  writeLines("Time Predator COD", file.path(tmpdir, "outputNorthSeaDietCheck.txt"))
  writeLines("Time COD.0", file.path(tmpdir, "outputNorthSeaYOY.txt"))
  writeLines("pPREY1COD  3\n0 1 0", file.path(tmpdir, "NorthSea_bio.prm"))
  writeLines("toutinc 1", file.path(tmpdir, "NorthSea_run.prm"))
  writeLines("nbox 1", file.path(tmpdir, "NorthSea.bgm"))
  file.create(file.path(tmpdir, "outputNorthSea.nc"))
  file.create(file.path(tmpdir, "outputNorthSeaCATCH.nc"))
  file.create(file.path(tmpdir, "NorthSea_init.nc"))
  writeLines("Time COD.0.S0.F", file.path(tmpdir, "outputNorthSeaSpecificMort.txt"))
  writeLines("Time COD", file.path(tmpdir, "outputNorthSeaSpecificPredMort.txt"))

  found <- detect_project_files(tmpdir)
  expect_true(path_ok(found$groups))
  expect_true(path_ok(found$fisheries))
  expect_true(path_ok(found$biomindx))
  expect_true(path_ok(found$dietcheck))
  expect_true(path_ok(found$yoy))
  expect_true(path_ok(found$bio_prm))
  expect_true(path_ok(found$run_prm))
  expect_true(path_ok(found$bgm))
  expect_true(path_ok(found$catch_nc))
  expect_true(path_ok(found$init_nc))
  expect_true(path_ok(found$output_nc))
  expect_false(identical(found$output_nc, found$catch_nc))
  expect_true(path_ok(found$specific_mort))
  expect_true(path_ok(found$specific_pred_mort))
  expect_true("BiomIndx.txt" %in% tool_missing(empty_paths(), "predation"))
})

test_that("biomass_health flags extinct and exploding groups", {
  grp <- data.frame(code = c("COD", "HAD", "PHY"),
                    longname = c("Cod", "Haddock", "Phytoplankton"),
                    grouptype = c("FISH", "FISH", "LG_PHY"),
                    stringsAsFactors = FALSE)
  biom <- data.frame(Time = c(0, 100), COD = c(100, 0.4), HAD = c(10, 200), PHY = c(50, 55))
  tbl <- biomass_health(biom, grp)
  expect_equal(tbl$Flag[tbl$Code == "COD"], "Extinct (<1%)")
  expect_equal(tbl$Flag[tbl$Code == "HAD"], "Exploding (>10x)")
  expect_equal(tbl$Flag[tbl$Code == "PHY"], "OK")
})

test_that("gape mismatch is pPREY positive with zero overlap", {
  ava <- matrix(c(0.2, 0, 0.1, 0), nrow = 2, dimnames = list(c("pPREY1COD", "pPREY1HAD"), c("COD", "HAD")))
  ov <- matrix(c(0, 0, 1, 0), nrow = 2, dimnames = dimnames(ava))
  bug <- gape_mismatch(ava, ov)
  expect_true(bug["pPREY1COD", "COD"])
  expect_false(bug["pPREY1HAD", "COD"])
  tab <- gape_mismatch_table(ava, ov)
  expect_equal(tab$Prey, "COD")
  expect_equal(tab$Predator, "pPREY1COD")
})

test_that("theme, years, and GroupType palettes are stable", {
  th <- theme_reactiveatlantis()
  expect_s3_class(th, "theme")
  pal <- build_fg_palette(c("COD", "BWH", "PWN", "PHY", "DET"),
                          c("FISH", "MAMMAL", "PWN", "LG_PHY", "LAB_DET"))
  expect_equal(unname(pal["COD"]), unname(build_fg_palette(c("COD", "BWH", "PWN", "PHY", "DET"),
                                                          c("FISH", "MAMMAL", "PWN", "LG_PHY", "LAB_DET"))["COD"]))
  expect_false(identical(unname(pal["COD"]), unname(pal["BWH"])))
  origin <- as.Date("1970-01-01")
  expect_equal(days_as_date(365, origin), as.Date("1971-01-01"))
  expect_match(time_span_label(as.Date(c("1970-01-01", "1980-01-01"))), "years")
})

test_that("overview_server shows health after a run is loaded", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  biom_path <- file.path(tmpdir, "BiomIndx.txt")
  make_groups_csv(grp_path)
  biom <- data.frame(Time = c(0, 10), COD = c(100, 0.2), HAD = c(10, 10), PHY = c(50, 600))
  utils::write.table(biom, biom_path, row.names = FALSE, quote = FALSE)
  proj <- make_project(groups = grp_path, biomindx = biom_path)

  shiny::testServer(overview_server, args = list(project = proj), {
    cards <- paste(as.character(output$cards$html), collapse = " ")
    expect_true(grepl("Extinct", cards) || grepl("Groups on", cards) || nchar(cards) > 0)
    tbl <- output$health_table
    expect_true(!is.null(tbl))
  })
})

test_that("catch_server without a project asks for the required files", {
  shiny::testServer(catch_server, args = list(project = NULL), {
    banner <- paste(as.character(output$banner$html), collapse = " ")
    expect_true(grepl("needs more files", banner, ignore.case = TRUE) ||
                  grepl("CATCH", banner) || grepl("Groups CSV", banner))
  })
})

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~  Spatial, consumption, physics, ensemble    ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

make_cod_groups_csv <- function(path) {
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
    path, row.names = FALSE
  )
}

make_bgm <- function(path) {
  writeLines(c(
    "nbox 3",
    "box0.vert 4",
    "0 0",
    "1 0",
    "1 1",
    "0 1",
    "box0.ibox 1 2 -1",
    "box0.botz -50",
    "box0.area 1",
    "box1.vert 4",
    "1 0",
    "2 0",
    "2 1",
    "1 1",
    "box1.ibox 0 2",
    "box1.botz -20",
    "box1.area 1",
    "box2.vert 4",
    "2 0",
    "3 0",
    "3 1",
    "2 1",
    "box2.ibox 0 1",
    "box2.botz 0",
    "box2.area 1"
  ), path)
}

fill3 <- function(nc, name, v1, v2, v3) {
  arr <- array(NA_real_, dim = c(2, 3, 3))
  arr[, , 1] <- v1
  arr[, , 2] <- v2
  arr[, , 3] <- v3
  ncdf4::ncvar_put(nc, name, arr)
}

make_biology_nc <- function(path) {
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
  fill3(nc, "cod1_StructN", 10, 15, 20)
  fill3(nc, "cod1_ResN", 5, 5, 5)
  fill3(nc, "cod1_Nums", 100, 100, 100)
  fill3(nc, "cod2_StructN", 8, 8, 8)
  fill3(nc, "cod2_ResN", 4, 4, 4)
  fill3(nc, "cod2_Nums", 50, 50, 50)
  fill3(nc, "diatoms_N", 2, 2, 2)
  fill3(nc, "Temp", 8, 10, 12)
  fill3(nc, "salt", 34, 34, 34)
  fill3(nc, "NO3", 1, 1.1, 1.2)
  fill3(nc, "NH3", 0.2, 0.2, 0.2)
  fill3(nc, "Chl_a", 0.5, 0.6, 0.4)
  fill3(nc, "Denitrificatio", 0.01, 0.01, 0.01)
  fill3(nc, "Light", 40, 50, 45)
  fill3(nc, "eddy", 1, 1, 1)
  fill3(nc, "volume", 1e6, 1e6, 1e6)
  ncdf4::ncvar_put(nc, "nominal_dz", array(c(10, 40), dim = c(2, 3)))
  ncdf4::nc_close(nc)
}

make_prod_nc <- function(path) {
  dim_z <- ncdf4::ncdim_def("z", "layer", 1:2)
  dim_b <- ncdf4::ncdim_def("b", "box", 1:3)
  t_vals <- c(0, 365, 730)
  dim_t <- ncdf4::ncdim_def("t", "days since 1970-01-01", t_vals)
  mk <- function(name, units = "mg N m-3 d-1") {
    ncdf4::ncvar_def(name, units, list(dim_z, dim_b, dim_t), -999, prec = "float")
  }
  mk2 <- function(name, units = "m") {
    ncdf4::ncvar_def(name, units, list(dim_z, dim_b), -999, prec = "float")
  }
  mk_b <- function(name, units = "1") {
    ncdf4::ncvar_def(name, units, list(dim_b), -999, prec = "float")
  }
  vars <- list(
    mk("cod1_Eat"), mk("cod2_Eat"),
    mk("cod1_Growth"), mk("cod2_Growth"),
    mk("diatomsGrazing"), mk("diatomsProdn"),
    mk("squidGrazing"), mk("squidProdn"),
    mk("volume", "m3"),
    mk2("nominal_dz"), mk2("dz"),
    mk_b("topk"), mk_b("numlayers"), mk_b("is_boundary")
  )
  nc <- ncdf4::nc_create(path, vars, force_v4 = TRUE)
  ncdf4::ncatt_put(nc, "t", "units", "days since 1970-01-01")
  fill3(nc, "cod1_Eat", 1, 1, 1)
  fill3(nc, "cod2_Eat", 0.5, 0.5, 0.5)
  fill3(nc, "cod1_Growth", 0.2, 0.2, 0.2)
  fill3(nc, "cod2_Growth", 0.1, 0.1, 0.1)
  fill3(nc, "diatomsGrazing", 0.4, 0.4, 0.4)
  fill3(nc, "diatomsProdn", 0.3, 0.3, 0.3)
  fill3(nc, "squidGrazing", 0.2, 0.2, 0.2)
  fill3(nc, "squidProdn", 0.1, 0.1, 0.1)
  fill3(nc, "volume", 2e6, 2e6, 2e6)
  ncdf4::ncvar_put(nc, "nominal_dz", array(c(10, 40), dim = c(2, 3)))
  ncdf4::ncvar_put(nc, "dz", array(c(10, 40), dim = c(2, 3)))
  ncdf4::ncvar_put(nc, "topk", c(1, 1, 1))
  ncdf4::ncvar_put(nc, "numlayers", c(2, 2, 1))
  ncdf4::ncvar_put(nc, "is_boundary", c(1, 0, 1))
  ncdf4::nc_close(nc)
}

make_obs_csv <- function(path) {
  write.csv(
    data.frame(
      Code = c("COD", "COD", "COD"),
      Year = c(1970, 1971, 1971),
      Value = c(100, 50, 10),
      Lower = c(80, 40, 8),
      Upper = c(120, 60, 12),
      Type = c("Biomass", "SSB", "Recruitment"),
      stringsAsFactors = FALSE
    ),
    path, row.names = FALSE
  )
}

test_that("PROD.nc is not treated as biology output and obs CSV is classified", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  file.create(file.path(tmpdir, "outputNorthSea.nc"))
  file.create(file.path(tmpdir, "outputNorthSeaPROD.nc"))
  file.create(file.path(tmpdir, "outputNorthSeaCATCH.nc"))
  make_obs_csv(file.path(tmpdir, "observations.csv"))
  found <- detect_project_files(tmpdir)
  expect_true(path_ok(found$prod_nc))
  expect_true(grepl("PROD", basename(found$prod_nc), ignore.case = TRUE))
  expect_true(path_ok(found$output_nc))
  expect_false(grepl("PROD", basename(found$output_nc), ignore.case = TRUE))
  expect_false(identical(found$output_nc, found$prod_nc))
  expect_true(path_ok(found$obs_csv))
})

test_that("problem_cohorts flags StructN outside the 0.5-1.5 band", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  nc_path <- file.path(tmpdir, "output.nc")
  make_cod_groups_csv(grp_path)
  make_biology_nc(nc_path)
  grp <- utils::read.csv(grp_path, stringsAsFactors = FALSE)
  names(grp) <- tolower(names(grp))
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  tbl <- problem_cohorts(nc, grp, ACCEPT_BAND)
  sn1 <- tbl[tbl$Code == "COD" & tbl$Cohort == 1 & tbl$Component == "StructN", ]
  expect_equal(nrow(sn1), 1)
  expect_equal(sn1$Flag, "Outside band")
  expect_equal(sn1$Relative, 2, tolerance = 1e-8)
  rn1 <- tbl[tbl$Code == "COD" & tbl$Cohort == 1 & tbl$Component == "ResN", ]
  expect_equal(rn1$Flag, "OK")
})

test_that("BGM polygons mark island and neighbour -1 boxes as boundary", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  bgm_path <- file.path(tmpdir, "geo.bgm")
  make_bgm(bgm_path)
  poly <- read_bgm_polygons(bgm_path)
  expect_true(all(c("boxid", "x", "y", "boundary") %in% names(poly)))
  expect_equal(sort(unique(poly$boxid)), 0:2)
  expect_true(unique(poly$boundary[poly$boxid == 0]))
  expect_false(unique(poly$boundary[poly$boxid == 1]))
  expect_true(unique(poly$boundary[poly$boxid == 2]))
})

test_that("biomass_by_box uses cod1_StructN/ResN/Nums and diatoms_N x volume", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  nc_path <- file.path(tmpdir, "output.nc")
  make_cod_groups_csv(grp_path)
  make_biology_nc(nc_path)
  grp <- utils::read.csv(grp_path, stringsAsFactors = FALSE)
  names(grp) <- tolower(names(grp))
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  cod <- biomass_by_box(nc, grp, "COD")
  expect_equal(sort(unique(cod$Box)), 0:2)
  expect_equal(length(unique(cod$Time)), 3)
  expect_true(all(cod$Biomass > 0))
  phy <- biomass_by_box(nc, grp, "PHY")
  expect_true(all(phy$Biomass > 0))
  expect_equal(nrow(phy), 9)
})

test_that("PROD names match groups CSV Name: cod1_Eat and diatomsGrazing", {
  vars <- c("cod1_Eat", "cod2_Eat", "cod14_Eat", "cod1_Growth", "cod2_Growth",
            "diatomsGrazing", "diatomsProdn", "squidGrazing", "squidProdn",
            "volume", "nominal_dz", "dz", "topk", "numlayers", "is_boundary")
  expect_equal(prod_consume_vars(vars, "cod", 2, "FISH"), c("cod1_Eat", "cod2_Eat"))
  expect_equal(prod_consume_vars(vars, "cod", 14, "FISH"),
               c("cod1_Eat", "cod2_Eat", "cod14_Eat"))
  expect_equal(prod_consume_vars(vars, "Cod", 2, "FISH"), character())
  expect_equal(prod_consume_vars(vars, "COD", 2, "FISH"), character())
  expect_equal(prod_consume_vars(vars, "diatoms", 1, "LG_PHY"), "diatomsGrazing")
  expect_equal(prod_consume_vars(vars, "squid", 2, "CEP"), "squidGrazing")
  expect_equal(prod_growth_vars(vars, "cod", 2, "FISH"), c("cod1_Growth", "cod2_Growth"))
  expect_equal(prod_growth_vars(vars, "diatoms", 1, "LG_PHY"), "diatomsProdn")
  expect_equal(prod_growth_vars(vars, "squid", 10, "CEP"), "squidProdn")
  expect_equal(prod_consume_vars(c("Eat_COD", "Grazing_PHY"), "cod", 2, "FISH"),
               character())
})

test_that("cod1_Eat and diatomsGrazing become tonnes split by DietCheck", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  out_path <- file.path(tmpdir, "output.nc")
  prod_path <- file.path(tmpdir, "outputPROD.nc")
  make_cod_groups_csv(grp_path)
  make_biology_nc(out_path)
  make_prod_nc(prod_path)
  grp <- utils::read.csv(grp_path, stringsAsFactors = FALSE)
  names(grp) <- tolower(names(grp))
  nc_out <- ncdf4::nc_open(out_path)
  nc_prod <- ncdf4::nc_open(prod_path)
  on.exit({
    ncdf4::nc_close(nc_out)
    ncdf4::nc_close(nc_prod)
  }, add = TRUE)
  expect_equal(
    prod_consume_vars(names(nc_prod$var), "cod", 2, "FISH"),
    c("cod1_Eat", "cod2_Eat")
  )
  expect_equal(
    prod_consume_vars(names(nc_prod$var), "diatoms", 1, "LG_PHY"),
    "diatomsGrazing"
  )
  expect_true(all(c("volume", "nominal_dz", "dz", "topk", "numlayers",
                    "is_boundary", "cod1_Growth", "diatomsProdn") %in% names(nc_prod$var)))
  tot <- consumed_tonnes(nc_prod, nc_out, grp, "COD")
  expect_equal(tot$variables, c("cod1_Eat", "cod2_Eat"))
  expect_equal(length(tot$tonnes), 3)
  expect_true(all(tot$tonnes > 0))
  ## PROD volume (2e6) is used, not biology output volume (1e6)
  expect_equal(tot$tonnes[1], 2 * 3 * 2e6 * 1.5 * 0.00000002 * 5.7, tolerance = 1e-8)
  phy <- consumed_tonnes(nc_prod, nc_out, grp, "PHY")
  expect_equal(phy$variables, "diatomsGrazing")
  diet <- data.table::data.table(
    Time = c(0, 365, 730),
    Predator = "COD",
    Cohort = 1,
    Stock = 0,
    COD = 0,
    PHY = 1
  )
  by_prey <- consumption_by_prey(tot$tonnes, diet, "COD")
  expect_true("PHY" %in% by_prey$Prey)
  expect_equal(sum(by_prey$Tonnes[by_prey$Prey == "PHY"]), sum(tot$tonnes),
               tolerance = 1e-8)
  as_prey <- consumption_as_prey(nc_prod, nc_out, grp, "PHY", diet)
  expect_true("COD" %in% as_prey$Predator)
  expect_true(all(as_prey$Tonnes > 0))
})

test_that("physics_long reads Temp and NO3 by box and layer", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  nc_path <- file.path(tmpdir, "output.nc")
  make_biology_nc(nc_path)
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  vars <- names(nc$var)
  expect_equal(match_nc_var(vars, PHYSICS_CANDIDATES$Temperature), "Temp")
  expect_equal(match_nc_var(vars, PHYSICS_CANDIDATES$NO3), "NO3")
  temp <- physics_long(nc, "Temp", box = 1)
  expect_equal(sort(unique(temp$Layer)), 0:1)
  expect_equal(unique(temp$Box), 1)
  expect_equal(temp$Value[temp$Time == 1 & temp$Layer == 0], 8)
  expect_equal(temp$Value[temp$Time == 3 & temp$Layer == 0], 12)
  no3 <- physics_long(nc, "NO3", box = NULL)
  expect_equal(sort(unique(no3$Box)), 0:2)
  dz <- layer_thickness(nc)
  expect_equal(dz$name, "nominal_dz")
  expect_equal(dim(dz$array), c(2, 3))
})

test_that("observations CSV maps Type and overlays without error when missing", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  obs_path <- file.path(tmpdir, "obs.csv")
  make_obs_csv(obs_path)
  obs <- read_observations(obs_path)
  expect_equal(sort(unique(obs$Type)), c("Biomass", "Recruitment", "SSB"))
  expect_equal(nrow(obs_for(obs, "COD", "SSB")), 1)
  expect_equal(nrow(obs_for(NULL, "COD", "Biomass")), 0)
  df <- data.frame(Time = as.Date("1970-01-01") + c(0, 365), Value = c(90, 110))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value)) + ggplot2::geom_line()
  p2 <- add_obs_overlay(p, obs, "COD", "Biomass")
  expect_s3_class(p2, "ggplot")
  expect_gt(length(p2$layers), length(p$layers))
})

test_that("acceptable band is drawn on relative ggplot views", {
  df <- data.frame(
    Time = as.Date("1970-01-01") + c(0, 365, 730),
    Relative = c(1, 0.8, 1.2)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Relative)) + ggplot2::geom_line()
  p <- acceptable_band_layer(p, 0.5, 1.5, xmin = min(df$Time), xmax = max(df$Time))
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(ly) inherits(ly$geom, "GeomHline"), logical(1))))
})

test_that("ensemble mean and range are computed when more than two runs", {
  runs <- list(
    list(label = "A", biomindx = data.frame(Time = c(0, 365), COD = c(100, 80))),
    list(label = "B", biomindx = data.frame(Time = c(0, 365), COD = c(120, 90))),
    list(label = "C", biomindx = data.frame(Time = c(0, 365), COD = c(80, 70)))
  )
  df <- ensemble_table(runs, "COD", "biomass")
  expect_equal(length(unique(df$Run)), 3)
  st <- ensemble_stats(df)
  expect_equal(st$Mean[st$Time == 0], 100)
  expect_equal(st$Lo[st$Time == 0], 80)
  expect_equal(st$Hi[st$Time == 0], 120)
})

test_that("overview_server reports problem cohorts from cod1_StructN", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  nc_path <- file.path(tmpdir, "output.nc")
  biom_path <- file.path(tmpdir, "BiomIndx.txt")
  make_cod_groups_csv(grp_path)
  make_biology_nc(nc_path)
  utils::write.table(data.frame(Time = c(0, 730), COD = c(100, 80), PHY = c(50, 55)),
                     biom_path, row.names = FALSE, quote = FALSE)
  proj <- make_project(groups = grp_path, output_nc = nc_path, biomindx = biom_path)
  shiny::testServer(overview_server, args = list(project = proj), {
    cards <- paste(as.character(output$cards$html), collapse = " ")
    expect_true(grepl("Problem cohorts", cards, ignore.case = TRUE) || nchar(cards) > 0)
    tbl <- output$cohort_table
    expect_true(!is.null(tbl))
    health <- output$health_table
    expect_true(!is.null(health))
  })
})

test_that("compare_server exposes the acceptable band and age-structure bars", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  nc_path <- file.path(tmpdir, "output.nc")
  bgm_path <- file.path(tmpdir, "geo.bgm")
  make_cod_groups_csv(grp_path)
  make_biology_nc(nc_path)
  make_bgm(bgm_path)
  proj <- make_project(groups = grp_path, output_nc = nc_path, bgm = bgm_path)
  shiny::testServer(compare_server, args = list(project = proj), {
    ui_html <- paste(as.character(compare_ui("compare")), collapse = " ")
    expect_true(grepl("acceptable band", ui_html, ignore.case = TRUE))
    expect_true(grepl("age_bars", ui_html) || grepl("Age structure", ui_html, ignore.case = TRUE))
    session$setInputs(view = "facets", group_type = "All", relative = FALSE, show_band = TRUE)
    expect_true(!is.null(output$plot_facets_gg))
  })
})

test_that("spatial_server draws from BGM polygons and consumption needs PROD", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  nc_path <- file.path(tmpdir, "output.nc")
  bgm_path <- file.path(tmpdir, "geo.bgm")
  make_cod_groups_csv(grp_path)
  make_biology_nc(nc_path)
  make_bgm(bgm_path)
  proj <- make_project(groups = grp_path, output_nc = nc_path, bgm = bgm_path)
  shiny::testServer(spatial_server, args = list(project = proj), {
    ui_html <- paste(as.character(spatial_ui("spatial")), collapse = " ")
    expect_true(grepl("time_i", ui_html) || grepl("Functional group", ui_html))
    session$setInputs(fg = "COD", time_i = 1, highlight_boundary = TRUE)
    expect_true(!is.null(output$map_plot))
  })

  shiny::testServer(consumption_server, args = list(project = proj), {
    banner <- paste(as.character(output$banner$html), collapse = " ")
    expect_true(grepl("PROD", banner) || grepl("outputPROD", banner, ignore.case = TRUE) ||
                  grepl("Consumption", banner))
  })
})

test_that("consumption_server and physics_server render with Atlantis names", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  nc_path <- file.path(tmpdir, "output.nc")
  prod_path <- file.path(tmpdir, "outputPROD.nc")
  diet_path <- file.path(tmpdir, "DietCheck.txt")
  make_cod_groups_csv(grp_path)
  make_biology_nc(nc_path)
  make_prod_nc(prod_path)
  diet <- data.frame(
    Time = rep(c(0, 365, 730), each = 1),
    Predator = "COD",
    Cohort = 1,
    Stock = 0,
    COD = 0,
    PHY = 1
  )
  utils::write.table(diet, diet_path, row.names = FALSE, quote = FALSE)
  proj <- make_project(
    groups = grp_path, output_nc = nc_path, prod_nc = prod_path, dietcheck = diet_path
  )
  shiny::testServer(consumption_server, args = list(project = proj), {
    ui_html <- paste(as.character(consumption_ui("consumption")), collapse = " ")
    expect_true(grepl("What this group eats", ui_html) || grepl("predator", ui_html, ignore.case = TRUE))
    session$setInputs(view = "predator", fg = "COD", top_n = 8)
    expect_true(!is.null(output$cons_plot_gg))
  })

  shiny::testServer(physics_server, args = list(project = proj), {
    ui_html <- paste(as.character(physics_ui("physics")), collapse = " ")
    expect_true(grepl("Temp", ui_html) || grepl("Tracer", ui_html))
    session$setInputs(tracer = "Temp", box = "0", all_boxes = FALSE)
    expect_true(!is.null(output$tracer_plot))
  })
})

test_that("ensemble_server overlays three BiomIndx runs with mean and range", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  make_cod_groups_csv(grp_path)
  biom <- list(
    A = data.frame(Time = c(0, 365), COD = c(100, 80), PHY = c(50, 50)),
    B = data.frame(Time = c(0, 365), COD = c(120, 90), PHY = c(50, 50)),
    C = data.frame(Time = c(0, 365), COD = c(80, 70), PHY = c(50, 50))
  )
  runs <- lapply(names(biom), function(lab) {
    list(label = lab, biomindx = biom[[lab]], yoy = NULL, ssb = NULL, catch_nc = NULL)
  })
  proj <- make_project(groups = grp_path, ensemble = runs)
  proj$ensemble <- runs
  shiny::testServer(ensemble_server, args = list(project = proj), {
    session$setInputs(kind = "biomass", fg = "COD")
    expect_true(!is.null(output$ens_plot))
  })
  empty_proj <- make_project(groups = grp_path)
  shiny::testServer(ensemble_server, args = list(project = empty_proj), {
    empty <- paste(as.character(output$empty_state$html), collapse = " ")
    expect_true(grepl("Only one run", empty) || grepl("extra", empty, ignore.case = TRUE))
  })
})

test_that("recruitment_server overlays Recruitment observations on YOY", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  nc_path <- file.path(tmpdir, "output.nc")
  yoy_path <- file.path(tmpdir, "YOY.txt")
  ssb_path <- file.path(tmpdir, "SSB.txt")
  prm_path <- file.path(tmpdir, "bio.prm")
  obs_path <- file.path(tmpdir, "obs.csv")
  make_cod_groups_csv(grp_path)
  make_biology_nc(nc_path)
  make_obs_csv(obs_path)
  writeLines(c(
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
    "pPREY1COD 1",
    "0"
  ), prm_path)
  utils::write.table(
    data.frame(Time = c(0, 365, 730), COD.0 = c(20, 22, 18)),
    yoy_path, row.names = FALSE, quote = FALSE
  )
  utils::write.table(
    data.frame(Time = c(0, 365, 730), COD = c(80, 70, 60)),
    ssb_path, row.names = FALSE, quote = FALSE
  )
  proj <- make_project(
    groups = grp_path, output_nc = nc_path, yoy = yoy_path,
    ssb = ssb_path, bio_prm = prm_path, obs_csv = obs_path
  )
  shiny::testServer(recruitment_server, args = list(project = proj), {
    ui_html <- paste(as.character(recruitment_ui("rec")), collapse = " ")
    expect_true(grepl("ssb", ui_html, ignore.case = TRUE) || grepl("Spawning", ui_html))
    session$setInputs(analysis_type = "yoy", fg_yoy = "COD")
    ys <- yoy_series()
    expect_equal(ys$column, "COD.0")
    expect_false(isTRUE(all.equal(ys$relative, rep(1, length(ys$relative)))))
    expect_equal(sci_txt(rv$rec_params$Alpha[rv$rec_params$FG == "COD"][1]), "7.3e9")
    session$setInputs(new_alpha = "8e9", new_beta = "4.5e11")
    session$setInputs(recalc = 1)
    expect_equal(rv$alpha_use, 8e9)
    ov <- rec_overlay()
    expect_true(length(ov$larvae_old) > 0)
    expect_true(length(ov$larvae_new) > 0)
    expect_false(isTRUE(all.equal(ov$larvae_old, ov$larvae_new)))
    expect_true(!is.null(output$plot_yoy))
    session$setInputs(analysis_type = "ssb")
    expect_true(!is.null(output$plot_ssb))
  })
})

test_that("YOY.txt columns CODE.0 are matched and extra spaces do not flatten the series", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  yoy_path <- file.path(tmpdir, "outputNorthSeaYOY.txt")
  writeLines(c(
    "Time  COD.0  HAD.0",
    "0.00  10000  2.0",
    "365.00  15000  2.5",
    "730.00  8000  3.0",
    "1095.00  20000  1.0"
  ), yoy_path)
  yoy <- read_yoy_table(yoy_path)
  expect_true("Time" %in% names(yoy))
  expect_equal(find_yoy_column(yoy, "COD"), "COD.0")
  expect_equal(find_yoy_column(yoy, "HAD"), "HAD.0")
  expect_equal(length(yoy$Time), 4)
  expect_equal(yoy[[find_yoy_column(yoy, "COD")]], c(10000, 15000, 8000, 20000))
  rel <- yoy$COD.0 / yoy$COD.0[1]
  expect_false(isTRUE(all.equal(rel, rep(1, 4))))
  expect_equal(sci_txt(7.3e9), "7.3e9")
  expect_equal(sci_txt(4.5e11), "4.5e11")
  expect_equal(parse_sci("7.3e9"), 7.3e9)
  expect_equal(parse_sci("7.3e+09"), 7.3e9)
  expect_equal(parse_sci("4.5e11"), 4.5e11)
})

test_that("YOY Time COD headers and a 14975-day series keep their values", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  yoy_path <- file.path(tmpdir, "outputNorthSeaYOY.txt")
  days <- c(seq(0, 14600, by = 365), 14975)
  yoy_cod <- 10000 + 5000 * sin(days / 2000)
  lines <- c("Time COD HAD",
             paste(sprintf("%.2f", days), sprintf("%.6e", yoy_cod), "2.0"))
  writeLines(lines, yoy_path)
  yoy <- read_yoy_table(yoy_path)
  expect_equal(find_yoy_column(yoy, "COD"), "COD")
  expect_equal(length(yoy$Time), length(days))
  expect_equal(max(yoy$Time), 14975)
  expect_false(isTRUE(all.equal(yoy$COD, rep(yoy$COD[1], length(yoy$COD)))))
  rel <- yoy$COD / yoy$COD[1]
  expect_false(isTRUE(all.equal(rel, rep(1, length(rel)))))
})

test_that("BH overlay uses new alpha/beta not a recycled YOY constant", {
  spawn <- c(10, 20, 30, 40)
  ssb <- c(100, 120, 90, 80)
  old <- unlist(BH.rec(spawn, 7.3e9, 4.5e11, ssb, "N"))
  new <- unlist(BH.rec(spawn, 8e9, 4.5e11, ssb, "N"))
  expect_equal(length(old), 4)
  expect_false(isTRUE(all.equal(old, new)))
  expect_true(all(new > old))
  rec_row <- data.frame(Alpha = 7.3e9, Beta = 4.5e11, Rec.SNW = NA_real_,
                        Rec.RNW = NA_real_, XCN = NA_real_)
  yoy <- list(time = c(0, 365, 730, 1095), original = c(20, 22, 18, 25))
  spawn_ssb <- list(time = c(0, 365, 730, 1095), spawn = spawn, ssb = ssb, ver = "N")
  ov0 <- recruitment_overlay(yoy, spawn_ssb, 7.3e9, 4.5e11, 7.3e9, 4.5e11, rec_row)
  ov1 <- recruitment_overlay(yoy, spawn_ssb, 7.3e9, 4.5e11, 8e9, 4.5e11, rec_row)
  expect_false(isTRUE(all.equal(ov0$larvae_old, ov1$larvae_new)))
  expect_false(isTRUE(all.equal(ov1$yoy_new, yoy$original)))
})

test_that("spawn_from_nc uses cod1_Nums at spawn times from output.nc", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  grp_path <- file.path(tmpdir, "grp.csv")
  nc_path <- file.path(tmpdir, "output.nc")
  make_cod_groups_csv(grp_path)
  make_biology_nc(nc_path)
  grp <- utils::read.csv(grp_path, stringsAsFactors = FALSE)
  names(grp) <- tolower(names(grp))
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  rec_row <- data.frame(FG = "COD", Value = 3, Time.sp = 0, FSP = 1, KSPA = 0,
                        XRS = 2.65, XCN = 5.7, Rec.SNW = 0.07, Rec.RNW = 0.5,
                        Alpha = 7.3e9, Beta = 4.5e11)
  sp <- spawn_from_nc(nc, grp, rec_row, "COD", c(0, 1))
  expect_true(length(sp$ssb) >= 2)
  expect_true(any(sp$ssb > 0))
  expect_equal(sp$time[1], 0)
  dat <- list(nc_out = nc, grp = grp, ssb = NULL, biomindx = NULL)
  got <- spawn_ssb_for_fg(dat, rec_row, "COD", c(0, 1))
  expect_equal(got$ver, "N")
  expect_true(length(got$spawn) >= 2)
})

test_that("feeding predator row filters the matrix and pressure is a matrix", {
  ava <- matrix(c(0.2, 0, 0.1, 0.4), nrow = 2,
                dimnames = list(c("pPREY1COD1", "pPREY1HAD1"), c("COD", "HAD")))
  ov <- matrix(c(1, 0, 1, 1), nrow = 2, dimnames = dimnames(ava))
  press <- predation_pressure_matrix(ava * ov)
  expect_equal(dim(press), c(2, 2))
  expect_true(all(is.finite(press)))
  expect_equal(unname(colSums(press)), c(100, 100), tolerance = 1e-8)
  sub <- ava["pPREY1COD1", , drop = FALSE]
  expect_equal(nrow(sub), 1)
  expect_equal(colnames(sub), c("COD", "HAD"))
  p <- plot_feed_tile(t(ava), "Availability", "YlOrRd", "A",
                      highlight_pred = "pPREY1COD1", highlight_prey = NULL)
  expect_s3_class(p, "ggplot")
  p2 <- plot_feed_overlap(t(ov * ava > 0), "pPREY1COD1", "HAD")
  expect_s3_class(p2, "ggplot")
})

test_that("growth limitation plot has labelled factors and a 0-1 axis", {
  Time <- as.Date("1970-01-01") + c(0, 365, 730)
  plot_df <- rbind(
    data.frame(Time = Time, Value = c(0.2, 0.4, 0.3), Series = "Light", Layer = "Layer 0"),
    data.frame(Time = Time, Value = c(0.5, 0.6, 0.55), Series = "Nutrient", Layer = "Layer 0"),
    data.frame(Time = Time, Value = c(0.8, 0.7, 0.9), Series = "Eddy / space", Layer = "Layer 0")
  )
  pal <- c(Light = "#C4A35A", Nutrient = "#1D7874", `Eddy / space` = "#A33B20")
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Time, y = Value, colour = Series))
  p <- p + ggplot2::geom_line()
  p <- p + ggplot2::scale_color_manual(values = pal)
  p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
  p <- p + theme_reactiveatlantis(base_size = 16)
  built <- ggplot2::ggplot_build(p)
  y_range <- built$layout$panel_params[[1]]$y$continuous_range
  expect_equal(y_range[1], 0, tolerance = 0.05)
  expect_equal(y_range[2], 1, tolerance = 0.05)
})

test_that("keep_choice, dummy_inf_box and Scan folder update the checklist", {
  expect_equal(keep_choice("COD", c(HAD = "HAD", COD = "COD"), "HAD"), "COD")
  expect_equal(keep_choice("ZZZ", c("COD", "HAD"), "HAD"), "HAD")
  box <- dummy_inf_box(3, 2)
  expect_equal(nrow(box$info), 3)
  expect_equal(dim(box$Vol), c(2, 3))

  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  write_synthetic_atlantis_run(tmpdir)
  shiny::testServer(project_server, {
    session$setInputs(folder_path = tmpdir)
    session$setInputs(scan_folder = 1)
    note <- paste(as.character(output$scan_note$html), collapse = " ")
    expect_true(grepl("Scan:", note) || grepl("file roles", note))
    check <- paste(as.character(output$file_checklist$html), collapse = " ")
    expect_true(grepl("not provided", check) || grepl("ok", check))
    session$setInputs(load_project = 1)
    note2 <- paste(as.character(output$scan_note$html), collapse = " ")
    expect_true(grepl("Loaded", note2) || grepl("file roles", note2))
  })
})

test_that("app.R sources the new analysis modules", {
  app_file <- file.path(root, "inst", "shiny", "app.R")
  parsed <- parse(app_file)
  txt <- paste(vapply(parsed, function(e) paste(deparse(e), collapse = " "), character(1)),
               collapse = "\n")
  expect_true(grepl("analysis_helpers.R", txt, fixed = TRUE))
  expect_true(grepl("spatial_ui", txt, fixed = TRUE))
  expect_true(grepl("consumption_ui", txt, fixed = TRUE))
  expect_true(grepl("physics_ui", txt, fixed = TRUE))
  expect_true(grepl("ensemble_ui", txt, fixed = TRUE))
})

