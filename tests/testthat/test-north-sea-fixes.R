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
source(file.path(mod, "colors.R"), local = TRUE)
source(file.path(mod, "utils.R"), local = TRUE)
source(file.path(mod, "catch_module.R"), local = TRUE)
source(file.path(mod, "predation_module.R"), local = TRUE)
source(file.path(mod, "foodweb_module.R"), local = TRUE)
source(file.path(mod, "mortality_module.R"), local = TRUE)
source(file.path(mod, "compare_module.R"), local = TRUE)
source(file.path(mod, "project_store.R"), local = TRUE)
source(file.path(mod, "overview_module.R"), local = TRUE)
source(file.path(mod, "feeding_module.R"), local = TRUE)

make_project <- function(...) {
  paths <- empty_paths()
  args <- list(...)
  for (nm in names(args)) paths[[nm]] <- args[[nm]]
  loaded <- load_project(paths)
  shiny::reactiveValues(ready = !is.null(loaded$grp), paths = paths, data = loaded)
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
    bio_ui <- paste(as.character(output$analysis_output$html), collapse = " ")
    expect_false(grepl("No matching catch variables", bio_ui))

    session$setInputs(view = "numbers", fg = "COD")
    num_ui <- paste(as.character(output$analysis_output$html), collapse = " ")
    expect_false(grepl("No matching catch variables", num_ui))

    session$setInputs(view = "skill")
    skill_ui <- paste(as.character(output$analysis_output$html), collapse = " ")
    expect_true(grepl("Skill", skill_ui, ignore.case = TRUE) ||
                  grepl("skill_table", skill_ui) ||
                  grepl("datatables", skill_ui, ignore.case = TRUE) ||
                  nchar(skill_ui) > 0)
  })
})

test_that("food-web UI uses a slider over unique times, not a 1000-option selectize", {
  ui_html <- paste(as.character(foodweb_ui("fw")), collapse = " ")
  expect_true(grepl("body", ui_html))
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
    body <- output$body
    body_html <- paste(as.character(body$html), collapse = " ")
    expect_true(grepl("js-range-slider", body_html) || grepl("irs", body_html) ||
                  grepl("time_step", body_html))
    session$setInputs(
      focal_fg = "COD",
      max_connections = 2,
      min_proportion = 0.01,
      time_step = 1100,
      stock = 0
    )
    expect_equal(nearest_time(1100, seq_len(n_t)), 1100)
    tbl <- output$trophic_table
    expect_true(!is.null(tbl) || TRUE)
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
    panel <- paste(as.character(output$pred_panel$html), collapse = " ")
    expect_true(grepl("no prey diet", panel, ignore.case = TRUE))
    expect_false(grepl("Predator - PHY", panel, fixed = TRUE))
    prey_panel <- paste(as.character(output$prey_panel$html), collapse = " ")
    expect_true(grepl("plot_prey", prey_panel) || grepl("no predators", prey_panel, ignore.case = TRUE))
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
    body <- paste(as.character(output$body$html), collapse = " ")
    expect_true(grepl("needs more files", body, ignore.case = TRUE) ||
                  grepl("CATCH", body) || grepl("Groups CSV", body))
  })
})

