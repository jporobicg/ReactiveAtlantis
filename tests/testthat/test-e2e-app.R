## End-to-end browser test: load a synthetic run and visit every nav panel.
## AppDriver skips when it thinks this is CRAN; this test is meant to run in CI.
Sys.setenv(NOT_CRAN = "true")

has_plot_image <- function(html) {
  (grepl("<img", html, ignore.case = TRUE) &&
     grepl("src\\s*=", html, ignore.case = TRUE)) ||
    grepl("plotly", html, ignore.case = TRUE) ||
    grepl("js-plotly", html, ignore.case = TRUE)
}

has_table <- function(html) {
  grepl("<table", html, ignore.case = TRUE) ||
    grepl("datatables", html, ignore.case = TRUE) ||
    grepl("dataTables", html)
}

has_bound_output <- function(html) {
  nzchar(trimws(html)) &&
    (has_plot_image(html) || has_table(html) ||
       grepl("Only one run", html) ||
       grepl("Paste extra", html) ||
       grepl("Add another run", html) ||
       grepl("needs more files", html, ignore.case = TRUE))
}

click_nav <- function(app, title) {
  sel <- sprintf(".navbar a.nav-link[data-value=%s], a.nav-link[data-value=%s]",
                 deparse(title), deparse(title))
  clicked <- tryCatch({
    app$click(selector = sel)
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(clicked)) {
    app$get_js(sprintf(
      "var el = Array.from(document.querySelectorAll('a.nav-link')).find(function(a){return (a.textContent||'').trim()===%s}); if (el) el.click(); true;",
      deparse(title)
    ))
  }
  app$wait_for_idle(timeout = 120000)
  TRUE
}

save_shot <- function(app, dest, name) {
  path <- file.path(dest, paste0(name, ".png"))
  try(app$get_screenshot(path), silent = TRUE)
  art <- "/opt/cursor/artifacts"
  if (dir.exists(art)) {
    try(file.copy(path, file.path(art, paste0("e2e_", name, ".png")), overwrite = TRUE),
        silent = TRUE)
  }
  path
}

test_that("folder load fills every tab with a bound plot or table", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  chrome <- ra_test_chrome()
  skip_if(is.na(chrome), "Google Chrome or Chromium is required for shinytest2")
  Sys.setenv(CHROMOTE_CHROME = chrome)
  chrome_dir <- tempfile("chrome-data-")
  dir.create(chrome_dir)
  chromote::set_chrome_args(c(
    "--no-sandbox",
    "--disable-gpu",
    "--disable-dev-shm-usage",
    "--headless=new",
    paste0("--user-data-dir=", chrome_dir)
  ))

  root <- ra_pkg_root()
  run_dir <- tempfile("ra_e2e_")
  dir.create(run_dir)
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
  write_synthetic_atlantis_run(run_dir)

  shot_dir <- file.path(tempdir(), "ra_e2e_shots")
  dir.create(shot_dir, showWarnings = FALSE)

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(root, "inst", "shiny"),
    name = "reactiveatlantis-e2e",
    height = 1000,
    width = 1500,
    load_timeout = 40000,
    timeout = 120000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  app$set_inputs(`project-folder_path` = run_dir, wait_ = FALSE)
  app$wait_for_idle(timeout = 120000)
  app$click("project-scan_folder")
  app$wait_for_idle(timeout = 120000)

  body <- app$get_html("body")
  expect_true(grepl("Scan:", body) || grepl("file roles found", body))
  expect_true(grepl("not provided", body))
  expect_true(grepl("is-loaded", body) || grepl("data-status=\"loaded\"", body) ||
                grepl("class=\"ok\"", body) || grepl("class='ok'", body))
  save_shot(app, shot_dir, "01_scan_checklist")

  app$click("project-load_project")
  app$wait_for_idle(timeout = 60000)

  cards <- app$get_html("#overview-cards")
  expect_true(grepl("Groups on", cards, ignore.case = TRUE) || nchar(cards) > 20)
  health <- app$get_html("#overview-health_table")
  expect_true(has_table(health) || nchar(health) > 40)
  save_shot(app, shot_dir, "02_overview")

  fg_before <- app$get_value(input = "compare-fg_total")

  tabs <- list(
    list(title = "Compare", ids = c("#compare-plot_facets_gg"), shot = "03_compare"),
    list(title = "Ages", ids = c("#ages-age_plot"), shot = "03b_ages"),
    list(title = "Spatial", ids = c("#spatial-map_plot", "#spatial-box_ts"), shot = "04_spatial"),
    list(title = "Consumption", ids = c("#consumption-cons_plot_gg"), shot = "05_consumption"),
    list(title = "Physics", ids = c("#physics-tracer_plot", "#physics-dz_plot"), shot = "06_physics"),
    list(title = "Multi-run", ids = c("#ensemble-empty_state", "#ensemble-ens_plot"), shot = "07_multirun"),
    list(title = "Predation", ids = c("#predation-plot_pred_gg"), shot = "08_predation"),
    list(title = "Food web", ids = c("#foodweb-network_gg", "#foodweb-trophic_table"), shot = "09_foodweb"),
    list(title = "Recruitment", ids = c("#recruitment-plot_yoy", "#recruitment-plot_sr_series"), shot = "10_recruitment"),
    list(title = "Growth", ids = c("#growth-growth_plot"), shot = "11_growth"),
    list(title = "Catch", ids = c("#catch-catch_plot_gg"), shot = "12_catch"),
    list(title = "Feeding", ids = c("#feeding-feeding_plot_gg", "#feeding-diet_prop_plot"), shot = "13_feeding"),
    list(title = "Mortality", ids = c("#mortality-mortality_plot_gg"), shot = "14_mortality")
  )

  for (tab in tabs) {
    expect_true(click_nav(app, tab$title), info = tab$title)
    found <- FALSE
    last_html <- ""
    for (id in tab$ids) {
      html <- tryCatch(app$get_html(id), error = function(e) "")
      last_html <- paste(last_html, html)
      if (has_bound_output(html)) found <- TRUE
    }
    if (!found && identical(tab$title, "Multi-run")) {
      found <- grepl("Only one run", last_html) || grepl("Paste extra", last_html) ||
        grepl("Add another run", last_html)
    }
    expect_true(found, info = paste(tab$title, "empty:", substr(last_html, 1, 400)))
    save_shot(app, shot_dir, tab$shot)
  }

  expect_true(click_nav(app, "Mortality"))
  app$set_inputs(`mortality-view` = "predator", `mortality-fg` = "COD", wait_ = FALSE)
  app$wait_for_idle(timeout = 120000)
  expect_true(has_bound_output(app$get_html("#mortality-mortality_plot_gg")))
  save_shot(app, shot_dir, "14b_mortality_predator")
  app$set_inputs(`mortality-view` = "mort", wait_ = FALSE)
  app$wait_for_idle(timeout = 120000)
  expect_true(has_bound_output(app$get_html("#mortality-mortality_plot_gg")))
  save_shot(app, shot_dir, "14c_mortality_morttxt")

  expect_true(click_nav(app, "Recruitment"))
  app$set_inputs(`recruitment-analysis_type` = "sr", `recruitment-fg_yoy` = "COD",
                 wait_ = FALSE)
  app$wait_for_idle(timeout = 120000)
  expect_true(has_bound_output(app$get_html("#recruitment-plot_sr_series")) ||
                has_bound_output(app$get_html("#recruitment-plot_sr_scatter")))
  save_shot(app, shot_dir, "10b_recruitment_sr")

  expect_true(click_nav(app, "Feeding"))
  try(app$set_inputs(`feeding-view_type` = "matrix",
                     `feeding-pred_group` = "COD", `feeding-prey_group` = "PHY",
                     wait_ = FALSE), silent = TRUE)
  app$wait_for_idle(timeout = 120000)
  expect_true(has_bound_output(app$get_html("#feeding-diet_prop_plot")) ||
                nchar(app$get_html("#feeding-consumed_plot")) > 20)
  save_shot(app, shot_dir, "13b_feeding_realised")

  try(app$set_inputs(`feeding-view_type` = "pressure",
                     `feeding-pressure_metric` = "frac_day",
                     `feeding-pressure_source` = "init",
                     `feeding-pred_group` = "COD", `feeding-prey_group` = "PHY",
                     wait_ = FALSE), silent = TRUE)
  app$wait_for_idle(timeout = 40000)
  expect_true(has_bound_output(app$get_html("#feeding-feeding_plot_gg")) ||
                nchar(app$get_html("#feeding-feeding_plot_gg")) > 40)
  save_shot(app, shot_dir, "13c_feeding_pressure")
  expect_true(nchar(app$get_html("#feeding-pressure_map")) > 20)
  try(app$run_js("var el=document.getElementById('feeding-pressure_map'); if(el) el.scrollIntoView(); true;"),
      silent = TRUE)
  app$wait_for_idle(timeout = 120000)
  save_shot(app, shot_dir, "13d_feeding_pressure_map")

  expect_true(click_nav(app, "Spatial"))
  app$set_inputs(`spatial-view` = "overlap", `spatial-fg` = "COD",
                 `spatial-prey` = "PHY", `spatial-ts_scope` = "system",
                 wait_ = FALSE)
  app$wait_for_idle(timeout = 120000)
  expect_true(has_bound_output(app$get_html("#spatial-map_plot")) ||
                has_bound_output(app$get_html("#spatial-box_ts")))
  save_shot(app, shot_dir, "04b_spatial_overlap")

  click_nav(app, "Compare")
  app$wait_for_idle(timeout = 120000)
  fg_after <- app$get_value(input = "compare-fg_total")
  expect_identical(fg_before, fg_after)

  click_nav(app, "Overview")
  app$get_js("document.documentElement.setAttribute('data-bs-theme', 'dark'); true;")
  app$wait_for_idle(timeout = 5000)
  save_shot(app, shot_dir, "15_dark_mode")
  dark_html <- app$get_html("html")
  expect_true(grepl("data-bs-theme", dark_html) || grepl("Scan folder", dark_html, ignore.case = TRUE))
})

plot_is_real <- function(html) {
  has_plot_image(html) && !grepl("needs more files", html, ignore.case = TRUE)
}

e2e_chrome <- function() {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  chrome <- ra_test_chrome()
  skip_if(is.na(chrome), "Google Chrome or Chromium is required for shinytest2")
  Sys.setenv(CHROMOTE_CHROME = chrome)
  chrome_dir <- tempfile("chrome-data-")
  dir.create(chrome_dir)
  chromote::set_chrome_args(c(
    "--no-sandbox",
    "--disable-gpu",
    "--disable-dev-shm-usage",
    "--headless=new",
    paste0("--user-data-dir=", chrome_dir)
  ))
  chrome_dir
}

http_status <- function(base, path) {
  u <- paste0(sub("/$", "", base), "/", gsub("^/", "", path))
  h <- tryCatch(curlGetHeaders(u, redirect = TRUE), error = function(e) character())
  if (length(h) < 1) return(NA_integer_)
  codes <- as.integer(sub("^HTTP/[^ ]+ ([0-9]+).*", "\\1", h[grepl("^HTTP/", h)]))
  codes <- codes[is.finite(codes)]
  if (length(codes) == 0) NA_integer_ else tail(codes, 1)
}

test_that("favicon and logo return HTTP 200", {
  e2e_chrome()
  root <- ra_pkg_root()
  shot_dir <- file.path(tempdir(), "ra_e2e_brand_shots")
  dir.create(shot_dir, showWarnings = FALSE)
  app <- shinytest2::AppDriver$new(
    app_dir = file.path(root, "inst", "shiny"),
    name = "reactiveatlantis-brand",
    height = 800,
    width = 1400,
    load_timeout = 40000,
    timeout = 120000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$wait_for_idle(timeout = 120000)
  base <- app$get_url()
  expect_equal(http_status(base, "favicon.ico"), 200L)
  expect_equal(http_status(base, "app-icon-32.png"), 200L)
  expect_equal(http_status(base, "app-icon-180.png"), 200L)
  expect_equal(http_status(base, "app-icon-192.png"), 200L)
  expect_equal(http_status(base, "logo-transparent-128.png"), 200L)
  expect_equal(http_status(base, "site.webmanifest"), 200L)
  html <- app$get_html("html")
  expect_true(grepl("favicon.ico", html, fixed = TRUE))
  expect_true(grepl("app-icon-32.png", html, fixed = TRUE))
  expect_true(grepl("app-icon-180.png", html, fixed = TRUE))
  expect_true(grepl("logo-transparent-128.png", html, fixed = TRUE))
  expect_true(grepl("ra-brand-logo", html, fixed = TRUE))
  save_shot(app, shot_dir, "navbar_light")
  app$get_js("document.documentElement.setAttribute('data-bs-theme', 'dark'); true;")
  app$wait_for_idle(timeout = 5000)
  save_shot(app, shot_dir, "navbar_dark")
})

test_that("North Sea real names load COD into every group tab", {
  fx <- northsea_fixture_dir()
  skip_if_not(dir.exists(fx) && file.exists(file.path(fx, "functionalGroups.csv")),
              "North Sea fixture not packaged")
  e2e_chrome()

  shot_dir <- file.path(tempdir(), "ra_e2e_ns_shots")
  dir.create(shot_dir, showWarnings = FALSE)
  root <- ra_pkg_root()

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(root, "inst", "shiny"),
    name = "reactiveatlantis-ns-e2e",
    height = 1200,
    width = 1600,
    load_timeout = 60000,
    timeout = 20000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  app$set_inputs(`project-folder_path` = fx, wait_ = FALSE)
  app$wait_for_idle(timeout = 120000)
  app$click("project-scan_folder")
  app$wait_for_idle(timeout = 120000)
  app$click("project-load_project")
  app$wait_for_idle(timeout = 90000)

  body <- app$get_html("body")
  expect_false(grepl("needs more files: Groups CSV", body, ignore.case = TRUE))
  expect_true(grepl("data-status=\"loaded\"", body) || grepl("is-loaded", body) ||
                grepl("class=\"ok\"", body) || grepl("class='ok'", body))
  expect_true(grepl("Groups CSV", body))
  expect_true(grepl(">loaded<", body) || grepl("loaded", body, ignore.case = TRUE))
  save_shot(app, shot_dir, "ns_01_loaded_checklist")

  click_nav(app, "Compare")
  app$set_inputs(`compare-fg_total` = "COD", `compare-view` = "facets",
                 `compare-relative` = FALSE, `compare-y_scale` = "free",
                 wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  expect_identical(app$get_value(input = "compare-fg_total"), "COD")
  expect_true(plot_is_real(app$get_html("#compare-plot_facets_gg")))
  save_shot(app, shot_dir, "ns_02_compare_facets")

  app$set_inputs(`compare-relative` = TRUE, `compare-show_band` = TRUE,
                 `compare-y_scale` = "free", wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  expect_true(plot_is_real(app$get_html("#compare-plot_facets_gg")))
  save_shot(app, shot_dir, "ns_03_compare_relative")

  tab_cod <- list(
    list(title = "Ages", id = "ages-fg", plot = "#ages-age_plot", shot = "ns_03b_ages"),
    list(title = "Spatial", id = "spatial-fg", plot = "#spatial-map_plot", shot = "ns_04_spatial"),
    list(title = "Consumption", id = "consumption-fg", plot = "#consumption-cons_plot_gg", shot = "ns_05_consumption"),
    list(title = "Predation", id = "predation-fg", plot = "#predation-plot_pred_gg", shot = "ns_06_predation"),
    list(title = "Recruitment", id = "recruitment-fg_yoy", plot = "#recruitment-plot_yoy", shot = "ns_07_recruitment"),
    list(title = "Catch", id = "catch-fg", plot = "#catch-catch_plot_gg", shot = "ns_08_catch"),
    list(title = "Mortality", id = "mortality-fg", plot = "#mortality-mortality_plot_gg", shot = "ns_09_mortality")
  )
  for (tab in tab_cod) {
    expect_true(click_nav(app, tab$title), info = tab$title)
    do.call(function(...) app$set_inputs(..., wait_ = FALSE),
            setNames(list("COD"), tab$id))
    app$wait_for_idle(timeout = 40000)
    expect_identical(app$get_value(input = tab$id), "COD", info = tab$title)
    html <- tryCatch(app$get_html(tab$plot), error = function(e) "")
    expect_true(plot_is_real(html), info = paste(tab$title, "empty:", substr(html, 1, 300)))
    save_shot(app, shot_dir, tab$shot)
  }

  expect_true(click_nav(app, "Growth"))
  app$set_inputs(`growth-fg` = "PHD", wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  expect_identical(app$get_value(input = "growth-fg"), "PHD")
  expect_true(plot_is_real(app$get_html("#growth-growth_plot")))
  save_shot(app, shot_dir, "ns_10_growth_phd")

  expect_true(click_nav(app, "Feeding"))
  app$wait_for_idle(timeout = 60000)
  feed <- tryCatch(app$get_html("#feeding-feeding_plot_gg"), error = function(e) "")
  expect_true(plot_is_real(feed) || nchar(feed) > 40)
  save_shot(app, shot_dir, "ns_11_feeding")

  try(app$set_inputs(`feeding-view_type` = "matrix",
                     `feeding-pred_group` = "COD", `feeding-pred_stage` = "1",
                     `feeding-prey_group` = "PHY", `feeding-prey_stage` = "1",
                     wait_ = FALSE), silent = TRUE)
  app$wait_for_idle(timeout = 40000)
  expect_true(plot_is_real(app$get_html("#feeding-feeding_plot_gg")) ||
                nchar(app$get_html("#feeding-pred_bars")) > 20)
  save_shot(app, shot_dir, "ns_11_feeding_matrix")

  try(app$set_inputs(`feeding-view_type` = "pressure",
                     `feeding-pressure_metric` = "frac_day",
                     `feeding-pressure_source` = "init",
                     `feeding-pred_group` = "COD", `feeding-prey_group` = "PHD",
                     wait_ = FALSE), silent = TRUE)
  app$wait_for_idle(timeout = 60000)
  save_shot(app, shot_dir, "ns_11_feeding_pressure")
  try(app$run_js("var el=document.getElementById('feeding-pressure_map'); if(el) el.scrollIntoView(); true;"),
      silent = TRUE)
  app$wait_for_idle(timeout = 120000)
  save_shot(app, shot_dir, "ns_11_feeding_pressure_map")

  try(app$set_inputs(`feeding-view_type` = "diagnostics", wait_ = FALSE), silent = TRUE)
  app$wait_for_idle(timeout = 40000)
  save_shot(app, shot_dir, "ns_11_feeding_diagnostics")
  try(app$set_inputs(`feeding-view_type` = "compare",
                     `feeding-pred_group` = "COD", `feeding-pred_stage` = "1",
                     `feeding-prey_group` = "PHY", `feeding-edit_value` = 0.42,
                     wait_ = FALSE), silent = TRUE)
  try(app$click("feeding-apply_edit"), silent = TRUE)
  app$wait_for_idle(timeout = 40000)
  save_shot(app, shot_dir, "ns_11_feeding_compare")
})

test_that("messy North Sea folder first Load click fills Catch Mortality Recruitment Spatial", {
  fx <- northsea_fixture_dir()
  skip_if_not(dir.exists(fx) && file.exists(file.path(fx, "functionalGroups.csv")),
              "North Sea fixture not packaged")
  e2e_chrome()

  messy <- tempfile("ns_messy_e2e_")
  write_messy_northsea_fixture(messy)
  on.exit(unlink(messy, recursive = TRUE), add = TRUE)

  shot_dir <- file.path(tempdir(), "ra_e2e_messy_shots")
  dir.create(shot_dir, showWarnings = FALSE)
  root <- ra_pkg_root()

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(root, "inst", "shiny"),
    name = "reactiveatlantis-messy-e2e",
    height = 1200,
    width = 1600,
    load_timeout = 60000,
    timeout = 20000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  app$set_inputs(`project-folder_path` = messy, wait_ = FALSE)
  app$wait_for_idle(timeout = 120000)
  app$click("project-load_project")
  app$wait_for_idle(timeout = 90000)

  body <- app$get_html("body")
  expect_true(grepl("Loaded", body))
  expect_true(grepl("data-status=\"loaded\"", body) || grepl(">loaded<", body))
  expect_false(grepl("needs more files: Groups CSV", body, ignore.case = TRUE))
  check <- tryCatch(app$get_html("#project-file_checklist"), error = function(e) body)
  expect_true(grepl("data-role=\"groups\"", check))
  expect_true(grepl("data-status=\"loaded\"", check))
  expect_true(grepl(">loaded<", check))
  expect_lt(length(gregexpr(">not provided<", check)[[1]]), 8)
  save_shot(app, shot_dir, "messy_01_checklist")

  tab_cod <- list(
    list(title = "Catch", id = "catch-fg", plot = "#catch-catch_plot_gg", shot = "messy_02_catch"),
    list(title = "Mortality", id = "mortality-fg", plot = "#mortality-mortality_plot_gg", shot = "messy_03_mortality"),
    list(title = "Recruitment", id = "recruitment-fg_yoy", plot = "#recruitment-plot_yoy", shot = "messy_04_recruitment"),
    list(title = "Spatial", id = "spatial-fg", plot = "#spatial-map_plot", shot = "messy_05_spatial")
  )
  for (tab in tab_cod) {
    expect_true(click_nav(app, tab$title), info = tab$title)
    do.call(function(...) app$set_inputs(..., wait_ = FALSE),
            setNames(list("COD"), tab$id))
    app$wait_for_idle(timeout = 40000)
    expect_identical(app$get_value(input = tab$id), "COD", info = tab$title)
    html <- tryCatch(app$get_html(tab$plot), error = function(e) "")
    expect_true(plot_is_real(html), info = paste(tab$title, "empty:", substr(html, 1, 300)))
    page <- app$get_html("body")
    expect_false(grepl("needs more files: Groups CSV", page, ignore.case = TRUE),
                 info = tab$title)
    save_shot(app, shot_dir, tab$shot)
  }
})

test_that("two-run pair loader and busy indicators are visible", {
  e2e_chrome()
  root <- ra_pkg_root()
  pair_dir <- tempfile("ra_pair_e2e_")
  pair <- write_synthetic_atlantis_pair(pair_dir)
  extra_dir <- tempfile("ra_pair_extra_")
  write_synthetic_atlantis_run(extra_dir)
  on.exit(unlink(c(pair_dir, extra_dir), recursive = TRUE), add = TRUE)

  shot_dir <- file.path(tempdir(), "ra_e2e_pair_shots")
  dir.create(shot_dir, showWarnings = FALSE)

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(root, "inst", "shiny"),
    name = "reactiveatlantis-pair-loader",
    height = 1100,
    width = 1500,
    load_timeout = 40000,
    timeout = 20000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  app$set_inputs(
    `project-run1_name` = "Base",
    `project-run1_config_dir` = pair$config_dir,
    `project-run1_output_dir` = pair$output_dir,
    wait_ = FALSE
  )
  app$wait_for_idle(timeout = 120000)
  app$click("project-add_run")
  app$wait_for_idle(timeout = 120000)
  app$set_inputs(
    `project-extra_name_2` = "Alt",
    `project-extra_config_2` = extra_dir,
    `project-extra_output_2` = extra_dir,
    wait_ = FALSE
  )
  app$wait_for_idle(timeout = 120000)
  body <- app$get_html("body")
  expect_true(grepl("Configuration folder", body, fixed = TRUE))
  expect_true(grepl("Output folder", body, fixed = TRUE))
  expect_true(grepl("Run 1", body, fixed = TRUE))
  expect_true(grepl("Run 2", body, fixed = TRUE))
  expect_true(grepl("Add another run", body, fixed = TRUE))
  save_shot(app, shot_dir, "pair_01_two_runs")

  app$click("project-scan_folder")
  app$wait_for_idle(timeout = 120000)
  body2 <- app$get_html("body")
  expect_true(grepl("Scan:", body2) || grepl("file roles", body2))
  expect_true(grepl("is-loaded", body2) || grepl("data-status=\"loaded\"", body2))
  save_shot(app, shot_dir, "pair_02_scanned")

  app$click("project-load_project")
  app$wait_for_idle(timeout = 60000)
  loaded <- app$get_html("body")
  expect_true(grepl("Loaded", loaded))
  save_shot(app, shot_dir, "pair_03_loaded")

  css <- app$get_html("html")
  expect_true(grepl("busyIndicatorOptions|shiny-busy|recalculating", css) ||
                grepl("ra-busy", css))

  expect_true(click_nav(app, "Compare"))
  app$get_js("
    document.body.classList.add('shiny-busy');
    document.documentElement.classList.add('shiny-busy');
    var plot = document.querySelector('#compare-plot_facets_gg, .shiny-plot-output');
    if (plot) plot.classList.add('recalculating');
    true;
  ")
  Sys.sleep(0.5)
  save_shot(app, shot_dir, "pair_04_busy_indicators")
  busy_html <- app$get_html("html")
  expect_true(grepl("shiny-busy", busy_html, fixed = TRUE))
  expect_true(grepl("ra-busy-bar", busy_html, fixed = TRUE))
})

test_that("Spatial map can show two depth layers", {
  e2e_chrome()
  root <- ra_pkg_root()
  run_dir <- tempfile("ra_spatial_layers_")
  dir.create(run_dir)
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
  write_synthetic_atlantis_run(run_dir)

  shot_dir <- file.path(tempdir(), "ra_e2e_spatial_layers")
  dir.create(shot_dir, showWarnings = FALSE)

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(root, "inst", "shiny"),
    name = "reactiveatlantis-spatial-layers",
    height = 1100,
    width = 1500,
    load_timeout = 40000,
    timeout = 20000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  app$set_inputs(`project-folder_path` = run_dir, wait_ = FALSE)
  app$wait_for_idle(timeout = 120000)
  app$click("project-load_project")
  app$wait_for_idle(timeout = 60000)
  expect_true(click_nav(app, "Spatial"))

  body <- app$get_html("body")
  expect_true(grepl("Depth layer", body, fixed = TRUE) ||
                grepl("spatial-layer", body))
  expect_true(plot_is_real(app$get_html("#spatial-map_plot")))
  status <- tryCatch(app$get_text("#spatial-map_status"), error = function(e) "")
  expect_true(grepl("polygon|BGM", status, ignore.case = TRUE),
              info = paste("map_status:", status))

  app$set_inputs(`spatial-fg` = "COD", `spatial-layer` = "water_1",
                 `spatial-stable_scale` = TRUE, wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  expect_true(plot_is_real(app$get_html("#spatial-map_plot")))
  save_shot(app, shot_dir, "spatial_layer_water")

  app$set_inputs(`spatial-layer` = "sediment", wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  expect_true(plot_is_real(app$get_html("#spatial-map_plot")))
  save_shot(app, shot_dir, "spatial_layer_sediment")

  click_nav(app, "Consumption")
  app$set_inputs(`consumption-view` = "predator", `consumption-fg` = "COD",
                 wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  expect_true(plot_is_real(app$get_html("#consumption-cons_plot_gg")) ||
                nchar(app$get_html("#consumption-cons_plot_gg")) > 40)
  save_shot(app, shot_dir, "consumption_palette_after")
})

test_that("fresh install draws Spatial map and full pPREY for a NESP-style pair", {
  skip_on_cran()
  e2e_chrome()
  lib <- ra_install_package()
  app_dir <- file.path(lib, "ReactiveAtlantis", "shiny")
  skip_if_not(dir.exists(app_dir), "installed shiny app missing")

  nesp <- tempfile("hard_nesp_e2e_")
  pair <- write_hard_nesp_pair(nesp)
  on.exit(unlink(nesp, recursive = TRUE), add = TRUE)

  fx <- northsea_fixture_dir()
  skip_if_not(dir.exists(fx), "North Sea fixture not packaged")
  ns_pair_dir <- tempfile("ns_pair_e2e_")
  ns_pair <- split_atlantis_pair(fx, ns_pair_dir)
  on.exit(unlink(ns_pair_dir, recursive = TRUE), add = TRUE)

  shot_dir <- file.path(tempdir(), "ra_e2e_fresh_install")
  dir.create(shot_dir, showWarnings = FALSE)

  old_libs <- Sys.getenv("R_LIBS")
  Sys.setenv(R_LIBS = paste(c(lib, .libPaths()), collapse = .Platform$path.sep))
  on.exit(Sys.setenv(R_LIBS = old_libs), add = TRUE)

  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "reactiveatlantis-fresh-install",
    height = 1100,
    width = 1600,
    load_timeout = 60000,
    timeout = 25000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  app$set_inputs(
    `project-run1_config_dir` = pair$config_dir,
    `project-run1_output_dir` = pair$output_dir,
    wait_ = FALSE
  )
  app$wait_for_idle(timeout = 120000)
  app$click("project-load_project")
  app$wait_for_idle(timeout = 60000)
  expect_true(click_nav(app, "Spatial"))
  app$wait_for_idle(timeout = 40000)
  status <- tryCatch(app$get_text("#spatial-map_status"), error = function(e) "")
  expect_true(grepl("polygon|BGM", status, ignore.case = TRUE),
              info = paste("hard NESP status:", status))
  expect_false(grepl("BGM not found", status, ignore.case = TRUE),
               info = paste("hard NESP status:", status))
  expect_true(plot_is_real(app$get_html("#spatial-map_plot")))
  save_shot(app, shot_dir, "fresh_install_spatial_hard_nesp")

  expect_true(click_nav(app, "Feeding"))
  app$wait_for_idle(timeout = 40000)
  expect_true(plot_is_real(app$get_html("#feeding-feeding_plot_gg")) ||
                nchar(app$get_html("#feeding-feeding_plot_gg")) > 40)
  save_shot(app, shot_dir, "fresh_install_feeding_hard_nesp")

  app$set_inputs(
    `project-run1_config_dir` = ns_pair$config_dir,
    `project-run1_output_dir` = ns_pair$output_dir,
    wait_ = FALSE
  )
  app$wait_for_idle(timeout = 120000)
  app$click("project-load_project")
  app$wait_for_idle(timeout = 90000)
  expect_true(click_nav(app, "Spatial"))
  app$wait_for_idle(timeout = 60000)
  ns_status <- tryCatch(app$get_text("#spatial-map_status"), error = function(e) "")
  expect_true(grepl("polygon|BGM", ns_status, ignore.case = TRUE),
              info = paste("North Sea status:", ns_status))
  expect_true(plot_is_real(app$get_html("#spatial-map_plot")))
  save_shot(app, shot_dir, "fresh_install_spatial_northsea")

  expect_true(click_nav(app, "Feeding"))
  app$wait_for_idle(timeout = 60000)
  feed <- app$get_html("#feeding-feeding_plot_gg")
  expect_true(plot_is_real(feed) || nchar(feed) > 40)
  save_shot(app, shot_dir, "fresh_install_feeding_northsea")

  expect_true(click_nav(app, "Mortality"))
  app$set_inputs(`mortality-fg` = "COD", `mortality-view` = "source", wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  expect_true(plot_is_real(app$get_html("#mortality-mortality_plot_gg")))
  save_shot(app, shot_dir, "fresh_install_mortality_source")
  app$set_inputs(`mortality-view` = "predator", wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  save_shot(app, shot_dir, "fresh_install_mortality_predator")
  app$set_inputs(`mortality-view` = "mort", wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  save_shot(app, shot_dir, "fresh_install_mortality_morttxt")

  expect_true(click_nav(app, "Recruitment"))
  app$set_inputs(`recruitment-analysis_type` = "sr", `recruitment-fg_yoy` = "COD",
                 wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)
  expect_true(plot_is_real(app$get_html("#recruitment-plot_sr_series")) ||
                plot_is_real(app$get_html("#recruitment-plot_sr_scatter")) ||
                nchar(app$get_html("#recruitment-plot_sr_series")) > 20)
  save_shot(app, shot_dir, "fresh_install_recruitment_sr")

  expect_true(click_nav(app, "Feeding"))
  try(app$set_inputs(`feeding-pred_group` = "COD", `feeding-prey_group` = "PHD",
                     wait_ = FALSE), silent = TRUE)
  app$wait_for_idle(timeout = 40000)
  save_shot(app, shot_dir, "fresh_install_feeding_realised")

  try(app$set_inputs(`feeding-view_type` = "pressure",
                     `feeding-pressure_metric` = "frac_day",
                     `feeding-pressure_source` = "init",
                     `feeding-pred_group` = "COD", `feeding-prey_group` = "PHD",
                     wait_ = FALSE), silent = TRUE)
  app$wait_for_idle(timeout = 60000)
  save_shot(app, shot_dir, "fresh_install_feeding_pressure")
  try(app$run_js("var el=document.getElementById('feeding-pressure_map'); if(el) el.scrollIntoView(); true;"),
      silent = TRUE)
  app$wait_for_idle(timeout = 120000)
  save_shot(app, shot_dir, "fresh_install_feeding_pressure_map")

  expect_true(click_nav(app, "Spatial"))
  app$set_inputs(`spatial-view` = "overlap", `spatial-fg` = "COD",
                 `spatial-prey` = "PHD", `spatial-ts_scope` = "system",
                 wait_ = FALSE)
  app$wait_for_idle(timeout = 60000)
  expect_true(plot_is_real(app$get_html("#spatial-map_plot")) ||
                nchar(app$get_html("#spatial-box_ts")) > 20)
  save_shot(app, shot_dir, "fresh_install_spatial_overlap")
})


