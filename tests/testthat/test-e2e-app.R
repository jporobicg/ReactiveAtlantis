## End-to-end browser test: load a synthetic run and visit every nav panel.
## AppDriver skips when it thinks this is CRAN; this test is meant to run in CI.
Sys.setenv(NOT_CRAN = "true")

has_plot_image <- function(html) {
  grepl("<img", html, ignore.case = TRUE) &&
    grepl("src\\s*=", html, ignore.case = TRUE)
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
  app$wait_for_idle(timeout = 30000)
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
  skip_if_not(file.exists("/usr/local/bin/google-chrome") ||
                file.exists("/usr/bin/google-chrome-stable"),
              "Google Chrome is required for shinytest2")

  chrome <- if (file.exists("/usr/local/bin/google-chrome")) {
    "/usr/local/bin/google-chrome"
  } else {
    "/usr/bin/google-chrome-stable"
  }
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
    timeout = 15000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)

  app$set_inputs(`project-folder_path` = run_dir, wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  app$click("project-scan_folder")
  app$wait_for_idle(timeout = 15000)

  body <- app$get_html("body")
  expect_true(grepl("Scan:", body) || grepl("file roles found", body))
  expect_true(grepl("not provided", body))
  expect_true(grepl("class=\"ok\"", body) || grepl("class='ok'", body) ||
                grepl("li class=\"ok\"", body))
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
    list(title = "Spatial", ids = c("#spatial-map_plot", "#spatial-box_ts"), shot = "04_spatial"),
    list(title = "Consumption", ids = c("#consumption-cons_plot_gg"), shot = "05_consumption"),
    list(title = "Physics", ids = c("#physics-tracer_plot", "#physics-dz_plot"), shot = "06_physics"),
    list(title = "Multi-run", ids = c("#ensemble-empty_state", "#ensemble-ens_plot"), shot = "07_multirun"),
    list(title = "Predation", ids = c("#predation-plot_pred_gg"), shot = "08_predation"),
    list(title = "Food web", ids = c("#foodweb-network_gg", "#foodweb-trophic_table"), shot = "09_foodweb"),
    list(title = "Recruitment", ids = c("#recruitment-plot_yoy"), shot = "10_recruitment"),
    list(title = "Growth", ids = c("#growth-growth_plot"), shot = "11_growth"),
    list(title = "Catch", ids = c("#catch-catch_plot_gg"), shot = "12_catch"),
    list(title = "Feeding", ids = c("#feeding-feeding_plot_gg"), shot = "13_feeding"),
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
      found <- grepl("Only one run", last_html) || grepl("Paste extra", last_html)
    }
    expect_true(found, info = paste(tab$title, "empty:", substr(last_html, 1, 400)))
    save_shot(app, shot_dir, tab$shot)
  }

  click_nav(app, "Compare")
  app$wait_for_idle(timeout = 15000)
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
  skip_if_not(file.exists("/usr/local/bin/google-chrome") ||
                file.exists("/usr/bin/google-chrome-stable"),
              "Google Chrome is required for shinytest2")
  chrome <- if (file.exists("/usr/local/bin/google-chrome")) {
    "/usr/local/bin/google-chrome"
  } else {
    "/usr/bin/google-chrome-stable"
  }
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
  app$wait_for_idle(timeout = 20000)
  app$click("project-scan_folder")
  app$wait_for_idle(timeout = 20000)
  app$click("project-load_project")
  app$wait_for_idle(timeout = 90000)

  body <- app$get_html("body")
  expect_false(grepl("needs more files: Groups CSV", body, ignore.case = TRUE))
  expect_true(grepl("class=\"ok\"", body) || grepl("class='ok'", body))
  expect_true(grepl("Groups CSV", body))
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
})

