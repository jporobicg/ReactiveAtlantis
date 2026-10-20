## shinytest2 coverage for the optional Ask Atlantis panel.

Sys.setenv(NOT_CRAN = "true")

test_that("Ask Atlantis is absent from the navbar when disabled", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not(file.exists("/usr/local/bin/google-chrome") ||
                file.exists("/usr/bin/google-chrome-stable") ||
                file.exists("/usr/bin/google-chrome"),
              "Google Chrome is required for shinytest2")
  old <- Sys.getenv("RA_AI_ENABLED", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RA_AI_ENABLED") else Sys.setenv(RA_AI_ENABLED = old)
  }, add = TRUE)
  Sys.setenv(RA_AI_ENABLED = "false")

  chrome <- if (file.exists("/usr/local/bin/google-chrome")) {
    "/usr/local/bin/google-chrome"
  } else if (file.exists("/usr/bin/google-chrome-stable")) {
    "/usr/bin/google-chrome-stable"
  } else {
    "/usr/bin/google-chrome"
  }
  Sys.setenv(CHROMOTE_CHROME = chrome)
  chrome_dir <- tempfile("chrome-data-")
  dir.create(chrome_dir)
  chromote::set_chrome_args(c(
    "--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage",
    "--headless=new", paste0("--user-data-dir=", chrome_dir)
  ))

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(ra_pkg_root(), "inst", "shiny"),
    name = "ask-atlantis-disabled",
    height = 800, width = 1400,
    load_timeout = 40000, timeout = 15000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$wait_for_idle(timeout = 15000)
  body <- app$get_html("body")
  expect_false(grepl("Ask Atlantis", body, fixed = TRUE))
  expect_true(grepl("Overview", body, fixed = TRUE))
  expect_true(grepl("Predation", body, fixed = TRUE))
})

test_that("Ask Atlantis enabled-offline panel shows status and does not break Overview", {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not(file.exists("/usr/local/bin/google-chrome") ||
                file.exists("/usr/bin/google-chrome-stable") ||
                file.exists("/usr/bin/google-chrome"),
              "Google Chrome is required for shinytest2")
  old <- Sys.getenv("RA_AI_ENABLED", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RA_AI_ENABLED") else Sys.setenv(RA_AI_ENABLED = old)
  }, add = TRUE)
  Sys.setenv(RA_AI_ENABLED = "true")
  ## Force the health check away from any local daemon.
  Sys.setenv(RA_OLLAMA_URL = "http://127.0.0.1:1")

  chrome <- if (file.exists("/usr/local/bin/google-chrome")) {
    "/usr/local/bin/google-chrome"
  } else if (file.exists("/usr/bin/google-chrome-stable")) {
    "/usr/bin/google-chrome-stable"
  } else {
    "/usr/bin/google-chrome"
  }
  Sys.setenv(CHROMOTE_CHROME = chrome)
  chrome_dir <- tempfile("chrome-data-")
  dir.create(chrome_dir)
  chromote::set_chrome_args(c(
    "--no-sandbox", "--disable-gpu", "--disable-dev-shm-usage",
    "--headless=new", paste0("--user-data-dir=", chrome_dir)
  ))

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(ra_pkg_root(), "inst", "shiny"),
    name = "ask-atlantis-offline",
    height = 900, width = 1400,
    load_timeout = 40000, timeout = 20000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$wait_for_idle(timeout = 20000)
  body <- app$get_html("body")
  expect_true(grepl("Ask Atlantis", body, fixed = TRUE))

  clicked <- tryCatch({
    app$get_js("var el = Array.from(document.querySelectorAll('a.nav-link')).find(function(a){return (a.textContent||'').trim()==='Ask Atlantis'}); if (el) el.click(); true;")
    TRUE
  }, error = function(e) FALSE)
  expect_true(clicked)
  app$wait_for_idle(timeout = 20000)
  panel <- app$get_html("body")
  expect_true(grepl("Offline|Model missing|Ollama|Guidance only", panel,
                    ignore.case = TRUE))
  expect_true(grepl("What can Reactive Atlantis do", panel, ignore.case = TRUE))

  save_ai_shot <- function(app, name) {
    shot_dir <- file.path(tempdir(), "ra_ai_shots")
    dir.create(shot_dir, showWarnings = FALSE)
    shot <- file.path(shot_dir, paste0(name, ".png"))
    try(app$get_screenshot(shot), silent = TRUE)
    art <- "/opt/cursor/artifacts"
    if (dir.exists(art) && file.exists(shot)) {
      file.copy(shot, file.path(art, paste0(name, ".png")), overwrite = TRUE)
    }
    man_fig <- file.path(ra_pkg_root(), "man", "figures")
    if (identical(name, "ask_atlantis_offline") && dir.exists(man_fig) &&
        file.exists(shot)) {
      file.copy(shot, file.path(man_fig, "ask_atlantis.png"), overwrite = TRUE)
    }
    shot
  }
  save_ai_shot(app, "ask_atlantis_offline")

  app$set_inputs(`assistant-question` = "Where do I inspect predation?",
                 wait_ = FALSE)
  app$click("assistant-send")
  app$wait_for_idle(timeout = 30000)
  after <- app$get_html("body")
  expect_true(grepl("Ollama|local language model|not running|could not", after,
                    ignore.case = TRUE))
  save_ai_shot(app, "ask_atlantis_offline_error")

  app$get_js("var el = Array.from(document.querySelectorAll('a.nav-link')).find(function(a){return (a.textContent||'').trim()==='Overview'}); if (el) el.click(); true;")
  app$wait_for_idle(timeout = 15000)
  ov <- app$get_html("body")
  expect_true(grepl("Load a run|Overview|Groups", ov, ignore.case = TRUE))
})
