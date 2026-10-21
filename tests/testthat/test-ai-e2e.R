## shinytest2 coverage for the optional Ask Atlantis drawer.

Sys.setenv(NOT_CRAN = "true")


click_nav <- function(app, title) {
  app$get_js(sprintf(
    "var el = Array.from(document.querySelectorAll('a.nav-link')).find(function(a){return (a.textContent||'').trim()===%s}); if (el) el.click(); true;",
    deparse(title)
  ))
  app$wait_for_idle(timeout = 20000)
}

test_that("Ask Atlantis button and drawer are absent when disabled", {
  ai_e2e_chrome()
  old <- Sys.getenv("RA_AI_ENABLED", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RA_AI_ENABLED") else Sys.setenv(RA_AI_ENABLED = old)
  }, add = TRUE)
  Sys.setenv(RA_AI_ENABLED = "false")

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(ra_pkg_root(), "inst", "shiny"),
    name = "ask-atlantis-disabled",
    height = 800, width = 1400,
    load_timeout = 40000, timeout = 15000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$wait_for_idle(timeout = 15000)
  body <- app$get_html("body")
  expect_false(grepl("ra-ai-fab", body, fixed = TRUE))
  expect_false(grepl("ra-ai-drawer", body, fixed = TRUE))
  expect_false(grepl('data-value="Ask Atlantis"', body, fixed = TRUE))
  expect_true(grepl("Overview", body, fixed = TRUE))
  expect_true(grepl("Predation", body, fixed = TRUE))
})

test_that("Ask Atlantis drawer is closed by default and survives tab switches", {
  ai_e2e_chrome()
  old <- Sys.getenv("RA_AI_ENABLED", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RA_AI_ENABLED") else Sys.setenv(RA_AI_ENABLED = old)
  }, add = TRUE)
  Sys.setenv(RA_AI_ENABLED = "true")
  Sys.setenv(RA_OLLAMA_URL = "http://127.0.0.1:1")

  app <- shinytest2::AppDriver$new(
    app_dir = file.path(ra_pkg_root(), "inst", "shiny"),
    name = "ask-atlantis-drawer",
    height = 900, width = 1500,
    load_timeout = 40000, timeout = 20000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$wait_for_idle(timeout = 20000)
  body <- app$get_html("body")
  expect_true(grepl("ra-ai-fab", body, fixed = TRUE))
  expect_true(grepl("ra-ai-drawer", body, fixed = TRUE))
  expect_false(grepl('data-value="Ask Atlantis"', body, fixed = TRUE))
  drawer <- tryCatch(app$get_html("#assistant-drawer"), error = function(e) "")
  expect_false(grepl("is-open", drawer, fixed = TRUE))
  save_ai_shot(app, "ask_atlantis_drawer_closed")

  app$click(selector = "#assistant-fab")
  app$wait_for_idle(timeout = 10000)
  drawer_open <- tryCatch(app$get_html("#assistant-drawer"), error = function(e) "")
  expect_true(grepl("is-open", drawer_open, fixed = TRUE))
  expect_true(grepl("Guidance only", drawer_open, ignore.case = TRUE))
  expect_true(grepl("Offline|Model missing|Ollama", drawer_open, ignore.case = TRUE))

  app$set_inputs(`assistant-question` = "Where do I inspect predation?",
                 wait_ = FALSE)
  app$click("assistant-send")
  app$wait_for_idle(timeout = 30000)
  after_send <- tryCatch(app$get_html("#assistant-drawer"), error = function(e) "")
  expect_true(grepl("Where do I inspect predation", after_send, fixed = TRUE))
  expect_true(grepl("Ollama|local language model|not running|could not", after_send,
                    ignore.case = TRUE))
  expect_true(grepl("Open Predation", after_send, fixed = TRUE))

  click_nav(app, "Predation")
  still <- tryCatch(app$get_html("#assistant-drawer"), error = function(e) "")
  expect_true(grepl("is-open", still, fixed = TRUE))
  expect_true(grepl("Where do I inspect predation", still, fixed = TRUE))
  expect_true(grepl("Open Predation", still, fixed = TRUE))
  page <- app$get_html("body")
  expect_true(grepl("Predation", page, fixed = TRUE))
  save_ai_shot(app, "ask_atlantis_drawer_open")

  app$click(selector = "#assistant-drawer .ra-open-tool")
  app$wait_for_idle(timeout = 15000)
  page2 <- app$get_html("body")
  expect_true(grepl("Predation", page2, fixed = TRUE))

  click_nav(app, "Overview")
  app$wait_for_idle(timeout = 15000)
  ov <- app$get_html("body")
  expect_true(grepl("Load a run|Overview|Groups", ov, ignore.case = TRUE))
  kept <- tryCatch(app$get_html("#assistant-drawer"), error = function(e) "")
  expect_true(grepl("Where do I inspect predation", kept, fixed = TRUE))
})
