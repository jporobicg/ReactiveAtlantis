## When Ask Atlantis is off, the unified app must not show the drawer.

test_that("Ask Atlantis is not a navbar tab and is gated on RA_AI_ENABLED", {
  source_ask_atlantis()
  old <- Sys.getenv("RA_AI_ENABLED", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RA_AI_ENABLED") else Sys.setenv(RA_AI_ENABLED = old)
  }, add = TRUE)
  Sys.setenv(RA_AI_ENABLED = "false")
  expect_false(ra_ai_enabled())
  app <- readLines(file.path(ra_pkg_root(), "inst", "shiny", "app.R"), warn = FALSE)
  expect_false(any(grepl('nav_panel\\("Ask Atlantis"', app)))
  expect_true(any(grepl("assistant_ui\\(\"assistant\"\\)", app)))
  expect_true(any(grepl("ra_ai_on", app)))
})
