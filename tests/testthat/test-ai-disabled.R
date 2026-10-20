## When Ask Atlantis is off, the unified app must not show the panel.

test_that("Ask Atlantis strings are not in the default navbar construction", {
  source_ask_atlantis()
  old <- Sys.getenv("RA_AI_ENABLED", unset = NA)
  on.exit({
    if (is.na(old)) Sys.unsetenv("RA_AI_ENABLED") else Sys.setenv(RA_AI_ENABLED = old)
  }, add = TRUE)
  Sys.setenv(RA_AI_ENABLED = "false")
  expect_false(ra_ai_enabled())
  app <- readLines(file.path(ra_pkg_root(), "inst", "shiny", "app.R"), warn = FALSE)
  expect_true(any(grepl("ra_ai_on", app)))
  expect_true(any(grepl("RA_AI_ENABLED", app)))
})
