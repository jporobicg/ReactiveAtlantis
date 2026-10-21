## Regression: exported Ask Atlantis helpers must work from an *installed*
## package (locked namespace). Source-tree / load_all sessions hide this.

test_that("download_atlantis_manuals works after R CMD INSTALL", {
  skip_on_cran()
  lib <- ra_install_package()
  dest <- tempfile("ra-installed-corpus-")
  got <- ra_installed_rscript(c(
    sprintf("lib <- %s", deparse(lib)),
    sprintf("dest <- %s", deparse(dest)),
    "dir.create(dest, recursive = TRUE, showWarnings = FALSE)",
    "library(ReactiveAtlantis, lib.loc = lib)",
    "stopifnot(environmentIsLocked(asNamespace('ReactiveAtlantis')))",
    "options(reactiveatlantis.download_file = function(url, destfile) {",
    "  writeBin(charToRaw('%PDF-1.4 mock'), destfile)",
    "  list(ok = TRUE, status = 200L, url = url, error = NULL, bytes = 14)",
    "})",
    "res <- download_atlantis_manuals(dest_dir = dest, overwrite = TRUE)",
    "stopifnot(isTRUE(res$ok))",
    "stopifnot(file.exists(file.path(dest, 'AtlantisUserGuide_PartI.pdf')))",
    "stopifnot(file.exists(file.path(dest, 'AtlantisUserGuide_PartII.pdf')))",
    "stopifnot(environmentIsLocked(asNamespace('ReactiveAtlantis')))",
    "idx <- tempfile('ra-installed-idx-')",
    "dir.create(idx)",
    "man <- system.file('ai', 'sources.json', package = 'ReactiveAtlantis')",
    "index_ask_atlantis(sources_config = man, index_dir = idx, embed = FALSE)",
    "diagnose_ask_atlantis_index(index_dir = idx)",
    "cat('DOWNLOAD_OK\\n')"
  ), lib = lib)

  expect_equal(got$status, 0L, info = paste(got$out, collapse = "\n"))
  expect_true(any(grepl("DOWNLOAD_OK", got$out, fixed = TRUE)),
              info = paste(got$out, collapse = "\n"))
})

test_that("enabled app loads Ask Atlantis from the installed package", {
  skip_on_cran()
  lib <- ra_install_package()
  got <- ra_installed_rscript(c(
    sprintf("lib <- %s", deparse(lib)),
    "library(ReactiveAtlantis, lib.loc = lib)",
    "Sys.setenv(RA_AI_ENABLED = 'true')",
    "Sys.setenv(RA_AI_SOURCES_CONFIG = system.file(",
    "  'ai', 'sources.atlantis-manuals.json', package = 'ReactiveAtlantis'))",
    "app_dir <- system.file('shiny', package = 'ReactiveAtlantis')",
    "stopifnot(nzchar(app_dir), dir.exists(app_dir))",
    "e <- new.env(parent = globalenv())",
    "oldwd <- setwd(app_dir)",
    "sys.source(file.path(app_dir, 'app.R'), envir = e, keep.source = FALSE)",
    "setwd(oldwd)",
    "stopifnot(isTRUE(e$ra_ai_on))",
    "stopifnot(is.function(e$assistant_ui))",
    "ui <- as.character(e$assistant_ui('assistant'))",
    "stopifnot(any(grepl('ra-ai-fab', ui, fixed = TRUE)))",
    "stopifnot(any(grepl('ra-ai-drawer', ui, fixed = TRUE)))",
    "cat('APP_OK\\n')"
  ), lib = lib)
  expect_equal(got$status, 0L, info = paste(got$out, collapse = "\n"))
  expect_true(any(grepl("APP_OK", got$out, fixed = TRUE)),
              info = paste(got$out, collapse = "\n"))
})

test_that("installed app can download, enable, launch, and open the drawer", {
  skip_on_cran()
  ai_e2e_chrome()
  lib <- ra_install_package()
  dest <- tempfile("ra-installed-corpus-")
  prep <- ra_installed_rscript(c(
    sprintf("lib <- %s", deparse(lib)),
    sprintf("dest <- %s", deparse(dest)),
    "dir.create(dest, recursive = TRUE, showWarnings = FALSE)",
    "library(ReactiveAtlantis, lib.loc = lib)",
    "options(reactiveatlantis.download_file = function(url, destfile) {",
    "  writeBin(charToRaw('%PDF-1.4 mock'), destfile)",
    "  list(ok = TRUE, status = 200L, url = url, error = NULL, bytes = 14)",
    "})",
    "res <- download_atlantis_manuals(dest_dir = dest, overwrite = TRUE)",
    "stopifnot(isTRUE(res$ok))",
    "cat('PREP_OK\\n')"
  ), lib = lib)
  expect_equal(prep$status, 0L, info = paste(prep$out, collapse = "\n"))

  old_paths <- .libPaths()
  on.exit(.libPaths(old_paths), add = TRUE)
  .libPaths(c(lib, old_paths))
  old_en <- Sys.getenv("RA_AI_ENABLED", unset = NA)
  old_src <- Sys.getenv("RA_AI_SOURCES_CONFIG", unset = NA)
  old_url <- Sys.getenv("RA_OLLAMA_URL", unset = NA)
  on.exit({
    if (is.na(old_en)) Sys.unsetenv("RA_AI_ENABLED") else Sys.setenv(RA_AI_ENABLED = old_en)
    if (is.na(old_src)) Sys.unsetenv("RA_AI_SOURCES_CONFIG") else {
      Sys.setenv(RA_AI_SOURCES_CONFIG = old_src)
    }
    if (is.na(old_url)) Sys.unsetenv("RA_OLLAMA_URL") else {
      Sys.setenv(RA_OLLAMA_URL = old_url)
    }
  }, add = TRUE)
  Sys.setenv(RA_AI_ENABLED = "true")
  Sys.setenv(RA_AI_SOURCES_CONFIG = system.file(
    "ai", "sources.atlantis-manuals.json",
    package = "ReactiveAtlantis", lib.loc = lib
  ))
  Sys.setenv(RA_OLLAMA_URL = "http://127.0.0.1:1")

  app_dir <- system.file("shiny", package = "ReactiveAtlantis", lib.loc = lib)
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "ask-atlantis-installed",
    height = 900, width = 1500,
    load_timeout = 40000, timeout = 20000
  )
  on.exit(try(app$stop(), silent = TRUE), add = TRUE)
  app$wait_for_idle(timeout = 20000)
  body <- app$get_html("body")
  expect_true(grepl("ra-ai-fab", body, fixed = TRUE))
  expect_true(grepl("ra-ai-drawer", body, fixed = TRUE))
  drawer <- tryCatch(app$get_html("#assistant-drawer"), error = function(e) "")
  expect_false(grepl("is-open", drawer, fixed = TRUE))

  app$click(selector = "#assistant-fab")
  app$wait_for_idle(timeout = 10000)
  drawer_open <- tryCatch(app$get_html("#assistant-drawer"), error = function(e) "")
  expect_true(grepl("is-open", drawer_open, fixed = TRUE))
  expect_true(grepl("Guidance only", drawer_open, ignore.case = TRUE))
  save_ai_shot(app, "ask_atlantis_installed_drawer_open")
})

