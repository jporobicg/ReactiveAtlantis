# Phase 3: installation, package check and existing tests (2026-10-25)

## Environment

- **Host:** estigia, Ubuntu 26.04.1, R 4.5.2. Load average was about 34 during the runs: 14–18 Atlantis runs were going.
- **R packages:** from apt (`r-cran-*`), except `proj4` 1.0.15, `shinytest2` 0.5.1, `chromote` 0.5.1, `rbgm` 0.2.0 and `plotly` 4.12.1, which came from CRAN (see S16 for why plotly). shiny 1.10.0, promises 1.3.2, testthat 3.3.2.
- **Browser:** Playwright Chromium 1248, run with `--no-sandbox` because AppArmor blocks user namespaces for user-installed browsers.

## Installation

| Method | Result |
|---|---|
| README command `devtools::install_github("jporobicg/ReactiveAtlantis")` | Installs **master `bb37888`**. It has no `predation_pressure.R` and no `inst/ai`. **FAIL** for the release candidate (S01). |
| `install_github(..., ref = "cursor/ask-atlantis-assistant-b4d2")` | Installs `92974ba`. `launch_reactiveatlantis` is exported. PASS |
| Local source `R CMD build` | Builds in 3 s. PASS |

## R CMD check --as-cran --no-manual

**Status: 1 ERROR, 1 WARNING, 5 NOTEs.** The full log is in `evidence/phase3/check.log`.

- **ERROR, tests:** `find_pkg_root` cannot find the source tree, so no tests run under check (S13).
- **WARNING:** `launch_reactiveatlantis` and `download_atlantis_html_manual` are undocumented (S04).
- **NOTEs:**
  - CRAN incoming feasibility.
  - Hidden files.
  - Top-level files.
  - Imports `bslib`, `plotly` and `visNetwork` are not imported.
  - `plot.*` functions are not registered as S3 methods (S15).

## Existing test suite

The suite was run from the source tree, on a scratch copy, so the screenshots the tests write into `man/figures` do not touch the repository.

| Run | Result |
|---|---|
| All files, default | **1,095 expectations, 0 failed, 0 errors, 10 skipped.** All 10 skips are browser tests: Chrome is only looked for at hard-coded paths (S14). |
| Browser tests with Chrome supplied (scratch patch) | 4 of 7 app tests and 1 Guide test errored. The cause was apt's broken plotly JS (S16). |
| After installing CRAN plotly | The same 4 app tests still errored, with idle timeouts at 15–20 s. |
| With 120 s timeouts | **All 7 app e2e tests pass (156 expectations). The Guide drawer test still errors (S17).** |

## Interpretation

The suite is **green, but it does not validate the science**:

- It asserts the current behaviour, including D01 (transposed SpecificPredMort): `test-process-diagnostics.R:36-55` passes.
- The NetCDF fixtures are synthetic: 3 boxes, constant values.

Browser coverage depends on machine-specific Chrome paths, and it found a real P1 (S17) once it was able to run.
