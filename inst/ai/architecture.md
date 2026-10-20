# Ask Atlantis architecture note

## What this is

A read-only question-answering panel for the unified ReactiveAtlantis Shiny
app. It explains concepts from a local corpus and points users at **existing**
tabs. It does not calibrate, run Atlantis, write files, or compute new
diagnostics.

## Application shape (as inspected)

- Package entry: `launch_reactiveatlantis()` in `R/launch_app.R` runs
  `inst/shiny/app.R`.
- The app is a `bslib::page_navbar` with a left **Run** sidebar
  (`project_store.R`) and these tabs, confirmed in `app.R`:
  Overview, Compare, Spatial, Consumption, Physics, Multi-run, Predation,
  Food web, Recruitment, Growth, Catch, Feeding, Mortality.
- Shared run state lives in `project_server()`; each tool is a Shiny module.
- Selected functional group is per-module (`compare-fg_total`, `predation-fg`,
  …). There is no single global “active group”.
- Tests: `tests/testthat/test-north-sea-fixes.R` (unit) and
  `test-e2e-app.R` (shinytest2). Those suites must keep passing with the
  assistant off.

## Integration hook

- Core R lives under `inst/ai/R/` and is sourced from `inst/ai/load.R`.
- `inst/shiny/app.R` sources that loader and `modules/assistant_module.R`.
- The **Ask Atlantis** nav panel is appended only when `RA_AI_ENABLED` is
  true. Default is false, so the navbar is unchanged.
- `page_navbar(..., id = "ra_page")` exposes the current tab to the assistant
  as safe metadata. Group codes are read from known module input ids only.
- Navigation suggestions are validated against `ra_ai_nav_catalog()`, which
  is tested against `nav_panel()` titles in `app.R`. The UI may click a real
  tab via a custom message; invented targets are dropped in R before render.
- LLM calls run in `shiny::ExtendedTask` when available so a slow Ollama
  request does not freeze other tabs. Timeouts still apply.

## Backend

- Ollama HTTP only: `POST /api/chat` (`stream: false`) and `POST /api/embed`,
  with `/api/embeddings` as a fallback. No cloud substitute.
- Transport is `httr2` (Suggests). Tests inject
  `options(reactiveatlantis.ollama_transport = ...)`.
- Index: SQLite + FTS5 (`RSQLite` / `DBI`, Suggests) under
  `tools::R_user_dir("ReactiveAtlantis", "cache")/ask-atlantis` unless
  `RA_AI_INDEX_DIR` is set.
- Retrieval: FTS5 first, embeddings when present, source-type weights so
  manuals and code outrank calibration examples.
- Chunking: headings for text, function boundaries for code, page + heading
  for PDFs (`pdftools` optional; failures are logged, not dropped).

## Corpus

Real Atlantis manuals, Atlantis C source, and calibration notes were **not**
supplied in this repository. The default manifest
(`inst/ai/sources.json`) indexes:

1. A fixture tree under `inst/ai/fixtures/` marked `FIXTURE-NOT-OFFICIAL`.
2. ReactiveAtlantis’s own README, `R/`, and `inst/shiny/modules`.
3. A navigation catalog generated from the live tab list.

Replace fixture paths by copying `inst/ai/sources.example.json` and setting
`RA_AI_SOURCES_CONFIG`.

## Assumptions

- Scientists who enable the panel have Ollama on localhost. The app never
  binds or proxies that port.
- `qwen3.5:4b` and `nomic-embed-text` are development defaults, not the only
  supported models.
- Chat history is a per-session `reactiveVal` of bounded length.
- Model HTML is escaped. Citations render only after id checks against the
  index.
