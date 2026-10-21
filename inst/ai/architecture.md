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

- Core R lives under `inst/ai/R/` and is sourced from `inst/ai/load.R`
  into an unlocked environment (a child of the package namespace for
  exported wrappers, or the Shiny app environment). The installed
  namespace is locked, so helpers must not be `source()`d into `topenv()`.
- `inst/shiny/app.R` sources that loader and `modules/assistant_module.R`.
- When `RA_AI_ENABLED` is true, a floating speech-bubble button and a
  right-hand drawer are added to the page header (fixed overlay). Default is
  false, so neither the button nor the drawer is in the DOM, and the navbar
  is unchanged.
- The drawer is closed by default (`translateX(100%)`, `pointer-events: none`)
  so plots and controls stay fully usable. One module instance holds the
  conversation for the session, so switching tabs does not clear chat.
- `page_navbar(..., id = "ra_page")` exposes the current tab to the assistant
  as safe metadata. Group codes are read from known module input ids only.
- Navigation suggestions are validated against `ra_ai_nav_catalog()`, which
  is tested against `nav_panel()` titles in `app.R`. The UI may click a real
  tab via a custom message from the drawer; invented targets are dropped in R
  before render.
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

The default manifest (`inst/ai/sources.json`) indexes a fixture tree under
`inst/ai/fixtures/` marked `FIXTURE-NOT-OFFICIAL`, plus this package’s own
README, `R/`, Shiny modules, and a navigation catalog from the live tab list.
Tests keep using that fixture corpus.

CSIRO Atlantis user-guide PDFs are linked from
https://research.csiro.au/atlantis/home/links/ (Part I ecology, Part II human
components). They are CSIRO copyright: download a local copy for research or
study, but do **not** commit the PDF files. `inst/ai/atlantis-manuals.json`
holds the URLs. `download_atlantis_manuals()` / `inst/ai/download_atlantis_manuals.R`
writes them to `RA_AI_CORPUS_DIR`. Index with
`inst/ai/sources.atlantis-manuals.json` (`max_file_bytes` is 25 MB because
Part I is ~11 MB). The Wiley overview paper on that page is a paywall
journal article and is not downloaded.

Atlantis C source and calibration notes still need a local path; copy
`inst/ai/sources.example.json` and set `RA_AI_SOURCES_CONFIG`.

## Assumptions

- Scientists who enable the panel have Ollama on localhost. The app never
  binds or proxies that port.
- `qwen3.5:4b` and `nomic-embed-text` are development defaults, not the only
  supported models.
- Chat history is a per-session `reactiveVal` of bounded length.
- Model HTML is escaped. Citations render only after id checks against the
  index.
