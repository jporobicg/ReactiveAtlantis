# Ask Atlantis implementation report

Optional local guidance assistant for the unified ReactiveAtlantis Shiny app.
It answers questions from a local SQLite/FTS index using a local Ollama model
and points users at existing tabs. It does not run Atlantis, write files, or
replace any calibration tool.

## Files changed

### Application integration
- `inst/shiny/app.R` — source helpers; add a floating **Ask Atlantis** button and right-hand drawer only when `RA_AI_ENABLED=true`; `id = "ra_page"` for the current tab. No extra navbar item.
- `inst/shiny/modules/assistant_module.R` — floating button, collapsible right drawer, chat UI/server (status badge, starters, sources, validated Open-tab buttons, `ExtendedTask` when available). Conversation is one session-scoped module so it survives tab changes.
- `inst/shiny/www/custom.css` — floating button and right-drawer styling aligned with navy/teal/sand.

### Backend (sourced from `inst/ai/load.R`)
- `inst/ai/R/ai_config.R` — environment variables and paths (`RA_AI_*`, `RA_OLLAMA_*`).
- `inst/ai/R/ai_ollama.R` — local HTTP client (`/api/chat`, `/api/embed`); mockable transport; no cloud fallback.
- `inst/ai/R/ai_store.R` — SQLite + FTS5 store.
- `inst/ai/R/ai_ingest.R` — manifest ingest, heading/function/PDF chunking, incremental re-index.
- `inst/ai/R/ai_retrieve.R` — hybrid FTS + embeddings with source-type weights.
- `inst/ai/R/ai_nav.R` — catalog of real tabs; citation/nav validation.
- `inst/ai/R/ai_prompt.R` — policy prompt plus application-level scope/injection/action gates.
- `inst/ai/R/ai_answer.R` — answer pipeline.

### Package API and docs
- `R/ai.R` — exported `index_ask_atlantis()`, `diagnose_ask_atlantis_index()`, `download_atlantis_manuals()`.
- `man/index_ask_atlantis.Rd`, `man/diagnose_ask_atlantis_index.Rd`, `man/download_atlantis_manuals.Rd`
- `NAMESPACE`, `DESCRIPTION` (Suggests: `httr2`, `jsonlite`, `RSQLite`, `DBI`, `pdftools`)
- `README.md` — short pointer to `inst/ai/README.md`
- `inst/ai/README.md` — setup and troubleshooting
- `inst/ai/architecture.md` — hook points and assumptions
- `inst/ai/sources.json` (fixture corpus for tests), `inst/ai/sources.example.json`
- `inst/ai/atlantis-manuals.json` — CSIRO user-guide URLs (not the PDF bytes)
- `inst/ai/sources.atlantis-manuals.json` — indexer manifest pointing at `${RA_AI_CORPUS_DIR}`
- `inst/ai/R/ai_download.R`, `inst/ai/download_atlantis_manuals.R` — local download helper
- `inst/ai/index.R` — CLI indexer
- `man/figures/ask_atlantis.png` — enabled-offline panel

### Fixture corpus (not official Atlantis materials)
- `inst/ai/fixtures/README.md` and manuals / stand-in C / calibration notes under `inst/ai/fixtures/`

### Tests and evaluation
- `tests/testthat/helper-ai.R`
- `tests/testthat/test-ai-assistant.R`
- `tests/testthat/test-ai-disabled.R`
- `tests/testthat/test-ai-e2e.R`
- `inst/ai/evaluation/questions.json` (24 questions)
- `inst/ai/evaluation/run_eval.R`

## Validation commands

From the package root, with Suggests installed:

```r
# Unit tests for the assistant (HTTP mocked; FTS index on the fixture corpus)
testthat::test_file("tests/testthat/test-ai-assistant.R")
testthat::test_file("tests/testthat/test-ai-disabled.R")

# Existing scientific unit tests
testthat::test_file("tests/testthat/test-north-sea-fixes.R")

# shinytest2 (Google Chrome required)
testthat::test_file("tests/testthat/test-ai-e2e.R")
testthat::test_file("tests/testthat/test-e2e-app.R")

# Retrieval-only evaluation set (no Ollama)
# Rscript inst/ai/evaluation/run_eval.R
# Rscript inst/ai/evaluation/run_eval.R --sources inst/ai/sources.atlantis-manuals.json
```

## Test results (this workspace)

| Suite | Result |
| --- | --- |
| `test-ai-assistant.R` | PASS 119 |
| `test-ai-disabled.R` | PASS 4 |
| `test-ai-e2e.R` | PASS 22 |
| `test-north-sea-fixes.R` | PASS 343 |
| `test-e2e-app.R` | PASS 95 |
| `Rscript inst/ai/evaluation/run_eval.R` | retrieval mapping ran on all 24 questions |

Covered relative to the brief:

1. Ollama missing: informative Offline badge and chat error; Open-tab still validated; other tabs load.
2. Manual parameter (KLP) retrieved from fixture `parameters.md` with a real citation.
3. Fixture `consume_fixture.c` cited with path and `FIXTURE-code-build-X`.
4. Recruitment / Predation navigation matches `app.R`; `SuperCalibrator` is dropped.
5. Calibration notes labelled as examples.
6. No simulation numbers without data.
7. Out-of-scope, injection, and “rerun Atlantis for me” blocked in R (not only the system prompt).
8. History is an argument / per-session `reactiveVal`; two histories do not leak.
9. Incremental re-index skips unchanged files and picks up edits.
10. Default `RA_AI_ENABLED=false`: no button or drawer; existing e2e still passes.

## Live Ollama run

**No.** `ollama` is not installed in this environment, and `http://127.0.0.1:11434` refused connections. Generation and embeddings were exercised through `options(reactiveatlantis.ollama_transport = ...)` in unit tests. The Shiny drawer was checked live against a deliberately unreachable URL (`RA_OLLAMA_URL=http://127.0.0.1:1`): Offline badge, error message, and a validated **Open Predation** button.

Screenshots: drawer closed with the floating button on Overview, and the drawer open on Predation (`ask_atlantis_drawer_closed.png`, `ask_atlantis_drawer_open.png`). `man/figures/ask_atlantis.png` is the open-drawer view.

## Corpus status

**Fixture corpus (tests):** `inst/ai/sources.json` still indexes the marked `FIXTURE-NOT-OFFICIAL` tree plus this package’s own source. Unit tests keep using that.

**CSIRO user guides (local only, not in git):** listed on https://research.csiro.au/atlantis/home/links/. Copyright CSIRO (Part II: “© 2017 CSIRO and Asta Audzijonyte … no part of this publication covered by copyright may be reproduced or copied … except with the permission of CSIRO”). CSIRO’s website notice allows a single local copy for research/study; the PDFs are **not** committed.

| Manual | Listed URL | Working URL | Size | Pages indexed |
| --- | --- | --- | --- | --- |
| Part I (ecology), May 2025 | 2025/05 `AtlantisUserGuide_PartI.pdf` | same (HTTP 200, 11683805 bytes) | 233 pages | 623 chunks |
| Part II (socio-economics), June 2025 | 2024/03 `AtlantisUserGuide_PartII.pdf` (**404**) | Useful References 2025/06 (HTTP 200, 2268375 bytes) | 110 pages | 268 chunks |

Not downloaded: Wiley overview paper (paywall). Wiki is relocating. Atlantis C source and calibration notes are still local-path placeholders.

Download: `Rscript inst/ai/download_atlantis_manuals.R --dest ...` then index with `RA_AI_SOURCES_CONFIG=inst/ai/sources.atlantis-manuals.json` (`max_file_bytes` 25 MB). Index in this environment: 1213 chunks (891 manual + 322 ReactiveAtlantis), embeddings 0 (no Ollama).

**Extraction / unreadable pages**

- Part I pages 6, 146, 220: page-number only (<20 characters of text).
- Part II page 84: empty.
- All other pages extracted with `pdftools`. No encrypted files. No ingest failures (`status=ok`).

**Evaluation set vs CSIRO manuals** (retrieval only, no Ollama)

Against the manuals+RA index, **all 24 questions** had at least one CSIRO passage in the top 8. **12 questions** had a CSIRO PDF as the top hit: q02, q08, q13, q15–q21, q23, q24.

Success questions that now retrieve real user-guide passages (including when the top hit is still RA source):

- q01 KLP — Part I p.157 table: `KLP_XXX` = min gape limit (top hit remained `pprey.mat.R`; CSIRO was in top-k).
- q02 pPREY — Part I p.147 “Ensuring meaningful prey choice and diets” and p.218 pPREY ontogenetic diet matrices (**top hit CSIRO**).
- q08 decline without higher F — Part II §16 Management (**top hit CSIRO**).
- q13 temperature/nitrate — Part I passages retrieved; top hit was a figure page, not the physics chapter.
- q23 declining biomass / processes — Part I findings pages (**top hit CSIRO**).
- q24 mL vs fishing mortality — Part I growth-effects / mortality discussion (**top hit CSIRO**).

Against the **fixture** corpus, `real_manual` is 0/24 (as designed).

The Wiley overview paper on that page was skipped (paywall). Atlantis source and calibration notes are still fixtures unless you point `sources.example.json` at local trees.

## Limitations / follow-on

- Chat is not streamed (`stream: false`).
- PDF extraction needs `pdftools`; unreadable pages are logged.
- The assistant cannot see plot pixels or NetCDF values from the loaded run (by design).
- Embedding quality was not measured on a real model here.
- Default models (`qwen3.5:4b`, `nomic-embed-text`) are documented defaults only; any Ollama chat/embed pair works via env vars.
