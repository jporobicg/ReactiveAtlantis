# Ask Atlantis (optional local guidance)

A small, optional panel in the unified ReactiveAtlantis app. It answers
questions from a **local** index (manuals, code, calibration notes) using a
**local** Ollama model, and it points you at existing tabs. It does not run
Atlantis, edit parameters, or replace any plot or table in the app.

Disabled by default (`RA_AI_ENABLED=false`). When it is off, or when Ollama
is missing, the rest of the app behaves as before.

![Ask Atlantis offline](../../man/figures/ask_atlantis.png)

## Set up (developers)

1. Install [Ollama](https://ollama.com) and pull a chat model plus embeddings:

   ```bash
   ollama pull qwen3.5:4b
   ollama pull nomic-embed-text
   ollama list
   ```

   A smaller chat model such as `qwen3.5:2b` is fine. Set `RA_OLLAMA_MODEL`
   to whatever you pulled. Do not hard-code a single family in your own
   wrappers; the app already reads the environment.

2. Install optional R packages (Suggests): `httr2`, `jsonlite`, `RSQLite`,
   `DBI`. For PDF manuals, also `pdftools`.

3. Point the sources manifest at your corpus. The packaged
   `inst/ai/sources.json` indexes a **fixture** corpus plus this package’s
   own source. Copy `inst/ai/sources.example.json` and set
   `RA_AI_SOURCES_CONFIG` when real Atlantis manuals/source/calibration notes
   are available.

4. Build the index (does not run on every app start):

   ```r
   library(ReactiveAtlantis)
   index_ask_atlantis()              # skip unchanged files
   diagnose_ask_atlantis_index()
   ```

   Or from the package root:

   ```bash
   Rscript inst/ai/index.R --no-embed
   Rscript inst/ai/index.R --diagnose
   ```

5. Enable the panel and launch:

   ```r
   Sys.setenv(RA_AI_ENABLED = "true")
   # optional:
   # Sys.setenv(RA_OLLAMA_URL = "http://127.0.0.1:11434")
   # Sys.setenv(RA_OLLAMA_MODEL = "qwen3.5:4b")
   # Sys.setenv(RA_OLLAMA_EMBED_MODEL = "nomic-embed-text")
   # Sys.setenv(RA_AI_INDEX_DIR = "/path/to/index")
   # Sys.setenv(RA_AI_SOURCES_CONFIG = "/path/to/sources.json")
   launch_reactiveatlantis()
   ```

## Troubleshooting

| Symptom | What to check |
| --- | --- |
| No Ask Atlantis tab | `RA_AI_ENABLED` is not `true` |
| Badge says Offline | Ollama is not running on `RA_OLLAMA_URL` |
| Badge says Model missing | `ollama pull` the name in `RA_OLLAMA_MODEL` |
| Empty or weak answers | Index missing or still on fixtures; run `diagnose_ask_atlantis_index()` |
| PDF pages missing | Install `pdftools`; inspect ingest_log failures |
| App frozen | Should not happen; generation uses `ExtendedTask` and a timeout. Other tabs stay usable. |

Nothing is sent to a cloud LLM. If Ollama is down, the panel says so; it does
not switch to a remote API.

## Evaluation questions

`inst/ai/evaluation/questions.json` is a 24-question set (success, ambiguous,
unsupported, adversarial) with expected source types, nav ids, and a 0–2
rubric. Retrieval-only scoring:

```bash
Rscript inst/ai/evaluation/run_eval.R
```

Add `--model` only when a live Ollama chat model is installed.

## Security / scientific limits

- Read-only: no file writes, no shell, no simulation launch from the chat.
- Navigation targets and citation ids are checked in R before display.
- Retrieved text is treated as data, not as instructions.
- Calibration notes are examples, not universal parameter rules.
- The assistant cannot see plot pixels or NetCDF values from the loaded run.
