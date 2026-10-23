# Atlantis Scientific Guide — maintenance

## Commands

```bash
# Models (once; not at question time)
ollama pull qwen3.5:4b
ollama pull nomic-embed-text
ollama list

# Index (from the package root)
Rscript inst/ai/index.R --no-embed
Rscript inst/ai/index.R --diagnose
# or, after library(ReactiveAtlantis):
# index_ask_atlantis(); diagnose_ask_atlantis_index()

# CSIRO PDFs (not committed)
Rscript inst/ai/download_atlantis_manuals.R
export RA_AI_SOURCES_CONFIG="$(pwd)/inst/ai/sources.atlantis-manuals.json"
Rscript inst/ai/index.R --no-embed

# Maintained HTML manual (not committed)
# download_atlantis_html_manual() then index the corpus dir

# Evaluation (fixture corpus; no live model)
Rscript inst/ai/evaluation/run_eval.R
```

Environment variables: `RA_AI_ENABLED`, `RA_OLLAMA_URL`,
`RA_OLLAMA_MODEL`, `RA_OLLAMA_EMBED_MODEL`, `RA_AI_INDEX_DIR`,
`RA_AI_SOURCES_CONFIG`, `RA_AI_CORPUS_DIR`.

Changing the embedding model name invalidates stored vectors on the
next `index_ask_atlantis(embed = TRUE)`.

## Add or review a diagnostic case

1. Add a card under `inst/ai/knowledge/cases/` with a stable `case_id`.
2. Register it in `inst/ai/knowledge/cases.json` (observation,
   hypotheses with expected and counter evidence, checks,
   interpretation, what_not_to_conclude, `tool_ids`, `sources`,
   `review_status`).
3. Use only `tool_ids` that exist in `inst/ai/tools.json`.
4. Do not write a one-symptom–one-cause card or a universal
   “increase parameter X” rule.
5. Set `review_status` to `pending` until a person has read it.
6. Rebuild the index and add at least one eval question that ranks the
   intended tools.

## Add a source

1. Add a row to `inst/ai/source_registry.json` and
   `docs/ai/SOURCE_REGISTRY.md` (all registry fields).
2. Check rights. If the file cannot be committed, add a download
   helper and a local path under `RA_AI_CORPUS_DIR`.
3. Point `sources.json` or an overlay manifest at the local path.
4. Run `index_ask_atlantis()` and `diagnose_ask_atlantis_index()`.
5. Record `ingest_status`, checksum, `retrieved_at`, and review.

Never commit CSIRO PDFs or the historical Confluence PDF.

## Add an evaluation question

Edit `inst/ai/evaluation/questions.json`. Required: `id`, `kind`,
`question`, `expect`. Use only real `nav_ids`. Run
`Rscript inst/ai/evaluation/run_eval.R` and update
`docs/ai/EVALUATION_REPORT.md` if metrics move.

## Tests

```r
testthat::test_file("tests/testthat/test-ai-assistant.R")
testthat::test_file("tests/testthat/test-ai-scientific-guide.R")
```

Keep `RA_AI_ENABLED` default false. Do not add a navbar tab.
