# Implementation audit (Phase 0)

Verified against this checkout on 2026-10-21 (commit `11c4ee7` and the
files listed below). Nothing in this note assumes a tool exists just
because a standalone launcher is exported. Navigation targets are the
fourteen `nav_panel()` titles in `inst/shiny/app.R`.

This is a reuse-first plan for evolving the existing Ask Atlantis drawer
into the Atlantis Scientific Guide. There is no second chat stack.

## Package and Shiny structure

- Package: `ReactiveAtlantis` 0.0.2.0, GPL-3, `R/`, `inst/shiny/`, `inst/ai/`.
- Entry: `launch_reactiveatlantis()` in `R/launch_app.R` runs `inst/shiny/app.R`.
- App: `bslib::page_navbar(..., id = "ra_page")` plus a Run sidebar from
  `inst/shiny/modules/project_store.R`.
- Shared analysis helpers live in `analysis_helpers.R`, `layers.R`,
  `colors.R`, `nc_cache.R`, `ui_helpers.R`, `theme.R`.
- Optional assistant is sourced from `inst/ai/load.R` into an **unlocked**
  environment. The installed namespace is locked (`R/ai.R` already works
  around that).
- The drawer is `assistant_ui()` / `assistant_server()` in
  `inst/shiny/modules/assistant_module.R`. It is added only when
  `RA_AI_ENABLED` is truthy (default **false**). It is a floating button
  plus a closed-by-default overlay, not a navbar tab.

## Existing Ollama, prompts, and index (reuse)

| Piece | File | Status |
| --- | --- | --- |
| Config / paths / `max_chunks` | `inst/ai/R/ai_config.R` | Reuse. Default `max_chunks` was 8; Guide uses 3–6. |
| Ollama HTTP (`/api/chat`, `/api/embed`) | `inst/ai/R/ai_ollama.R` | Reuse. Local only; no cloud fallback. |
| System prompt + scope gate | `inst/ai/R/ai_prompt.R` | Extend JSON keys; keep injection / action / domain gates. |
| Answer pipeline | `inst/ai/R/ai_answer.R` | Extend; keep citation and nav validation. |
| Hybrid FTS + embeddings | `inst/ai/R/ai_retrieve.R` | Reuse; add dedup and authority ranking. |
| SQLite + FTS5 store | `inst/ai/R/ai_store.R` | Reuse. Incremental hash/mtime skip already exists. |
| Ingest / chunking | `inst/ai/R/ai_ingest.R` | Reuse. Add embed-model invalidation. |
| Nav catalog | `inst/ai/R/ai_nav.R` | Reuse as the only allowed destinations. |
| CSIRO PDF download | `inst/ai/R/ai_download.R` | Keep. PDFs stay uncommitted. |
| Exports | `R/ai.R` | `index_ask_atlantis()`, `diagnose_ask_atlantis_index()`, `download_atlantis_manuals()`. |
| Fixture corpus | `inst/ai/sources.json` | Keep for tests. |

Graceful degradation already exists: missing index returns an empty
chunk set; unreachable Ollama returns `ok = FALSE` with a readable
message; disabled mode never mounts the drawer.

## Verified UI tools (unified app)

Confirmed `nav_panel()` titles and module ids in `inst/shiny/app.R`:

| tool_id | Title | Module | Module inputs (verified) | Outputs |
| --- | --- | --- | --- | --- |
| overview | Overview | `overview` | (none) | `cards`, `run_window`, `health_table`, `cohort_table` |
| compare | Compare | `compare` | `group_type`, `search`, `relative`, `show_band`, `y_scale`, `view` (`facets`/`total`/`age`/`age_bars`), `fg_total`, `show_biomass`, `show_numbers`, `scaled`, `limit_axis`, `fg_age`, `show_biomass_age`, `show_numbers_age`, `scaled_age`, `limit_axis_age` | `plot_facets_gg`, `plot_total`, `plot_age`, `plot_age_bars` |
| ages | Ages | `ages` | `fg`, `variable` | `age_plot` |
| spatial | Spatial | `spatial` | `view` (`map`/`age`), `kind`, `fg`, `tracer`, `layer`, `time_i`, `play`, `play_speed`, `ts_scope`, `stable_scale`, `highlight_boundary`, `box`, `show_biomass`, `show_resn`, `show_structn`, `show_numbers` | `map_plot`, `box_ts`, `age_plot` |
| consumption | Consumption | `consumption` | `view`, `fg`, `top_n` | `cons_plot_gg` |
| physics | Physics | `physics` | `tracer`, `box`, `all_boxes` | `tracer_plot`, `dz_plot` |
| ensemble | Multi-run | `ensemble` | `kind`, `fg` | `ens_plot` |
| predation | Predation | `predation` | `fg`, `stock`, `threshold`, `top_n`, `scaled`, `time_age` | `plot_pred_gg`, `plot_prey_gg`, `plot_age` |
| foodweb | Food web | `foodweb` | `focal_fg`, `max_connections`, `min_proportion`, `time_step`, `stock` | `network_gg`, `trophic_table` |
| recruitment | Recruitment | `recruitment` | `analysis_type` (`yoy`/`ssb`/`growth`), `fg_yoy`, `new_alpha`, `new_beta`, `recalc`, `fg_growth`, `box`, `layer_prop`, `box_prop`, `log_scale` | `plot_yoy`, `plot_relative_yoy`, `plot_ssb`, `plot_pp_growth` |
| growth | Growth | `growth` | `fg`, `box`, `layer`, `all_layers`, `log_scale` | `growth_plot` |
| catch | Catch | `catch` | `view` (`by_group`/`by_fleet`/`numbers`/`skill`), `fishery`, `fg`, `by_year` | `catch_plot_gg`, `skill_table` |
| feeding | Feeding | `feeding` | `view_type` (`matrix`/`diagnostics`/`compare`), `pred_group`, `pred_stage`, `prey_group`, `prey_stage`, `diet_eps`, `edit_value`, `apply_edit`, `reset_edits` | `feeding_plot_gg`, `pred_bars`, `prey_bars`, `diag_plot`, `compare_plot`, `bug_table` |
| mortality | Mortality | `mortality` | `fg` | `mortality_plot_gg` |

`TOOL_REQUIREMENTS` in `project_store.R` (file roles a tab needs):

- overview: `groups`
- compare: `groups` (also biomindx or output_nc for series)
- ages: `groups`, `output_nc`
- spatial: `groups`, `output_nc`, `bgm`
- consumption: `groups`, `prod_nc`
- physics: `output_nc`
- ensemble: `groups`
- predation: `groups`, `biomindx`, `dietcheck`
- foodweb: `groups`, `dietcheck`
- recruitment: `groups`, `output_nc`, `yoy`, `bio_prm`
- growth: `groups`, `output_nc`, `bio_prm`
- catch: `groups`, `fisheries`, `catch_nc`
- feeding: `groups`, `bio_prm`, `init_nc`
- mortality: `groups`, `specific_mort`, `specific_pred_mort`

### What does **not** exist as a unified-app tab

These names are **real** as standalone exported launchers
(`NAMESPACE`): `predation()`, `growth.pp()`, `recruitment.cal()`,
`mortality()`, `compare()`, `feeding.mat()`, `food.web()`, `catch()`.
They are **not** `nav_panel` destinations and must not be recommended
as `tool_id` values.

`skill_assessment` exists inside `R/catch.R` and the Catch module
(`view = "skill"`). It is **not** exported and is **not** a separate
tab. The Guide may mention Catch → Skill vs observed, never a
`skill_assessment` navigation target.

Feeding no longer has a filter/reduction view. The live views are
matrix, diagnostics, and compare.

## How runs are loaded and what stays in memory

Pipeline (all in `project_store.R`):

1. `collect_run_pairs()` — named config/output pairs from the sidebar.
2. `detect_run_pair(config_dir, output_dir)` — fills `FILE_ROLES`.
3. `merge_paths()` — uploads override detection.
4. `load_project(paths)` — eager tables; DietCheck deferred; NetCDF
   handles stay closed until a tab asks.
5. `ensure_project_data()` / `ensure_diet()` / `ensure_inf_box()` —
   lazy open on demand.

`FILE_ROLES` (20): `groups`, `fisheries`, `output_nc`, `output_nc_old`,
`catch_nc`, `init_nc`, `prod_nc`, `biomindx`, `dietcheck`, `diet_bypol`,
`ssb`, `yoy`, `bio_prm`, `run_prm`, `force_prm`, `bgm`, `specific_mort`,
`specific_pred_mort`, `ext_catch`, `obs_csv`.

Eager `load_project()` fields: `paths`, `grp`, `fish`, `biomindx`,
`yoy`, `ssb`, `spe_mort`, `pred_mort`, `prm_bio`, `prm_run`, `bgm_path`,
`inf_box` (when cheap), `fg_palette`, `ext_catch`, `observations`,
`diet_deferred`, `errors`. NetCDF slots (`nc_out`, `nc_old`, `nc_catch`,
`nc_init`, `nc_prod`) start `NULL`. DietCheck is not dumped into the
assistant.

Safe Guide context may include: current tab, selected group code,
group code/name table, run pair labels, and which file **roles exist
on disk**. It must not include NetCDF arrays, DietCheck matrices, or
plot pixels.

## Tests and dependencies

- Suggests for the assistant: `httr2`, `jsonlite`, `RSQLite`, `DBI`,
  `pdftools`.
- Unit: `tests/testthat/test-ai-assistant.R` (fixture corpus, mock
  Ollama), `test-ai-disabled.R`, `test-ai-installed.R`.
- e2e: `test-ai-e2e.R` (drawer absent when off; closed by default;
  Open-tool; history survives tab switch).
- Scientific tabs: `test-north-sea-fixes.R`, `test-e2e-app.R`.
- Eval: `inst/ai/evaluation/questions.json` (27 cases at audit time)
  and `run_eval.R` (retrieval-only unless `--model`).

Constraints that Phase 1–2 must not break:

- `RA_AI_ENABLED` default false; no navbar tab named Ask Atlantis.
- Nav catalog titles == `nav_panel()` titles.
- Invented destinations (e.g. SuperCalibrator) dropped in R.
- Collapse / FDO questions stay in-scope with structured hypotheses.
- Helpers source into an unlocked environment only.
- CSIRO PDFs are never committed.

## Gaps relative to the Scientific Guide spec

1. Docs live under `inst/ai/` only; `docs/ai/` did not exist.
2. No machine-readable source registry with authority tier, rights,
   checksum, ingest/review status.
3. No reviewed diagnostic case cards or concept IDs.
4. No validated tool-guidance registry beyond `ra_ai_nav_catalog()`.
5. Answer JSON is only `answer`, `claim_kinds`, `citations`,
   `nav_targets` — missing `what_is_known`, `possible_explanations`,
   `suggested_checks`, `recommended_tool_ids`, `limitations`,
   `evidence_status`, `follow_up_question`.
6. Session context is page + selected group + groups table only; no
   file-role availability; Ages/Feeding group inputs are not mapped.
7. Retrieval returns up to 8 passages; no explicit 3–6 cap or
   near-duplicate drop; embed-model change does not wipe vectors.
8. Drawer title is still “Ask Atlantis”; only three starter questions;
   answers render as a single escaped paragraph.
9. Eval set is 27 cases, not 60–100, and does not score tool top-1/3
   or evidence status.

## Reuse-first plan

1. Keep one drawer, one Ollama client, one SQLite index, one
   `ra_ai_answer()` pipeline.
2. Add `ai_tools.R` (deterministic question → tool ranking) and
   `ai_cases.R` / `ai_context.R` sourced from the same `load.R`.
3. Index curated knowledge markdown through the existing manifest.
4. Validate new JSON fields in R; reject unknown `tool_id`s and
   citations that were not retrieved.
5. Render sections in the existing drawer (never raw JSON).
6. Grow `questions.json` and keep the fixture corpus as the test
   source of truth. No fine-tuning. No second model stack.

Read-only invariants stay in application code: no file writes, no
parameter edits, no code execution, no internet at question time, no
cloud model.
