# Feature inventory (PR #4 head 92974ba)

`M/` = `inst/shiny/modules/`. The test status is the same for every row: nothing has been executed yet (Phase 1 is static).

## Launch and loading

| Feature | Code | Notes |
|---|---|---|
| `launch_reactiveatlantis(host, port, launch.browser, max_upload_mb=8192, project_dir, config_dir, output_dir)` | R/launch_app.R:32-58 | Only sets `options(reactiveatlantis.*)` and the upload limit, then calls `runApp(system.file("shiny"))` |
| Default sidebar folders | M/project_store.R:749-756 | `project_dir` fills both config and output when they are not given |
| File detection | project_store.R:390-445 | By filename regex, shortest basename first. `output_nc` is "any .nc that is not CATCH/init/_ini/restart/PROD", so a forcing file such as `temp.nc` can win. `output_nc_old` is never auto-detected. CSVs are classified by header and `.prm` files by tokens. |
| Config/output pairing | project_store.R:453-496 | Config roles are taken from the config folder and output roles from the output folder. A file found only in the other folder is used and recorded as a fallback. |
| BGM search | project_store.R:360-385 | Walks parent folders and `geometry/`, `bgm/`, using names found in the `.prm` |
| Eager vs lazy reads | project_store.R:687-745 | Groups, fisheries, BiomIndx, YOY, SSB, Mort tables and prm are read at load; NetCDF and DietCheck are read later |
| Tool requirements | project_store.R:49-64; ui_helpers.R:159 | A tab whose files are missing shows a "needs" card |
| Uploads | project_store.R:820-838 | 19 `up_*` fileInputs |
| Extra runs | analysis_helpers.R:1504 (`load_run_lite`) | `add_run` adds config/output pairs |
| NetCDF cache | M/nc_cache.R:44-78 | Handles and arrays are cached per path + mtime + size. Handles are never closed and the cache has no bound. |

## Tabs

| Tab / view | Inputs (IDs) | Files | Computation | Units shown |
|---|---|---|---|---|
| Overview: cards | — | output.nc | overview_module.R:54,102 | Boxes, layers, start date |
| Overview: health table | — | BiomIndx | `biomass_health` project_store.R:518-555. "Extinct" if final/initial < 0.01, "Exploding" if > 10 | ratio |
| Overview: problem cohorts | — | output.nc | `problem_cohorts` analysis_helpers.R:188-243. Mean SN/RN last/first outside 0.5–1.5 | ratio |
| Compare: facets / total / age / age_bars | `group_type, search, relative, show_band, y_scale, view, fg_total, show_biomass, show_numbers, scaled, limit_axis, fg_age` | BiomIndx, or output.nc + BGM | `biomindx_facet_df` 309, `nc_facet_biomass` 348, `bio.age/pool/pwn` 403-484, `nitro.weight` 493 | t, or Bt/B0 with a 0.5–1.5 band |
| Ages | `fg, variable` | output.nc | `age_class_long` analysis_helpers.R:116. Nums summed; RN/SN as numbers-weighted means | mg N, numbers, t |
| Spatial: map | `view, kind, fg, tracer, layer, time_i, play, play_speed, ts_scope, stable_scale, highlight_boundary, box, dl_map, dl_csv` | output.nc, BGM | `spatial_box_values` analysis_helpers.R:844; `read_bgm_polygons` 686 (rbgm fallback 473); join on 0-based boxid 736 | t per box, or concentration |
| Spatial: by age | `fg`, age | output.nc, BGM | idem | numbers / t |
| Spatial: overlap | `fg, prey` | output.nc, BGM | `spatial_overlap_index` process_diagnostics.R:351. Schoener's D per time, plus a per-box 2·min/(a+b) | 0–1 |
| Spatial: diagnostics panel | — | — | `diagnose_spatial_map` analysis_helpers.R:936 | text |
| Physics | `tracer, box, all_boxes` | output.nc | `physics_long` analysis_helpers.R:1424; `layer_thickness_long` layers.R:188 | tracer units, `nominal_dz` |
| Multi-run | `kind` (biomass/ssb/yoy/catch), `fg` | each run's BiomIndx/SSB/YOY/CATCH | `ensemble_table` 1582, `ensemble_stats` (mean/min/max when > 2 runs) | t / numbers |
| Feeding: pPREY matrix | `view_type, pred_group, pred_stage, prey_group, prey_stage, edit_value, apply_edit, reset_edits`, plotly click | bio prm, groups | `read_pprey_matrix` feeding_module.R:1444; `text2num` fallback 148-175 | pPREY 0–1 |
| Feeding: gape / realised-feed overlap | idem | prm, init.nc | `feed_bio_struct`, `gape.func`, `Over.mat.func`, `Bio.ages` 304-344 | flag |
| Feeding: predation pressure | `pressure_metric (frac_day/t_day/frac_year), pressure_source (init/output), pressure_time_i, pressure_box` | prm, groups, init.nc or output.nc, PROD, DietCheck, SpecificPredMort | `expected_predation_pressure` predation_pressure.R:521; `init_stage_stocks` 308; `holling_ingest_mgn_day` 181; flags at 5 %/day, above standing stock, above production (1016); `pressure_vs_realised` 1056; per-box map 904 | t/day, fraction/day, fraction/year |
| Feeding: realised diet | `diet_eps` (1e-3) | DietCheck, PROD | `dietcheck_pair_series` process_diagnostics.R:296, `consumed_pair_series` 322 | proportion, t |
| Feeding: diagnostics table | — | prm | `pprey_diagnostics_table` 1849 | — |
| Consumption | `view, fg, top_n` | PROD.nc, DietCheck, BGM | `consumed_tonnes` analysis_helpers.R:1302; `consumption_by_prey` 1342; `consumption_as_prey` 1369 | "Consumption (tons)" |
| Mortality: source / predator / Mort | `view, fg, age` | SpecificMort, SpecificPredMort, Mort.txt | `calc_mort` mortality_module.R:215; `parse_pred_mort_by_predator` process_diagnostics.R:130; `parse_mort_txt` 206; file status 16 | fraction |
| Recruitment: YOY / SSB / S-R / growth | `analysis_type, fg_yoy, new_alpha, new_beta, recalc, fg_growth, box, layer_prop, box_prop, log_scale` | YOY, SSB, output.nc, prm | `build_rec_params` 373; `spawn_from_nc` 524; `BH.rec` 445; `recruitment_overlay` 471; `join_recruits_ssb` process_diagnostics.R:252 | recruits, t, mg N |
| Growth (primary producers) | `fg, box, layer, all_layers, log_scale` | output.nc, prm | `growth_limitation_for_fg` growth_module.R:124 | limitation 0–1 |
| Catch | `view` (by_group/by_fleet/numbers/skill), `fishery, fg, by_year` | CATCH.nc, fisheries CSV, observed CSV | `extract_catch_series` catch_module.R:513; `match_catch_vars` 398; `skill_assessment` 626 | t / numbers |
| Food web | `focal_fg, max_connections, min_proportion (0.01), time_step, stock` | DietCheck | `tot.prey`, `trophic.lvl`, `plot.tlvl` foodweb_module.R:200-281 | network, trophic level |
| Predation (diet composition) | `fg, stock, threshold (0.001), top_n, scaled, time_age` | DietCheck, BiomIndx | `diet_through_time_predator/_prey` predation_module.R:202/236; `age_diet_slice` 257 | proportion / "t" |
| Atlantis Scientific Guide (optional) | drawer | local Ollama + SQLite index | inst/ai/*; assistant_module.R | text, tab links |

## Exported legacy functions (stand-alone apps)

`catch`, `compare`, `feeding.mat`, `food.web`, `growth.pp`, `mortality`, `predation`, `recruitment.cal`, `text2num`, `time_calc`, plus the AI helpers `index_ask_atlantis`, `diagnose_ask_atlantis_index`, `download_atlantis_manuals` and `download_atlantis_html_manual`. None of the legacy apps is covered by tests. 20+ of their helpers are duplicated under the same names in `M/`.

## Three independent biomass paths in M/ (they can disagree)

1. `nitro.weight` / `bio.*`: inf_box Vol, boundary boxes excluded.
2. `biomass_by_box`: NetCDF `volume`, boundary boxes included.
3. `init_stage_stocks`: whole-box volume for pools.

A cross-diagnostic check should compare all three with BiomIndx (`cross_diagnostic_checks.md`, Phase 4).
