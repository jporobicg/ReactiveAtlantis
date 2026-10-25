# Phase 8: remediation (2026-10-25)

The owner approved "the main changes" (A–G) on 2026-10-25. Where the owner gave no choice, the audit took the safer option:

- **F, predation pressure:** option 2. It is relabelled as a screening index and Atlantis's realised intake is shown beside it; its clear-cut bugs are fixed. The per-cell, per-cohort rebuild is left for later.
- **H, Scientific Guide:** kept, marked experimental, with its bugs fixed.

All commits are on the local branch `audit/pre-release-validation`, on top of PR #4 (`92974ba`).

## Commits

- `7478697`: scientific fixes A–G, with regression tests
- `f1ea2c4`: software fixes, with regression tests
- `62fdf73`: `.Rbuildignore` (keeps `validation/`, `docs/` and `.travis.yml` out of the package)
- `ce1f9f6`: Mortality-by-predator legend shows only predators with non-zero mortality

## Fix log

| ID | Change | Code | Verified on NESP c3 | Regression test |
|---|---|---|---|---|
| D01 | SpecificPredMort: Group/Cohort are the prey, columns the predators | process_diagnostics.R | Predators of FTM: SHW 324.77, FVL 0.32, identical to the Atlantis rows (`evidence/phase9/d01_after.txt`) | test-process-diagnostics "rows are prey and columns are predators". The test fixture was rewritten in the real layout. |
| D02 | SpecificPredMort labelled "fraction per output period", not "per day" | predation_pressure.R | — | test-predation-pressure (labels) |
| D03 | Spawn: subtract the weight deficit, cap at RN (`atlantis_ind_spawn`); legacy `recruitment.cal()` fixed too | recruitment_module.R, R/recruitment.R | FTM and FTB, 5 spawnings: RA / Atlantis = 1.000 (`d03_after.txt`) | "individual spawn follows Atlantis" (4 hand cases) |
| D04 | No BH curve from SSB.txt or BiomIndx tonnes | recruitment_module.R | — | covered by existing recruitment tests |
| D06 | predcase other than 0–2 is not computed and is listed in the notes | predation_pressure.R | — | — (no fixture uses such a predcase) |
| D07 | Pools are stage-1 prey only | predation_pressure.R | — | existing pressure tests pass |
| D08 | Juvenile/adult split on the 0-based cohort ≥ age_mat (pressure, gape, legacy `feeding.mat`) | predation_pressure.R, feeding_module.R, R/pprey.mat.R | — | existing tests pass |
| D09 | Inclusive hard window; juvenile upper gape uses KUP; (SN+RN)·Nums precedence | predation_pressure.R, feeding_module.R, R/pprey.mat.R | — | existing gape tests pass |
| D10 | Carrion eaten as live prey; turf and microphytobenthos as plant; type I cap mum/E_live | predation_pressure.R | — | Holling tests pass |
| D11, D12 | Epibenthos × box area (from NetCDF volume/dz); all cohorts of multi-cohort pools | analysis_helpers.R, layers.R, predation_pressure.R | Per box, 101 groups, 3 times: **all exact** against the independent reference (`bbox_summary_after.txt`) | north-sea consumption/biomass tests |
| D13 | 2-D [b,t] variables sliced on t | predation_pressure.R | — | — |
| D15 | Diet shares averaged over feeding cohorts (sum 1); consumption weighted by each cohort's intake | predation_module.R, analysis_helpers.R | — | "predator diet shares sum to 1"; "consumption uses Atlantis intake … per cohort" |
| D16 | Consumption = Σ Nums·Growth/E per cohort (t/day); pools keep the old method, labelled unverified | analysis_helpers.R, consumption_module.R, feeding_module.R, process_diagnostics.R | FTM, FTS, FPL, SHW at 3 times: RA / realised = 1.000 (`d16_after.txt`) | hand-computed 2-cohort, 2-box NetCDF |
| D19b, D31, N1 | Compare pools in tonnes from NetCDF volume and area, boundary boxes excluded | compare_module.R | ZL, ZS, PL, BO, MA, SG, BFF within 0.1–0.4 % of BiomIndx (`compare_total_after.txt`); scaled ZL now shows the crash to about 0.13 (`screenshots/after/.../05`) | north-sea tests |
| D21 | Overview and time axes open output.nc when needed | nc_cache.R + 6 modules | Overview shows 147 boxes, 7 layers, 2020-01-01, 5 years (`screenshots/after/.../01`) | — (screenshot evidence) |
| D22 | Undefined ratio when B0 ≤ 0 or not finite; flags renamed Collapsed / Increased; relative views NA | project_store.R, compare_module.R, overview_module.R | DL and DR no longer "Exploding" | "health flags: exact thresholds and undefined initial values" |
| D26 | Multi-run finds YOY as CODE-0 or CODE.0; one YOY reader for all runs | analysis_helpers.R | screenshot `after/multirun` | "Multi-run finds YOY in every run" |
| L01 | One folder finds outputs/ below and config above | project_store.R | — | "one folder finds outputs/ below it and config above it" |
| S01 | README install text; the default branch is what installs | README.md | — | — |
| S03 | On Load, keep only the loaded runs' files (close other handles, drop arrays); clear on session end | nc_cache.R, project_store.R, app.R | performance re-run (`performance_report.md`) | "loading keeps only the loaded runs' files open" |
| S04 | Rd for `launch_reactiveatlantis`, `download_atlantis_html_manual` | man/ | R CMD check: WARNING gone | R CMD check |
| S06 | Tests no longer overwrite `man/figures` (opt in with `RA_UPDATE_FIGURES=true`) | helper-ai.R | — | — |
| S09 | R ≥ 4.0; promises in Imports; htmltools in Suggests | DESCRIPTION | R CMD check | — |
| S13 | Tests find the source tree under R CMD check (`00_pkg_src`) | helper-synthetic-run.R + 3 test files | R CMD check runs **1,319 tests, 0 failures, 0 skips** | R CMD check |
| S14, S18 | One Chrome finder (CHROMOTE_CHROME, common paths, chromote); 120 s browser timeouts | helper-ai.R, test-e2e-app.R, test-ai-e2e.R | All browser tests run and pass | R CMD check |
| S17 | ExtendedTask gets a promise; errors become an answer | assistant_module.R | — | "returns a promise so Shiny ExtendedTask accepts it" |
| S19 | Map hover numbers via `ra_fmt_num` (100005 → "100,005") | analysis_helpers.R | — | "map hover numbers are plain" |
| S22 | HTML manual download keeps HTML and reports the real error | inst/ai/R/ai_download.R | — | "HTML manual download reports a failed fetch" |
| S23 | `think: false` by default (`RA_OLLAMA_THINK`); a timeout is reported as a timeout | inst/ai/R/ai_config.R, ai_ollama.R, ai_answer.R | Guide benchmark re-run with shipped defaults (`ai_guide_validation.md`) | "chat requests switch model reasoning off"; "a slow model is reported as a timeout" |
| — | README: real signatures, Ages and the new views, Consumption method, Guide experimental, Debian plotly note | README.md | — | — |

## Not changed (needs a decision or more work)

| ID | Item |
|---|---|
| D05 (rest) | The predation-pressure calculation is still box-level with stage-mean rates; it is only relabelled. A per-cell, per-cohort rebuild would remove the ~35× gap (option 1). |
| D14 | Catch fleet fallback and unit mixing. Validation is blocked by data (no real fished run). |
| D18 | Intensive/extensive aggregation in Physics, Spatial "integrated" tracers and problem cohorts. |
| D19 (BGM volumes) | `boxes.prop` volumes still differ from the NetCDF; Compare no longer uses them for pools. |
| D23 | flagnut 2 limitation formula. |
| D25, D27, D28, D29, D30 | Lower priority; see `defects.md`. |
| N2–N13 | Display and labelling items from the science review. |
| PF1, PF2 | Map rendering speed; predation-pressure speed. |
| S05 | CI (no GitHub Actions). |
| S07, S12, S25 | Guide safety and answer quality. These are design limits, not bugs. |
| Visual redesigns | All approved redesigns in `visual_design/proposed_improvements.md` are proposals; none is implemented. |
