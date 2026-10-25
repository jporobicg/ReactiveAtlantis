# Phase 7: Performance and resource use (2026-10-25)

## Test setup

| Item | Value |
|---|---|
| Hardware | estigia: Dell Precision 7910, 2 × Xeon E5-2630 v4 (40 threads, governor `performance`), 62 GB RAM, SATA SSD, Ubuntu 26.04.1, R 4.5.2, Chrome 156 headless |
| Machine load | **Not idle:** load average 18–21 from 9 concurrent Atlantis runs, so absolute times are pessimistic |
| App | PR #4 (92974ba), installed package, driven by shinytest2 |
| Dataset | NESP `v8d_t12_5y/c3`: output.nc 1.4 GB (62 time steps × 147 boxes × 7 z, 101 groups); PROD, DietCheck 51k rows, SpecificMort, SpecificPredMort |
| Measures | Wall time from action to Shiny idle; RSS of the Shiny R process; open file descriptors, and how many of them are `.nc` (/proc) |

Raw data: `evidence/perf_steps.csv`. Script: `evidence/perf.R`. Per-tab render times on the first visit: `visual_design/screenshots/before/*/timing.csv`.

## Results

| Action | Time | RSS | Open .nc handles |
|---|---|---|---|
| App start (no data) | 7.8 s | 266 MB | 0 |
| Load c3 (first) | 67.9 s | 747 MB | 1 |
| Spatial: change group (30 changes, 10 groups × 3 passes) | **8.4–9.2 s each**, no faster on repeats | 509 → 609 MB, then flat | 1 |
| Spatial: move the time slider (10 steps) | **4.7–5.4 s per step** | flat | 1 |
| Compare tab | 10.8 s | 623 MB | 1 |
| Feeding → Predation pressure | **119.7 s** | 993 MB | 3 |
| Same view again after switching away and back | **94.3 s**: not cached | 980 MB | 3 |
| Reload the same run (×3) | **122–125 s each**, about 2× the first load | 1,260 → 1,314 MB | 3 |
| Switch to run c1 | 156 s | **1,406 MB** | **6** |

## Findings

| ID | Severity | Finding |
|---|---|---|
| PF1 | P2 | **The Spatial map costs about 9 s per group change, and repeats are no faster.** Array caching works (memory plateaus), so the cost is in building 147 separate plotly polygon traces for each render. On this hardware the Play animation (5 s per step) cannot animate. Proposed fix (software only): one trace with `fill` per polygon via `split`, or `plot_ly(type = "scatter", mode = "lines", fill = "toself")` with grouped data; or reuse the geometry and update only the fill colours with `plotlyProxy`. |
| PF2 | P2 | **Predation pressure takes 94–120 s and is not cached.** It loops in R over every pPREY row × prey column × box. On a busy machine it is hard to tell from a hang. |
| PF3 | P2 | **A reload takes twice as long as the first load, and memory is never released.** RSS grows 747 → 1,406 MB over one session, and the NetCDF handles of earlier loads stay open (1 → 3 → 6). This confirms S03 (no `nc_close`, no cache eviction, no `onSessionEnded`). A user comparing several large runs will run out of memory. |
| PF4 | P3 | Load time is about 68 s for a 1.4 GB output, and the first render of the busiest tabs is 10–30 s (Compare facets 12–20 s, overlap 30 s). This is acceptable for a calibration tool, but the app needs progress feedback; today there is only the busy chip. |

Possible stale view (unconfirmed): after switching to run c1, the Spatial tab redrew in 1.3 s, where a fresh map takes about 9 s. That points to the previous run's cached map being shown. Needs a value check in Phase 9.

## Acceptance targets (proposed, for estigia-class hardware and NESP-size models)

| Action | Target |
|---|---|
| Load | < 60 s |
| Group or time change on a map | < 2 s |
| Any first tab render | < 15 s |
| Revisiting a view | < 2 s (cache) |
| Memory after loading N runs | ≤ the size of what is loaded, released when a run is removed |

**Currently not met:** map interactions, predation pressure, reload, and memory release.
