# Test dataset manifest

Real NESP outputs stay where they are, under `~/whacamole_nesp/whacamole_local/results/runs/`. The audit opens them read-only, and their NetCDF files are never copied into the repository. Every derived dataset will be written to `~/whacamole_nesp/ra_audit_data/`, outside the repository.

| Set | Purpose | Source | Status |
|---|---|---|---|
| A | Realistic model | NESP v8d (satiation 0) runs, e.g. `v8d_t12_5y/c3/outputs` (5 y, 30-day output, 1.5 GB). All optional outputs are present: output.nc, PROD, DietCheck, SpecificMort, SpecificPredMort, Mort, SSB, YOY, BiomIndx. 147 boxes × 7 z, about 99 groups. **No fisheries (fishing off), so there is no CATCH.nc.** | Available |
| A-catch | Catch and fleet validation | North Sea fixture `tests/testthat/fixtures/northsea`. Its CATCH.nc is **synthetic** (3 boxes); the groups, BGM and prm are real. A real fished Atlantis run is needed. | **Data gap** |
| B | Poorly calibrated model | NESP `tuna_feed_5y/ctrl` (satiation cap: starving tunas), `v8c_base_2y`, `wave2_extremes_1y` (collapses and explosions), `v8d_bh_5y` (recruitment failure at 18 °C) | Available |
| C | Minimal controlled | Synthetic NetCDF built from NESP geometry with planted values (unique value per box and layer, known N, known Nums), clearly labelled synthetic. Hand-computed reference values. | To build (Phase 2) |
| D | Incomplete | Folder of symlinks to set A with optional files removed one at a time and in combinations | To build |
| E | Malformed | Copies of the small files plus truncated, empty, renamed-variable, wrong-dimension, NaN/Inf NetCDF (small, synthetic) | To build |
| F | Multiple runs | v8d c1–c5 (identical structure, different BHalpha); v8c 2 y vs v8d 5 y (different durations); f1 10 y; North Sea (different group structure) | Available, except different time steps (wave 8 will use 182.5-day output) |
