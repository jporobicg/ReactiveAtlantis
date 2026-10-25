# Phase 4: independent scientific validation (in progress)

- **Datasets:**
  - **A:** NESP `v8d_t12_5y/c3`. Real Atlantis output from `BethAtlantis_fix` 16792e4: 5 years, 30-day output, 147 boxes × 7 z, 101 active groups.
  - **C1:** a **synthetic** file on NESP geometry: tracer `SynthID = 1000·box + z` planted in every existing cell, with the geometry variables copied from c3.
- **Reference code:** Python/netCDF4, written from the Atlantis source definitions and independent of ReactiveAtlantis (RA). Scripts are in `evidence/phase4/`.
- **RA side:** the Shiny modules are sourced exactly as `inst/shiny/app.R` does (`ra_harness.R`), from the installed PR #4 package.

## V4.1 The reference reproduces Atlantis's own BiomIndx (validates the reference)

Reference = Σ(SN+RN)·Nums for age-structured groups, `_N`·volume for 3-D pools and `_N`·area for 2-D pools, then × X_CN 5.7 × 2e-8. Boundary boxes are excluded: 0, 110, 116-120, 122-132, 134, 135 and 136. Rows are matched by time; BiomIndx has 66 rows against 62 NetCDF steps.

| Group kind | Within 0.1 % of BiomIndx (t = 0, mid, end) | Explained exceptions |
|---|---|---|
| Age-structured (73) | 67 / 67 / 68 | Whales and seabirds (WHH, WHR, WHB, SBA, SBP, SBS, SBW) are migrants, and BiomIndx counts animals outside the domain. FTO and SHS are within 0.25 %. |
| 3-D pools (18) | 12 / 6 / 7 | Detritus and bacteria (DL, DR, DC, DF, BD, BB, BC): Atlantis excludes some detritus, and sediment in boxes with botz < maxwcbotz (atbiolmessage.c:347,435), which is not reproduced. Zooplankton within 0.25 %. |
| 2-D pools (10) | 3 / 4 / 4 | Within 0.2–2.6 %; same sediment / boundary handling as above. |

Tolerance: 0.1 % relative. That is enough to treat age-structured, non-migrant groups as reference quality.

## V4.2 RA `biomass_by_box` (Spatial map and box series) vs reference, per box, all 147 boxes, 3 times

| Kind | Groups exact (rel. 1e-6) | Result |
|---|---|---|
| Age-structured | 73 / 73 | **PASS** |
| 3-D pools | 18 / 18 | **PASS** |
| 2-D epibenthic pools | **0 / 10** | **FAIL, D11 (P0).** RA = reference × 3e-11 to 9e-10, because the area factor is missing. |

## V4.3 SpecificPredMort attribution (D01): **FAIL (P0)**

Asked for "predation mortality on FTM by predator", RA returns FPL, FPS, FPB, FPA, FMM, SQG, FMN and SQN, which are FTM's prey. The file's own FTM rows give SHW 325 and FVL 0.32, with everything else below 1e-240. Evidence: `evidence/phase4/d01_predmort_FTM.txt`.

## V4.4 Polygon and layer indexing (Spatial): **PASS**

Run on synthetic C1 with the real NESP BGM:

- **BGM parse:** RA vertices 1,871 = independent parse 1,871, with identical coordinates. The independent parse puts every box's `inside` point in its own polygon.
- **Layers:** each water layer key (`water_1` = surface … `water_6`), sediment and integrated return exactly `1000·box + z` for the expected storage z in all 147 boxes. Padded cells are NA.
- **Plotly map:** 147 labelled traces. Every trace's vertices belong to the box named in its hover, and every hover value matches.
- **One display issue (S19, P3):** the hover shows 4 significant digits, so 100005 appears as "1e+05".

Design note for the visual review: `water_6` is labelled "Deepest water", but it is the deepest layer only in full-depth boxes. In a 3-layer box the deepest water is `water_3`.

## Still to do in Phase 4

- Spawn and BH (D03/D04) on NESP tuna, against the v8d runs.
- Predation pressure (D05–D10): one cell reproduced by hand, and the size of the satiation-cap error on v8c.
- PROD consumption units (D16) and DietCheck aggregation (D15).
- Compare-tab `nitro.weight` path and Overview health vs reference.
- Schoener's D on controlled distributions.
- Catch is blocked by data: no real fished run.

## V4.5 Spawn feeding the BH expectation (D03): **FAIL**

RA spawn / Atlantis formula (atdemography.c:571-592, aggregated as in atdemography.c:1032-1048 with Σ FSPB·IndSpawn·Nums):

- 1.00 in well-fed years (days 330 and 1800);
- FTM 2.6, 12.4, 11.9 and FTB 1.8, 7.0, 8.1 in thin years (days 690, 1080, 1440).

Evidence: `evidence/phase4/d03_spawn_tuna.txt`.

## V4.6 Expected predation pressure (D05–D10): **FAIL**

Case: adult FTM (cohorts 4–10, age_mat 3) in box 94 (the densest), day 930, v8d (flag_satiation 0, soft window `UseHardFeedingWindow 0`).

| Quantity | t/day |
|---|---|
| RA `expected_predation_pressure` | **0.184** |
| Atlantis-faithful reference (per cohort × per water cell, soft window, one Holling denominator, no Tcorr) | **0.00523** |
| Realised intake from Atlantis output, Σ Nums·Growth / E | **0.00503** |
| Reference + Tcorr (0.693·1.066^T) | 0.0109 |
| Reference with hard SN window / no window | 0.0205 |
| Reference box-integrated instead of per cell | 0.0619 |
| Reference with a separate denominator per prey stage | 0.00524 |

- The reference without Tcorr reproduces realised intake within 4 %.
- RA is **35×** realised. The main causes are box integration (~12×) and the window (~4×).
- Applying the nominal Tcorr would *double* the error. This agrees with the earlier finding that the effective temperature multiplier in this Atlantis build is about 1.0–1.2, not 2.1–2.5.

Evidence: `evidence/phase4/d05_epp_FTM_box94_day930.txt`, scripts `epp_ra.R` and `epp_atlantis.py`.

## V4.7 Consumption tab (D16): **FAIL**

RA `consumed_tonnes` vs realised Σ Nums·Growth / E across the whole system:

| Group | Ratio |
|---|---|
| FTM | 3e-11 |
| FTS | 1e-11 |
| FPL | 2.5e-11 |
| SHW | 1e-10 – 2e-9 |

Evidence: `evidence/phase4/d16_consumption.txt`.

## V4.8 DietCheck aggregation (D15): **FAIL**

After `preaggregate_diet`, the diet proportions per predator and time sum to 10.0 for 10-cohort groups (FTM, FTS, FPL, SHW) and to 1.0 for the pool ZL. Absolute "consumption" from diet × biomass is n_cohorts × too high, and the shares are unweighted over cohorts.

Evidence: `evidence/phase4/d15_diet.txt`.

## V4.9 Schoener's D (controlled synthetic cases): **PASS**, with interpretation caveats (D30)

Complete overlap, no overlap, partial, scale invariance, unequal shares and two independent time steps all give the expected values. The problems are in interpretation:

- A predator with zero biomass everywhere gives D = 0 instead of "undefined".
- The per-box map index depends on scale.

Evidence: `evidence/phase4/schoener.txt`.

## V4.10 Compare tab "total" biomass (`nitro.weight`)

| Groups | Result |
|---|---|
| Age-structured (FTM, FTS, FPL, SHW, FMN) | **PASS**: within 0.1 % of BiomIndx and the reference |
| Pools (ZL, ZS, PL, BO, MA, SG, BFF) | **FAIL, D31**: 2.5e3 to 8.8e6 × too high. They are left in mg N while labelled tonnes, and the `boxes.prop` volumes differ from the NetCDF `volume` (D19b) |

Evidence: `evidence/phase4/d31_compare_total.txt`.

## V4.11 Overview health thresholds (controlled cases)

- The exact boundaries are correct.
- Zero, Inf and negative initial values receive "Extinct" or "Exploding" verdicts. On real NESP data this affects DL and DR (D22).

Evidence: `evidence/phase4/health_thresholds.txt`.

## V4.12 Mortality (SpecificMort)

- The column parsing matches Atlantis.
- 8 multi-stock NESP groups lose their S1+ stocks without notice (D27).
- Predator attribution is transposed (D01).

## Phase 4 coverage summary

| Diagnostic | Status |
|---|---|
| Per-box biomass (Spatial) | PASS for age-structured and 3-D pools; **FAIL** for epibenthos (D11) |
| Compare totals | PASS for age-structured; **FAIL** for pools (D31) |
| Polygon / layer indexing | **PASS** |
| SpecificPredMort attribution | **FAIL** (D01) |
| Spawn → BH | **FAIL** (D03) |
| Expected predation pressure | **FAIL**, 35× (D05) |
| Consumption tab | **FAIL**, about 1e-11 (D16) |
| DietCheck aggregation | **FAIL** (D15) |
| Schoener's D | PASS, with caveats (D30) |
| Health thresholds | Boundaries PASS; edge cases FAIL (D22) |

**Not tested yet:**

- Ages tab totals.
- Growth limitation (D23).
- Satiation-cap magnitude on v8c.
- The BH curve plot end-to-end.
- The mortality vs abundance-change consistency check.

**Blocked by data:** Catch and fleets. A real fished Atlantis run is needed.
