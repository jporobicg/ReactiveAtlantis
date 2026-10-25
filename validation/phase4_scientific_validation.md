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
