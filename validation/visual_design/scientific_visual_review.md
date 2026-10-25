# Scientific validation of the app screenshots (visual-design audit, "before" state)

- **Run shown:** NESP `v8d_t12_5y/c3` (147 boxes, 7 z, 101 active groups, 5 years, 30-day output). The multi-run states add c1, c2, c4 and c5.
- **App:** PR #4 head `92974ba`.
- **Scope:** this review covers what each figure represents and whether that is scientifically correct. It does not judge aesthetics; the design reviewer covers that.
- **Ground truth:** `validation/defects.md` (D-IDs) and `validation/phase4_scientific_validation.md` (V4.x).
- **New checks:** made for this review against the run's own files in `whacamole_local/results/runs/v8d_t12_5y/c3/outputs/` (`output.nc`, BiomIndx, YOY, SSB, SpecificMort, SpecificPredMort, DietCheck, `NESP_bio.prm`, `NESP_groups.csv`), using Python/awk. Each new check is marked **NEW** and gives its evidence.
- **`M/`** means `inst/shiny/modules/`.

**Verdict scale**

| Verdict | Meaning |
|---|---|
| CORRECT | The plotted quantity matches the data and the label. |
| INCORRECT | The quantity or label is wrong (defect ID or NEW given). |
| UNVERIFIED | No reference calculation exists yet. |
| NOT RENDERED | Empty plot, placeholder, or nothing scientific on screen. |

**Viewports.** The desktop (1600×3200) captures are the basis of each verdict. The small (1024×700) captures show the same states. They add no new values but do add the scientific risks listed in the small-viewport section.

---

## 01 Overview

1. **Question:** Is the run sane? Which groups crashed or exploded, and which cohorts are in poor condition?
2. **What is plotted:**
   - **Cards:** groups on = 101. Boxes, Layers and Problem cohorts show "—". Extinct = 2. Exploding = 8.
   - **Health table:** BiomIndx final/initial ratio, flagged at <1 % or >10×.
   - **Problem-cohort table:** StructN and ResN at the last step divided by t0, flagged outside 0.5–1.5. These are unweighted cell means (D18).
   - **Run window:** start date "not in NetCDF", span "unknown".
3. **Verdict: INCORRECT in parts.**
   - The blank cards and run window are D21. NetCDF `t` units are `seconds since 2020-01-01 00:00:00 +10`.
   - DR (ratio 8.2e20) and DL (4.5e20) start at 0 and are counted as 2 of the 8 "Exploding" groups (D22). Only 6 real explosions exist (FPS, FMN, SQG, SQN, FDO, SG).
   - The cohort drift is an unweighted mean that includes empty cells (D18, UNVERIFIED in magnitude).
4. **Interpretation risk:**
   - The user would count 8 exploding groups and give detritus the top priority.
   - Final/initial alone hides a group that crashed and recovered, and a group that oscillates.
   - Problem cohorts are sorted only by flag, so every visible row is WHD (dolphins).
5. **Redesign must:**
   - mark B0 ≤ 0 or non-finite as "undefined / no data", never "Exploding";
   - show the trajectory (sparkline or min/max over the run), not only the endpoint ratio;
   - state that the source is BiomIndx (boundary boxes excluded, D20);
   - fill the cards from the NetCDF (147 boxes, 7 z, 2020-01-01, 5 y);
   - rank problem cohorts by deviation size, across groups;
   - compute cohort condition as a numbers-weighted mean over water cells only.

## 02 Compare: small multiples (absolute)

1. **Question:** How does each group's biomass change through time?
2. **What is plotted:** BiomIndx tonnes per group (`compare_module.R:80-82`, `biomindx_facet_df`). This is extensive and system-wide (boundary boxes excluded by Atlantis), with free y axes.
3. **Verdict: CORRECT** as data, but captured while still rendering. The screenshot is the greyed "Working…" state, so the panels are stale.
4. **Interpretation risk:**
   - Free y axes make a ±2 % wiggle (e.g. Crabs BCR, Macrozoobenthos BMS) look like a large trend.
   - DL and DC start at or near 0 and look like explosions or crashes.
5. **Redesign must:**
   - show units (t) on every panel or once globally;
   - make the free-axis state explicit, or show the % change annotation;
   - show the full group code in every panel title (codes are truncated at 1024 px; see small viewport).

## 03 Compare: relative to initial + 0.5–1.5 band

1. **Question:** Which groups have left the acceptable band (Bt/B0 outside 0.5–1.5)?
2. **What is plotted:** BiomIndx Bt / B(first row), with the shaded 0.5–1.5 band and free y axes.
3. **Verdict: INCORRECT in parts.**
   - The DL panel is blank because B0 = 0, so Bt/B0 = Inf/NaN (D22, `compare_module.R:211,242`). DR is the same.
   - Carrion (DC) plunges to 0 at step 2. This is real data, but no reference is shown.
   - All other panels are CORRECT.
4. **Interpretation risk:**
   - A blank panel reads as "no change" or "no data" rather than "undefined ratio".
   - With free y axes the band is sometimes partly or wholly off-screen, so a group at 3× (e.g. FTB, FDC) looks visually similar to one at 1.2×.
5. **Redesign must:**
   - put a fixed or log y axis on relative views so the band is comparable across panels;
   - label undefined ratios explicitly;
   - always draw the 1.0 line;
   - not hide the absolute B0 (show it in hover or the panel subtitle).

## 04 Compare: one group totals, FTM, scaled

1. **Question:** How has southern bluefin tuna total biomass changed relative to t0?
2. **What is plotted:** `nitro.weight(By="Total")`, i.e. Σ(SN+RN)·Nums·X_CN·mg2t from `output.nc`, divided by its first value. The y label is "Relative Values (X_t/X_0)". All boxes are included (D20).
3. **Verdict: CORRECT.** This is an age-structured group (V4.10 PASS). An independent sum from `output.nc` gives 6720 t → 13810 t, a ratio of 2.05. The plot ends at about 2.07.
4. **Interpretation risk:** low. The final drop at the last step comes from the 1824.5-day output (the last interval is not 30 days) and is real.
5. **Redesign must:**
   - keep the B0 value visible;
   - state the source (`output.nc` all boxes, vs BiomIndx);
   - if both sources are shown, explain why the absolute values differ (D20).

## 05 Compare: one group totals, ZL (carnivorous zooplankton), scaled

1. **Question:** Same as 04 for a pool group.
2. **What is plotted:** the `nitro.weight` pool branch, concentration × `boxes.prop` volume in mg N (D31). The units cancel when scaled, but the volume weighting does not (D19b).
3. **Verdict: INCORRECT. NEW evidence that D31/D19b corrupt the shape of the trajectory, not only its units.**
   - **The screen shows:** ZL rises to about 1.28 at step 2, then settles at about 0.5.
   - **Truth:**
     - Σ(ZL_N × NetCDF `volume`) gives 1.00 → 0.32 (step 2) → 0.24 → 0.17 (step 11) → 0.14 (end).
     - BiomIndx gives the same: 198.4 kt → 59.4 kt at day 30 → 21.6 kt at day 1650, a ratio of 0.11–0.14.
     - The screen's +28 % spike is the reverse of the true −68 %.
     - The unweighted Σ concentration gives 1 → 2.06 → … → 0.93. This shows the result is very sensitive to the volume weights.
   - The 0.5 plateau would put ZL on the band's lower edge, when it is actually at about 0.14.
4. **Interpretation risk:**
   - The user would conclude ZL bloomed initially and is only mildly depressed (Bt/B0 about 0.5, borderline acceptable).
   - In fact it lost about 86 % within the first months. This is the "ZL starved by plankton forcing" problem the NESP calibration is trying to fix.
5. **Redesign must:**
   - compute pool biomass with the NetCDF `volume` (3-D) or `area` (2-D, D11), × X_CN × mg2t;
   - check every total view against BiomIndx and show a warning when they differ by more than a tolerance;
   - never present scaled pool totals until D31/D19b are fixed.

## 06 Compare: age structure (stacked area), FTM

1. **Question:** How is FTM biomass distributed across cohorts through time?
2. **What is plotted:** `nitro.weight(By="Cohort")` per cohort, stacked, in t. The legend reads Age01…Age10.
3. **Verdict: CORRECT values, INCORRECT labels (NEW).**
   - The stacked total matches 04: about 6.7 kt → 14.6 kt.
   - FTM has `NumAgeClassSize = 4` (`NESP_groups.csv`). Each cohort spans 4 years, so "Age01" is ages 0–3 and "Age10" is ages 36–39.
   - No module reads `NumAgeClassSize`: grep finds no `ageclasssize` handling in `M/`.
   - Cohort indexing also differs between tabs:
     - Compare: Age01–Age10 (1-based);
     - Ages: "Age 1…Age 10";
     - Predation diet-by-age: Age00–Age09 (0-based);
     - Mortality: Age01–Age10, from SpecificMort cohorts 0–9.
4. **Interpretation risk:**
   - The user would read "Age01 is half of the biomass" as 1-year-olds dominating southern bluefin tuna. In fact cohort 1 covers the first 4 years.
   - Adult cohorts (5–10) look like a thin red band, as they truly are. That is a model state, not an app artefact.
5. **Redesign must:**
   - label cohorts as "cohort k (ages a–b)" using NumAgeClassSize;
   - use one indexing convention across all tabs;
   - order the stack by age;
   - provide a numbers view alongside biomass (the checkbox exists).

## 07 Ages: FTM (biomass, numbers, ResN, StructN per cohort)

1. **Question:** Per cohort, how do numbers and individual condition (RN, SN) evolve?
2. **What is plotted:**
   - whole-system numbers: Σ boxes and layers;
   - biomass, in t;
   - ResN and StructN: numbers-weighted means in mg N per individual, as the sidebar text states.
3. **Verdict: CORRECT** for the values. Cohort-1 biomass at t0 is about 3.7 kt, consistent with 06. Labelling problems (NEW):
   - "Age k" ignores NumAgeClassSize, as in 06.
   - The legend and colour order are lexicographic: Age 1, Age 10, Age 2, … (`ages_module.R:67`, `sort(unique(df$AgeLab))`). Age 10 gets the second palette colour, next to Age 1.
4. **Interpretation risk:**
   - Age 1 and Age 10 both render dark, so the user can confuse the youngest and oldest cohort in the RN panel. That panel is exactly where "thin adults" are diagnosed.
   - The y axes have no units.
5. **Redesign must:**
   - order colours by numeric cohort with a sequential scale;
   - show units (individuals; mg N ind⁻¹; t);
   - show the reference weights KWRR/KWSR (or the initial RN and SN per cohort) as dashed references, so condition can be judged against the target and not only through time.

## 08 Spatial map: FTM biomass, all layers, time step 1

1. **Question:** Where is FTM biomass, per box?
2. **What is plotted:** per-box Σ over layers of (SN+RN)·Nums·X_CN·mg2t, in t. The colour scale is stable across time. The series below is for the "selected box", which defaults to box 0.
3. **Verdict: CORRECT** for the map (V4.2 and V4.4 PASS for age-structured groups). The colour maximum of about 7400 t matches the independent maximum, 7406 t in box 114 at step 60. The default series is box 0, a boundary box with FTM constant at 1.74 t (76.9 individuals in z = 5); that is real.
4. **Interpretation risk:**
   - At t1 nearly every box looks empty, because the stable scale is set by the step-60 maximum.
   - The flat box-0 series suggests nothing happens through time. Box 0 is a boundary box whose state Atlantis holds fixed.
5. **Redesign must:**
   - default the series to the system total or the densest box, not boundary box 0;
   - mark boundary boxes as non-dynamic;
   - state on screen the colour-scale basis (fixed over time vs per step);
   - show biomass density (t km⁻²) as an option, because absolute t per box is dominated by box area.

## 09 Spatial map: MA (macroalgae, epibenthic), all layers

1. **Question:** Where is macroalgal biomass?
2. **What is plotted:** the 2-D `_N` (mg N m⁻²) × X_CN × mg2t **without × area**, labelled "Biomass (t)". The scale runs 0–500 µt per box and the series shows box 0 = 0.000.
3. **Verdict: INCORRECT (D11, P0).** The values are about 1e-9 of the true values (MA reference about 50 Mt).
4. **Interpretation risk:** the user would conclude macroalgae are effectively absent ("500 micro-tonnes"), and so that they are not habitat or food anywhere.
5. **Redesign must:**
   - multiply 2-D epibenthic fields by box area;
   - put units on the scale (t, or g N m⁻² for density);
   - add a check that the system total matches BiomIndx;
   - offer the density view, which is the natural quantity for benthos.

## 10 Spatial map: temperature, surface layer

1. **Question:** What is the spatial pattern of surface temperature?
2. **What is plotted:** `Temp` at storage z = numlayers−1 for each box. This is intensive (°C) and per cell, not aggregated. The series shows box 0.
3. **Verdict: CORRECT** for the map (V4.4 layer indexing PASS). The box-0 series is a constant 15.000 °C for 5 years. **NEW (data fact, not an app bug):** in `output.nc`, box 0 is 15.0 in every existing layer and in sediment at all times. Boundary-box tracers are not dynamic.
4. **Interpretation risk:**
   - The colour bar is labelled "Temp" with no unit.
   - The default boundary-box series suggests a model with no seasonality. Box 94 has 16–23 °C seasonal cycles (state 16).
   - The info panel still says "Group: MA", left over from the previous state, which suggests the map is group-specific.
5. **Redesign must:**
   - label the unit (°C);
   - never default to a boundary box;
   - say that surface means "top water layer of each box", whose depth differs between boxes;
   - hide fields that do not apply (Group) in tracer mode.

## 11 Spatial map: temperature, sediment layer

1. **Question:** Bottom and sediment temperature pattern.
2. **What is plotted:** `Temp` at storage z = 6 (sediment). For box 94 this equals the deepest water layer (9.10 °C at t0, z = 0 and z = 6 identical), which is how Atlantis stores it.
3. **Verdict: CORRECT.** Same box-0 caveat as 10.
4. **Interpretation risk:** low. A user might think sediment temperature is computed independently.
5. **Redesign must:** have a unit and a note that sediment temperature is inherited from the bottom water cell; and the colour scale range (8–22) should be stated as fixed across time.

## 12 Spatial: predator–prey overlap FTM × FMM, sediment layer

1. **Question:** Where and how much do southern bluefin tuna and myctophids overlap?
2. **What is plotted:**
   - **Map:** per-box 2·min(Bpred, Bprey)/(Bpred+Bprey) in t, on the layer selected. Sediment was carried over from state 11.
   - **Series:** the box-0 local index. The sidebar text says "The series is Schoener's D across boxes", but D is only drawn when scope = "system" (`spatial_module.R:371-384`).
3. **Verdict: INCORRECT.** D30 applies, and there are new findings.
   - **NEW:** FTM and FMM have no real biomass in sediment. In `output.nc` the maximum sediment biomass is 2.3e-30 t (FTM) and 1.1e-30 t (FMM), which is numerical residue.
   - Yet the per-box index on those residues is 0.667 in 7198 box-time cells. This sets the stable colour bar to 0–0.67, as on screen.
   - The index is pure noise for a layer where neither group lives, because it is scale-invariant to absolute magnitude (D30b).
   - For comparison, Schoener's D on integrated biomass is 0.54 at t0 and 0.22 at the end. On integrated biomass the maximum of the per-box index is 0.73.
   - **NEW:** the explanatory text and the default series disagree. The text promises Schoener's D, but the series shows the local index for boundary box 0.
4. **Interpretation risk:** the user could read "up to 67 % overlap" between tuna and myctophids in the sediment and conclude benthic feeding interactions that cannot exist.
5. **Redesign must:**
   - default overlap to integrated water-column biomass;
   - mask the index where either biomass is below a meaningful threshold (return NA, not 0 or 0.67);
   - show Schoener's D as the headline number;
   - show the local index on shares (p_i, q_i), not on tonnes;
   - make the layer choice explicit in the title.

## 13 Spatial: by age class, FTM, box 0, sediment

1. **Question:** Cohort biomass, numbers and condition in one box and layer.
2. **What is plotted:** per-cohort biomass, numbers, ResN and StructN for box 0, sediment (z = 6). The Variable selector still says "Tracer / Temperature", left over from the previous state.
3. **Verdict: NOT RENDERED.**
   - Biomass and numbers are flat at 0; ResN and StructN are empty panels. FTM does not occur in sediment, and box 0 is a boundary box.
   - The values themselves are faithful (FTM sediment numbers are about 1e-12 in total).
4. **Interpretation risk:** an empty panel looks like missing data or a broken tool, rather than "this group never occupies this cell".
5. **Redesign must:**
   - default to a water layer (or integrated) and a dynamic box where the group occurs;
   - say explicitly "no individuals in this cell";
   - hide tracer controls in this view.

## 14 Consumption: what FTM eats

1. **Question:** How many tonnes of each prey does FTM consume through time?
2. **What is plotted:** PROD `_Eat` summed without volume or area (2-D on NESP), × DietCheck shares summed over cohorts. The label is "Tonnes consumed by FTM", and the scale reaches 1e-8 t.
3. **Verdict: INCORRECT (D16, P0; D15).**
   - The magnitude is about 1e-11 of realised intake (about 10.6 t/day at day 930).
   - The share split is an unweighted cohort mean.
   - The rate per day is presented as tonnes.
4. **Interpretation risk:** the user would conclude bluefin tuna eat nanograms, a negligible role, and could not compare consumption with production or standing stock.
5. **Redesign must:**
   - use a validated intake (e.g. Σ Nums·Growth/E, or the correct PROD conversion) in t d⁻¹ or t y⁻¹, with the unit explicit;
   - weight shares by cohort numbers or biomass;
   - show total intake alongside the split;
   - do a sanity check against the predator's biomass (e.g. intake as % of body mass per day).

## 15 Consumption: who eats FMM (myctophids)

1. **Question:** Which predators remove myctophids, and how much?
2. **What is plotted:** the same pipeline as 14, from the prey side. The scale reaches 4e-6 t, and FMN (non-migratory mesopelagics) dominates.
3. **Verdict: INCORRECT (D16, D15).** The magnitudes are not physical. The relative split between predators is UNVERIFIED, because it inherits the unweighted cohort mean and each predator's own PROD scale.
4. **Interpretation risk:** the user would conclude predation on myctophids is negligible (micro-tonnes) when it is a major flux.
5. **Redesign must:** meet the requirements of 14, and also show the removal as a fraction of prey biomass per unit time, so it can be compared with the M2 on the Mortality tab.

## 16 Physics: temperature by layer, box 94, plus nominal_dz

1. **Question:** What is the vertical and seasonal temperature structure in one box, and how thick is each layer?
2. **What is plotted:** `Temp` per storage layer of box 94, faceted surface → deepest → sediment; bars of `nominal_dz` per layer.
3. **Verdict: CORRECT.**
   - Box 94 `nominal_dz` = 3366, 450, 150, 50, 30, 20, 1 m (z = 0…6), as drawn.
   - Surface temperature at t0 is 16.44 °C and 21.9 °C at day 30, as drawn.
   - The 1 m sediment bar renders as about 0, which is correct but invisible.
4. **Interpretation risk:**
   - The Temp axis has no unit.
   - Each facet has its own y range, so the user may not see that deep water is about 10 °C colder.
   - The x axes here use calendar years, while Predation and Mortality use 0–5 (D21).
5. **Redesign must:**
   - show the unit;
   - use a shared y scale or overlay layers on one axis (the vertical gradient is the science);
   - label each layer with its depth range (m) rather than "Water layer k";
   - note that "Deepest water" is z = 0 only for full-depth boxes (V4.4 design note).

## 17 Multi-run with one run loaded

1. **Question:** Compare BiomIndx, SSB or YOY across runs.
2. **What is plotted:** nothing; a placeholder message.
3. **Verdict: NOT RENDERED.** This is the expected empty state.
4. **Interpretation risk:** none.
5. **Redesign must:** show the single run's series rather than an empty frame, so the tab is not blank.

## 18 Predation: diet of FTM, predators of FTM, diet by age at day 30

1. **Question:**
   - What does FTM eat (realised diet)?
   - Who eats FTM?
   - How does diet change with age?
2. **What is plotted:**
   - **Diet:** DietCheck proportions, aggregated over cohorts (sum, then "Scale to 1"), stacked over time in years 0–5.
   - **Predators:** diet share × predator BiomIndx biomass, normalised to 1 (D15: "not consumption").
   - **Diet by age:** DietCheck per cohort at the slider time.
3. **Verdict:**
   - **Diet:** UNVERIFIED (D15). With "Scale to 1" the n_cohort inflation cancels, but the shares are an unweighted cohort mean. Each of the 10 cohorts counts equally even though cohort 1 holds about half the biomass (06).
   - **Predators:** UNVERIFIED (D15). The identity of the predators (SHW, FVL) agrees with SpecificPredMort (V4.3). The proportions are a proxy: from DietCheck × BiomIndx, FVL is about 10 % at year boundaries.
     - **NEW, partial:** SpecificPredMort gives FVL about 78 % of the FTM predation mortality from day 360 (it targets the new cohort 0) and about 0 before that.
     - So the two sources disagree in timing and size.
   - **Diet-by-age:** CORRECT as a reading of DietCheck. The labels are Age00–Age09 (0-based) and not ages (NumAgeClassSize = 4).
4. **Interpretation risk:**
   - **Unweighted diet:** the user would conclude adult-type diet (FPL, mackerel) dominates the population's intake when juveniles may dominate the biomass.
   - **Predator panel:** the user would read it as a share of mortality, but it is a share of predator diet × predator biomass.
   - **X axis:** "Year 0–5" here vs calendar years in Compare (D21).
5. **Redesign must:**
   - weight by cohort numbers or biomass;
   - label the predator panel as "share of predator diet × predator biomass (proxy)", or replace it with SpecificPredMort once D01 is fixed;
   - label cohorts by age range;
   - use one time axis across tabs.

## 19 Food web: FTM focal, trophic levels

1. **Question:** Where does FTM sit in the food web, and what are the trophic levels?
2. **What is plotted:**
   - a network of DietCheck links (cohort-summed, `foodweb_module.R:202-203`) within 4 connections of FTM at day 30;
   - TL = 1 + Σ(diet·TL_prey), computed in two passes seeded with hard-coded GroupType priors (`foodweb_module.R:155-174, 229-257`).
3. **Verdict: INCORRECT (NEW).**
   - Primary producers are assigned TL ≠ 1 by fiat: LG_PHY (PL, diatoms) = 1.5, DINOFLAG/MICROPHYTOBENTHOS = 1.2 (`foodweb_module.R:172-173`). The table shows PL = 1.5, which violates the definition (autotrophs = 1).
   - The algorithm runs two fixed iterations from priors (FISH = 3.24, SHARK = 4, …). It is not solved to convergence, so TLs depend on the priors and on which groups fall inside the 4-connection window.
   - Example: carnivorous zooplankton ZL = 4.06 sits above sardine (3.85) and mackerel (3.95), which is implausible and likely a prior or iteration artefact.
   - Diet weights are cohort-summed (D15).
4. **Interpretation risk:**
   - Wrong TLs feed directly into fishing-down or trophic-structure interpretations.
   - The user would conclude ZL is a top-level predator comparable to tuna.
5. **Redesign must:**
   - set primary producers and detritus to TL = 1;
   - solve the linear system TL = 1 + D·TL over the whole web (not a local window), with a biomass-weighted diet;
   - show the diet time used;
   - flag cycles.

## 20 Recruitment: YOY and larvae, FTM

1. **Question:** How many recruits does FTM produce each year, and how would new BH α and β change that?
2. **What is plotted** (`recruitment_module.R:148-195`):
   - **Atlantis YOY:** the YOY.txt column FTM-0, 66 rows (919.7 → 130.4 → 56.5 → 75.6 → 86.5 → 134.5).
   - **Larvae:** BH(RA spawn, α0, β0) converted to YOY units.
   - **New larvae:** BH with the typed α and β.
   - **New YOY:** Atlantis YOY × new/old ratio.
   - The second panel is YOY/YOY0.
3. **Verdict: INCORRECT.**
   - **Larvae and New larvae** (teal, about 260–290 per year) use RA spawn, which has the wrong sign: 2.6–12× too high for thin FTM (D03). The BH "SSB" input is total stock biomass (D04).
   - **NEW (display):** the typed α and β equal the current ones, so New YOY is identical to Atlantis YOY. The gold "New YOY" line is drawn on top of the navy "Atlantis YOY" line and hides it completely, and "New larvae" hides "Larvae" the same way.
     - What looks like "Atlantis YOY" on screen (teal points at about 260–290) is actually the BH larvae.
     - The real Atlantis YOY (919 → 130 …) is shown in the "New YOY" colour.
   - The y axis has no units. YOY.txt is a biomass (t) and larvae are converted to the same units; neither is stated.
4. **Interpretation risk:**
   - The user would read the teal series as Atlantis recruitment (about 280 per year, stable) when Atlantis actually produced 56–135 after year 1.
   - They would then conclude BH expectations agree with the model.
5. **Redesign must:**
   - always draw the Atlantis YOY on top and in a distinct style;
   - hide "new" series until the parameters differ;
   - fix D03 and D04 before showing larvae;
   - give units;
   - mark the first-year value (919.7, a carry-over or initial value) as a start-up artefact.

## 21 Recruitment: recruits vs SSB, FTM

1. **Question:** Is there a stock–recruit relationship?
2. **What is plotted:** YOY.txt FTM-0 and SSB.txt FTM, joined on the same time stamps without lag (`process_diagnostics.R:252-293`). Time series are faceted, with a scatter of Recruits vs SSB coloured by year and connected by a path.
3. **Verdict: UNVERIFIED / misleading (NEW).** The values are faithful. SSB.txt is 0 for days 0–330 and at day 1800, then 3052 at day 1824.5; those are artefacts of the Atlantis file. However:
   - YOY and SSB are paired at the same time stamp. Recruits produced from a spawning event should be paired with the SSB at spawning (a lag of age at recruitment or the spawn-to-recruit delay).
   - The 66 step-function rows repeat each annual value 12 times. The scatter therefore over-weights repeated points and has a point at SSB = 0 with the largest recruitment (919.7, the start-up artefact).
   - The zeros in SSB.txt (start-up, and day 1800) are plotted as real stock states.
4. **Interpretation risk:**
   - The user would see "highest recruitment at zero SSB" and a negative S–R slope, and could conclude compensation or a data error.
   - Neither is a real stock–recruit signal.
5. **Redesign must:**
   - plot one point per spawning event, with lagged pairing;
   - exclude or flag SSB = 0 rows and the first-year YOY;
   - overlay the BH curve with the run's α and β (with D03 and D04 fixed);
   - give units (t).

## 22 Recruitment: SSB time series, FTM

1. **Question:** Spawning stock biomass through time.
2. **What is plotted:** SSB.txt, column FTM.
3. **Verdict: CORRECT** as a reading of the file (0 → 1461 → 1285 → 1555 → 1867 → 0 at day 1800 → 3052 at day 1824.5). The drop to zero is in SSB.txt itself.
4. **Interpretation risk:** the user would believe the spawning stock vanished near the end of year 5 and then doubled. This is a reporting artefact of Atlantis (annual accumulator reset), not biology.
5. **Redesign must:**
   - flag accumulator resets and first-year zeros;
   - give units (t);
   - consider showing SSB computed from `output.nc` (FSPB-weighted adult biomass) alongside, for cross-checking.

## 23 Growth: PL (diatoms), box 0, layer 0

1. **Question:** What limits phytoplankton growth (light, nutrients, space or eddy) in a cell?
2. **What is plotted:**
   - DIN/(KN+DIN) (with the Si term per flagnut);
   - light = min(IRR/KI, 1);
   - growth = min(nut × light, 1);
   - eddy × eddy_scale, averaged over layers;
   - all for storage layer index 0 of box 0 (`growth_module.R:60-90, 147-189`).
3. **Verdict: INCORRECT for this state (D29 and NEW).**
   - Box 0 has water only at z = 4–5. Storage z = 0 is a padded cell (Temp = 0 there in `output.nc`). The plot shows growth, light and nutrient limitation = 0 for a cell that does not exist (D29: no padding mask).
   - "Layer 0" is the storage index, not the surface.
   - The light-limitation form min(IRR/KI, 1) and flagnut = 2 (D23) are UNVERIFIED against the Atlantis source for NESP's flags.
4. **Interpretation risk:** the user would conclude diatoms are totally light- and nutrient-limited (growth = 0) everywhere at this site, a false limitation diagnosis.
5. **Redesign must:**
   - mask padded and sediment cells;
   - label layers by position and depth (surface…bottom);
   - default to a dynamic, non-boundary box and the surface layer;
   - state the formulas used, and verify them against the Atlantis flags (flagnut, light formulation);
   - draw the limitation factors on the same 0–1 axis.

## 24 Catch: disabled

1. **Question:** Catch by fleet and group.
2. **What is plotted:** nothing; the message "Load CATCH.nc to plot catch". The NESP run is unfished and has no fisheries CSV.
3. **Verdict: NOT RENDERED** (expected). The open catch defects (D14) cannot be checked visually here.
4. **Interpretation risk:** none.
5. **Redesign must:** label the fallback catch as numbers when the cohort `_Catch` variables are used (D14), and never sum it as tonnes.

## 25 Feeding: pPREY matrix

1. **Question:** Which predator stage is allowed to eat which prey, and at what availability?
2. **What is plotted:**
   - rows: pPREY vectors from `biology.prm` (314 rows × 105 prey columns = 102 groups + DLsed, DRsed, DCsed);
   - row labels: "predator · predator stage / prey stage prey";
   - colour: pPREY value.
3. **Verdict: CORRECT for the parsing on NESP (NEW check).** `parse_pprey_rownames` maps pPREY1WHH1 → juvenile predator / juvenile prey … pPREY2WHH2 → adult / adult, with no duplicate labels.
   - Plotly shows only every second row label. The visible labels ("juvenile / adult prey", "adult / adult prey") are not the labels of the adjacent unlabelled rows.
   - D25 (digit-containing codes, hard-coded sediment columns) does not bite on NESP.
4. **Interpretation risk:**
   - With half the tick labels missing, a user can attribute a cell to the wrong predator stage.
   - pPREY is an availability, not a diet proportion, and the colour scale does not say so.
5. **Redesign must:**
   - label every row, or group rows by predator with stage sub-rows;
   - title the scale "availability (pPREY)";
   - mark the pool and sediment prey columns.

## 26 Feeding: predation pressure

1. **Question:** For each predator–prey pair, how much would Atlantis's feeding equations remove (t/day, % of prey per day), and is that unsustainable?
2. **What is plotted:**
   - matrix colour: "Prey loss fraction/day" from init.nc biomass at time step 1, all boxes;
   - the flag count ("986 cells flagged (loss > 5 % of prey per day, or more than the standing stock)");
   - a pair table (e.g. pPREY1FMM2 and pPREY2FMM2 both list BC with pPREY 0.425 and consumption about 891 t/day).
3. **Verdict: INCORRECT (D05 P0, D06–D10).**
   - The expected intake is about 35× realised (V4.6).
   - **Visual evidence of D07 in this screenshot:** BC (benthic carnivore infauna) is a pool, yet it appears as both a "Juvenile" prey (pPREY1FMM2) and an "Adult" prey (pPREY2FMM2), each with the full consumption. The pool is counted twice.
   - The heatmap is uniformly pale even though 986 cells are flagged, so the flags are not visible in the matrix.
4. **Interpretation risk:** the user would conclude myctophids remove about 1.8 kt/day of benthic infauna and that hundreds of interactions are unsustainable, and would then cut pPREY values that are not the problem.
5. **Redesign must:**
   - label the output as a screening index (not an Atlantis flux) until D05 is fixed;
   - show units;
   - show realised intake (Growth/E) next to the expected intake;
   - de-duplicate pool prey (stage 0 only);
   - use a log colour scale so flags are visible.

## 27 Feeding: diagnostics

1. **Question:** Are the pPREY settings internally consistent, and do they match the realised diet?
2. **What is plotted:**
   - bar chart: sum of pPREY per predator row (about 100 rows, labels overprinted);
   - table: pPREY vs DietCheck mismatches (9604 rows).
3. **Verdict: INCORRECT (NEW).**
   - The rule is `if (diet_val > eps && pprey_val <= eps) "DietCheck > 0, pPREY = 0"`, with eps = the 0.001 threshold (`feeding_module.R:1802, 1830`).
   - On NESP, pPREYBD has DL = DR = DC = 0.001 and pPREYBCR has DR = 0.001. The table nevertheless reports "DietCheck > 0, pPREY = 0" for BD→DL, BD→DR and BCR→DR. pPREY is not 0; it is at the threshold.
   - Many real pPREY entries are small (e.g. pPREYBCR BGA = 1.68e-7), so they are reported as zero.
   - Pool predators (BD, BCR) are labelled Stage "Juvenile".
   - In the bar chart, sums of up to about 8.5 are all flagged "ok". That is right (pPREY are availabilities, so sums > 1 are allowed), but the subtitle's "values outside 0–1" refers to entries, not sums.
4. **Interpretation risk:**
   - The user would conclude that deposit feeders eat detritus they are not parameterised to eat, a supposed Atlantis or parameter inconsistency.
   - They might then add pPREY entries that already exist.
5. **Redesign must:**
   - report the actual pPREY value;
   - separate "zero" from "below threshold";
   - label pool predators as "pool";
   - match prey stage and the sediment columns (DLsed/DRsed) to DietCheck's aggregated detritus columns;
   - rank mismatches by size rather than listing 9604 rows.

## 28 Mortality: by source (F, M1, M2), FTM

1. **Question:** For each cohort, how much mortality comes from fishing, other natural causes and predation?
2. **What is plotted:**
   - SpecificMort.txt stock S0, cohorts 0–9 labelled Age01–Age10;
   - stacked areas of F, M1 and M2 per annual row, x axis years 0–4;
   - y label "Mortality".
3. **Verdict: INCORRECT (D02, D27 and NEW evidence).**
   - The values are faithful to the file: FTM-0-S0-M2 = 741.9 at day 365; FTM-9 M2 = 14.5; M1 ≤ 0.013.
   - But the file's units are accumulated per-step fractions of the start-of-period stock (D02). A value of 742 cannot be an annual instantaneous rate (exp(−742) ≈ 0), and the axis gives no unit.
   - M1 is invisible next to M2.
   - Stock S0 only (D27; FTM has 1 stock, so no loss here).
   - The age labels ignore NumAgeClassSize = 4.
4. **Interpretation risk:**
   - The user would compare "M2 = 742" with typical M (0.1–1 y⁻¹) and conclude catastrophic predation on cohort 1.
   - They would conclude M1 (where the high adult mL lives in NESP) is zero.
   - Natural mortality would be ignored as a lever.
5. **Redesign must:**
   - convert to a stated unit (instantaneous y⁻¹, or fraction of stock per period);
   - show F, M1 and M2 on a log scale or as shares, so a small M1 is visible;
   - label cohorts by age range;
   - show the S1+ stocks.

## 29 Mortality: predation by predator, FTM

1. **Question:** Which predators cause FTM's predation mortality, by cohort?
2. **What is plotted:**
   - SpecificPredMort with `Group` renamed to "Predator" and the column "FTM" treated as the prey (`process_diagnostics.R:130-175`);
   - faceted by `Cohort` of the row group.
3. **Verdict: INCORRECT (D01, P0) and NOT RENDERED.**
   - **NEW visual consequence of D01:** the facets run Age01…Age15 for a 10-cohort group, because the "ages" are the cohorts of the row groups, which are FTM's prey and include long-lived groups.
   - The legend lists about 100 "predators" of tuna, including DL, DR, MA, PL, PS and BB, which cannot be predators.
   - The panels are empty or degenerate: overprinted axis labels, y ticks "−0.080 … 30".
4. **Interpretation risk:** if values had rendered, the user would conclude tuna are eaten by sardine, anchovy and detritus (the D01 example).
5. **Redesign must:**
   - read SpecificPredMort as rows = prey (Group, Cohort), columns = predators;
   - facet by the focal group's own cohorts only;
   - give units per D02;
   - restrict the legend to predators with a non-zero share;
   - pass a regression test on FTM → SHW and FVL.

## 30 Mortality: Mort.txt relative F vs M, FTM

1. **Question:** How does relative fishing compare with other mortality through time?
2. **What is plotted:** Mort.txt FTM-M and FTM-F per annual row (M: 0, 6.0e-7, 2.94e-6, 2.11e-6, 1.61e-6; F: 1.9e-9–5.1e-9). The subtitle says "Relative only; Atlantis does not treat these as instantaneous rates."
3. **Verdict: CORRECT** as a reading of the file, with an honest caveat.
4. **Interpretation risk:** low. Users may still compare the 1e-6 magnitudes with M from state 28.
5. **Redesign must:** keep the caveat; plot F/M as a ratio (the only meaningful quantity); avoid placing it on an axis that invites comparison with SpecificMort.

---

## Multi-run states (folder `multirun/`, five BH-α sweep runs c1–c5; run 1 = c3)

### 31 Sidebar with five runs (Overview)

- **Verdict: INCORRECT,** for the same reasons as 01 (D21, D22).
- The Overview shows run 1 (c3) only, with no indication that it ignores runs 2–5.
- **Redesign must:** state which run the Overview describes.

### 32 Multi-run: BiomIndx biomass, FTM

- **What is plotted:**
  - BiomIndx FTM per run;
  - a ribbon = min–max over runs;
  - a thick navy line = ensemble mean (`ensemble_module.R:73-91`).
- **Verdict: CORRECT** values. Each run starts at 6660.8 t and ends at 11738 (c1), 12445, 13750 (c3), 15754 and 17717 (c5) t, as drawn.
- **NEW (display):**
  - The ensemble-mean line has no legend entry.
  - It uses `#0B3C5D`, the same colour as the first run in the palette (c1 here).
  - The mean (about 14280 t at the end) is visually indistinguishable from a run.
- **Interpretation risk:**
  - The user can mistake the mean for c1 or c3.
  - These five runs are a parameter sweep (BH α), not a stochastic ensemble, so the "mean and range" framing suggests uncertainty that does not exist.
- **Redesign must:**
  - give the mean a legend entry and a distinct style;
  - say "range across runs", not uncertainty;
  - give units (t);
  - align time axes (D26).

### 33 Multi-run: YOY, FTM

- **What is plotted:** YOY.txt FTM-0 per run, plus the ribbon and mean.
- **Verdict: INCORRECT (NEW, observed).** The legend lists only c1, c2, c4 and c5: **run 1 (c3) is missing.**
  - The navy line at about 160 in year 1 is the unlabelled 4-run mean: (59.5 + 88.2 + 211.0 + 274.5)/4 = 158. It is not a run, and c3's true value is 130.4.
  - Root cause UNVERIFIED. It is consistent with D26 (the YOY reading path differs between run 1 and the extra runs).
- **Interpretation risk:** the user would believe the current run (c3) is plotted. They would read the mean as c1 (same colour), and the ensemble statistics would exclude the run under study.
- **Redesign must:**
  - assert that every loaded run is present in every series (warn otherwise);
  - give the mean its own legend entry;
  - give units.

### 34 Multi-run: SSB, FTM

- **Verdict: CORRECT** values; all five runs are present.
- The SSB.txt artefacts (zero in year 1 and at day 1800) appear in every run.
- **Redesign must:** flag accumulator resets, as in 22.

### 35 Multi-run: BiomIndx biomass, FTB

- **Verdict: CORRECT** values (BiomIndx, about 96 t → 160–440 t).
- **Redesign must:** meet the same mean-line and units requirements as 32.

---

## Small viewport (1024×700) — scientific consequences only

The values are identical to desktop; the review checked each of the 30 captures. The following layout effects change what a scientist can actually read.

| States | Effect |
|---|---|
| 01 | The card labels wrap letter by letter. The Exploding count is still visible without D22 context. |
| 02/03 | Panel titles are truncated to fragments ("e Tuna", "revalla"). **FTB and FTM cannot be told apart**, so a user can attribute bigeye's 3× increase to southern bluefin. Group codes must stay visible. |
| 06/07/14/15/18 | The legends are cut off below the fold, so cohorts, prey and predators cannot be identified. |
| 08–12 | Only the colour bar is above the fold. The map, and therefore the spatial pattern, is not visible. |
| 21 | The S–R scatter is mostly below the fold. Only the start-up artefact point (919 at SSB 0) is visible, which emphasises the misleading point. |
| 25/26 | Row labels are truncated ("Humpback whale (WH"), so the predator stage is not readable. |
| 27 | The bar-chart labels collapse into an unreadable block. |
| 29 | The facet labels overprint each other, as on desktop. |

---

## New findings (not in `defects.md`)

| # | Finding | Evidence | Location |
|---|---|---|---|
| N1 | The relative ("scaled") Compare total for pools has the **wrong shape**, not only the wrong units. ZL is shown at +28 % then about 0.5. The truth is −68 % then about 0.14 (both BiomIndx and Σ ZL_N·`volume`). D31/D19b volume weighting does not cancel when scaled. | `output.nc` and BiomIndx ZL, state 05 | `M/compare_module.R:206-212`; `utils.R` `boxes.prop` |
| N2 | Cohort labels ignore NumAgeClassSize (FTM = 4 years per cohort). Indexing is inconsistent across tabs: Age01–10, Age 1–10, Age00–09. | `NESP_groups.csv`; grep finds no ageclasssize in `M/` | `compare_module`, `ages_module`, `predation_module`, `mortality_module` |
| N3 | The Ages legend and colours are sorted lexicographically (Age 1, Age 10, Age 2…). | state 07 | `M/ages_module.R:67` |
| N4 | The overlap index on a layer where neither group lives gives 0.667 from 1e-30 t residues (7198 cells) and sets the colour scale. The sidebar says the series is Schoener's D, but it shows the box-0 local index. | `output.nc` FTM/FMM z = 6 | `M/spatial_module.R:30, 161-190, 366-384`; `process_diagnostics.R:351-401` |
| N5 | Trophic levels: primary producers are hard-coded to TL 1.5 (LG_PHY) and 1.2 (DINOFLAG, MPB). The two-pass prior-seeded iteration is not solved to convergence. ZL = 4.06 sits above sardine and mackerel. | state 19 table | `M/foodweb_module.R:155-174, 229-257` |
| N6 | Recruitment YOY plot: "New YOY" and "New larvae" overplot and hide "Atlantis YOY" and "Larvae" when α and β are unchanged, so the visible "Atlantis-coloured" series is the BH larvae. | YOY.txt FTM-0 = 919.7/130.4/…; code | `M/recruitment_module.R:157-185, 488-497` |
| N7 | The S–R scatter pairs YOY and SSB at the same timestamp, with no lag, over 66 repeated step rows. It includes SSB = 0 start-up and reset rows. | SSB.txt FTM, YOY.txt | `M/process_diagnostics.R:252-293`; `recruitment_module.R:290-315` |
| N8 | Feeding diagnostics label pPREY ≤ threshold as "pPREY = 0". BD→DL/DR and BCR→DR are falsely flagged; their pPREY is 0.001. | `NESP_bio.prm` pPREYBD, pPREYBCR | `M/feeding_module.R:1801-1802, 1829-1830` |
| N9 | SpecificPredMort-by-predator shows Age11–Age15 facets for a 10-cohort group, and a legend of about 100 "predators" including detritus and phytoplankton. This is the visible symptom of D01. | state 29 | `M/process_diagnostics.R:130-175` |
| N10 | Growth view defaults to padded cell z = 0 of boundary box 0 and shows limitation = 0 (D29 made visible). | `output.nc` box 0 Temp z0–3 = 0 | `M/growth_module.R:60-90` |
| N11 | Multi-run: the ensemble-mean line has no legend entry and shares run 1's palette colour. In the YOY series run 1 (c3) is absent, so the plotted mean excludes the current run. | YOY.txt c1–c5 FTM-0; mean 158 = 4-run mean | `M/ensemble_module.R:73-91` |
| N12 | Spatial, Growth and By-age default to boundary box 0, whose state Atlantis holds constant (Temp 15.0 in all layers; FTM 1.74 t). Every default series is flat. | `output.nc` box 0 | `M/spatial_module.R` (box default 0); `growth_module.R` |
| N13 (partial) | The Predation "predators of FTM" proxy (DietCheck × biomass) and SpecificPredMort disagree on the FVL share: about 0–0.15 in the proxy vs about 0.78 from day 360 in SpecificPredMort. | DietCheck, BiomIndx, SpecificPredMort FTM rows | `M/predation_module.R:193-224` |

---

## Summary table

| State | Verdict | Defect IDs | Must-haves for redesign |
|---|---|---|---|
| 01 Overview | INCORRECT (parts) | D21, D22, D18, D20 | B0≤0 → "undefined"; trajectory not only endpoint; cards from NetCDF; weighted cohort condition; ranked problems |
| 02 Compare facets abs | CORRECT (captured mid-render) | D20 | units; free-axis disclosure; full group codes |
| 03 Compare relative | INCORRECT (parts) | D22 | undefined-ratio label; fixed or log y with 1.0 line and band; B0 in hover |
| 04 Total FTM scaled | CORRECT | — | show B0 and source |
| 05 Total ZL scaled | INCORRECT | D31, D19b, N1 | NetCDF volume/area × X_CN × mg2t; BiomIndx cross-check warning |
| 06 Age bars FTM | CORRECT values / INCORRECT labels | N2 | cohort age ranges; one indexing; numbers view |
| 07 Ages FTM | CORRECT values / labels | N2, N3 | numeric colour order; units; KWRR/KWSR reference lines |
| 08 Map FTM | CORRECT | N12 | non-boundary default; density option; scale-basis note |
| 09 Map MA | INCORRECT | D11, P0 | × area; units; total vs BiomIndx check |
| 10 Map Temp surface | CORRECT | N12 | °C; no boundary default; depth of "surface" |
| 11 Map Temp sediment | CORRECT | — | °C; inherited-from-bottom note |
| 12 Overlap FTM×FMM | INCORRECT | D30, N4 | integrated default; mask low biomass; Schoener's D headline; index on shares |
| 13 By age, box 0 sediment | NOT RENDERED | N12, D29 | dynamic default cell; "no individuals" message |
| 14 Consumption FTM eats | INCORRECT | D16, D15, P0 | validated intake in t d⁻¹; weighted shares; % body mass check |
| 15 Consumption FMM eaten | INCORRECT | D16, D15 | as 14, plus removal as fraction of prey biomass |
| 16 Physics Temp box 94 | CORRECT | D21 (axis) | °C; shared y; layer depths |
| 17 Multi-run, 1 run | NOT RENDERED (expected) | — | show the single run |
| 18 Predation FTM | UNVERIFIED | D15, D21, N2, N13 | weighted diet; honest predator-panel label or SpecificPredMort; one time axis |
| 19 Food web FTM | INCORRECT | N5, D15 | producers TL = 1; solved system; weighted diet |
| 20 Recruitment YOY | INCORRECT | D03, D04, N6 | Atlantis series on top; hide unchanged "new"; units; flag year-1 |
| 21 Recruits vs SSB | UNVERIFIED / misleading | D04, N7 | lagged per-event pairs; drop SSB = 0 artefacts; BH overlay |
| 22 SSB series | CORRECT (file artefacts) | — | flag resets and zeros; units |
| 23 Growth PL | INCORRECT (this state) | D29, D23, N10 | mask padded cells; depth labels; verify formulas |
| 24 Catch | NOT RENDERED (expected) | D14 (untestable) | numbers vs tonnes labelling |
| 25 pPREY matrix | CORRECT (parsing) | D25 (n/a on NESP) | all row labels; "availability" scale; pool and sed columns marked |
| 26 Predation pressure | INCORRECT | D05, D06–D10 (D07 visible) | "screening index" label; realised intake alongside; pool dedupe; log colour |
| 27 Feeding diagnostics | INCORRECT | N8, D08 | report actual pPREY; zero vs below threshold; pool labels; rank |
| 28 Mortality by source | INCORRECT | D02, D27, N2 | convert units; log or share scale; age ranges; S1+ |
| 29 Mortality by predator | INCORRECT / NOT RENDERED | D01, N9, D02 | transpose fix; own-cohort facets; regression FTM → SHW, FVL |
| 30 Mort.txt F vs M | CORRECT | — | keep caveat; plot F/M ratio |
| 31 Overview, 5 runs | INCORRECT | D21, D22 | say which run |
| 32 Multi-run biomass FTM | CORRECT values | N11, D26 | labelled mean; "range", not uncertainty; units |
| 33 Multi-run YOY FTM | INCORRECT | N11, D26 | all runs present check; labelled mean |
| 34 Multi-run SSB FTM | CORRECT | — | flag resets |
| 35 Multi-run biomass FTB | CORRECT | N11 | as 32 |

**Counts (35 states):**

| Verdict | States | Count |
|---|---|---|
| CORRECT (incl. correct values with label problems) | 02, 04, 06, 07, 08, 10, 11, 16, 22, 25, 30, 32, 34, 35 | 14 |
| INCORRECT | 01, 03, 05, 09, 12, 14, 15, 19, 20, 23, 26, 27, 28, 29, 31, 33 | 16 |
| UNVERIFIED | 18, 21 | 2 |
| NOT RENDERED | 13, 17, 24 | 3 |

29 is counted as INCORRECT; it is also effectively not rendered.

### Cross-cutting requirements for any redesign

1. Every axis and colour bar carries a unit.
2. No 0, Inf, NaN or residue (1e-30) value is converted into a verdict, a colour or an index.
3. Pool and epibenthic totals are checked against BiomIndx.
4. Boundary boxes and padded cells are never defaults, and are marked when shown.
5. Cohorts are labelled by age range, with one indexing convention.
6. Atlantis's own output is drawn on top of any RA-derived or "what-if" series.
7. Proxy diagnostics (expected predation pressure, diet × biomass, BH larvae) are labelled as proxies until D03, D05, D15 and D16 are fixed.
8. One calendar time axis is used across all tabs.
