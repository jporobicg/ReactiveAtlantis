# Visual inventory (static, pre-screenshot)

**The visual audit is INCOMPLETE.** No plot has been rendered yet, because R and a headless browser are not installed on estigia. The scores, the screenshot review and the prototypes (sections 7–9 of the brief) begin once the app can run.

Each plot below is listed with:

- the scientific question it is meant to answer;
- science issues already known from `../defects.md`;
- the design questions to check on the rendered screenshot.

A plot with a P0/P1 science issue fails until that issue is fixed, regardless of how it looks.

| # | Plot | Question it answers | Known science issues | Design questions for screenshots |
|---|---|---|---|---|
| V01 | Overview cards | Model size and time span | D21: cards may always show "—" | Hierarchy; empty-state wording |
| V02 | Overview health table | Which groups collapse or explode? | D22: zero-initial groups flagged "Exploding". Thresholds are screening rules but not labelled as such | Sorting, colour coding, distinction between screening and diagnosis |
| V03 | Compare facets | Biomass trajectory per group | D22 (Bt/B0 base is t1 for pools); D11, D12 for pools | Absolute vs relative; free y hides magnitude; 0.5–1.5 band meaning |
| V04 | Compare total / age / age_bars | Age structure through time | `geom_col(width=20)` on a Date axis | Heatmap vs bars |
| V05 | Ages | RN, SN, numbers by age class | — | Small multiples vs overlaid lines; reference to KWRR/KWSR |
| V06 | Spatial map (water, sediment, integrated) | Where is the biomass? | D18 (integrated = mean incl. sediment; tonnes per box, so large boxes dominate); polygon mapping not yet tested | Density vs total; fixed vs per-step scale; skewed distributions; zero vs missing; hover content |
| V07 | Spatial by age | Where are the age classes? | as V06 | as V06 |
| V08 | Spatial overlap map + D series | Do predator and prey co-occur? | D30 (local index is scale-dependent) | Show both distributions side by side? |
| V09 | Spatial per-box time series | When does a box change? | D18 ("whole system" sums concentrations) | Linking to the map |
| V10 | Physics tracers | Physical drivers by box and layer | D18 (unweighted box mean) | Layer labels; depth axis |
| V11 | Multi-run overlay / ensemble band | How do runs differ? | D26 (run 1 catch missing; no compatibility check) | Difference-to-baseline view |
| V12 | Feeding: pPREY heatmap | What can each predator eat? | D25 (column fallback) | Large-matrix readability (~100 × 200 for NESP); stage labels |
| V13 | Feeding: gape flags | Is a diet link blocked by gape? | D08, D09 | — |
| V14 | Feeding: predation-pressure heatmap + per-box map + realised table | Where is predation excessive? | D01, D02, D05–D10: **currently fails science review** | Ranked contributions vs matrix |
| V15 | Feeding: realised diet series | What is actually eaten? | D15 | Stacked area vs heatmap |
| V16 | Consumption (predator / as prey) | How much is consumed? | D15, D16 | Units on the axis |
| V17 | Mortality stacked by age | Which mortality source dominates? | D01 (predator view transposed), D27 | Rate vs fraction labelling |
| V18 | Recruitment: YOY, SSB, S-R with BH curve | Is recruitment consistent with BH? | D03, D04: **currently fails science review** | Scatter + curve + residuals; realised vs expected |
| V19 | Growth limitation | What limits primary producers? | D23 | Layer selection |
| V20 | Catch by group / fleet / skill | Is catch right? | D14: **currently fails science review** | Stacked vs lines |
| V21 | Food web network | Trophic structure | not reviewed | Readability with ~100 nodes |
| V22 | Predation diet composition | Diet through time | D15 | Top N + "Other" |
| V23 | Scientific Guide drawer | Q&A and navigation | S07, S12 | Distinguishing evidence from hypothesis |
