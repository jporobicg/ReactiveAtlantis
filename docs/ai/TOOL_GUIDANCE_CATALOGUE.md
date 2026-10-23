# Tool guidance catalogue

Validated registry: `inst/ai/tools.json`.
`tool_id` values are the only navigation targets the Guide may emit.
They match `ra_ai_nav_catalog()` and `nav_panel()` titles in
`inst/shiny/app.R`, verified against commit `11c4ee7`.

Standalone exports (`predation()`, `growth.pp()`, `recruitment.cal()`,
`mortality()`, `compare()`, `feeding.mat()`, `food.web()`, `catch()`)
are **not** tool ids. `skill_assessment` is Catch view `skill`, not a
tab.

Question → tool ranking is deterministic (`ra_ai_rank_tools()`) and
runs before any language-model call.

## overview

- purpose: Run size and group-health flags (extinct, exploding, odd
  ResN/StructN cohorts).
- example questions: What can Reactive Atlantis do? Which groups look
  extinct? Why is a group collapsing?
- preconditions: Groups CSV.
- function refs: `overview_ui()`, `overview_server()`,
  health table builder in `project_store.R`.
- navigation_target: Overview
- look for: groups below 1% of initial biomass, >10× growth, age
  classes outside 0.5–1.5 of initial ResN/StructN.
- limitations: Flags are ratios, not diagnoses.

## compare

- purpose: Biomass (and numbers) through time; relative Bt/B0; age
  structure.
- views: `facets`, `total`, `age`, `age_bars`.
- example questions: Where do I see biomass trajectories? Is Bt/B0
  outside 0.5–1.5?
- preconditions: Groups CSV plus BiomIndx or output NetCDF.
- navigation_target: Compare
- look for: trend vs initial, age-structure change, observed points
  when an observations CSV is loaded.
- limitations: Does not identify the process that caused a trend.

## ages

- purpose: Whole-system age-class ResN, StructN, Numbers, Biomass.
- example questions: How do I inspect condition by age? Where is
  reserve nitrogen by cohort?
- preconditions: Groups CSV and output NetCDF.
- navigation_target: Ages
- look for: numbers-weighted ResN/StructN; biomass =
  `Nums*(ResN+StructN)*mg2t*x.cn` with mg2t `2e-8` and x.cn `5.7`.
- limitations: Whole-system only. Spatial by-age is on Spatial.

## spatial

- purpose: Box map by layer and time; series for one box, all boxes,
  or the system; by-age in one box/layer; predator-prey overlap.
- views: `map`, `age`, `overlap`.
- example questions: Where is the biomass? Did it move into poor
  boxes? What does one layer look like?
- preconditions: Groups CSV, output NetCDF, BGM.
- navigation_target: Spatial
- look for: unused boxes, padded layers excluded, sediment last.
- limitations: North Sea fixture BGM can have more boxes than the
  sample NetCDF.

## consumption

- purpose: Tonnes consumed from `outputPROD.nc` (Eat / Grazing) with
  DietCheck.
- example questions: Is intake too low? Where is consumption?
- preconditions: Groups CSV and production NetCDF.
- navigation_target: Consumption
- limitations: Disabled when PROD is missing. Not a diet-preference
  editor.

## physics

- purpose: Temperature, salinity, NO3, NH3, Chl_a, denitrification,
  `nominal_dz` by box and real water-column layers.
- example questions: Where are temperature and nitrate shown?
- preconditions: Output NetCDF (BGM / nlayers / dz for layer counts).
- navigation_target: Physics
- limitations: Does not prove a biological response to a tracer.

## ensemble

- purpose: Overlay biomass, SSB, YOY or catch across extra run pairs.
- example questions: How do I compare two runs?
- preconditions: Groups CSV and at least one extra pair.
- navigation_target: Multi-run
- limitations: Needs named extra runs in the sidebar.

## predation

- purpose: Realised diet composition (DietCheck), predator and prey
  views.
- example questions: Where do I inspect predation? What is this group
  eating?
- preconditions: Groups CSV, BiomIndx, DietCheck.
- navigation_target: Predation
- look for: diet shifts, top prey, Other remainder.
- limitations: Realised diet is not pPREY.

## foodweb

- purpose: Network plus trophic-level table.
- example questions: How do I see the food web?
- preconditions: Groups CSV and DietCheck.
- navigation_target: Food web
- limitations: Thresholds hide weak links.

## recruitment

- purpose: YOY, SSB, recruits vs spawning stock, Beverton-Holt
  alpha/beta side panel; PP growth view.
- views: `yoy`, `ssb`, `sr`, `growth`.
- example questions: What should I check when recruitment is low?
- preconditions: Groups, output NetCDF, YOY, biology prm.
- navigation_target: Recruitment
- look for: YOY.txt `CODE.0`, SSB, larvae from the curve.
- limitations: Recalculate is a what-if on the curve, not a file write
  of a new official run.

## growth

- purpose: Light / nutrient / eddy limitation for **primary producers**.
- example questions: Are primary producers limited?
- preconditions: Groups, output NetCDF, biology prm.
- navigation_target: Growth
- limitations: Not a vertebrate growth-in-weight tab. Use Ages /
  Compare for ResN/StructN.

## catch

- purpose: Catch by group or fleet; numbers-at-age; skill vs observed.
- views: `by_group`, `by_fleet`, `numbers`, `skill`.
- example questions: How do I compare catch to observations?
- preconditions: Groups, fisheries CSV, catch NetCDF. Skill needs
  observed catch.
- navigation_target: Catch
- limitations: Skill is this tab’s `skill` view, not a separate tool.

## feeding

- purpose: Full pPREY matrix with hover/click and row/column bars;
  realised DietCheck and consumed biomass for the selected pair;
  diagnostics; edited-vs-original compare.
- views: `matrix`, `diagnostics`, `compare`.
- example questions: What is pPREY? What does KLP mean?
- preconditions: Groups, biology prm, initial-conditions NetCDF.
- navigation_target: Feeding
- look for: pPREY > 0 with zero gape overlap; uneaten prey; DietCheck
  mismatch.
- limitations: pPREY is availability, not realised diet. In-session
  edits are not written to the prm file by the Guide.

## mortality

- purpose: Fishing, natural, and predation mortality stacked by age;
  predation mortality by predator; Mort.txt relative F vs M.
- views: `source`, `predator`, `mort`.
- example questions: What does mL represent? Has fishing risen?
  Which predators drive predation mortality?
- preconditions: Groups and SpecificMort. SpecificPredMort and Mort.txt
  are optional and reported when missing.
- navigation_target: Mortality
- limitations: mL is linear natural mortality, not fishing. Mort.txt is
  relative only.
