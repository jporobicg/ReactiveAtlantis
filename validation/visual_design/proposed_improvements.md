# Proposed improvements (communication)

## About this document

- **Scope.** Each entry concerns a plot that needs substantial change. IDs match `visual_inventory.md`; the scores are in `plot_scorecard.md`; tokens and palettes are in `design_system.md`.
- **Science defects come first.** Where a P0/P1 defect applies, the redesign is **blocked on the science fix**, and redesigning a wrong number only makes it more convincing. Those entries are marked "fails science review — see scientific_visual_review.md". They are still listed because their communication problems survive the fix.
- **Cost.** S = under 1 day, M = 1–3 days, L = more than 3 days, for someone who knows the modules.
- **Class.** Critical = a signal is hidden or a reader will be misled. High = it blocks a calibration task. Medium = slows the task. Low = polish.
- **Owner approval.** Per the owner's rule, every change to defaults or aggregation needs to be shown with concrete examples and approved before it is implemented.

---

## 1. Spatial maps (V11–V14, V41): Critical, cost M

**Problem.**
- The ramp starts at `#F7F4EC`, which is the page colour, and is linear with a "stable" global maximum. On FTM at t1 almost every box is background-coloured (D/08), so the user cannot see where the biomass is.
- Zero, low values and the page look identical, and NA (`#C5C0B2`) sits between them.
- The hover gives only "Box id / value": no group, layer, time or units.
- The epibenthic colour bar reads "500µ" t (D/09).
- The default overlap layer (Sediment for a tuna) gives an all-zero map with no message (D/12).
- At 1024 the map is below the fold, under a debug box (S/08).

**Alternatives.**
- **A (recommended).**
  - Sequential blue 150→700 on a **log10** scale for biomass and density, or quantile bins (≤ 7). Tracers stay linear.
  - Zero = white with outline; no data / not applicable = hatched light grey; values above a robust 98th-percentile cap are shown as "≥".
  - Default quantity = density (t km⁻²), with total per box as an option.
  - The full tooltip spec in design_system §6.
  - Move the debug list into a collapsed "Diagnostics" panel.
- **B.** Two linked maps side by side for overlap (predator | prey, each with its own ramp), with the overlap index as a third small map or as a box-ranked bar chart. This shows *why* overlap is low, not just that it is.
- **C.** Add a ranked "top-20 boxes" bar chart beside the map. It is an exact readout that does not depend on colour discrimination, and doubles as the table view.

**What becomes easier.** Seeing the distribution at a glance, telling zero from missing, comparing time steps, and accessibility (the tooltip and ranked bars give exact values).

**What could be lost.**
- A log scale under-emphasises absolute dominance by one box. Mitigate with a "linear / log / quantile" toggle and a printed transform.
- Density vs total changes which boxes look important. The science reviewer must sign off.

**Workflow impact.** Positive. The map becomes usable without hovering every box. The defaults change (box, layer), so the change must be announced.

**Note.** V12 also fails science review (D11). Redesign only after the fix.

---

## 2. Compare small multiples (V05/V06): Critical, cost M

**Problem.**
- 101 panels in a fixed-height card; about half are clipped (D/03).
- Facet titles are about 7 px and truncated.
- Free y puts the 0.5–1.5 band at a different height in every panel.
- Panels are ordered alphabetically, so collapsing and exploding groups are scattered.
- Each panel has an identity colour that means nothing.
- Unusable at 1024 (S/03): titles cut to 3 letters.

**Alternatives.**
- **A (recommended): status-sorted multiples.**
  - Shared log2 y for B/B0 (ticks 0.25 … 4), with the band at the same height everywhere.
  - Panels sorted worst first, using the Overview health status; guild strip colour on the title; ink line.
  - Card height grows with rows (120 px per row), and the column count is responsive.
- **B: B/B0 heatmap.**
  - Rows = groups (sorted by status or guild), columns = time.
  - Diverging blue/red around 1 on log2, with the band edges marked in the legend.
  - All 101 groups fit on one screen, and each row expands on hover or click into the line plot.
- **C.** Show A and B as two views of the same data; B is the default for more than 30 groups.

**What becomes easier.** Finding the worst groups first, comparing magnitudes of change across groups, and seeing all 101 at once (B).

**What could be lost.**
- A shared log y flattens subtle within-band oscillation (keep the "free y" option).
- The heatmap loses exact trajectory shape (the drill-down restores it).
- Absolute magnitudes are not shown in the relative view.

**Workflow impact.** Large positive. This is the primary triage screen in calibration.

---

## 3. Feeding matrices: pPREY (V36) and pressure (V39) plus placeholders (V37/V40/V41/V42): Critical, cost L

**Problem.**
- About 200 × 100 cells in a fixed-pixel plotly heatmap; only about 18 × 25 are visible.
- Labels are long, repeated ("· juvenile / adult prey") and truncated.
- Zero is drawn as pale yellow, the same as small values.
- The pressure matrix is uniformly pale (no signal) and took 113 s to render.
- Four empty 300 px placeholder plots, a methods paragraph listing 147 box IDs, and a red "Spatial map cannot draw" box in the normal state.
- The click handler is unregistered (warning S20), although the text promises clicking.
- Pressure **fails science review** (D05–D10).

**Alternatives.**
- **A (recommended).**
  - Order rows and columns by guild, then trophic level, with guild bands on both margins.
  - Zero = white; non-zero = sequential blue (log for pPREY). Gape-blocked links get an overlaid "×" glyph; DietCheck-realised links get a dot (size = realised share). Structure, gape and realisation are then read in one matrix.
  - Collapse juvenile/adult into a 2-row sub-band per predator, with a shared label.
  - Add a "Focus" mode: picking a predator shows a one-row ranked bar chart of its pPREY vs realised diet. This replaces three of the four placeholders.
- **B.** A predator-centric diet-comparison bar chart (pPREY share vs DietCheck share per prey) as the default. The full matrix stays as an overview thumbnail with brushing.
- **C (pressure, after the science fix).** Ranked list of the top-N predator→prey pairs by expected/realised log ratio, coloured diverging around 0, with the per-box map shown for the selected pair.

**What becomes easier.** Seeing the structure of the diet matrix, spotting gape-blocked and unrealised links, and acting on one predator at a time.

**What could be lost.**
- Matrix ordering differs from the prm order. Offer an "as in biology.prm" toggle so modellers can map rows to lines.
- The overlaid glyphs add density, so offer an option to hide them.

**Workflow impact.** High. pPREY tuning is a core calibration loop. Placeholders disappear and the screen fits 1024.

---

## 4. Predation mortality by predator (V48): Critical, cost S (after the science fix)

**Problem.**
- The plot collapses into overprinted facet labels with a legend of about 100 groups (D/29).
- It also **fails science review** (D01, transposed).

**Alternatives.**
- **A.** Top-N predators + Other (Tier-B slots in CSV order), a single stacked area for the selected age class, and a direct-labelled legend. Facets only when the user picks "by age" (≤ 4 ages per row).
- **B.** A predator × age heatmap of mean M2 (sequential), with a sparkline per predator.

**What becomes easier.** Identifying the main predators of a group at a given age.

**What could be lost.** Minor predators fold into Other; the tooltip lists them.

**Workflow impact.** Positive. This view is needed for the mortality and predation diagnostics the owner uses.

---

## 5. Recruitment: YOY/larvae (V29/V30) and Recruits vs SSB (V31/V32): High, cost M

**Problem.**
- The legend shows 4 series but 2 are hidden by overlap, and the navy and teal series are near-identical.
- Larvae and YOY share one axis without units, and the second panel duplicates the first.
- The S-R plot has no BH curve, a continuous colour bar for 5 years whose labels collide ("20202021…"), and SSB = 0 artefacts.
- The x axis is labelled "SSB", but D04 says it is total biomass.
- **Fails science review** (D03, D04).

**Alternatives.**
- **A (recommended).**
  - Panel 1: Atlantis YOY (ink) vs BH-predicted YOY from the current α/β (blue) and from the edited α/β (orange dashed), all as annual points with lines, plus a log-ratio residual strip below.
  - Panel 2: the stock–recruit scatter, with discrete year labels on the points, the BH curve(s) overlaid, and the x axis named exactly as computed ("Total stock biomass, mg N", per D04).
- **B.** A small-multiple "what-if" grid of 3 α × 3 β BH curves over the observed points, for quick parameter search.

**What becomes easier.** Judging whether the BH parameters reproduce Atlantis recruitment, and seeing what an α/β edit does.

**What could be lost.** The larvae series (keep it as a toggle).

**Workflow impact.** High for the tuna recruitment work.

---

## 6. Consumption (V18/V19) and Predation diet (V24/V25): High, cost S–M

**Problem.**
- Stacked areas with spiky seasonality; the top bands are unreadable.
- Near-duplicate colours in the same view (FMN/SQO ΔE 2.6; SQN/FPL CVD ΔE 0.6).
- Code-only legend, out of stack order.
- 100 % stacks hide absolute magnitude.
- Consumption **fails science review** (D15, D16: magnitudes about 1e-11).

**Alternatives.**
- **A (recommended).**
  - Tier-B palette assigned in CSV order within the view, with "Other" grey + hatch.
  - Bands separated by 1 px white gaps; legend in stack order with code + name; direct labels for the top 3.
  - An "absolute | share" toggle shown side by side as two small panels sharing x.
  - Annual or seasonal aggregation by default (the raw steps stay available).
- **B.** A prey × time heatmap (share, sequential) for more than 6 prey, plus a total-intake line above it.

**What becomes easier.** Reading the minor prey, comparing shares across years, and seeing absolute intake.

**What could be lost.** Within-season detail under annual aggregation (keep the raw view).

**Workflow impact.** Positive. The diet checks feed pPREY tuning.

---

## 7. Multi-run overlay (V23): High, cost S

**Problem.**
- **Colours follow rank, not run**: c1 is teal in M/32 but navy in M/33.
- c1 and c5 are close (normal-vision ΔE 12.5).
- No units, model years on the x axis, the stale "add another run" help text, and the ensemble mean is not distinguishable.
- SSB overlaps completely (M/34).

**Alternatives.**
- **A (recommended).**
  - Base run in ink at 2.5 px; other runs in Tier-B slots fixed by run index, with direct end labels.
  - Ribbon = min–max, with a dashed median.
  - A "difference to base" toggle that plots run − base or run/base on a diverging scale.
- **B.** Small multiples per run, sharing y, for more than 8 runs.

**What becomes easier.** Tracking a run across series, and seeing which lever moves a group.

**What could be lost.** Nothing material.

**Workflow impact.** High for wave or ensemble comparison.

---

## 8. Ages (V09), age structure (V08) and spatial by age (V17): Medium–High, cost S–M

**Problem.**
- V08 uses Brewer Paired, which implies pairs that do not exist, and makes old ages slivers. The validator fails it.
- V09 has a text-sorted legend (1, 10, 2), viridis yellow invisible on white, and no units or KWRR/KWSR references.
- V17's default is empty panels.
- The age label base differs between tabs (Age00 vs Age01).

**Alternatives.**
- **A.** Age × time heatmaps (one per variable: biomass, numbers, ResN/KWRR, StructN/KWSR).
  - Sequential for absolute values; **diverging around 1 for ResN/KWRR and StructN/KWSR**, which is the "thin adult" signal the owner cares about.
- **B.** Lines with the selected age highlighted (ink) and the others grey, plus reference lines at KWRR/KWSR.
- In both, the legend is sorted numerically, ages are 1-based app-wide, and V17 defaults to the box with the most biomass.

**What becomes easier.** Seeing condition (ResN relative to its reference) across ages at a glance, and seeing old-age collapse.

**What could be lost.** Exact line values (the tooltip restores them).

**Workflow impact.** High. This connects to the "thin-adult" rule.

---

## 9. Physics (V20/V21) and Growth limitation (V34): Medium, cost S

**Problem.**
- V20: 7 facets about 40 px tall with overlapping ticks, rainbow hues for ordered layers, and no °C.
- V21: the deepest layer flattens all the others.
- V34: the default box 0 / layer 0 shows nothing, and the series overlap at 0.

**Alternatives.**
- **V20: a layer × time heatmap** (sequential, one hue, with units), plus an optional line for one selected layer.
- **V21:** a depth-profile bar with a log x axis, or labelled layer bands on a depth axis.
- **V34:**
  - small multiples per factor (4 rows sharing a 0–1 y), with "limiting factor" = the minimum highlighted;
  - default to the box and layer of maximum producer biomass;
  - an option for a map of the limiting factor per box.

**What becomes easier.** Seeing the vertical structure, and what limits production.

**What could be lost.** Little.

**Workflow impact.** Medium.

---

## 10. Feeding diagnostics: preference sums (V43) and table (V44): Medium, cost S

**Problem.**
- About 200 overprinted labels, and a single "ok" colour, so nothing stands out.
- The table shows 9,604 rows of raw doubles (1.87e-35), not sorted by severity.

**Alternatives.**
- **A.** A dot plot of the preference sum per predator-stage, sorted, labelled only for flagged or out-of-range rows, with status colours + icons and a band at the plausible range. The others are grey and not labelled.
- **B.** Table: formatted values, a "Severity" column, the default sort by severity, and rows suppressed below a noise threshold (the count is shown).

**What becomes easier.** Finding the predators with odd preference sums.

**What could be lost.** Labels for unflagged rows (the tooltip has them).

**Workflow impact.** Medium.

---

## 11. Overview KPI tiles and health table (V01, V03): Medium, cost S

**Problem.**
- Tiles break letter by letter at 1024; white on gold is 2.4:1; "—" is ambiguous.
- Ratios print as 21 digits; groups with initial = 0 head the list as "Exploding" (D22).

**Alternatives.**
- Neutral tiles with status chips (icon + label) that click through to the filtered table.
- Ratio formatter ("> 10⁶ (initial = 0)" → status "n/a").
- A sparkline column (B/B0 on log2) and a "Compare" link per row.

**What becomes easier.** Triage straight from the Overview.

**What could be lost.** Little.

**Workflow impact.** Positive.

---

## 12. Mortality by age (V47): Medium, cost S

**Problem.**
- The legend lists F, M1 and M2 but only M2 is visible.
- Free y per age hides the age gradient; there are no units.
- Five annual points are drawn as smooth areas.

**Alternatives.**
- **A.** An age × source grouped bar per year, or a line per source on a **shared** y (log), with the age on the x axis for a selected year: the classic mortality-at-age plot.
- Sources with zero values are listed as "F = 0 throughout" in the subtitle.

**What becomes easier.** Comparing mortality across ages, which is what the high adult mL work needs.

**What could be lost.** The per-age time detail (keep it as a toggle).

**Workflow impact.** Medium–High.

---

## 13. Food web (V27): Low–Medium, cost M

**Problem.**
- "DownTop/TopDown" jargon; the focal group is not highlighted; arbitrary x.
- White labels on yellow; edge colours fail CVD (protan 5.6).

**Alternatives.**
- Focal group in the centre column; prey below-left and predators above-right; y = TL.
- Edge width = diet share; edge colour ink, with direction shown by an arrowhead rather than colour.
- Node fill = guild hue, with labels outside the node in ink.

**What becomes easier.** Reading who eats whom around the focal group.

**What could be lost.** The overall network shape (keep an "all groups" layout).

**Workflow impact.** Low.

---

## 14. Application shell: responsive layout and empty states: High, cost M

**Problem.**
- At 1024 two sidebars leave about 300 px for plots. Primary plots start below the fold on Spatial, Feeding and Multi-run.
- The navbar wraps; the busy state shows a 15 % ghost of the stale plot.
- Empty states render blank white plot boxes under active controls (V22, V35).

**Alternatives.**
- The run sidebar auto-collapses after load; tool controls become a toolbar at < 1400 px; tabs are grouped into menus.
- Empty state = a single card with the action button (Add run / Attach files), and **no** plot or controls rendered.
- A skeleton loader replaces the ghost plot.

**What becomes easier.** Laptop and iPad use. The owner works from an iPad via estigia, so this matters.

**What could be lost.** The always-visible file checklist (move it to a "Run" popover).

**Workflow impact.** High.

---

## Top redesigns by impact

| Rank | Redesign | Class | Why first |
|---|---|---|---|
| 1 | Spatial maps: log/quantile ramp, zero ≠ missing ≠ background, full tooltip, sensible defaults (§1) | Critical | Today the map hides where biomass is; every spatial question depends on it. |
| 2 | Compare: status-sorted multiples + B/B0 diverging heatmap, no clipping (§2) | Critical | The primary calibration triage screen; half the groups are currently invisible. |
| 3 | Group colour strategy: guild hue + Tier-B slots by CSV order + Other/hatch; drop the 101-colour walk (design_system §4.1) | Critical | Near-duplicate colours inside the same diet/consumer views (ΔE 0.6–2.6) mislead identity across Consumption, Predation and Mortality. |
| 4 | Feeding matrices: guild/TL ordering, zero = white, gape/realised glyphs, focus mode, no placeholders (§3) | Critical | pPREY tuning loop; currently no visible signal (pressure) and only about 2 % of the matrix fits on screen. |
| 5 | Responsive shell and empty states (§14) | High | At 1024 or on an iPad the plots get about 300 px or start below the fold. |
| 6 | Multi-run: colour fixed by run, base in ink, difference-to-base view (§7) | High | Colour currently changes meaning between series. |
| 7 | Recruitment: Atlantis vs BH YOY + residuals; S-R with curve and discrete years (§5) | High (after D03/D04) | The recruitment-calibration question cannot be answered visually today. |
| 8 | Ages: age × time heatmaps with ResN/KWRR diverging (§8) | High | Directly serves the thin-adult rule; fixes the 1/10 legend and the invisible yellow. |
| 9 | Predation mortality by predator: top-N or heatmap (§4) | Critical (after D01) | The plot is unreadable, and its data is transposed. |
| 10 | Number/unit formatter app-wide (design_system §2) | Medium, cost S, broad reach | Removes "1.0e-08", "500µ", 21-digit ratios and 17-digit table values in one change. |

---

## Reconciliation with science review (coordinator, 2026-10-25)

Rule: a redesign is approved only when the science review (`scientific_visual_review.md`) and this design review agree. Science comes first, interpretability second, aesthetics third.

| # | Redesign | Science verdict on the current plot | Disagreement | Resolution | Status |
|---|---|---|---|---|---|
| 1 | Spatial maps (§1) | 08, 10 and 11 CORRECT; 09 INCORRECT (D11); 12 INCORRECT (D30, N4) | Design wants a log or quantile ramp by default. Science warns that log hides one dominant box and that density vs total changes which boxes look important. | **Agreed with conditions:** the legend title states the transform ("Biomass (t), log scale"); zero, missing and background are three distinct classes; a linear/log/quantile toggle is kept; density (t km⁻², per box area) is a labelled option, never a silent replacement for totals; the default box is the densest non-boundary box (N12); the full hover shows box, group, time, layer, value and unit. Map 09 waits for the D11 fix; the overlap map waits for D30 (shares, a low-biomass mask, and D as the headline). | **Approved** for the 08/10/11 states; **blocked by science** for 09 and 12 |
| 2 | Compare multiples and B/B0 heatmap (§2) | 02 and 04 CORRECT; 03 INCORRECT (D22); 05 INCORRECT (D31, N1) | Design wants the heatmap as default above 30 groups and a shared log2 y. Science: normalisation must not hide absolute size, and undefined ratios must not be coloured. | **Agreed:** the heatmap is a triage view, with B0 and Bt in the hover and the absolute plot one click away; B0 ≤ 0 or non-finite rows are drawn as hatched grey labelled "undefined"; the log2 axis is labelled; free y stays available. Pools are excluded from the relative view until D31/D19b are fixed, because N1 shows the shape is wrong, not just the units. | **Approved** for age-structured groups; pools **blocked** (D31) |
| 3 | Group colour strategy (design_system §4.1) | n/a | None. Science adds: the guild hue must mean the same thing in every tab, and the same colour must never be reused for a different group in the same view (JEL = SQN today). | Agreed as proposed. | **Approved** |
| 4 | Feeding matrices (§3) | 25 CORRECT; 26 and 27 INCORRECT (D05–D10, N8) | Design proposes guild/TL ordering, zero = white and a log colour scale for the pressure matrix. Science: the pressure values are about 35× realised, so making them more legible spreads a wrong number. | pPREY matrix redesign **approved** (zero = white, guild ordering, focus mode, no placeholders). Pressure matrix: **no visual redesign until D05–D10 are decided**. Meanwhile only relabel it as a "screening index (not Atlantis intake)" and show realised intake (Σ Nums·Growth/E) beside the selected cell. Diagnostics table: report the actual pPREY rather than "0" (N8). | Partly approved |
| 5 | Responsive shell and empty states (§14) | n/a | None | Agreed. Empty states must say why (missing file or boundary box), not show a blank plot. | **Approved** |
| 6 | Multi-run (§7) | 32, 34, 35 CORRECT; 33 INCORRECT (D26, N11) | Design adds a difference-to-base view. Science: the base run is missing today (D26), so a difference-to-base view would compare against the wrong run. | **Agreed after the D26 fix.** The ribbon is labelled "range across runs" (not uncertainty), the mean has a legend entry, and colour is fixed per run across all series. | Approved, **after D26** |
| 7 | Recruitment (§5) | 20 INCORRECT; 21 UNVERIFIED/misleading | None on the target design (Atlantis YOY on top, BH prediction, residuals). | **Blocked by science** (D03, D04, N6, N7). In the S–R plot, recruits must be lagged to the spawning that produced them, SSB = 0 rows dropped, and the x-axis labelled "total stock N" unless SSB is computed with FSPB. | **Blocked** |
| 8 | Ages heatmaps (§8) | 06 and 07 values CORRECT, labels wrong (N2, N3) | **Real disagreement.** Design proposes a diverging ResN/KWRR scale; the science reviewer asked for KWRR/KWSR reference lines. **Both are scientifically wrong:** KWRR and KWSR are the reserve and structural weights *of a recruit*, not a reference for older cohorts. | **Resolved by the coordinator from the Atlantis definitions:** the condition reference is RN / (X_RS · SN), which is 1 when well fed (X_RS = 2.65, `X_RS` per group if set). Diverge around 1 (thin < 1 < fat), and label the axis "condition RN/(X_RS·SN)". Cohort labels use the age range (FTM cohort = 4 years, N2) and numeric legend order (N3). | **Approved with the corrected reference** |
| 9 | Predation mortality by predator (§4) | 29 INCORRECT (D01, N9) | None | **Blocked by science** (D01). After the fix, redesign as top-N predators plus "Other"; regression check FTM → SHW, FVL. | **Blocked** |
| 10 | App-wide number and unit formatter | n/a | None. Science adds: never format a ratio from a zero or non-finite base (show "undefined"); show values in the hover to 4 significant digits *without* switching to e-notation for ordinary magnitudes (S19). | Agreed. | **Approved** |
| 11 | Consumption / Predation diet (§6) | 14 and 15 INCORRECT (D15, D16) | None | **Blocked by science** (D15, D16). Diet shares must become numbers-weighted (D15) before any restyling. | **Blocked** |
| 12 | Overview tiles and health table (§11) | 01 INCORRECT (D21, D22) | Design ranks by severity. Science: the "Extinct" label overstates a screening rule. | **Agreed:** rename the flags to "Collapsed (< 1 % of initial)" and "Increased > 10×", with a note that they are screening rules. Undefined ratios get a separate "undefined" flag. Tiles are fixed by D21. | **Approved** |
| 13 | Food web (§13) | 19 INCORRECT (N5) | None | **Blocked** until trophic levels are solved properly (producers = 1). | **Blocked** |

**Final top redesigns, after reconciliation (science-approved first):**

1. Spatial maps (08/10/11 states).
2. Compare multiples and heatmap (age-structured groups).
3. Group colour strategy.
4. pPREY matrix.
5. Responsive shell and empty states.
6. App-wide formatter.
7. Overview flags.
8. Ages condition heatmap (corrected reference).

Then, after the scientific fixes: multi-run, recruitment, predation mortality, consumption/diet, predation pressure and food web.

