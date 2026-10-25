# ReactiveAtlantis design system (proposal)

## Scope and status

This is a proposed unified visual language for every plot, map, matrix, table and card in the app. It is a specification only: no app code was changed.

- **Method.** Colour rules follow the dataviz-skill method: pick the form, assign colour by its job, then validate.
- **Validation.** Every palette below was run through `validate_palette.js`; the outputs are quoted.
- **Approval.** Any change that alters what a figure *shows* (scales, defaults, aggregation) must be shown to the owner with concrete examples before it is implemented. That follows the "discuss before implementing" rule.

---

## 1. Principles (in priority order)

1. **Never hide an ecological signal.**
   - No default may produce an empty or flat plot. This rule exists because Box 0, Layer 0 and the Sediment layer for pelagic fish produced empty defaults.
   - Zero, missing and "not applicable" must look different.
   - A linear scale that sends 95 % of the data to the bottom 5 % of the ramp is a bug, not a style.
2. **Every number carries its unit and its basis.**
   - Units: t, t d⁻¹, mg N m⁻³, °C, d⁻¹, "fraction of start-of-period stock".
   - Basis: the aggregation (sum over boxes, volume-weighted mean, cohort-weighted mean) and the source file.
3. **Colour follows the entity, never its rank or its position in a filtered list.**
4. **Reference lines and bands are first-class marks**: B/B0 = 1, the 0.5–1.5 band, KWRR/KWSR, the BH curve, and observations.
5. **Communicate the screening rule, not just the flag.** Write "Exploding (>10× initial; screening rule)", not "Exploding".
6. **One time axis convention app-wide.** Use calendar dates when the NetCDF origin is known. Otherwise use "Model year 0–5" everywhere, never a mix (the app mixes them today: Compare vs Predation, Mortality and Multi-run).

---

## 2. Typography hierarchy

The app's system-sans stack is kept (theme.R). Sizes are CSS px at 1600 px width. ggplot sizes are in pt and must be scaled so that they render at the same px. Today ggplot facet strips render at about 7 px, which is the main legibility failure.

| Role | Size / weight | Colour | Notes |
|---|---|---|---|
| Card / plot title | 16 px, 600 | ink `#1B2833` | Must name the group code **and** the long name: "FTM · Southern bluefin tuna — total biomass". Navy titles become ink; navy is reserved for selection. |
| Subtitle (basis line) | 13 px, 400 | secondary `#52514e` | States units, aggregation and source, e.g. "t · sum over boxes and layers · output.nc". |
| Axis title | 13 px, 500 | ink | Always "Quantity (unit)". Never raw code such as `X_t/X_0`. |
| Tick labels | 12 px, 400, tabular numerals | muted `#5C6B73` (5.6:1 on white) | Minimum 11 px at 1024. |
| Facet strip | 12 px, 600 | ink | Truncate with an ellipsis plus a full-name tooltip. Never let the label clip silently. |
| Legend text | 12 px | ink | Code + short name ("FPS sardine"). |
| Direct labels | 12 px, 500 | ink, with a 2 px surface halo | Text never wears the series colour. |
| Table body | 13 px, tabular-nums | ink | — |
| Footnote / provenance | 11 px | muted | File name, time step, flagged-cell count. |

**Number formatting**, one helper for every axis, tooltip and table:

- 3 significant figures with thin-space thousands: `12 400 t`.
- Scientific notation only outside 1e-3…1e6, written as `3.2 × 10⁻⁶`. Never `3e-06`, and never SI prefixes on tonnes (`500µ`).
- Ratios that exceed 1e6 are shown as "> 10⁶ (initial = 0)" rather than as 21 raw digits.

---

## 3. Surfaces, grids and axes

| Token | Light | Dark | Rule |
|---|---|---|---|
| Page plane | `#F7F4EC` (current sand) | `#0d0d0d` | — |
| Chart surface | `#FFFFFF` | `#1a1a19` | Plots sit on white cards, as they do today. **The map ramp must not start at the page sand.** |
| Gridline | `#E6E2D6` hairline 0.35 | `#2c2c2a` | Major y only. No x grid on time series except at year boundaries. |
| Axis / baseline | `#C3C2B7` | `#383835` | Draw a zero baseline when 0 is in range. |
| Reference line | `#1B2833` dashed 1 px | `#f0efec` | Used for B/B0 = 1, YOY/YOY0 = 1 and the BH curve. |
| Reference band | `#1D7874` at 10 % alpha, edges dashed | same, 18 % | The 0.5–1.5 band. **Edges stay visible when "Limit axis" clips the range**, with an arrow label "band continues to 1.5". |
| Selection | navy `#0B3C5D`, 2.5 px outline | `#f0efec` | Selected box, cell, node or row. This is the only use of navy in the data area. |
| Hover | 1 px ink outline plus a lighten wash | — | — |

---

## 4. Colour by job

### 4.1 Functional-group identity (about 100 groups): the current palette fails

`build_fg_palette()` was run on the real NESP groups CSV (`/tmp/radv/pal.R`, using the app's own code). Results:

| Check | Result |
|---|---|
| Unique colours | **101 for 102 groups**: JEL and SQN share `#0080A4` (ΔE 0). |
| Pairs with Lab ΔE76 < 10 (indistinguishable to most viewers) | 59 |
| First 12 groups, adjacent (validator) | FAIL lightness band (black `#000000`, `#FFFF99` at L 0.98), FAIL chroma floor. Contrast WARN: `#FFFF99` is 1.05:1 on white. |
| FTM diet top-8 exactly as drawn in D/14 and D/18, all pairs | **FAIL on all hard checks**: FMN `#AA4499` vs SQO `#AA519D` normal-vision ΔE **2.6**; SQN `#0080A4` vs FPL `#4E79A7` deutan ΔE **0.6**. |
| Full 102, all pairs | FAIL (duplicate pair ΔE 0). |

**Conclusion.** No set of 100 hues can be told apart, and a farthest-colour walk with no CVD model will keep producing near-duplicates inside the top-8 sets that actually appear together. The proposal below uses three tiers.

**Tier A: guild hue (always available, validated).** A hue says *what kind of organism* this is. This is the "strategy" encoding for when colour alone fails.

| Guild (from GroupType) | Hue | Light | Dark |
|---|---|---|---|
| Fish, sharks, rays | blue | `#2a78d6` | `#3987e5` |
| Mammals, birds, reptiles | yellow | `#eda100` | `#c98500` |
| Invertebrates (incl. zooplankton, cephalopods, benthos) | magenta | `#e87ba4` | `#d55181` |
| Primary producers (phyto, macroalgae, seagrass, MPB) | green | `#008300` | `#008300` |
| Detritus, bacteria, carrion, "Other" | neutral grey `#898781` + **45° hatch** | same | — |

The validator was run with `--pairs all` on white:

- The four hues pass every check: CVD worst ΔE 13.0 (protan, magenta↔blue) and normal-vision worst ΔE 19.6.
- Yellow (2.17:1) and magenta (2.69:1) are below 3:1, so the relief rule applies: direct labels or a table.
- Grey vs magenta is protan ΔE 5.4. That is why grey must always carry the hatch texture and an "Other/detritus" label.

**Tier B: series identity inside one view (≤ 8 series, validated).** When a view shows several groups at once (diet, consumers, predators, multi-run):

- Use the reference categorical order: `#2a78d6, #eb6834, #1baf7a, #eda100, #e87ba4, #008300, #6250d6, #e34948`. The validator passes all checks on white and on sand: worst adjacent CVD ΔE 9.1, normal-vision 19.6. Dark steps pass 8.4 / 19.3.
- Slots are assigned by the **stable groups-CSV order of the set**, not by rank. Changing Top-N from 8 to 6 then drops series without repainting the survivors.
- Everything past slot 8 folds into **Other** (grey + hatch).
- The legend and direct labels carry `code + short name`.
- Stacked areas get a 1 px white gap between bands.
- Under all-pairs use (scatter, choropleth), only the first 3 slots validate (CVD 9.2 / normal 24.0). Use more and you must facet.

**Tier C: a focal group against context (101 groups at once).** In small multiples, the food web, the preference-sum bars and the network:

- Panels are single-series, so the **line is ink**. The guild appears as a 4 px coloured strip on the facet title.
- The focal or selected group gets the selection outline; all other marks are `#C3C2B7`.
- The 101-entry group legend is removed (V48 shows how such a legend crowds out the plot).

A pre-computed, validated, fixed 101-colour "group palette" is therefore **not** recommended. If one is kept for continuity, it must at least be de-duplicated and pass a ΔE ≥ 15 check within every top-8 set produced by the app's own data.

### 4.2 Run identity (Multi-run)

- Run 1 (the base) is always ink `#1B2833`, 2.5 px.
- Other runs take Tier B slots 1… in **run order** (c1 → slot 1 always), whatever series is selected. Today the colours shift between M/32 and M/33.
- More than 8 runs: show the ensemble ribbon plus the median, with individual runs in grey, and highlight on hover.
- Every line gets a direct end label (`c4`).

### 4.3 Ordered categories (age class, depth layer)

- **Age, at most 5 shown:** blue ramp steps `#86b6ef, #3987e5, #256abf, #184f95, #0d366b`. Validator `--ordinal`: PASS (light end 2.11:1).
- **Age, 10 classes:** a 10-step single-hue ramp **fails** the adjacent-ΔL check (0.047 < 0.06), so 10 ages cannot be told apart by colour. Use one of:
  - an age × time heatmap (sequential), or
  - small multiples by age, or
  - 5 colours for age pairs with the selected age highlighted.
- Legends are sorted numerically (1, 2, … 10), never as text (1, 10, 2).
- **Depth layers:** the same idea. Surface is lightest and deepest is darkest, or better a layer × time heatmap. The current rainbow hues (V20) are replaced. Sediment gets a distinct brown-grey with hatch because it is not a water layer.
- **One age-labelling convention app-wide:** "Age class 1…n", 1-based (V26 uses Age00–09).

### 4.4 Sequential (magnitude: maps, matrices)

- One hue, light → dark.
- Default: blue 150 → 700 (`#b7d3f6 … #0d366b`). The second concurrent context takes orange, e.g. a predator map beside a prey map.
- The ramp starts **above** the surface colour: step 150 is about 1.4:1 on white.
- **Zero** is drawn as white `#FFFFFF` with the normal box outline, a separate legend key "0".
- **Missing / not applicable** (land, padded layer, sediment for pelagic groups) is drawn as `#E1E0D9` with a 45° hatch and the legend key "no data".
- Today NA `#C5C0B2` sits between zero and low values, and zero equals the page background. Both are fixed by the rules above.
- **Scale transform** is chosen per variable:
  - biomass and density default to **log10** (or quantile bins with ≤ 7 classes);
  - tracers stay linear;
  - the transform is printed in the legend title, e.g. "Biomass (t, log scale)".
- **Per-step vs stable scale.** A "stable across time" scale must use a robust maximum (the 98th percentile) and show the clipped values with a "≥" legend cap. The toggle label states which one is active.
- Density (t km⁻²) should be the default map quantity, with total per box as an option. Large boxes otherwise dominate (science reviewer to confirm).

### 4.5 Diverging (polarity around a meaningful reference)

- Poles: blue `#2a78d6` ↔ red `#e34948`. The midpoint is neutral grey `#f0efec`, never a hue.
- **B/B0, YOY/YOY0, model/observed:**
  - log2 scale centred at 1, symmetric (0.25, 0.5, 1, 2, 4);
  - below 1 = red arm, above 1 = blue arm;
  - beyond the 0.5–1.5 screening band the steps saturate and the legend marks the band;
  - used for the Compare status heatmap and for the B/B0 maps.
- **pPREY edits (Δ):** keep the existing diverging red–sand–teal idea (`feeding_module.R:1974`), but swap to the validated blue/red poles and the grey midpoint.
- **Pressure:** expected minus realised, log ratio centred at 0 (once the science review signs off on D05).

### 4.6 Status (screening flags), reserved and never used for series

| Status | Colour | Always with |
|---|---|---|
| OK / in band | `#0ca30c` | ✓ + "OK" |
| Watch (0.5–1.5 edge, drift) | `#fab219` | ▲ + "Watch" |
| Outside band | `#ec835a` | ◆ + "Outside 0.5–1.5" |
| Collapse / explode | `#d03b3b` | ✕ + "Extinct < 1 %" or "Exploding > 10×" |
| Not evaluable (initial = 0) | `#898781` | ○ + "n/a (initial 0)" |

- KPI tiles stop being solid status-coloured blocks. Use a white tile with a status chip; the current white-on-gold is 2.4:1.
- Red is never used for a neutral series. Today the spatial series line, the time cursor and the "Spatial map cannot draw" placeholder all use red.

---

## 5. Legends and annotation

- Two or more series: a legend is always present. Four or fewer: also direct-labelled at the line end or band centre. Stacked areas order the legend **in stack order**, top band first.
- Legend position: right of the plot at ≥ 1200 px, below at < 1200 px. Legends never exceed 2 rows; overflow goes to "Other (n groups)", expandable in the tooltip.
- Mandatory annotations:
  - the screening band label ("0.5–1.5 × initial, screening band");
  - the time cursor (ink dashed, labelled "t = 2022-03-01");
  - "Initial = 0, ratio undefined" badges on panels with no data;
  - a "Values ≤ 0 not shown on log scale" footnote.
- Provenance footnote on every plot: source file(s), stock, aggregation and time step.
- Units are never only in the help text.

---

## 6. Tooltips (hover content spec)

The same template is used for plotly, ggplotly and DT row hovers. Values use the §2 formatter; every line ends with its unit.

| Plot | Tooltip lines |
|---|---|
| Spatial map | **Box 94** (boundary? yes/no) · Group FTM · Southern bluefin tuna · Layer: Surface (0–25 m) or "Integrated (water column)" · Time 2022-03-01 (step 31/62) · **Value 12.4 t** (density 0.031 t km⁻²) · Rank 12/147 · Area 402 km² |
| Time series | Date · series name · value + unit · Δ vs t0 (%) · for band views, in/out of band |
| Stacked area | Date · hovered band (code + name) · value + unit · share of total (%) · total |
| Matrix cell (pPREY / pressure) | Predator code + name + stage · Prey code + name + stage · pPREY value · realised DietCheck share · gape OK? · flag reason |
| Network node | Group · TL · biomass · top 3 prey and predators with shares |
| Recruitment | Year · SSB (unit, definition) · recruits (unit) · BH prediction · residual (log) |

Tooltips appear within 100 ms, follow the cursor with an 8 px offset, never cover the hovered mark, and stay at 13 px on a white card with a hairline ring.

---

## 7. Selection and linking

- **Selected entity:** the navy 2.5 px outline on the map box, matrix cell, node or bar, plus the row highlighted in linked tables.
- **Linked views:** a map click sets the box for the series below, and a matrix click sets the predator and prey dropdowns. These clicks must be registered (S20 warns that `plotly_click` is not registered), and the placeholder text must not promise a click that does not work.
- **Defaults:**
  - spatial series = the box with the maximum value at t (never Box 0);
  - growth = the box and layer with the largest producer biomass;
  - overlap layer = the layer where the predator occurs;
  - Feeding = the focal group carried over from the other tabs (one app-wide "focal group" state).

---

## 8. Panel layouts and spacing

- **Spacing:** an 8 px grid, with 16 px card padding, 24 px between cards and 12 px between facets.
- **Height:** each tool body is a two-column grid (controls 260 px, plot fluid). Plot heights adapt to content:
  - small multiples are 120 px per row, with the card growing and no clipping (V05 is clipped at about 52/101 panels today);
  - a facet is never below 90 px;
  - heatmaps size to the rows, with sticky axis labels.
- **Small multiples (Compare):**
  - sort by status (worst first) or by guild, then by name, with a "sort by" control;
  - 6 columns at ≥ 1400 px, 4 at ≥ 1000 px, 2 below;
  - a shared y on B/B0 views (log2) so that the band sits at the same height in every panel.
- **Placeholders:** one inline hint replaces four empty 300 px plot cards (D/25, D/26). Detail panels appear only after a selection.
- **Debug and info boxes** (BGM path, parser, vertex count) move into a collapsed "Diagnostics" disclosure (V10).
- **Long text:** method paragraphs go in a collapsible "How this is computed" with citations. Never inline lists of 147 box IDs.

---

## 9. Responsive behaviour (1024×700 is a target, not an afterthought)

| Width | Behaviour |
|---|---|
| ≥ 1400 | Run sidebar (300 px) + tool controls (260 px) + plot. |
| 1024–1399 | **The run sidebar auto-collapses** to an icon rail once a run is loaded; tool controls become a 2-row toolbar above the plot. The plot then gets about 960 px instead of about 300 px (S/*). |
| < 1024 | Single column. Controls go in a drawer and the plot comes first. |

- Navbar: tabs grouped into 4 menus (Overview · Biomass & ages · Space & physics · Trophic · Demography: recruitment, growth, mortality, catch · Runs) so that it stays on one row at 1024.
- KPI tiles: a minimum 160 px width that wraps to 3 per row. Text never breaks mid-word (S/01).
- First-viewport rule: at 1024×700 the primary plot of every tab must start above the fold. Today it starts below the fold on Spatial, Feeding and Multi-run.

---

## 10. Export styles

| Export | Spec |
|---|---|
| PNG (Map PNG, plotly camera) | 2× scale, white background, title + subtitle (units, source) + provenance footnote burned in, 12 pt minimum text |
| Publication PDF/SVG (ggplot) | `theme_reactiveatlantis(base_size = 9)`, 180 mm width, Tier-B palette or grayscale + texture when "print" is chosen |
| CSV (Series CSV, tables) | Columns `group, box, layer, time_iso, model_day, value, unit, source_file, aggregation` and a header comment with the app version and run path |
| Accessibility | "Show table" toggle on every plot. The texture fills (45°/135° hatches) are enabled by a "Colour-blind / print" setting and by `forced-colors`. |

---

## 11. Dark mode

- The dark steps in §4 are validated: the reference 8 pass all checks on `#1a1a19`.
- The sequential ramp inverts so that dark = low and light = high. Map zero = `#1a1a19` with outline; missing = `#2c2c2a` + hatch.
- Today the toggle exists (A1) but the plot surfaces stay white. Charts need their own dark tokens; an automatic flip is not enough.

---

## 12. Validator log (reproducible)

Commands were run with `node <dataviz>/scripts/validate_palette.js … --surface "#ffffff"`. Scripts are in `/tmp/radv/` (temporary).

| Palette | Result |
|---|---|
| App group palette, NESP first 12 (adjacent) | FAIL (lightness, chroma), contrast WARN |
| App FTM diet top-8 as drawn (all pairs) | FAIL (CVD ΔE 0.6, normal 2.6) |
| App full 102 (all pairs) | FAIL (duplicate) |
| App run colours (multi-run) | FAIL (normal-vision 12.5 c1↔c5, lightness, chroma) |
| App mortality F/M1/M2 | CVD PASS, lightness/chroma FAIL, gold 2.4:1 |
| App Brewer Paired (age bars) | FAIL (normal 14.2, 6 slots < 3:1) |
| App food-web edges teal/salmon | FAIL CVD (protan 5.6) |
| Proposed Tier B, 8 slots, light, white and sand | PASS (contrast WARN → labels) |
| Proposed Tier B, dark | PASS |
| Proposed Tier B first 3, all pairs | PASS |
| Proposed guild hues blue/yellow/magenta/green, all pairs | PASS (contrast WARN → labels) |
| Guild + grey Other | grey↔magenta protan 5.4 → hatch required |
| Ordinal blue 5-step | PASS |
| Ordinal blue 10-step | FAIL adjacent ΔL → do not encode 10 ages by colour alone |
