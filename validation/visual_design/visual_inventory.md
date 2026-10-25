# Visual inventory (rendered, NESP run)

Replaces the static pre-screenshot draft. Every element below was seen in the screenshots of the app running on a real Atlantis run: NESP v8d_t12_5y, c3, with 147 boxes, 7 z layers, 101 groups and 5 years.

- **Screenshot folders.** `D/` = `screenshots/before/desktop_1600x3200/`, `S/` = `screenshots/before/small_1024/` (1024×700 viewport), `M/` = `screenshots/before/multirun/` (five runs c1–c5).
- **Library.** The library is inferred from what is rendered (plotly mode bar, ggplot facet strips, DataTables chrome) and from `inst/shiny/modules`. "ggplot2→plotly" means a ggplot passed through `ra_ggplotly()`, which falls back to a static ggplot when there are more than 36 series.
- **Science flag.** This repeats `../defects.md`. "Fails science review" means a P0/P1 defect affects the numbers shown, so the communication score does not matter until it is fixed.

## Application shell (on every screenshot)

| ID | Element | Type | Library | Notes seen |
|---|---|---|---|---|
| A1 | Top navbar with 14 tabs and a theme toggle | navigation | bslib | At 1024 px the tabs wrap onto 2 rows and the bar is 110 px tall (S/*). The theme-toggle icon is near-invisible: a dark sun on navy. |
| A2 | Run sidebar: run cards, folder paths, buttons, file-role checklist, tool checklist | form + status list | bslib/Shiny | About 340 px wide at 1024. With 5 runs it is about 2,000 px tall (M/31). Paths are truncated with no tooltip. |
| A3 | Per-tool control sidebar (collapsible "<") | form | bslib | A second sidebar. At 1024 the plot gets about 300 px of width (S/02–S/30). |
| A4 | "Working…" busy badge plus grey wash over a stale plot | busy indicator | Shiny | D/02, S/02. The stale plot stays visible at about 15 % opacity. |
| A5 | "X needs more files" card | empty-state card | bslib | D/24: the heading is lower-case ("catch needs more files"). It sits above a still-active control panel and a blank plot. |
| A6 | Scientific Guide drawer | drawer | bslib | Not captured in these screenshots (not exercised). |

## Overview

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V01 | Six KPI tiles: Groups on, Boxes, Layers, Extinct, Exploding, Problem cohorts | D/01, S/01, M/31 | stat tiles with icons | bslib value_box | D21: Boxes, Layers and Problem cohorts show "—" |
| V02 | Run window card: start date and time span | D/01 | text card | bslib | D21: "not in NetCDF" / "unknown" |
| V03 | Group health table: Code, LongName, GroupType, Initial, Final, Ratio, Flag | D/01 | data table | DT | D22: DR and DL are "Exploding" with ratios of 8e20 |
| V04 | Problem cohorts table (StructN/ResN relative to t0) | D/01 | data table | DT | D18 |

## Compare

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V05 | Biomass small multiples, 4 columns × 101 groups, free y | D/02 (mid-recalc), D/03, S/02, S/03 | faceted line chart | ggplot2 (static; >36 series) | D11 (epibenthic), D12, D22 (pools) |
| V06 | Same, Bt/B0 with the 0.5–1.5 band shaded | D/03, S/03 | faceted line chart plus reference band | ggplot2 | as V05 |
| V07 | One group, total biomass, scaled, with band | D/04 (FTM), D/05 (ZL pool), S/04, S/05 | line chart plus band | ggplot2→plotly | D31 for pools when unscaled |
| V08 | Age structure ("bars" view, rendered as stacked area) | D/06, S/06 | stacked area chart | ggplot2→plotly | — |

## Ages

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V09 | Age classes: Biomass, Numbers, ResN, StructN stacked facets, one line per age | D/07, S/07 | multi-panel line chart | ggplot2→plotly | D18 (unweighted means of empty cohorts) |

## Spatial

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V10 | Diagnostic text box: BGM path, parser, coordinate range, finite values, group, layer | D/08–D/12, S/08–S/12 | bullet list | Shiny HTML | — (debug info shown to users) |
| V11 | Box choropleth, biomass (blue ramp) | D/08 (FTM), S/08 | choropleth map | plotly (one fill trace per box) | D18 (tonnes per box, integrated layer) |
| V12 | Box choropleth, epibenthic biomass (brown ramp) | D/09 (MA), S/09 | choropleth map | plotly | **D11 (P0)**: values about 1e-10 of the true value |
| V13 | Box choropleth, tracer (navy ramp), surface and sediment | D/10, D/11, S/10, S/11 | choropleth map | plotly | D18 |
| V14 | Overlap choropleth, 2·min/(a+b) (teal ramp) | D/12, S/12 | choropleth map | plotly | D30 |
| V15 | Per-box time series under the map ("Box 0") | D/08–D/12 | line plus markers | ggplot2→plotly | D18 |
| V16 | Play/Speed animation, time slider, stable-scale and boundary toggles, Map PNG and Series CSV buttons | D/08 | controls | Shiny | — |
| V17 | By-age-class view for one box: 4 facets | D/13, S/13 | multi-panel line chart | ggplot2→plotly | — (empty panels) |

## Consumption

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V18 | "What this group eats", tonnes, top-N stacked | D/14, S/14 | stacked area chart | ggplot2→plotly | **D15, D16 (P0)**: about 1e-11 of realised intake |
| V19 | "Who eats this group", tonnes, stacked | D/15, S/15 | stacked area chart | ggplot2→plotly | **D15, D16 (P0)** |

## Physics

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V20 | Tracer by layer for one box, 7 stacked facets | D/16, S/16 | multi-panel line chart | ggplot2→plotly | D18 when "Mean across boxes" is on |
| V21 | nominal_dz layer thickness for one box | D/16 | bar chart | ggplot2→plotly | — |

## Multi-run

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V22 | Empty state: info card plus blank plot with a title-only message | D/17, S/17 | empty state | bslib + ggplot | — |
| V23 | Run overlay with min–max ensemble ribbon: Biomass, YOY, SSB | M/32–M/35 | line chart plus ribbon | ggplot2→plotly | D26 (no compatibility check; run 1 catch) |

## Predation (DietCheck)

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V24 | Diet of group, 100 % stacked over time | D/18, S/18 | 100 % stacked area chart | ggplot2→plotly | D15 (unweighted cohort mean) |
| V25 | Predators of group, 100 % stacked | D/18 | 100 % stacked area chart | ggplot2→plotly | D15 |
| V26 | Diet by age at the selected time | D/18 | 100 % stacked bar chart | ggplot2→plotly | — |

## Food web

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V27 | Trophic network: y = trophic level, labelled circles, edges coloured "DownTop/TopDown" | D/19, S/19 | node-link diagram | ggplot2→plotly | not reviewed |
| V28 | Trophic levels table | D/19 | data table | DT | — |

## Recruitment

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V29 | YOY and larvae: Atlantis YOY, Larvae, New larvae, New YOY | D/20, S/20 | line plus markers | ggplot2→plotly | **D03 (P1)** |
| V30 | Relative YOY (YOY/YOY0) with a dashed 1.0 line | D/20 | line chart | ggplot2→plotly | D03 |
| V31 | Recruits and SSB time series, two facets | D/21, S/21 | multi-panel line chart | ggplot2→plotly | **D04**: the "SSB" is total biomass |
| V32 | Recruits vs spawning stock: path coloured by year | D/21 | connected scatter | ggplot2→plotly | **D04** |
| V33 | SSB time series | D/22, S/22 | line chart | ggplot2→plotly | D04 |

## Growth

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V34 | Primary-producer limitation factors (Eddy, growth, light, nutrient) | D/23, S/23 | multi-line chart | ggplot2→plotly | D23 |

## Catch

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V35 | Catch empty state (NESP has no catch files) | D/24, S/24 | empty state | bslib + ggplot | D14 (not reachable here) |

## Feeding

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V36 | pPREY matrix, about 200 predator-stage rows × about 100 prey columns, YlOrRd | D/25, S/25 | heatmap | plotly (fixed px size, scrolls) | D25 |
| V37 | Four "Select a predator and prey" placeholder panels: pair detail, predator row, prey column | D/25, D/26 | empty plot placeholders | ggplot2/plotly | — |
| V38 | Note table ("Hover a cell…") | D/25 | data table | DT | — |
| V39 | Predation-pressure heatmap | D/26, S/26 | heatmap | plotly | **D05–D10 (P0)** |
| V40 | Methods paragraph, including a raw list of 147 box IDs | D/26 | text | Shiny HTML | — |
| V41 | Pressure-per-box map ("Spatial map cannot draw" error) | D/26 | choropleth (error state) | plotly | **D05 (P0)** |
| V42 | Realised/expected table (Predator_row, Pred_code, …, Consumption) | D/26 | data table | DT | **D01, D02, D05** |
| V43 | Predator preference sums (about 200 bars, sorted) | D/27, S/27 | horizontal bar chart | ggplot2→plotly | D08 |
| V44 | Diagnostics table (9,604 rows) | D/27 | data table | DT | — |
| V45 | pPREY edit-delta heatmap, diverging red–sand–teal (in code, `feeding_module.R:1974`) | not captured | diverging heatmap | ggplot2 | — |

## Mortality

| ID | Element | Screenshot | Type | Library | Science |
|---|---|---|---|---|---|
| V46 | File-explainer list: SpecificMort, SpecificPredMort, Mort.txt | D/28–D/30 | bulleted cards | Shiny HTML | — |
| V47 | Mortality by age, stacked F/M1/M2, 10 facets | D/28, S/28 | faceted stacked area chart | ggplot2→plotly | **D27**, S0 only |
| V48 | Predation mortality by predator, stacked, faceted by age, legend of about 100 groups | D/29, S/29 | faceted stacked area chart (collapsed) | ggplot2→plotly | **D01 (P0)**: transposed |
| V49 | Mort.txt relative F vs M | D/30, S/30 | line plus markers | ggplot2→plotly | — |

## Colour sources (`inst/shiny/modules/colors.R`, `theme.R`)

| Use | Palette | Where seen |
|---|---|---|
| Functional groups | `build_fg_palette()`: farthest-colour walk over a 62-colour pool (Okabe-Ito, Tol, Tableau, Brewer Dark2/Paired/Set2), with a GroupType preference | V05, V06, V18, V19, V24–V27, V43, V48 |
| Age classes | viridis (`age_class_palette`) in V09 and V17; **Brewer Paired** in V08 | V08, V09, V17 |
| Mortality source | navy / sand-gold / rust | V47 |
| Runs | navy / teal / gold / rust / blue (catch palette) | V23 |
| Physics layers | ggplot default hue (rainbow) | V20 |
| Maps | linear RGB lerp from `#F7F4EC` (equal to the page background) to a navy, brown, teal or rust "high"; NA = `#C5C0B2` | V11–V14, V41 |
| pPREY heatmap | plotly YlOrRd, reversed (zero = pale yellow) | V36, V39 |
| Food-web edges | teal `#1D7874` vs salmon `#C46D5E` | V27 |
| Recruits vs SSB path | continuous year gradient, rust → teal | V32 |
