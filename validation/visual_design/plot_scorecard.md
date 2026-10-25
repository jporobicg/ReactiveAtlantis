# Plot scorecard (communication review, NESP screenshots)

## How to read this

- **Scale.** 1 = fails, 2 = poor, 3 = adequate, 4 = good, 5 = exemplary.
- **Scientific validity.** This column is left for reconciliation with `scientific_visual_review.md`. Where `../defects.md` already records a P0 or P1 defect for the numbers shown, the cell also reads **"fails science review — see scientific_visual_review.md"**, and the communication scores describe the design only.
- **Interaction quality.** This is judged from the controls and mode bar visible in the screenshots. Hover and click were not exercised by the capture script, so most hover behaviour is marked "not exercised". Hover content was checked in the code where noted.
- **Accessibility.** This covers text size at 1600 px, contrast, colour-only encoding and fit at 1024×700 (`S/`).
- **No averages**, by design.

IDs match `visual_inventory.md`.

---

### V01 Overview KPI tiles (D/01, S/01)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D21: three of the six tiles show "—" on NESP; D22 inflates "Exploding". |
| Interpretability | 3 | The labels are plain, but "Groups on" is cryptic, and "—" does not say whether a value is missing or zero. |
| Diagnostic usefulness | 3 | "Extinct 2 / Exploding 8" is the right headline, but the tiles do not link to the rows they count. |
| Visual clarity | 3 | Big numbers, clear. The red/gold tiles use status colour without an icon. |
| Aesthetic quality | 3 | Heavy saturated blocks; the decorative icons take half of each tile. |
| Interaction quality | not exercised | No click-through visible. |
| Accessibility | 1 | At 1024 the tile labels break letter by letter ("Ex/tin/ct", "Pr/ob/le/m co/ho/rts", S/01). White on gold `#C4A35A` is about 2.4:1. |

### V03/V04 Group health and Problem cohort tables (D/01)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D22: detritus groups that start at 0 head the list with 8e20 ratios. |
| Interpretability | 3 | Clear columns, but Ratio is printed as a raw 21-digit integer and the flag thresholds are not stated. |
| Diagnostic usefulness | 3 | Sorted by ratio, which is useful. There is no sparkline or link to Compare. Problem cohorts show only WHD rows, with 15 per page. |
| Visual clarity | 3 | Plain DT. The flag text is not visually differentiated (no status chip). |
| Aesthetic quality | 3 | Neutral. |
| Interaction quality | 3 | Sort and search are visible. No row → plot linking. |
| Accessibility | 3 | Readable text. At 1024 the table is pushed below the fold. |

### V05/V06 Compare small multiples, absolute and Bt/B0 (D/02, D/03, S/02, S/03)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D11, D12, D22 affect pool and epibenthic panels. |
| Interpretability | 2 | Facet titles are about 7 px and truncated ("posit Feeders (sea cucumber etc"). Y ticks are tiny. Free y scales put the 0.5–1.5 band at a different height in every panel. |
| Diagnostic usefulness | 2 | Only about 52 of 101 panels are visible; the card has a fixed height and clips the rest (D/03 is cut mid-row). With no ordering by status, collapsing groups are scattered alphabetically. |
| Visual clarity | 2 | 101 identity colours carry no meaning in single-series facets. Some panels go blank (DL is all NaN) with no explanation. |
| Aesthetic quality | 3 | Consistent band shading and light grid. |
| Interaction quality | 2 | Static ggplot (>36 series), so no hover. The capture caught a 15 %-opacity stale plot under "Working…" (D/02). |
| Accessibility | 1 | Labels below 8 px. At 1024 the panels are about 60 px wide and every title is truncated to a few letters (S/03). |

### V07 Compare one group, totals with band (D/04 FTM, D/05 ZL, S/04, S/05)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D31 for pools when the view is unscaled; the scaled view is a ratio. |
| Interpretability | 3 | The y title is a raw expression ("Relative Values (X_t/X_0)"). The title "Total Biomass Over Time" does not name the group. |
| Diagnostic usefulness | 4 | The band against B/B0 = 1 shows immediately that FTM leaves the band in 2023 and ZL sits on its lower edge. |
| Visual clarity | 3 | "Limit axis" clips the band's upper edge in D/05, so the 1.5 line disappears and the band looks open-ended. |
| Aesthetic quality | 4 | Clean single navy line. |
| Interaction quality | not exercised | plotly; hover not captured. |
| Accessibility | 3 | Tick labels about 9 px. It fits at 1024 but is narrow. |

### V08 Age structure, "bars" view (D/06, S/06)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | — |
| Interpretability | 2 | Labelled "bars" but drawn as a stacked area. Brewer *Paired* implies pairs (Age01/Age02 light/dark blue) that do not exist. |
| Diagnostic usefulness | 2 | Ages 5–10 are thin slivers at the bottom, so the old-age collapse that matters for tuna is invisible. Proportion and absolute views are not offered side by side. |
| Visual clarity | 2 | The ordinal age variable is encoded with categorical hues; red/orange slivers cannot be told apart. |
| Aesthetic quality | 3 | Pleasant colours, but the hues jump. |
| Interaction quality | not exercised | — |
| Accessibility | 2 | The validator fails Paired on the lightness band and on Δ15 normal vision (FF7F00↔FDBF6F 14.2). 6 of 10 slots are below 3:1. |

### V09 Ages: Biomass/Numbers/ResN/StructN by age (D/07, S/07)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D18 (weighted-mean caveats). |
| Interpretability | 2 | No units on any facet. "6e+05" notation. The legend is sorted as text (Age 1, Age 10, Age 2…), so age 10 sits next to age 1 and both are dark purple. |
| Diagnostic usefulness | 3 | The four aligned panels share time, and the ResN drop at 2021 is visible across ages. No KWRR/KWSR reference lines. |
| Visual clarity | 3 | The viridis order works. The yellow end is invisible on white (ages 8–9). |
| Aesthetic quality | 3 | Tidy, but the panels are short. |
| Interaction quality | not exercised | — |
| Accessibility | 2 | The yellow lines are about 1.1:1 against the surface. Colour is the only age encoding. |

### V11 Spatial map, biomass (D/08, S/08)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D18 (tonnes per box, "integrated" over layers). |
| Interpretability | 2 | Nearly every box renders at the background colour. The ramp starts at `#F7F4EC`, which is the page sand, so zero, low and the background are the same colour. No basemap or coastline context. |
| Diagnostic usefulness | 2 | Linear scale with the "stable across time" maximum (7,000 t), so at t1 the whole distribution sits in the bottom 5 % of the ramp. You cannot see where the tuna are. |
| Visual clarity | 2 | Low-chroma lavender ramp. Box outlines dominate the fill. |
| Aesthetic quality | 3 | Clean outline map. The mode bar overlaps the title. |
| Interaction quality | 3 | Play, speed, slider, PNG/CSV export and box selection exist. Hover shows only "Box id / value" (code, `analysis_helpers.R:1094`): no group, layer, time or units, and values are formatted to 4 significant figures (S19). |
| Accessibility | 1 | At 1024 the map does not appear in the first viewport; the debug box comes first (S/08). The value is colour-only, with no table alternative on screen. |

### V12 Spatial map, epibenthic MA (D/09, S/09)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | **fails science review — see scientific_visual_review.md** | D11 (P0): about 1e-10 of the true value. |
| Interpretability | 1 | The colour bar reads "500µ", an SI-prefixed tonne that no ecologist will parse. |
| Diagnostic usefulness | 2 | One box stands out; the rest is background-coloured. |
| Visual clarity | 3 | The brown ramp separates better than the blue one. |
| Aesthetic quality | 3 | — |
| Interaction quality | 3 | As V11. |
| Accessibility | 2 | As V11. |

### V13 Spatial map, tracer Temp (D/10, D/11, S/10, S/11)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D18 for integrated layers. |
| Interpretability | 3 | "Temp" has no °C. Surface uses a 12–22 range and sediment 8–22, so the two layers cannot be compared by colour. |
| Diagnostic usefulness | 3 | The cross-shelf gradient is visible. The boundary boxes are the darkest, which draws the eye to boxes that are not dynamic. |
| Visual clarity | 3 | The navy single-hue ramp is fine. The Functional-group dropdown stays visible for a tracer, which is irrelevant. |
| Aesthetic quality | 3 | — |
| Interaction quality | 3 | As V11. The info box says "Layer: water_1" while the control says "Surface": two names for one thing. |
| Accessibility | 2 | As V11. |

### V14 Spatial overlap map (D/12, S/12)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D30 (local index is scale-dependent; zero predator gives 0). |
| Interpretability | 2 | The default layer is Sediment for a tuna × myctophid pair, so the map is entirely zero with no message saying why. |
| Diagnostic usefulness | 2 | Shows only the index, not the two distributions behind it. |
| Visual clarity | 3 | Teal ramp. Zero equals the background, as V11. |
| Aesthetic quality | 3 | — |
| Interaction quality | 3 | As V11. |
| Accessibility | 2 | As V11. |

### V15 Spatial per-box series under the map (D/08–D/12)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D18. |
| Interpretability | 2 | Defaults to "Box 0", a boundary box with flat values. The y range 1.70–1.78 exaggerates nothing into a flat line. |
| Diagnostic usefulness | 1 | Every capture shows the same flat red dotted line, whatever the variable. |
| Visual clarity | 2 | Red dotted line with markers every step. The time cursor is a red dashed vertical line in the same red. |
| Aesthetic quality | 2 | Red is the status/alarm colour used for a neutral series. |
| Interaction quality | not exercised | Linking to the map click was not exercised. |
| Accessibility | 3 | Readable. |

### V17 Spatial by age class, one box (D/13, S/13)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | — |
| Interpretability | 1 | The ResN and StructN panels are blank with no message. Biomass and Numbers are flat 0 for box 0 / sediment. |
| Diagnostic usefulness | 1 | The default state shows nothing. |
| Visual clarity | 2 | Empty grids. |
| Aesthetic quality | 2 | — |
| Interaction quality | 2 | Tracer and Depth controls remain visible although they do not apply. |
| Accessibility | 2 | Yellow viridis lines on white. |

### V18/V19 Consumption, eats / eaten (D/14, D/15, S/14, S/15)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | **fails science review — see scientific_visual_review.md** | D15, D16 (P0): 1e-8 t/day is about 1e-11 of realised intake. |
| Interpretability | 2 | The axis reads "1.0e-08" tonnes, an implausible magnitude shown with no warning. The legend gives codes only and its order differs from the stack order. |
| Diagnostic usefulness | 3 | A stacked area of top-8 prey plus Other is the right idea, but the dominant FPS/FMN band visually swamps the rest. |
| Visual clarity | 2 | Stacked areas with spiky seasonality make the upper layers unreadable. FMN (`#AA4499`) and SQO (`#AA519D`) are near-identical magenta (normal-vision ΔE 2.6). |
| Aesthetic quality | 3 | Smooth and colourful. |
| Interaction quality | not exercised | — |
| Accessibility | 2 | The validator fails this 8-colour set on every hard check. Pale tan FMM and yellow FPS are below 3:1. Codes only. |

### V20 Physics tracer by layer (D/16, S/16)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D18 for the box mean. |
| Interpretability | 3 | Layers are named (Surface…Sediment), but "Temp" has no unit and each facet is about 40 px tall, so 5 tick labels overlap ("23 21 19 17 15"). |
| Diagnostic usefulness | 3 | The seasonal cycle and the attenuation with depth are visible, but free y per layer hides the vertical gradient. A depth × time heatmap would show it at once. |
| Visual clarity | 2 | A rainbow hue per layer encodes an ordered variable with unordered colours. |
| Aesthetic quality | 3 | — |
| Interaction quality | not exercised | — |
| Accessibility | 2 | Overlapping tick text. Pink and yellow lines are low-contrast. |

### V21 Layer thickness nominal_dz (D/16)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | — |
| Interpretability | 3 | Clear axis in m. Sediment shows 0 with no explanation. |
| Diagnostic usefulness | 2 | The deepest layer (3,300 m) flattens the other five to zero height. Log scale, or a depth profile with layer bands, would help. |
| Visual clarity | 3 | Single navy. |
| Aesthetic quality | 3 | — |
| Interaction quality | not exercised | — |
| Accessibility | 3 | — |

### V22 Multi-run empty state (D/17, S/17)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | n/a | — |
| Interpretability | 3 | The message is clear but repeated three times (card, sidebar help, plot title). |
| Diagnostic usefulness | n/a | — |
| Visual clarity | 2 | A 350 px white void with a small bold sentence. At 1024 the sentence is clipped on both sides (S/17). |
| Aesthetic quality | 2 | — |
| Interaction quality | 2 | No "Add run" action in place. |
| Accessibility | 2 | Clipped text at 1024. |

### V23 Multi-run overlay plus ensemble ribbon (M/32–M/35)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D26 (no time or group compatibility check). |
| Interpretability | 3 | Lines plus a min–max ribbon are easy to read. The y axis is "Biomass" with no units. Year is shown as 0–5 here but as calendar years in Compare. |
| Diagnostic usefulness | 3 | Run spread is clear (M/32, M/35). There is no difference-to-baseline view, and the "ensemble mean" announced in the help is not distinguishable. |
| Visual clarity | 2 | **Colour follows rank, not run.** In M/32 c1 is teal and c3 navy; in M/33 (YOY, c3 missing) c1 becomes navy and c2 teal. Teal c1 and blue c5 are close (normal-vision ΔE 12.5, below the 15 floor). SSB (M/34): the lines overlap and the ribbon is invisible. |
| Aesthetic quality | 3 | Restrained colours, thin lines. |
| Interaction quality | 2 | The help text "Add another run … to overlay it here" stays on screen with 5 runs loaded. |
| Accessibility | 2 | Colour-only run identity, with no direct end labels. |

### V24 Predation: diet of group, 100 % stacked (D/18, S/18)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D15 (unweighted cohort mean). |
| Interpretability | 3 | Clear title and y axis. The x axis is "Year 0–5", unlike the calendar dates on other tabs. |
| Diagnostic usefulness | 3 | The diet switch from FPL to FPS after year 1 is visible. Proportions hide absolute intake. |
| Visual clarity | 2 | FMN/SQO magenta clash (as V18). A 100 % area with jagged seasonality creates moiré. |
| Aesthetic quality | 3 | — |
| Interaction quality | 3 | Threshold, Top-N and Scale-to-1 controls are sensible. |
| Accessibility | 2 | Colour-only, code-only legend. |

### V25 Predation: predators of group (D/18)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | Uses DietCheck; check against D01 in the science review. |
| Interpretability | 3 | Two predators read clearly. |
| Diagnostic usefulness | 2 | A 100 % share hides that total predation may be tiny. The large olive SHW block looks alarming regardless of magnitude. |
| Visual clarity | 3 | — |
| Aesthetic quality | 2 | A large olive `#7A7A00`-like fill is visually heavy. |
| Interaction quality | not exercised | — |
| Accessibility | 3 | 2 series, distinct. |

### V26 Diet by age at the selected time (D/18)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | — |
| Interpretability | 3 | Ages are labelled Age00–Age09 here but Age01–Age10 in V08 and V47: an off-by-one between tabs. |
| Diagnostic usefulness | 4 | The ontogenetic diet shift (FPS → FPL) is clear. |
| Visual clarity | 3 | No gap between stacked segments. |
| Aesthetic quality | 3 | — |
| Interaction quality | 3 | The day slider is shared. |
| Accessibility | 2 | Colour-only identity. |

### V27 Food web (D/19, S/19)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | Not reviewed. |
| Interpretability | 2 | The legend says "DownTop/TopDown" (jargon). The focal group FTM is not highlighted. The x position is arbitrary. |
| Diagnostic usefulness | 2 | 13 nodes and many crossing edges. Edge width does not clearly show diet share. |
| Visual clarity | 2 | Tiny white codes on coloured circles (white on yellow FPS is unreadable). The edge colours fail CVD (protan ΔE 5.6). |
| Aesthetic quality | 3 | — |
| Interaction quality | not exercised | — |
| Accessibility | 1 | At 1024 the node labels are about 6 px (S/19). Colour-only edge direction. |

### V29/V30 Recruitment: YOY and larvae, relative YOY (D/20, S/20)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | **fails science review — see scientific_visual_review.md** | D03 (P1). |
| Interpretability | 2 | The legend lists 4 series but only 2 are visible: Larvae is hidden under New YOY, and "Atlantis YOY" navy vs "New larvae" teal are nearly the same colour. Units are absent. |
| Diagnostic usefulness | 2 | The core question (does the BH prediction match Atlantis YOY?) needs the two on one scale with residuals. The tool shows mixed larvae/YOY magnitudes on one axis. |
| Visual clarity | 2 | Markers on every step and step artefacts. The second panel duplicates the first. |
| Aesthetic quality | 3 | — |
| Interaction quality | 3 | Alpha/beta inputs plus Recalculate is a good what-if pattern. |
| Accessibility | 2 | Colour-only series. At 1024 the legend is clipped ("eries … New larvae —", S/20). |

### V31/V32 Recruits vs SSB (D/21, S/21)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | **fails science review — see scientific_visual_review.md** | D04: the "SSB" axis is total biomass. |
| Interpretability | 2 | The year colour bar's labels collide into "20202021202220232024". No BH curve is drawn on the S-R plot. |
| Diagnostic usefulness | 2 | Only about 6 annual points, joined by lines, plus an SSB = 0 artefact at the last step. A stock–recruit plot needs the BH curve and the model points. |
| Visual clarity | 2 | A continuous rust→teal gradient for 5 discrete years. |
| Aesthetic quality | 3 | — |
| Interaction quality | not exercised | — |
| Accessibility | 2 | The path order is colour-only. |

### V33 SSB time series (D/22, S/22)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D04 (label). |
| Interpretability | 3 | No units on "SSB". |
| Diagnostic usefulness | 3 | The annual steps are shown as sloped interpolations, and the drop to 0 just before the end looks like a crash. |
| Visual clarity | 4 | Single series, clean. |
| Aesthetic quality | 4 | — |
| Interaction quality | not exercised | — |
| Accessibility | 4 | Fits at 1024. |

### V34 Growth limitation (D/23, S/23)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D23. |
| Interpretability | 2 | The default is box 0 / layer 0 (boundary, likely padded), so three of the four lines sit hidden on 0 and the one visible line is "Eddy" at 1. |
| Diagnostic usefulness | 1 | The default state tells the user nothing about what limits PL. |
| Visual clarity | 2 | Overlapping lines at 0 hide series, and no jitter or facets are offered by default. |
| Aesthetic quality | 3 | — |
| Interaction quality | 3 | Box, Layer and facet toggles are present. |
| Accessibility | 3 | — |

### V35 Catch empty state (D/24, S/24)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | n/a | D14 (not reachable). |
| Interpretability | 3 | It lists the missing files clearly. |
| Diagnostic usefulness | n/a | — |
| Visual clarity | 2 | Controls and an empty white plot are still rendered under the card. |
| Aesthetic quality | 2 | Lower-case "catch" in the heading. |
| Interaction quality | 2 | The controls look usable but do nothing. |
| Accessibility | 3 | — |

### V36 pPREY matrix (D/25, S/25)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D25. |
| Interpretability | 2 | Each row label repeats "· juvenile / adult prey". Column labels are rotated 60° and truncated. There is no colour bar in view. |
| Diagnostic usefulness | 2 | Only about 18 of about 200 rows and about 25 of about 100 columns fit the viewport. With no ordering or clustering by trophic level or guild, structure is invisible. |
| Visual clarity | 2 | The reversed YlOrRd makes zero pale yellow, barely different from 0.01. Structural zeros should be the surface colour. |
| Aesthetic quality | 2 | A bright yellow field. |
| Interaction quality | 2 | The click handler is unregistered (`plotly_click` warning in `app_errors.txt`, S20), so the "Click to select" text promises an action that may not work. Four empty placeholder panels wait for that click. |
| Accessibility | 1 | At 1024 the matrix starts below the fold, as a blank white block with clipped labels (S/25). |

### V39 Predation-pressure heatmap (D/26, S/26)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | **fails science review — see scientific_visual_review.md** | D05–D10 (P0). |
| Interpretability | 1 | The entire visible field is uniform pale yellow: a linear scale dominated by an off-screen maximum. |
| Diagnostic usefulness | 1 | "986 cells flagged" appears in prose, but no flagged cell is visible in the matrix. |
| Visual clarity | 1 | No signal. |
| Aesthetic quality | 2 | — |
| Interaction quality | 2 | Took 113 s to render (timing.csv). Hover not exercised. |
| Accessibility | 1 | As V36. |

### V40/V41/V42 Pressure methods text, per-box map and table (D/26)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | **fails science review — see scientific_visual_review.md** | D01, D02, D05. |
| Interpretability | 2 | The methods paragraph prints all 147 box IDs inline and overflows the card. The map area shows a red "Spatial map cannot draw" box in its normal (nothing selected) state, which reads as an error. |
| Diagnostic usefulness | 2 | The table starts with FMM→BC rows and is not sorted by the flag. The Consumption column is cut off at the right edge. |
| Visual clarity | 2 | Table rows are about 60 px tall (wasted). |
| Aesthetic quality | 2 | — |
| Interaction quality | 2 | — |
| Accessibility | 2 | Horizontal overflow. |

### V43 Predator preference sums (D/27, S/27)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D08. |
| Interpretability | 1 | About 200 y labels overprint into a solid grey smear (D/27, and worse in S/27). |
| Diagnostic usefulness | 2 | The sorted profile is visible, but no bar can be identified. A single "ok" flag colour means nothing is highlighted. |
| Visual clarity | 1 | Label collision. |
| Aesthetic quality | 2 | — |
| Interaction quality | not exercised | Hover would be the only way to identify a bar. |
| Accessibility | 1 | Unreadable at both sizes. |

### V44 Feeding diagnostics table (D/27)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | — |
| Interpretability | 2 | Values such as `1.866498753846154e-35` and `0.00001703539415384615` are full doubles. The "Value" column has no meaning or unit. |
| Diagnostic usefulness | 2 | 9,604 rows, 10 per page, unsorted by severity. Rows with value 0 or 1e-35 are noise. |
| Visual clarity | 3 | — |
| Aesthetic quality | 3 | — |
| Interaction quality | 3 | Search and pagination. |
| Accessibility | 3 | — |

### V47 Mortality by age, F/M1/M2 (D/28, S/28)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | D27 (S0 only). |
| Interpretability | 2 | The legend shows three sources but only predation is visible; zeros are indistinguishable from "absent". "Mortality" has no unit or rate basis. |
| Diagnostic usefulness | 2 | Free y per age (0–600 for Age01, 0–20 for the others) hides the age gradient that is the point. Five annual values are drawn as smooth areas. |
| Visual clarity | 3 | Small multiples are tidy. |
| Aesthetic quality | 3 | — |
| Interaction quality | not exercised | — |
| Accessibility | 3 | Navy, gold and rust are CVD-separable (validator PASS on CVD), but the gold is 2.4:1. |

### V48 Predation mortality by predator (D/29, S/29)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | **fails science review — see scientific_visual_review.md** | D01 (P0): transposed. |
| Interpretability | 1 | The plot collapsed: 15 age facets are crushed into two columns of overprinted labels and ticks, and the legend lists about 100 groups, including non-predators. |
| Diagnostic usefulness | 1 | Nothing can be read. |
| Visual clarity | 1 | — |
| Aesthetic quality | 1 | — |
| Interaction quality | not exercised | — |
| Accessibility | 1 | — |

### V49 Mort.txt F vs M (D/30, S/30)
| Dimension | Score | Justification |
|---|---|---|
| Scientific validity | see science review | — |
| Interpretability | 3 | The subtitle "Relative only…" is good. The axis "3e-06" is hard to read. |
| Diagnostic usefulness | 3 | Shows F = 0 and an M peak in year 2. |
| Visual clarity | 4 | Two series, distinct hues. |
| Aesthetic quality | 3 | — |
| Interaction quality | not exercised | — |
| Accessibility | 3 | Gold is 2.4:1. |
