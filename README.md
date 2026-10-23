# ReactiveAtlantis <img src="man/figures/logo.png" align="right" alt="ReactiveAtlantis logo" width="120" />

Calibration tools for Atlantis ecosystem models, in one Shiny app.

Load a run once (a configuration folder plus an output folder, one mixed
folder, or individual files). Every tool then reads from that shared store.
Calibration maths are unchanged from the original functions; this package
changes how you load data and how results are shown.

![App shell](man/figures/overview.png)

## Install

```r
install.packages("devtools")
devtools::install_github("jporobicg/ReactiveAtlantis", dependencies = TRUE)
library(ReactiveAtlantis)
```

## How to use the app

```r
library(ReactiveAtlantis)

## Configuration + output folders — preferred for large NetCDF / DietCheck files
launch_reactiveatlantis(
  config_dir = "/path/to/Atlantis/config",
  output_dir = "/path/to/Atlantis/output"
)

## One mixed folder still works
launch_reactiveatlantis(project_dir = "/path/to/Atlantis/output")

## Or open the app and paste the folder paths in the Run sidebar
launch_reactiveatlantis()
```

1. Point the **Run** sidebar at a configuration folder (groups CSV, `.prm`,
   init NetCDF, BGM) and an output folder (`*BiomIndx.txt`, `*DietCheck.txt`,
   `*CATCH.nc`, `*PROD.nc`). Leave one blank to use the other as a single
   folder. Files are detected by suffix and header (`pPREY` vs `toutinc` in
   `.prm`). A file found only in the other folder is kept and marked
   “loaded (other folder)”. `outputPROD.nc` is never treated as the biology
   output NetCDF.
2. Click **Load run**. The checklist shows which files are present and which
   tools are enabled. A tool whose files are missing stays on a short “needs”
   card instead of a blank plot. A **Working…** chip and plot/table spinners
   show when the app is busy.

![Two-run loader](man/figures/pair_loader.png)

![Busy indicators](man/figures/busy_indicators.png)
3. Optional: click **Add another run** for the Multi-run tab (each extra run
   is also a config + output pair), and attach an observations CSV
   (`Code, Year, Value, Lower, Upper, Type` where Type is `Biomass`, `SSB`
   or `Recruitment`).
4. Start on **Overview**. Summary cards (groups, boxes, layers, time span in
   years, start date from the NetCDF time units) sit above a sortable health
   table: groups that fall below 1% of initial biomass, or grow more than
   tenfold, plus age classes whose mean StructN or ResN at the end of the run
   sits outside 0.5–1.5 of the initial value.
5. Use the other tabs as usual. Dark mode is the toggle in the navbar.

![Group health](man/figures/overview_health.png)

### Views

| Tab | What you get |
| --- | --- |
| Compare | Faceted biomass with free y, GroupType / search filter, relative-to-initial toggle, optional 0.5–1.5 band on relative views, age-structure bars, observed biomass points |
| Spatial | Biomass per box from `.bgm` polygons, depth-layer selector (each water layer, sediment, integrated), time slider, optional stable colour scale, per-box series; dark box outlines |

![Spatial water layer](man/figures/spatial_layer_water.png)

![Spatial sediment layer](man/figures/spatial_layer_sediment.png)
| Consumption | Tonnes consumed from `outputPROD.nc`: age groups `<name><cohort>_Eat`, pools `<name>Grazing` (Name column, case-sensitive) × volume × DietCheck; predator and prey views. Disabled if PROD is missing |
| Physics | Temperature, salinity, NO3, NH3, Chl_a and denitrification by box and the water/sediment layers that exist in that box, plus `nominal_dz` |
| Multi-run | Overlay biomass, SSB, YOY or catch from extra run pairs; ensemble mean and range when more than two runs are loaded |
| Predation | Stacked diet composition, top N prey plus Other, year axis |
| Food web | Interactive network (visNetwork when installed) with a time slider |
| Recruitment | Beverton–Holt alpha/beta in a side panel; SSB view; observed Recruitment / SSB ribbons |
| Growth | Light / nutrient / eddy limitation by layer, years on the x-axis |
| Catch | Stacked catch by group or fleet; observed overlay and skill when a catch CSV is loaded |
| Feeding | Searchable heatmap; cells with pPREY > 0 and gape overlap = 0 are flagged |
| Mortality | Fishing, natural and predation mortality stacked per age (Age01…Age10) |

![Compare small multiples](man/figures/compare.png)

![Spatial biomass map](man/figures/spatial.png)

![Consumption from PROD.nc](man/figures/consumption.png)

![Physics tracers](man/figures/physics.png)

![Multi-run ensemble](man/figures/ensemble.png)

![Predation diet](man/figures/predation.png)

![Catch by group](man/figures/catch.png)

![Mortality by age](man/figures/mortality.png)

![Group colour palette](man/figures/palette.png)

Colours are keyed by functional-group **Code** (groups CSV order, GroupType hue
families). The same code is the same colour in every tool. Current vs previous
runs stay firebrick / olive.

Time axes use years from the NetCDF time units, not raw seconds or days. Plots
share `theme_reactiveatlantis()`. Time series use plotly when the view is small
enough to hover.

## Atlantis Scientific Guide (optional)

An optional **Atlantis Scientific Guide** drawer (the existing local
Ask Atlantis stack) can answer modelling questions from a local index
and point you at the existing tools above. When enabled, a speech-bubble
button at the bottom-right opens a right-hand drawer from any tab. While a
reply or index build is in progress the drawer shows **Thinking…**. It is off
unless you set `RA_AI_ENABLED=true`, install [Ollama](https://ollama.com) on
the same machine, and build the index. It never calls a cloud LLM. User and
maintenance notes are in [`docs/ai/`](docs/ai/). Setup of the index is in
[`inst/ai/README.md`](inst/ai/README.md). Tests use a clearly marked fixture
corpus plus this package’s own help. CSIRO user-guide PDFs can be downloaded
locally (they are not shipped in the package) and indexed with
`inst/ai/sources.atlantis-manuals.json`.

![Ask Atlantis thinking](man/figures/ask_atlantis_thinking.png)

## Legacy functions

The original exported functions still open their own apps for scripts:

```r
compare(nc.current, nc.out.old = NULL, grp.csv, bgm.file, cum.depths)
predation(biom, grp.csv, diet.file, bio.age = NULL)
feeding.mat(prm.file, grp.file, nc.initial, bgm.file, cum.depths)
food.web(diet.file, grp.file, diet.file.bypol = NULL)
growth.pp(nc.initial, grp.csv, prm.file, nc.current)
recruitment.cal(nc.initial, nc.current, yoy.file, grp.file, prm.file)
catch(grp.csv, fsh.csv, catch.nc, ext.catch.by.fleet = NULL, ext.catch.total = NULL)
mortality(grp.file, prm.file, SpeMort, PredMort)
```

## Authors

Javier Porobic

## License

[GPL-3](https://www.gnu.org/licenses/gpl-3.0.en.html)
