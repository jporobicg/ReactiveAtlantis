# ReactiveAtlantis

Calibration tools for Atlantis ecosystem models, in one Shiny app.

Load a run once (a local output folder, or individual files). Every tool then
reads from that shared store. Calibration maths are unchanged from the original
functions; this package changes how you load data and how results are shown.

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

## Local folder — preferred for large NetCDF / DietCheck files
launch_reactiveatlantis(project_dir = "/path/to/Atlantis/output")

## Or open the app and paste the folder path in the Run sidebar
launch_reactiveatlantis()
```

1. Point the **Run** sidebar at an Atlantis output folder (or attach files).
   Files are detected by suffix (`*BiomIndx.txt`, `*DietCheck.txt`, `*CATCH.nc`,
   `*.bgm`, groups vs fisheries CSV headers, `pPREY` vs `toutinc` in `.prm`).
2. Click **Load run**. The checklist shows which files are present and which
   tools are enabled. A tool whose files are missing stays on a short “needs”
   card instead of a blank plot.
3. Start on **Overview**. Summary cards (groups, boxes, layers, time span in
   years, start date from the NetCDF time units) sit above a sortable health
   table: groups that fall below 1% of initial biomass, or grow more than
   tenfold.
4. Use the other tabs as usual. Dark mode is the toggle in the navbar.

![Group health](man/figures/overview_health.png)

### Views

| Tab | What you get |
| --- | --- |
| Compare | Faceted biomass with free y, GroupType / search filter, relative-to-initial toggle |
| Predation | Stacked diet composition, top N prey plus Other, year axis |
| Food web | Interactive network (visNetwork when installed) with a time slider |
| Recruitment | Beverton–Holt alpha/beta in a side panel; plots update as you type |
| Growth | Light / nutrient / eddy limitation by layer, years on the x-axis |
| Catch | Stacked catch by group or fleet; observed overlay and skill when a catch CSV is loaded |
| Feeding | Searchable heatmap; cells with pPREY > 0 and gape overlap = 0 are flagged |
| Mortality | Fishing, natural and predation mortality stacked per age (Age01…Age10) |

![Compare small multiples](man/figures/compare.png)

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
