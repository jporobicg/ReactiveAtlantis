## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   ReactiveAtlantis Unified Application   ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

library(shiny)
library(bslib)
library(ggplot2)
library(ncdf4)
library(stringr)
library(data.table)
library(RColorBrewer)
library(reshape2)
library(plotrix)
library(DT)
library(dplyr)
library(scales)
library(proj4)
library(tidyr)
library(stats)

if (requireNamespace("plotly", quietly = TRUE)) library(plotly)
if (requireNamespace("visNetwork", quietly = TRUE)) library(visNetwork)

max_upload_mb <- getOption("reactiveatlantis.max_upload_mb", default = 8192)
requested_upload <- as.numeric(max_upload_mb) * 1024^2
current_upload <- suppressWarnings(as.numeric(getOption("shiny.maxRequestSize")))
options(shiny.maxRequestSize = max(c(requested_upload, current_upload), na.rm = TRUE))

app_dir <- getwd()
source(file.path(app_dir, "modules", "theme.R"))
source(file.path(app_dir, "modules", "ui_helpers.R"))
source(file.path(app_dir, "modules", "colors.R"))
source(file.path(app_dir, "modules", "utils.R"))
source(file.path(app_dir, "modules", "catch_module.R"))
source(file.path(app_dir, "modules", "predation_module.R"))
source(file.path(app_dir, "modules", "foodweb_module.R"))
source(file.path(app_dir, "modules", "recruitment_module.R"))
source(file.path(app_dir, "modules", "growth_module.R"))
source(file.path(app_dir, "modules", "feeding_module.R"))
source(file.path(app_dir, "modules", "mortality_module.R"))
source(file.path(app_dir, "modules", "compare_module.R"))
source(file.path(app_dir, "modules", "project_store.R"))
source(file.path(app_dir, "modules", "nc_cache.R"))
source(file.path(app_dir, "modules", "analysis_helpers.R"))
source(file.path(app_dir, "modules", "overview_module.R"))
source(file.path(app_dir, "modules", "spatial_module.R"))
source(file.path(app_dir, "modules", "consumption_module.R"))
source(file.path(app_dir, "modules", "physics_module.R"))
source(file.path(app_dir, "modules", "ensemble_module.R"))

ui <- page_navbar(
  title = "ReactiveAtlantis",
  window_title = "ReactiveAtlantis",
  theme = ra_theme(),
  fillable = FALSE,
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  sidebar = sidebar(
    title = "Run",
    width = 340,
    open = "open",
    project_sidebar_ui("project")
  ),
  nav_spacer(),
  nav_item(input_dark_mode(id = "color_mode", mode = "light")),
  nav_panel("Overview", overview_ui("overview")),
  nav_panel("Compare", compare_ui("compare")),
  nav_panel("Spatial", spatial_ui("spatial")),
  nav_panel("Consumption", consumption_ui("consumption")),
  nav_panel("Physics", physics_ui("physics")),
  nav_panel("Multi-run", ensemble_ui("ensemble")),
  nav_panel("Predation", predation_ui("predation")),
  nav_panel("Food web", foodweb_ui("foodweb")),
  nav_panel("Recruitment", recruitment_ui("recruitment")),
  nav_panel("Growth", growth_ui("growth")),
  nav_panel("Catch", catch_ui("catch")),
  nav_panel("Feeding", feeding_ui("feeding")),
  nav_panel("Mortality", mortality_ui("mortality"))
)

server <- function(input, output, session) {
  project <- project_server("project")
  overview_server("overview", project)
  compare_server("compare", project)
  spatial_server("spatial", project)
  consumption_server("consumption", project)
  physics_server("physics", project)
  ensemble_server("ensemble", project)
  predation_server("predation", project)
  foodweb_server("foodweb", project)
  recruitment_server("recruitment", project)
  growth_server("growth", project)
  catch_server("catch", project)
  feeding_server("feeding", project)
  mortality_server("mortality", project)
}

shinyApp(ui, server)
