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

## Optional Ask Atlantis helpers. The panel is shown only when
## RA_AI_ENABLED is true so the default app is unchanged. Failures here
## must not prevent the calibration tools from starting.
ai_load <- normalizePath(file.path(app_dir, "..", "ai", "load.R"),
                         mustWork = FALSE)
if (!file.exists(ai_load)) {
  ai_load <- tryCatch(system.file("ai", "load.R", package = "ReactiveAtlantis"),
                      error = function(e) "")
}
ra_ai_on <- isTRUE(tryCatch({
  if (nzchar(ai_load) && file.exists(ai_load)) {
    source(ai_load, local = FALSE)
  }
  source(file.path(app_dir, "modules", "assistant_module.R"))
  ra_ai_enabled()
}, error = function(e) FALSE))

ra_navbar_title <- tags$span(
  class = "ra-brand",
  tags$img(
    src = "logo-transparent-128.png",
    alt = "ReactiveAtlantis",
    class = "ra-brand-logo"
  ),
  tags$span("ReactiveAtlantis", class = "ra-brand-text")
)

ra_header <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
  tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "app-icon-32.png"),
  tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "app-icon-180.png"),
  tags$link(rel = "manifest", href = "site.webmanifest"),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('ra-open-tab', function(title) {
      var el = Array.from(document.querySelectorAll('a.nav-link')).find(function(a){
        return (a.textContent || '').trim() === title;
      });
      if (el) el.click();
    });
  "))
)

ra_nav_args <- list(
  title = ra_navbar_title,
  id = "ra_page",
  window_title = "ReactiveAtlantis",
  theme = ra_theme(),
  fillable = FALSE,
  header = ra_header,
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
if (isTRUE(ra_ai_on)) {
  ra_nav_args <- c(ra_nav_args, list(
    nav_panel("Ask Atlantis", assistant_ui("assistant"))
  ))
}
ui <- do.call(page_navbar, ra_nav_args)

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
  if (isTRUE(ra_ai_on)) {
    assistant_server("assistant", project, parent_session = session)
  }
}

shinyApp(ui, server)
