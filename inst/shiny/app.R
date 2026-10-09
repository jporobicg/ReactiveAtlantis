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

options(shiny.maxRequestSize = 500 * 1024^2)

app_dir <- getwd()
source(file.path(app_dir, "modules", "colors.R"))
source(file.path(app_dir, "modules", "utils.R"))
source(file.path(app_dir, "modules", "compare_module.R"))
source(file.path(app_dir, "modules", "predation_module.R"))
source(file.path(app_dir, "modules", "foodweb_module.R"))
source(file.path(app_dir, "modules", "recruitment_module.R"))
source(file.path(app_dir, "modules", "growth_module.R"))
source(file.path(app_dir, "modules", "catch_module.R"))
source(file.path(app_dir, "modules", "feeding_module.R"))
source(file.path(app_dir, "modules", "mortality_module.R"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Application Theme Setup           ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

app_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#2c3e50",
  primary = "#3498db",
  secondary = "#95a5a6",
  success = "#27ae60",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  base_font = font_google("Source Sans Pro"),
  heading_font = font_google("Source Sans Pro"),
  code_font = font_google("Fira Code"),
  "navbar-bg" = "#2c3e50",
  "navbar-fg" = "#ecf0f1"
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~                  UI Layout                  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ui <- page_navbar(
  title = "ReactiveAtlantis",
  theme = app_theme,
  fillable = FALSE,
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  
  nav_panel(
    title = "Compare Outputs",
    compare_ui("compare")
  ),
  
  nav_panel(
    title = "Predation",
    predation_ui("predation")
  ),
  
  nav_panel(
    title = "Food Web",
    foodweb_ui("foodweb")
  ),
  
  nav_panel(
    title = "Recruitment",
    recruitment_ui("recruitment")
  ),
  
  nav_panel(
    title = "Growth",
    growth_ui("growth")
  ),
  
  nav_panel(
    title = "Catch Analysis",
    catch_ui("catch")
  ),
  
  nav_panel(
    title = "Feeding Matrix",
    feeding_ui("feeding")
  ),
  
  nav_panel(
    title = "Mortality",
    mortality_ui("mortality")
  ),
  
  nav_panel(
    title = "About",
    card(
      card_header("About ReactiveAtlantis"),
      card_body(
        h3("Calibration Tools for Atlantis Ecosystem Models"),
        p("ReactiveAtlantis provides integrated tools for tuning, parameterization, 
          and analysis of processes commonly modified during Atlantis model calibration."),
        hr(),
        h4("Available Tools"),
        tags$ul(
          tags$li(strong("Compare Outputs:"), " Visualize and compare biomass, abundance, 
                  and nitrogen between simulation outputs."),
          tags$li(strong("Predation:"), " Analyze predator-prey interactions and 
                  predation pressure through time."),
          tags$li(strong("Food Web:"), " Explore food web structure and calculate 
                  trophic levels."),
          tags$li(strong("Recruitment:"), " Estimate recruitment for age-structured 
                  functional groups."),
          tags$li(strong("Growth:"), " Analyze limitation factors for primary producer growth."),
          tags$li(strong("Catch Analysis:"), " Visualize harvest outputs and perform 
                  model skill assessment."),
          tags$li(strong("Feeding Matrix:"), " Calibrate predator-prey availability 
                  matrices."),
          tags$li(strong("Mortality:"), " Explore natural, fishing, and predation mortality.")
        ),
        hr(),
        p(class = "text-muted", "Version 0.0.2.0")
      )
    )
  )
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~                Server Logic                 ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

server <- function(input, output, session) {
  
  compare_server("compare")
  predation_server("predation")
  foodweb_server("foodweb")
  recruitment_server("recruitment")
  growth_server("growth")
  catch_server("catch")
  feeding_server("feeding")
  mortality_server("mortality")
  
}

shinyApp(ui, server)
