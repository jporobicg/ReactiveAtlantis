## Source the ReactiveAtlantis Shiny modules exactly as inst/shiny/app.R does, without starting the app.
ra_source_modules <- function(app_dir = system.file("shiny", package = "ReactiveAtlantis")) {
  suppressPackageStartupMessages({library(shiny); library(ncdf4); library(dplyr); library(ggplot2); library(tidyr)})
  mods <- c("theme","ui_helpers","colors","utils","catch_module","predation_module","foodweb_module","recruitment_module",
            "growth_module","predation_pressure","feeding_module","mortality_module","compare_module","project_store",
            "nc_cache","analysis_helpers","process_diagnostics","layers","overview_module","spatial_module","ages_module",
            "consumption_module","physics_module","ensemble_module")
  for (m in mods) sys.source(file.path(app_dir, "modules", paste0(m, ".R")), envir = globalenv())
  invisible(app_dir)
}
