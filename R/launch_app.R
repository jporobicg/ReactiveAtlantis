##' Launch the ReactiveAtlantis unified application
##'
##' This function launches the unified ReactiveAtlantis Shiny application that
##' combines all calibration tools into a single interface.
##'
##' @return Starts the Shiny application
##' @export
##' @examples
##' \dontrun{
##' launch_reactiveatlantis()
##' }
launch_reactiveatlantis <- function() {
  app_dir <- system.file("shiny", package = "ReactiveAtlantis")
  
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing ReactiveAtlantis.", 
         call. = FALSE)
  }
  
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
}
