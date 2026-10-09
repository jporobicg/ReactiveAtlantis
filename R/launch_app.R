##' Launch the ReactiveAtlantis unified application
##'
##' This function launches the unified ReactiveAtlantis Shiny application that
##' combines all calibration tools into a single interface.
##'
##' @param host Host to bind to (defaults to localhost)
##' @param port Port to listen on (NULL for random port)
##' @param launch.browser Whether to launch browser automatically (defaults to TRUE)
##' @return Starts the Shiny application
##' @export
##' @examples
##' \dontrun{
##' launch_reactiveatlantis()
##' launch_reactiveatlantis(port = 8080, launch.browser = FALSE)
##' }
launch_reactiveatlantis <- function(host = "127.0.0.1", port = NULL, launch.browser = TRUE) {
  app_dir <- system.file("shiny", package = "ReactiveAtlantis")
  
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing ReactiveAtlantis.", 
         call. = FALSE)
  }
  
  options(shiny.maxRequestSize = 500 * 1024^2)
  
  shiny::runApp(app_dir, host = host, port = port, launch.browser = launch.browser)
}
