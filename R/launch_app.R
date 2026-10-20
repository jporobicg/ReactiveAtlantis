##' Launch the ReactiveAtlantis unified application
##'
##' This function launches the unified ReactiveAtlantis Shiny application that
##' combines all calibration tools into a single interface.
##'
##' @param host Host to bind to (defaults to localhost)
##' @param port Port to listen on (NULL for random port)
##' @param launch.browser Whether to launch browser automatically (defaults to TRUE)
##' @param max_upload_mb Maximum Shiny upload size in megabytes. Applied only
##'   when it is larger than the current \code{shiny.maxRequestSize} option.
##'   Default is 4096 (4 GB).
##' @param project_dir Optional local folder. The app scans it for Atlantis
##'   files (BiomIndx, DietCheck, CATCH.nc, groups CSV, and so on) so large
##'   outputs do not need to be uploaded through the browser.
##' @return Starts the Shiny application
##' @export
##' @examples
##' \dontrun{
##' launch_reactiveatlantis()
##' launch_reactiveatlantis(port = 8080, launch.browser = FALSE)
##' launch_reactiveatlantis(max_upload_mb = 8192)
##' launch_reactiveatlantis(project_dir = "/path/to/NorthSea/output")
##' }
launch_reactiveatlantis <- function(host = "127.0.0.1", port = NULL,
                                    launch.browser = TRUE,
                                    max_upload_mb = 4096,
                                    project_dir = NULL) {
  app_dir <- system.file("shiny", package = "ReactiveAtlantis")
  
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing ReactiveAtlantis.", 
         call. = FALSE)
  }
  
  options(reactiveatlantis.max_upload_mb = max_upload_mb)
  if (!is.null(project_dir)) {
    options(reactiveatlantis.project_dir = project_dir)
  }
  apply_max_upload_mb(max_upload_mb)
  
  shiny::runApp(app_dir, host = host, port = port, launch.browser = launch.browser)
}

##' Raise the Shiny upload limit without shrinking a larger existing value
##' @param max_upload_mb Requested limit in megabytes
##' @return The effective \code{shiny.maxRequestSize} in bytes, invisibly
##' @author Javier Porobic
apply_max_upload_mb <- function(max_upload_mb = 4096) {
  requested <- as.numeric(max_upload_mb) * 1024^2
  if (length(requested) != 1 || !is.finite(requested) || requested <= 0) {
    stop("max_upload_mb must be a single positive number.", call. = FALSE)
  }
  current <- getOption("shiny.maxRequestSize")
  if (is.null(current) || is.na(current) || current < requested) {
    options(shiny.maxRequestSize = requested)
  }
  invisible(getOption("shiny.maxRequestSize"))
}
