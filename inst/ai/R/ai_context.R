## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Scientific Guide: session-scoped context    ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Read-only metadata about the loaded run. Never NetCDF arrays,
## DietCheck matrices, or plot pixels. Isolated per Shiny session
## because callers pass the current project snapshot.

ra_ai_role_names <- function() {
  c("groups", "fisheries", "output_nc", "output_nc_old", "catch_nc",
    "init_nc", "prod_nc", "biomindx", "dietcheck", "diet_bypol",
    "ssb", "yoy", "bio_prm", "run_prm", "force_prm", "bgm",
    "specific_mort", "specific_pred_mort", "mort", "ext_catch", "obs_csv")
}

ra_ai_path_exists <- function(p) {
  x <- as.character(p %||% "")[1]
  !is.na(x) && nzchar(x) && file.exists(x)
}

##' Named logical list: which FILE_ROLES have a path on disk.
ra_ai_file_availability <- function(project = NULL) {
  roles <- ra_ai_role_names()
  avail <- setNames(as.list(rep(FALSE, length(roles))), roles)
  paths <- ra_ai_paths_from_project(project)
  if (is.null(paths)) return(avail)
  for (role in intersect(names(paths), roles)) {
    avail[[role]] <- ra_ai_path_exists(paths[[role]])
  }
  avail
}

ra_ai_paths_from_project <- function(project) {
  if (is.null(project)) return(NULL)
  tryCatch({
    if (inherits(project, "reactivevalues") ||
        inherits(project, "reactiveValues")) {
      st <- project$state
      if (is.list(st) && is.list(st$paths)) return(st$paths)
      if (is.list(project$paths)) return(project$paths)
      return(NULL)
    }
    if (is.list(project)) {
      if (is.list(project$paths)) return(project$paths)
      if (is.list(project$data) && is.list(project$data$paths)) {
        return(project$data$paths)
      }
      if (is.list(project$state) && is.list(project$state$paths)) {
        return(project$state$paths)
      }
    }
    NULL
  }, error = function(e) NULL)
}

ra_ai_run_labels <- function(project = NULL) {
  if (is.null(project)) return(character())
  labs <- tryCatch({
    if (inherits(project, "reactivevalues") ||
        inherits(project, "reactiveValues")) {
      ens <- project$ensemble
      if (is.null(ens) && !is.null(project$state)) ens <- project$state$ensemble
      if (is.list(ens) && !is.null(ens$labels)) return(as.character(ens$labels))
      if (is.character(ens)) return(ens)
    }
    if (is.list(project)) {
      if (!is.null(project$run_labels)) return(as.character(project$run_labels))
      if (!is.null(project$ensemble$labels)) {
        return(as.character(project$ensemble$labels))
      }
    }
    character()
  }, error = function(e) character())
  labs[nzchar(labs)]
}

##' Session-isolated metadata for the prompt. No simulation dumps.
ra_ai_session_context <- function(input = NULL, project = NULL) {
  base <- ra_ai_safe_ui_context(input, project)
  base$file_availability <- ra_ai_file_availability(project)
  base$run_labels <- ra_ai_run_labels(project)
  base$available_roles <- names(base$file_availability)[
    vapply(base$file_availability, isTRUE, logical(1))
  ]
  base
}
