## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis: validated UI navigation map  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Titles and module ids must match inst/shiny/app.R. A unit test compares
## this catalog with nav_panel() titles so invented destinations cannot ship.

##' Catalog of existing ReactiveAtlantis tabs the assistant may suggest.
##' @return Named list of tool entries
ra_ai_nav_catalog <- function() {
  list(
    overview = list(
      id = "overview",
      title = "Overview",
      module = "overview",
      summary = paste(
        "Summary cards (groups, boxes, layers, time span) and a sortable health",
        "table: groups below 1% of initial biomass, groups that grow more than",
        "tenfold, and age classes whose mean StructN or ResN sits outside 0.5-1.5",
        "of the initial value."
      ),
      keywords = c("overview", "health", "extinct", "exploding", "summary",
                   "structn", "resn", "what can reactive", "collapse", "crash",
                   "declin")
    ),
    compare = list(
      id = "compare",
      title = "Compare",
      module = "compare",
      summary = paste(
        "Faceted biomass with free y, GroupType / search filter, relative-to-initial",
        "toggle, optional 0.5-1.5 band, age-structure bars, and observed biomass points."
      ),
      keywords = c("compare", "biomass", "bt/b0", "relative", "age structure",
                   "small multiples", "collapse", "crash", "declin")
    ),
    ages = list(
      id = "ages",
      title = "Ages",
      module = "ages",
      summary = paste(
        "Whole-system age-class time series for a cohort group: reserve and",
        "structural nitrogen (numbers-weighted means), numbers, and biomass."
      ),
      keywords = c("ages", "age class", "cohort", "resn", "structn", "numbers",
                   "weight", "age structure", "condition")
    ),
    spatial = list(
      id = "spatial",
      title = "Spatial",
      module = "spatial",
      summary = paste(
        "Box map for a group or tracer at one depth layer (per-box water",
        "layers, sediment, or integrated) with a time slider and play control.",
        "Time series for the selected box, all boxes, or the whole system.",
        "Click a box to select it. By-age view plots ResN, StructN, numbers",
        "and biomass for one box and layer."
      ),
      keywords = c("spatial", "map", "box", "polygon", "bgm", "distribution",
                   "layer", "depth", "sediment", "collapse", "crash", "age class")
    ),
    consumption = list(
      id = "consumption",
      title = "Consumption",
      module = "consumption",
      summary = paste(
        "Tonnes consumed from outputPROD.nc: age groups <name><cohort>_Eat,",
        "pools <name>Grazing (Name column) times volume times DietCheck.",
        "Predator and prey views. Disabled if PROD is missing."
      ),
      keywords = c("consumption", "consumed", "eat", "grazing", "prod.nc",
                   "dietcheck", "starv", "food limitation")
    ),
    physics = list(
      id = "physics",
      title = "Physics",
      module = "physics",
      summary = paste(
        "Temperature, salinity, NO3, NH3, Chl_a and denitrification by box and",
        "layer, plus nominal_dz."
      ),
      keywords = c("physics", "temperature", "salinity", "nitrate", "nh3",
                   "chl", "denitrif", "nominal_dz", "tracer", "oxygen")
    ),
    ensemble = list(
      id = "ensemble",
      title = "Multi-run",
      module = "ensemble",
      summary = paste(
        "Overlay biomass, SSB, YOY or catch from extra run pairs; ensemble",
        "mean and range when more than two runs are loaded."
      ),
      keywords = c("multi-run", "ensemble", "overlay", "scenario compare")
    ),
    predation = list(
      id = "predation",
      title = "Predation",
      module = "predation",
      summary = paste(
        "Stacked diet composition, top N prey plus Other, year axis."
      ),
      keywords = c("predation", "predator", "prey", "diet", "dietcheck",
                   "collapse", "crash")
    ),
    foodweb = list(
      id = "foodweb",
      title = "Food web",
      module = "foodweb",
      summary = paste(
        "Interactive network (visNetwork when installed) with a time slider",
        "and a trophic-level table."
      ),
      keywords = c("food web", "foodweb", "trophic", "network")
    ),
    recruitment = list(
      id = "recruitment",
      title = "Recruitment",
      module = "recruitment",
      summary = paste(
        "Beverton-Holt alpha/beta in a side panel; SSB view; observed",
        "Recruitment / SSB ribbons. YOY.txt CODE.0 plus larvae from the curve."
      ),
      keywords = c("recruit", "yoy", "ssb", "beverton", "alpha", "beta", "larvae",
                   "kdenr", "collapse", "crash")
    ),
    growth = list(
      id = "growth",
      title = "Growth",
      module = "growth",
      summary = paste(
        "Light / nutrient / eddy limitation by layer for primary producers,",
        "years on the x-axis."
      ),
      keywords = c("growth", "light", "nutrient", "eddy", "primary producer",
                   "phyto", "condition", "resn", "structn")
    ),
    catch = list(
      id = "catch",
      title = "Catch",
      module = "catch",
      summary = paste(
        "Stacked catch by group or fleet; observed overlay and skill when a",
        "catch CSV is loaded."
      ),
      keywords = c("catch", "harvest", "fleet", "landings", "discard", "skill",
                   "fishing")
    ),
    feeding = list(
      id = "feeding",
      title = "Feeding",
      module = "feeding",
      summary = paste(
        "Full pPREY matrix; hover a cell for value and group names with age;",
        "click to set predator and prey dropdowns and highlight the row/column;",
        "bar charts of the selected predator row and prey column.",
        "Diagnostics and compare stay as secondary views."
      ),
      keywords = c("feeding", "pprey", "gape", "availability", "preference",
                   "klp", "kup", "starv", "mum", "dietcheck", "diet")
    ),
    mortality = list(
      id = "mortality",
      title = "Mortality",
      module = "mortality",
      summary = paste(
        "Fishing, natural and predation mortality stacked per age (Age01-Age10)."
      ),
      keywords = c("mortality", "fishing mortality", "natural mortality",
                   "predation mortality", "specificmort", "collapse", "crash")
    )
  )
}

ra_ai_nav_titles <- function(catalog = ra_ai_nav_catalog()) {
  vapply(catalog, function(x) x$title, character(1))
}

##' Drop any navigation target that is not in the catalog.
ra_ai_validate_nav <- function(targets, catalog = ra_ai_nav_catalog()) {
  if (is.null(targets) || length(targets) == 0) return(list())
  titles <- ra_ai_nav_titles(catalog)
  ids <- names(catalog)
  out <- list()
  if (is.data.frame(targets)) {
    targets <- split(targets, seq_len(nrow(targets)))
  }
  for (item in targets) {
    if (is.null(item)) next
    if (is.character(item) && length(item) == 1) {
      key <- item
      hit <- NULL
    } else {
      key <- as.character(item$id %||% item$title %||% item$label %||% "")[1]
      hit <- item
    }
    if (!nzchar(key)) next
    id <- NA_character_
    if (key %in% ids) {
      id <- key
    } else if (key %in% titles) {
      id <- ids[match(key, titles)]
    } else {
      low <- tolower(key)
      id_match <- ids[tolower(ids) == low]
      title_match <- ids[tolower(titles) == low]
      if (length(id_match)) id <- id_match[1]
      if (is.na(id) && length(title_match)) id <- title_match[1]
    }
    if (is.na(id)) next
    entry <- catalog[[id]]
    out[[id]] <- list(
      id = entry$id,
      title = entry$title,
      summary = entry$summary
    )
  }
  unname(out)
}

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
}

##' Existing tabs that inspect a collapsing / crashing group.
ra_ai_investigation_nav_ids <- function() {
  c("overview", "compare", "ages", "predation", "consumption", "feeding",
    "recruitment", "growth", "mortality", "catch", "spatial", "physics")
}

##' Keyword map from a question onto existing tabs (no LLM involved).
ra_ai_suggest_nav <- function(question, catalog = ra_ai_nav_catalog()) {
  q <- tolower(as.character(question)[1])
  if (!nzchar(q)) return(list())
  hits <- list()
  for (id in names(catalog)) {
    kws <- catalog[[id]]$keywords
    if (any(vapply(kws, function(k) grepl(k, q, fixed = TRUE), logical(1)))) {
      hits[[length(hits) + 1]] <- list(
        id = catalog[[id]]$id,
        title = catalog[[id]]$title,
        summary = catalog[[id]]$summary
      )
    }
  }
  if (exists("ra_ai_is_calibration_investigation", mode = "function") &&
      isTRUE(ra_ai_is_calibration_investigation(question))) {
    extra <- ra_ai_validate_nav(ra_ai_investigation_nav_ids(), catalog)
    seen <- if (length(hits) > 0) {
      vapply(hits, function(x) x$id, character(1))
    } else {
      character()
    }
    for (n in extra) {
      if (!n$id %in% seen) {
        hits[[length(hits) + 1]] <- n
        seen <- c(seen, n$id)
      }
    }
  }
  hits
}

##' Safe session metadata: current tab, selected group, loaded groups table.
ra_ai_safe_ui_context <- function(input, project = NULL) {
  page <- NULL
  if (!is.null(input) && !is.null(input$ra_page) && nzchar(as.character(input$ra_page)[1])) {
    page <- as.character(input$ra_page)[1]
  }
  fg_map <- c(
    Compare = "compare-fg_total",
    Ages = "ages-fg",
    Spatial = "spatial-fg",
    Consumption = "consumption-fg",
    Predation = "predation-fg",
    Recruitment = "recruitment-fg_yoy",
    Growth = "growth-fg",
    Catch = "catch-fg",
    Mortality = "mortality-fg",
    Feeding = "feeding-pred_group",
    `Multi-run` = "ensemble-fg",
    `Food web` = "foodweb-focal_fg"
  )
  selected_group <- NULL
  if (!is.null(page) && page %in% names(fg_map) && !is.null(input)) {
    val <- input[[fg_map[[page]]]]
    if (!is.null(val) && nzchar(as.character(val)[1]) &&
        !identical(as.character(val)[1], "All")) {
      selected_group <- as.character(val)[1]
    }
  }
  groups <- ra_ai_groups_from_project(project)
  out <- list(
    page = page,
    selected_group = selected_group,
    groups = groups
  )
  if (exists("ra_ai_file_availability", mode = "function")) {
    out$file_availability <- ra_ai_file_availability(project)
    out$available_roles <- names(out$file_availability)[
      vapply(out$file_availability, isTRUE, logical(1))
    ]
    out$run_labels <- ra_ai_run_labels(project)
  }
  out
}

##' Extract the loaded groups table from a project snapshot or reactive.
ra_ai_groups_from_project <- function(project) {
  if (is.null(project)) return(NULL)
  grp <- tryCatch({
    if (inherits(project, "reactivevalues") || inherits(project, "reactiveValues")) {
      st <- project$state
      if (!is.null(st) && is.list(st$data) && !is.null(st$data$grp)) {
        st$data$grp
      } else if (!is.null(project$data) && !is.null(project$data$grp)) {
        project$data$grp
      } else {
        NULL
      }
    } else if (is.list(project)) {
      if (!is.null(project$data) && !is.null(project$data$grp)) {
        project$data$grp
      } else if (!is.null(project$grp)) {
        project$grp
      } else {
        NULL
      }
    } else {
      NULL
    }
  }, error = function(e) NULL)
  ra_ai_normalise_groups(grp)
}
