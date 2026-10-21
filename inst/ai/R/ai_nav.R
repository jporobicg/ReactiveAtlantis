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
                   "structn", "resn", "what can reactive")
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
                   "small multiples")
    ),
    spatial = list(
      id = "spatial",
      title = "Spatial",
      module = "spatial",
      summary = paste(
        "Biomass per box drawn from .bgm polygons, time slider, group selector,",
        "per-box series; boundary boxes outlined."
      ),
      keywords = c("spatial", "map", "box", "polygon", "bgm", "distribution")
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
      keywords = c("consumption", "consumed", "eat", "grazing", "prod.nc", "dietcheck")
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
                   "chl", "denitrif", "nominal_dz", "tracer")
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
      keywords = c("predation", "predator", "prey", "diet", "dietcheck")
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
      keywords = c("recruit", "yoy", "ssb", "beverton", "alpha", "beta", "larvae")
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
                   "phyto")
    ),
    catch = list(
      id = "catch",
      title = "Catch",
      module = "catch",
      summary = paste(
        "Stacked catch by group or fleet; observed overlay and skill when a",
        "catch CSV is loaded."
      ),
      keywords = c("catch", "harvest", "fleet", "landings", "discard", "skill")
    ),
    feeding = list(
      id = "feeding",
      title = "Feeding",
      module = "feeding",
      summary = paste(
        "Searchable pPREY heatmap; gape overlap; cells with pPREY > 0 and",
        "gape overlap = 0 are flagged."
      ),
      keywords = c("feeding", "pprey", "gape", "availability", "preference",
                   "klp", "kup")
    ),
    mortality = list(
      id = "mortality",
      title = "Mortality",
      module = "mortality",
      summary = paste(
        "Fishing, natural and predation mortality stacked per age (Age01-Age10)."
      ),
      keywords = c("mortality", "fishing mortality", "natural mortality",
                   "predation mortality", "specificmort")
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
  hits
}

##' Safe session metadata only: current tab title and selected group code.
ra_ai_safe_ui_context <- function(input) {
  page <- NULL
  if (!is.null(input) && !is.null(input$ra_page) && nzchar(as.character(input$ra_page)[1])) {
    page <- as.character(input$ra_page)[1]
  }
  fg_map <- c(
    Compare = "compare-fg_total",
    Spatial = "spatial-fg",
    Consumption = "consumption-fg",
    Predation = "predation-fg",
    Recruitment = "recruitment-fg_yoy",
    Growth = "growth-fg",
    Catch = "catch-fg",
    Mortality = "mortality-fg",
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
  list(
    page = page,
    selected_group = selected_group
  )
}
