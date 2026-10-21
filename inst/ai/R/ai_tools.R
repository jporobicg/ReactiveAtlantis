## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Scientific Guide: tool registry and ranking ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ra_ai_tools_path <- function() {
  ra_ai_ai_file("tools.json")
}

##' Load the validated tool registry. Falls back to the nav catalog.
ra_ai_tool_catalog <- function() {
  path <- ra_ai_tools_path()
  nav <- ra_ai_nav_catalog()
  if (!file.exists(path) || !ra_ai_need_pkg("jsonlite")) {
    return(lapply(nav, function(x) {
      x$tool_id <- x$id
      x$preconditions <- character()
      x
    }))
  }
  spec <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(spec) || is.null(spec$tools)) {
    return(lapply(nav, function(x) {
      x$tool_id <- x$id
      x
    }))
  }
  out <- list()
  for (t in spec$tools) {
    id <- as.character(t$tool_id %||% t$id %||% "")[1]
    if (!nzchar(id) || !id %in% names(nav)) next
    entry <- nav[[id]]
    entry$tool_id <- id
    entry$purpose <- t$purpose %||% entry$summary
    entry$example_questions <- as.character(t$example_questions %||% character())
    entry$preconditions <- as.character(t$preconditions %||% character())
    entry$function_refs <- as.character(t$function_refs %||% character())
    entry$navigation_target <- as.character(t$navigation_target %||% entry$title)[1]
    entry$what_to_look_for <- as.character(t$what_to_look_for %||% "")[1]
    entry$limitations <- as.character(t$limitations %||% "")[1]
    if (!is.null(t$keywords)) entry$keywords <- as.character(t$keywords)
    out[[id]] <- entry
  }
  out
}

ra_ai_known_tool_ids <- function(catalog = ra_ai_tool_catalog()) {
  unique(c(names(catalog),
           vapply(catalog, function(x) as.character(x$tool_id %||% x$id)[1],
                  character(1))))
}

##' Drop tool ids that are not in the unified-app catalog.
ra_ai_validate_tool_ids <- function(ids, catalog = ra_ai_tool_catalog()) {
  known <- ra_ai_known_tool_ids(catalog)
  ids <- unique(as.character(unlist(ids)))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  ids[ids %in% known]
}

ra_ai_tools_as_nav <- function(ids, catalog = ra_ai_tool_catalog()) {
  ids <- ra_ai_validate_tool_ids(ids, catalog)
  lapply(ids, function(id) {
    entry <- catalog[[id]]
    list(id = entry$id, title = entry$title, summary = entry$summary)
  })
}

ra_ai_example_questions <- function() {
  c(
    "What does the Atlantis parameter KLP represent?",
    "What is pPREY? Is it a diet proportion?",
    "Where do I inspect predation?",
    "What should I check when biomass is declining?",
    "What should I check when recruitment is low?"
  )
}

##' Deterministic question -> tool ranking (no LLM).
ra_ai_rank_tools <- function(question, context = list(),
                             catalog = ra_ai_tool_catalog(),
                             k = 6L) {
  q <- tolower(as.character(question)[1])
  if (!nzchar(q) || length(catalog) == 0) return(list())
  avail <- context$file_availability
  page <- tolower(as.character(context$page %||% "")[1])

  scores <- vapply(names(catalog), function(id) {
    entry <- catalog[[id]]
    kws <- as.character(entry$keywords %||% character())
    s <- sum(vapply(kws, function(w) {
      grepl(w, q, fixed = TRUE)
    }, logical(1)))
    eqs <- as.character(entry$example_questions %||% character())
    if (length(eqs) > 0) {
      s <- s + 0.5 * sum(vapply(eqs, function(e) {
        grepl(tolower(e), q, fixed = TRUE) || grepl(q, tolower(e), fixed = TRUE)
      }, logical(1)))
    }
    if (nzchar(page) &&
        (identical(page, tolower(entry$title)) || identical(page, id))) {
      s <- s + 0.25
    }
    need <- as.character(entry$preconditions %||% character())
    if (is.list(avail) && length(avail) > 0 && length(need) > 0) {
      miss <- need[!vapply(need, function(r) isTRUE(avail[[r]]), logical(1))]
      if (length(miss) > 0) s <- s * 0.55
    }
    as.numeric(s)
  }, numeric(1))

  if (exists("ra_ai_is_calibration_investigation", mode = "function") &&
      isTRUE(ra_ai_is_calibration_investigation(question))) {
    extra <- ra_ai_investigation_nav_ids()
    scores[names(scores) %in% extra] <- scores[names(scores) %in% extra] + 0.35
  }

  matched <- ra_ai_match_cases(question)
  if (length(matched) > 0) {
    tids <- unique(unlist(lapply(matched, function(m) m$tool_ids)))
    scores[names(scores) %in% tids] <- scores[names(scores) %in% tids] + 0.4
  }

  ord <- order(scores, decreasing = TRUE)
  keep <- ord[scores[ord] > 0]
  if (length(keep) == 0) return(list())
  keep <- keep[seq_len(min(as.integer(k), length(keep)))]
  lapply(names(catalog)[keep], function(id) {
    entry <- catalog[[id]]
    list(
      id = entry$id,
      tool_id = entry$tool_id %||% entry$id,
      title = entry$title,
      summary = entry$summary,
      score = unname(scores[[id]])
    )
  })
}
