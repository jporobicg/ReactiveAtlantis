## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     UI helpers: binding, DT, selections     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' Keep a previous select value if it is still among the choices
keep_choice <- function(current, choices, fallback = NULL) {
  ch <- unname(as.character(choices))
  ch <- ch[nzchar(ch)]
  if (length(ch) == 0) return(fallback)
  cur <- as.character(current)[1]
  if (!is.null(cur) && nzchar(cur) && cur %in% ch) return(cur)
  fb <- as.character(fallback)[1]
  if (!is.null(fb) && nzchar(fb) && fb %in% ch) return(fb)
  ch[1]
}

update_select_keep <- function(session, inputId, choices, selected = NULL) {
  if (is.null(choices) || length(choices) == 0) return(invisible(NULL))
  sel <- keep_choice(selected, choices)
  shiny::updateSelectInput(session, inputId, choices = choices, selected = sel)
}

##' Run an expression under shiny::withProgress when a session is active
ra_progress <- function(message, expr, detail = NULL, value = 0) {
  sess <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (is.null(sess)) return(force(expr))
  shiny::withProgress(
    message = message, detail = detail, value = value, session = sess, {
      force(expr)
    }
  )
}

##' Advance a withProgress bar; no-op outside a progress context
ra_inc <- function(amount = 0.1, detail = NULL) {
  suppressWarnings(tryCatch(
    shiny::incProgress(amount, detail = detail),
    error = function(e) invisible(NULL)
  ))
}

##' Do not suspend outputs on hidden bslib nav panels
keep_outputs_alive <- function(output, names) {
  for (nm in names) {
    try(shiny::outputOptions(output, nm, suspendWhenHidden = FALSE), silent = TRUE)
  }
}

##' Debounce a reactive; 250 ms for group selects, 400 ms for search boxes
ra_debounce <- function(r, millis = 250) {
  shiny::debounce(r, millis)
}

##' DT without Bootstrap 5 CSS (missing on some Debian DT builds)
ra_datatable <- function(data, ..., filter = "none") {
  dots <- list(...)
  opts <- dots$options
  if (is.null(opts)) opts <- list()
  dots$options <- NULL
  do.call(DT::datatable, c(list(
    data = data,
    rownames = FALSE,
    style = "default",
    class = "display compact",
    filter = filter,
    options = opts
  ), dots))
}

plot_message <- function(msg) {
  graphics::plot.new()
  graphics::title(main = msg, cex.main = 1.05)
  invisible(NULL)
}

##' Catch req() / errors so a plotOutput always has an image
plot_or_message <- function(msg, expr) {
  tryCatch(force(expr), error = function(e) {
    if (inherits(e, "shiny.silent.error")) {
      plot_message(msg)
    } else {
      plot_message(e$message)
    }
    invisible(NULL)
  })
}

ra_scale_x_year <- function(p, x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(p)
  xr <- range(x)
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    x <- as.Date(x)
    xr <- range(x)
    y0 <- as.integer(format(xr[1], "%Y"))
    y1 <- as.integer(format(xr[2], "%Y"))
    span <- max(1L, y1 - y0)
    step <- max(1L, as.integer(round(span / 4)))
    yrs <- seq(y0, y1, by = step)
    if (tail(yrs, 1) != y1) yrs <- c(yrs, y1)
    br <- as.Date(sprintf("%d-01-01", unique(yrs)))
    p + ggplot2::scale_x_date(breaks = br, date_labels = "%Y")
  } else {
    p + ggplot2::scale_x_continuous(breaks = pretty(as.numeric(xr), n = 5))
  }
}

keep_numeric <- function(current, min_v, max_v, fallback = min_v) {
  x <- suppressWarnings(as.numeric(current)[1])
  if (!is.finite(x)) return(as.numeric(fallback)[1])
  min(max(x, min_v), max_v)
}

fg_select_choices <- function(grp, on_only = TRUE) {
  if (is.null(grp) || nrow(grp) == 0) return(c("Load a run" = ""))
  keep <- grp
  if (on_only && "isturnedon" %in% names(grp)) {
    on <- grp[grp$isturnedon == 1, , drop = FALSE]
    if (nrow(on) > 0) keep <- on
  }
  codes <- as.character(keep$code)
  if ("longname" %in% names(keep) && any(nzchar(as.character(keep$longname)))) {
    return(stats::setNames(codes, paste(codes, keep$longname, sep = " — ")))
  }
  codes
}

age_select_choices <- function(grp) {
  if (is.null(grp) || nrow(grp) == 0) return(character())
  keep <- grp
  if ("numcohorts" %in% names(grp)) {
    keep <- keep[keep$numcohorts > 1, , drop = FALSE]
  }
  special <- c("PWN", "PRAWNS", "PRAWN", "CEP", "MOB_EP_OTHER",
               "SEAGRASS", "CORAL", "MANGROVE", "MANGROVES", "SPONGE")
  if ("grouptype" %in% names(keep)) {
    keep <- keep[!keep$grouptype %in% special, , drop = FALSE]
  }
  fg_select_choices(keep, on_only = TRUE)
}

dummy_inf_box <- function(n_box = 1, n_z = 2) {
  n_box <- max(1L, as.integer(n_box)[1])
  n_z <- max(2L, as.integer(n_z)[1])
  info <- data.frame(
    Boxid = seq_len(n_box) - 1L,
    Area = 1,
    Volumen = 1,
    Depth = 50,
    Layers = n_z - 1L,
    dynamic = 1,
    Depth_Bound = 50,
    stringsAsFactors = FALSE
  )
  vol <- matrix(1, nrow = n_z, ncol = n_box)
  list(info = info, Vol = vol, VolInf = vol, dummy = TRUE)
}

tool_banner <- function(project, tool) {
  snap <- project_snapshot(project)
  title <- paste0(toupper(substring(tool, 1, 1)), substring(tool, 2))
  if (!isTRUE(snap$ready)) {
    return(tool_missing_card(unname(as.character(FILE_LABELS[TOOL_REQUIREMENTS[[tool]]])),
                             title))
  }
  miss <- tool_missing(snap$paths, tool, snap)
  if (length(miss)) return(tool_missing_card(miss, tool))
  ## Without a BGM the app cannot tell boundary boxes apart (validation D17):
  ## say so where totals or maps depend on it, instead of failing silently.
  if (tool %in% c("compare", "ages", "physics", "overview") &&
      !path_ok(snap$paths$bgm)) {
    return(shiny::div(
      class = "alert alert-warning", role = "alert",
      shiny::strong("No BGM geometry found. "),
      "Boundary boxes cannot be identified, so system totals here include them",
      " and will not match BiomIndx.txt. Add the .bgm file to the configuration folder."
    ))
  }
  NULL
}
