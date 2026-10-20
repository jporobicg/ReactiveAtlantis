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

##' Do not suspend outputs on hidden bslib nav panels
keep_outputs_alive <- function(output, names) {
  for (nm in names) {
    try(shiny::outputOptions(output, nm, suspendWhenHidden = FALSE), silent = TRUE)
  }
}

##' DT without Bootstrap 5 CSS (missing on some Debian DT builds)
ra_datatable <- function(data, ...) {
  DT::datatable(
    data,
    rownames = FALSE,
    style = "default",
    class = "display compact",
    ...
  )
}

plot_message <- function(msg) {
  graphics::plot.new()
  graphics::title(main = msg, cex.main = 1.05)
  invisible(NULL)
}

##' Catch req() / errors so a plotOutput always has an image
plot_or_message <- function(msg, expr) {
  tryCatch(force(expr), error = function(e) {
    plot_message(msg)
    invisible(NULL)
  })
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
  list(info = info, Vol = vol, VolInf = vol)
}

tool_banner <- function(project, tool) {
  if (is.null(project) || !isTRUE(project$ready)) {
    return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS[[tool]]],
                             paste0(toupper(substring(tool, 1, 1)), substring(tool, 2))))
  }
  miss <- tool_missing(project$paths, tool, project)
  if (length(miss)) return(tool_missing_card(miss, tool))
  NULL
}
