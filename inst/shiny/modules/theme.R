## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     ReactiveAtlantis ggplot and app theme  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

RA_NAVY <- "#0B3C5D"
RA_TEAL <- "#1D7874"
RA_SAND <- "#F7F4EC"
RA_INK <- "#1B2833"
RA_GRID <- "#E6E2D6"
RA_MUTED <- "#5C6B73"
## Darker than the fill and the sand theme so Spatial box edges stay visible.
RA_BOX_OUTLINE <- "#0A1218"

ra_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = RA_SAND,
    fg = RA_INK,
    primary = RA_NAVY,
    secondary = RA_TEAL,
    success = RA_TEAL,
    info = "#328CC1",
    warning = "#C4A35A",
    danger = "#A33B20",
    base_font = bslib::font_collection(
      "-apple-system", "BlinkMacSystemFont", "Segoe UI",
      "Helvetica Neue", "Arial", "sans-serif"
    ),
    heading_font = bslib::font_collection(
      "-apple-system", "BlinkMacSystemFont", "Segoe UI",
      "Helvetica Neue", "Arial", "sans-serif"
    ),
    "navbar-bg" = RA_NAVY,
    "navbar-color" = "#F7F4EC",
    "card-border-radius" = "0.6rem"
  )
}

##' Shared ggplot theme for every plot in the app
##' @param base_size Base font size
##' @return A ggplot2 theme
theme_reactiveatlantis <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "") %+replace%
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = RA_GRID, linewidth = 0.35),
      axis.title = ggplot2::element_text(size = 12, colour = RA_INK),
      axis.text = ggplot2::element_text(size = 11, colour = RA_MUTED),
      plot.title = ggplot2::element_text(
        size = base_size + 1, face = "bold", colour = RA_NAVY,
        hjust = 0, margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(size = 11, colour = RA_MUTED, hjust = 0),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 11),
      strip.text = ggplot2::element_text(size = 11, face = "bold", colour = RA_NAVY),
      strip.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
}

##' Convert a ggplot to plotly when it helps; otherwise return the ggplot
##' @param p A ggplot
##' @param tooltip Tooltip fields
##' @param max_series Skip plotly when the plot is too busy
##' @param n_series Number of series/facets
ra_ggplotly <- function(p, tooltip = c("x", "y", "colour", "fill", "text"),
                        n_series = 1, max_series = 36) {
  if (!requireNamespace("plotly", quietly = TRUE) || n_series > max_series) {
    return(p)
  }
  plt <- plotly::ggplotly(p, tooltip = tooltip)
  plotly::config(plt, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

ra_is_plotly <- function(x) {
  inherits(x, "plotly")
}

tool_missing_card <- function(missing_labels, tool_name) {
  labs <- unname(as.character(missing_labels))
  labs <- labs[!is.na(labs) & nzchar(labs)]
  items <- lapply(seq_along(labs), function(i) shiny::tags$li(labs[[i]]))
  bslib::card(
    bslib::card_header(paste(tool_name, "needs more files")),
    bslib::card_body(
      shiny::p("Load these from the Run panel on the left:"),
      shiny::tags$ul(items)
    )
  )
}
