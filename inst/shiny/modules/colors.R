## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~        Deterministic Color Palettes       ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' Base hues used to build the functional-group palette
.fg_base_colors <- function() {
  c(
    RColorBrewer::brewer.pal(8, "Dark2"),
    RColorBrewer::brewer.pal(8, "Set2"),
    RColorBrewer::brewer.pal(9, "Set1"),
    c("#000000", "#E69F00", "#56B4E9", "#009E73",
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
    RColorBrewer::brewer.pal(9, "BuPu")[2:9],
    RColorBrewer::brewer.pal(9, "BrBG")[1:3],
    RColorBrewer::brewer.pal(9, "OrRd")[2:9]
  )
}

##' Build one named colour palette keyed by functional-group Code
##'
##' Colours are assigned once from the groups CSV Code column. Looking up a
##' subset later keeps the same colour for each Code.
##' @param codes Character vector of group codes from the groups CSV
##' @return Named character vector of hex colours
build_fg_palette <- function(codes) {
  codes <- unique(as.character(codes))
  codes <- trimws(codes)
  codes <- codes[!is.na(codes) & nzchar(codes)]
  n <- length(codes)
  if (n == 0) {
    return(setNames(character(0), character(0)))
  }
  pal <- grDevices::colorRampPalette(.fg_base_colors())(n)
  names(pal) <- codes
  pal
}

##' Get a fixed color palette
##' @param n Number of colors needed
##' @return Vector of hex colors
get_palette <- function(n) {
  grDevices::colorRampPalette(.fg_base_colors())(n)
}

##' Get predation colors (fixed palette for non-group series)
##' @return Vector of 4 colors
get_predation_colors <- function() {
  mycol <- get_palette(15)
  mycol[c(14, 13, 12, 10)]
}

##' Get biomass comparison colors (fixed for current vs previous)
##' @return Named vector with 'Current' and 'Previous' colors
get_comparison_colors <- function() {
  c(Current = "firebrick3", Previous = "darkolivegreen")
}

##' Get food web colors (fixed for predation direction)
##' @return Vector of 2 colors for DownTop and TopDown
get_foodweb_colors <- function() {
  RColorBrewer::brewer.pal(8, 'RdYlBu')[c(7, 2)]
}

##' Look up colours for group codes from a full palette
##' @param fg_names Character vector of functional group codes
##' @param palette Optional named palette from \code{build_fg_palette}
##' @return Named vector of colors
get_fg_colors <- function(fg_names, palette = NULL) {
  fg_names <- as.character(fg_names)
  if (is.null(palette)) {
    return(build_fg_palette(fg_names))
  }
  cols <- unname(palette[fg_names])
  missing <- is.na(cols)
  if (any(missing)) {
    extra <- build_fg_palette(fg_names[missing])
    cols[missing] <- unname(extra[fg_names[missing]])
  }
  names(cols) <- fg_names
  cols
}

##' Get prey colors from the named functional-group palette
##' @param prey_names Prey group codes
##' @param palette Named palette from \code{build_fg_palette}
##' @return Named vector of colors
get_prey_colors <- function(prey_names, palette = NULL) {
  get_fg_colors(prey_names, palette)
}

##' Get predator colors from the named functional-group palette
##' @param predator_names Predator group codes
##' @param palette Named palette from \code{build_fg_palette}
##' @return Named vector of colors
get_predator_colors <- function(predator_names, palette = NULL) {
  get_fg_colors(predator_names, palette)
}

##' Get primary producer colors
##' @param n Number of colors
##' @return Vector of colors
get_pp_colors <- function(n) {
  base_colors <- c(
    RColorBrewer::brewer.pal(9, "Reds")[4:9],
    RColorBrewer::brewer.pal(9, "Purples")[4:9],
    RColorBrewer::brewer.pal(9, "Blues")[4:9]
  )
  grDevices::colorRampPalette(base_colors)(n)
}

##' Get catch/fishery colors (cohort series, not functional groups)
##' @param n Number of colors
##' @return Vector of colors
get_catch_colors <- function(n) {
  base_colors <- c('#241309','#292B15','#7A6F42','#56471E',
                   '#562F0E','#BA9B5B','#844D14','#A16F26')
  if (n <= length(base_colors)) {
    base_colors[1:n]
  } else {
    grDevices::colorRampPalette(base_colors)(n)
  }
}

##' Get mortality colors
##' @param n Number of colors
##' @return Vector of colors
get_mortality_colors <- function(n) {
  grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(n)
}
