## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~        Deterministic Color Palettes       ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' Seed for reproducible color assignment
COLOR_SEED <- 42

##' Get a fixed color palette for functional groups
##' @param n Number of colors needed
##' @return Vector of hex colors
get_palette <- function(n) {
  base_colors <- c(
    RColorBrewer::brewer.pal(8, "Dark2"),
    c("#000000", "#E69F00", "#56B4E9", "#009E73", 
      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  )
  colorRampPalette(base_colors)(n)
}

##' Get predation colors (fixed palette for predation plots)
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

##' Get functional group colors with deterministic assignment
##' @param fg_names Character vector of functional group names
##' @return Named vector of colors
get_fg_colors <- function(fg_names) {
  set.seed(COLOR_SEED)
  n <- length(fg_names)
  
  base_colors <- c(
    RColorBrewer::brewer.pal(9, "BuPu")[2:9],
    RColorBrewer::brewer.pal(9, "BrBG")[1:3],
    RColorBrewer::brewer.pal(9, "OrRd")[2:9]
  )
  
  colors <- colorRampPalette(base_colors)(n)
  names(colors) <- fg_names
  colors
}

##' Get prey colors for predation plots
##' @param n Number of colors
##' @return Vector of colors
get_prey_colors <- function(n) {
  base_colors <- c(
    RColorBrewer::brewer.pal(9, "BrBG")[1:3],
    RColorBrewer::brewer.pal(9, "OrRd")[2:9]
  )
  colorRampPalette(base_colors)(n)
}

##' Get predator colors for predation plots
##' @param n Number of colors
##' @return Vector of colors
get_predator_colors <- function(n) {
  base_colors <- c(
    RColorBrewer::brewer.pal(9, "YlGnBu")[1:3],
    RColorBrewer::brewer.pal(9, "BuPu")[2:9]
  )
  colorRampPalette(base_colors)(n)
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
  colorRampPalette(base_colors)(n)
}

##' Get catch/fishery colors
##' @param n Number of colors
##' @return Vector of colors
get_catch_colors <- function(n) {
  base_colors <- c('#241309','#292B15','#7A6F42','#56471E',
                   '#562F0E','#BA9B5B','#844D14','#A16F26')
  if (n <= length(base_colors)) {
    base_colors[1:n]
  } else {
    colorRampPalette(base_colors)(n)
  }
}

##' Get mortality colors
##' @param n Number of colors
##' @return Vector of colors
get_mortality_colors <- function(n) {
  colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(n)
}
