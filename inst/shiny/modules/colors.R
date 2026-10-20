## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   Deterministic colours keyed by Code      ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

.okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#000000"
)

.tableau_10 <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
)

.brewer_paired <- function() {
  RColorBrewer::brewer.pal(12, "Paired")
}

.qualitative_pool <- function() {
  unique(c(.okabe_ito, .tableau_10, .brewer_paired(),
           RColorBrewer::brewer.pal(8, "Dark2"),
           RColorBrewer::brewer.pal(8, "Set2")))
}

.family_pools <- list(
  fish = c("#0B3C5D", "#328CC1", "#1D7874", "#56B4E9", "#0072B2",
           "#134074", "#4C9F70", "#5FA8D3", "#009E73", "#4E79A7"),
  warm = c("#D55E00", "#C46D5E", "#E69F00", "#A33B20", "#CC79A7",
           "#B56576", "#E07A5F", "#F28E2B", "#E15759", "#9C755F"),
  invert = c("#1D7874", "#2A9D8F", "#40916C", "#52B788", "#74C69D",
             "#76B7B2", "#95D5B2", "#2D6A4F", "#0081A7", "#3D5A80"),
  producer = c("#C4A35A", "#E9C46A", "#B7B35A", "#A7C957", "#BCBD8B",
               "#D4A373", "#F0E442", "#EDC948", "#A7C4A0", "#C9A227"),
  detritus = c("#6C757D", "#8D99AE", "#ADB5BD", "#495057", "#9A8C98",
               "#BAB0AC", "#7D6E83", "#5C6B73")
)

grouptype_family <- function(grouptypes) {
  g <- toupper(trimws(as.character(grouptypes)))
  fam <- rep("invert", length(g))
  fam[g %in% c("FISH", "SHARK", "FISH_INVERT")] <- "fish"
  fam[g %in% c("MAMMAL", "BIRD", "REPTILE")] <- "warm"
  fam[g %in% c("LG_PHY", "SM_PHY", "PHYTOBEN", "PHYTOBENTHOS", "DINOFLAG",
               "SEAGRASS", "TURF", "MICROPHYTOBENTHOS", "MICROPHTYBENTHOS",
               "LG_PHYTOPLANKTON", "SM_PHYTOPLANKTON")] <- "producer"
  fam[g %in% c("LAB_DET", "REF_DET", "CARRION", "BACT", "SED_LAB_DET",
               "SED_REF_DET", "PL_BACT", "SED_BACT", "LABILE_DET",
               "REFRACTORY_DET", "DETRITUS", "BACTERIA")] <- "detritus"
  fam
}

cycle_pool <- function(n, pool) {
  if (n <= 0) return(character(0))
  if (n <= length(pool)) return(pool[seq_len(n)])
  extra <- grDevices::colorRampPalette(pool)(n - length(pool))
  c(pool, extra)
}

##' Build one named colour palette keyed by functional-group Code
##'
##' Colours follow groups-CSV order. When GroupType is supplied, hues stay
##' inside a family (fish blues, mammals/birds warm, inverts green, producers
##' yellow/olive, detritus grey). The same Code always maps to the same colour
##' for a given CSV.
##' @param codes Character vector of group codes
##' @param grouptypes Optional parallel GroupType values
##' @return Named character vector of hex colours
build_fg_palette <- function(codes, grouptypes = NULL) {
  codes <- trimws(as.character(codes))
  keep <- !is.na(codes) & nzchar(codes)
  codes <- codes[keep]
  if (!is.null(grouptypes)) {
    grouptypes <- as.character(grouptypes)[keep]
  }
  uniq <- !duplicated(codes)
  codes <- codes[uniq]
  if (!is.null(grouptypes)) grouptypes <- grouptypes[uniq]
  n <- length(codes)
  if (n == 0) {
    return(setNames(character(0), character(0)))
  }

  pal <- character(n)
  names(pal) <- codes

  if (is.null(grouptypes) || length(grouptypes) != n) {
    pal[] <- cycle_pool(n, .qualitative_pool())
    return(pal)
  }

  fam <- grouptype_family(grouptypes)
  for (family in unique(fam)) {
    idx <- which(fam == family)
    pool <- .family_pools[[family]]
    if (is.null(pool)) pool <- .qualitative_pool()
    pal[idx] <- cycle_pool(length(idx), pool)
  }
  pal
}

get_palette <- function(n) {
  cycle_pool(n, .qualitative_pool())
}

get_predation_colors <- function() {
  c("#0B3C5D", "#1D7874", "#C4A35A", "#A33B20")
}

get_comparison_colors <- function() {
  c(Current = "firebrick3", Previous = "darkolivegreen")
}

get_foodweb_colors <- function() {
  c("#1D7874", "#C46D5E")
}

get_fg_colors <- function(fg_names, palette = NULL) {
  fg_names <- as.character(fg_names)
  if (is.null(palette)) {
    return(build_fg_palette(fg_names))
  }
  cols <- unname(palette[fg_names])
  missing <- is.na(cols)
  if (any(missing)) {
    extra <- build_fg_palette(fg_names[missing])
    cols[missing] <- unname(extra[match(fg_names[missing], names(extra))])
  }
  names(cols) <- fg_names
  cols
}

get_prey_colors <- function(prey_names, palette = NULL) {
  get_fg_colors(prey_names, palette)
}

get_predator_colors <- function(predator_names, palette = NULL) {
  get_fg_colors(predator_names, palette)
}

get_pp_colors <- function(n) {
  cycle_pool(n, .family_pools$producer)
}

get_catch_colors <- function(n) {
  cycle_pool(n, c("#0B3C5D", "#1D7874", "#C4A35A", "#A33B20",
                  "#328CC1", "#5C6B73", "#4C9F70", "#C46D5E"))
}

get_mortality_colors <- function(n) {
  cycle_pool(n, RColorBrewer::brewer.pal(12, "Paired"))
}

mortality_type_colors <- function() {
  c(
    "Fishing" = "#0B3C5D",
    "Natural" = "#C4A35A",
    "Predation" = "#A33B20",
    "Fishing Mortality" = "#0B3C5D",
    "Natural Mortality" = "#C4A35A",
    "Predation Mortality" = "#A33B20"
  )
}
