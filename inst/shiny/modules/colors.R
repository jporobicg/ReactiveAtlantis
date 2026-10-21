## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   Deterministic colours keyed by Code      ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

.okabe_ito <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442",
  "#0072B2", "#D55E00", "#CC79A7", "#000000"
)

.paul_tol_bright <- c(
  "#4477AA", "#EE6677", "#228833", "#CCBB44",
  "#66CCEE", "#AA3377", "#BBBBBB"
)

.paul_tol_muted <- c(
  "#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
  "#DDCC77", "#CC6677", "#882255", "#AA4499"
)

.tableau_10 <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC"
)

.brewer_paired <- function() {
  RColorBrewer::brewer.pal(12, "Paired")
}

##' Colourblind-friendly qualitative pool (Okabe-Ito, Tol, Tableau, Brewer).
.qualitative_pool <- function() {
  unique(c(
    .okabe_ito[c(1, 2, 3, 5, 6, 7)],
    .paul_tol_bright,
    .tableau_10,
    .paul_tol_muted,
    RColorBrewer::brewer.pal(8, "Dark2"),
    .brewer_paired(),
    RColorBrewer::brewer.pal(8, "Set2"),
    .okabe_ito[c(4, 8)]
  ))
}

.family_pools <- list(
  fish = c("#0072B2", "#E69F00", "#009E73", "#D55E00", "#56B4E9",
           "#CC79A7", "#4E79A7", "#F28E2B", "#117733", "#AA3377",
           "#0B3C5D", "#E15759", "#76B7B2", "#EDC948", "#332288"),
  warm = c("#D55E00", "#CC79A7", "#E69F00", "#A33B20", "#F28E2B",
           "#882255", "#E07A5F", "#AA4499", "#C46D5E", "#9C755F"),
  invert = c("#1D7874", "#6A4C93", "#2A9D8F", "#332288", "#40916C",
             "#0081A7", "#76B7B2", "#117733", "#44AA99", "#3D5A80"),
  producer = c("#C4A35A", "#117733", "#E69F00", "#44AA99", "#A7C957",
               "#B35806", "#F0E442", "#66A61E", "#D4A373", "#999933"),
  detritus = c("#4D4D4D", "#8D99AE", "#332288", "#6C757D", "#BAB0AC",
               "#5C6B73", "#7D6E83", "#ADB5BD")
)

##' Weighted RGB distance (no extra packages).
colour_rgb_dist <- function(a, b) {
  ra <- as.numeric(grDevices::col2rgb(a))
  rb <- as.numeric(grDevices::col2rgb(b))
  rmean <- (ra[1] + rb[1]) / 2
  sqrt((2 + rmean / 256) * (ra[1] - rb[1])^2 +
         4 * (ra[2] - rb[2])^2 +
         (2 + (255 - rmean) / 256) * (ra[3] - rb[3])^2)
}

##' Pick n colours so each new one is far from those already chosen.
farthest_color_sequence <- function(n, pool, prefer = NULL) {
  pool <- unique(as.character(pool))
  pool <- pool[!is.na(pool) & nzchar(pool)]
  if (n <= 0) return(character(0))
  if (length(pool) < n) {
    extra_n <- n - length(pool)
    extra <- grDevices::hcl(
      h = seq(12, 372, length.out = extra_n + 1)[seq_len(extra_n)],
      c = 62, l = 48
    )
    pool <- unique(c(pool, extra))
  }
  start <- if (!is.null(prefer) && prefer[1] %in% pool) prefer[1] else pool[1]
  chosen <- start
  rest <- setdiff(pool, chosen)
  while (length(chosen) < n && length(rest) > 0) {
    dmin <- vapply(rest, function(col) {
      min(vapply(chosen, function(x) colour_rgb_dist(col, x), numeric(1)))
    }, numeric(1))
    if (!is.null(prefer) && length(prefer) > 0) {
      dmin <- dmin + ifelse(rest %in% prefer, 16, 0)
    }
    pick <- which.max(dmin)[1]
    chosen <- c(chosen, rest[pick])
    rest <- rest[-pick]
  }
  chosen[seq_len(min(n, length(chosen)))]
}

##' Reorder a pool so neighbours in CSV order are far apart in hue
spread_pool <- function(n, pool) {
  farthest_color_sequence(n, pool)
}

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
##' Colours follow groups-CSV order. Assignment is a farthest-colour walk
##' over a colourblind qualitative pool so adjacent / co-plotted series stay
##' distinguishable. GroupType is a light preference only (not a blue-only
##' fish family). The same Code always maps to the same colour for a given CSV.
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
  pool <- .qualitative_pool()
  fam <- if (is.null(grouptypes) || length(grouptypes) != n) {
    NULL
  } else {
    grouptype_family(grouptypes)
  }

  used <- character(0)
  for (i in seq_len(n)) {
    prefer <- if (!is.null(fam)) .family_pools[[fam[i]]] else NULL
    rest <- setdiff(pool, used)
    if (length(rest) == 0) {
      rest <- grDevices::hcl(h = (length(used) * 47) %% 360, c = 62, l = 48)
    }
    if (length(used) == 0) {
      pick <- if (!is.null(prefer) && prefer[1] %in% rest) prefer[1] else rest[1]
    } else {
      dmin <- vapply(rest, function(col) {
        min(vapply(used, function(x) colour_rgb_dist(col, x), numeric(1)))
      }, numeric(1))
      bonus <- if (!is.null(prefer) && length(prefer) > 0) {
        ifelse(rest %in% prefer, 16, 0)
      } else {
        0
      }
      pick <- rest[which.max(dmin + bonus)[1]]
    }
    pal[i] <- pick
    used <- c(used, pick)
  }
  pal
}

##' Order series so neighbouring stack/legend items have distant hues.
spread_series_levels <- function(series, pal) {
  series <- unique(as.character(series))
  special <- intersect(c("Other", "Total"), series)
  main <- setdiff(series, special)
  if (length(main) <= 1) return(c(main, special))
  cols <- unname(pal[main])
  ok <- !is.na(cols) & nzchar(cols)
  main_ok <- main[ok]
  main_miss <- main[!ok]
  if (length(main_ok) <= 1) return(c(main, special))
  hsv <- grDevices::rgb2hsv(grDevices::col2rgb(cols[ok]))
  ord <- main_ok[order(hsv[1, ], hsv[3, ])]
  if (length(ord) > 2) {
    idx <- c(seq(1L, length(ord), by = 2L), seq(2L, length(ord), by = 2L))
    ord <- ord[idx]
  }
  c(ord, main_miss, special)
}

get_palette <- function(n) {
  farthest_color_sequence(n, .qualitative_pool())
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
