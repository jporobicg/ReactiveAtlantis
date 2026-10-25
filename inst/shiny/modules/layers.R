## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~  Atlantis water-column / sediment layers   ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## NetCDF z is stored deepest water → surface, then sediment last
## (Javier's viewer arrange_layers(): c(rev(1:water), sediment)).
## Shallow boxes pad unused water at the deep end (start of z). Those
## padded slots must be dropped, not plotted as zeros. Per-box water
## counts come from numlayers (init/output), nlayers (output), BGM
## Layers, or non-zero nominal_dz water cells — never the raw z length.

box_water_layer_counts <- function(nc, inf_box = NULL) {
  n_z <- if (!is.null(nc$dim$z)) as.integer(nc$dim$z$len) else 1L
  n_b <- if (!is.null(nc$dim$b)) as.integer(nc$dim$b$len) else 1L
  n_water_max <- if (n_z >= 2L) n_z - 1L else max(1L, n_z)
  fallback <- rep(n_water_max, n_b)

  as_counts <- function(vec) {
    vec <- as.integer(as.numeric(vec))
    vec[!is.finite(vec)] <- n_water_max
    if (length(vec) < n_b) vec <- c(vec, rep(n_water_max, n_b - length(vec)))
    pmin(pmax(vec[seq_len(min(length(vec), n_b))], 0L), n_water_max)
  }

  collapse_box_counts <- function(nl) {
    if (is.null(nl)) return(NULL)
    d <- dim(nl)
    if (is.null(d) || length(d) == 1L) return(as_counts(nl))
    if (length(d) >= 3L) {
      nl <- nl[, , 1]
      d <- dim(nl)
      if (is.null(d)) return(as_counts(nl))
    }
    if (length(d) == 2L) {
      if (d[1] == n_b) return(as_counts(nl[, 1]))
      if (d[2] == n_b) {
        return(as_counts(apply(nl, 2, function(x) {
          x <- x[is.finite(x)]
          if (!length(x)) n_water_max else max(x)
        })))
      }
      return(as_counts(nl[, 1]))
    }
    as_counts(nl)
  }

  vars <- names(nc$var)
  for (vn in c("numlayers", "nlayers")) {
    hit <- vars[tolower(vars) == tolower(vn)]
    if (!length(hit)) next
    got <- collapse_box_counts(nc_get(nc, hit[1]))
    if (!is.null(got)) return(got)
  }
  if (!is.null(inf_box) && is.list(inf_box$info) &&
      "Layers" %in% names(inf_box$info)) {
    return(as_counts(inf_box$info$Layers))
  }
  dz <- tryCatch(layer_thickness(nc), error = function(e) NULL)
  if (!is.null(dz) && !is.null(dz$array)) {
    arr <- dz$array
    if (length(dim(arr)) >= 2) {
      if (!is.null(n_z) && ncol(arr) == n_z && nrow(arr) != n_z) arr <- t(arr)
      n_take <- min(n_water_max, nrow(arr))
      water <- arr[seq_len(n_take), , drop = FALSE]
      cnt <- apply(water, 2, function(x) sum(is.finite(x) & abs(x) > 0))
      return(as_counts(cnt))
    }
  }
  fallback
}

atlantis_layer_info <- function(nc, inf_box = NULL) {
  n_z <- if (!is.null(nc$dim$z)) as.integer(nc$dim$z$len) else 1L
  n_b <- if (!is.null(nc$dim$b)) as.integer(nc$dim$b$len) else 1L
  if (!is.finite(n_z) || n_z < 1L) n_z <- 1L
  if (!is.finite(n_b) || n_b < 1L) n_b <- 1L
  has_sediment <- n_z >= 2L
  n_water_max <- if (has_sediment) n_z - 1L else n_z
  n_water <- box_water_layer_counts(nc, inf_box)
  water_keys <- paste0("water_", seq_len(max(1L, n_water_max)))
  if (n_water_max <= 1L) {
    water_labs <- "Water column"
  } else {
    mid <- if (n_water_max > 2L) paste("Water layer", seq.int(2L, n_water_max - 1L)) else character()
    water_labs <- c("Surface", mid, "Deepest water")
  }
  list(
    n_z = n_z,
    n_box = n_b,
    has_sediment = isTRUE(has_sediment),
    n_water_max = as.integer(n_water_max),
    n_water = as.integer(n_water),
    water_keys = water_keys[seq_len(n_water_max)],
    water_labs = water_labs[seq_len(n_water_max)]
  )
}

layer_choices <- function(info) {
  ch <- c("All layers (integrated)" = "integrated")
  if (!is.null(info) && info$n_water_max > 0) {
    ch <- c(ch, stats::setNames(info$water_keys, info$water_labs))
  }
  if (isTRUE(info$has_sediment)) {
    ch <- c(ch, Sediment = "sediment")
  }
  ch
}

layer_label <- function(info, layer_key) {
  if (is.null(layer_key) || identical(layer_key, "integrated")) {
    return("All layers (integrated)")
  }
  if (identical(layer_key, "sediment")) return("Sediment")
  i <- match(layer_key, info$water_keys)
  if (!is.na(i)) return(info$water_labs[[i]])
  layer_key
}

##' 1-based storage z for a display key; NA for integrated
layer_storage_z <- function(info, layer_key) {
  if (identical(layer_key, "sediment")) return(as.integer(info$n_z))
  if (is.character(layer_key) && grepl("^water_", layer_key)) {
    k <- suppressWarnings(as.integer(sub("^water_", "", layer_key)))
    if (is.finite(k) && k >= 1L && k <= info$n_water_max) {
      return(as.integer(info$n_water_max - k + 1L))
    }
  }
  NA_integer_
}

box_has_storage_z <- function(info, box_i, z) {
  box_i <- as.integer(box_i)[1]
  z <- as.integer(z)[1]
  if (!is.finite(box_i) || box_i < 1 || box_i > info$n_box) return(FALSE)
  if (!is.finite(z) || z < 1) return(FALSE)
  if (isTRUE(info$has_sediment) && z == info$n_z) return(TRUE)
  nw <- info$n_water[box_i]
  if (!is.finite(nw) || nw < 1) return(FALSE)
  first_water <- info$n_water_max - nw + 1L
  z >= first_water && z <= info$n_water_max
}

##' Set padded (unused) water cells to NA. Array is [z, b] or [z, b, t].
mask_padded_layers <- function(arr, info) {
  if (is.null(arr) || is.null(info)) return(arr)
  d <- dim(arr)
  if (is.null(d) || length(d) < 2) return(arr)
  n_z <- d[1]
  n_b <- d[2]
  for (b in seq_len(min(n_b, info$n_box))) {
    for (z in seq_len(n_z)) {
      if (box_has_storage_z(info, b, z)) next
      if (length(d) == 2) {
        arr[z, b] <- NA_real_
      } else {
        arr[z, b, ] <- NA_real_
      }
    }
  }
  arr
}

##' Human label for a 1-based storage z index
storage_z_label <- function(info, z) {
  z <- as.integer(z)[1]
  if (isTRUE(info$has_sediment) && z == info$n_z) return("Sediment")
  k <- info$n_water_max - z + 1L
  if (is.finite(k) && k >= 1L && k <= info$n_water_max) {
    return(info$water_labs[[k]])
  }
  paste("Layer", z - 1L)
}

##' Display order matching arrange_layers(): surface → deep water, sediment last.
layer_lab_levels <- function(info) {
  labs <- if (!is.null(info)) info$water_labs else character()
  if (isTRUE(info$has_sediment)) labs <- c(labs, "Sediment")
  labs
}

##' Storage-z order used by Javier's arrange_layers() (surface first, sediment last).
viewer_layer_storage_order <- function(info) {
  water <- if (info$n_water_max > 0) rev(seq_len(info$n_water_max)) else integer()
  if (isTRUE(info$has_sediment)) c(water, as.integer(info$n_z)) else water
}

##' Long table of nominal_dz for layers that actually exist in each box.
layer_thickness_long <- function(nc, box = NULL, info = NULL, inf_box = NULL) {
  if (is.null(info)) info <- atlantis_layer_info(nc, inf_box)
  dz <- tryCatch(layer_thickness(nc), error = function(e) NULL)
  if (is.null(dz) || is.null(dz$array)) return(NULL)
  arr <- dz$array
  d <- dim(arr)
  if (is.null(d)) return(NULL)
  empty <- data.frame(Box = integer(), Layer = integer(), Time = integer(),
                      Thickness = numeric(), LayerLab = character(),
                      stringsAsFactors = FALSE)
  take_boxes <- function(n_b_use) {
    if (is.null(box)) seq_len(n_b_use) - 1L else as.integer(box)
  }
  rows <- list()
  if (length(d) == 2) {
    if (nrow(arr) != info$n_z && ncol(arr) == info$n_z) arr <- t(arr)
    n_z_use <- nrow(arr)
    n_b_use <- ncol(arr)
    for (b in take_boxes(n_b_use)) {
      b1 <- as.integer(b) + 1L
      if (b1 < 1 || b1 > n_b_use) next
      for (z in seq_len(n_z_use)) {
        if (!box_has_storage_z(info, b1, z)) next
        rows[[length(rows) + 1]] <- data.frame(
          Box = b, Layer = z - 1L, Time = 1L,
          Thickness = as.numeric(arr[z, b1]),
          LayerLab = storage_z_label(info, z),
          stringsAsFactors = FALSE
        )
      }
    }
  } else if (length(d) == 3) {
    if (d[1] != info$n_z && d[2] == info$n_z) arr <- aperm(arr, c(2, 1, 3))
    n_z_use <- dim(arr)[1]
    n_b_use <- dim(arr)[2]
    n_t <- dim(arr)[3]
    for (b in take_boxes(n_b_use)) {
      b1 <- as.integer(b) + 1L
      if (b1 < 1 || b1 > n_b_use) next
      for (z in seq_len(n_z_use)) {
        if (!box_has_storage_z(info, b1, z)) next
        rows[[length(rows) + 1]] <- data.frame(
          Box = b, Layer = z - 1L, Time = seq_len(n_t),
          Thickness = as.numeric(arr[z, b1, ]),
          LayerLab = storage_z_label(info, z),
          stringsAsFactors = FALSE
        )
      }
    }
  } else {
    return(NULL)
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

##' 3D cell biomass [z, b, t] in tonnes, or NULL if the group is 2D
biomass_cell_array <- function(nc, grp, fg_code, mg2t = 0.00000002, x.cn = 5.7) {
  row <- grp[grp$code == fg_code, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  vars <- names(nc$var)
  n.coh <- as.integer(row$numcohorts[1])
  gtype <- as.character(row$grouptype[1])
  name <- as.character(row$name[1])
  acc <- NULL
  if (n.coh > 1 && !gtype %in% AGE_SPECIAL) {
    for (coh in seq_len(n.coh)) {
      n_var <- paste0(name, coh, "_Nums")
      r_var <- paste0(name, coh, "_ResN")
      s_var <- paste0(name, coh, "_StructN")
      if (!all(c(n_var, r_var, s_var) %in% vars)) next
      cell <- (nc_get(nc, r_var) + nc_get(nc, s_var)) * nc_get(nc, n_var)
      acc <- add_nc_arrays(acc, cell)
    }
    if (is.null(acc)) return(NULL)
    return(acc * mg2t * x.cn)
  }
  vol <- nc_volume(nc)
  for (vname in pool_var_names(vars, name, n.coh, gtype)) {
    biom <- nc_get(nc, vname)
    if (is.null(biom) || length(dim(biom)) < 3) next
    if (!is.null(vol) && length(dim(vol)) == 2) {
      biom <- sweep(biom, c(1, 2), vol, "*")
    } else if (!is.null(vol) && length(dim(vol)) == 3 && all(dim(biom) == dim(vol))) {
      biom <- biom * vol
    }
    acc <- add_nc_arrays(acc, biom)
  }
  if (is.null(acc)) return(NULL)
  acc * mg2t * x.cn
}

##' Biomass by box for one display layer (or integrated)
biomass_by_box_layer <- function(nc, grp, fg_code, layer_key = "integrated",
                                 info = NULL, mg2t = 0.00000002, x.cn = 5.7) {
  if (is.null(info)) info <- atlantis_layer_info(nc)
  if (is.null(layer_key) || identical(layer_key, "integrated")) {
    return(biomass_by_box(nc, grp, fg_code, mg2t, x.cn))
  }
  arr <- biomass_cell_array(nc, grp, fg_code, mg2t, x.cn)
  if (is.null(arr) || length(dim(arr)) < 3) {
    return(biomass_by_box(nc, grp, fg_code, mg2t, x.cn))
  }
  arr <- mask_padded_layers(arr, info)
  z <- layer_storage_z(info, layer_key)
  if (!is.finite(z) || z < 1 || z > dim(arr)[1]) {
    return(data.frame(Time = integer(), Box = integer(), Biomass = numeric(),
                      Code = character(), stringsAsFactors = FALSE))
  }
  sl <- arr[z, , , drop = FALSE]
  n_box <- dim(arr)[2]
  n_t <- dim(arr)[3]
  data.frame(
    Time = rep(seq_len(n_t), each = n_box),
    Box = rep(seq_len(n_box) - 1L, times = n_t),
    Biomass = as.numeric(sl),
    Code = fg_code,
    stringsAsFactors = FALSE
  )
}
