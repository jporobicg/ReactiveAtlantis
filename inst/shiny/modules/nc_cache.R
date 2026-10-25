## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~  Path+mtime cache for NetCDF and DietCheck  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

.ra_cache <- new.env(parent = emptyenv())

ra_cache_close_entry <- function(key) {
  v <- get(key, envir = .ra_cache, inherits = FALSE)
  if (inherits(v, "ncdf4")) try(ncdf4::nc_close(v), silent = TRUE)
  rm(list = key, envir = .ra_cache)
}

ra_cache_clear <- function() {
  for (k in ls(envir = .ra_cache, all.names = TRUE)) ra_cache_close_entry(k)
  invisible(NULL)
}

##' Keep only cache entries for the given files; close other NetCDF handles
##' and drop their arrays. Called on every Load so a session holds at most the
##' loaded runs (validation S03: handles 1 -> 6 and RSS 747 -> 1406 MB over
##' reloads). Returns the number of entries dropped.
ra_cache_retain <- function(paths) {
  keep <- unique(stats::na.omit(as.character(unlist(paths))))
  keep <- keep[nzchar(keep)]
  keys <- ls(envir = .ra_cache, all.names = TRUE)
  dropped <- 0L
  for (k in keys) {
    hit <- any(vapply(keep, function(p) grepl(p, k, fixed = TRUE), logical(1)))
    if (!hit) {
      ra_cache_close_entry(k)
      dropped <- dropped + 1L
    }
  }
  invisible(dropped)
}

file_cache_key <- function(path, extra = "") {
  path <- if (is.null(path) || length(path) < 1) NA_character_ else as.character(path)[1]
  mt <- NA_real_
  sz <- NA_real_
  if (!is.na(path) && nzchar(path) && file.exists(path)) {
    mt <- as.numeric(file.mtime(path))
    sz <- as.numeric(file.size(path))
  }
  paste(path, mt, sz, extra, sep = "|")
}

ra_cache_get <- function(key) {
  if (exists(key, envir = .ra_cache, inherits = FALSE)) {
    get(key, envir = .ra_cache, inherits = FALSE)
  } else {
    NULL
  }
}

ra_cache_set <- function(key, value) {
  assign(key, value, envir = .ra_cache)
  value
}

ra_timed <- function(label, expr) {
  if (!isTRUE(getOption("reactiveatlantis.timing", FALSE))) return(force(expr))
  t <- system.time(out <- force(expr), gcFirst = FALSE)
  message(sprintf("[ra-time] %s %.3fs", label, t[["elapsed"]]))
  out
}

##' Open a NetCDF once per path+mtime (readunlim = FALSE so open stays cheap)
nc_handle <- function(path) {
  path <- as_path(path)
  if (!path_ok(path)) return(NULL)
  key <- paste0("handle|", file_cache_key(path))
  h <- ra_cache_get(key)
  if (!is.null(h) && inherits(h, "ncdf4")) return(h)
  h <- tryCatch(ncdf4::nc_open(path, readunlim = FALSE), error = function(e) NULL)
  if (!is.null(h)) ra_cache_set(key, h)
  h
}

##' Cached ncvar_get keyed by path, mtime, variable and start/count
nc_get <- function(nc, varname, start = NA, count = NA, collapse_degen = TRUE) {
  if (is.null(nc) || is.null(varname) || !nzchar(as.character(varname)[1])) {
    return(NULL)
  }
  varname <- as.character(varname)[1]
  path <- tryCatch(nc$filename, error = function(e) "anon")
  key <- paste("var", file_cache_key(path, varname),
               paste(start, collapse = ","),
               paste(count, collapse = ","),
               collapse_degen, sep = "|")
  hit <- ra_cache_get(key)
  if (!is.null(hit)) return(hit)
  arr <- tryCatch({
    if (length(start) == 1L && is.na(start)[1]) {
      ncdf4::ncvar_get(nc, varname, collapse_degen = collapse_degen)
    } else {
      ncdf4::ncvar_get(nc, varname, start = start, count = count,
                       collapse_degen = collapse_degen)
    }
  }, error = function(e) NULL)
  if (!is.null(arr)) ra_cache_set(key, arr)
  arr
}

##' One time index without reading the full t axis
nc_get_at_t <- function(nc, varname, t_idx) {
  v <- nc$var[[varname]]
  if (is.null(v)) return(NULL)
  d <- as.integer(v$size)
  if (length(d) < 1) return(NULL)
  nd <- length(d)
  nt <- d[nd]
  t_idx <- as.integer(t_idx)[1]
  if (!is.finite(t_idx) || t_idx < 1 || t_idx > nt) return(NULL)
  start <- rep(1L, nd)
  start[nd] <- t_idx
  count <- d
  count[nd] <- 1L
  nc_get(nc, varname, start, count, collapse_degen = FALSE)
}

##' First and last time slices without reading the full t axis
nc_get_time_ends <- function(nc, varname) {
  v <- nc$var[[varname]]
  if (is.null(v)) return(NULL)
  d <- as.integer(v$size)
  if (length(d) < 1) return(NULL)
  nd <- length(d)
  nt <- d[nd]
  if (!is.finite(nt) || nt < 1) return(NULL)
  start1 <- rep(1L, nd)
  count1 <- d
  count1[nd] <- 1L
  startN <- start1
  startN[nd] <- nt
  list(
    first = nc_get(nc, varname, start1, count1, collapse_degen = FALSE),
    last = nc_get(nc, varname, startN, count1, collapse_degen = FALSE),
    n_t = nt
  )
}

diet_cached <- function(path) {
  path <- as_path(path)
  if (!path_ok(path)) return(NULL)
  key <- paste0("diet|", file_cache_key(path))
  hit <- ra_cache_get(key)
  if (!is.null(hit)) return(hit)
  raw <- tryCatch(read_space_table(path), error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  agg <- tryCatch(preaggregate_diet(raw), error = function(e) NULL)
  if (!is.null(agg)) ra_cache_set(key, agg)
  agg
}

ensure_inf_box <- function(dat, paths = NULL) {
  if (!is.null(dat$inf_box)) return(dat)
  paths <- normalize_paths(if (is.null(paths)) dat$paths else paths)
  if (is.null(dat$nc_out) || !inherits(dat$nc_out, "ncdf4")) {
    dat$nc_out <- nc_handle(paths$output_nc)
  }
  bgm <- as_path(paths$bgm)
  if (path_ok(bgm) && !is.null(dat$nc_out)) {
    depths <- infer_cum_depths(dat$nc_out)
    if (!is.null(depths)) {
      dat$inf_box <- tryCatch(boxes.prop(bgm, depths), error = function(e) NULL)
    }
    dat$bgm_path <- bgm
  } else if (path_ok(bgm)) {
    dat$bgm_path <- bgm
  }
  if (is.null(dat$inf_box) && !is.null(dat$nc_out)) {
    n_box <- if (!is.null(dat$nc_out$dim$b)) dat$nc_out$dim$b$len else 1
    n_z <- if (!is.null(dat$nc_out$dim$z)) dat$nc_out$dim$z$len else 2
    dat$inf_box <- dummy_inf_box(n_box, n_z)
  }
  dat
}

##' Open only the heavy objects a tab asks for; handles are cached
ensure_project_data <- function(snap, need = character()) {
  snap <- project_snapshot(snap)
  dat <- snap$data
  if (is.null(dat)) dat <- list(paths = snap$paths)
  paths <- normalize_paths(if (!is.null(dat$paths)) dat$paths else snap$paths)
  dat$paths <- paths
  map <- c(
    nc_out = "output_nc",
    nc_prod = "prod_nc",
    nc_catch = "catch_nc",
    nc_init = "init_nc",
    nc_old = "output_nc_old"
  )
  pending <- character()
  for (fld in names(map)) {
    if (!(fld %in% need || map[[fld]] %in% need)) next
    if (is.null(dat[[fld]]) || !inherits(dat[[fld]], "ncdf4")) {
      if (path_ok(paths[[map[[fld]]]])) pending <- c(pending, fld)
    }
  }
  open_needed <- function() {
    n_open <- max(1L, length(pending))
    for (fld in names(map)) {
      if (!(fld %in% need || map[[fld]] %in% need)) next
      if (is.null(dat[[fld]]) || !inherits(dat[[fld]], "ncdf4")) {
        lab <- unname(as.character(FILE_LABELS[[map[[fld]]]]))
        if (is.na(lab) || !nzchar(lab)) lab <- fld
        ra_inc(0.7 / n_open, detail = paste("Opening", lab))
        dat[[fld]] <<- nc_handle(paths[[map[[fld]]]])
      }
      if (identical(fld, "nc_out") && !is.null(dat$nc_out)) {
        if (is.null(dat$time_dates)) {
          dat$time_dates <<- tryCatch(time_calc(dat$nc_out), error = function(e) NULL)
        }
        if (is.null(dat$origin) || (length(dat$origin) == 1 && is.na(dat$origin))) {
          dat$origin <<- tryCatch(nc_time_origin(dat$nc_out), error = function(e) as.Date(NA))
        }
      }
    }
    if ("diet" %in% need || "dietcheck" %in% need) {
      ra_inc(0.1, detail = "Reading DietCheck")
      dat <<- ensure_diet(dat, paths)
    }
    if ("inf_box" %in% need) {
      ra_inc(0.1, detail = "Reading box geometry")
      dat <<- ensure_inf_box(dat, paths)
    }
    dat
  }
  if (length(pending) > 0 || "diet" %in% need || "dietcheck" %in% need ||
      "inf_box" %in% need) {
    ra_progress("Reading NetCDF…", open_needed())
  } else {
    open_needed()
  }
}

## Project data with output.nc opened (handle, origin, time_dates). The
## snapshot from project_snapshot() leaves these NULL until a tab asks for
## them, so tabs that only read ps()$data saw no time origin and the Overview
## cards showed "-" (validation D21). Falls back to the plain snapshot.
ra_with_nc_out <- function(ps) {
  tryCatch(ensure_project_data(ps(), "nc_out"), error = function(e) ps()$data)
}

ra_run_origin <- function(ps) {
  o <- tryCatch(ra_with_nc_out(ps)$origin, error = function(e) NULL)
  if (is.null(o)) NA else o
}
