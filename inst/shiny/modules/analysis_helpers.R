## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~  Shared helpers for spatial, physics, obs   ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ACCEPT_BAND <- c(0.5, 1.5)

AGE_SPECIAL <- c("PWN", "PRAWNS", "PRAWN", "CEP", "MOB_EP_OTHER",
                 "SEAGRASS", "CORAL", "MANGROVE", "MANGROVES", "SPONGE")

time_slice <- function(arr, t) {
  d <- dim(arr)
  if (is.null(d) || length(d) == 1) {
    return(arr[t])
  }
  if (length(d) == 2) return(arr[, t, drop = TRUE])
  if (length(d) == 3) return(arr[, , t, drop = TRUE])
  arr[, , , t, drop = TRUE]
}

mean_space <- function(arr) {
  d <- dim(arr)
  if (is.null(d) || length(d) == 1) return(as.numeric(arr))
  apply(arr, length(d), mean, na.rm = TRUE)
}

sum_space <- function(arr) {
  d <- dim(arr)
  if (is.null(d) || length(d) == 1) return(as.numeric(arr))
  apply(arr, length(d), sum, na.rm = TRUE)
}

sum_layers_to_box_time <- function(arr) {
  d <- dim(arr)
  if (is.null(d)) {
    return(matrix(as.numeric(arr), nrow = 1))
  }
  if (length(d) == 2) {
    return(arr)
  }
  apply(arr, c(2, 3), sum, na.rm = TRUE)
}

code_from_fg_label <- function(fg) {
  m <- regmatches(as.character(fg), regexpr("\\(([^)]+)\\)$", as.character(fg)))
  out <- gsub("[()]", "", m)
  ifelse(nzchar(out), out, as.character(fg))
}

AGE_CLASS_VARS <- c("ResN", "StructN", "Numbers", "Biomass")

##' Subset a tracer array to a box and/or display layer (pads already NA).
age_subset_array <- function(arr, box = NULL, layer_key = "integrated", info = NULL) {
  if (is.null(arr)) return(NULL)
  d <- dim(arr)
  if (!is.null(info) && !is.null(d) && length(d) >= 2 &&
      exists("mask_padded_layers", mode = "function")) {
    arr <- mask_padded_layers(arr, info)
    d <- dim(arr)
  }
  if (!is.null(d) && length(d) == 3) {
    if (!is.null(layer_key) && !identical(layer_key, "integrated") &&
        exists("layer_storage_z", mode = "function") && !is.null(info)) {
      z <- layer_storage_z(info, layer_key)
      if (!is.finite(z) || z < 1 || z > d[1]) {
        arr[] <- NA_real_
      } else {
        arr <- arr[z, , , drop = FALSE]
      }
    }
    if (!is.null(box)) {
      b1 <- as.integer(box)[1] + 1L
      if (is.finite(b1) && b1 >= 1 && b1 <= dim(arr)[2]) {
        arr <- arr[, b1, , drop = FALSE]
      } else {
        arr[] <- NA_real_
      }
    }
    return(arr)
  }
  if (!is.null(d) && length(d) == 2 && !is.null(box)) {
    b1 <- as.integer(box)[1] + 1L
    if (is.finite(b1) && b1 >= 1 && b1 <= d[1]) {
      arr <- arr[b1, , drop = FALSE]
    } else {
      arr[] <- NA_real_
    }
  }
  arr
}

weighted_mean_space <- function(val, w) {
  if (is.null(val) || is.null(w)) return(numeric())
  d <- dim(val)
  if (is.null(d) || length(d) == 1) {
    ww <- as.numeric(w)
    vv <- as.numeric(val)
    ok <- is.finite(vv) & is.finite(ww) & ww > 0
    if (!any(ok)) return(NA_real_)
    return(sum(vv[ok] * ww[ok]) / sum(ww[ok]))
  }
  n_t <- d[length(d)]
  vapply(seq_len(n_t), function(t) {
    vv <- as.numeric(time_slice(val, t))
    ww <- as.numeric(time_slice(w, t))
    ok <- is.finite(vv) & is.finite(ww) & ww > 0
    if (!any(ok)) return(NA_real_)
    sum(vv[ok] * ww[ok]) / sum(ww[ok])
  }, numeric(1))
}

##' Age-class time series: ResN, StructN, Numbers, Biomass.
##'
##' System-wide when \code{box} is NULL and \code{layer_key} is integrated.
##' Numbers are summed; ResN/StructN are numbers-weighted means; biomass uses
##' the same \code{mg2t * x.cn} conversion as Compare / Spatial.
age_class_long <- function(nc, grp, fg_code, box = NULL,
                           layer_key = "integrated", info = NULL,
                           mg2t = 0.00000002, x.cn = 5.7) {
  empty <- data.frame(
    Time = integer(), Age = integer(), AgeLab = character(),
    Variable = character(), Value = numeric(),
    stringsAsFactors = FALSE
  )
  if (is.null(nc) || is.null(grp) || !nzchar(as.character(fg_code)[1])) {
    return(empty)
  }
  row <- grp[grp$code == fg_code, , drop = FALSE]
  if (nrow(row) == 0) return(empty)
  n.coh <- as.integer(row$numcohorts[1])
  gtype <- as.character(row$grouptype[1])
  if (!is.finite(n.coh) || n.coh < 2 || gtype %in% AGE_SPECIAL) return(empty)
  if (is.null(info) && exists("atlantis_layer_info", mode = "function")) {
    info <- tryCatch(atlantis_layer_info(nc), error = function(e) NULL)
  }
  path <- tryCatch(nc$filename, error = function(e) "anon")
  box_key <- if (is.null(box)) "all" else paste(as.integer(box), collapse = ",")
  ck <- paste("agecls", file_cache_key(path, fg_code), box_key,
              as.character(layer_key)[1], mg2t, x.cn, sep = "|")
  hit <- tryCatch(ra_cache_get(ck), error = function(e) NULL)
  if (!is.null(hit)) return(hit)

  name <- as.character(row$name[1])
  vars <- names(nc$var)
  rows <- list()
  for (coh in seq_len(n.coh)) {
    n_var <- paste0(name, coh, "_Nums")
    r_var <- paste0(name, coh, "_ResN")
    s_var <- paste0(name, coh, "_StructN")
    if (!all(c(n_var, r_var, s_var) %in% vars)) next
    nums <- age_subset_array(nc_get(nc, n_var), box, layer_key, info)
    resn <- age_subset_array(nc_get(nc, r_var), box, layer_key, info)
    strn <- age_subset_array(nc_get(nc, s_var), box, layer_key, info)
    if (is.null(nums)) next
    n_t <- if (!is.null(dim(nums))) dim(nums)[length(dim(nums))] else length(nums)
    nums_t <- as.numeric(sum_space(nums))
    resn_t <- as.numeric(weighted_mean_space(resn, nums))
    strn_t <- as.numeric(weighted_mean_space(strn, nums))
    biom_t <- as.numeric(sum_space((resn + strn) * nums)) * mg2t * x.cn
    if (length(nums_t) != n_t) n_t <- length(nums_t)
    rows[[length(rows) + 1]] <- data.frame(
      Time = rep(seq_len(n_t), 4L),
      Age = coh,
      AgeLab = paste("Age", coh),
      Variable = rep(c("ResN", "StructN", "Numbers", "Biomass"), each = n_t),
      Value = c(resn_t, strn_t, nums_t, biom_t),
      stringsAsFactors = FALSE
    )
  }
  out <- if (length(rows)) do.call(rbind, rows) else empty
  tryCatch(ra_cache_set(ck, out), error = function(e) NULL)
  out
}

age_class_ylab <- function(variable) {
  switch(as.character(variable)[1],
         ResN = "Reserve nitrogen (mg N)",
         StructN = "Structural nitrogen (mg N)",
         Numbers = "Numbers",
         Biomass = "Biomass (t)",
         "Value")
}

##' Relative StructN / ResN at the end of the run versus t0
##'
##' Age-class groups only. A cohort is a problem when the spatial-mean
##' structural or reserve nitrogen at the last time step is outside
##' \code{band} (default 0.5–1.5).
problem_cohorts <- function(nc, grp, band = ACCEPT_BAND) {
  empty <- data.frame(
    Code = character(), Cohort = integer(), Age = character(),
    Component = character(), Initial = numeric(), Final = numeric(),
    Relative = numeric(), Flag = character(), stringsAsFactors = FALSE
  )
  if (is.null(nc) || is.null(grp) || nrow(grp) == 0) return(empty)
  lo <- band[1]
  hi <- band[2]
  vars <- names(nc$var)
  age <- grp
  if ("isturnedon" %in% names(age)) age <- age[age$isturnedon == 1, , drop = FALSE]
  if ("numcohorts" %in% names(age)) {
    age <- age[age$numcohorts > 1, , drop = FALSE]
  }
  if ("grouptype" %in% names(age)) {
    age <- age[!age$grouptype %in% AGE_SPECIAL, , drop = FALSE]
  }
  if (nrow(age) == 0) return(empty)

  rows <- list()
  for (i in seq_len(nrow(age))) {
    n.coh <- as.integer(age$numcohorts[i])
    for (coh in seq_len(n.coh)) {
      for (comp in c("StructN", "ResN")) {
        vn <- paste0(age$name[i], coh, "_", comp)
        if (!vn %in% vars) next
        ends <- tryCatch(nc_get_time_ends(nc, vn), error = function(e) NULL)
        if (is.null(ends) || is.null(ends$first) || is.null(ends$last)) next
        first <- mean(as.numeric(ends$first), na.rm = TRUE)
        last <- mean(as.numeric(ends$last), na.rm = TRUE)
        ratio <- last / pmax(first, 1e-12)
        flag <- if (!is.finite(ratio)) {
          "No data"
        } else if (ratio < lo || ratio > hi) {
          "Outside band"
        } else {
          "OK"
        }
        rows[[length(rows) + 1]] <- data.frame(
          Code = as.character(age$code[i]),
          Cohort = coh,
          Age = sprintf("Age%02d", coh),
          Component = comp,
          Initial = first,
          Final = last,
          Relative = ratio,
          Flag = flag,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (length(rows) == 0) return(empty)
  do.call(rbind, rows)
}

acceptable_band_layer <- function(p, lo = 0.5, hi = 1.5, xmin, xmax) {
  p <- p + ggplot2::annotate(
    "rect", xmin = xmin, xmax = xmax, ymin = lo, ymax = hi,
    fill = "#1D7874", alpha = 0.12
  )
  p <- p + ggplot2::geom_hline(yintercept = c(lo, hi), linetype = 2,
                               colour = "#5C6B73", linewidth = 0.35)
  p
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~                 YOY.txt                     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' Read Atlantis YOY.txt (whitespace-separated; columns Time, CODE.0)
##'
##' Atlantis writes extra spaces between columns. \code{read.csv(sep = " ")}
##' then invents empty \code{X} columns and can collapse the file to one row,
##' which plots as a flat line with relative YOY of 1. This reader collapses
##' whitespace and keeps every time step.
read_yoy_table <- function(path) {
  n_data <- max(0L, length(readLines(path, warn = FALSE)) - 1L)
  raw <- tryCatch(
    data.table::fread(path, header = TRUE, strip.white = TRUE, fill = TRUE,
                      showProgress = FALSE, data.table = FALSE,
                      check.names = FALSE, blank.lines.skip = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw) || ncol(raw) < 1 || (n_data > 2 && nrow(raw) <= 1)) {
    raw <- utils::read.table(path, header = TRUE, check.names = FALSE,
                             stringsAsFactors = FALSE, comment.char = "#",
                             fill = TRUE, strip.white = TRUE)
  }
  names(raw) <- sub("^\ufeff", "", trimws(as.character(names(raw))))
  keep <- nzchar(names(raw)) & !grepl("^X\\.?[0-9]*$", names(raw)) &
    !grepl("^V[0-9]+$", names(raw))
  raw <- raw[, keep, drop = FALSE]
  empty <- vapply(raw, function(col) all(is.na(col) | !nzchar(as.character(col))),
                  logical(1))
  if (any(empty) && !all(empty)) raw <- raw[, !empty, drop = FALSE]
  if (ncol(raw) == 0) stop("YOY file has no columns.")
  tname <- names(raw)[tolower(names(raw)) %in% c("time", "t", "day", "days")]
  if (length(tname) > 0) {
    names(raw)[names(raw) == tname[1]] <- "Time"
  } else {
    raw$Time <- seq_len(nrow(raw)) - 1L
  }
  raw$Time <- as.numeric(raw$Time)
  ## Seconds since a date origin, not model days (14975 is a long run in days)
  if (length(raw$Time) > 0 && is.finite(max(raw$Time, na.rm = TRUE)) &&
      max(raw$Time, na.rm = TRUE) > 1e6) {
    raw$Time <- raw$Time / 86400
  }
  raw
}

##' Read SpecificMort / SpecificPredMort, keeping Atlantis hyphen names usable
##'
##' North Sea writes \code{COD-0-S0-M1}. \code{read.csv} would turn those into
##' dots; this reader accepts either form and normalises hyphens to dots so
##' \code{calc_mort} can match \code{COD.0.S0.M1}.
read_mort_table <- function(path) {
  raw <- tryCatch(
    data.table::fread(path, header = TRUE, strip.white = TRUE, fill = TRUE,
                      showProgress = FALSE, data.table = FALSE,
                      check.names = FALSE, blank.lines.skip = TRUE),
    error = function(e) NULL
  )
  if (is.null(raw) || ncol(raw) < 2) {
    raw <- utils::read.table(path, header = TRUE, check.names = FALSE,
                             stringsAsFactors = FALSE, comment.char = "#",
                             fill = TRUE, strip.white = TRUE)
  }
  names(raw) <- gsub("-", ".", sub("^\ufeff", "", trimws(as.character(names(raw)))))
  keep <- nzchar(names(raw)) & !grepl("^X\\.?[0-9]*$", names(raw))
  raw <- raw[, keep, drop = FALSE]
  tname <- names(raw)[tolower(names(raw)) %in% c("time", "t", "day", "days")]
  if (length(tname) > 0) names(raw)[names(raw) == tname[1]] <- "Time"
  raw
}

find_yoy_column <- function(yoy, code) {
  if (is.null(yoy) || ncol(yoy) == 0) return(NA_character_)
  nms <- trimws(names(yoy))
  nms <- nms[tolower(nms) != "time"]
  code <- trimws(as.character(code)[1])
  cands <- c(
    paste0(code, ".0"), paste0(code, "-0"), paste0(code, "_0"),
    paste0("X", code, ".0"), paste0(code, ".0.0"),
    paste0("Time.", code), paste0("Time ", code),
    code
  )
  hit <- cands[cands %in% nms]
  if (length(hit) > 0) return(hit[1])
  ci <- nms[tolower(nms) %in% tolower(cands)]
  if (length(ci) > 0) return(ci[1])
  rx <- paste0("^", gsub("([.|()\\^$*+?{}])", "\\\\\\1", code), "[._-]0$")
  g <- grep(rx, nms, ignore.case = TRUE, value = TRUE)
  if (length(g) > 0) return(g[1])
  NA_character_
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~                 Observations                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

read_observations <- function(path) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE)
  names(raw) <- tolower(names(raw))
  need <- c("code", "year", "value")
  if (!all(need %in% names(raw))) {
    stop("Observations CSV needs columns Code, Year, Value (optional Lower, Upper, Type).")
  }
  out <- data.frame(
    Code = trimws(as.character(raw$code)),
    Year = as.numeric(raw$year),
    Value = as.numeric(raw$value),
    Lower = if ("lower" %in% names(raw)) as.numeric(raw$lower) else NA_real_,
    Upper = if ("upper" %in% names(raw)) as.numeric(raw$upper) else NA_real_,
    Type = if ("type" %in% names(raw)) {
      ty <- toupper(trimws(as.character(raw$type)))
      ifelse(ty %in% c("SSB"), "SSB",
             ifelse(ty %in% c("RECRUITMENT", "YOY", "RECRUITS"), "Recruitment", "Biomass"))
    } else {
      "Biomass"
    },
    stringsAsFactors = FALSE
  )
  out$Type[out$Type %in% c("Yoy", "Recruits")] <- "Recruitment"
  out$Time <- as.Date(paste0(out$Year, "-01-01"))
  out
}

obs_empty <- function() {
  data.frame(
    Code = character(), Year = numeric(), Value = numeric(),
    Lower = numeric(), Upper = numeric(), Type = character(),
    Time = as.Date(character()), stringsAsFactors = FALSE
  )
}

obs_for <- function(obs, codes, type) {
  if (is.null(obs) || nrow(obs) == 0) return(obs_empty())
  codes <- unique(as.character(codes))
  type <- as.character(type)
  obs[obs$Code %in% codes & tolower(obs$Type) == tolower(type), , drop = FALSE]
}

add_obs_overlay <- function(p, obs, codes, type, facet_var = NULL) {
  sub <- obs_for(obs, codes, type)
  if (is.null(sub) || nrow(sub) == 0) return(p)
  has_band <- any(is.finite(sub$Lower) & is.finite(sub$Upper))
  if (has_band) {
    p <- p + ggplot2::geom_ribbon(
      data = sub,
      ggplot2::aes(x = Time, ymin = Lower, ymax = Upper),
      inherit.aes = FALSE, fill = "#C4A35A", alpha = 0.25
    )
  }
  p <- p + ggplot2::geom_point(
    data = sub,
    ggplot2::aes(x = Time, y = Value),
    inherit.aes = FALSE, colour = "#A33B20", size = 2.2
  )
  p
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~              BGM polygons                   ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

bgm_nbox <- function(lines) {
  hit <- grep("^nbox[[:space:]]+", lines)
  if (length(hit) == 0) return(NA_integer_)
  as.integer(sub("^nbox[[:space:]]+", "", lines[hit[1]]))
}

bgm_parse_xy <- function(txt) {
  nums <- suppressWarnings(as.numeric(unlist(strsplit(trimws(txt), "[[:space:]]+"))))
  nums <- nums[is.finite(nums)]
  if (length(nums) < 2) return(NULL)
  c(nums[1], nums[2])
}

##' Global vertex table: `vert <id> <x> <y>` or `vert <x> <y>`.
bgm_global_verts <- function(lines) {
  hits <- grep("^vert[[:space:]]+", lines, value = TRUE)
  if (!length(hits)) return(NULL)
  rows <- lapply(hits, function(ln) {
    rest <- sub("^vert[[:space:]]+", "", ln)
    nums <- suppressWarnings(as.numeric(unlist(strsplit(trimws(rest), "[[:space:]]+"))))
    nums <- nums[is.finite(nums)]
    if (length(nums) >= 3) {
      data.frame(id = as.integer(nums[1]), x = nums[2], y = nums[3])
    } else if (length(nums) == 2) {
      data.frame(id = NA_integer_, x = nums[1], y = nums[2])
    } else {
      NULL
    }
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  df <- do.call(rbind, rows)
  if (all(is.na(df$id))) df$id <- seq_len(nrow(df)) - 1L
  df
}

bgm_lookup_verts <- function(idx, gverts) {
  if (is.null(gverts) || !length(idx)) {
    return(data.frame(x = numeric(), y = numeric()))
  }
  m <- match(as.integer(idx), gverts$id)
  if (anyNA(m)) m <- as.integer(idx) + 1L
  ok <- is.finite(m) & m >= 1 & m <= nrow(gverts)
  if (!any(ok)) return(data.frame(x = numeric(), y = numeric()))
  data.frame(x = gverts$x[m[ok]], y = gverts$y[m[ok]])
}

bgm_box_from_faces <- function(lines, boxid) {
  iface <- grep(paste0("^box", boxid, "\\.iface[[:space:]]+"), lines, value = TRUE)
  if (!length(iface)) return(data.frame(x = numeric(), y = numeric()))
  ids <- suppressWarnings(as.integer(unlist(strsplit(
    sub(paste0("^box", boxid, "\\.iface[[:space:]]+"), "", iface[1]),
    "[[:space:]]+"))))
  ids <- ids[is.finite(ids)]
  if (!length(ids)) return(data.frame(x = numeric(), y = numeric()))
  xs <- numeric()
  ys <- numeric()
  last <- NULL
  for (f in ids) {
    p1 <- grep(paste0("^face", f, "\\.p1[[:space:]]+"), lines, value = TRUE)
    p2 <- grep(paste0("^face", f, "\\.p2[[:space:]]+"), lines, value = TRUE)
    a <- if (length(p1)) bgm_parse_xy(sub(".*\\.p1[[:space:]]+", "", p1[1])) else NULL
    b <- if (length(p2)) bgm_parse_xy(sub(".*\\.p2[[:space:]]+", "", p2[1])) else NULL
    if (!is.null(a)) {
      xs <- c(xs, a[1]); ys <- c(ys, a[2])
    }
    last <- b
  }
  if (!is.null(last)) {
    xs <- c(xs, last[1]); ys <- c(ys, last[2])
  }
  if (!length(xs)) return(data.frame(x = numeric(), y = numeric()))
  data.frame(x = xs, y = ys)
}

bgm_box_verts <- function(lines, boxid, gverts = NULL) {
  pat_xy <- paste0("^box", boxid, "\\.vert[[:space:]]+")
  xy_lines <- grep(pat_xy, lines, value = TRUE)
  if (length(xy_lines) > 0) {
    coords <- strsplit(sub(pat_xy, "", xy_lines), "[[:space:]]+")
    n1 <- suppressWarnings(as.numeric(coords[[1]]))
    n1 <- n1[is.finite(n1)]
    if (length(n1) == 1 && length(coords) == 1) {
      n <- as.integer(n1[1])
      start <- grep(pat_xy, lines)[1]
      if (is.finite(n) && n > 1) {
        block <- lines[(start + 1):(start + n)]
        mat <- do.call(rbind, lapply(strsplit(trimws(block), "[[:space:]]+"),
                                     function(v) as.numeric(v[1:2])))
        if (!is.null(dim(mat)) && ncol(mat) >= 2 && all(is.finite(mat[, 1:2]))) {
          return(data.frame(x = mat[, 1], y = mat[, 2], stringsAsFactors = FALSE))
        }
      }
    }
    lens <- vapply(coords, length, integer(1))
    if (all(lens == 1L)) {
      idx <- suppressWarnings(as.integer(vapply(coords, function(v) v[1], character(1))))
      got <- bgm_lookup_verts(idx, gverts)
      if (nrow(got) > 2) return(got)
    }
    mat <- do.call(rbind, lapply(coords, function(v) as.numeric(v[1:2])))
    if (!is.null(dim(mat)) && ncol(mat) >= 2 &&
        sum(is.finite(mat[, 1]) & is.finite(mat[, 2])) > 2) {
      return(data.frame(x = mat[, 1], y = mat[, 2], stringsAsFactors = FALSE))
    }
  }
  pat2 <- paste0("^box", boxid, "\\.vert2[[:space:]]+")
  hit2 <- grep(pat2, lines)
  if (length(hit2) == 1) {
    n <- suppressWarnings(as.integer(sub(pat2, "", lines[hit2])))
    if (is.finite(n) && n > 1) {
      block <- lines[(hit2 + 1):(hit2 + n)]
      mat <- do.call(rbind, lapply(strsplit(trimws(block), "[[:space:]]+"),
                                   function(v) as.numeric(v[1:2])))
      return(data.frame(x = mat[, 1], y = mat[, 2], stringsAsFactors = FALSE))
    }
  }
  bgm_box_from_faces(lines, boxid)
}

bgm_box_flag <- function(lines, boxid) {
  ibox <- grep(paste0("^box", boxid, "\\.ibox[[:space:]]+"), lines, value = TRUE)
  iface <- grep(paste0("^box", boxid, "\\.iface[[:space:]]+"), lines, value = TRUE)
  botz <- grep(paste0("^box", boxid, "\\.botz[[:space:]]+"), lines, value = TRUE)
  tokens <- character()
  if (length(ibox)) tokens <- c(tokens, unlist(strsplit(sub(".*ibox[[:space:]]+", "", ibox), "[[:space:]]+")))
  if (length(iface)) tokens <- c(tokens, unlist(strsplit(sub(".*iface[[:space:]]+", "", iface), "[[:space:]]+")))
  boundary <- any(tokens == "-1")
  z <- NA_real_
  if (length(botz)) z <- suppressWarnings(as.numeric(sub(".*botz[[:space:]]+", "", botz[1])))
  if (is.finite(z) && z >= 0) boundary <- TRUE
  list(boundary = isTRUE(boundary), botz = z)
}

##' Polygon vertices for every box in a BGM file
read_bgm_polygons <- function(bgm_path) {
  empty <- data.frame(boxid = integer(), x = numeric(), y = numeric(),
                      boundary = logical(), order = integer(),
                      stringsAsFactors = FALSE)
  if (is.null(bgm_path) || !file.exists(bgm_path)) return(empty)
  lines <- readLines(bgm_path, warn = FALSE, encoding = "UTF-8")
  lines <- sub("^\ufeff", "", lines)
  lines <- sub("#.*$", "", lines)
  lines <- trimws(lines)
  nbox <- bgm_nbox(lines)
  if (!is.finite(nbox) || nbox < 1) return(empty)
  gverts <- bgm_global_verts(lines)
  rows <- list()
  for (b in seq_len(nbox) - 1L) {
    verts <- bgm_box_verts(lines, b, gverts)
    if (nrow(verts) == 0) next
    ok <- is.finite(verts$x) & is.finite(verts$y)
    if (sum(ok) < 3) next
    verts <- verts[ok, , drop = FALSE]
    fl <- bgm_box_flag(lines, b)
    verts$boxid <- as.integer(b)
    verts$boundary <- fl$boundary
    verts$order <- seq_len(nrow(verts))
    rows[[length(rows) + 1]] <- verts
  }
  if (length(rows) == 0) return(empty)
  do.call(rbind, rows)
}

##' Join box values onto BGM polygons (0-based box ids on both sides).
spatial_join_polygons <- function(poly, values, time_i = 1L, value_col = "Value") {
  if (is.null(poly) || nrow(poly) == 0) {
    poly$Value <- numeric()
    return(poly)
  }
  poly$boxid <- as.integer(poly$boxid)
  if (is.null(values) || nrow(values) == 0) {
    poly$Value <- NA_real_
    return(poly)
  }
  if ("Time" %in% names(values)) {
    values <- values[as.integer(values$Time) == as.integer(time_i)[1], , drop = FALSE]
  }
  src <- if (value_col %in% names(values)) values[[value_col]] else values$Biomass
  box <- as.integer(values$Box)
  poly$Value <- as.numeric(src[match(poly$boxid, box)])
  poly
}

empty_spatial_values <- function() {
  data.frame(Time = integer(), Box = integer(), Value = numeric(),
             Variable = character(), stringsAsFactors = FALSE)
}

tracer_by_box_layer <- function(nc, varname, layer_key = "integrated", info = NULL) {
  empty <- empty_spatial_values()
  if (is.null(nc) || !nzchar(as.character(varname)[1])) return(empty)
  if (is.null(info)) info <- tryCatch(atlantis_layer_info(nc), error = function(e) NULL)
  arr <- tryCatch(nc_get(nc, varname), error = function(e) NULL)
  if (is.null(arr)) return(empty)
  d <- dim(arr)
  if (!is.null(info) && !is.null(d) && length(d) >= 2 &&
      exists("mask_padded_layers", mode = "function")) {
    arr <- mask_padded_layers(arr, info)
    d <- dim(arr)
  }
  if (is.null(d)) {
    return(data.frame(Time = 1L, Box = 0L, Value = as.numeric(arr)[1],
                      Variable = varname, stringsAsFactors = FALSE))
  }
  if (length(d) == 2 && !is.null(nc$dim$t) && d[2] == nc$dim$t$len) {
    n_b <- d[1]; n_t <- d[2]
    return(data.frame(
      Time = rep(seq_len(n_t), each = n_b),
      Box = rep(seq_len(n_b) - 1L, times = n_t),
      Value = as.numeric(arr),
      Variable = varname, stringsAsFactors = FALSE
    ))
  }
  if (length(d) >= 3) {
    if (!identical(layer_key, "integrated") && !is.null(info) &&
        exists("layer_storage_z", mode = "function")) {
      z <- layer_storage_z(info, layer_key)
      if (is.finite(z) && z >= 1 && z <= d[1]) {
        sl <- arr[z, , , drop = FALSE]
      } else {
        sl <- arr[rep(FALSE, d[1]), , , drop = FALSE]
        sl <- array(NA_real_, dim = c(1, d[2], d[3]))
      }
    } else {
      sl <- arr
    }
    acc <- apply(sl, c(2, 3), function(x) {
      x <- x[is.finite(x)]
      if (!length(x)) NA_real_ else mean(x)
    })
    n_b <- nrow(acc); n_t <- ncol(acc)
    return(data.frame(
      Time = rep(seq_len(n_t), each = n_b),
      Box = rep(seq_len(n_b) - 1L, times = n_t),
      Value = as.numeric(acc),
      Variable = varname, stringsAsFactors = FALSE
    ))
  }
  empty
}

numbers_by_box_layer <- function(nc, grp, fg_code, layer_key = "integrated",
                                 info = NULL) {
  empty <- empty_spatial_values()
  row <- grp[grp$code == fg_code, , drop = FALSE]
  if (nrow(row) == 0) return(empty)
  n.coh <- as.integer(row$numcohorts[1])
  name <- as.character(row$name[1])
  vars <- names(nc$var)
  if (is.null(info)) info <- tryCatch(atlantis_layer_info(nc), error = function(e) NULL)
  acc <- NULL
  if (is.finite(n.coh) && n.coh > 1) {
    for (coh in seq_len(n.coh)) {
      vn <- paste0(name, coh, "_Nums")
      if (!vn %in% vars) next
      arr <- age_subset_array(nc_get(nc, vn), box = NULL, layer_key = layer_key, info)
      if (is.null(arr) || is.null(dim(arr))) next
      bt <- sum_layers_to_box_time(arr)
      acc <- if (is.null(acc)) bt else acc + bt
    }
  }
  if (is.null(acc)) return(empty)
  n_b <- nrow(acc); n_t <- ncol(acc)
  data.frame(
    Time = rep(seq_len(n_t), each = n_b),
    Box = rep(seq_len(n_b) - 1L, times = n_t),
    Value = as.numeric(acc),
    Variable = "Numbers", stringsAsFactors = FALSE
  )
}

##' Box-level series for the Spatial map / time series.
spatial_box_values <- function(nc, grp, kind = "biomass", fg_code = NULL,
                               tracer = NULL, layer_key = "integrated",
                               info = NULL, mg2t = 0.00000002, x.cn = 5.7) {
  kind <- tolower(as.character(kind)[1])
  if (kind %in% c("tracer", "physics")) {
    vn <- as.character(tracer)[1]
    return(tracer_by_box_layer(nc, vn, layer_key, info))
  }
  if (kind %in% c("numbers", "nums")) {
    return(numbers_by_box_layer(nc, grp, fg_code, layer_key, info))
  }
  bio <- biomass_by_box_layer(nc, grp, fg_code, layer_key, info, mg2t, x.cn)
  if (is.null(bio) || nrow(bio) == 0) return(empty_spatial_values())
  data.frame(Time = bio$Time, Box = as.integer(bio$Box), Value = bio$Biomass,
             Variable = "Biomass", stringsAsFactors = FALSE)
}

spatial_ts_values <- function(values, scope = "box", box = 0L, boxes = NULL) {
  if (is.null(values) || nrow(values) == 0) return(values)
  scope <- as.character(scope)[1]
  if (identical(scope, "system")) {
    agg <- stats::aggregate(Value ~ Time, data = values, sum, na.rm = TRUE)
    agg$Box <- -1L
    agg$Variable <- values$Variable[1]
    return(agg)
  }
  if (identical(scope, "boxes") && !is.null(boxes) && length(boxes)) {
    return(values[as.integer(values$Box) %in% as.integer(boxes), , drop = FALSE])
  }
  values[as.integer(values$Box) == as.integer(box)[1], , drop = FALSE]
}

plot_spatial_map <- function(poly, fill_lim = NULL, fill_high = "#0B3C5D",
                             selected_box = NULL, title = NULL,
                             legend = "Value") {
  if (is.null(poly) || nrow(poly) == 0) {
    stop("No box polygons were found in the BGM file.")
  }
  poly$boxid <- as.integer(poly$boxid)
  poly$lab <- paste0("Box ", poly$boxid, "<br>",
                     if ("Value" %in% names(poly)) {
                       ifelse(is.finite(poly$Value),
                              format(poly$Value, digits = 4, trim = TRUE),
                              "NA")
                     } else "NA")
  p <- ggplot2::ggplot(poly, ggplot2::aes(x = x, y = y, group = boxid,
                                          fill = Value, text = lab, key = boxid))
  p <- p + ggplot2::geom_polygon(colour = RA_BOX_OUTLINE, linewidth = 0.25)
  p <- p + ggplot2::coord_equal()
  fill_args <- list(low = "#F7F4EC", high = fill_high,
                    na.value = "#C5C0B2", name = legend)
  if (!is.null(fill_lim) && all(is.finite(fill_lim))) fill_args$limits <- fill_lim
  p <- p + do.call(ggplot2::scale_fill_gradient, fill_args)
  if (!is.null(selected_box) && is.finite(as.integer(selected_box)[1])) {
    sel <- poly[poly$boxid == as.integer(selected_box)[1], , drop = FALSE]
    if (nrow(sel) > 0) {
      p <- p + ggplot2::geom_polygon(
        data = sel, ggplot2::aes(x = x, y = y, group = boxid),
        fill = NA, colour = "#121311", linewidth = 1.4, inherit.aes = FALSE
      )
    }
  }
  if (isTRUE(any(poly$boundary))) {
    bnd <- poly[poly$boundary, ]
    p <- p + ggplot2::geom_polygon(
      data = bnd, ggplot2::aes(x = x, y = y, group = boxid),
      fill = NA, colour = "#0B3C5D", linewidth = 0.7, inherit.aes = FALSE
    )
  }
  p <- p + ggplot2::labs(title = title, x = NULL, y = NULL)
  p + theme_reactiveatlantis() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank())
}

plot_spatial_ts <- function(df, dates, time_i = NULL, title = NULL, ylab = "Value") {
  if (is.null(df) || nrow(df) == 0) stop("No time series for this box / layer.")
  df$Date <- if (!is.null(dates) && length(dates) >= max(df$Time)) {
    dates[df$Time]
  } else {
    as.Date("1970-01-01") + (df$Time - 1L)
  }
  df$BoxLab <- ifelse(df$Box < 0, "Whole system", paste("Box", df$Box))
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Value, colour = BoxLab))
  p <- p + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)
  p <- p + ggplot2::geom_point(size = 1.4, na.rm = TRUE)
  if (!is.null(time_i) && is.finite(as.integer(time_i)[1]) &&
      as.integer(time_i)[1] >= 1 && as.integer(time_i)[1] <= max(df$Time)) {
    xint <- df$Date[df$Time == as.integer(time_i)[1]][1]
    if (!is.na(xint)) {
      p <- p + ggplot2::geom_vline(xintercept = xint, linetype = "dashed",
                                   colour = "#A33B20")
    }
  }
  p <- p + ggplot2::labs(title = title, x = "Year", y = ylab, colour = NULL)
  p <- p + theme_reactiveatlantis()
  ra_scale_x_year(p, df$Date)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Biomass per box                  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

nc_volume <- function(nc) {
  if (is.null(nc)) return(NULL)
  vars <- names(nc$var)
  if ("volume" %in% vars) return(nc_get(nc, "volume"))
  if ("vol" %in% vars) return(nc_get(nc, "vol"))
  NULL
}

add_nc_arrays <- function(acc, arr) {
  if (is.null(acc)) return(arr)
  if (length(dim(acc)) == length(dim(arr)) && all(dim(acc) == dim(arr))) {
    return(acc + arr)
  }
  acc
}

biomass_by_box <- function(nc, grp, fg_code, mg2t = 0.00000002, x.cn = 5.7) {
  path <- tryCatch(nc$filename, error = function(e) "anon")
  ck <- paste("bbox", file_cache_key(path, fg_code), mg2t, x.cn, sep = "|")
  hit <- ra_cache_get(ck)
  if (!is.null(hit)) return(hit)
  empty <- data.frame(Time = integer(), Box = integer(), Biomass = numeric(),
                      Code = character(), stringsAsFactors = FALSE)
  row <- grp[grp$code == fg_code, , drop = FALSE]
  if (nrow(row) == 0) return(empty)
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
      nums <- nc_get(nc, n_var)
      resn <- nc_get(nc, r_var)
      strn <- nc_get(nc, s_var)
      cell <- (resn + strn) * nums
      bt <- sum_layers_to_box_time(cell)
      acc <- if (is.null(acc)) bt else acc + bt
    }
    if (is.null(acc)) return(empty)
    acc <- acc * mg2t * x.cn
  } else {
    vname <- if (n.coh > 1 && gtype %in% AGE_SPECIAL) {
      paste0(name, "_N1")
    } else {
      paste0(name, "_N")
    }
    if (!vname %in% vars && paste0(name, "_N") %in% vars) vname <- paste0(name, "_N")
    if (!vname %in% vars) return(empty)
    biom <- nc_get(nc, vname)
    vol <- nc_volume(nc)
    if (!is.null(vol) && length(dim(biom)) == 3) {
      if (length(dim(vol)) == 2) {
        nt <- dim(biom)[3]
        acc <- apply(sweep(biom, c(1, 2), vol, "*"), c(2, 3), sum, na.rm = TRUE)
      } else if (length(dim(vol)) == 3) {
        acc <- apply(biom * vol, c(2, 3), sum, na.rm = TRUE)
      } else {
        acc <- sum_layers_to_box_time(biom)
      }
    } else {
      acc <- sum_layers_to_box_time(biom)
    }
    acc <- acc * mg2t * x.cn
  }
  n_box <- nrow(acc)
  n_t <- ncol(acc)
  df <- data.frame(
    Time = rep(seq_len(n_t), each = n_box),
    Box = rep(seq_len(n_box) - 1L, times = n_t),
    Biomass = as.numeric(acc),
    Code = fg_code,
    stringsAsFactors = FALSE
  )
  ra_cache_set(ck, df)
  df
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          PROD Eat / Grazing                 ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' North Sea outputPROD.nc names, keyed by groups CSV Name (case-sensitive)
##'
##' Age-structured groups (numcohorts > 1, not CEP/PWN/…):
##' \code{<name><cohort>_Eat} (and \code{<name><cohort>_Growth}).
##' Biomass pools and AGE_SPECIAL groups: \code{<name>Grazing}
##' (and \code{<name>Prodn}).
prod_is_age_structured <- function(n_cohorts, grouptype = NULL) {
  n_cohorts <- as.integer(n_cohorts)[1]
  if (!is.finite(n_cohorts) || n_cohorts < 1) n_cohorts <- 1
  gtype <- if (is.null(grouptype)) "" else as.character(grouptype)[1]
  n_cohorts > 1 && !gtype %in% AGE_SPECIAL
}

prod_consume_vars <- function(nc_vars, name, n_cohorts = 1,
                              grouptype = NULL, code = NULL) {
  name <- trimws(as.character(name)[1])
  n_cohorts <- as.integer(n_cohorts)[1]
  if (!is.finite(n_cohorts) || n_cohorts < 1) n_cohorts <- 1
  if (prod_is_age_structured(n_cohorts, grouptype)) {
    hit <- paste0(name, seq_len(n_cohorts), "_Eat")
    hit <- hit[hit %in% nc_vars]
    if (length(hit) > 0) return(hit)
  } else {
    graz <- paste0(name, "Grazing")
    if (graz %in% nc_vars) return(graz)
  }
  character()
}

prod_growth_vars <- function(nc_vars, name, n_cohorts = 1, grouptype = NULL) {
  name <- trimws(as.character(name)[1])
  n_cohorts <- as.integer(n_cohorts)[1]
  if (!is.finite(n_cohorts) || n_cohorts < 1) n_cohorts <- 1
  if (prod_is_age_structured(n_cohorts, grouptype)) {
    hit <- paste0(name, seq_len(n_cohorts), "_Growth")
    return(hit[hit %in% nc_vars])
  }
  prodn <- paste0(name, "Prodn")
  if (prodn %in% nc_vars) return(prodn)
  character()
}

prod_eat_name <- function(nc_vars, code, name, n_cohorts = 1, grouptype = NULL) {
  vars <- prod_consume_vars(nc_vars, name, n_cohorts, grouptype, code)
  if (length(vars) == 0) return(NA_character_)
  vars[1]
}

consumed_tonnes <- function(nc_prod, nc_out, grp, fg_code,
                            mg2t = 0.00000002, x.cn = 5.7) {
  prod_path <- tryCatch(nc_prod$filename, error = function(e) "anon")
  ck <- paste("eat", file_cache_key(prod_path, fg_code), mg2t, x.cn, sep = "|")
  hit <- ra_cache_get(ck)
  if (!is.null(hit)) return(hit)
  row <- grp[grp$code == fg_code, , drop = FALSE]
  if (nrow(row) == 0) stop("Unknown functional group: ", fg_code)
  name <- as.character(row$name[1])
  n.coh <- if ("numcohorts" %in% names(row)) as.integer(row$numcohorts[1]) else 1L
  gtype <- if ("grouptype" %in% names(row)) as.character(row$grouptype[1]) else ""
  vns <- prod_consume_vars(names(nc_prod$var), name, n.coh, gtype, row$code[1])
  if (length(vns) == 0) {
    stop("No consumption tracer for ", fg_code,
         ". Looked for ", name, "1_Eat … ", name, n.coh, "_Eat or ",
         name, "Grazing (groups CSV Name, case-sensitive).")
  }
  eat <- NULL
  for (vn in vns) {
    arr <- nc_get(nc_prod, vn)
    eat <- add_nc_arrays(eat, arr)
  }
  vol <- nc_volume(nc_prod)
  if (is.null(vol)) vol <- nc_volume(nc_out)
  if (!is.null(vol) && length(dim(eat)) == 3 && length(dim(vol)) %in% c(2, 3)) {
    if (length(dim(vol)) == 2) {
      mass <- sweep(eat, c(1, 2), vol, "*")
    } else {
      n <- min(dim(eat)[3], dim(vol)[3])
      mass <- eat[, , seq_len(n), drop = FALSE] * vol[, , seq_len(n), drop = FALSE]
    }
    tonnes <- sum_space(mass) * mg2t * x.cn
  } else {
    tonnes <- sum_space(eat) * mg2t * x.cn
  }
  out <- list(variable = vns[1], variables = vns, tonnes = as.numeric(tonnes))
  ra_cache_set(ck, out)
  out
}

consumption_by_prey <- function(tonnes, diet_time, predator, stock = NULL) {
  empty <- data.frame(Time = numeric(), Prey = character(), Tonnes = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(diet_time) || nrow(diet_time) == 0) return(empty)
  dt <- diet_time
  if ("Predator" %in% names(dt)) {
    dt <- dt[dt$Predator == predator]
  }
  if ("Stock" %in% names(dt) && !is.null(stock) && nzchar(as.character(stock))) {
    dt <- dt[dt$Stock == as.numeric(stock)]
  }
  if (nrow(dt) == 0) return(empty)
  prey_cols <- setdiff(names(dt), c("Time", "Predator", "Stock", "Cohort"))
  if (length(prey_cols) == 0) return(empty)
  times <- as.numeric(dt$Time)
  n <- min(length(tonnes), length(unique(times)))
  long <- data.table::melt(
    data.table::as.data.table(dt)[, c("Time", prey_cols), with = FALSE],
    id.vars = "Time", variable.name = "Prey", value.name = "prop",
    variable.factor = FALSE
  )
  long$prop <- as.numeric(long$prop)
  t_index <- match(long$Time, sort(unique(long$Time)))
  long$Tonnes <- long$prop * tonnes[pmin(t_index, length(tonnes))]
  as.data.frame(long[, c("Time", "Prey", "Tonnes")])
}

consumption_as_prey <- function(nc_prod, nc_out, grp, prey_code, diet_time,
                                mg2t = 0.00000002, x.cn = 5.7) {
  empty <- data.frame(Time = numeric(), Predator = character(), Tonnes = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(diet_time) || !prey_code %in% names(diet_time)) return(empty)
  prod_path <- tryCatch(nc_prod$filename, error = function(e) "anon")
  ck <- paste("asprey", file_cache_key(prod_path, prey_code), mg2t, x.cn, sep = "|")
  hit <- ra_cache_get(ck)
  if (!is.null(hit)) return(hit)
  dt <- data.table::as.data.table(diet_time)
  prop_col <- dt[[prey_code]]
  preds <- unique(as.character(dt$Predator[is.finite(as.numeric(prop_col)) &
                                            as.numeric(prop_col) > 0]))
  rows <- list()
  for (pr in preds) {
    tot <- tryCatch(consumed_tonnes(nc_prod, nc_out, grp, pr, mg2t, x.cn),
                    error = function(e) NULL)
    if (is.null(tot)) next
    slice <- dt[Predator == pr, c("Time", "Predator", prey_code), with = FALSE]
    data.table::setnames(slice, prey_code, "prop")
    if (nrow(slice) == 0) next
    t_index <- match(slice$Time, sort(unique(dt$Time)))
    slice$Tonnes <- as.numeric(slice$prop) * tot$tonnes[pmin(t_index, length(tot$tonnes))]
    rows[[length(rows) + 1]] <- data.frame(
      Time = slice$Time, Predator = pr, Tonnes = slice$Tonnes,
      stringsAsFactors = FALSE
    )
  }
  out <- if (length(rows) == 0) empty else do.call(rbind, rows)
  ra_cache_set(ck, out)
  out
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~             Physics tracers                 ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

PHYSICS_CANDIDATES <- list(
  Temperature = c("Temp", "temperature", "temp"),
  Salinity = c("salt", "Salinity", "salt_sed"),
  NO3 = c("NO3"),
  NH3 = c("NH3", "NH4"),
  Chl_a = c("Chl_a", "Chla", "CHL", "Chl"),
  Denitrification = c("Denitrificatio", "Denitrification", "Denitri")
)

match_nc_var <- function(nc_vars, candidates) {
  hit <- candidates[candidates %in% nc_vars]
  if (length(hit) == 0) {
    hit <- nc_vars[tolower(nc_vars) %in% tolower(candidates)]
  }
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

physics_long <- function(nc, varname, box = NULL, info = NULL, inf_box = NULL) {
  arr <- nc_get(nc, varname)
  d <- dim(arr)
  if (is.null(d)) {
    return(data.frame(Time = seq_along(arr), Layer = 0, Box = 0,
                      Value = as.numeric(arr), LayerLab = "Layer 0",
                      stringsAsFactors = FALSE))
  }
  if (exists("atlantis_layer_info", mode = "function") && is.null(info)) {
    info <- tryCatch(atlantis_layer_info(nc, inf_box), error = function(e) NULL)
  }
  if (exists("mask_padded_layers", mode = "function") && !is.null(info) &&
      length(d) >= 2) {
    arr <- mask_padded_layers(arr, info)
  }
  lab_for <- function(z1) {
    if (!is.null(info) && exists("storage_z_label", mode = "function")) {
      storage_z_label(info, z1)
    } else {
      paste("Layer", z1 - 1L)
    }
  }
  if (length(d) == 2) {
    ## b, t  or z, b (no time) — treat columns as time if 2nd dim looks like t
    n_t <- if (!is.null(nc$dim$t)) nc$dim$t$len else d[2]
    if (d[2] == n_t) {
      boxes <- if (is.null(box)) seq_len(d[1]) - 1L else box
      rows <- lapply(boxes, function(b) {
        data.frame(Time = seq_len(d[2]), Layer = 0, Box = b,
                   Value = as.numeric(arr[b + 1, ]), LayerLab = "Layer 0",
                   stringsAsFactors = FALSE)
      })
      return(do.call(rbind, rows))
    }
  }
  if (length(d) == 3) {
    n_z <- d[1]
    n_b <- d[2]
    n_t <- d[3]
    boxes <- if (is.null(box)) seq_len(n_b) - 1L else as.integer(box)
    rows <- list()
    for (b in boxes) {
      b1 <- as.integer(b) + 1L
      for (z in seq_len(n_z)) {
        if (!is.null(info) && exists("box_has_storage_z", mode = "function") &&
            !box_has_storage_z(info, b1, z)) {
          next
        }
        rows[[length(rows) + 1]] <- data.frame(
          Time = seq_len(n_t), Layer = z - 1L, Box = b,
          Value = as.numeric(arr[z, b1, ]),
          LayerLab = lab_for(z),
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(rows) == 0) {
      return(data.frame(Time = integer(), Layer = integer(), Box = integer(),
                        Value = numeric(), LayerLab = character(),
                        stringsAsFactors = FALSE))
    }
    return(do.call(rbind, rows))
  }
  data.frame(Time = integer(), Layer = integer(), Box = integer(),
             Value = numeric(), LayerLab = character(),
             stringsAsFactors = FALSE)
}

layer_thickness <- function(nc) {
  vars <- names(nc$var)
  vn <- match_nc_var(vars, c("nominal_dz", "dz", "nhgt"))
  if (is.na(vn)) return(NULL)
  arr <- nc_get(nc, vn)
  list(name = vn, array = arr)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~               Ensemble runs                 ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

load_run_lite <- function(folder, label = NULL, config_dir = NULL, output_dir = NULL) {
  fold <- ra_nz(folder)
  cfg <- ra_nz(config_dir)
  outd <- ra_nz(output_dir)
  if (!nzchar(cfg) && nzchar(fold)) cfg <- fold
  if (!nzchar(outd) && nzchar(fold)) outd <- fold
  det <- detect_run_pair(cfg, outd)
  paths <- det$paths
  if (!is.null(label) && nzchar(ra_nz(label))) {
    lab <- ra_nz(label)
  } else if (nzchar(cfg) && nzchar(outd) && !same_dir(cfg, outd)) {
    lab <- paste(basename(cfg), basename(outd), sep = " + ")
  } else {
    lab <- basename(if (nzchar(outd)) outd else cfg)
  }
  out <- list(
    label = lab,
    folder = if (nzchar(fold)) fold else if (nzchar(outd)) outd else cfg,
    config_dir = det$config_dir,
    output_dir = det$output_dir,
    paths = paths,
    biomindx = NULL,
    yoy = NULL,
    ssb = NULL,
    catch_nc = NULL
  )
  if (path_ok(paths$biomindx)) {
    out$biomindx <- tryCatch(read_space_table(paths$biomindx), error = function(e) NULL)
  }
  if (path_ok(paths$yoy)) {
    out$yoy <- tryCatch(utils::read.csv(paths$yoy, sep = " ", stringsAsFactors = FALSE),
                        error = function(e) NULL)
  }
  if (path_ok(paths$ssb)) {
    out$ssb <- tryCatch(read_space_table(paths$ssb), error = function(e) NULL)
  }
  if (path_ok(paths$catch_nc)) {
    out$catch_nc <- nc_handle(paths$catch_nc)
  }
  out
}

ensemble_biomass <- function(runs, code) {
  rows <- list()
  for (rn in runs) {
    biom <- rn$biomindx
    if (is.null(biom) || !code %in% names(biom)) next
    rows[[length(rows) + 1]] <- data.frame(
      Time = as.numeric(biom$Time),
      Value = as.numeric(biom[[code]]),
      Run = rn$label,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(Time = numeric(), Value = numeric(), Run = character()))
  }
  do.call(rbind, rows)
}

catch_total_for_code <- function(nc, code) {
  if (is.null(nc)) return(NULL)
  vars <- names(nc$var)
  hits <- vars[grepl(paste0("^", code, "_Catch"), vars, ignore.case = TRUE)]
  if (length(hits) == 0) return(NULL)
  acc <- NULL
  for (vn in hits) {
    s <- sum_space(nc_get(nc, vn))
    acc <- if (is.null(acc)) s else acc + s
  }
  t_days <- tryCatch({
    units <- ncdf4::ncatt_get(nc, "t")$units
    raw <- as.numeric(nc_get(nc, "t"))
    if (isTRUE(grepl("^seconds", units, ignore.case = TRUE))) raw / 86400 else raw
  }, error = function(e) seq_along(acc) - 1)
  data.frame(Time = as.numeric(t_days), Value = as.numeric(acc), stringsAsFactors = FALSE)
}

ensemble_table <- function(runs, code, kind = "biomass") {
  rows <- list()
  for (rn in runs) {
    if (identical(kind, "catch")) {
      tab <- catch_total_for_code(rn$catch_nc, code)
      if (is.null(tab) || nrow(tab) == 0) next
      rows[[length(rows) + 1]] <- data.frame(
        Time = tab$Time, Value = tab$Value, Run = rn$label, stringsAsFactors = FALSE
      )
      next
    }
    tab <- switch(kind,
                  biomass = rn$biomindx,
                  yoy = rn$yoy,
                  ssb = rn$ssb,
                  NULL)
    if (is.null(tab)) next
    col <- if (kind == "yoy") paste0(code, ".0") else code
    if (!col %in% names(tab)) next
    tcol <- if ("Time" %in% names(tab)) tab$Time else seq_len(nrow(tab))
    rows[[length(rows) + 1]] <- data.frame(
      Time = as.numeric(tcol),
      Value = as.numeric(tab[[col]]),
      Run = rn$label,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(Time = numeric(), Value = numeric(), Run = character()))
  }
  do.call(rbind, rows)
}

ensemble_stats <- function(df) {
  if (nrow(df) == 0) {
    return(data.frame(Time = numeric(), Mean = numeric(), Lo = numeric(), Hi = numeric()))
  }
  agg <- stats::aggregate(Value ~ Time, data = df, function(x) {
    c(Mean = mean(x, na.rm = TRUE), Lo = min(x, na.rm = TRUE), Hi = max(x, na.rm = TRUE))
  })
  mat <- as.data.frame(agg$Value)
  data.frame(Time = agg$Time, Mean = mat$Mean, Lo = mat$Lo, Hi = mat$Hi)
}
