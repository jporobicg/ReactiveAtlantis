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
        arr <- tryCatch(ncdf4::ncvar_get(nc, vn), error = function(e) NULL)
        if (is.null(arr)) next
        d <- dim(arr)
        nt <- if (is.null(d)) length(arr) else d[length(d)]
        first <- mean(as.numeric(time_slice(arr, 1)), na.rm = TRUE)
        last <- mean(as.numeric(time_slice(arr, nt)), na.rm = TRUE)
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

bgm_box_verts <- function(lines, boxid) {
  pat_xy <- paste0("^box", boxid, "\\.vert[[:space:]]+")
  xy_lines <- grep(pat_xy, lines, value = TRUE)
  if (length(xy_lines) > 0) {
    coords <- strsplit(sub(pat_xy, "", xy_lines), "[[:space:]]+")
    if (length(coords[[1]]) == 1 && length(coords) == 1) {
      n <- suppressWarnings(as.integer(coords[[1]][1]))
      start <- grep(pat_xy, lines)[1]
      if (is.finite(n) && n > 1) {
        block <- lines[(start + 1):(start + n)]
        mat <- do.call(rbind, lapply(strsplit(trimws(block), "[[:space:]]+"), as.numeric))
        return(data.frame(x = mat[, 1], y = mat[, 2], stringsAsFactors = FALSE))
      }
    }
    mat <- do.call(rbind, lapply(coords, function(v) as.numeric(v[1:2])))
    return(data.frame(x = mat[, 1], y = mat[, 2], stringsAsFactors = FALSE))
  }
  pat2 <- paste0("^box", boxid, "\\.vert2[[:space:]]+")
  hit2 <- grep(pat2, lines)
  if (length(hit2) == 1) {
    n <- suppressWarnings(as.integer(sub(pat2, "", lines[hit2])))
    if (is.finite(n) && n > 1) {
      block <- lines[(hit2 + 1):(hit2 + n)]
      mat <- do.call(rbind, lapply(strsplit(trimws(block), "[[:space:]]+"), as.numeric))
      return(data.frame(x = mat[, 1], y = mat[, 2], stringsAsFactors = FALSE))
    }
  }
  data.frame(x = numeric(), y = numeric())
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
  if (is.null(bgm_path) || !file.exists(bgm_path)) {
    return(data.frame(boxid = integer(), x = numeric(), y = numeric(),
                      boundary = logical(), order = integer(),
                      stringsAsFactors = FALSE))
  }
  lines <- readLines(bgm_path, warn = FALSE)
  lines <- sub("#.*$", "", lines)
  nbox <- bgm_nbox(lines)
  if (!is.finite(nbox) || nbox < 1) {
    stop("Could not read nbox from the BGM file.")
  }
  rows <- list()
  for (b in seq_len(nbox) - 1L) {
    verts <- bgm_box_verts(lines, b)
    if (nrow(verts) == 0) next
    fl <- bgm_box_flag(lines, b)
    verts$boxid <- b
    verts$boundary <- fl$boundary
    verts$order <- seq_len(nrow(verts))
    rows[[length(rows) + 1]] <- verts
  }
  if (length(rows) == 0) {
    stop("No box vertices were found in the BGM file.")
  }
  do.call(rbind, rows)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Biomass per box                  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

nc_volume <- function(nc) {
  vars <- names(nc$var)
  if ("volume" %in% vars) return(ncdf4::ncvar_get(nc, "volume"))
  if ("vol" %in% vars) return(ncdf4::ncvar_get(nc, "vol"))
  NULL
}

biomass_by_box <- function(nc, grp, fg_code, mg2t = 0.00000002, x.cn = 5.7) {
  row <- grp[grp$code == fg_code, , drop = FALSE]
  if (nrow(row) == 0) {
    stop("Unknown functional group: ", fg_code)
  }
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
      nums <- ncdf4::ncvar_get(nc, n_var)
      resn <- ncdf4::ncvar_get(nc, r_var)
      strn <- ncdf4::ncvar_get(nc, s_var)
      cell <- (resn + strn) * nums
      bt <- sum_layers_to_box_time(cell)
      acc <- if (is.null(acc)) bt else acc + bt
    }
    if (is.null(acc)) stop("No StructN/ResN/Nums variables for ", fg_code)
    acc <- acc * mg2t * x.cn
  } else {
    vname <- if (n.coh > 1 && gtype %in% AGE_SPECIAL) {
      paste0(name, "_N1")
    } else {
      paste0(name, "_N")
    }
    if (!vname %in% vars && paste0(name, "_N") %in% vars) vname <- paste0(name, "_N")
    if (!vname %in% vars) stop("No nitrogen tracer for ", fg_code)
    biom <- ncdf4::ncvar_get(nc, vname)
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
  df
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          PROD Eat / Grazing                 ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

prod_eat_name <- function(nc_vars, code, name) {
  code <- trimws(as.character(code))
  name <- trimws(as.character(name))
  cands <- c(
    paste0("Eat_", code), paste0("Grazing_", code),
    paste0("Eat_", name), paste0("Grazing_", name),
    paste0(code, "_Eat"), paste0(name, "_Eat"),
    paste0("Eat", code), paste0("Grazing", code),
    paste0("Eat", name), paste0("Grazing", name)
  )
  hit <- cands[cands %in% nc_vars]
  if (length(hit) == 0) {
    hit <- nc_vars[tolower(nc_vars) %in% tolower(cands)]
  }
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

consumed_tonnes <- function(nc_prod, nc_out, grp, fg_code,
                            mg2t = 0.00000002, x.cn = 5.7) {
  row <- grp[grp$code == fg_code, , drop = FALSE]
  if (nrow(row) == 0) stop("Unknown functional group: ", fg_code)
  vn <- prod_eat_name(names(nc_prod$var), row$code[1], row$name[1])
  if (is.na(vn)) {
    stop("No Eat_ / Grazing_ variable for ", fg_code,
         ". Looked for Eat_", row$code[1], " and Grazing_", row$code[1], ".")
  }
  eat <- ncdf4::ncvar_get(nc_prod, vn)
  vol <- if (is.null(nc_out)) NULL else nc_volume(nc_out)
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
  list(variable = vn, tonnes = as.numeric(tonnes))
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
  dt <- data.table::as.data.table(diet_time)
  preds <- unique(as.character(dt$Predator))
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
  if (length(rows) == 0) return(empty)
  do.call(rbind, rows)
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

physics_long <- function(nc, varname, box = NULL) {
  arr <- ncdf4::ncvar_get(nc, varname)
  d <- dim(arr)
  if (is.null(d)) {
    return(data.frame(Time = seq_along(arr), Layer = 0, Box = 0,
                      Value = as.numeric(arr), stringsAsFactors = FALSE))
  }
  if (length(d) == 2) {
    ## b, t  or z, b (no time) — treat columns as time if 2nd dim looks like t
    n_t <- if (!is.null(nc$dim$t)) nc$dim$t$len else d[2]
    if (d[2] == n_t) {
      boxes <- if (is.null(box)) seq_len(d[1]) - 1L else box
      rows <- lapply(boxes, function(b) {
        data.frame(Time = seq_len(d[2]), Layer = 0, Box = b,
                   Value = as.numeric(arr[b + 1, ]), stringsAsFactors = FALSE)
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
      for (z in seq_len(n_z)) {
        rows[[length(rows) + 1]] <- data.frame(
          Time = seq_len(n_t), Layer = z - 1L, Box = b,
          Value = as.numeric(arr[z, b + 1, ]),
          stringsAsFactors = FALSE
        )
      }
    }
    return(do.call(rbind, rows))
  }
  data.frame(Time = integer(), Layer = integer(), Box = integer(), Value = numeric())
}

layer_thickness <- function(nc) {
  vars <- names(nc$var)
  vn <- match_nc_var(vars, c("nominal_dz", "dz", "nhgt"))
  if (is.na(vn)) return(NULL)
  arr <- ncdf4::ncvar_get(nc, vn)
  list(name = vn, array = arr)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~               Ensemble runs                 ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

load_run_lite <- function(folder, label = NULL) {
  paths <- detect_project_files(folder)
  out <- list(
    label = if (is.null(label) || !nzchar(label)) basename(folder) else label,
    folder = folder,
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
    out$catch_nc <- tryCatch(ncdf4::nc_open(paths$catch_nc), error = function(e) NULL)
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
    s <- sum_space(ncdf4::ncvar_get(nc, vn))
    acc <- if (is.null(acc)) s else acc + s
  }
  t_days <- tryCatch({
    units <- ncdf4::ncatt_get(nc, "t")$units
    raw <- as.numeric(ncdf4::ncvar_get(nc, "t"))
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
