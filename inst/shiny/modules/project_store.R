## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Shared Atlantis run / project store    ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

FILE_ROLES <- c(
  "groups", "fisheries", "output_nc", "output_nc_old", "catch_nc", "init_nc",
  "prod_nc", "biomindx", "dietcheck", "diet_bypol", "ssb", "yoy",
  "bio_prm", "run_prm", "force_prm", "bgm", "specific_mort", "specific_pred_mort",
  "ext_catch", "obs_csv"
)

FILE_LABELS <- c(
  groups = "Groups CSV",
  fisheries = "Fisheries CSV",
  output_nc = "Output NetCDF",
  output_nc_old = "Previous output NetCDF",
  catch_nc = "Catch NetCDF",
  init_nc = "Initial conditions NetCDF",
  biomindx = "BiomIndx.txt",
  dietcheck = "DietCheck.txt",
  diet_bypol = "Diet by polygon",
  ssb = "SSB.txt",
  yoy = "YOY.txt",
  bio_prm = "Biology .prm",
  run_prm = "Run .prm",
  force_prm = "Forcing .prm",
  bgm = "BGM geometry",
  prod_nc = "Production NetCDF (PROD)",
  specific_mort = "SpecificMort.txt",
  specific_pred_mort = "SpecificPredMort.txt",
  ext_catch = "External catch CSV",
  obs_csv = "Observations CSV"
)

##' Roles that usually live in the Atlantis configuration folder
CONFIG_ROLES <- c(
  "groups", "fisheries", "init_nc", "bio_prm", "run_prm", "force_prm",
  "bgm", "obs_csv", "ext_catch"
)

##' Roles that usually live in the Atlantis output folder
OUTPUT_ROLES <- c(
  "output_nc", "output_nc_old", "catch_nc", "prod_nc",
  "biomindx", "dietcheck", "diet_bypol", "ssb", "yoy",
  "specific_mort", "specific_pred_mort"
)

TOOL_REQUIREMENTS <- list(
  overview = c("groups"),
  compare = c("groups"),
  predation = c("groups", "biomindx", "dietcheck"),
  foodweb = c("groups", "dietcheck"),
  recruitment = c("groups", "output_nc", "yoy", "bio_prm"),
  growth = c("groups", "output_nc", "bio_prm"),
  catch = c("groups", "fisheries", "catch_nc"),
  feeding = c("groups", "bio_prm", "init_nc"),
  mortality = c("groups", "specific_mort", "specific_pred_mort"),
  spatial = c("groups", "output_nc", "bgm"),
  consumption = c("groups", "prod_nc"),
  physics = c("output_nc"),
  ages = c("groups", "output_nc"),
  ensemble = c("groups")
)

empty_paths <- function() {
  setNames(as.list(rep(NA_character_, length(FILE_ROLES))), FILE_ROLES)
}

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
}

ra_nz <- function(x) {
  x <- as.character(x)[1]
  if (length(x) == 0 || is.na(x)) return("")
  trimws(x)
}

same_dir <- function(a, b) {
  a <- ra_nz(a)
  b <- ra_nz(b)
  if (!nzchar(a) || !nzchar(b)) return(FALSE)
  if (!dir.exists(a) || !dir.exists(b)) {
    return(identical(normalizePath(a, winslash = "/", mustWork = FALSE),
                     normalizePath(b, winslash = "/", mustWork = FALSE)))
  }
  identical(normalizePath(a, winslash = "/", mustWork = TRUE),
            normalizePath(b, winslash = "/", mustWork = TRUE))
}

empty_state <- function() {
  list(
    ready = FALSE,
    paths = empty_paths(),
    data = NULL,
    ensemble = NULL,
    scan_note = NULL,
    gen = 0L,
    fallback_roles = character(),
    config_dir = NA_character_,
    output_dir = NA_character_,
    run_pairs = list()
  )
}

##' Browser / OS duplicate names ("NorthSea (1).bgm", "copy", AppleDouble)
is_os_duplicate <- function(bn) {
  bn <- as.character(bn)[1]
  if (is.na(bn) || !nzchar(bn)) return(FALSE)
  grepl(" \\([0-9]+\\)\\.[^.]+$", bn) ||
    grepl("[- _]copy\\.", bn, ignore.case = TRUE) ||
    grepl("^\\._", bn)
}

##' Single existing path; length>1 or mixed NA never counts as missing
as_path <- function(p) {
  if (is.null(p) || length(p) < 1) return(NA_character_)
  p <- as.character(unlist(p, use.names = FALSE))
  p <- p[!is.na(p) & nzchar(trimws(p))]
  p <- trimws(p)
  if (length(p) == 0) return(NA_character_)
  exist <- p[file.exists(p)]
  if (length(exist) == 0) return(p[1])
  exist <- exist[!is_os_duplicate(basename(exist))]
  if (length(exist) == 0) exist <- p[file.exists(p)]
  exist[order(nchar(basename(exist)), basename(exist))][1]
}

pick_first <- function(files, pattern, exclude = NULL) {
  if (length(files) == 0) return(NA_character_)
  bn <- basename(files)
  hit <- grepl(pattern, bn, ignore.case = TRUE)
  if (!is.null(exclude)) hit <- hit & !grepl(exclude, bn, ignore.case = TRUE)
  cand <- files[hit]
  if (length(cand) == 0) return(NA_character_)
  primary <- cand[!is_os_duplicate(basename(cand))]
  if (length(primary) == 0) primary <- cand
  as_path(primary[order(nchar(basename(primary)), basename(primary))])
}

classify_csv <- function(path) {
  hdr <- tryCatch(tolower(names(utils::read.csv(path, nrows = 0, check.names = FALSE))),
                  error = function(e) character())
  if (length(hdr) == 0) return(NA_character_)
  if (("code" %in% hdr || "code" %in% gsub("[^a-z]", "", hdr)) &&
      any(c("numcohorts", "isturnedon", "grouptype", "longname") %in% hdr)) {
    return("groups")
  }
  if (all(c("code", "year", "value") %in% hdr)) return("obs")
  if ("index" %in% hdr && "code" %in% hdr) return("fisheries")
  NA_character_
}

classify_prm <- function(path) {
  sz <- suppressWarnings(file.size(path))
  if (is.finite(sz) && sz > 20 * 1024^2) return(NA_character_)
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  if (length(lines) == 0) return(NA_character_)
  ## Comments can mention pPREY; require a real token (pPREY1COD / KUP_COD).
  if (any(grepl("(^|[[:space:]])pPREY[A-Za-z0-9_]*[[:space:]]", lines)) ||
      any(grepl("(^|[[:space:]])KUP_[A-Za-z0-9]+", lines))) {
    return("bio_prm")
  }
  if (any(grepl("hd[0-9]+\\.name", lines, ignore.case = TRUE))) return("force_prm")
  if (any(grepl("(^|[[:space:]])tstop([[:space:]]|$)", lines, ignore.case = TRUE)) ||
      any(grepl("(^|[[:space:]])dt([[:space:]]|$)", lines))) {
    return("run_prm")
  }
  NA_character_
}

##' Named length-1 character list of roles, always aligned with FILE_ROLES
normalize_paths <- function(paths) {
  out <- empty_paths()
  if (is.null(paths)) return(out)
  nms <- names(paths)
  if (is.null(nms) || !any(nzchar(nms), na.rm = TRUE)) {
    if (length(paths) == length(FILE_ROLES)) nms <- FILE_ROLES else return(out)
  }
  nms[is.na(nms)] <- ""
  for (i in seq_along(paths)) {
    nm <- as.character(nms[i])[1]
    if (!nzchar(nm) || !nm %in% FILE_ROLES) next
    val <- as_path(paths[[i]])
    if (!is.na(val) && nzchar(val)) out[[nm]] <- val
  }
  out
}

##' One consistent view of the run: paths + in-memory objects + ready flag
project_snapshot <- function(project) {
  if (is.null(project)) return(empty_state())
  if (inherits(project, "reactivevalues") || inherits(project, "reactiveValues")) {
    st <- project$state
    if (is.list(st) && !is.null(st$paths)) {
      st$paths <- normalize_paths(st$paths)
      st$ready <- isTRUE(st$ready)
      if (is.null(st$gen)) st$gen <- 0L
      if (is.null(st$fallback_roles)) st$fallback_roles <- character()
      return(st)
    }
    return(list(
      ready = isTRUE(project$ready),
      paths = normalize_paths(project$paths),
      data = project$data,
      ensemble = project$ensemble,
      scan_note = project$scan_note,
      gen = 0L,
      fallback_roles = project$fallback_roles %||% character(),
      config_dir = project$config_dir,
      output_dir = project$output_dir,
      run_pairs = project$run_pairs %||% list()
    ))
  }
  if (is.list(project)) {
    dat <- project$data
    if (is.null(dat) && !is.null(project$grp)) dat <- project
    return(list(
      ready = isTRUE(project$ready),
      paths = normalize_paths(project$paths),
      data = dat,
      ensemble = project$ensemble,
      scan_note = project$scan_note,
      gen = if (is.null(project$gen)) 0L else project$gen,
      fallback_roles = project$fallback_roles %||% character(),
      config_dir = project$config_dir,
      output_dir = project$output_dir,
      run_pairs = project$run_pairs %||% list()
    ))
  }
  empty_state()
}

##' True when a role has a readable path or the object is already in memory
role_is_loaded <- function(project, role) {
  if (is.null(project) || is.null(role) || !nzchar(as.character(role)[1])) return(FALSE)
  snap <- project_snapshot(project)
  paths <- snap$paths
  if (path_ok(paths[[role]])) return(TRUE)
  dat <- snap$data
  if (is.null(dat)) return(FALSE)
  if (!is.null(dat$paths) && path_ok(normalize_paths(dat$paths)[[role]])) return(TRUE)
  switch(as.character(role),
    groups = !is.null(dat$grp) && NROW(dat$grp) > 0,
    fisheries = !is.null(dat$fish) && NROW(dat$fish) > 0,
    output_nc = !is.null(dat$nc_out),
    output_nc_old = !is.null(dat$nc_old),
    catch_nc = !is.null(dat$nc_catch),
    init_nc = !is.null(dat$nc_init),
    prod_nc = !is.null(dat$nc_prod),
    biomindx = !is.null(dat$biomindx) && NROW(dat$biomindx) > 0,
    dietcheck = !is.null(dat$diet_time) || !is.null(dat$diet_full) || isTRUE(dat$diet_deferred),
    diet_bypol = path_ok(paths$diet_bypol),
    ssb = !is.null(dat$ssb) && NROW(dat$ssb) > 0,
    yoy = !is.null(dat$yoy) && NROW(dat$yoy) > 0,
    bio_prm = !is.null(dat$prm_bio) && length(dat$prm_bio) > 0,
    run_prm = !is.null(dat$prm_run) && length(dat$prm_run) > 0,
    force_prm = path_ok(paths$force_prm),
    bgm = path_ok(dat$bgm_path) || !is.null(dat$inf_box),
    specific_mort = !is.null(dat$spe_mort) && NROW(dat$spe_mort) > 0,
    specific_pred_mort = !is.null(dat$pred_mort) && NROW(dat$pred_mort) > 0,
    ext_catch = !is.null(dat$ext_catch),
    obs_csv = !is.null(dat$observations),
    FALSE
  )
}

role_checklist_ui <- function(project) {
  snap <- project_snapshot(project)
  rows <- vector("list", length(FILE_ROLES))
  for (i in seq_along(FILE_ROLES)) {
    role <- FILE_ROLES[[i]]
    ok <- isTRUE(role_is_loaded(snap, role))
    p <- snap$paths[[role]]
    fb <- role %in% as.character(snap$fallback_roles %||% character())
    status <- if (!ok) {
      "not-provided"
    } else if (isTRUE(fb)) {
      "fallback"
    } else {
      "loaded"
    }
    have <- if (!ok) {
      "not provided"
    } else if (isTRUE(fb)) {
      "loaded (other folder)"
    } else {
      "loaded"
    }
    rows[[i]] <- shiny::div(
      class = paste("ra-check-row", if (ok) "is-loaded" else "is-missing",
                    if (isTRUE(fb)) "is-fallback"),
      `data-role` = role,
      `data-status` = status,
      title = if (ok && path_ok(p)) {
        if (isTRUE(fb)) paste0(p, " (found in the other folder)") else p
      } else unname(FILE_LABELS[[role]]),
      shiny::span(class = "ra-dot"),
      shiny::span(class = "ra-check-label", unname(FILE_LABELS[[role]])),
      shiny::span(class = if (ok) "ra-have" else "ra-need", have)
    )
  }
  shiny::div(class = "ra-check", rows)
}

tool_checklist_ui <- function(project) {
  snap <- project_snapshot(project)
  tools <- names(TOOL_REQUIREMENTS)
  rows <- vector("list", length(tools))
  for (i in seq_along(tools)) {
    tool <- tools[[i]]
    miss <- tool_missing(snap$paths, tool, snap)
    ok <- length(miss) == 0 && isTRUE(snap$ready)
    tip <- if (ok) "Ready" else paste("Needs:", paste(miss, collapse = ", "))
    lab <- paste0(toupper(substring(tool, 1, 1)), substring(tool, 2))
    rows[[i]] <- shiny::div(
      class = paste("ra-check-row", if (ok) "is-loaded" else "is-missing"),
      `data-tool` = tool,
      title = tip,
      shiny::span(class = "ra-dot"),
      shiny::span(class = "ra-check-label", lab),
      if (!ok) shiny::span(class = "ra-need", tip)
    )
  }
  shiny::div(class = "ra-check", rows)
}

##' Detect Atlantis files in a folder by suffix and header
##' @param folder Directory path
##' @return Named list of file paths (NA if missing)
detect_project_files <- function(folder) {
  paths <- empty_paths()
  if (is.null(folder) || !dir.exists(folder)) return(paths)
  files <- list.files(folder, full.names = TRUE, recursive = FALSE)
  files <- files[!dir.exists(files)]
  if (length(files) == 0) return(paths)

  paths$biomindx <- pick_first(files, "BiomIndx\\.txt$")
  paths$dietcheck <- pick_first(files, "DietCheck\\.txt$")
  paths$diet_bypol <- pick_first(files, "DietCheck.*(Pol|Box|ByPol).*\\.txt$|Diet.*[Bb]y[Pp]ol")
  paths$ssb <- pick_first(files, "SSB\\.txt$")
  paths$yoy <- pick_first(files, "YOY\\.txt$")
  paths$specific_pred_mort <- pick_first(files, "SpecificPredMort\\.txt$")
  paths$specific_mort <- pick_first(files, "SpecificMort\\.txt$", exclude = "Pred")
  paths$bgm <- pick_first(files, "\\.bgm$")
  paths$catch_nc <- pick_first(files, "CATCH.*\\.nc$|Catch.*\\.nc$")
  paths$prod_nc <- pick_first(files, "PROD.*\\.nc$|Prod.*\\.nc$")
  paths$init_nc <- pick_first(files, "init.*\\.nc$|_ini.*\\.nc$")
  paths$output_nc <- pick_first(files, "\\.nc$",
                                exclude = "CATCH|Catch|init|_ini|restart|PROD|Prod")
  paths$ext_catch <- pick_first(files, "catch.*\\.csv$|observed.*catch")

  csvs <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  for (p in csvs) {
    kind <- classify_csv(p)
    if (isTRUE(kind == "groups") && is.na(paths$groups)) paths$groups <- p
    if (isTRUE(kind == "fisheries") && is.na(paths$fisheries)) paths$fisheries <- p
    if (isTRUE(kind == "obs") && is.na(paths$obs_csv)) paths$obs_csv <- p
  }
  if (is.na(paths$groups)) paths$groups <- pick_first(files, "group.*\\.csv$|functionalGroup")
  if (is.na(paths$fisheries)) paths$fisheries <- pick_first(files, "fish.*\\.csv$")

  prms <- files[grepl("\\.prm$", files, ignore.case = TRUE)]
  for (p in prms) {
    kind <- classify_prm(p)
    if (isTRUE(kind == "bio_prm") && is.na(paths$bio_prm)) paths$bio_prm <- p
    if (isTRUE(kind == "run_prm") && is.na(paths$run_prm)) paths$run_prm <- p
    if (isTRUE(kind == "force_prm") && is.na(paths$force_prm)) paths$force_prm <- p
  }
  if (is.na(paths$bio_prm)) paths$bio_prm <- pick_first(files, "bio.*\\.prm$|biol.*\\.prm$")
  if (is.na(paths$run_prm)) paths$run_prm <- pick_first(files, "run.*\\.prm$")
  if (is.na(paths$force_prm)) {
    paths$force_prm <- pick_first(files, "force.*\\.prm$|forcing.*\\.prm$")
  }

  normalize_paths(paths)
}

##' Detect a run from a configuration folder plus an output folder.
##'
##' Config roles are taken from \code{config_dir} first, output roles from
##' \code{output_dir}. A file found only in the other folder is kept and
##' listed in \code{fallback}. The same path for both folders is the
##' single-folder (backward compatible) case.
detect_run_pair <- function(config_dir = NULL, output_dir = NULL) {
  cfg_dir <- ra_nz(config_dir)
  out_dir <- ra_nz(output_dir)
  if (!nzchar(cfg_dir) && nzchar(out_dir)) cfg_dir <- out_dir
  if (!nzchar(out_dir) && nzchar(cfg_dir)) out_dir <- cfg_dir
  cfg <- if (nzchar(cfg_dir) && dir.exists(cfg_dir)) {
    detect_project_files(cfg_dir)
  } else empty_paths()
  out <- if (nzchar(out_dir) && dir.exists(out_dir)) {
    detect_project_files(out_dir)
  } else empty_paths()
  same <- same_dir(cfg_dir, out_dir)
  paths <- empty_paths()
  fallback <- character()
  take <- function(role, preferred, other) {
    if (path_ok(preferred[[role]])) {
      paths[[role]] <<- preferred[[role]]
    } else if (path_ok(other[[role]])) {
      paths[[role]] <<- other[[role]]
      if (!isTRUE(same)) fallback <<- c(fallback, role)
    }
  }
  for (role in CONFIG_ROLES) take(role, cfg, out)
  for (role in OUTPUT_ROLES) take(role, out, cfg)
  list(
    paths = normalize_paths(paths),
    fallback = unique(fallback),
    same_folder = isTRUE(same),
    config_dir = if (nzchar(cfg_dir)) cfg_dir else NA_character_,
    output_dir = if (nzchar(out_dir)) out_dir else NA_character_
  )
}

scan_note_text <- function(det, verb = "Found") {
  n_found <- sum(vapply(det$paths, path_ok, logical(1)))
  note <- paste0(verb, " ", n_found, " of ", length(FILE_ROLES), " file roles")
  if (isTRUE(det$same_folder)) {
    return(paste0(note, " in one folder."))
  }
  if (length(det$fallback) > 0) {
    labs <- unname(as.character(FILE_LABELS[det$fallback]))
    note <- paste0(note, ". Taken from the other folder: ",
                   paste(labs, collapse = ", "), ".")
  } else {
    note <- paste0(note, " (config + output folders).")
  }
  note
}

read_space_table <- function(path) {
  data.table::fread(path, header = TRUE, sep = " ", showProgress = FALSE)
}

biomass_health <- function(biomindx, grp) {
  if (is.null(biomindx) || nrow(biomindx) < 2) {
    return(data.frame(
      Code = character(), LongName = character(), GroupType = character(),
      Initial = numeric(), Final = numeric(), Ratio = numeric(), Flag = character()
    ))
  }
  biomindx <- data.table::as.data.table(biomindx)
  codes <- as.character(grp$code)
  codes <- codes[codes %in% names(biomindx)]
  if (length(codes) == 0) {
    skip <- c("Time", grep("^Rel", names(biomindx), value = TRUE),
              "PelDemRatio", "PiscivPlankRatio")
    codes <- setdiff(names(biomindx), skip)
  }
  first <- as.numeric(unlist(biomindx[1, codes, with = FALSE], use.names = FALSE))
  last <- as.numeric(unlist(biomindx[nrow(biomindx), codes, with = FALSE], use.names = FALSE))
  ratio <- last / pmax(first, 1e-12)
  flag <- ifelse(!is.finite(ratio), "No data",
          ifelse(ratio < 0.01, "Extinct (<1%)",
          ifelse(ratio > 10, "Exploding (>10x)", "OK")))
  longname <- if ("longname" %in% names(grp)) {
    grp$longname[match(codes, grp$code)]
  } else codes
  gtype <- if ("grouptype" %in% names(grp)) {
    grp$grouptype[match(codes, grp$code)]
  } else NA_character_
  data.frame(
    Code = codes,
    LongName = as.character(longname),
    GroupType = as.character(gtype),
    Initial = first,
    Final = last,
    Ratio = ratio,
    Flag = flag,
    stringsAsFactors = FALSE
  )
}

summarise_run <- function(grp, nc, biomindx, time_dates) {
  n_groups <- if (!is.null(grp) && "isturnedon" %in% names(grp)) {
    sum(grp$isturnedon == 1, na.rm = TRUE)
  } else if (!is.null(grp)) nrow(grp) else NA_integer_
  n_boxes <- NA_integer_
  n_layers <- NA_integer_
  origin <- as.Date(NA)
  if (!is.null(nc)) {
    if (!is.null(nc$dim$b)) n_boxes <- nc$dim$b$len
    if (!is.null(nc$dim$z)) n_layers <- nc$dim$z$len
    origin <- nc_time_origin(nc)
  }
  list(
    n_groups = n_groups,
    n_boxes = n_boxes,
    n_layers = n_layers,
    origin = origin,
    time_span = time_span_label(time_dates)
  )
}

tool_missing <- function(paths, tool, project = NULL) {
  need <- TOOL_REQUIREMENTS[[tool]]
  if (is.null(need)) return(character())
  snap <- if (!is.null(project)) {
    st <- project_snapshot(project)
    if (is.null(st$paths) || !any(vapply(st$paths, path_ok, logical(1)))) {
      st$paths <- normalize_paths(paths)
    }
    st
  } else {
    list(ready = TRUE, paths = normalize_paths(paths), data = NULL)
  }
  loaded <- function(role) role_is_loaded(snap, role)
  miss <- need[!vapply(need, loaded, logical(1))]
  labs <- unname(as.character(FILE_LABELS[miss]))
  if (identical(tool, "compare") && !loaded("biomindx") && !loaded("output_nc")) {
    labs <- unique(c(labs, unname(as.character(FILE_LABELS[c("biomindx", "output_nc")]))))
  }
  labs
}

path_ok <- function(p) {
  x <- as_path(p)
  !is.na(x) && nzchar(x) && file.exists(x)
}

eager_table_max_bytes <- function() {
  as.numeric(getOption("reactiveatlantis.eager_table_bytes", 48 * 1024^2))
}

file_too_big <- function(path) {
  sz <- suppressWarnings(file.size(path))
  is.finite(sz) && sz > eager_table_max_bytes()
}

##' Read DietCheck when load skipped it; path still counts as loaded
ensure_diet <- function(dat, paths = NULL) {
  if (is.null(dat)) return(dat)
  if (!is.null(dat$diet_time) || !is.null(dat$diet_full)) return(dat)
  p <- NA_character_
  if (!is.null(paths)) p <- as_path(normalize_paths(paths)$dietcheck)
  if (!path_ok(p) && path_ok(dat$paths$dietcheck)) p <- as_path(dat$paths$dietcheck)
  if (!path_ok(p)) return(dat)
  agg <- if (exists("diet_cached", mode = "function")) {
    tryCatch(diet_cached(p), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(agg)) {
    diet_raw <- tryCatch(read_space_table(p), error = function(e) NULL)
    if (is.null(diet_raw)) return(dat)
    agg <- tryCatch(preaggregate_diet(diet_raw), error = function(e) NULL)
  }
  if (is.null(agg)) return(dat)
  dat$diet_full <- agg$diet_full
  dat$diet_time <- agg$diet_time
  dat$diet_deferred <- FALSE
  dat
}

upload_path <- function(x) {
  if (is.null(x)) return(NA_character_)
  p <- x$datapath
  if (is.null(p) || !nzchar(p)) return(NA_character_)
  p
}

merge_paths <- function(detected, uploads) {
  out <- detected
  for (role in names(uploads)) {
    if (path_ok(uploads[[role]])) out[[role]] <- uploads[[role]]
  }
  out
}

load_project <- function(paths) {
  paths <- normalize_paths(paths)
  out <- list(
    paths = paths,
    grp = NULL,
    fish = NULL,
    nc_out = NULL,
    nc_old = NULL,
    nc_catch = NULL,
    nc_init = NULL,
    nc_prod = NULL,
    biomindx = NULL,
    diet_full = NULL,
    diet_time = NULL,
    yoy = NULL,
    ssb = NULL,
    spe_mort = NULL,
    pred_mort = NULL,
    prm_bio = NULL,
    prm_run = NULL,
    bgm_path = NULL,
    inf_box = NULL,
    time_dates = NULL,
    origin = as.Date(NA),
    fg_palette = NULL,
    ext_catch = NULL,
    observations = NULL,
    diet_deferred = FALSE,
    errors = character()
  )

  add_err <- function(msg) out$errors <<- c(out$errors, msg)

  if (path_ok(paths$groups)) {
    grp <- utils::read.csv(paths$groups, stringsAsFactors = FALSE)
    names(grp) <- tolower(names(grp))
    if ("code" %in% names(grp)) grp$code <- trimws(as.character(grp$code))
    if ("name" %in% names(grp)) grp$name <- trimws(as.character(grp$name))
    out$grp <- grp
    gtype <- if ("grouptype" %in% names(grp)) grp$grouptype else NULL
    out$fg_palette <- build_fg_palette(grp$code, gtype)
  } else {
    add_err("Groups CSV is required.")
  }

  if (path_ok(paths$fisheries)) {
    out$fish <- normalise_fisheries(utils::read.csv(paths$fisheries, stringsAsFactors = FALSE))
  }

  ## Heavy NetCDF / DietCheck stay on disk until a tab asks (ensure_project_data).
  if (path_ok(paths$bgm)) out$bgm_path <- paths$bgm

  if (path_ok(paths$biomindx)) {
    out$biomindx <- tryCatch(read_space_table(paths$biomindx), error = function(e) {
      add_err(paste("BiomIndx:", e$message)); NULL
    })
  }
  if (path_ok(paths$dietcheck)) {
    out$diet_deferred <- TRUE
  }
  if (path_ok(paths$ext_catch)) {
    out$ext_catch <- tryCatch(utils::read.csv(paths$ext_catch, stringsAsFactors = FALSE),
                              error = function(e) NULL)
  }
  if (path_ok(paths$obs_csv)) {
    out$observations <- tryCatch(read_observations(paths$obs_csv), error = function(e) {
      add_err(paste("Observations:", e$message)); NULL
    })
  }
  if (path_ok(paths$yoy)) {
    out$yoy <- tryCatch(read_yoy_table(paths$yoy), error = function(e) {
      add_err(paste("YOY:", e$message)); NULL
    })
  }
  if (path_ok(paths$ssb)) {
    out$ssb <- tryCatch(read_space_table(paths$ssb), error = function(e) NULL)
  }
  if (path_ok(paths$specific_mort)) {
    out$spe_mort <- tryCatch(read_mort_table(paths$specific_mort), error = function(e) NULL)
  }
  if (path_ok(paths$specific_pred_mort)) {
    out$pred_mort <- tryCatch(read_mort_table(paths$specific_pred_mort), error = function(e) NULL)
  }
  if (path_ok(paths$bio_prm)) {
    out$prm_bio <- tryCatch(readLines(paths$bio_prm, warn = FALSE), error = function(e) NULL)
  }
  if (path_ok(paths$run_prm)) {
    out$prm_run <- tryCatch(readLines(paths$run_prm, warn = FALSE), error = function(e) NULL)
  }
  out
}

run_pair_defaults <- function() {
  one <- getOption("reactiveatlantis.project_dir", "")
  list(
    name = getOption("reactiveatlantis.run_name", ""),
    config_dir = getOption("reactiveatlantis.config_dir", one),
    output_dir = getOption("reactiveatlantis.output_dir", one)
  )
}

run_pair_fields_ui <- function(ns, prefix, title, values = list()) {
  shiny::div(
    class = "ra-run-pair",
    `data-run-prefix` = prefix,
    shiny::h6(title),
    shiny::textInput(
      ns(paste0(prefix, "name")), "Name (optional)",
      value = values$name %||% "",
      placeholder = "e.g. Base, F0.8, NESP"
    ),
    shiny::textInput(
      ns(paste0(prefix, "config_dir")), "Configuration folder",
      value = values$config_dir %||% "",
      placeholder = "/path/to/Atlantis/config"
    ),
    shiny::textInput(
      ns(paste0(prefix, "output_dir")), "Output folder",
      value = values$output_dir %||% "",
      placeholder = "/path/to/Atlantis/output"
    )
  )
}

project_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  defaults <- run_pair_defaults()
  shiny::tagList(
    run_pair_fields_ui(ns, "run1_", "Run 1", defaults),
    shiny::uiOutput(ns("extra_runs")),
    shiny::div(
      class = "ra-btn-row",
      shiny::actionButton(ns("add_run"), "Add another run",
                          class = "btn btn-outline-secondary btn-sm")
    ),
    shiny::div(
      class = "ra-btn-row",
      shiny::actionButton(ns("scan_folder"), "Scan folders",
                          class = "btn btn-outline-primary btn-sm"),
      shiny::actionButton(ns("load_project"), "Load run",
                          class = "btn btn-primary btn-sm")
    ),
    shiny::p(
      class = "ra-muted",
      "Each run is a configuration folder (groups, prm, init, bgm) plus an output folder (NetCDF, BiomIndx, DietCheck). Leave one blank to use the other as a single folder."
    ),
    shiny::div(
      class = "ra-compat-hidden",
      shiny::textInput(ns("folder_path"), NULL,
                       value = getOption("reactiveatlantis.project_dir", "")),
      shiny::textAreaInput(ns("extra_folders"), NULL, rows = 1, value = "")
    ),
    shiny::uiOutput(ns("scan_note")),
    shiny::h6("Loaded files"),
    shiny::uiOutput(ns("file_checklist")),
    shiny::h6("Tools"),
    shiny::uiOutput(ns("tool_checklist")),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Attach individual files",
        shiny::p(class = "ra-muted",
                 "Optional. Skip this when the folders above already have the run. Large NetCDF files should be loaded from disk, not uploaded."),
        shiny::fileInput(ns("up_groups"), "Groups CSV", accept = ".csv"),
        shiny::fileInput(ns("up_fish"), "Fisheries CSV", accept = ".csv"),
        shiny::fileInput(ns("up_output"), "Output .nc", accept = ".nc"),
        shiny::fileInput(ns("up_output_old"), "Previous output .nc", accept = ".nc"),
        shiny::fileInput(ns("up_catch"), "CATCH.nc", accept = ".nc"),
        shiny::fileInput(ns("up_prod"), "PROD.nc", accept = ".nc"),
        shiny::fileInput(ns("up_init"), "Initial .nc", accept = ".nc"),
        shiny::fileInput(ns("up_biom"), "BiomIndx.txt", accept = ".txt"),
        shiny::fileInput(ns("up_diet"), "DietCheck.txt", accept = ".txt"),
        shiny::fileInput(ns("up_yoy"), "YOY.txt", accept = ".txt"),
        shiny::fileInput(ns("up_ssb"), "SSB.txt", accept = ".txt"),
        shiny::fileInput(ns("up_bio_prm"), "Biology .prm", accept = ".prm"),
        shiny::fileInput(ns("up_run_prm"), "Run .prm", accept = ".prm"),
        shiny::fileInput(ns("up_bgm"), "BGM", accept = ".bgm"),
        shiny::fileInput(ns("up_smort"), "SpecificMort.txt", accept = ".txt"),
        shiny::fileInput(ns("up_pmort"), "SpecificPredMort.txt", accept = ".txt"),
        shiny::fileInput(ns("up_ext_catch"), "External catch CSV", accept = ".csv"),
        shiny::fileInput(ns("up_obs"), "Observations CSV", accept = ".csv")
      )
    )
  )
}

project_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      state = empty_state(),
      ready = FALSE,
      paths = empty_paths(),
      data = NULL,
      ensemble = NULL,
      scan_note = NULL
    )
    n_extra <- shiny::reactiveVal(0L)

    input_nz <- function(id) {
      ra_nz(input[[id]])
    }

    existing_val <- function(id, default = "") {
      v <- shiny::isolate(input[[id]])
      x <- ra_nz(v)
      if (nzchar(x)) x else default
    }

    collect_uploads <- function() {
      list(
        groups = upload_path(input$up_groups),
        fisheries = upload_path(input$up_fish),
        output_nc = upload_path(input$up_output),
        output_nc_old = upload_path(input$up_output_old),
        catch_nc = upload_path(input$up_catch),
        prod_nc = upload_path(input$up_prod),
        init_nc = upload_path(input$up_init),
        biomindx = upload_path(input$up_biom),
        dietcheck = upload_path(input$up_diet),
        yoy = upload_path(input$up_yoy),
        ssb = upload_path(input$up_ssb),
        bio_prm = upload_path(input$up_bio_prm),
        run_prm = upload_path(input$up_run_prm),
        bgm = upload_path(input$up_bgm),
        specific_mort = upload_path(input$up_smort),
        specific_pred_mort = upload_path(input$up_pmort),
        ext_catch = upload_path(input$up_ext_catch),
        obs_csv = upload_path(input$up_obs)
      )
    }

    collect_run_pairs <- function() {
      pairs <- list()
      hidden <- input_nz("folder_path")
      cfg1 <- input_nz("run1_config_dir")
      out1 <- input_nz("run1_output_dir")
      if (!nzchar(cfg1) && !nzchar(out1) && nzchar(hidden)) {
        cfg1 <- hidden
        out1 <- hidden
      }
      if (nzchar(cfg1) || nzchar(out1)) {
        pairs[[length(pairs) + 1]] <- list(
          name = input_nz("run1_name"),
          config_dir = cfg1,
          output_dir = out1
        )
      }
      n <- shiny::isolate(n_extra())
      if (n > 0) {
        for (i in seq_len(n)) {
          idx <- i + 1L
          cdir <- input_nz(paste0("extra_config_", idx))
          odir <- input_nz(paste0("extra_output_", idx))
          if (!nzchar(cdir) && !nzchar(odir)) next
          pairs[[length(pairs) + 1]] <- list(
            name = input_nz(paste0("extra_name_", idx)),
            config_dir = cdir,
            output_dir = odir
          )
        }
      }
      extra <- trimws(unlist(strsplit(as.character(input$extra_folders %||% ""), "\n")))
      extra <- extra[nzchar(extra)]
      for (f in extra) {
        pairs[[length(pairs) + 1]] <- list(
          name = basename(f),
          config_dir = f,
          output_dir = f
        )
      }
      pairs
    }

    pair_label <- function(pair, fallback = "Current") {
      nm <- ra_nz(pair$name)
      if (nzchar(nm)) return(nm)
      fallback
    }

    pair_folder <- function(pair) {
      outd <- ra_nz(pair$output_dir)
      if (nzchar(outd)) return(outd)
      ra_nz(pair$config_dir)
    }

    build_ensemble <- function(loaded, pairs) {
      runs <- list()
      p1 <- if (length(pairs) >= 1) pairs[[1]] else list(name = "Current")
      runs[[1]] <- list(
        label = pair_label(p1, "Current"),
        folder = pair_folder(p1),
        config_dir = p1$config_dir,
        output_dir = p1$output_dir,
        paths = loaded$paths,
        biomindx = loaded$biomindx,
        yoy = loaded$yoy,
        ssb = loaded$ssb,
        catch_nc = loaded$nc_catch
      )
      if (length(pairs) > 1) {
        for (i in 2:length(pairs)) {
          p <- pairs[[i]]
          runs[[length(runs) + 1]] <- load_run_lite(
            folder = pair_folder(p),
            label = p$name,
            config_dir = p$config_dir,
            output_dir = p$output_dir
          )
        }
      }
      runs
    }

    commit <- function(paths = NULL, data = NULL, ensemble = NULL, note = NULL,
                       ready = NULL, touch_data = FALSE,
                       fallback_roles = NULL, config_dir = NULL,
                       output_dir = NULL, run_pairs = NULL) {
      prev <- shiny::isolate(rv$state)
      if (is.null(prev) || !is.list(prev)) prev <- empty_state()
      if (!is.null(paths)) prev$paths <- normalize_paths(paths)
      if (isTRUE(touch_data)) {
        prev$data <- data
        prev$ensemble <- ensemble
      }
      if (!is.null(note)) prev$scan_note <- note
      if (!is.null(ready)) prev$ready <- isTRUE(ready)
      if (!is.null(fallback_roles)) prev$fallback_roles <- as.character(fallback_roles)
      if (!is.null(config_dir)) prev$config_dir <- config_dir
      if (!is.null(output_dir)) prev$output_dir <- output_dir
      if (!is.null(run_pairs)) prev$run_pairs <- run_pairs
      prev$gen <- prev$gen + 1L
      ## One list assignment so every module sees paths, data and ready together.
      rv$state <- prev
      rv$ready <- prev$ready
      rv$paths <- prev$paths
      rv$data <- prev$data
      rv$ensemble <- prev$ensemble
      rv$scan_note <- prev$scan_note
      prev
    }

    apply_detection <- function(det, verb = "Found", extra_n = 0L) {
      paths <- normalize_paths(merge_paths(det$paths, collect_uploads()))
      note <- scan_note_text(det, verb)
      if (extra_n > 0) {
        note <- paste0(note, " Plus ", extra_n, " extra run",
                       if (extra_n == 1L) "." else "s.")
      }
      commit(
        paths = paths,
        note = note,
        fallback_roles = det$fallback,
        config_dir = det$config_dir,
        output_dir = det$output_dir
      )
      list(paths = paths, note = note, det = det)
    }

    shiny::observeEvent(input$add_run, {
      n_extra(n_extra() + 1L)
    }, ignoreInit = TRUE)

    output$extra_runs <- shiny::renderUI({
      n <- n_extra()
      if (n < 1) return(NULL)
      ns <- session$ns
      lapply(seq_len(n), function(i) {
        idx <- i + 1L
        shiny::div(
          class = "ra-run-pair",
          `data-run-index` = idx,
          shiny::h6(paste("Run", idx)),
          shiny::textInput(
            ns(paste0("extra_name_", idx)), "Name (optional)",
            value = existing_val(paste0("extra_name_", idx)),
            placeholder = "e.g. F0.8"
          ),
          shiny::textInput(
            ns(paste0("extra_config_", idx)), "Configuration folder",
            value = existing_val(paste0("extra_config_", idx)),
            placeholder = "/path/to/run/config"
          ),
          shiny::textInput(
            ns(paste0("extra_output_", idx)), "Output folder",
            value = existing_val(paste0("extra_output_", idx)),
            placeholder = "/path/to/run/output"
          )
        )
      })
    })

    shiny::observeEvent(input$folder_path, {
      folder <- ra_nz(input$folder_path)
      if (!nzchar(folder) || !dir.exists(folder)) return()
      if (isTRUE(shiny::isolate(rv$state$ready))) return()
      cfg <- input_nz("run1_config_dir")
      out <- input_nz("run1_output_dir")
      if (!nzchar(cfg) && !nzchar(out)) {
        shiny::updateTextInput(session, "run1_config_dir", value = folder)
        shiny::updateTextInput(session, "run1_output_dir", value = folder)
      }
      det <- detect_run_pair(folder, folder)
      apply_detection(det, verb = "Found")
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$scan_folder, {
      pairs <- collect_run_pairs()
      if (length(pairs) == 0) {
        shiny::showNotification(
          "Enter a configuration folder and/or an output folder.",
          type = "error", duration = 8
        )
        return()
      }
      p1 <- pairs[[1]]
      cfg <- ra_nz(p1$config_dir)
      outd <- ra_nz(p1$output_dir)
      if ((!nzchar(cfg) || !dir.exists(cfg)) && (!nzchar(outd) || !dir.exists(outd))) {
        shiny::showNotification(
          "Folder not found. Enter a local path that this machine can read.",
          type = "error", duration = 8
        )
        return()
      }
      scanned <- ra_progress("Scanning folders…", {
        ra_inc(0.25, detail = "Detecting Atlantis files")
        det <- detect_run_pair(p1$config_dir, p1$output_dir)
        ra_inc(0.55, detail = "Updating checklist")
        apply_detection(det, verb = "Scan:", extra_n = max(0L, length(pairs) - 1L))
      })
      n_found <- sum(vapply(scanned$paths, path_ok, logical(1)))
      shiny::showNotification(
        paste("Found", n_found, "Atlantis file roles."),
        type = "message", duration = 5
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$load_project, {
      shiny::showNotification("Loading run…", type = "message", id = "load_run",
                              duration = NULL)
      pairs <- collect_run_pairs()
      loaded <- ra_progress("Loading run…", {
        ra_inc(0.1, detail = "Scanning folders")
        det <- if (length(pairs) >= 1) {
          detect_run_pair(pairs[[1]]$config_dir, pairs[[1]]$output_dir)
        } else {
          detect_run_pair(NULL, NULL)
        }
        paths <- normalize_paths(merge_paths(det$paths, collect_uploads()))
        ra_inc(0.25, detail = "Reading tables")
        out <- tryCatch(load_project(paths), error = function(e) e)
        if (inherits(out, "error")) {
          return(list(error = out, paths = paths, det = det, pairs = pairs))
        }
        ra_inc(0.55, detail = "Loading extra runs")
        ens <- build_ensemble(out, pairs)
        ra_inc(0.85, detail = "Updating tools")
        list(data = out, paths = paths, det = det, pairs = pairs, ensemble = ens)
      })
      shiny::removeNotification("load_run")
      if (!is.null(loaded$error)) {
        shiny::showNotification(paste("Could not load run:", loaded$error$message),
                                type = "error", duration = 12)
        return()
      }
      if (is.null(loaded$data)) return()
      n_found <- sum(vapply(loaded$paths, path_ok, logical(1)))
      extra_n <- max(0L, length(loaded$pairs) - 1L)
      note <- scan_note_text(loaded$det, "Loaded")
      if (extra_n > 0) {
        note <- paste0(note, " Plus ", extra_n, " extra run",
                       if (extra_n == 1L) "." else "s.")
      }
      commit(
        paths = loaded$paths,
        data = loaded$data,
        ensemble = loaded$ensemble,
        note = note,
        ready = !is.null(loaded$data$grp),
        touch_data = TRUE,
        fallback_roles = loaded$det$fallback,
        config_dir = loaded$det$config_dir,
        output_dir = loaded$det$output_dir,
        run_pairs = loaded$pairs
      )
      msg <- if (length(loaded$data$errors)) {
        paste("Loaded with notes:", paste(loaded$data$errors, collapse = "; "))
      } else {
        "Run loaded. Tools with the required files are ready."
      }
      shiny::showNotification(
        msg,
        type = if (length(loaded$data$errors)) "warning" else "message",
        duration = 6
      )
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    output$scan_note <- shiny::renderUI({
      note <- rv$state$scan_note
      if (is.null(note) || !nzchar(note)) return(NULL)
      shiny::tags$p(class = "ra-scan-note", note)
    })

    output$file_checklist <- shiny::renderUI({
      role_checklist_ui(rv$state)
    })

    output$tool_checklist <- shiny::renderUI({
      tool_checklist_ui(rv$state)
    })

    keep_outputs_alive(output, c("scan_note", "file_checklist", "tool_checklist",
                                 "extra_runs"))

    rv
  })
}
