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
  ensemble = c("groups")
)

empty_paths <- function() {
  setNames(as.list(rep(NA_character_, length(FILE_ROLES))), FILE_ROLES)
}

empty_state <- function() {
  list(
    ready = FALSE,
    paths = empty_paths(),
    data = NULL,
    ensemble = NULL,
    scan_note = NULL,
    gen = 0L
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
      return(st)
    }
    return(list(
      ready = isTRUE(project$ready),
      paths = normalize_paths(project$paths),
      data = project$data,
      ensemble = project$ensemble,
      scan_note = project$scan_note,
      gen = 0L
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
      gen = if (is.null(project$gen)) 0L else project$gen
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
    rows[[i]] <- shiny::div(
      class = paste("ra-check-row", if (ok) "is-loaded" else "is-missing"),
      `data-role` = role,
      `data-status` = if (ok) "loaded" else "not-provided",
      title = if (ok && path_ok(p)) p else unname(FILE_LABELS[[role]]),
      shiny::span(class = "ra-dot"),
      shiny::span(class = "ra-check-label", unname(FILE_LABELS[[role]])),
      shiny::span(class = if (ok) "ra-have" else "ra-need",
                  if (ok) "loaded" else "not provided")
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

project_sidebar_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::textInput(ns("folder_path"), "Local run folder",
                     value = getOption("reactiveatlantis.project_dir", ""),
                     placeholder = "/path/to/Atlantis/output"),
    shiny::div(
      class = "ra-btn-row",
      shiny::actionButton(ns("scan_folder"), "Scan folder", class = "btn btn-outline-primary btn-sm"),
      shiny::actionButton(ns("load_project"), "Load run", class = "btn btn-primary btn-sm")
    ),
    shiny::p(class = "ra-muted",
             "Prefer a local folder. Uploads are optional and some browsers stop around 500 MB; a North Sea output.nc is larger than that."),
    shiny::textAreaInput(ns("extra_folders"), "Other run folders (one per line)",
                         rows = 3, placeholder = "/path/to/run2\n/path/to/run3"),
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
                 "Optional. Skip this when the folder path above already has the run. Large NetCDF files should be loaded from disk, not uploaded."),
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

    build_ensemble <- function(loaded, folder) {
      runs <- list()
      runs[[1]] <- list(
        label = "Current",
        folder = folder,
        paths = loaded$paths,
        biomindx = loaded$biomindx,
        yoy = loaded$yoy,
        ssb = loaded$ssb,
        catch_nc = loaded$nc_catch
      )
      extra <- trimws(unlist(strsplit(as.character(input$extra_folders), "\n")))
      extra <- extra[nzchar(extra) & dir.exists(extra)]
      for (f in extra) {
        runs[[length(runs) + 1]] <- load_run_lite(f)
      }
      runs
    }

    commit <- function(paths = NULL, data = NULL, ensemble = NULL, note = NULL,
                       ready = NULL, touch_data = FALSE) {
      prev <- shiny::isolate(rv$state)
      if (is.null(prev) || !is.list(prev)) prev <- empty_state()
      if (!is.null(paths)) prev$paths <- normalize_paths(paths)
      if (isTRUE(touch_data)) {
        prev$data <- data
        prev$ensemble <- ensemble
      }
      if (!is.null(note)) prev$scan_note <- note
      if (!is.null(ready)) prev$ready <- isTRUE(ready)
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

    shiny::observeEvent(input$folder_path, {
      folder <- trimws(input$folder_path)
      if (!nzchar(folder) || !dir.exists(folder)) return()
      if (isTRUE(shiny::isolate(rv$state$ready))) return()
      detected <- detect_project_files(folder)
      n_found <- sum(vapply(detected, path_ok, logical(1)))
      commit(
        paths = normalize_paths(merge_paths(detected, collect_uploads())),
        note = paste0("Found ", n_found, " of ", length(FILE_ROLES),
                      " file roles. Click Load run to open them.")
      )
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$scan_folder, {
      folder <- trimws(input$folder_path)
      if (!nzchar(folder) || !dir.exists(folder)) {
        shiny::showNotification("Folder not found. Enter a local path that this machine can read.",
                                type = "error", duration = 8)
        return()
      }
      detected <- detect_project_files(folder)
      n_found <- sum(vapply(detected, path_ok, logical(1)))
      commit(
        paths = normalize_paths(merge_paths(detected, collect_uploads())),
        note = paste0("Scan: ", n_found, " of ", length(FILE_ROLES),
                      " file roles found. Click Load run to open them.")
      )
      shiny::showNotification(paste("Found", n_found, "Atlantis files in the folder."),
                              type = "message", duration = 5)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$load_project, {
      shiny::showNotification("Loading run…", type = "message", id = "load_run", duration = NULL)
      detected <- empty_paths()
      folder <- trimws(input$folder_path)
      if (nzchar(folder) && dir.exists(folder)) {
        detected <- detect_project_files(folder)
      }
      paths <- normalize_paths(merge_paths(detected, collect_uploads()))
      loaded <- tryCatch(load_project(paths), error = function(e) {
        shiny::removeNotification("load_run")
        shiny::showNotification(paste("Could not load run:", e$message), type = "error", duration = 12)
        NULL
      })
      shiny::removeNotification("load_run")
      if (is.null(loaded)) return()
      n_found <- sum(vapply(paths, path_ok, logical(1)))
      commit(
        paths = paths,
        data = loaded,
        ensemble = build_ensemble(loaded, folder),
        note = paste0("Loaded ", n_found, " of ", length(FILE_ROLES), " file roles."),
        ready = !is.null(loaded$grp),
        touch_data = TRUE
      )
      msg <- if (length(loaded$errors)) {
        paste("Loaded with notes:", paste(loaded$errors, collapse = "; "))
      } else {
        "Run loaded. Tools with the required files are ready."
      }
      shiny::showNotification(msg, type = if (length(loaded$errors)) "warning" else "message", duration = 6)
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

    keep_outputs_alive(output, c("scan_note", "file_checklist", "tool_checklist"))

    rv
  })
}
