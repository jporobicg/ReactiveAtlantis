## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Shared Atlantis run / project store    ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

FILE_ROLES <- c(
  "groups", "fisheries", "output_nc", "output_nc_old", "catch_nc", "init_nc",
  "biomindx", "dietcheck", "diet_bypol", "ssb", "yoy",
  "bio_prm", "run_prm", "bgm", "specific_mort", "specific_pred_mort", "ext_catch"
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
  bgm = "BGM geometry",
  specific_mort = "SpecificMort.txt",
  specific_pred_mort = "SpecificPredMort.txt",
  ext_catch = "External catch CSV"
)

TOOL_REQUIREMENTS <- list(
  overview = c("groups"),
  compare = c("groups", "output_nc", "bgm"),
  predation = c("groups", "biomindx", "dietcheck"),
  foodweb = c("groups", "dietcheck"),
  recruitment = c("groups", "output_nc", "yoy", "bio_prm"),
  growth = c("groups", "output_nc", "bio_prm"),
  catch = c("groups", "fisheries", "catch_nc"),
  feeding = c("groups", "bio_prm", "init_nc"),
  mortality = c("groups", "specific_mort", "specific_pred_mort")
)

empty_paths <- function() {
  setNames(as.list(rep(NA_character_, length(FILE_ROLES))), FILE_ROLES)
}

pick_first <- function(files, pattern, exclude = NULL) {
  bn <- basename(files)
  hit <- grepl(pattern, bn, ignore.case = TRUE)
  if (!is.null(exclude)) hit <- hit & !grepl(exclude, bn, ignore.case = TRUE)
  cand <- files[hit]
  if (length(cand) == 0) return(NA_character_)
  cand[order(nchar(basename(cand)))][1]
}

classify_csv <- function(path) {
  hdr <- tryCatch(tolower(names(utils::read.csv(path, nrows = 0, check.names = FALSE))),
                  error = function(e) character())
  if (length(hdr) == 0) return(NA_character_)
  if (("code" %in% hdr || "code" %in% gsub("[^a-z]", "", hdr)) &&
      any(c("numcohorts", "isturnedon", "grouptype", "longname") %in% hdr)) {
    return("groups")
  }
  if ("index" %in% hdr && "code" %in% hdr) return("fisheries")
  NA_character_
}

classify_prm <- function(path) {
  lines <- tryCatch(readLines(path, n = 400, warn = FALSE), error = function(e) character())
  if (any(grepl("pPREY", lines, ignore.case = TRUE))) return("bio_prm")
  if (any(grepl("toutinc|flagrecruit|tinc", lines, ignore.case = TRUE))) return("run_prm")
  NA_character_
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
  paths$init_nc <- pick_first(files, "init.*\\.nc$|_ini.*\\.nc$")
  paths$output_nc <- pick_first(files, "\\.nc$", exclude = "CATCH|Catch|init|_ini|restart")
  paths$ext_catch <- pick_first(files, "catch.*\\.csv$|observed.*catch")

  csvs <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  for (p in csvs) {
    kind <- classify_csv(p)
    if (isTRUE(kind == "groups") && is.na(paths$groups)) paths$groups <- p
    if (isTRUE(kind == "fisheries") && is.na(paths$fisheries)) paths$fisheries <- p
  }
  if (is.na(paths$groups)) paths$groups <- pick_first(files, "group.*\\.csv$|functionalGroup")
  if (is.na(paths$fisheries)) paths$fisheries <- pick_first(files, "fish.*\\.csv$")

  prms <- files[grepl("\\.prm$", files, ignore.case = TRUE)]
  for (p in prms) {
    kind <- classify_prm(p)
    if (isTRUE(kind == "bio_prm") && is.na(paths$bio_prm)) paths$bio_prm <- p
    if (isTRUE(kind == "run_prm") && is.na(paths$run_prm)) paths$run_prm <- p
  }
  if (is.na(paths$bio_prm)) paths$bio_prm <- pick_first(files, "bio.*\\.prm$|biol.*\\.prm$")
  if (is.na(paths$run_prm)) paths$run_prm <- pick_first(files, "run.*\\.prm$")

  paths
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

tool_missing <- function(paths, tool) {
  need <- TOOL_REQUIREMENTS[[tool]]
  if (is.null(need)) return(character())
  missing <- need[is.na(unlist(paths[need], use.names = FALSE)) | !nzchar(unlist(paths[need], use.names = FALSE))]
  unname(FILE_LABELS[missing])
}

path_ok <- function(p) {
  !is.null(p) && length(p) == 1 && !is.na(p) && nzchar(p) && file.exists(p)
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
  out <- list(
    paths = paths,
    grp = NULL,
    fish = NULL,
    nc_out = NULL,
    nc_old = NULL,
    nc_catch = NULL,
    nc_init = NULL,
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

  if (path_ok(paths$output_nc)) {
    out$nc_out <- tryCatch(ncdf4::nc_open(paths$output_nc), error = function(e) {
      add_err(paste("Output NetCDF:", e$message)); NULL
    })
    if (!is.null(out$nc_out)) {
      out$time_dates <- tryCatch(time_calc(out$nc_out), error = function(e) NULL)
      out$origin <- nc_time_origin(out$nc_out)
    }
  }
  if (path_ok(paths$output_nc_old)) {
    out$nc_old <- tryCatch(ncdf4::nc_open(paths$output_nc_old), error = function(e) NULL)
  }
  if (path_ok(paths$catch_nc)) {
    out$nc_catch <- tryCatch(ncdf4::nc_open(paths$catch_nc), error = function(e) NULL)
  }
  if (path_ok(paths$init_nc)) {
    out$nc_init <- tryCatch(ncdf4::nc_open(paths$init_nc), error = function(e) NULL)
  }

  if (path_ok(paths$biomindx)) {
    out$biomindx <- tryCatch(read_space_table(paths$biomindx), error = function(e) {
      add_err(paste("BiomIndx:", e$message)); NULL
    })
  }
  if (path_ok(paths$dietcheck)) {
    diet_raw <- tryCatch(read_space_table(paths$dietcheck), error = function(e) {
      add_err(paste("DietCheck:", e$message)); NULL
    })
    if (!is.null(diet_raw)) {
      agg <- preaggregate_diet(diet_raw)
      out$diet_full <- agg$diet_full
      out$diet_time <- agg$diet_time
    }
  }
  if (path_ok(paths$ext_catch)) {
    out$ext_catch <- tryCatch(utils::read.csv(paths$ext_catch, stringsAsFactors = FALSE),
                              error = function(e) NULL)
  }
  if (path_ok(paths$yoy)) {
    out$yoy <- tryCatch(utils::read.csv(paths$yoy, sep = " ", stringsAsFactors = FALSE),
                        error = function(e) NULL)
  }
  if (path_ok(paths$ssb)) {
    out$ssb <- tryCatch(read_space_table(paths$ssb), error = function(e) NULL)
  }
  if (path_ok(paths$specific_mort)) {
    out$spe_mort <- tryCatch(utils::read.csv(paths$specific_mort, sep = " "), error = function(e) NULL)
  }
  if (path_ok(paths$specific_pred_mort)) {
    out$pred_mort <- tryCatch(utils::read.csv(paths$specific_pred_mort, sep = " "), error = function(e) NULL)
  }
  if (path_ok(paths$bio_prm)) {
    out$prm_bio <- tryCatch(readLines(paths$bio_prm, warn = FALSE), error = function(e) NULL)
  }
  if (path_ok(paths$run_prm)) {
    out$prm_run <- tryCatch(readLines(paths$run_prm, warn = FALSE), error = function(e) NULL)
  }
  if (path_ok(paths$bgm) && !is.null(out$nc_out)) {
    depths <- infer_cum_depths(out$nc_out)
    if (!is.null(depths)) {
      out$inf_box <- tryCatch(boxes.prop(paths$bgm, depths), error = function(e) NULL)
    }
    out$bgm_path <- paths$bgm
  } else if (path_ok(paths$bgm)) {
    out$bgm_path <- paths$bgm
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
             "A local folder path avoids browser upload limits for large NetCDF and DietCheck files."),
    shiny::h6("Loaded files"),
    shiny::uiOutput(ns("file_checklist")),
    shiny::h6("Tools"),
    shiny::uiOutput(ns("tool_checklist")),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Attach individual files",
        shiny::p(class = "ra-muted", "Used when a folder is not available on this machine."),
        shiny::fileInput(ns("up_groups"), "Groups CSV", accept = ".csv"),
        shiny::fileInput(ns("up_fish"), "Fisheries CSV", accept = ".csv"),
        shiny::fileInput(ns("up_output"), "Output .nc", accept = ".nc"),
        shiny::fileInput(ns("up_output_old"), "Previous output .nc", accept = ".nc"),
        shiny::fileInput(ns("up_catch"), "CATCH.nc", accept = ".nc"),
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
        shiny::fileInput(ns("up_ext_catch"), "External catch CSV", accept = ".csv")
      )
    )
  )
}

project_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      ready = FALSE,
      paths = empty_paths(),
      data = NULL
    )

    collect_uploads <- function() {
      list(
        groups = upload_path(input$up_groups),
        fisheries = upload_path(input$up_fish),
        output_nc = upload_path(input$up_output),
        output_nc_old = upload_path(input$up_output_old),
        catch_nc = upload_path(input$up_catch),
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
        ext_catch = upload_path(input$up_ext_catch)
      )
    }

    try_autoload <- function() {
      folder <- trimws(input$folder_path)
      if (!nzchar(folder) || !dir.exists(folder) || isTRUE(rv$ready)) return()
      detected <- detect_project_files(folder)
      paths <- merge_paths(detected, collect_uploads())
      loaded <- tryCatch(load_project(paths), error = function(e) NULL)
      if (is.null(loaded) || is.null(loaded$grp)) return()
      rv$paths <- paths
      rv$data <- loaded
      rv$ready <- TRUE
    }

    auto_done <- FALSE
    shiny::observeEvent(input$folder_path, {
      if (isTRUE(auto_done) || isTRUE(rv$ready)) return()
      folder <- trimws(input$folder_path)
      if (!nzchar(folder) || !dir.exists(folder)) return()
      auto_done <<- TRUE
      try_autoload()
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
      rv$paths <- merge_paths(detected, collect_uploads())
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
      paths <- merge_paths(detected, collect_uploads())
      loaded <- tryCatch(load_project(paths), error = function(e) {
        shiny::removeNotification("load_run")
        shiny::showNotification(paste("Could not load run:", e$message), type = "error", duration = 12)
        NULL
      })
      shiny::removeNotification("load_run")
      if (is.null(loaded)) return()
      rv$paths <- paths
      rv$data <- loaded
      rv$ready <- !is.null(loaded$grp)
      msg <- if (length(loaded$errors)) {
        paste("Loaded with notes:", paste(loaded$errors, collapse = "; "))
      } else {
        "Run loaded. Tools with the required files are ready."
      }
      shiny::showNotification(msg, type = if (length(loaded$errors)) "warning" else "message", duration = 6)
    }, ignoreInit = TRUE)

    output$file_checklist <- shiny::renderUI({
      paths <- rv$paths
      shiny::tags$ul(
        class = "ra-check",
        lapply(FILE_ROLES, function(role) {
          ok <- path_ok(paths[[role]])
          shiny::tags$li(
            class = if (ok) "ok" else "missing",
            title = if (ok) paths[[role]] else paste("Not loaded:", FILE_LABELS[[role]]),
            shiny::span(class = "ra-dot"),
            FILE_LABELS[[role]]
          )
        })
      )
    })

    output$tool_checklist <- shiny::renderUI({
      paths <- rv$paths
      tools <- names(TOOL_REQUIREMENTS)
      shiny::tags$ul(
        class = "ra-check",
        lapply(tools, function(tool) {
          miss <- tool_missing(paths, tool)
          ok <- length(miss) == 0 && isTRUE(rv$ready)
          tip <- if (ok) "Ready" else paste("Needs:", paste(miss, collapse = ", "))
          shiny::tags$li(
            class = if (ok) "ok" else "missing",
            title = tip,
            shiny::span(class = "ra-dot"),
            paste0(toupper(substring(tool, 1, 1)), substring(tool, 2)),
            if (!ok) shiny::span(class = "ra-need", tip)
          )
        })
      )
    })

    rv
  })
}
