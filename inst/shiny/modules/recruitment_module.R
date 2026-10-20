## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~         Recruitment Analysis Module        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

recruitment_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("body")))
}

recruitment_body_ui <- function(ns, grp, rec, n_box) {
  age_groups <- grp[grp$numcohorts > 1, ]
  pp.pos <- with(grp, which(grouptype %in% c("MED_ZOO", "LG_ZOO", "LG_PHY", "SM_PHY",
                                             "PHYTOBEN", "DINOFLAG", "TURF") &
                            isturnedon == 1))
  pp.cod <- as.character(grp$code[pp.pos])
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "open",
      selectInput(ns("analysis_type"), "View",
                  choices = c("Recruits and YOY" = "yoy",
                              "Spawning stock biomass" = "ssb",
                              "Zooplankton and primary producers" = "growth")),
      conditionalPanel(
        "input.analysis_type == 'yoy' || input.analysis_type == 'ssb'", ns = ns,
        selectInput(ns("fg_yoy"), "Functional group",
                    choices = as.character(age_groups$code))
      ),
      conditionalPanel(
        "input.analysis_type == 'yoy'", ns = ns,
        textInput(ns("new_alpha"), "New alpha", value = "", placeholder = "7.3e9"),
        textInput(ns("new_beta"), "New beta", value = "", placeholder = "4.5e11"),
        actionButton(ns("recalc"), "Recalculate", class = "btn btn-primary btn-sm"),
        p(class = "ra-muted",
          "Atlantis YOY (CODE.0 in YOY.txt) plus larvae from the Beverton–Holt curve. Recalculate redraws new larvae and new YOY from the alpha and beta you type (scientific notation is fine).")
      ),
      conditionalPanel(
        "input.analysis_type == 'ssb'", ns = ns,
        p(class = "ra-muted", "SSB.txt from the run, with observed SSB ribbons when an observations CSV is loaded.")
      ),
      conditionalPanel(
        "input.analysis_type == 'growth'", ns = ns,
        selectInput(ns("fg_growth"), "Functional group", choices = pp.cod),
        selectInput(ns("box"), "Box", choices = seq(0, max(0, n_box - 1))),
        checkboxInput(ns("layer_prop"), "Layer proportion", TRUE),
        checkboxInput(ns("box_prop"), "Box proportion", FALSE),
        checkboxInput(ns("log_scale"), "Logarithmic", FALSE)
      )
    ),
    uiOutput(ns("analysis_output"))
  )
}

recruitment_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(rec_params = NULL, pp_list = NULL, n_box = 1, built = FALSE,
                         token = NULL, alpha_use = NA_real_, beta_use = NA_real_,
                         fspb = NULL)

    output$body <- renderUI({
      if (is.null(project) || !isTRUE(project$ready)) {
        return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS$recruitment], "Recruitment"))
      }
      miss <- tool_missing(project$paths, "recruitment")
      if (length(miss)) return(tool_missing_card(miss, "Recruitment"))
      recruitment_body_ui(session$ns, project$data$grp, rv$rec_params, rv$n_box)
    })

    observe({
      req(!is.null(project), isTRUE(project$ready), project$data)
      miss <- tool_missing(project$paths, "recruitment")
      token <- paste(unlist(project$paths), collapse = "|")
      if (length(miss) || (isTRUE(rv$built) && identical(rv$token, token))) return()
      rv$token <- token
      dat <- project$data
      req(dat$nc_out, dat$grp, dat$prm_bio, dat$yoy)
      grp <- dat$grp
      prm <- dat$prm_bio
      nc.out <- dat$nc_out

      sp.dat <- with(grp, which(isturnedon == 1 & numcohorts > 1))
      options(warn = -1)
      rec <- text2num(prm, "^flagrecruit", FG = "look")
      options(warn = 0)
      rec <- rec[complete.cases(rec), ]
      rec <- cbind(rec, Alpha = NA, Beta = NA, KSPA = NA, FSP = NA, Time.sp = NA,
                   XCN = NA, XRS = NA, Rec.SNW = NA, Rec.RNW = NA)
      sps <- gsub(pattern = "^flagrecruit", "", rec$FG)
      rec$FG <- sps
      prm_num <- function(pat) {
        val <- tryCatch(suppressWarnings(text2num(prm, pat, FG = "look")[1, 2]),
                        error = function(e) NA_real_)
        if (length(val) == 0) NA_real_ else as.numeric(val)
      }
      xcn <- prm_num("X_CN")
      xrs <- prm_num("X_RS")
      fspb_list <- list()
      for (fg.r in seq_along(sps)) {
        if (!sps[fg.r] %in% grp$code[sp.dat]) next()
        rec$XCN[fg.r] <- xcn
        rec$XRS[fg.r] <- xrs
        rec$Time.sp[fg.r] <- prm_num(paste0(sps[fg.r], "_Time_Spawn"))
        if (rec$Value[fg.r] %in% c(3, 10)) {
          rec$Alpha[fg.r] <- prm_num(paste0("BHalpha_", sps[fg.r]))
          rec$Beta[fg.r] <- prm_num(paste0("BHbeta_", sps[fg.r]))
        }
        if (rec$Value[fg.r] == 3) {
          rec$KSPA[fg.r] <- prm_num(paste0("KSPA_", sps[fg.r]))
          rec$FSP[fg.r] <- prm_num(paste0("FSP_", sps[fg.r]))
          rec$Rec.SNW[fg.r] <- prm_num(paste0("KWSR_", sps[fg.r]))
          rec$Rec.RNW[fg.r] <- prm_num(paste0("KWRR_", sps[fg.r]))
          fspb.tmp <- tryCatch(suppressWarnings(
            text2num(prm, paste0("FSPB_", sps[fg.r]), FG = sps[fg.r], Vector = TRUE)
          ), error = function(e) NULL)
          if (!is.null(fspb.tmp)) fspb_list[[sps[fg.r]]] <- as.numeric(fspb.tmp)
        }
      }
      rv$fspb <- fspb_list

      pp.pos <- with(grp, which(grouptype %in% c("MED_ZOO", "LG_ZOO", "LG_PHY", "SM_PHY",
                                                 "PHYTOBEN", "DINOFLAG", "TURF") &
                                isturnedon == 1))
      pp.list <- list()
      if (length(pp.pos) > 0) {
        pp.fg <- grp$name[pp.pos]
        pp.cod <- as.character(grp$code[pp.pos])
        for (l.pp in seq_along(pp.fg)) {
          pp.list[[l.pp]] <- ncdf4::ncvar_get(nc.out, paste0(pp.fg[l.pp], "_N"))
          names(pp.list)[l.pp] <- pp.cod[l.pp]
        }
      }
      quiet_nc <- function(name) {
        tryCatch({
          capture.output(val <- ncdf4::ncvar_get(nc.out, name))
          val
        }, error = function(e) NULL)
      }
      pp.list[["Light"]] <- quiet_nc("Light")
      pp.list[["Eddy"]] <- quiet_nc("eddy")

      n_box <- 1
      if (!is.null(nc.out$dim$b)) n_box <- nc.out$dim$b$len
      rv$rec_params <- rec
      rv$pp_list <- pp.list
      rv$n_box <- n_box
      rv$built <- TRUE
    })

    fill_bh_inputs <- function(fg) {
      rec_row <- which(rv$rec_params$FG == fg)
      if (length(rec_row) == 0) return()
      updateTextInput(session, "new_alpha", value = sci_txt(rv$rec_params$Alpha[rec_row[1]]))
      updateTextInput(session, "new_beta", value = sci_txt(rv$rec_params$Beta[rec_row[1]]))
      rv$alpha_use <- rv$rec_params$Alpha[rec_row[1]]
      rv$beta_use <- rv$rec_params$Beta[rec_row[1]]
    }

    observeEvent(input$fg_yoy, {
      req(rv$rec_params, input$fg_yoy)
      fill_bh_inputs(input$fg_yoy)
    }, ignoreInit = FALSE)

    observeEvent(input$recalc, {
      rv$alpha_use <- parse_sci(input$new_alpha)
      rv$beta_use <- parse_sci(input$new_beta)
    }, ignoreInit = TRUE)

    output$analysis_output <- renderUI({
      ns <- session$ns
      req(input$analysis_type)
      if (input$analysis_type == "yoy") {
        tagList(
          plotOutput(ns("plot_yoy"), height = "340px"),
          plotOutput(ns("plot_relative_yoy"), height = "340px")
        )
      } else if (input$analysis_type == "ssb") {
        plotOutput(ns("plot_ssb"), height = "480px")
      } else {
        plotOutput(ns("plot_pp_growth"), height = "640px")
      }
    })

    yoy_series <- reactive({
      dat <- project$data
      req(dat$yoy, input$fg_yoy)
      col <- find_yoy_column(dat$yoy, input$fg_yoy)
      if (is.na(col)) return(NULL)
      val <- as.numeric(dat$yoy[[col]])
      tdays <- as.numeric(dat$yoy$Time)
      if (length(tdays) != length(val)) tdays <- seq_along(val) - 1
      first <- val[which(is.finite(val))[1]]
      rel <- if (is.finite(first) && abs(first) > 1e-12) val / first else rep(NA_real_, length(val))
      list(time = tdays, original = val, relative = rel, column = col)
    })

    rec_overlay <- reactive({
      dat <- project$data
      req(input$fg_yoy, rv$rec_params)
      yoy <- yoy_series()
      rec_row <- rv$rec_params[rv$rec_params$FG == input$fg_yoy, , drop = FALSE]
      alpha0 <- if (nrow(rec_row) && is.finite(rec_row$Alpha[1])) rec_row$Alpha[1] else NA_real_
      beta0 <- if (nrow(rec_row) && is.finite(rec_row$Beta[1])) rec_row$Beta[1] else NA_real_
      alpha_n <- rv$alpha_use
      if (is.null(alpha_n) || !is.finite(alpha_n)) alpha_n <- parse_sci(input$new_alpha)
      beta_n <- rv$beta_use
      if (is.null(beta_n) || !is.finite(beta_n)) beta_n <- parse_sci(input$new_beta)
      spawn_ssb <- spawn_ssb_for_fg(dat, rec_row, input$fg_yoy, rv$fspb[[input$fg_yoy]])
      recruitment_overlay(yoy, spawn_ssb, alpha0, beta0, alpha_n, beta_n, rec_row)
    })

    yoy_dates <- function(time) {
      days_as_date(time, project$data$origin)
    }

    output$plot_yoy <- renderPlot({
      yoy <- yoy_series()
      if (is.null(yoy)) {
        plot.new()
        title(paste("No YOY column for", input$fg_yoy, "(expected CODE.0 in YOY.txt)"))
        return(invisible(NULL))
      }
      ov <- rec_overlay()
      rows <- list(
        data.frame(Time = yoy_dates(yoy$time), Value = yoy$original,
                   Series = "Atlantis YOY", stringsAsFactors = FALSE)
      )
      if (!is.null(ov$larvae_old) && length(ov$larvae_old) > 0) {
        rows[[length(rows) + 1]] <- data.frame(
          Time = yoy_dates(ov$time_bh), Value = as.numeric(ov$larvae_old),
          Series = "Larvae", stringsAsFactors = FALSE
        )
      }
      if (!is.null(ov$larvae_new) && length(ov$larvae_new) > 0) {
        rows[[length(rows) + 1]] <- data.frame(
          Time = yoy_dates(ov$time_bh), Value = as.numeric(ov$larvae_new),
          Series = "New larvae", stringsAsFactors = FALSE
        )
      }
      if (!is.null(ov$yoy_new) && length(ov$yoy_new) > 0) {
        rows[[length(rows) + 1]] <- data.frame(
          Time = yoy_dates(yoy$time), Value = as.numeric(ov$yoy_new),
          Series = "New YOY", stringsAsFactors = FALSE
        )
      }
      df <- do.call(rbind, rows)
      pal <- c(`Atlantis YOY` = "#0B3C5D", Larvae = "#A33B20",
               `New larvae` = "#1D7874", `New YOY` = "#C4A35A")
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value, colour = Series))
      p <- p + ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE)
      p <- p + ggplot2::geom_point(size = 1.6, na.rm = TRUE)
      p <- p + ggplot2::scale_color_manual(values = pal)
      p <- p + ggplot2::labs(
        title = paste("YOY and larvae —", input$fg_yoy),
        subtitle = paste("YOY column", yoy$column, "·", length(yoy$original), "time steps"),
        x = "Year", y = "Young of the year / larvae"
      )
      p <- p + theme_reactiveatlantis()
      add_obs_overlay(p, project$data$observations, input$fg_yoy, "Recruitment")
    })

    output$plot_relative_yoy <- renderPlot({
      yoy <- yoy_series()
      if (is.null(yoy)) {
        plot.new()
        title(paste("No YOY column for", input$fg_yoy))
        return(invisible(NULL))
      }
      ov <- rec_overlay()
      df <- data.frame(Time = yoy_dates(yoy$time), Value = yoy$relative,
                       Series = "YOY / YOY0", stringsAsFactors = FALSE)
      if (!is.null(ov$relative_new) && length(ov$relative_new) == length(yoy$relative)) {
        df <- rbind(df, data.frame(Time = yoy_dates(yoy$time),
                                   Value = as.numeric(ov$relative_new),
                                   Series = "New YOY / YOY0", stringsAsFactors = FALSE))
      }
      pal <- c(`YOY / YOY0` = "#0B3C5D", `New YOY / YOY0` = "#1D7874")
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value, colour = Series))
      p <- p + ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE)
      p <- p + ggplot2::geom_point(size = 1.6, na.rm = TRUE)
      p <- p + ggplot2::geom_hline(yintercept = 1, linetype = 2, colour = "#8D99AE")
      p <- p + ggplot2::scale_color_manual(values = pal)
      p <- p + ggplot2::labs(title = "Relative YOY (YOY/YOY0)", x = "Year", y = "Relative YOY")
      p + theme_reactiveatlantis()
    })

    output$plot_ssb <- renderPlot({
      dat <- project$data
      if (is.null(dat$ssb)) {
        plot.new()
        title("Load SSB.txt to plot spawning stock biomass")
        return(invisible(NULL))
      }
      req(input$fg_yoy)
      if (!(input$fg_yoy %in% names(dat$ssb))) {
        plot.new()
        title(paste("No SSB column for", input$fg_yoy))
        return(invisible(NULL))
      }
      tcol <- if ("Time" %in% names(dat$ssb)) dat$ssb$Time else seq_len(nrow(dat$ssb))
      df <- data.frame(
        Time = yoy_dates(tcol),
        Value = as.numeric(dat$ssb[[input$fg_yoy]]),
        stringsAsFactors = FALSE
      )
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value))
      p <- p + ggplot2::geom_line(linewidth = 0.9, colour = "#0B3C5D", na.rm = TRUE)
      p <- p + ggplot2::labs(title = paste("SSB —", input$fg_yoy), x = "Year", y = "SSB")
      p <- p + theme_reactiveatlantis()
      add_obs_overlay(p, dat$observations, input$fg_yoy, "SSB")
    })

    output$plot_pp_growth <- renderPlot({
      req(rv$pp_list, input$fg_growth, input$box)
      fg_data <- rv$pp_list[[input$fg_growth]]
      if (is.null(fg_data)) return()
      Time <- project$data$time_dates
      rows <- list()
      if (length(dim(fg_data)) == 3) {
        box_num <- as.numeric(input$box) + 1
        if (box_num > dim(fg_data)[2]) box_num <- 1
        n_layers <- dim(fg_data)[1]
        n_time <- dim(fg_data)[3]
        if (is.null(Time) || length(Time) != n_time) Time <- seq_len(n_time)
        for (layer in seq_len(n_layers)) {
          layer_data <- fg_data[layer, box_num, ]
          if (isTRUE(input$log_scale)) layer_data <- log(layer_data + 1)
          if (isTRUE(input$layer_prop) || isTRUE(input$box_prop)) {
            layer_data <- layer_data / max(layer_data, na.rm = TRUE)
          }
          rows[[layer]] <- data.frame(
            Time = Time, Value = as.numeric(layer_data),
            Layer = paste("Layer", layer)
          )
        }
      } else {
        if (is.null(Time) || length(Time) != length(fg_data)) Time <- seq_along(fg_data)
        val <- as.numeric(fg_data)
        if (isTRUE(input$log_scale)) val <- log(val + 1)
        rows[[1]] <- data.frame(Time = Time, Value = val, Layer = "Total")
      }
      plot_df <- do.call(rbind, rows)
      ylab <- if (isTRUE(input$log_scale)) "log(Biomass)" else "Biomass"
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Time, y = Value, colour = Layer))
      p <- p + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)
      p <- p + ggplot2::facet_wrap(~ Layer, ncol = 1, scales = "free_y")
      p <- p + ggplot2::labs(
        title = paste(input$fg_growth, "— box", input$box),
        x = "Year", y = ylab
      )
      p + theme_reactiveatlantis() + ggplot2::theme(legend.position = "none")
    })
  })
}

sci_txt <- function(x) {
  if (is.null(x) || length(x) == 0 || !is.finite(x[1])) return("")
  txt <- format(x[1], scientific = TRUE, digits = 3, trim = TRUE)
  txt <- gsub("[eE]\\+0*", "e", txt)
  gsub("[eE]\\-0*", "e-", txt)
}

parse_sci <- function(x) {
  if (is.null(x) || !nzchar(trimws(as.character(x)))) return(NA_real_)
  as.numeric(gsub("[[:space:]]", "", as.character(x)[1]))
}

match_time_index <- function(yoy_time, bh_time) {
  if (is.null(bh_time) || length(bh_time) == 0) return(rep(1L, length(yoy_time)))
  vapply(yoy_time, function(t) which.min(abs(bh_time - t)), integer(1))
}

arr_at_times <- function(arr, idx) {
  d <- dim(arr)
  if (is.null(d) || length(d) == 1) return(arr[idx])
  if (length(d) == 2) return(arr[, idx, drop = FALSE])
  arr[, , idx, drop = FALSE]
}

sum_arr_space <- function(arr) {
  d <- dim(arr)
  if (is.null(d) || length(d) == 1) return(as.numeric(arr))
  apply(arr, length(d), sum, na.rm = TRUE)
}

##' Beverton-Holt recruits (same maths as recruitment.cal)
BH.rec <- function(sp, bha, bhb, bio, ver = "N") {
  if (ver == "N") {
    num <- lapply(sp, "*", bha)
    den <- lapply(bio, "+", bhb)
  } else {
    num <- lapply(bio, "*", bha)
    den <- lapply(bio, "+", bhb)
  }
  mapply("/", num, den, SIMPLIFY = FALSE)
}

MG2T <- 0.00000002

larvae_to_yoy_units <- function(recruits, rec_row) {
  rec <- as.numeric(unlist(recruits))
  if (nrow(rec_row) == 0) return(rec)
  snw <- rec_row$Rec.SNW[1]
  rnw <- rec_row$Rec.RNW[1]
  xcn <- rec_row$XCN[1]
  if (is.finite(snw) && is.finite(rnw) && (abs(snw) + abs(rnw)) > 0 && is.finite(xcn)) {
    rec <- rec * (snw + rnw) * xcn * MG2T
  }
  rec
}

##' Overlay larvae / new YOY from current and typed Beverton–Holt parameters
recruitment_overlay <- function(yoy, spawn_ssb, alpha0, beta0, alpha_n, beta_n,
                                rec_row) {
  bh_old <- bh_new <- NULL
  t_bh <- NULL
  if (!is.null(spawn_ssb) && is.finite(alpha0) && is.finite(beta0)) {
    t_bh <- spawn_ssb$time
    bh_old <- larvae_to_yoy_units(
      unlist(BH.rec(spawn_ssb$spawn, alpha0, beta0, spawn_ssb$ssb, spawn_ssb$ver)),
      rec_row
    )
    if (is.finite(alpha_n) && is.finite(beta_n)) {
      bh_new <- larvae_to_yoy_units(
        unlist(BH.rec(spawn_ssb$spawn, alpha_n, beta_n, spawn_ssb$ssb, spawn_ssb$ver)),
        rec_row
      )
    }
  }
  yoy_new <- NULL
  rel_new <- NULL
  if (!is.null(yoy) && !is.null(bh_old) && !is.null(bh_new) && length(bh_old) > 0) {
    idx <- match_time_index(yoy$time, t_bh)
    rec_at <- bh_old[pmin(idx, length(bh_old))]
    new_at <- bh_new[pmin(idx, length(bh_new))]
    ratio <- ifelse(is.finite(rec_at) & rec_at > 0, yoy$original / rec_at, NA_real_)
    yoy_new <- new_at * ratio
    y0 <- yoy$original[which(is.finite(yoy$original))[1]]
    rel_new <- if (is.finite(y0) && abs(y0) > 1e-12) yoy_new / y0 else NA_real_
  }
  list(time_bh = t_bh, larvae_old = bh_old, larvae_new = bh_new,
       yoy_new = yoy_new, relative_new = rel_new, spawn = spawn_ssb)
}

nc_time_days <- function(nc) {
  t <- tryCatch(ncdf4::ncvar_get(nc, "t"), error = function(e) NULL)
  if (is.null(t)) return(NULL)
  units <- tryCatch(ncdf4::ncatt_get(nc, "t", "units")$value, error = function(e) "")
  if (is.character(units) && grepl("second", units, ignore.case = TRUE)) {
    t <- as.numeric(t) / 86400
  }
  as.numeric(t)
}

spawn_times_idx <- function(time_days, time_spawn) {
  if (is.null(time_days) || length(time_days) == 0) return(integer())
  t_sp <- if (is.finite(time_spawn[1])) time_spawn[1] else 0
  t_end <- tail(time_days, 1)
  spawn_days <- seq(from = 0, by = 365, to = t_end) + t_sp
  spawn_days <- spawn_days[spawn_days < t_end]
  if (length(spawn_days) == 0) spawn_days <- time_days[1]
  vapply(spawn_days, function(x) which.min(abs(time_days - x)), integer(1))
}

##' Spawn and SSB at reproduction events from output.nc (recruitment.cal maths)
spawn_from_nc <- function(nc, grp, rec_row, fg_code, fspb_vec = NULL) {
  if (is.null(nc) || is.null(grp) || nrow(rec_row) == 0) return(NULL)
  g <- grp[as.character(grp$code) == as.character(fg_code), , drop = FALSE]
  if (nrow(g) == 0) return(NULL)
  name <- as.character(g$name[1])
  n_coh <- as.integer(g$numcohorts[1])
  if (!is.finite(n_coh) || n_coh < 1) return(NULL)
  gtype <- if ("grouptype" %in% names(g)) as.character(g$grouptype[1]) else ""
  time <- nc_time_days(nc)
  if (is.null(time) || length(time) < 1) return(NULL)
  idx <- spawn_times_idx(time, rec_row$Time.sp[1])
  if (length(idx) == 0) return(NULL)
  xrs <- rec_row$XRS[1]
  if (!is.finite(xrs)) xrs <- 2.65
  fsp <- rec_row$FSP[1]
  if (!is.finite(fsp)) fsp <- 1
  kspa <- rec_row$KSPA[1]
  if (!is.finite(kspa)) kspa <- 0
  if (is.null(fspb_vec) || length(fspb_vec) == 0) fspb_vec <- rep(1, n_coh)
  if (length(fspb_vec) < n_coh) {
    fspb_vec <- c(as.numeric(fspb_vec), rep(0, n_coh - length(fspb_vec)))
  }
  special <- gtype %in% c("PWN", "CEP", "PRAWNS", "PRAWN")
  vars <- names(nc$var)
  spawn_tot <- rep(0, length(idx))
  ssb_tot <- rep(0, length(idx))
  for (coh in seq_len(n_coh)) {
    fspb <- as.numeric(fspb_vec[coh])
    if (!is.finite(fspb)) fspb <- 0
    if (special) {
      vn <- paste0(name, "_N", coh)
      if (!vn %in% vars) vn <- paste0(name, "_N")
      if (!vn %in% vars) next
      nums <- tryCatch(ncdf4::ncvar_get(nc, vn), error = function(e) NULL)
      if (is.null(nums)) next
      for (j in seq_along(idx)) {
        nsl <- as.numeric(arr_at_times(nums, idx[j]))
        nsl <- nsl * as.numeric(nsl > 1e-16)
        spawn_tot[j] <- spawn_tot[j] + sum(nsl * fspb, na.rm = TRUE)
        ssb_tot[j] <- ssb_tot[j] + sum(nsl, na.rm = TRUE)
      }
    } else {
      n_var <- paste0(name, coh, "_Nums")
      r_var <- paste0(name, coh, "_ResN")
      s_var <- paste0(name, coh, "_StructN")
      if (!all(c(n_var, r_var, s_var) %in% vars)) next
      nums <- tryCatch(ncdf4::ncvar_get(nc, n_var), error = function(e) NULL)
      rn <- tryCatch(ncdf4::ncvar_get(nc, r_var), error = function(e) NULL)
      sn <- tryCatch(ncdf4::ncvar_get(nc, s_var), error = function(e) NULL)
      if (is.null(nums) || is.null(rn) || is.null(sn)) next
      for (j in seq_along(idx)) {
        nsl <- as.numeric(arr_at_times(nums, idx[j]))
        rsl <- as.numeric(arr_at_times(rn, idx[j]))
        ssl <- as.numeric(arr_at_times(sn, idx[j]))
        mask <- as.numeric(nsl > 1e-16)
        nsl <- nsl * mask
        rsl <- rsl * mask
        ssl <- ssl * mask
        wspi <- ssl * (1 + xrs)
        rat <- (rsl + ssl) - wspi
        rat[rat < 0] <- 0
        spawn <- ((wspi * fsp - kspa) - rat)
        spawn[spawn < 0] <- 0
        spawn_tot[j] <- spawn_tot[j] + sum(spawn * fspb * nsl, na.rm = TRUE)
        ssb_tot[j] <- ssb_tot[j] + sum((rsl + ssl) * nsl, na.rm = TRUE)
      }
    }
  }
  list(time = time[idx], spawn = spawn_tot, ssb = ssb_tot)
}

bh_version <- function(rec_row) {
  if (nrow(rec_row) > 0 && is.finite(rec_row$Value[1]) && rec_row$Value[1] == 3) {
    "N"
  } else {
    "B"
  }
}

table_ssb <- function(tab, fg_code) {
  if (is.null(tab) || !(fg_code %in% names(tab))) return(NULL)
  tcol <- if ("Time" %in% names(tab)) as.numeric(tab$Time) else seq_len(nrow(tab)) - 1
  list(time = tcol, spawn = as.numeric(tab[[fg_code]]),
       ssb = as.numeric(tab[[fg_code]]))
}

##' Spawn and SSB time series for one functional group
spawn_ssb_for_fg <- function(dat, rec_row, fg_code, fspb_vec = NULL) {
  ver <- bh_version(rec_row)
  nc_spawn <- tryCatch(
    spawn_from_nc(dat$nc_out, dat$grp, rec_row, fg_code, fspb_vec),
    error = function(e) NULL
  )
  if (!is.null(nc_spawn) && length(nc_spawn$ssb) > 0 &&
      any(is.finite(nc_spawn$ssb) & nc_spawn$ssb > 0)) {
    nc_spawn$ver <- ver
    return(nc_spawn)
  }
  from_ssb <- table_ssb(dat$ssb, fg_code)
  if (!is.null(from_ssb)) {
    from_ssb$ver <- ver
    return(from_ssb)
  }
  from_bio <- table_ssb(dat$biomindx, fg_code)
  if (!is.null(from_bio)) {
    from_bio$ver <- ver
    return(from_bio)
  }
  NULL
}
