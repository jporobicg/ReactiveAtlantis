## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~         Recruitment Analysis Module        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

recruitment_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("analysis_type"), "View",
                    choices = c("Recruits and YOY" = "yoy",
                                "Spawning stock biomass" = "ssb",
                                "Recruits vs SSB" = "sr",
                                "Zooplankton and primary producers" = "growth")),
        conditionalPanel(
          "input.analysis_type == 'yoy' || input.analysis_type == 'ssb' || input.analysis_type == 'sr'", ns = ns,
          selectInput(ns("fg_yoy"), "Functional group", choices = character())
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
          "input.analysis_type == 'sr'", ns = ns,
          p(class = "ra-muted",
            "YOY.txt (CODE.0) against SSB.txt for the same age-structured group. Time series plus recruits versus spawning stock.")
        ),
        conditionalPanel(
          "input.analysis_type == 'growth'", ns = ns,
          selectInput(ns("fg_growth"), "Functional group", choices = character()),
          selectInput(ns("box"), "Box", choices = 0),
          checkboxInput(ns("layer_prop"), "Layer proportion", TRUE),
          checkboxInput(ns("box_prop"), "Box proportion", FALSE),
          checkboxInput(ns("log_scale"), "Logarithmic", FALSE)
        )
      ),
      conditionalPanel("input.analysis_type == 'yoy'", ns = ns,
                       plotOutput(ns("plot_yoy"), height = "340px"),
                       plotOutput(ns("plot_relative_yoy"), height = "340px")),
      conditionalPanel("input.analysis_type == 'ssb'", ns = ns,
                       plotOutput(ns("plot_ssb"), height = "480px")),
      conditionalPanel("input.analysis_type == 'sr'", ns = ns,
                       plotOutput(ns("plot_sr_series"), height = "360px"),
                       plotOutput(ns("plot_sr_scatter"), height = "360px")),
      conditionalPanel("input.analysis_type == 'growth'", ns = ns,
                       plotOutput(ns("plot_pp_growth"), height = "640px"))
    )
  )
}

recruitment_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))
    rv <- reactiveValues(rec_params = NULL, pp_list = NULL, n_box = 1, built = FALSE,
                         token = NULL, alpha_use = NA_real_, beta_use = NA_real_,
                         fspb = NULL)

    output$banner <- renderUI(tool_banner(ps(), "recruitment"))

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      grp <- ps()$data$grp
      age_groups <- grp[grp$numcohorts > 1, ]
      pp.pos <- with(grp, which(grouptype %in% c("MED_ZOO", "LG_ZOO", "LG_PHY", "SM_PHY",
                                                 "PHYTOBEN", "DINOFLAG", "TURF") &
                                isturnedon == 1))
      update_select_keep(session, "fg_yoy", as.character(age_groups$code), input$fg_yoy)
      update_select_keep(session, "fg_growth", as.character(grp$code[pp.pos]), input$fg_growth)
      update_select_keep(session, "box", as.character(seq(0, max(0, rv$n_box - 1))), input$box)
    })

    rec_built <- reactive({
      dat <- ps()$data
      req(isTRUE(ps()$ready), dat$grp, dat$prm_bio)
      built <- build_rec_params(dat$grp, dat$prm_bio)
      rv$rec_params <- built$rec
      rv$fspb <- built$fspb
      rv$built <- TRUE
      built
    })

    fg_yoy_d <- ra_debounce(shiny::reactive(input$fg_yoy), 250)
    fg_growth_d <- ra_debounce(shiny::reactive(input$fg_growth), 250)

    fill_bh_inputs <- function(fg) {
      rec <- rec_built()$rec
      rec_row <- which(rec$FG == fg)
      if (length(rec_row) == 0) return()
      updateTextInput(session, "new_alpha", value = sci_txt(rec$Alpha[rec_row[1]]))
      updateTextInput(session, "new_beta", value = sci_txt(rec$Beta[rec_row[1]]))
      rv$alpha_use <- rec$Alpha[rec_row[1]]
      rv$beta_use <- rec$Beta[rec_row[1]]
    }

    observeEvent(fg_yoy_d(), {
      req(rec_built()$rec, fg_yoy_d())
      fill_bh_inputs(fg_yoy_d())
    }, ignoreInit = FALSE)

    observeEvent(input$recalc, {
      rv$alpha_use <- parse_sci(input$new_alpha)
      rv$beta_use <- parse_sci(input$new_beta)
    }, ignoreInit = TRUE)

    yoy_series <- reactive({
      dat <- ps()$data
      req(dat$yoy, fg_yoy_d())
      col <- find_yoy_column(dat$yoy, fg_yoy_d())
      if (is.na(col)) return(NULL)
      val <- as.numeric(dat$yoy[[col]])
      tdays <- as.numeric(dat$yoy$Time)
      if (length(tdays) != length(val)) tdays <- seq_along(val) - 1
      first <- val[which(is.finite(val))[1]]
      rel <- if (is.finite(first) && abs(first) > 1e-12) val / first else rep(NA_real_, length(val))
      list(time = tdays, original = val, relative = rel, column = col)
    })

    rec_overlay <- reactive({
      dat <- ensure_project_data(ps(), "nc_out")
      rec <- rec_built()$rec
      req(fg_yoy_d(), rec)
      yoy <- yoy_series()
      rec_row <- rec[rec$FG == fg_yoy_d(), , drop = FALSE]
      alpha0 <- if (nrow(rec_row) && is.finite(rec_row$Alpha[1])) rec_row$Alpha[1] else NA_real_
      beta0 <- if (nrow(rec_row) && is.finite(rec_row$Beta[1])) rec_row$Beta[1] else NA_real_
      alpha_n <- rv$alpha_use
      if (is.null(alpha_n) || !is.finite(alpha_n)) alpha_n <- parse_sci(input$new_alpha)
      beta_n <- rv$beta_use
      if (is.null(beta_n) || !is.finite(beta_n)) beta_n <- parse_sci(input$new_beta)
      spawn_ssb <- spawn_ssb_for_fg(dat, rec_row, fg_yoy_d(), rec_built()$fspb[[fg_yoy_d()]])
      recruitment_overlay(yoy, spawn_ssb, alpha0, beta0, alpha_n, beta_n, rec_row)
    })

    yoy_dates <- function(time) {
      days_as_date(time, ensure_project_data(ps(), "nc_out")$origin)
    }

    output$plot_yoy <- renderPlot({
      plot_or_message("Load YOY.txt to plot recruits", {
      yoy <- yoy_series()
      if (is.null(yoy)) {
        plot.new()
        title(paste("No YOY column for", fg_yoy_d(), "(expected CODE.0 in YOY.txt)"))
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
        title = paste("YOY and larvae —", fg_yoy_d()),
        subtitle = paste("YOY column", yoy$column, "·", length(yoy$original), "time steps"),
        x = "Year", y = "Young of the year / larvae"
      )
      p <- p + theme_reactiveatlantis()
      p <- ra_scale_x_year(p, df$Time)
      add_obs_overlay(p, ps()$data$observations, fg_yoy_d(), "Recruitment")
      })
    })

    output$plot_relative_yoy <- renderPlot({
      plot_or_message("Load YOY.txt to plot relative recruits", {
      yoy <- yoy_series()
      if (is.null(yoy)) {
        plot.new()
        title(paste("No YOY column for", fg_yoy_d()))
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
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, df$Time)
      })
    })

    output$plot_ssb <- renderPlot({
      plot_or_message("Load SSB.txt to plot spawning stock biomass", {
      dat <- ps()$data
      if (is.null(dat$ssb)) {
        plot.new()
        title("Load SSB.txt to plot spawning stock biomass")
        return(invisible(NULL))
      }
      req(fg_yoy_d())
      if (!(fg_yoy_d() %in% names(dat$ssb))) {
        plot.new()
        title(paste("No SSB column for", fg_yoy_d()))
        return(invisible(NULL))
      }
      tcol <- if ("Time" %in% names(dat$ssb)) dat$ssb$Time else seq_len(nrow(dat$ssb))
      df <- data.frame(
        Time = yoy_dates(tcol),
        Value = as.numeric(dat$ssb[[fg_yoy_d()]]),
        stringsAsFactors = FALSE
      )
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value))
      p <- p + ggplot2::geom_line(linewidth = 0.9, colour = "#0B3C5D", na.rm = TRUE)
      p <- p + ggplot2::labs(title = paste("SSB —", fg_yoy_d()), x = "Year", y = "SSB")
      p <- p + theme_reactiveatlantis()
      p <- ra_scale_x_year(p, df$Time)
      add_obs_overlay(p, dat$observations, fg_yoy_d(), "SSB")
      })
    })

    sr_table <- reactive({
      dat <- ps()$data
      req(fg_yoy_d())
      join_recruits_ssb(dat$yoy, dat$ssb, fg_yoy_d())
    })

    output$plot_sr_series <- renderPlot({
      plot_or_message("Load YOY.txt and SSB.txt to plot recruits versus spawning stock", {
        df <- sr_table()
        if (is.null(df) || nrow(df) == 0) {
          plot_message(attr(df, "reason") %||%
                         "Load YOY.txt and SSB.txt to plot recruits versus spawning stock")
          return(invisible(NULL))
        }
        long <- rbind(
          data.frame(Time = yoy_dates(df$Time), Value = df$Recruits,
                     Series = "Recruits (YOY)", stringsAsFactors = FALSE),
          data.frame(Time = yoy_dates(df$Time), Value = df$SSB,
                     Series = "SSB", stringsAsFactors = FALSE)
        )
        pal <- c(`Recruits (YOY)` = "#A33B20", SSB = "#0B3C5D")
        p <- ggplot2::ggplot(long, ggplot2::aes(x = Time, y = Value, colour = Series))
        p <- p + ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE)
        p <- p + ggplot2::geom_point(size = 1.6, na.rm = TRUE)
        p <- p + ggplot2::facet_wrap(~ Series, ncol = 1, scales = "free_y")
        p <- p + ggplot2::scale_color_manual(values = pal)
        p <- p + ggplot2::labs(
          title = paste("Recruits and SSB —", fg_yoy_d()),
          subtitle = paste("YOY", unique(df$yoy_column), "· SSB", unique(df$ssb_column)),
          x = "Year", y = NULL
        )
        p <- p + theme_reactiveatlantis()
        ra_scale_x_year(p, long$Time)
      })
    })

    output$plot_sr_scatter <- renderPlot({
      plot_or_message("Load YOY.txt and SSB.txt to plot the stock-recruit scatter", {
        df <- sr_table()
        if (is.null(df) || nrow(df) == 0) {
          plot_message(attr(df, "reason") %||%
                         "Load YOY.txt and SSB.txt to plot the stock-recruit scatter")
          return(invisible(NULL))
        }
        origin <- tryCatch(ensure_project_data(ps(), "nc_out")$origin,
                           error = function(e) ps()$data$origin)
        df$Date <- days_as_date(df$Time, origin)
        p <- ggplot2::ggplot(df, ggplot2::aes(x = SSB, y = Recruits, colour = as.numeric(Date)))
        p <- p + ggplot2::geom_point(size = 2.4, na.rm = TRUE)
        p <- p + ggplot2::geom_path(linewidth = 0.4, alpha = 0.45, na.rm = TRUE)
        p <- p + ggplot2::scale_colour_gradient(low = "#A33B20", high = "#1D7874",
                                               name = "Year",
                                               labels = function(x) {
                                                 format(as.Date(x, origin = "1970-01-01"), "%Y")
                                               })
        p <- p + ggplot2::labs(
          title = paste("Recruits vs spawning stock —", fg_yoy_d()),
          x = "SSB", y = "Recruits (YOY.txt)"
        )
        p + theme_reactiveatlantis()
      })
    })

    output$plot_pp_growth <- renderPlot({
      plot_or_message("Load output.nc to plot zooplankton and primary producers", {
      dat <- ensure_project_data(ps(), "nc_out")
      req(dat$nc_out, dat$grp, fg_growth_d(), input$box)
      grp <- dat$grp
      g <- grp[as.character(grp$code) == as.character(fg_growth_d()), , drop = FALSE]
      req(nrow(g) > 0)
      vn <- paste0(g$name[1], "_N")
      fg_data <- tryCatch(nc_get(dat$nc_out, vn), error = function(e) NULL)
      if (is.null(fg_data)) return()
      n_box <- if (!is.null(dat$nc_out$dim$b)) dat$nc_out$dim$b$len else 1
      rv$n_box <- n_box
      update_select_keep(session, "box", as.character(seq(0, max(0, n_box - 1))), input$box)
      Time <- dat$time_dates
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
        title = paste(fg_growth_d(), "— box", input$box),
        x = "Year", y = ylab
      )
      p <- p + theme_reactiveatlantis() + ggplot2::theme(legend.position = "none")
      ra_scale_x_year(p, plot_df$Time)
      })
    })

    keep_outputs_alive(output, "banner")
  })
}

build_rec_params <- function(grp, prm) {
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
  list(rec = rec, fspb = fspb_list)
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
  t <- tryCatch(nc_get(nc, "t"), error = function(e) NULL)
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
      for (j in seq_along(idx)) {
        nums <- tryCatch(nc_get_at_t(nc, vn, idx[j]), error = function(e) NULL)
        if (is.null(nums)) next
        nsl <- as.numeric(nums)
        nsl <- nsl * as.numeric(nsl > 1e-16)
        spawn_tot[j] <- spawn_tot[j] + sum(nsl * fspb, na.rm = TRUE)
        ssb_tot[j] <- ssb_tot[j] + sum(nsl, na.rm = TRUE)
      }
    } else {
      n_var <- paste0(name, coh, "_Nums")
      r_var <- paste0(name, coh, "_ResN")
      s_var <- paste0(name, coh, "_StructN")
      if (!all(c(n_var, r_var, s_var) %in% vars)) next
      for (j in seq_along(idx)) {
        nums <- tryCatch(nc_get_at_t(nc, n_var, idx[j]), error = function(e) NULL)
        rn <- tryCatch(nc_get_at_t(nc, r_var, idx[j]), error = function(e) NULL)
        sn <- tryCatch(nc_get_at_t(nc, s_var, idx[j]), error = function(e) NULL)
        if (is.null(nums) || is.null(rn) || is.null(sn)) next
        nsl <- as.numeric(nums)
        rsl <- as.numeric(rn)
        ssl <- as.numeric(sn)
        mask <- as.numeric(nsl > 1e-16)
        nsl <- nsl * mask
        rsl <- rsl * mask
        ssl <- ssl * mask
        spawn <- atlantis_ind_spawn(ssl, rsl, fsp, kspa, xrs)
        spawn_tot[j] <- spawn_tot[j] + sum(spawn * fspb * nsl, na.rm = TRUE)
        ssb_tot[j] <- ssb_tot[j] + sum((rsl + ssl) * nsl, na.rm = TRUE)
      }
    }
  }
  list(time = time[idx], spawn = spawn_tot, ssb = ssb_tot)
}

##' Spawn per individual (mg N), as Atlantis Ecology_Age_Structured_Spawn
##' (atdemography.c:571-592, flag_repcostSpawn = 0):
##' WforS = (1 + X_RS) SN; Spwn = FSP WforS - KSPA, minus the weight deficit
##' (WforS - (SN + RN)) when the fish is below WforS; floored at 0 and capped
##' at RN. Validation D03: the previous code subtracted the weight surplus.
atlantis_ind_spawn <- function(SN, RN, FSP, KSPA, X_RS) {
  wfors <- (1 + X_RS) * SN
  wgt <- SN + RN
  spwn <- FSP * wfors - KSPA
  deficit <- wfors - wgt
  spwn <- spwn - ifelse(is.finite(deficit) & deficit > 0, deficit, 0)
  spwn[!is.finite(spwn) | spwn < 0] <- 0
  over <- is.finite(RN) & spwn > RN
  spwn[over] <- RN[over]
  spwn
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
  ## No fallback to SSB.txt or BiomIndx: those are tonnes, while spawn and
  ## BHbeta are mg N, so a Beverton-Holt curve built from them is wrong
  ## (validation D04). The caller shows Atlantis YOY without the BH line.
  NULL
}
