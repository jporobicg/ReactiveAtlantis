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
        numericInput(ns("new_alpha"), "New alpha", value = 0, step = 0.1),
        numericInput(ns("new_beta"), "New beta", value = 0, step = 0.1),
        p(class = "ra-muted", "The Beverton-Holt overlay uses alpha * YOY / (1 + beta * YOY) and updates as you edit alpha and beta. Observed Recruitment points come from the observations CSV in the Run panel.")
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
    rv <- reactiveValues(rec_params = NULL, pp_list = NULL, n_box = 1, built = FALSE, token = NULL)

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
      rec <- cbind(rec, Alpha = NA, Beta = NA)
      sps <- gsub(pattern = "^flagrecruit", "", rec$FG)
      rec$FG <- sps
      for (fg.r in seq_along(sps)) {
        if (!sps[fg.r] %in% grp$code[sp.dat]) next()
        if (rec$Value[fg.r] == 3) {
          rec$Alpha[fg.r] <- text2num(prm, paste0("BHalpha_", sps[fg.r]), FG = "look")[1, 2]
          rec$Beta[fg.r] <- text2num(prm, paste0("BHbeta_", sps[fg.r]), FG = "look")[1, 2]
        }
      }

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
      pp.list[["Light"]] <- tryCatch(ncdf4::ncvar_get(nc.out, "Light"), error = function(e) NULL)
      pp.list[["Eddy"]] <- tryCatch(ncdf4::ncvar_get(nc.out, "eddy"), error = function(e) NULL)

      n_box <- 1
      if (!is.null(nc.out$dim$b)) n_box <- nc.out$dim$b$len
      rv$rec_params <- rec
      rv$pp_list <- pp.list
      rv$n_box <- n_box
      rv$built <- TRUE
    })

    observeEvent(input$fg_yoy, {
      req(rv$rec_params, input$fg_yoy)
      rec_row <- which(rv$rec_params$FG == input$fg_yoy)
      if (length(rec_row) > 0) {
        updateNumericInput(session, "new_alpha", value = rv$rec_params$Alpha[rec_row])
        updateNumericInput(session, "new_beta", value = rv$rec_params$Beta[rec_row])
      }
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

    yoy_calculated <- reactive({
      dat <- project$data
      req(dat$yoy, input$fg_yoy)
      yoy_col <- paste0(input$fg_yoy, ".0")
      if (!(yoy_col %in% names(dat$yoy))) return(NULL)
      list(
        time = dat$yoy$Time,
        original = dat$yoy[[yoy_col]],
        relative = dat$yoy[[yoy_col]] / dat$yoy[[yoy_col]][1]
      )
    })

    yoy_new <- reactive({
      req(yoy_calculated(), !is.null(input$new_alpha), !is.null(input$new_beta))
      alpha <- input$new_alpha
      beta <- input$new_beta
      original <- yoy_calculated()$original
      if (is.null(original) || !is.finite(alpha) || !is.finite(beta)) return(NULL)
      new_yoy <- alpha * original / (1 + beta * original)
      list(
        time = yoy_calculated()$time,
        new = new_yoy,
        relative_new = new_yoy / new_yoy[1]
      )
    })

    yoy_dates <- function(time) {
      days_as_date(time, project$data$origin)
    }

    output$plot_yoy <- renderPlot({
      yoy <- yoy_calculated()
      if (is.null(yoy)) {
        plot.new()
        title("No YOY column for this group")
        return(invisible(NULL))
      }
      df <- data.frame(Time = yoy_dates(yoy$time), Value = yoy$original, Series = "Original")
      new_vals <- yoy_new()
      if (!is.null(new_vals)) {
        df <- rbind(df, data.frame(Time = yoy_dates(new_vals$time),
                                   Value = new_vals$new, Series = "New parameters"))
      }
      pal <- c(Original = "#0B3C5D", `New parameters` = "#1D7874")
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value, colour = Series))
      p <- p + ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE)
      p <- p + ggplot2::scale_color_manual(values = pal)
      p <- p + ggplot2::labs(title = paste("YOY —", input$fg_yoy), x = "Year", y = "Young of the year")
      p <- p + theme_reactiveatlantis()
      add_obs_overlay(p, project$data$observations, input$fg_yoy, "Recruitment")
    })

    output$plot_relative_yoy <- renderPlot({
      yoy <- yoy_calculated()
      if (is.null(yoy)) {
        plot.new()
        title("No YOY column for this group")
        return(invisible(NULL))
      }
      df <- data.frame(Time = yoy_dates(yoy$time), Value = yoy$relative, Series = "Original")
      new_vals <- yoy_new()
      if (!is.null(new_vals)) {
        df <- rbind(df, data.frame(Time = yoy_dates(new_vals$time),
                                   Value = new_vals$relative_new, Series = "New parameters"))
      }
      pal <- c(Original = "#0B3C5D", `New parameters` = "#1D7874")
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = Value, colour = Series))
      p <- p + ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE)
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
