## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   Primary Producer Growth Analysis Module  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

growth_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("body")))
}

growth_body_ui <- function(ns, cod_fg, n_box) {
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "open",
      selectInput(ns("fg"), "Primary producer", choices = cod_fg),
      selectInput(ns("box"), "Box", choices = seq(0, max(0, n_box - 1))),
      checkboxInput(ns("log_scale"), "Logarithmic", FALSE)
    ),
    uiOutput(ns("plot_host"))
  )
}

growth_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      l_nut = NULL, l_light = NULL, l_eddy = NULL,
      cod_fg = NULL, n_box = NULL, built = FALSE, token = NULL
    )

    output$body <- renderUI({
      if (is.null(project) || !isTRUE(project$ready)) {
        return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS$growth], "Growth"))
      }
      miss <- tool_missing(project$paths, "growth")
      if (length(miss)) return(tool_missing_card(miss, "Growth"))
      if (is.null(rv$cod_fg)) {
        return(bslib::card(bslib::card_header("Growth"),
                           bslib::card_body("Reading limitation factors from the run…")))
      }
      growth_body_ui(session$ns, rv$cod_fg, rv$n_box)
    })

    observe({
      req(!is.null(project), isTRUE(project$ready), project$data)
      miss <- tool_missing(project$paths, "growth")
      token <- paste(unlist(project$paths), collapse = "|")
      if (length(miss) || (isTRUE(rv$built) && identical(rv$token, token))) return()
      rv$token <- token
      dat <- project$data
      req(dat$nc_out, dat$grp, dat$prm_bio)
      grp <- dat$grp
      prm <- dat$prm_bio
      nc.out <- dat$nc_out

      pp.grp <- with(grp, which(grouptype %in% c("PHYTOBEN", "SM_PHY", "LG_PHY", "SEAGRASS",
                                                 "DINOFLAG", "TURF", "MICROPHTYBENTHOS") &
                                isturnedon == 1))
      if (length(pp.grp) == 0) {
        shiny::showNotification("No primary producers found in the groups CSV.", type = "error")
        return()
      }
      cod.fg <- as.character(grp$code[pp.grp])
      nam.fg <- as.character(grp$name[pp.grp])

      options(warn = -1)
      flagnut <- text2num(prm, "flagnut ", FG = "look")
      if (nrow(flagnut) == 0) flagnut <- data.frame(FG = "flagnut", Value = 0)
      KN <- KS <- KI <- NULL
      for (i in seq_along(pp.grp)) {
        KN <- rbind(KN, text2num(prm, paste0("KN_", cod.fg[i]), FG = cod.fg[i]))
        KS <- rbind(KS, text2num(prm, paste0("KS_", cod.fg[i]), FG = cod.fg[i]))
        KI <- rbind(KI, text2num(prm, paste0("KI_", cod.fg[i]), FG = cod.fg[i]))
      }
      ed.scl <- text2num(prm, "eddy_scale", FG = "look")
      if (nrow(ed.scl) > 0) ed.scl <- ed.scl[1, 2] else ed.scl <- 1
      options(warn = 0)

      DIN <- ncdf4::ncvar_get(nc.out, "NO3") + ncdf4::ncvar_get(nc.out, "NH3")
      Si <- ncdf4::ncvar_get(nc.out, "Si")
      l.eddy <- ncdf4::ncvar_get(nc.out, "eddy") * ed.scl
      IRR <- ncdf4::ncvar_get(nc.out, "Light")

      l.light <- l.nut <- list()
      for (fg in seq_along(nam.fg)) {
        if (flagnut$Value[1] == 0) {
          l.nut[[fg]] <- pmin((DIN / (KN$Value[fg] + DIN)), (Si / (KS$Value[fg] + Si)), na.rm = TRUE)
        } else if (flagnut$Value[1] == 1) {
          l.nut[[fg]] <- sqrt((DIN / (KN$Value[fg] + DIN)) * (Si / (KS$Value[fg] + Si)))
        } else if (flagnut$Value[1] == 2) {
          l.nut[[fg]] <- 2 / ((DIN / (KN$Value[fg] + DIN)) + (Si / (KS$Value[fg] + Si)))
        } else {
          l.nut[[fg]] <- (DIN / (KN$Value[fg] + DIN))
        }
        l.light[[fg]] <- pmin((IRR / KI$Value[fg]), 1, na.rm = TRUE)
      }

      n.box <- dim(IRR)[2]
      rv$l_nut <- l.nut
      rv$l_light <- l.light
      rv$l_eddy <- l.eddy
      rv$cod_fg <- cod.fg
      rv$n_box <- n.box
      rv$built <- TRUE
    })

    output$plot_host <- renderUI({
      ns <- session$ns
      plotOutput(ns("growth_plot"), height = "720px")
    })

    output$growth_plot <- renderPlot({
      req(rv$built, input$fg, input$box)
      fg_idx <- which(rv$cod_fg == input$fg)
      box_idx <- as.numeric(input$box) + 1
      if (length(fg_idx) == 0 || box_idx > rv$n_box) return()

      nut_data <- rv$l_nut[[fg_idx]]
      light_data <- rv$l_light[[fg_idx]]
      eddy_data <- rv$l_eddy
      n_layers <- dim(nut_data)[1]
      n_time <- dim(nut_data)[3]
      Time <- project$data$time_dates
      if (is.null(Time) || length(Time) != n_time) Time <- seq_len(n_time)

      rows <- list()
      for (layer in seq_len(n_layers)) {
        nut_layer <- nut_data[layer, box_idx, ]
        light_layer <- light_data[layer, box_idx, ]
        eddy_layer <- eddy_data[box_idx, ]
        growth_layer <- nut_layer * light_layer
        if (isTRUE(input$log_scale)) {
          nut_layer <- log(nut_layer + 1e-10)
          light_layer <- log(light_layer + 1e-10)
          eddy_layer <- log(eddy_layer + 1e-10)
          growth_layer <- log(growth_layer + 1e-10)
        }
        rows[[length(rows) + 1]] <- data.frame(
          Time = Time,
          Value = as.numeric(growth_layer),
          Series = "Growth",
          Layer = paste("Layer", layer - 1)
        )
        rows[[length(rows) + 1]] <- data.frame(
          Time = Time, Value = as.numeric(nut_layer), Series = "Nutrients",
          Layer = paste("Layer", layer - 1)
        )
        rows[[length(rows) + 1]] <- data.frame(
          Time = Time, Value = as.numeric(light_layer), Series = "Light",
          Layer = paste("Layer", layer - 1)
        )
        if (layer == 1) {
          rows[[length(rows) + 1]] <- data.frame(
            Time = Time,
            Value = rep(mean(eddy_layer, na.rm = TRUE), n_time),
            Series = "Eddy",
            Layer = paste("Layer", layer - 1)
          )
        }
      }
      plot_df <- do.call(rbind, rows)
      pal <- c(Growth = "#0B3C5D", Nutrients = "#1D7874", Light = "#C4A35A", Eddy = "#A33B20")
      ylab <- if (isTRUE(input$log_scale)) "log(Limitation)" else "Limitation"
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Time, y = Value, colour = Series))
      p <- p + ggplot2::geom_line(linewidth = 0.85, na.rm = TRUE)
      p <- p + ggplot2::facet_wrap(~ Layer, ncol = 1, scales = "free_y")
      p <- p + ggplot2::scale_color_manual(values = pal)
      p <- p + ggplot2::labs(
        title = paste(input$fg, "— box", input$box),
        x = "Year", y = ylab
      )
      p + theme_reactiveatlantis()
    })
  })
}
