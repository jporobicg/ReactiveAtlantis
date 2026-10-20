## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   Primary Producer Growth Analysis Module  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

growth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("fg"), "Primary producer", choices = character()),
        selectInput(ns("box"), "Box", choices = 0),
        selectInput(ns("layer"), "Layer", choices = 0),
        checkboxInput(ns("all_layers"), "All layers (facets)", FALSE),
        checkboxInput(ns("log_scale"), "Logarithmic", FALSE)
      ),
      plotOutput(ns("growth_plot"), height = "720px")
    )
  )
}

growth_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      l_nut = NULL, l_light = NULL, l_eddy = NULL,
      cod_fg = NULL, n_box = NULL, n_layer = NULL, built = FALSE, token = NULL
    )

    output$banner <- renderUI(tool_banner(project, "growth"))

    observe({
      req(rv$built, rv$cod_fg)
      update_select_keep(session, "fg", rv$cod_fg, input$fg)
      update_select_keep(session, "box", as.character(seq(0, max(0, rv$n_box - 1))), input$box)
      update_select_keep(session, "layer", as.character(seq(0, max(0, rv$n_layer - 1))), input$layer)
    })

    observe({
      req(!is.null(project), isTRUE(project$ready), project$data)
      miss <- tool_missing(project$paths, "growth", project)
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

      nc_get <- function(name) {
        tryCatch(ncdf4::ncvar_get(nc.out, name), error = function(e) NULL)
      }
      no3 <- nc_get("NO3")
      nh3 <- nc_get("NH3")
      if (is.null(no3) && is.null(nh3)) {
        shiny::showNotification("NO3/NH3 not found in output.nc.", type = "error")
        return()
      }
      DIN <- if (is.null(nh3)) no3 else if (is.null(no3)) nh3 else no3 + nh3
      Si <- nc_get("Si")
      l.eddy <- nc_get("eddy")
      if (!is.null(l.eddy)) l.eddy <- l.eddy * ed.scl
      IRR <- nc_get("Light")
      if (is.null(IRR)) {
        shiny::showNotification("Light not found in output.nc.", type = "error")
        return()
      }

      l.light <- l.nut <- list()
      for (fg in seq_along(nam.fg)) {
        din_lim <- DIN / (KN$Value[fg] + DIN)
        if (!is.null(Si) && !is.null(KS) && fg <= nrow(KS) && is.finite(KS$Value[fg])) {
          si_lim <- Si / (KS$Value[fg] + Si)
          if (flagnut$Value[1] == 0) {
            l.nut[[fg]] <- pmin(din_lim, si_lim, na.rm = TRUE)
          } else if (flagnut$Value[1] == 1) {
            l.nut[[fg]] <- sqrt(din_lim * si_lim)
          } else if (flagnut$Value[1] == 2) {
            l.nut[[fg]] <- 2 / (din_lim + si_lim)
          } else {
            l.nut[[fg]] <- din_lim
          }
        } else {
          l.nut[[fg]] <- din_lim
        }
        ki <- if (!is.null(KI) && fg <= nrow(KI)) KI$Value[fg] else 1
        l.light[[fg]] <- pmin((IRR / ki), 1, na.rm = TRUE)
      }

      n.box <- if (length(dim(IRR)) >= 2) dim(IRR)[2] else 1
      n.lay <- if (length(dim(IRR)) >= 1) dim(IRR)[1] else 1
      rv$l_nut <- l.nut
      rv$l_light <- l.light
      rv$l_eddy <- l.eddy
      rv$cod_fg <- cod.fg
      rv$n_box <- n.box
      rv$n_layer <- n.lay
      rv$built <- TRUE
    })

    output$growth_plot <- renderPlot({
      plot_or_message("Load output.nc with Light and a biology .prm to plot growth limitation", {
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

      layers <- if (isTRUE(input$all_layers)) seq_len(n_layers) else {
        ly <- as.integer(input$layer) + 1L
        if (!is.finite(ly) || ly < 1 || ly > n_layers) 1L else ly
      }
      eddy_ts <- eddy_time_series(eddy_data, box_idx, n_time)
      rows <- list()
      for (layer in layers) {
        nut_layer <- as.numeric(nut_data[layer, box_idx, ])
        light_layer <- as.numeric(light_data[layer, box_idx, ])
        growth_layer <- pmin(nut_layer * light_layer, 1)
        eddy_layer <- eddy_ts
        if (isTRUE(input$log_scale)) {
          nut_layer <- log(pmax(nut_layer, 1e-10))
          light_layer <- log(pmax(light_layer, 1e-10))
          eddy_layer <- log(pmax(eddy_layer, 1e-10))
          growth_layer <- log(pmax(growth_layer, 1e-10))
        }
        lab <- paste("Layer", layer - 1)
        rows[[length(rows) + 1]] <- data.frame(Time = Time, Value = growth_layer,
                                               Series = "Growth (light × nutrient)", Layer = lab)
        rows[[length(rows) + 1]] <- data.frame(Time = Time, Value = nut_layer,
                                               Series = "Nutrient", Layer = lab)
        rows[[length(rows) + 1]] <- data.frame(Time = Time, Value = light_layer,
                                               Series = "Light", Layer = lab)
        rows[[length(rows) + 1]] <- data.frame(Time = Time, Value = eddy_layer,
                                               Series = "Eddy / space", Layer = lab)
      }
      plot_df <- do.call(rbind, rows)
      pal <- c(`Growth (light × nutrient)` = "#0B3C5D", Nutrient = "#1D7874",
               Light = "#C4A35A", `Eddy / space` = "#A33B20")
      ylab <- if (isTRUE(input$log_scale)) "log(Limitation)" else "Limitation (0–1)"
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Time, y = Value, colour = Series))
      p <- p + ggplot2::geom_line(linewidth = 1, na.rm = TRUE)
      if (length(layers) > 1) {
        p <- p + ggplot2::facet_wrap(~ Layer, ncol = 1)
      }
      p <- p + ggplot2::scale_color_manual(values = pal)
      if (!isTRUE(input$log_scale)) {
        p <- p + ggplot2::coord_cartesian(ylim = c(0, 1))
      }
      p <- p + ggplot2::labs(
        title = paste(input$fg, "— box", input$box),
        x = "Year", y = ylab, colour = "Limitation factor"
      )
      p + theme_reactiveatlantis(base_size = 16) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 13),
          axis.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 13),
          strip.text = ggplot2::element_text(size = 14)
        )
      })
    })

    keep_outputs_alive(output, c("banner", "growth_plot"))
  })
}

eddy_time_series <- function(eddy, box_idx, n_time) {
  if (is.null(eddy)) return(rep(NA_real_, n_time))
  d <- dim(eddy)
  if (is.null(d) || length(d) == 1) {
    v <- as.numeric(eddy)
    if (length(v) == n_time) return(v)
    return(rep(mean(v, na.rm = TRUE), n_time))
  }
  if (length(d) == 2) {
    if (d[2] == n_time) return(as.numeric(eddy[min(box_idx, d[1]), ]))
    if (d[1] == n_time) return(as.numeric(eddy[, min(box_idx, d[2])]))
    return(rep(mean(eddy[min(box_idx, d[1]), ], na.rm = TRUE), n_time))
  }
  ## z, b, t
  b <- min(box_idx, d[2])
  v <- apply(eddy[, b, , drop = FALSE], 3, mean, na.rm = TRUE)
  as.numeric(v)
}
