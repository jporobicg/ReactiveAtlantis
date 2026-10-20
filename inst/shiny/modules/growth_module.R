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
    ps <- shiny::reactive(project_snapshot(project))
    rv <- reactiveValues(
      l_nut = NULL, l_light = NULL, l_eddy = NULL,
      cod_fg = NULL, n_box = NULL, n_layer = NULL, built = FALSE, token = NULL
    )

    output$banner <- renderUI(tool_banner(ps(), "growth"))

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      grp <- ps()$data$grp
      pp.grp <- with(grp, which(grouptype %in% c("PHYTOBEN", "SM_PHY", "LG_PHY", "SEAGRASS",
                                                 "DINOFLAG", "TURF", "MICROPHTYBENTHOS") &
                                isturnedon == 1))
      update_select_keep(session, "fg", as.character(grp$code[pp.grp]), input$fg)
    })

    fg_d <- ra_debounce(shiny::reactive(input$fg), 250)

    output$growth_plot <- renderPlot({
      plot_or_message("Load output.nc with Light and a biology .prm to plot growth limitation", {
      dat <- ensure_project_data(ps(), "nc_out")
      req(dat$nc_out, dat$grp, dat$prm_bio, fg_d(), input$box)
      lim <- growth_limitation_for_fg(dat$nc_out, dat$prm_bio, dat$grp, fg_d())
      req(!is.null(lim))
      n_box <- lim$n_box
      n_layer <- lim$n_layer
      update_select_keep(session, "box", as.character(seq(0, max(0, n_box - 1))), input$box)
      update_select_keep(session, "layer", as.character(seq(0, max(0, n_layer - 1))), input$layer)
      box_idx <- as.numeric(input$box) + 1
      if (box_idx > n_box) return()

      nut_data <- lim$nut
      light_data <- lim$light
      eddy_data <- lim$eddy
      n_layers <- dim(nut_data)[1]
      n_time <- dim(nut_data)[3]
      Time <- dat$time_dates
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
        title = paste(fg_d(), "— box", input$box),
        x = "Year", y = ylab, colour = "Limitation factor"
      )
      p <- p + theme_reactiveatlantis(base_size = 16) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = 13),
          axis.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 13),
          strip.text = ggplot2::element_text(size = 14)
        )
      ra_scale_x_year(p, Time)
      })
    })

    keep_outputs_alive(output, "banner")
  })
}

growth_limitation_for_fg <- function(nc, prm, grp, fg_code) {
  path <- tryCatch(nc$filename, error = function(e) "anon")
  ck <- paste("growthlim", file_cache_key(path, fg_code), sep = "|")
  hit <- ra_cache_get(ck)
  if (!is.null(hit)) return(hit)
  pp.grp <- with(grp, which(grouptype %in% c("PHYTOBEN", "SM_PHY", "LG_PHY", "SEAGRASS",
                                             "DINOFLAG", "TURF", "MICROPHTYBENTHOS") &
                            isturnedon == 1))
  if (length(pp.grp) == 0) return(NULL)
  fg_idx <- which(as.character(grp$code[pp.grp]) == as.character(fg_code))
  if (length(fg_idx) == 0) return(NULL)
  fg_idx <- fg_idx[1]
  options(warn = -1)
  flagnut <- text2num(prm, "flagnut ", FG = "look")
  if (nrow(flagnut) == 0) flagnut <- data.frame(FG = "flagnut", Value = 0)
  code <- as.character(grp$code[pp.grp][fg_idx])
  KN <- text2num(prm, paste0("KN_", code), FG = code)
  KS <- text2num(prm, paste0("KS_", code), FG = code)
  KI <- text2num(prm, paste0("KI_", code), FG = code)
  ed.scl <- text2num(prm, "eddy_scale", FG = "look")
  if (nrow(ed.scl) > 0) ed.scl <- ed.scl[1, 2] else ed.scl <- 1
  options(warn = 0)
  no3 <- nc_get(nc, "NO3")
  nh3 <- nc_get(nc, "NH3")
  if (is.null(no3) && is.null(nh3)) return(NULL)
  DIN <- if (is.null(nh3)) no3 else if (is.null(no3)) nh3 else no3 + nh3
  Si <- nc_get(nc, "Si")
  l.eddy <- nc_get(nc, "eddy")
  if (!is.null(l.eddy)) l.eddy <- l.eddy * ed.scl
  IRR <- nc_get(nc, "Light")
  if (is.null(IRR)) return(NULL)
  kn <- if (!is.null(KN) && nrow(KN) > 0) KN$Value[1] else 1
  din_lim <- DIN / (kn + DIN)
  if (!is.null(Si) && !is.null(KS) && nrow(KS) > 0 && is.finite(KS$Value[1])) {
    si_lim <- Si / (KS$Value[1] + Si)
    if (flagnut$Value[1] == 0) {
      l.nut <- pmin(din_lim, si_lim, na.rm = TRUE)
    } else if (flagnut$Value[1] == 1) {
      l.nut <- sqrt(din_lim * si_lim)
    } else if (flagnut$Value[1] == 2) {
      l.nut <- 2 / (din_lim + si_lim)
    } else {
      l.nut <- din_lim
    }
  } else {
    l.nut <- din_lim
  }
  ki <- if (!is.null(KI) && nrow(KI) > 0) KI$Value[1] else 1
  l.light <- pmin((IRR / ki), 1, na.rm = TRUE)
  out <- list(
    nut = l.nut,
    light = l.light,
    eddy = l.eddy,
    n_box = if (length(dim(IRR)) >= 2) dim(IRR)[2] else 1,
    n_layer = if (length(dim(IRR)) >= 1) dim(IRR)[1] else 1
  )
  ra_cache_set(ck, out)
  out
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
