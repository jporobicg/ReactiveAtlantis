## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   Spatial map (viewer-style) + by-age view  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 300, open = "open",
        selectInput(ns("view"), "View",
                    choices = c("Map" = "map", "By age class" = "age"),
                    selected = "map"),
        selectInput(ns("kind"), "Variable",
                    choices = c("Biomass" = "biomass",
                                "Numbers" = "numbers",
                                "Tracer" = "tracer"),
                    selected = "biomass"),
        selectInput(ns("fg"), "Functional group", choices = character()),
        conditionalPanel(
          "input.kind == 'tracer'", ns = ns,
          selectInput(ns("tracer"), "Tracer", choices = character())
        ),
        selectInput(ns("layer"), "Depth layer",
                    choices = c("All layers (integrated)" = "integrated")),
        conditionalPanel(
          "input.view == 'map'", ns = ns,
          sliderInput(ns("time_i"), "Time step", min = 1, max = 1, value = 1, step = 1),
          textOutput(ns("time_lab")),
          fluidRow(
            column(6, actionButton(ns("play"), "Play", icon = icon("play"), width = "100%")),
            column(6, sliderInput(ns("play_speed"), "Speed", min = 0.2, max = 2,
                                  value = 0.6, step = 0.2, ticks = FALSE))
          ),
          selectInput(ns("ts_scope"), "Time series",
                      choices = c("Selected box" = "box",
                                  "Whole system" = "system",
                                  "All boxes" = "boxes"),
                      selected = "box"),
          checkboxInput(ns("stable_scale"), "Stable colour scale across time", TRUE),
          checkboxInput(ns("highlight_boundary"), "Highlight boundary boxes", TRUE),
          downloadButton(ns("dl_map"), "Map PNG", class = "btn-sm"),
          downloadButton(ns("dl_csv"), "Series CSV", class = "btn-sm")
        ),
        conditionalPanel(
          "input.view == 'age'", ns = ns,
          selectInput(ns("box"), "Box", choices = 0),
          checkboxInput(ns("show_biomass"), "Biomass", TRUE),
          checkboxInput(ns("show_resn"), "Reserve N", TRUE),
          checkboxInput(ns("show_structn"), "Structural N", TRUE),
          checkboxInput(ns("show_numbers"), "Numbers", TRUE)
        )
      ),
      conditionalPanel(
        "input.view == 'map'", ns = ns,
        plotly::plotlyOutput(ns("map_plot"), height = "520px"),
        plotOutput(ns("box_ts"), height = "280px")
      ),
      conditionalPanel(
        "input.view == 'age'", ns = ns,
        plotOutput(ns("age_plot"), height = "640px")
      )
    )
  )
}

spatial_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ps <- shiny::reactive(project_snapshot(project))
    playing <- shiny::reactiveVal(FALSE)

    output$banner <- renderUI(tool_banner(ps(), "spatial"))

    layer_info <- reactive({
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      if (is.null(dat$nc_out)) return(NULL)
      atlantis_layer_info(dat$nc_out, dat$inf_box)
    })

    tracers <- reactive({
      dat <- ensure_project_data(ps(), "nc_out")
      if (is.null(dat$nc_out)) return(character())
      vars <- names(dat$nc_out$var)
      found <- vapply(PHYSICS_CANDIDATES, function(cands) match_nc_var(vars, cands),
                      character(1))
      found[!is.na(found)]
    })

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      n_box <- if (!is.null(dat$nc_out) && !is.null(dat$nc_out$dim$b)) {
        dat$nc_out$dim$b$len
      } else {
        1
      }
      update_select_keep(session, "box", as.character(seq(0, max(0, n_box - 1))),
                         input$box)
      if (identical(input$view, "age") || identical(input$kind, "numbers")) {
        update_select_keep(session, "fg", age_select_choices(ps()$data$grp), input$fg)
      } else {
        update_select_keep(session, "fg", fg_select_choices(ps()$data$grp), input$fg)
      }
      tr <- tracers()
      if (length(tr) > 0) {
        labs <- names(tr)
        if (is.null(labs) || !any(nzchar(labs))) labs <- unname(tr)
        update_select_keep(session, "tracer", stats::setNames(unname(tr), labs),
                           input$tracer)
      }
    })

    observe({
      info <- layer_info()
      if (is.null(info)) return()
      update_select_keep(session, "layer", layer_choices(info), input$layer)
    })

    fg_d <- ra_debounce(shiny::reactive(input$fg), 250)
    layer_d <- ra_debounce(shiny::reactive(input$layer), 250)

    polys <- reactive({
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      path <- dat$bgm_path
      if (is.null(path) || !file.exists(as.character(path)[1])) {
        path <- ps()$paths$bgm
      }
      req(path_ok(path))
      poly <- read_bgm_polygons(path)
      if (nrow(poly) == 0) {
        stop("No box polygons were found in the BGM file.")
      }
      poly
    })

    box_vals <- reactive({
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out, dat$grp)
      info <- atlantis_layer_info(dat$nc_out, dat$inf_box)
      key <- layer_d()
      if (is.null(key) || !nzchar(as.character(key))) key <- "integrated"
      kind <- if (is.null(input$kind)) "biomass" else input$kind
      vals <- spatial_box_values(
        dat$nc_out, dat$grp, kind = kind, fg_code = fg_d(),
        tracer = input$tracer, layer_key = key, info = info
      )
      n_t <- if (nrow(vals) > 0) max(vals$Time) else 1
      shiny::updateSliderInput(session, "time_i", min = 1, max = max(1, n_t))
      vals
    })

    output$time_lab <- renderText({
      dat <- ps()$data
      req(input$time_i)
      tdates <- dat$time_dates
      if (!is.null(tdates) && length(tdates) >= input$time_i) {
        paste("Selected:", format(tdates[input$time_i]))
      } else {
        paste("Time step", input$time_i)
      }
    })

    observeEvent(input$play, {
      playing(!isTRUE(playing()))
      updateActionButton(session, "play",
                         label = if (isTRUE(playing())) "Stop" else "Play",
                         icon = icon(if (isTRUE(playing())) "stop" else "play"))
    })

    observe({
      req(isTRUE(playing()))
      n_t <- isolate({
        v <- tryCatch(box_vals(), error = function(e) NULL)
        if (is.null(v) || nrow(v) == 0) 1L else max(v$Time)
      })
      cur <- isolate(as.integer(input$time_i)[1])
      if (!is.finite(cur)) cur <- 1L
      nxt <- if (cur >= n_t) 1L else cur + 1L
      shiny::updateSliderInput(session, "time_i", value = nxt)
      shiny::invalidateLater(max(0.2, as.numeric(input$play_speed)[1]) * 1000)
    })

    observeEvent(plotly::event_data("plotly_click", source = ns("map")), {
      ev <- plotly::event_data("plotly_click", source = ns("map"))
      key <- ev$key
      if (is.null(key) || !length(key)) key <- ev$customdata
      box <- suppressWarnings(as.integer(key[1]))
      if (is.finite(box)) {
        shiny::updateSelectInput(session, "box", selected = as.character(box))
      }
    }, ignoreNULL = TRUE)

    map_title <- function() {
      info <- layer_info()
      lay_lab <- layer_label(info, layer_d())
      kind <- if (is.null(input$kind)) "biomass" else input$kind
      what <- if (identical(kind, "tracer")) {
        input$tracer
      } else if (identical(kind, "numbers")) {
        paste("Numbers", fg_d())
      } else {
        paste("Biomass", fg_d())
      }
      paste(what, "—", lay_lab)
    }

    map_gg <- reactive({
      poly <- polys()
      vals <- box_vals()
      req(nrow(poly) > 0)
      time_i <- if (is.null(input$time_i)) 1L else as.integer(input$time_i)[1]
      if (!is.finite(time_i)) time_i <- 1L
      poly <- spatial_join_polygons(poly, vals, time_i)
      if (!isTRUE(input$highlight_boundary)) poly$boundary <- FALSE
      fill_lim <- NULL
      if (isTRUE(input$stable_scale) && nrow(vals) > 0) {
        rng <- range(vals$Value, na.rm = TRUE)
        if (all(is.finite(rng))) fill_lim <- rng
      }
      pal <- tryCatch(get_fg_colors(fg_d(), ps()$data$fg_palette),
                      error = function(e) c("#0B3C5D"))
      fill_high <- if (identical(input$kind, "biomass") && length(pal)) {
        unname(pal[1])
      } else {
        "#0B3C5D"
      }
      plot_spatial_map(poly, fill_lim = fill_lim, fill_high = fill_high,
                       selected_box = input$box, title = map_title(),
                       legend = if (identical(input$kind, "biomass")) "Biomass (t)"
                       else if (identical(input$kind, "numbers")) "Numbers"
                       else as.character(input$tracer))
    })

    output$map_plot <- plotly::renderPlotly({
      p <- tryCatch(map_gg(), error = function(e) {
        msg <- if (inherits(e, "shiny.silent.error")) {
          "Load a run with output.nc and a BGM to map biomass"
        } else {
          e$message
        }
        ggplot2::ggplot() + ggplot2::labs(title = msg) + theme_reactiveatlantis()
      })
      plt <- ra_ggplotly(p, tooltip = "text")
      if (ra_is_plotly(plt)) {
        plt$x$source <- ns("map")
        tryCatch(plotly::event_register(plt, "plotly_click"), error = function(e) plt)
      }
      plt
    })

    output$box_ts <- renderPlot({
      plot_or_message("Load a run to plot the selected box or the whole system", {
      vals <- box_vals()
      req(nrow(vals) > 0)
      dat <- ensure_project_data(ps(), "nc_out")
      scope <- if (is.null(input$ts_scope)) "box" else input$ts_scope
      box <- as.integer(input$box)[1]
      if (!is.finite(box)) box <- 0L
      df <- spatial_ts_values(vals, scope = scope, box = box,
                              boxes = unique(vals$Box))
      ylab <- if (identical(input$kind, "biomass")) "Biomass (t)"
      else if (identical(input$kind, "numbers")) "Numbers"
      else as.character(input$tracer)
      plot_spatial_ts(df, dat$time_dates, time_i = input$time_i,
                      title = map_title(), ylab = ylab)
      })
    })

    output$dl_map <- downloadHandler(
      filename = function() paste0("spatial-map-", Sys.Date(), ".png"),
      content = function(file) {
        p <- map_gg()
        ggplot2::ggsave(file, p, width = 8, height = 7, dpi = 120)
      }
    )
    output$dl_csv <- downloadHandler(
      filename = function() paste0("spatial-series-", Sys.Date(), ".csv"),
      content = function(file) {
        vals <- box_vals()
        scope <- if (is.null(input$ts_scope)) "box" else input$ts_scope
        box <- as.integer(input$box)[1]
        if (!is.finite(box)) box <- 0L
        df <- spatial_ts_values(vals, scope = scope, box = box,
                                boxes = unique(vals$Box))
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$age_plot <- renderPlot({
      plot_or_message("Load a run to plot age classes in a box and layer", {
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out, dat$grp, fg_d())
      info <- atlantis_layer_info(dat$nc_out, dat$inf_box)
      key <- layer_d()
      if (is.null(key) || !nzchar(as.character(key))) key <- "integrated"
      box <- as.integer(input$box)
      df <- age_class_long(dat$nc_out, dat$grp, fg_d(), box = box,
                           layer_key = key, info = info)
      vars <- c(
        if (isTRUE(input$show_biomass)) "Biomass",
        if (isTRUE(input$show_resn)) "ResN",
        if (isTRUE(input$show_structn)) "StructN",
        if (isTRUE(input$show_numbers)) "Numbers"
      )
      df <- df[df$Variable %in% vars, , drop = FALSE]
      req(nrow(df) > 0)
      tdates <- dat$time_dates
      if (!is.null(tdates) && length(tdates) >= max(df$Time)) {
        df$Date <- tdates[df$Time]
      } else {
        df$Date <- days_as_date(df$Time, dat$origin)
      }
      pal <- age_class_palette(length(unique(df$Age)), sort(unique(df$AgeLab)))
      lay_lab <- layer_label(info, key)
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Value, colour = AgeLab))
      p <- p + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)
      p <- p + ggplot2::facet_wrap(~ Variable, ncol = 1, scales = "free_y")
      p <- p + ggplot2::scale_colour_manual(values = pal, name = "Age class")
      p <- p + ggplot2::labs(
        title = paste("Age classes —", fg_d(), "— box", box, "—", lay_lab),
        x = "Year", y = NULL
      )
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, df$Date)
      })
    })

    keep_outputs_alive(output, "banner")
  })
}
