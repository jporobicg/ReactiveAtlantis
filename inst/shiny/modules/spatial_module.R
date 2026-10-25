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
                    choices = c("Map" = "map",
                                "By age class" = "age",
                                "Predator-prey overlap" = "overlap"),
                    selected = "map"),
        conditionalPanel(
          "input.view != 'overlap'", ns = ns,
          selectInput(ns("kind"), "Variable",
                      choices = c("Biomass" = "biomass",
                                  "Numbers" = "numbers",
                                  "Tracer" = "tracer"),
                      selected = "biomass")
        ),
        selectInput(ns("fg"), "Functional group", choices = character()),
        conditionalPanel(
          "input.view == 'overlap'", ns = ns,
          selectInput(ns("prey"), "Prey group", choices = character()),
          p(class = "ra-muted",
            "Per-box overlap is 2 min(Bpred, Bprey) / (Bpred + Bprey). The series is Schoener's D across boxes.")
        ),
        conditionalPanel(
          "input.kind == 'tracer' && input.view != 'overlap'", ns = ns,
          selectInput(ns("tracer"), "Tracer", choices = character())
        ),
        selectInput(ns("layer"), "Depth layer",
                    choices = c("All layers (integrated)" = "integrated")),
        conditionalPanel(
          "input.view == 'map' || input.view == 'overlap'", ns = ns,
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
        "input.view == 'map' || input.view == 'overlap'", ns = ns,
        uiOutput(ns("map_status")),
        plotly::plotlyOutput(ns("map_plot"), height = "560px"),
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
      update_select_keep(session, "prey", fg_select_choices(ps()$data$grp), input$prey)
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
    prey_d <- ra_debounce(shiny::reactive(input$prey), 250)
    layer_d <- ra_debounce(shiny::reactive(input$layer), 250)

    bgm_lookup <- reactive({
      snap <- ps()
      dat <- tryCatch(ensure_project_data(snap, c("nc_out", "inf_box")),
                      error = function(e) snap$data)
      searched <- unique(c(
        ra_nz(snap$config_dir), ra_nz(snap$output_dir),
        ra_nz(dat$bgm_path), ra_nz(snap$paths$bgm)
      ))
      searched <- searched[nzchar(searched)]
      path <- find_bgm_path(
        explicit = c(dat$bgm_path, snap$paths$bgm),
        config_dir = snap$config_dir,
        output_dir = snap$output_dir,
        prm_paths = c(snap$paths$run_prm, snap$paths$bio_prm, snap$paths$force_prm)
      )
      list(path = path, searched = searched)
    })

    polys <- reactive({
      hit <- bgm_lookup()
      read_bgm_polygons(hit$path)
    })

    overlap_data <- reactive({
      dat <- tryCatch(ensure_project_data(ps(), c("nc_out", "inf_box")),
                      error = function(e) NULL)
      if (is.null(dat) || is.null(dat$nc_out) || is.null(dat$grp) ||
          is.null(fg_d()) || is.null(prey_d())) {
        return(spatial_overlap_index(NULL, NULL))
      }
      info <- atlantis_layer_info(dat$nc_out, dat$inf_box)
      key <- layer_d()
      if (is.null(key) || !nzchar(as.character(key))) key <- "integrated"
      pred <- spatial_box_values(
        dat$nc_out, dat$grp, kind = "biomass", fg_code = fg_d(),
        layer_key = key, info = info
      )
      prey <- spatial_box_values(
        dat$nc_out, dat$grp, kind = "biomass", fg_code = prey_d(),
        layer_key = key, info = info
      )
      ov <- spatial_overlap_index(pred, prey)
      if (nrow(ov$by_box) == 0) {
        miss <- c(
          if (is.null(pred) || nrow(pred) == 0) fg_d(),
          if (is.null(prey) || nrow(prey) == 0) prey_d()
        )
        attr(ov$by_box, "reason") <- paste(
          "No box biomass for", paste(unique(miss), collapse = " and "),
          "in output.nc (need Nums/ResN/StructN or _N)."
        )
      }
      ov
    })

    box_vals <- reactive({
      if (identical(input$view, "overlap")) {
        ov <- overlap_data()
        vals <- ov$by_box
        n_t <- if (!is.null(vals) && nrow(vals) > 0) max(vals$Time) else 1
        shiny::updateSliderInput(session, "time_i", min = 1, max = max(1, n_t))
        return(vals)
      }
      dat <- tryCatch(ensure_project_data(ps(), c("nc_out", "inf_box")),
                      error = function(e) NULL)
      if (is.null(dat) || is.null(dat$nc_out) || is.null(dat$grp)) {
        return(empty_spatial_values())
      }
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
      dat <- ra_with_nc_out(ps)
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
      if (identical(input$view, "overlap")) {
        return(paste("Overlap", fg_d(), "×", prey_d()))
      }
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

    map_diag <- reactive({
      hit <- tryCatch(bgm_lookup(), error = function(e) list(path = NA, searched = NULL))
      poly <- tryCatch(polys(), error = function(e) bgm_empty_poly(e$message))
      vals <- tryCatch(box_vals(), error = function(e) empty_spatial_values())
      dat <- tryCatch(ps()$data, error = function(e) NULL)
      diagnose_spatial_map(
        bgm_path = hit$path, poly = poly, values = vals,
        nc = if (!is.null(dat)) dat$nc_out else NULL,
        fg = fg_d(), layer_key = layer_d(),
        searched = hit$searched
      )
    })

    output$map_status <- renderUI({
      d <- tryCatch(map_diag(), error = function(e) {
        list(ok = FALSE, notes = e$message)
      })
      cls <- if (isTRUE(d$ok)) "ra-map-status is-ok" else "ra-map-status is-bad"
      shiny::div(
        class = cls,
        shiny::tags$ul(lapply(as.character(d$notes), function(n) shiny::tags$li(n)))
      )
    })

    map_joined <- reactive({
      poly <- polys()
      vals <- tryCatch(box_vals(), error = function(e) empty_spatial_values())
      if (is.null(poly) || nrow(poly) == 0) return(poly)
      time_i <- if (is.null(input$time_i)) 1L else as.integer(input$time_i)[1]
      if (!is.finite(time_i)) time_i <- 1L
      poly <- spatial_join_polygons(poly, vals, time_i)
      if (!isTRUE(input$highlight_boundary)) poly$boundary <- FALSE
      poly
    })

    map_fill <- reactive({
      vals <- tryCatch(box_vals(), error = function(e) empty_spatial_values())
      fill_lim <- NULL
      if (isTRUE(input$stable_scale) && !is.null(vals) && nrow(vals) > 0) {
        rng <- range(vals$Value, na.rm = TRUE)
        if (all(is.finite(rng))) fill_lim <- rng
      }
      pal <- tryCatch(get_fg_colors(fg_d(), ps()$data$fg_palette),
                      error = function(e) c("#0B3C5D"))
      fill_high <- if (identical(input$view, "overlap")) {
        "#1D7874"
      } else if (identical(input$kind, "biomass") && length(pal)) {
        unname(pal[1])
      } else {
        "#0B3C5D"
      }
      list(lim = fill_lim, high = fill_high)
    })

    map_gg <- reactive({
      poly <- map_joined()
      if (is.null(poly) || nrow(poly) == 0) {
        stop(attr(poly, "reason") %||% "No box polygons were found in the BGM file.")
      }
      fl <- map_fill()
      plot_spatial_map(poly, fill_lim = fl$lim, fill_high = fl$high,
                       selected_box = input$box, title = map_title(),
                       legend = spatial_legend_lab(input$view, input$kind, input$tracer))
    })

    output$map_plot <- plotly::renderPlotly({
      poly <- tryCatch(map_joined(), error = function(e) bgm_empty_poly(e$message))
      if (is.null(poly) || nrow(poly) == 0) {
        d <- tryCatch(map_diag(), error = function(e) NULL)
        msg <- if (!is.null(d) && length(d$notes)) {
          paste(d$notes, collapse = " ")
        } else {
          attr(poly, "reason") %||% "Load a run with output.nc and a BGM to map biomass"
        }
        plt <- spatial_empty_plotly(msg)
      } else {
        fl <- map_fill()
        plt <- plot_spatial_map_plotly(
          poly, fill_lim = fl$lim, fill_high = fl$high,
          selected_box = input$box, title = map_title(),
          legend = spatial_legend_lab(input$view, input$kind, input$tracer)
        )
      }
      if (ra_is_plotly(plt)) {
        plt$x$source <- ns("map")
        plt <- plotly::config(plt, displaylogo = FALSE, scrollZoom = TRUE,
                              modeBarButtonsToRemove = c("lasso2d", "select2d"))
        tryCatch(plotly::event_register(plt, "plotly_click"), error = function(e) plt)
      }
      plt
    })

    output$box_ts <- renderPlot({
      plot_or_message("Load a run to plot the selected box or the whole system", {
      if (identical(input$view, "overlap") &&
          identical(if (is.null(input$ts_scope)) "box" else input$ts_scope, "system")) {
        ov <- overlap_data()
        ts <- ov$by_time
        if (is.null(ts) || nrow(ts) == 0) {
          plot_message(attr(ov$by_box, "reason") %||%
                         "Need biomass by box for both predator and prey.")
          return(invisible(NULL))
        }
        dat <- ensure_project_data(ps(), "nc_out")
        df <- data.frame(Time = ts$Time, Box = -1L, Value = ts$Schoener,
                         Variable = "Schoener", stringsAsFactors = FALSE)
        plot_spatial_ts(df, dat$time_dates, time_i = input$time_i,
                        title = map_title(), ylab = "Schoener overlap")
      } else {
        vals <- box_vals()
        req(nrow(vals) > 0)
        dat <- ensure_project_data(ps(), "nc_out")
        scope <- if (is.null(input$ts_scope)) "box" else input$ts_scope
        box <- as.integer(input$box)[1]
        if (!is.finite(box)) box <- 0L
        df <- spatial_ts_values(vals, scope = scope, box = box,
                                boxes = unique(vals$Box))
        ylab <- if (identical(input$view, "overlap")) "Overlap"
        else if (identical(input$kind, "biomass")) "Biomass (t)"
        else if (identical(input$kind, "numbers")) "Numbers"
        else as.character(input$tracer)
        plot_spatial_ts(df, dat$time_dates, time_i = input$time_i,
                        title = map_title(), ylab = ylab)
      }
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
    keep_outputs_alive(output, "map_status")
    keep_outputs_alive(output, "map_plot")
  })
}

spatial_legend_lab <- function(view, kind, tracer) {
  if (identical(view, "overlap")) return("Overlap")
  if (identical(kind, "biomass")) return("Biomass (t)")
  if (identical(kind, "numbers")) return("Numbers")
  as.character(tracer)
}
