## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~              Spatial biomass map            ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## Darker than the fill and the sand theme so box edges stay visible.
## Line weight is unchanged (same as the previous hairline stroke).
RA_BOX_OUTLINE <- "#0A1218"

spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("view"), "View",
                    choices = c("Map" = "map", "By age class" = "age"),
                    selected = "map"),
        selectInput(ns("fg"), "Functional group", choices = character()),
        selectInput(ns("layer"), "Depth layer",
                    choices = c("All layers (integrated)" = "integrated")),
        conditionalPanel(
          "input.view == 'map'", ns = ns,
          sliderInput(ns("time_i"), "Time step", min = 1, max = 1, value = 1, step = 1),
          textOutput(ns("time_lab")),
          checkboxInput(ns("stable_scale"), "Stable colour scale across time", TRUE),
          checkboxInput(ns("highlight_boundary"), "Highlight boundary boxes", TRUE)
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
        plotOutput(ns("map_plot"), height = "420px"),
        plotOutput(ns("box_ts"), height = "320px")
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
    ps <- shiny::reactive(project_snapshot(project))

    output$banner <- renderUI(tool_banner(ps(), "spatial"))

    layer_info <- reactive({
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      if (is.null(dat$nc_out)) return(NULL)
      atlantis_layer_info(dat$nc_out, dat$inf_box)
    })

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      dat <- ensure_project_data(ps(), "nc_out")
      n_box <- if (!is.null(dat$nc_out) && !is.null(dat$nc_out$dim$b)) {
        dat$nc_out$dim$b$len
      } else {
        1
      }
      update_select_keep(session, "box", as.character(seq(0, max(0, n_box - 1))),
                         input$box)
      if (identical(input$view, "age")) {
        update_select_keep(session, "fg", age_select_choices(ps()$data$grp), input$fg)
      } else {
        update_select_keep(session, "fg", fg_select_choices(ps()$data$grp), input$fg)
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
      dat <- ps()$data
      req(dat$bgm_path)
      read_bgm_polygons(dat$bgm_path)
    })

    box_bio <- reactive({
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out, dat$grp, fg_d())
      info <- atlantis_layer_info(dat$nc_out, dat$inf_box)
      key <- layer_d()
      if (is.null(key) || !nzchar(as.character(key))) key <- "integrated"
      bio <- biomass_by_box_layer(dat$nc_out, dat$grp, fg_d(), key, info)
      n_t <- if (nrow(bio) > 0) max(bio$Time) else 1
      shiny::updateSliderInput(session, "time_i", min = 1, max = max(1, n_t))
      bio
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

    output$map_plot <- renderPlot({
      plot_or_message("Load a run with output.nc and a BGM to map biomass", {
      poly <- polys()
      bio <- box_bio()
      req(nrow(poly) > 0, nrow(bio) > 0, input$time_i)
      snap <- bio[bio$Time == input$time_i, ]
      poly$Biomass <- snap$Biomass[match(poly$boxid, snap$Box)]
      pal <- get_fg_colors(fg_d(), ps()$data$fg_palette)
      fill_lim <- NULL
      if (isTRUE(input$stable_scale)) {
        rng <- range(bio$Biomass, na.rm = TRUE)
        if (all(is.finite(rng))) fill_lim <- rng
      }
      info <- layer_info()
      lay_lab <- layer_label(info, layer_d())
      p <- ggplot2::ggplot(poly, ggplot2::aes(x = x, y = y, group = boxid, fill = Biomass))
      p <- p + ggplot2::geom_polygon(colour = RA_BOX_OUTLINE, linewidth = 0.25)
      p <- p + ggplot2::coord_equal()
      fill_args <- list(low = "#F7F4EC", high = unname(pal[1]),
                        na.value = "#E6E2D6", name = "Biomass (t)")
      if (!is.null(fill_lim)) fill_args$limits <- fill_lim
      p <- p + do.call(ggplot2::scale_fill_gradient, fill_args)
      if (isTRUE(input$highlight_boundary) && any(poly$boundary)) {
        bnd <- poly[poly$boundary, ]
        p <- p + ggplot2::geom_polygon(
          data = bnd, ggplot2::aes(x = x, y = y, group = boxid),
          fill = NA, colour = "#0B3C5D", linewidth = 0.9, inherit.aes = FALSE
        )
      }
      p <- p + ggplot2::labs(title = paste("Biomass by box —", fg_d(), "—", lay_lab),
                             x = NULL, y = NULL)
      p + theme_reactiveatlantis() +
        ggplot2::theme(axis.text = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank())
      })
    })

    output$box_ts <- renderPlot({
      plot_or_message("Load a run to plot biomass in each box", {
      bio <- box_bio()
      req(nrow(bio) > 0)
      dat <- ensure_project_data(ps(), "nc_out")
      tdates <- dat$time_dates
      if (!is.null(tdates) && length(tdates) >= max(bio$Time)) {
        bio$Date <- tdates[bio$Time]
      } else {
        bio$Date <- days_as_date(bio$Time, dat$origin)
      }
      poly <- tryCatch(polys(), error = function(e) NULL)
      bio$Boundary <- FALSE
      if (!is.null(poly)) {
        bnd <- unique(poly$boxid[poly$boundary])
        bio$Boundary <- bio$Box %in% bnd
      }
      bio$BoxLab <- paste("Box", bio$Box)
      n_box <- length(unique(bio$Box))
      p <- ggplot2::ggplot(bio, ggplot2::aes(x = Date, y = Biomass, colour = BoxLab,
                                             linewidth = Boundary))
      p <- p + ggplot2::geom_line(alpha = 0.85, na.rm = TRUE)
      p <- p + ggplot2::scale_linewidth_manual(values = c("FALSE" = 0.5, "TRUE" = 1.1),
                                               labels = c("Interior", "Boundary"),
                                               name = NULL)
      if (n_box > 12) {
        p <- p + ggplot2::guides(colour = "none")
      }
      info <- layer_info()
      lay_lab <- layer_label(info, layer_d())
      p <- p + ggplot2::labs(title = paste("Biomass through time in each box —", lay_lab),
                             x = "Year", y = "Biomass (tons)", colour = "Box")
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, bio$Date)
      })
    })

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
