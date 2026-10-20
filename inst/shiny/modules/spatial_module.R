## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~              Spatial biomass map            ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

spatial_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("fg"), "Functional group", choices = character()),
        sliderInput(ns("time_i"), "Time step", min = 1, max = 1, value = 1, step = 1),
        textOutput(ns("time_lab")),
        checkboxInput(ns("highlight_boundary"), "Highlight boundary boxes", TRUE)
      ),
      plotOutput(ns("map_plot"), height = "420px"),
      plotOutput(ns("box_ts"), height = "320px")
    )
  )
}

spatial_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {

    output$banner <- renderUI(tool_banner(project, "spatial"))

    observe({
      req(isTRUE(project$ready), project$data$grp)
      n_t <- if (!is.null(project$data$time_dates)) length(project$data$time_dates) else 1
      update_select_keep(session, "fg", fg_select_choices(project$data$grp), input$fg)
      shiny::updateSliderInput(session, "time_i", min = 1, max = max(1, n_t),
                               value = keep_numeric(input$time_i, 1, max(1, n_t), 1))
    })

    polys <- reactive({
      dat <- project$data
      req(dat$bgm_path)
      read_bgm_polygons(dat$bgm_path)
    })

    box_bio <- reactive({
      dat <- project$data
      req(dat$nc_out, dat$grp, input$fg)
      biomass_by_box(dat$nc_out, dat$grp, input$fg)
    })

    output$time_lab <- renderText({
      dat <- project$data
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
      pal <- get_fg_colors(input$fg, project$data$fg_palette)
      p <- ggplot2::ggplot(poly, ggplot2::aes(x = x, y = y, group = boxid, fill = Biomass))
      p <- p + ggplot2::geom_polygon(colour = "#F7F4EC", linewidth = 0.25)
      p <- p + ggplot2::coord_equal()
      p <- p + ggplot2::scale_fill_gradient(low = "#F7F4EC", high = unname(pal[1]),
                                            na.value = "#E6E2D6", name = "Biomass (t)")
      if (isTRUE(input$highlight_boundary) && any(poly$boundary)) {
        bnd <- poly[poly$boundary, ]
        p <- p + ggplot2::geom_polygon(
          data = bnd, ggplot2::aes(x = x, y = y, group = boxid),
          fill = NA, colour = "#0B3C5D", linewidth = 0.9, inherit.aes = FALSE
        )
      }
      p <- p + ggplot2::labs(title = paste("Biomass by box —", input$fg),
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
      tdates <- project$data$time_dates
      if (!is.null(tdates) && length(tdates) >= max(bio$Time)) {
        bio$Date <- tdates[bio$Time]
      } else {
        bio$Date <- days_as_date(bio$Time, project$data$origin)
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
      p <- p + ggplot2::geom_line(alpha = 0.85)
      p <- p + ggplot2::scale_linewidth_manual(values = c("FALSE" = 0.5, "TRUE" = 1.1),
                                               labels = c("Interior", "Boundary"),
                                               name = NULL)
      if (n_box > 12) {
        p <- p + ggplot2::guides(colour = "none")
      }
      p <- p + ggplot2::labs(title = "Biomass through time in each box",
                             x = "Year", y = "Biomass (tons)", colour = "Box")
      p + theme_reactiveatlantis()
      })
    })

    keep_outputs_alive(output, c("banner", "time_lab", "map_plot", "box_ts"))
  })
}
