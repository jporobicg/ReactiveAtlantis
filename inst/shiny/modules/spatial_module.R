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
    ps <- shiny::reactive(project_snapshot(project))

    output$banner <- renderUI(tool_banner(ps(), "spatial"))

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      update_select_keep(session, "fg", fg_select_choices(ps()$data$grp), input$fg)
    })

    fg_d <- ra_debounce(shiny::reactive(input$fg), 250)

    polys <- reactive({
      dat <- ps()$data
      req(dat$bgm_path)
      read_bgm_polygons(dat$bgm_path)
    })

    box_bio <- reactive({
      dat <- ensure_project_data(ps(), "nc_out")
      req(dat$nc_out, dat$grp, fg_d())
      bio <- biomass_by_box(dat$nc_out, dat$grp, fg_d())
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
      p <- p + ggplot2::labs(title = paste("Biomass by box —", fg_d()),
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
      p <- p + ggplot2::geom_line(alpha = 0.85)
      p <- p + ggplot2::scale_linewidth_manual(values = c("FALSE" = 0.5, "TRUE" = 1.1),
                                               labels = c("Interior", "Boundary"),
                                               name = NULL)
      if (n_box > 12) {
        p <- p + ggplot2::guides(colour = "none")
      }
      p <- p + ggplot2::labs(title = "Biomass through time in each box",
                             x = "Year", y = "Biomass (tons)", colour = "Box")
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, bio$Date)
      })
    })

    keep_outputs_alive(output, "banner")
  })
}
