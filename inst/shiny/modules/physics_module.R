## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~         Physics and nutrient tracers        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

physics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("tracer"), "Tracer", choices = character()),
        selectInput(ns("box"), "Box", choices = 0),
        checkboxInput(ns("all_boxes"), "Mean across boxes", FALSE),
        hr(),
        h6("Layer thickness"),
        p(class = "ra-muted", "nominal_dz compared with the number of water layers in this box.")
      ),
      plotOutput(ns("tracer_plot"), height = "480px"),
      plotOutput(ns("dz_plot"), height = "260px")
    )
  )
}

physics_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))

    available <- reactive({
      dat <- ensure_project_data(ps(), "nc_out")
      req(dat$nc_out)
      vars <- names(dat$nc_out$var)
      found <- vapply(PHYSICS_CANDIDATES, function(cands) match_nc_var(vars, cands),
                      character(1))
      found[!is.na(found)]
    })

    output$banner <- renderUI(tool_banner(ps(), "physics"))

    output$tracer_plot <- renderPlot({
      plot_or_message("Load output.nc to plot temperature, salt and nutrients", {
      dat <- ensure_project_data(ps(), "nc_out")
      req(dat$nc_out)
      tr <- available()
      if (length(tr) > 0) {
        n_box <- if (!is.null(dat$nc_out$dim$b)) dat$nc_out$dim$b$len else 1
        update_select_keep(session, "tracer", tr, input$tracer)
        update_select_keep(session, "box", as.character(seq(0, max(0, n_box - 1))), input$box)
      }
      req(input$tracer)
      box <- if (isTRUE(input$all_boxes)) NULL else as.integer(input$box)
      df <- physics_long(dat$nc_out, input$tracer, box = box)
      req(nrow(df) > 0)
      tdates <- dat$time_dates
      if (!is.null(tdates) && length(tdates) >= max(df$Time)) {
        df$Date <- tdates[df$Time]
      } else {
        df$Date <- days_as_date(df$Time, dat$origin)
      }
      if (!"LayerLab" %in% names(df) || all(!nzchar(as.character(df$LayerLab)))) {
        df$LayerLab <- paste("Layer", df$Layer)
      }
      if (isTRUE(input$all_boxes)) {
        df <- stats::aggregate(Value ~ Date + LayerLab, data = df, mean, na.rm = TRUE)
        names(df)[names(df) == "LayerLab"] <- "LayerLab"
      }
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Value, colour = LayerLab))
      p <- p + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)
      p <- p + ggplot2::facet_wrap(~ LayerLab, ncol = 1, scales = "free_y")
      p <- p + ggplot2::labs(
        title = paste(names(available())[available() == input$tracer],
                      if (isTRUE(input$all_boxes)) "— mean of boxes" else paste("— box", input$box)),
        x = "Year", y = input$tracer
      )
      p <- p + theme_reactiveatlantis() + ggplot2::theme(legend.position = "none")
      ra_scale_x_year(p, df$Date)
      })
    })

    output$dz_plot <- renderPlot({
      plot_or_message("Load output.nc to plot layer thickness", {
      dat <- ensure_project_data(ps(), "nc_out")
      req(dat$nc_out)
      dz <- layer_thickness(dat$nc_out)
      if (is.null(dz)) {
        plot.new()
        title("No nominal_dz in this NetCDF")
        return(invisible(NULL))
      }
      arr <- dz$array
      box <- as.integer(input$box) + 1L
      if (length(dim(arr)) == 2) {
        if (box > ncol(arr) && box <= nrow(arr)) {
          thick <- as.numeric(arr[box, ])
        } else {
          thick <- as.numeric(arr[, min(box, ncol(arr))])
        }
        df <- data.frame(Layer = seq_along(thick) - 1L, Thickness = thick)
        p <- ggplot2::ggplot(df, ggplot2::aes(x = factor(Layer), y = Thickness))
        p <- p + ggplot2::geom_col(fill = "#0B3C5D")
        p <- p + ggplot2::labs(title = paste("nominal_dz — box", input$box),
                               x = "Layer", y = "Thickness (m)")
        p + theme_reactiveatlantis()
      } else if (length(dim(arr)) == 3) {
        n_t <- dim(arr)[3]
        n_z <- dim(arr)[1]
        b <- min(box, dim(arr)[2])
        tdates <- dat$time_dates
        rows <- lapply(seq_len(n_z), function(z) {
          data.frame(
            Date = if (!is.null(tdates) && length(tdates) == n_t) tdates else seq_len(n_t),
            Thickness = as.numeric(arr[z, b, ]),
            Layer = paste("Layer", z - 1L)
          )
        })
        df <- do.call(rbind, rows)
        p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Thickness, colour = Layer))
        p <- p + ggplot2::geom_line()
        p <- p + ggplot2::labs(title = paste("Layer thickness through time — box", input$box),
                               x = "Year", y = "Thickness (m)")
        p <- p + theme_reactiveatlantis()
        ra_scale_x_year(p, df$Date)
      } else {
        plot.new()
        title("Unexpected nominal_dz shape")
      }
      })
    })

    keep_outputs_alive(output, "banner")
  })
}
