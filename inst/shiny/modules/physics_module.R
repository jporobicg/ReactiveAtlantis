## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~         Physics and nutrient tracers        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

physics_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("body")))
}

physics_body_ui <- function(ns, tracers, n_box, n_layer) {
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "open",
      selectInput(ns("tracer"), "Tracer", choices = tracers),
      selectInput(ns("box"), "Box", choices = seq(0, max(0, n_box - 1))),
      checkboxInput(ns("all_boxes"), "Mean across boxes", FALSE),
      hr(),
      h6("Layer thickness"),
      p(class = "ra-muted", "nominal_dz compared with the number of water layers in this box.")
    ),
    plotOutput(ns("tracer_plot"), height = "480px"),
    plotOutput(ns("dz_plot"), height = "260px")
  )
}

physics_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {

    available <- reactive({
      dat <- project$data
      req(dat$nc_out)
      vars <- names(dat$nc_out$var)
      found <- vapply(PHYSICS_CANDIDATES, function(cands) match_nc_var(vars, cands),
                      character(1))
      found[!is.na(found)]
    })

    output$body <- renderUI({
      if (is.null(project) || !isTRUE(project$ready)) {
        return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS$physics], "Physics"))
      }
      miss <- tool_missing(project$paths, "physics")
      if (length(miss)) return(tool_missing_card(miss, "Physics"))
      tr <- available()
      if (length(tr) == 0) {
        return(bslib::card(
          bslib::card_header("No physics tracers"),
          bslib::card_body("The output NetCDF has no Temp, salt, NO3, NH3, Chl_a or denitrification variables.")
        ))
      }
      dat <- project$data
      n_box <- if (!is.null(dat$nc_out$dim$b)) dat$nc_out$dim$b$len else 1
      n_z <- if (!is.null(dat$nc_out$dim$z)) dat$nc_out$dim$z$len else 1
      physics_body_ui(session$ns, tr, n_box, n_z)
    })

    output$tracer_plot <- renderPlot({
      dat <- project$data
      req(dat$nc_out, input$tracer)
      box <- if (isTRUE(input$all_boxes)) NULL else as.integer(input$box)
      df <- physics_long(dat$nc_out, input$tracer, box = box)
      req(nrow(df) > 0)
      tdates <- dat$time_dates
      if (!is.null(tdates) && length(tdates) >= max(df$Time)) {
        df$Date <- tdates[df$Time]
      } else {
        df$Date <- days_as_date(df$Time, dat$origin)
      }
      df$LayerLab <- paste("Layer", df$Layer)
      if (isTRUE(input$all_boxes)) {
        df <- stats::aggregate(Value ~ Date + LayerLab, data = df, mean)
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
      p + theme_reactiveatlantis() + ggplot2::theme(legend.position = "none")
    })

    output$dz_plot <- renderPlot({
      dat <- project$data
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
        p + theme_reactiveatlantis()
      } else {
        plot.new()
        title("Unexpected nominal_dz shape")
      }
    })
  })
}
