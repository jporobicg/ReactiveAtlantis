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
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out)
      vars <- names(dat$nc_out$var)
      found <- vapply(PHYSICS_CANDIDATES, function(cands) match_nc_var(vars, cands),
                      character(1))
      found[!is.na(found)]
    })

    layer_info <- reactive({
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      if (is.null(dat$nc_out)) return(NULL)
      atlantis_layer_info(dat$nc_out, dat$inf_box)
    })

    output$banner <- renderUI(tool_banner(ps(), "physics"))

    output$tracer_plot <- renderPlot({
      plot_or_message("Load output.nc to plot temperature, salt and nutrients", {
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out)
      tr <- available()
      if (length(tr) > 0) {
        n_box <- if (!is.null(dat$nc_out$dim$b)) dat$nc_out$dim$b$len else 1
        update_select_keep(session, "tracer", tr, input$tracer)
        update_select_keep(session, "box", as.character(seq(0, max(0, n_box - 1))), input$box)
      }
      req(input$tracer)
      info <- layer_info()
      box <- if (isTRUE(input$all_boxes)) NULL else as.integer(input$box)
      df <- physics_long(dat$nc_out, input$tracer, box = box, info = info)
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
      if (!is.null(info) && exists("layer_lab_levels", mode = "function")) {
        levs <- layer_lab_levels(info)
        keep <- levs[levs %in% as.character(df$LayerLab)]
        if (length(keep)) df$LayerLab <- factor(as.character(df$LayerLab), levels = keep)
      }
      if (isTRUE(input$all_boxes)) {
        ## Volume-weighted mean across boxes for each layer (validation D18).
        if ("Vol" %in% names(df) && any(is.finite(df$Vol) & df$Vol > 0)) {
          key <- interaction(df$Date, df$LayerLab, drop = TRUE)
          sp <- split(df, key)
          df <- do.call(rbind, lapply(sp, function(x) data.frame(
            Date = x$Date[1], LayerLab = x$LayerLab[1],
            Value = ra_weighted_mean(x$Value, x$Vol))))
        } else {
          df <- stats::aggregate(Value ~ Date + LayerLab, data = df, mean, na.rm = TRUE)
        }
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
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out)
      info <- layer_info()
      box <- if (isTRUE(input$all_boxes)) NULL else as.integer(input$box)
      df <- layer_thickness_long(dat$nc_out, box = box, info = info)
      if (is.null(df)) {
        plot.new()
        title("No nominal_dz in this NetCDF")
        return(invisible(NULL))
      }
      req(nrow(df) > 0)
      if (!is.null(info) && exists("layer_lab_levels", mode = "function")) {
        levs <- layer_lab_levels(info)
        keep <- levs[levs %in% as.character(df$LayerLab)]
        if (length(keep)) df$LayerLab <- factor(as.character(df$LayerLab), levels = keep)
      }
      if (length(unique(df$Time)) > 1) {
        tdates <- dat$time_dates
        if (!is.null(tdates) && length(tdates) >= max(df$Time)) {
          df$Date <- tdates[df$Time]
        } else {
          df$Date <- days_as_date(df$Time, dat$origin)
        }
        p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Thickness, colour = LayerLab))
        p <- p + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)
        p <- p + ggplot2::labs(title = paste("Layer thickness through time — box", input$box),
                               x = "Year", y = "Thickness (m)")
        p <- p + theme_reactiveatlantis() + ggplot2::theme(legend.position = "none")
        ra_scale_x_year(p, df$Date)
      } else {
        p <- ggplot2::ggplot(df, ggplot2::aes(x = LayerLab, y = Thickness))
        p <- p + ggplot2::geom_col(fill = "#0B3C5D")
        p <- p + ggplot2::labs(title = paste("nominal_dz — box", input$box),
                               x = "Layer", y = "Thickness (m)")
        p + theme_reactiveatlantis()
      }
      })
    })

    keep_outputs_alive(output, "banner")
  })
}
