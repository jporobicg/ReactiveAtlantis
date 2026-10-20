## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Consumption from outputPROD.nc          ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

consumption_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("body")))
}

consumption_body_ui <- function(ns, grp) {
  codes <- as.character(grp$code[if ("isturnedon" %in% names(grp)) grp$isturnedon == 1 else TRUE])
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "open",
      selectInput(ns("view"), "View",
                  choices = c("What this group eats" = "predator",
                              "Who eats this group" = "prey")),
      selectInput(ns("fg"), "Functional group", choices = codes),
      numericInput(ns("top_n"), "Top N (rest = Other)", 8, min = 3, max = 20)
    ),
    uiOutput(ns("plot_host"))
  )
}

consumption_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {

    output$body <- renderUI({
      if (is.null(project) || !isTRUE(project$ready)) {
        return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS$consumption], "Consumption"))
      }
      miss <- tool_missing(project$paths, "consumption")
      if (length(miss)) {
        return(bslib::card(
          bslib::card_header("Consumption needs outputPROD.nc"),
          bslib::card_body(
            shiny::p("Load an Atlantis production NetCDF (usually named like outputPROD.nc) in the Run panel."),
            shiny::p("Eat_* and Grazing_* tracers are converted to tonnes using the same mg N to wet-weight factor as biomass, then split across prey with DietCheck proportions when that file is present.")
          )
        ))
      }
      consumption_body_ui(session$ns, project$data$grp)
    })

    cons_df <- reactive({
      dat <- project$data
      req(dat$nc_prod, dat$grp, input$fg, input$view)
      if (identical(input$view, "predator")) {
        tot <- consumed_tonnes(dat$nc_prod, dat$nc_out, dat$grp, input$fg)
        if (is.null(dat$diet_time)) {
          return(data.frame(Time = seq_along(tot$tonnes), series = "Total",
                            Tonnes = tot$tonnes, stringsAsFactors = FALSE))
        }
        long <- consumption_by_prey(tot$tonnes, dat$diet_time, input$fg)
        long <- bucket_top_n(long, "Prey", "Tonnes", input$top_n)
        agg <- stats::aggregate(Tonnes ~ Time + series, data = long, sum)
        return(agg)
      }
      long <- consumption_as_prey(dat$nc_prod, dat$nc_out, dat$grp, input$fg, dat$diet_time)
      if (nrow(long) == 0) {
        return(long)
      }
      long <- bucket_top_n(long, "Predator", "Tonnes", input$top_n)
      stats::aggregate(Tonnes ~ Time + series, data = long, sum)
    })

    output$plot_host <- renderUI({
      df <- cons_df()
      if (is.null(df) || nrow(df) == 0) {
        return(div(class = "alert alert-warning",
                   "No Eat/Grazing tracer or diet proportions for this group."))
      }
      ns <- session$ns
      if (requireNamespace("plotly", quietly = TRUE)) {
        plotly::plotlyOutput(ns("cons_plot"), height = "560px")
      } else {
        plotOutput(ns("cons_plot_gg"), height = "560px")
      }
    })

    make_plot <- function() {
      df <- cons_df()
      req(nrow(df) > 0)
      origin <- project$data$origin
      df$Date <- days_as_date(df$Time, origin)
      pal <- get_fg_colors(setdiff(unique(df$series), "Other"), project$data$fg_palette)
      pal <- c(pal, Other = "#8D99AE", Total = "#0B3C5D")
      title <- if (identical(input$view, "predator")) {
        paste("Tonnes consumed by", input$fg)
      } else {
        paste("Tonnes of", input$fg, "consumed by predators")
      }
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Tonnes, fill = series))
      p <- p + ggplot2::geom_area(position = "stack", alpha = 0.92)
      p <- p + ggplot2::scale_fill_manual(values = pal, name = NULL)
      p <- p + ggplot2::labs(title = title, x = "Year", y = "Consumption (tons)")
      p + theme_reactiveatlantis()
    }

    output$cons_plot_gg <- renderPlot({ make_plot() })
    if (requireNamespace("plotly", quietly = TRUE)) {
      output$cons_plot <- plotly::renderPlotly({
        ra_ggplotly(make_plot(), n_series = 12)
      })
    }
  })
}
