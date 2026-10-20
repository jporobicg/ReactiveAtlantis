## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Multi-run overlay / ensemble       ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ensemble_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("body")))
}

ensemble_body_ui <- function(ns, grp, n_runs) {
  codes <- as.character(grp$code)
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "open",
      selectInput(ns("kind"), "Series",
                  choices = c("Biomass (BiomIndx)" = "biomass",
                              "SSB" = "ssb",
                              "YOY" = "yoy",
                              "Catch (CATCH.nc)" = "catch")),
      selectInput(ns("fg"), "Functional group", choices = codes),
      p(class = "ra-muted",
        if (n_runs > 2) {
          "More than two runs: individual lines plus the ensemble mean and range."
        } else {
          "Add extra run folders in the Run panel (one path per line) to overlay them here."
        })
    ),
    plotOutput(ns("ens_plot"), height = "560px")
  )
}

ensemble_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {

    output$body <- renderUI({
      if (is.null(project) || !isTRUE(project$ready)) {
        return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS$overview], "Multi-run"))
      }
      runs <- project$ensemble
      if (is.null(runs) || length(runs) == 0) {
        return(bslib::card(
          bslib::card_header("Add other run folders"),
          bslib::card_body(
            shiny::p("Paste extra Atlantis output folders (one per line) in the Run panel, then click Load run."),
            shiny::p("Each folder is scanned for BiomIndx, SSB and YOY. The current run is always included.")
          )
        ))
      }
      ensemble_body_ui(session$ns, project$data$grp, length(runs))
    })

    output$ens_plot <- renderPlot({
      runs <- project$ensemble
      req(length(runs) > 0, input$fg, input$kind)
      df <- ensemble_table(runs, input$fg, input$kind)
      if (is.null(df) || nrow(df) == 0) {
        plot.new()
        title("No series for this group in the loaded runs")
        return(invisible(NULL))
      }
      origin <- project$data$origin
      df$Date <- days_as_date(df$Time, origin)
      n_runs <- length(unique(df$Run))
      pal <- cycle_pool(n_runs, c("#0B3C5D", "#1D7874", "#C4A35A", "#A33B20",
                                  "#328CC1", "#5C6B73", "#4C9F70", "#C46D5E"))
      names(pal) <- unique(as.character(df$Run))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Value, colour = Run))
      if (n_runs > 2) {
        st <- ensemble_stats(df)
        st$Date <- days_as_date(st$Time, origin)
        p <- p + ggplot2::geom_ribbon(
          data = st, ggplot2::aes(x = Date, ymin = Lo, ymax = Hi),
          inherit.aes = FALSE, fill = "#1D7874", alpha = 0.18
        )
        p <- p + ggplot2::geom_line(
          data = st, ggplot2::aes(x = Date, y = Mean),
          inherit.aes = FALSE, colour = "#0B3C5D", linewidth = 1.15
        )
      }
      p <- p + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)
      p <- p + ggplot2::scale_color_manual(values = pal)
      ylab <- switch(input$kind, biomass = "Biomass", ssb = "SSB", yoy = "YOY",
                     catch = "Catch", "Value")
      p <- p + ggplot2::labs(
        title = paste(ylab, "—", input$fg),
        x = "Year", y = ylab
      )
      p + theme_reactiveatlantis()
    })
  })
}
