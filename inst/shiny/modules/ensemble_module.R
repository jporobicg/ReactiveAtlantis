## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Multi-run overlay / ensemble       ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ensemble_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    uiOutput(ns("empty_state")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("kind"), "Series",
                    choices = c("Biomass (BiomIndx)" = "biomass",
                                "SSB" = "ssb",
                                "YOY" = "yoy",
                                "Catch (CATCH.nc)" = "catch")),
        selectInput(ns("fg"), "Functional group", choices = character()),
        p(class = "ra-muted",
          "Add extra run folders in the Run panel (one path per line) to overlay them here.")
      ),
      plotOutput(ns("ens_plot"), height = "560px")
    )
  )
}

ensemble_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {

    output$banner <- renderUI(tool_banner(project, "ensemble"))

    output$empty_state <- renderUI({
      if (!isTRUE(project$ready)) return(NULL)
      runs <- project$ensemble
      if (is.null(runs) || length(runs) < 2) {
        return(bslib::card(
          bslib::card_header("Only one run is loaded"),
          bslib::card_body(
            shiny::p("Paste extra Atlantis output folders (one per line) in the Run panel, then click Load run."),
            shiny::p("Each folder is scanned for BiomIndx, SSB and YOY. The current run is always included. With more than two runs the plot adds the ensemble mean and range.")
          )
        ))
      }
      NULL
    })

    observe({
      req(isTRUE(project$ready), project$data$grp)
      update_select_keep(session, "fg", fg_select_choices(project$data$grp, on_only = FALSE),
                         input$fg)
    })

    output$ens_plot <- renderPlot({
      plot_or_message("Load a run, then paste extra folders to overlay them", {
      if (!isTRUE(project$ready)) {
        plot_message("Load a run to overlay BiomIndx, SSB or YOY")
        return(invisible(NULL))
      }
      runs <- project$ensemble
      if (is.null(runs) || length(runs) < 2) {
        plot_message("Paste extra run folders in the Run panel to overlay more than one simulation")
        return(invisible(NULL))
      }
      req(input$fg, input$kind)
      df <- ensemble_table(runs, input$fg, input$kind)
      if (is.null(df) || nrow(df) == 0) {
        plot_message("No series for this group in the loaded runs")
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

    keep_outputs_alive(output, c("banner", "empty_state", "ens_plot"))
  })
}
