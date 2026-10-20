## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Consumption from outputPROD.nc          ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

consumption_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("view"), "View",
                    choices = c("What this group eats" = "predator",
                                "Who eats this group" = "prey")),
        selectInput(ns("fg"), "Functional group", choices = character()),
        numericInput(ns("top_n"), "Top N (rest = Other)", 8, min = 3, max = 20)
      ),
      plotOutput(ns("cons_plot_gg"), height = "560px")
    )
  )
}

consumption_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))

    output$banner <- renderUI(tool_banner(ps(), "consumption"))

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      update_select_keep(session, "fg", fg_select_choices(ps()$data$grp), input$fg)
    })

    cons_df <- reactive({
      dat <- ps()$data
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

    make_plot <- function() {
      df <- cons_df()
      if (is.null(df) || nrow(df) == 0) {
        plot_message("No Eat/Grazing tracer, or no diet proportions, for this group.")
        return(invisible(NULL))
      }
      origin <- ps()$data$origin
      df$Date <- days_as_date(df$Time, origin)
      pal <- get_fg_colors(setdiff(unique(df$series), "Other"), ps()$data$fg_palette)
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

    output$cons_plot_gg <- renderPlot({
      plot_or_message("Load outputPROD.nc to plot consumption", make_plot())
    })

    keep_outputs_alive(output, c("banner", "cons_plot_gg"))
  })
}
