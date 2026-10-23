## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Age-class weights, numbers, biomass     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ages_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("fg"), "Functional group", choices = character()),
        selectInput(ns("variable"), "Variable",
                    choices = c("All four" = "all",
                                "Biomass" = "Biomass",
                                "Reserve N" = "ResN",
                                "Structural N" = "StructN",
                                "Numbers" = "Numbers"),
                    selected = "all"),
        p(class = "ra-muted",
          "Whole-system totals: numbers summed over boxes and layers; ResN and StructN are numbers-weighted means.")
      ),
      plotOutput(ns("age_plot"), height = "640px")
    )
  )
}

ages_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))

    output$banner <- renderUI(tool_banner(ps(), "ages"))

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      update_select_keep(session, "fg", age_select_choices(ps()$data$grp), input$fg)
    })

    fg_d <- ra_debounce(shiny::reactive(input$fg), 250)

    age_df <- reactive({
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out, dat$grp, fg_d())
      info <- atlantis_layer_info(dat$nc_out, dat$inf_box)
      age_class_long(dat$nc_out, dat$grp, fg_d(), box = NULL,
                     layer_key = "integrated", info = info)
    })

    output$age_plot <- renderPlot({
      plot_or_message("Load output.nc to plot age-class ResN, StructN, numbers and biomass", {
      df <- age_df()
      req(nrow(df) > 0)
      dat <- ensure_project_data(ps(), "nc_out")
      vars <- if (identical(input$variable, "all") || is.null(input$variable)) {
        AGE_CLASS_VARS
      } else {
        input$variable
      }
      df <- df[df$Variable %in% vars, , drop = FALSE]
      req(nrow(df) > 0)
      tdates <- dat$time_dates
      if (!is.null(tdates) && length(tdates) >= max(df$Time)) {
        df$Date <- tdates[df$Time]
      } else {
        df$Date <- days_as_date(df$Time, dat$origin)
      }
      pal <- age_class_palette(length(unique(df$Age)), sort(unique(df$AgeLab)))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Value, colour = AgeLab))
      p <- p + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)
      if (length(unique(df$Variable)) > 1) {
        p <- p + ggplot2::facet_wrap(~ Variable, ncol = 1, scales = "free_y")
        ylab <- NULL
      } else {
        ylab <- age_class_ylab(df$Variable[1])
      }
      p <- p + ggplot2::scale_colour_manual(values = pal, name = "Age class")
      p <- p + ggplot2::labs(
        title = paste("Age classes —", fg_d()),
        x = "Year", y = ylab
      )
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, df$Date)
      })
    })

    keep_outputs_alive(output, "banner")
  })
}
