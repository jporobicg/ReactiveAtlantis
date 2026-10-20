## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Mortality Analysis Module         ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

mortality_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("body")))
}

mortality_body_ui <- function(ns, grp) {
  labels <- if ("longname" %in% names(grp)) {
    setNames(as.character(grp$code), paste(grp$code, grp$longname, sep = " — "))
  } else {
    as.character(grp$code)
  }
  layout_sidebar(
    sidebar = sidebar(
      width = 280, open = "open",
      selectInput(ns("fg"), "Functional group", choices = labels)
    ),
    uiOutput(ns("plot_host"))
  )
}

mortality_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {

    output$body <- renderUI({
      if (is.null(project) || !isTRUE(project$ready)) {
        return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS$mortality], "Mortality"))
      }
      miss <- tool_missing(project$paths, "mortality")
      if (length(miss)) return(tool_missing_card(miss, "Mortality"))
      mortality_body_ui(session$ns, project$data$grp)
    })

    mort_data <- reactive({
      dat <- project$data
      req(dat$spe_mort, dat$pred_mort, dat$grp, input$fg)
      FG <- input$fg
      if (FG %in% dat$grp$longname) {
        FG <- dat$grp$code[dat$grp$longname == FG][1]
      }
      calc_mort(dat$spe_mort, dat$pred_mort, FG)
    })

    output$plot_host <- renderUI({
      ns <- session$ns
      if (requireNamespace("plotly", quietly = TRUE)) {
        plotly::plotlyOutput(ns("mortality_plot"), height = "720px")
      } else {
        plotOutput(ns("mortality_plot_gg"), height = "720px")
      }
    })

    make_plot <- function() {
      mort <- mort_data()
      req(!is.null(mort), nrow(mort) > 0)
      origin <- project$data$origin
      mort$Time <- days_as_date(mort$Time, origin)
      long <- reshape2::melt(mort, id.var = c("Time", "Mtype"))
      names(long)[names(long) == "variable"] <- "Age"
      long$Age <- as.character(long$Age)
      type_pal <- mortality_type_colors()
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Time, y = value, fill = Mtype))
      p <- p + ggplot2::geom_area(position = "stack", alpha = 0.92)
      p <- p + ggplot2::facet_wrap(~ Age, ncol = 2, scales = "free_y")
      p <- p + ggplot2::scale_fill_manual(values = type_pal, name = "Mortality")
      p <- p + ggplot2::labs(
        title = paste("Mortality by age —", input$fg),
        x = "Year", y = "Mortality"
      )
      p + theme_reactiveatlantis()
    }

    output$mortality_plot_gg <- renderPlot({ make_plot() })
    if (requireNamespace("plotly", quietly = TRUE)) {
      output$mortality_plot <- plotly::renderPlotly({
        n_age <- length(unique(reshape2::melt(mort_data(), id.var = c("Time", "Mtype"))$variable))
        ra_ggplotly(make_plot(), n_series = n_age)
      })
    }
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Helper Functions                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

calc_mort <- function(SpeMort, PredMort, FG){
  F.mort <- paste(as.character(FG), c(0:9), 'S0', 'F', sep = '.')
  N.mort <- paste(as.character(FG), c(0:9), 'S0', 'M1', sep = '.')
  P.mort <- paste(as.character(FG), c(0:9), 'S0', 'M2', sep = '.')
  
  Fmort <- SpeMort[, c(1, which(colnames(SpeMort) %in% F.mort))]
  colnames(Fmort) <- c('Time', format_age_labels(ncol(Fmort) - 1))
  
  Nmort <- SpeMort[, c(1, which(colnames(SpeMort) %in% N.mort))]
  colnames(Nmort) <- c('Time', format_age_labels(ncol(Nmort) - 1))
  
  Pmort <- SpeMort[, c(1, which(colnames(SpeMort) %in% P.mort))]
  colnames(Pmort) <- c('Time', format_age_labels(ncol(Pmort) - 1))
  
  Fmort$Mtype <- 'Fishing Mortality'
  Nmort$Mtype <- 'Natural Mortality'
  Pmort$Mtype <- 'Predation Mortality'
  
  return(rbind(Fmort, Nmort, Pmort))
}
