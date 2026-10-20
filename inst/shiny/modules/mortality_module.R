## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Mortality Analysis Module         ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

mortality_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("fg"), "Functional group", choices = character())
      ),
      plotOutput(ns("mortality_plot_gg"), height = "720px")
    )
  )
}

mortality_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {

    output$banner <- renderUI(tool_banner(project, "mortality"))

    observe({
      req(isTRUE(project$ready), project$data$grp)
      update_select_keep(session, "fg", fg_select_choices(project$data$grp, on_only = FALSE),
                         input$fg)
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

    output$mortality_plot_gg <- renderPlot({
      plot_or_message("Load SpecificMort.txt to plot mortality", {
        if (!isTRUE(project$ready)) {
          plot_message("Load SpecificMort.txt to plot mortality")
          return(invisible(NULL))
        }
        make_plot()
      })
    })

    make_plot <- function() {
      mort <- mort_data()
      if (is.null(mort) || nrow(mort) == 0) {
        plot_message("No mortality columns for this group in SpecificMort.txt")
        return(invisible(NULL))
      }
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

    keep_outputs_alive(output, c("banner", "mortality_plot_gg"))
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Helper Functions                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

calc_mort <- function(SpeMort, PredMort, FG){
  if (is.null(SpeMort) || ncol(SpeMort) < 2) {
    return(data.frame(Time = numeric(), Mtype = character(), stringsAsFactors = FALSE))
  }
  nms <- gsub("-", ".", as.character(colnames(SpeMort)))
  colnames(SpeMort) <- nms
  FG <- as.character(FG)[1]
  ages <- 0:20
  pick <- function(kind) {
    want <- paste(FG, ages, "S0", kind, sep = ".")
    have <- want[want %in% colnames(SpeMort)]
    time_col <- if ("Time" %in% colnames(SpeMort)) "Time" else colnames(SpeMort)[1]
    out <- SpeMort[, c(time_col, have), drop = FALSE]
    colnames(out) <- c("Time", format_age_labels(max(0, ncol(out) - 1)))
    out
  }
  Fmort <- pick("F")
  Nmort <- pick("M1")
  Pmort <- pick("M2")
  Fmort$Mtype <- "Fishing Mortality"
  Nmort$Mtype <- "Natural Mortality"
  Pmort$Mtype <- "Predation Mortality"
  rbind(Fmort, Nmort, Pmort)
}
