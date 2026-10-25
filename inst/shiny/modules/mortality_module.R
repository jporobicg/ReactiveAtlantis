## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Mortality Analysis Module         ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

mortality_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    uiOutput(ns("file_status")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(
          ns("view"), "View",
          choices = c(
            "By source (F, M1, M2)" = "source",
            "Predation by predator" = "predator",
            "Mort.txt F vs M" = "mort"
          ),
          selected = "source"
        ),
        selectInput(ns("fg"), "Functional group", choices = character()),
        conditionalPanel(
          "input.view == 'predator'", ns = ns,
          selectInput(ns("age"), "Age class", choices = c("All ages" = "all")),
          p(class = "ra-muted",
            "SpecificPredMort.txt: predation mortality on this group, stacked by predator.")
        ),
        conditionalPanel(
          "input.view == 'mort'", ns = ns,
          p(class = "ra-muted",
            "Mort.txt reports relative fishing (F) versus other mortality (M), not instantaneous rates.")
        )
      ),
      plotOutput(ns("mortality_plot_gg"), height = "720px")
    )
  )
}

mortality_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))

    output$banner <- renderUI(tool_banner(ps(), "mortality"))

    file_status <- reactive({
      mortality_file_status(ps()$paths, ps()$data)
    })

    output$file_status <- renderUI({
      st <- tryCatch(file_status(), error = function(e) NULL)
      if (is.null(st) || !isTRUE(ps()$ready)) return(NULL)
      items <- lapply(seq_len(nrow(st)), function(i) {
        cls <- if (isTRUE(st$present[i])) "ra-map-status is-ok" else "ra-map-status is-bad"
        shiny::tags$li(
          class = cls,
          shiny::tags$strong(st$label[i]), ": ", st$note[i]
        )
      })
      shiny::div(class = "ra-map-status", shiny::tags$ul(items))
    })

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      update_select_keep(session, "fg", fg_select_choices(ps()$data$grp, on_only = FALSE),
                         input$fg)
    })

    fg_d <- ra_debounce(shiny::reactive(input$fg), 250)
    view_d <- ra_debounce(shiny::reactive(input$view), 250)

    fg_code <- reactive({
      dat <- ps()$data
      req(dat$grp, fg_d())
      FG <- fg_d()
      if (FG %in% dat$grp$longname) {
        FG <- dat$grp$code[dat$grp$longname == FG][1]
      }
      as.character(FG)[1]
    })

    pred_long <- reactive({
      dat <- ps()$data
      req(fg_code())
      parse_pred_mort_by_predator(dat$pred_mort, fg_code())
    })

    observe({
      long <- tryCatch(pred_long(), error = function(e) NULL)
      ages <- if (!is.null(long) && nrow(long) > 0) {
        unique(as.character(long$Age))
      } else {
        character()
      }
      ch <- c("All ages" = "all")
      if (length(ages)) ch <- c(ch, stats::setNames(ages, ages))
      update_select_keep(session, "age", ch, input$age)
    })

    output$mortality_plot_gg <- renderPlot({
      plot_or_message("Load SpecificMort.txt to plot mortality", {
        if (!isTRUE(ps()$ready)) {
          plot_message("Load SpecificMort.txt to plot mortality")
          return(invisible(NULL))
        }
        make_plot()
      })
    })

    make_plot <- function() {
      view <- if (is.null(view_d())) "source" else view_d()
      st <- file_status()
      miss <- mortality_missing_message(st, view)
      if (!is.null(miss) && identical(view, "source") &&
          !isTRUE(st$present[st$role == "specific_mort"])) {
        plot_message(miss)
        return(invisible(NULL))
      }
      if (identical(view, "predator")) return(plot_by_predator())
      if (identical(view, "mort")) return(plot_mort_txt())
      plot_by_source()
    }

    plot_by_source <- function() {
      dat <- ps()$data
      if (is.null(dat$spe_mort)) {
        plot_message(mortality_missing_message(file_status(), "source"))
        return(invisible(NULL))
      }
      mort <- calc_mort(dat$spe_mort, dat$pred_mort, fg_code())
      if (is.null(mort) || nrow(mort) == 0) {
        plot_message("No mortality columns for this group in SpecificMort.txt")
        return(invisible(NULL))
      }
      origin <- ra_run_origin(ps)
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
        title = paste("Mortality by age —", fg_d()),
        x = "Year", y = "Mortality"
      )
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, mort$Time)
    }

    plot_by_predator <- function() {
      long <- pred_long()
      if (is.null(long) || nrow(long) == 0) {
        plot_message(attr(long, "reason") %||%
                       mortality_missing_message(file_status(), "predator"))
        return(invisible(NULL))
      }
      age <- if (is.null(input$age)) "all" else as.character(input$age)[1]
      if (!identical(age, "all")) long <- long[long$Age == age, , drop = FALSE]
      if (nrow(long) == 0) {
        plot_message("No predation mortality for this age class.")
        return(invisible(NULL))
      }
      ## Keep predators that kill this group at some time; the file has a
      ## column for every group, mostly zero.
      tot <- tapply(long$Value, long$Predator, sum, na.rm = TRUE)
      long <- long[long$Predator %in% names(tot)[is.finite(tot) & tot > 0], , drop = FALSE]
      if (nrow(long) == 0) {
        plot_message("No predator killed this group in SpecificPredMort.txt.")
        return(invisible(NULL))
      }
      origin <- ra_run_origin(ps)
      long$Date <- days_as_date(long$Time, origin)
      pal <- get_fg_colors(unique(long$Predator), ps()$data$fg_palette)
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Date, y = Value, fill = Predator))
      p <- p + ggplot2::geom_area(position = "stack", alpha = 0.92)
      if (identical(age, "all") && length(unique(long$Age)) > 1) {
        p <- p + ggplot2::facet_wrap(~ Age, ncol = 2, scales = "free_y")
      }
      p <- p + ggplot2::scale_fill_manual(values = pal, name = "Predator")
      p <- p + ggplot2::labs(
        title = paste("Predation mortality by predator —", fg_d()),
        x = "Year", y = "Predation mortality"
      )
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, long$Date)
    }

    plot_mort_txt <- function() {
      dat <- ps()$data
      long <- parse_mort_txt(dat$mort, fg_code())
      if (is.null(long) || nrow(long) == 0) {
        plot_message(attr(long, "reason") %||%
                       mortality_missing_message(file_status(), "mort"))
        return(invisible(NULL))
      }
      origin <- ra_run_origin(ps)
      long$Date <- days_as_date(long$Time, origin)
      pal <- mortality_type_colors()
      p <- ggplot2::ggplot(long, ggplot2::aes(x = Date, y = Value, colour = Source))
      p <- p + ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE)
      p <- p + ggplot2::geom_point(size = 1.6, na.rm = TRUE)
      p <- p + ggplot2::scale_colour_manual(values = pal)
      p <- p + ggplot2::labs(
        title = paste("Mort.txt relative F vs M —", fg_d()),
        subtitle = "Relative only; Atlantis does not treat these as instantaneous rates.",
        x = "Year", y = "Relative mortality"
      )
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, long$Date)
    }

    keep_outputs_alive(output, "banner")
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
