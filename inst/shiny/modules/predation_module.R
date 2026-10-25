## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Predation Analysis Module       ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

predation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("fg"), "Functional group", choices = character()),
        selectInput(ns("stock"), "Stock", choices = 0),
        numericInput(ns("threshold"), "Diet threshold", 0.001, min = 0, max = 1, step = 0.001),
        numericInput(ns("top_n"), "Top N prey (rest = Other)", 8, min = 3, max = 20, step = 1),
        checkboxInput(ns("scaled"), "Scale to 1", TRUE),
        sliderInput(ns("time_age"), "Age-diet time (days)", min = 0, max = 1, value = 0, step = 1)
      ),
      plotOutput(ns("plot_pred_gg"), height = "360px"),
      plotOutput(ns("plot_prey_gg"), height = "360px"),
      plotOutput(ns("plot_age"), height = "280px")
    )
  )
}

predation_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))
    diet_dat <- shiny::reactive({
      req(ps()$data)
      ensure_diet(ps()$data, ps()$paths)
    })

    output$banner <- renderUI(tool_banner(ps(), "predation"))

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      update_select_keep(session, "fg", fg_select_choices(ps()$data$grp, on_only = FALSE),
                         input$fg)
    })

    fg_d <- ra_debounce(shiny::reactive(input$fg), 250)

    new_bio <- reactive({
      dat <- diet_dat()
      req(dat$biomindx, dat$diet_time)
      biom <- as.data.frame(dat$biomindx)
      preds <- unique(as.character(dat$diet_time$Predator))
      keep <- c("Time", intersect(names(biom), preds))
      sub <- biom[, keep, drop = FALSE]
      nb <- reshape2::melt(sub, id.vars = "Time")
      names(nb) <- c("Time", "Predator", "Biomass")
      nb$Predator <- as.character(nb$Predator)
      data.table::setDT(nb)
      data.table::setkey(nb, Predator, Time)
      nb
    })

    predator_df <- reactive({
      dat <- diet_dat()
      req(dat$diet_time, fg_d())
      if (!is.null(dat$diet_full)) {
        times <- sort(unique(dat$diet_full$Time))
        step <- if (length(times) > 1) min(diff(times)) else 1
        shiny::updateSliderInput(session, "time_age",
                                 min = min(times), max = max(times), step = step)
      }
      stocks <- if (!is.null(dat$diet_time) && "Stock" %in% names(dat$diet_time)) {
        unique(dat$diet_time$Stock)
      } else 0
      update_select_keep(session, "stock", as.character(stocks), input$stock)
      diet_through_time_predator(dat$diet_time, new_bio(), fg_d(), input$stock,
                                 input$threshold, FALSE)
    })

    prey_df <- reactive({
      dat <- diet_dat()
      req(dat$diet_time, fg_d())
      diet_through_time_prey(dat$diet_time, new_bio(), fg_d(), input$threshold, FALSE)
    })

    to_year <- function(df) {
      origin <- ra_run_origin(ps)
      df$Time <- days_as_date(df$Time, origin)
      df
    }

    pred_plot <- reactive({
      df <- to_year(predator_df())
      req(nrow(df) > 0)
      df <- bucket_top_n(df, "variable", "consum", input$top_n)
      agg <- stats::aggregate(consum ~ Time + series, data = df, sum)
      pal <- get_fg_colors(setdiff(unique(agg$series), "Other"), ps()$data$fg_palette)
      pal <- c(pal, Other = "#8D99AE")
      agg$series <- factor(agg$series, levels = spread_series_levels(agg$series, pal))
      ylab <- if (isTRUE(input$scaled)) "Proportion of diet" else "Consumption (tons)"
      p <- ggplot2::ggplot(agg, ggplot2::aes(x = Time, y = consum, fill = series))
      p <- p + ggplot2::geom_area(position = if (isTRUE(input$scaled)) "fill" else "stack")
      p <- p + ggplot2::scale_fill_manual(values = pal, name = "Prey")
      p <- p + ggplot2::labs(title = paste("Diet of", fg_d()), x = "Year", y = ylab)
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, df$Time)
    })

    prey_plot <- reactive({
      df <- to_year(prey_df())
      req(nrow(df) > 0)
      df <- bucket_top_n(df, "Predator", "eff.pred", input$top_n)
      agg <- stats::aggregate(eff.pred ~ Time + series, data = df, sum)
      pal <- get_fg_colors(setdiff(unique(agg$series), "Other"), ps()$data$fg_palette)
      pal <- c(pal, Other = "#8D99AE")
      agg$series <- factor(agg$series, levels = spread_series_levels(agg$series, pal))
      ylab <- if (isTRUE(input$scaled)) "Proportion of predation" else "Predation pressure (tons)"
      p <- ggplot2::ggplot(agg, ggplot2::aes(x = Time, y = eff.pred, fill = series))
      p <- p + ggplot2::geom_area(position = if (isTRUE(input$scaled)) "fill" else "stack")
      p <- p + ggplot2::scale_fill_manual(values = pal, name = "Predator")
      p <- p + ggplot2::labs(title = paste("Predators of", fg_d()), x = "Year", y = ylab)
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, df$Time)
    })

    output$plot_pred_gg <- renderPlot({
      df <- tryCatch(predator_df(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        plot_message("This functional group has no prey diet recorded.")
        return(invisible(NULL))
      }
      pred_plot()
    })
    output$plot_prey_gg <- renderPlot({
      df <- tryCatch(prey_df(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) {
        plot_message("This functional group has no predators.")
        return(invisible(NULL))
      }
      prey_plot()
    })

    output$plot_age <- renderPlot({
      plot_or_message("Load DietCheck.txt to plot diet by age", {
      dat <- diet_dat()
      req(dat$diet_full, fg_d(), input$time_age)
      df <- age_diet_slice(dat$diet_full, fg_d(), input$stock, input$time_age, input$threshold)
      req(nrow(df) > 0)
      pal <- get_fg_colors(unique(as.character(df$variable)), dat$fg_palette)
      df$Age <- sprintf("Age%02d", as.integer(df$Cohort))
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Age, y = value, fill = variable))
      p <- p + ggplot2::geom_col(position = "stack")
      p <- p + ggplot2::scale_fill_manual(values = pal, name = "Prey")
      p <- p + ggplot2::labs(title = "Diet by age at the selected time",
                             x = "Age class", y = "Diet proportion")
      p + theme_reactiveatlantis()
      })
    })

    keep_outputs_alive(output, "banner")
  })
}

bucket_top_n <- function(df, key, value, n = 8) {
  if (nrow(df) == 0) {
    df$series <- character()
    return(df)
  }
  n <- max(1, as.integer(n))
  totals <- tapply(df[[value]], df[[key]], sum, na.rm = TRUE)
  keep <- names(sort(totals, decreasing = TRUE))[seq_len(min(n, length(totals)))]
  df$series <- ifelse(as.character(df[[key]]) %in% keep, as.character(df[[key]]), "Other")
  df
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~         DietCheck pre-aggregation          ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

preaggregate_diet <- function(diet_raw) {
  dt <- data.table::as.data.table(diet_raw)
  if ("Updated" %in% names(dt)) dt[, Updated := NULL]
  if ("Group" %in% names(dt) && !"Predator" %in% names(dt)) {
    data.table::setnames(dt, "Group", "Predator")
  }
  dt[, Predator := as.character(Predator)]
  meta <- intersect(c("Time", "Predator", "Stock"), names(dt))
  extra <- intersect("Cohort", names(dt))
  prey_cols <- setdiff(names(dt), c(meta, extra))
  for (col in prey_cols) {
    data.table::set(dt, j = col, value = as.numeric(dt[[col]]))
  }
  diet_full <- data.table::copy(dt)
  data.table::setkeyv(diet_full, intersect(c("Predator", "Stock", "Time"), names(diet_full)))
  
  if (length(extra) == 1) {
    ## DietCheck proportions sum to 1 within each cohort. Average over the
    ## cohorts that fed so a predator's shares sum to 1 (summing gave
    ## n_cohorts; validation D15). This is an unweighted cohort mean; the
    ## Consumption tab weights each cohort by its own intake instead.
    fed <- rowSums(as.matrix(dt[, prey_cols, with = FALSE]), na.rm = TRUE) > 0
    dt <- dt[fed]
    diet_time <- dt[, lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE)),
                    by = meta, .SDcols = prey_cols]
  } else {
    diet_time <- dt[, c(meta, prey_cols), with = FALSE]
  }
  data.table::setkeyv(diet_time, intersect(c("Predator", "Stock"), names(diet_time)))
  list(diet_full = diet_full, diet_time = diet_time)
}

diet_through_time_predator <- function(diet_time, new.bio, fg, stock, threshold, melt_time = FALSE) {
  empty <- data.frame(Time = numeric(), variable = character(), value = numeric(),
                      Predator = character(), Biomass = numeric(), consum = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(diet_time) || nrow(diet_time) == 0) return(empty)
  if ("Stock" %in% names(diet_time) && !is.null(stock) && nzchar(as.character(stock))) {
    predator <- diet_time[Predator == fg & Stock == as.numeric(stock)]
  } else {
    predator <- diet_time[Predator == fg]
  }
  if (nrow(predator) == 0) return(empty)
  prey_cols <- setdiff(names(predator), c("Time", "Predator", "Stock", "Cohort"))
  if (length(prey_cols) == 0) return(empty)
  keep <- prey_cols[vapply(predator[, prey_cols, with = FALSE], function(x) {
    sum(x, na.rm = TRUE) > threshold
  }, logical(1))]
  if (length(keep) == 0) return(empty)
  long <- data.table::melt(
    predator[, c("Time", keep), with = FALSE],
    id.vars = "Time", variable.name = "variable", value.name = "value",
    variable.factor = FALSE
  )
  long <- long[value > threshold]
  if (nrow(long) == 0) return(empty)
  bio <- new.bio[Predator == fg, .(Time, Biomass)]
  long <- bio[long, on = "Time"]
  long[, consum := value * Biomass]
  if (isTRUE(melt_time)) {
    lag <- diff(sort(unique(long$Time)))[1]
    if (is.finite(lag) && lag != 0) long[, Time := Time / lag]
  }
  as.data.frame(long)
}

diet_through_time_prey <- function(diet_time, new.bio, fg, threshold, melt_time = FALSE) {
  empty <- data.frame(Time = numeric(), Predator = character(), diet_frac = numeric(),
                      Biomass = numeric(), eff.pred = numeric(), stringsAsFactors = FALSE)
  if (is.null(diet_time) || !fg %in% names(diet_time)) return(empty)
  prey <- diet_time[, c("Time", "Predator", fg), with = FALSE]
  data.table::setnames(prey, fg, "diet_frac")
  prey <- prey[diet_frac > 0]
  if (nrow(prey) == 0) return(empty)
  prey <- new.bio[, .(Time, Predator, Biomass)][prey, on = c("Predator", "Time")]
  prey[, eff.pred := diet_frac * Biomass]
  prey <- prey[is.finite(eff.pred)]
  if (nrow(prey) == 0) return(empty)
  trh.max <- max(prey$eff.pred, na.rm = TRUE) * threshold
  if (isTRUE(melt_time)) {
    lag <- diff(sort(unique(prey$Time)))[1]
    if (is.finite(lag) && lag != 0) prey[, Time := Time / lag]
  }
  prey <- prey[eff.pred > trh.max]
  as.data.frame(prey)
}

age_diet_slice <- function(diet_full, fg, stock, time_age, threshold) {
  empty <- data.frame(Cohort = integer(), variable = character(), value = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(diet_full) || nrow(diet_full) == 0) return(empty)
  if ("Stock" %in% names(diet_full)) {
    age.diet <- diet_full[Predator == fg & Stock == as.numeric(stock) & Time == as.numeric(time_age)]
  } else {
    age.diet <- diet_full[Predator == fg & Time == as.numeric(time_age)]
  }
  if (nrow(age.diet) == 0) return(empty)
  drop_cols <- intersect(c("Predator", "Time", "Stock"), names(age.diet))
  age.diet <- age.diet[, !drop_cols, with = FALSE]
  keep <- names(age.diet)[vapply(age.diet, function(x) {
    if (is.numeric(x)) sum(x, na.rm = TRUE) > threshold else TRUE
  }, logical(1))]
  age.diet <- age.diet[, keep, with = FALSE]
  if (!"Cohort" %in% names(age.diet)) return(empty)
  long <- data.table::melt(age.diet, id.vars = "Cohort", variable.factor = FALSE)
  long <- long[value > threshold]
  as.data.frame(long)
}
