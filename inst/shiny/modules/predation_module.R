## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Predation Analysis Module       ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

predation_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Predation Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(ns("biom_file"), "Biomass File (BiomIndx.txt)", accept = ".txt"),
        fileInput(ns("grp_csv"), "Groups CSV", accept = ".csv"),
        fileInput(ns("diet_file"), "Diet File (DietCheck.txt)", accept = ".txt"),
        fileInput(ns("age_biomass"), "Age Biomass [Optional]", accept = ".txt"),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          h5("Analysis Options"),
          selectInput(ns("analysis_type"), "Analysis Type",
                     choices = c("Biomass Overview" = "biomass",
                               "Predation Through Time" = "predation",
                               "Predation by Age" = "age")),
          conditionalPanel(
            condition = "input.analysis_type == 'predation'",
            ns = ns,
            selectInput(ns("fg_pred"), "Functional Group:", choices = NULL),
            selectInput(ns("stock_pred"), "Stock:", choices = NULL),
            numericInput(ns("threshold_pred"), "Threshold:", value = 0.001, 
                        min = 0, max = 1, step = 0.001),
            checkboxInput(ns("scaled_pred"), "Scaled to 1", TRUE),
            checkboxInput(ns("melt_time"), "Melt Time Step", FALSE)
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'age'",
            ns = ns,
            selectInput(ns("fg_age"), "Functional Group:", choices = NULL),
            sliderInput(ns("time_age"), "Simulation Time:", min = 0, max = 100, value = 0, step = 1),
            selectInput(ns("stock_age"), "Stock:", choices = NULL),
            numericInput(ns("threshold_age"), "Threshold:", value = 0.001, 
                        min = 0, max = 1, step = 0.001)
          )
        )
      ),
      
      mainPanel(
        width = 9,
        conditionalPanel(
          condition = "!output.data_loaded",
          ns = ns,
          div(
            class = "alert alert-info",
            style = "margin-top: 50px;",
            icon("info-circle"),
            " Please load your predation data files to begin analysis."
          )
        ),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          uiOutput(ns("analysis_output"))
        )
      )
    )
  )
}

predation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      data_loaded = FALSE,
      biom.tot = NULL,
      rel.bio = NULL,
      diet.data = NULL,
      diet_full = NULL,
      diet_time = NULL,
      new.bio = NULL,
      grp.codes = NULL,
      grp.prd = NULL,
      time = NULL,
      stocks = NULL,
      age.gr.pred = NULL,
      col.nam = NULL,
      g.col = NULL,
      fg_palette = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$biom_file, input$grp_csv, input$diet_file)
      
      tryCatch({
        showNotification("Loading predation data...", type = "message", id = "load_pred", duration = NULL)
        
        cur.dat <- data.frame(data.table::fread(input$biom_file$datapath, header = TRUE, sep = ' ', showProgress = FALSE))
        diet_raw <- data.table::fread(input$diet_file$datapath, header = TRUE, sep = ' ', showProgress = FALSE)
        grp <- utils::read.csv(input$grp_csv$datapath, stringsAsFactors = FALSE)
        
        code_col <- if ("Code" %in% names(grp)) grp$Code else grp$code
        on_col <- if ("IsTurnedOn" %in% names(grp)) grp$IsTurnedOn else grp$isturnedon
        ncoh_col <- if ("NumCohorts" %in% names(grp)) grp$NumCohorts else grp$numcohorts
        grp.prd <- code_col[on_col == 1 & ncoh_col > 1]
        grp.codes <- code_col[on_col == 1]
        fg_palette <- build_fg_palette(code_col)
        
        sub.cur <- cbind(cur.dat[c('Time', as.character(grp.codes))])
        sp.name <- c("Time", paste0('Rel', grp.codes), "PelDemRatio", "PiscivPlankRatio")
        sp.name2 <- c("Time", as.character(grp.codes))
        
        biom.tot <- reshape2::melt(cur.dat[, sp.name2], id.vars = 'Time')
        rel.bio <- reshape2::melt(cur.dat[, sp.name], id.vars = 'Time')
        names(biom.tot) <- c('Time', 'FG', 'Biomass')
        names(rel.bio) <- c('Time', 'FG', 'RelBiom')
        
        diet_agg <- preaggregate_diet(diet_raw)
        diet.data <- as.data.frame(diet_agg$diet_full)
        
        predators <- as.character(unique(diet.data$Predator))
        time <- unique(diet.data$Time)
        stocks <- unique(diet.data$Stock)
        
        sub.cur <- cbind(Time = sub.cur$Time, sub.cur[, names(sub.cur) %in% predators])
        new.bio <- reshape2::melt(sub.cur, id = c('Time'))
        colnames(new.bio) <- c('Time', 'Predator', 'Biomass')
        new.bio$Predator <- as.character(new.bio$Predator)
        
        age.gr.pred <- NULL
        col.nam <- NULL
        if(!is.null(input$age_biomass)){
          age.gr.pred <- data.frame(data.table::fread(input$age_biomass$datapath, header = TRUE, sep = ' ', showProgress = FALSE))
          col.nam <- stringr::str_extract(names(age.gr.pred), "[aA-zZ]+")
        }
        
        data.table::setDT(new.bio)
        new.bio[, Predator := as.character(Predator)]
        data.table::setkey(new.bio, Predator, Time)
        
        g.col <- data.frame(grp = names(fg_palette), col = unname(fg_palette), stringsAsFactors = FALSE)
        
        rv$biom.tot <- biom.tot
        rv$rel.bio <- rel.bio
        rv$diet.data <- diet.data
        rv$diet_full <- diet_agg$diet_full
        rv$diet_time <- diet_agg$diet_time
        rv$new.bio <- new.bio
        rv$grp.codes <- grp.codes
        rv$grp.prd <- grp.prd
        rv$time <- time
        rv$stocks <- stocks
        rv$age.gr.pred <- age.gr.pred
        rv$col.nam <- col.nam
        rv$g.col <- g.col
        rv$fg_palette <- fg_palette
        rv$data_loaded <- TRUE
        
        updateSelectInput(session, "fg_pred", choices = as.character(grp.codes))
        updateSelectInput(session, "fg_age", choices = as.character(grp.codes))
        updateSelectInput(session, "stock_pred", choices = stocks)
        updateSelectInput(session, "stock_age", choices = stocks)
        updateSliderInput(session, "time_age", min = min(time), max = max(time), 
                         value = min(time), step = diff(time)[1])
        
        removeNotification("load_pred")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("load_pred")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    output$analysis_output <- renderUI({
      req(rv$data_loaded)
      ns <- session$ns
      
      if (input$analysis_type == "biomass") {
        tagList(
          tabsetPanel(
            tabPanel("Total Biomass",
                    plotOutput(ns("plot_biomass"), height = "800px")
            ),
            tabPanel("Relative Biomass",
                    plotOutput(ns("plot_rel_biomass"), height = "800px")
            )
          )
        )
      } else if (input$analysis_type == "predation") {
        tagList(
          uiOutput(ns("pred_time_panel")),
          uiOutput(ns("prey_time_panel"))
        )
      } else if (input$analysis_type == "age") {
        tagList(
          plotOutput(ns("plot_bio_pred"), height = "300px"),
          plotOutput(ns("plot_pred_age"), height = "500px")
        )
      }
    })
    
    output$plot_biomass <- renderPlot({
      req(rv$biom.tot)
      
      pal <- get_fg_colors(unique(as.character(rv$biom.tot$FG)), rv$fg_palette)
      
      p <- ggplot2::ggplot(data = rv$biom.tot, aes(x = Time, y = Biomass, colour = FG)) 
      p <- p + geom_line(linewidth = 0.8)
      p <- p + facet_wrap(~ FG, ncol = 4, scale = 'free_y') + theme_minimal()
      p <- p + scale_color_manual(values = pal, guide = "none")
      p <- p + labs(title = 'Total Biomass by Functional Group', x = 'Time (days)', y = 'Biomass (tons)')
      p <- p + theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 10, face = "bold")
      )
      p
    })
    
    output$plot_rel_biomass <- renderPlot({
      req(rv$rel.bio)
      
      colors <- get_palette(2)
      
      p <- ggplot2::ggplot(data = rv$rel.bio)
      p <- p + geom_line(aes(x = Time, y = RelBiom), colour = colors[2], linewidth = 0.8, na.rm = TRUE)
      p <- p + facet_wrap(~ FG, ncol = 4) + theme_minimal() + ylim(0, 2)
      p <- p + labs(title = 'Relative Biomass by Functional Group', x = 'Time (days)', y = 'Relative Biomass (Bt/B0)')
      p <- p + theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 10, face = "bold")
      )
      p
    })
    
    output$pred_time_panel <- renderUI({
      ns <- session$ns
      dat <- predator_data()
      if (is.null(dat) || nrow(dat) == 0) {
        shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 20px;",
          "This functional group has no prey diet recorded."
        )
      } else {
        plotOutput(ns("plot_pred_time"), height = "400px")
      }
    })
    
    output$prey_time_panel <- renderUI({
      ns <- session$ns
      dat <- prey_data()
      if (is.null(dat) || nrow(dat) == 0) {
        shiny::div(
          class = "alert alert-warning",
          style = "margin-top: 20px;",
          "This functional group has no predators."
        )
      } else {
        plotOutput(ns("plot_prey_time"), height = "400px")
      }
    })
    
    predator_data <- reactive({
      req(rv$diet_time, rv$new.bio, input$fg_pred, input$threshold_pred)
      diet_through_time_predator(
        rv$diet_time, rv$new.bio, input$fg_pred, input$stock_pred,
        input$threshold_pred, isTRUE(input$melt_time)
      )
    })
    
    prey_data <- reactive({
      req(rv$diet_time, rv$new.bio, input$fg_pred, input$threshold_pred)
      diet_through_time_prey(
        rv$diet_time, rv$new.bio, input$fg_pred,
        input$threshold_pred, isTRUE(input$melt_time)
      )
    })
    
    output$plot_pred_time <- renderPlot({
      predator <- predator_data()
      req(!is.null(predator), nrow(predator) > 0)
      
      prey_names <- unique(as.character(predator$variable))
      colors <- get_fg_colors(prey_names, rv$fg_palette)
      
      p <- ggplot2::ggplot(data = predator, aes(x = Time, y = consum, fill = variable, width = 1))
      p <- p + geom_bar(stat = "identity", position = if(input$scaled_pred) 'fill' else 'stack', na.rm = TRUE)
      p <- p + scale_fill_manual(values = colors, name = 'Prey')
      p <- p + labs(title = paste('Predator -', input$fg_pred),
                   x = diet_time_axis_label(input$melt_time),
                   y = if(input$scaled_pred) 'Proportion' else 'Biomass [tons]')
      p <- p + theme_minimal()
      p <- p + theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11),
        legend.position = "right"
      )
      p
    })
    
    output$plot_prey_time <- renderPlot({
      prey <- prey_data()
      req(!is.null(prey), nrow(prey) > 0)
      
      pred_names <- unique(as.character(prey$Predator))
      colors <- get_fg_colors(pred_names, rv$fg_palette)
      
      p <- ggplot2::ggplot(data = prey, aes(x = Time, y = eff.pred, fill = Predator, width = 1))
      p <- p + geom_bar(stat = "identity", position = if(input$scaled_pred) 'fill' else 'stack', na.rm = TRUE)
      p <- p + scale_fill_manual(values = colors, name = 'Predator')
      p <- p + labs(title = paste('Prey -', input$fg_pred),
                   x = diet_time_axis_label(input$melt_time),
                   y = if(input$scaled_pred) 'Proportion' else 'Biomass [tons]')
      p <- p + theme_minimal()
      p <- p + theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11),
        legend.position = "right"
      )
      p
    })
    
    age_diet_data <- reactive({
      req(rv$diet_full, input$fg_age, input$stock_age, input$time_age, input$threshold_age)
      age_diet_slice(rv$diet_full, input$fg_age, input$stock_age, input$time_age, input$threshold_age)
    })
    
    age_diet_bio <- reactive({
      req(rv$new.bio, input$fg_age)
      rv$new.bio[rv$new.bio$Predator == input$fg_age, ]
    })
    
    output$plot_bio_pred <- renderPlot({
      req(age_diet_bio(), input$time_age)
      
      bio_data <- age_diet_bio()
      
      plot(bio_data$Time, bio_data$Biomass, ylab = 'Biomass (tons)', xlab = 'Time (days)', 
           bty = 'n', type = 'l', ylim = range(bio_data$Biomass), las = 1,
           main = paste0('Biomass - ', input$fg_age))
      points(bio_data$Time[bio_data$Time == input$time_age], 
            bio_data$Biomass[bio_data$Time == input$time_age], 
            pch = 19, col = 'firebrick3', cex = 1.3)
    })
    
    output$plot_pred_age <- renderPlot({
      req(age_diet_data(), rv$g.col)
      
      age.diet <- age_diet_data()
      prey_names <- unique(as.character(age.diet$variable))
      colors <- get_fg_colors(prey_names, rv$fg_palette)
      
      p <- ggplot2::ggplot(data = age.diet, aes(x = Cohort, y = value, fill = variable, width = .75))
      p <- p + geom_bar(stat = "identity", position = 'fill') + scale_fill_manual(values = colors, name = 'Prey')
      p <- p + labs(title = paste('Predator -', input$fg_age, 'at', input$time_age, 'days'),
                   x = 'AgeGroup', y = 'Proportion')
      p <- p + theme_minimal()
      p <- p + theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11),
        legend.position = "right"
      )
      p
    })
  })
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
    diet_time <- dt[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)),
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
