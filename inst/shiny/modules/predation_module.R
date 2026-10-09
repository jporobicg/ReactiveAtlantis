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
      new.bio = NULL,
      grp.codes = NULL,
      grp.prd = NULL,
      time = NULL,
      stocks = NULL,
      age.gr.pred = NULL,
      col.nam = NULL,
      g.col = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$biom_file, input$grp_csv, input$diet_file)
      
      tryCatch({
        showNotification("Loading predation data...", type = "message", id = "load_pred", duration = NULL)
        
        cur.dat <- data.frame(data.table::fread(input$biom_file$datapath, header = TRUE, sep = ' ', showProgress = FALSE))
        diet.data <- data.frame(data.table::fread(input$diet_file$datapath, header = TRUE, sep = ' ', showProgress = FALSE))
        grp <- utils::read.csv(input$grp_csv$datapath)
        
        grp.prd <- grp[grp$IsTurnedOn == 1 & grp$NumCohorts > 1, ]$Code
        grp.codes <- grp[grp$IsTurnedOn == 1, ]$Code
        
        sub.cur <- cbind(cur.dat[c('Time', as.character(grp.codes))])
        sp.name <- c("Time", paste0('Rel', grp.codes), "PelDemRatio", "PiscivPlankRatio")
        sp.name2 <- c("Time", as.character(grp.codes))
        
        biom.tot <- reshape2::melt(cur.dat[, sp.name2], id.vars = 'Time')
        rel.bio <- reshape2::melt(cur.dat[, sp.name], id.vars = 'Time')
        names(biom.tot) <- c('Time', 'FG', 'Biomass')
        names(rel.bio) <- c('Time', 'FG', 'RelBiom')
        
        if(any('Updated' == colnames(diet.data))){
          rem <- which(names(diet.data) == 'Updated')
          diet.data <- diet.data[, -rem]
        }
        
        if(any(colnames(diet.data) == 'Group')){
          colnames(diet.data)[which(colnames(diet.data) == 'Group')] <- 'Predator'
        }
        
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
        
        g.col <- data.frame(grp = grp.codes, col = get_fg_colors(as.character(grp.codes)))
        
        rv$biom.tot <- biom.tot
        rv$rel.bio <- rel.bio
        rv$diet.data <- diet.data
        rv$new.bio <- new.bio
        rv$grp.codes <- grp.codes
        rv$grp.prd <- grp.prd
        rv$time <- time
        rv$stocks <- stocks
        rv$age.gr.pred <- age.gr.pred
        rv$col.nam <- col.nam
        rv$g.col <- g.col
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
          plotOutput(ns("plot_pred_time"), height = "400px"),
          plotOutput(ns("plot_prey_time"), height = "400px")
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
      
      colors <- get_palette(2)
      
      p <- ggplot2::ggplot(data = rv$biom.tot, aes(x = Time, y = Biomass)) 
      p <- p + geom_line(colour = colors[1], linewidth = 0.8)
      p <- p + facet_wrap(~ FG, ncol = 4, scale = 'free_y') + theme_minimal()
      p <- p + scale_color_manual(values = colors)
      p <- p + labs(title = 'Total Biomass by Functional Group', x = 'Time step', y = 'Biomass (tons)')
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
      p <- p + labs(title = 'Relative Biomass by Functional Group', x = 'Time step', y = 'Relative Biomass (Bt/B0)')
      p <- p + theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 10, face = "bold")
      )
      p
    })
    
    predator_data <- reactive({
      req(rv$diet.data, rv$new.bio, input$fg_pred, input$stock_pred, input$threshold_pred)
      
      predator <- rv$diet.data[rv$diet.data$Predator == input$fg_pred & rv$diet.data$Stock == as.numeric(input$stock_pred), ]
      predator <- predator[, -which(names(predator) %in% c('Predator', 'Cohort', 'Stock'))]
      predator <- predator[, (colSums(predator, na.rm = TRUE) > input$threshold_pred)]
      predator[which(predator < input$threshold_pred, arr.ind = TRUE)] <- NA
      predator <- reshape2::melt(predator, id.vars = "Time", na.rm = TRUE)
      predator <- dplyr::left_join(predator, rv$new.bio[rv$new.bio$Predator == input$fg_pred, ], by = 'Time')
      predator$consum <- with(predator, value * Biomass)
      if(input$melt_time) predator$Time <- predator$Time / diff(unique(predator$Time))[1]
      predator <- as.data.frame(predator)
      predator
    })
    
    prey_data <- reactive({
      req(rv$diet.data, rv$new.bio, input$fg_pred, input$threshold_pred)
      
      prey <- rv$diet.data[, names(rv$diet.data) %in% c('Time', 'Predator', input$fg_pred)]
      prey <- prey[prey[, input$fg_pred] > 0, ]
      prey <- dplyr::left_join(prey, rv$new.bio, by = c('Predator', 'Time'))
      prey$eff.pred <- prey[, input$fg_pred] * prey$Biomass
      trh.max <- max(prey$eff.pred, na.rm = TRUE) * input$threshold_pred
      if(input$melt_time) prey$Time <- prey$Time / diff(unique(prey$Time))[1]
      prey <- prey[prey$eff.pred > trh.max, ]
      prey
    })
    
    output$plot_pred_time <- renderPlot({
      req(predator_data())
      
      predator <- predator_data()
      colors <- get_prey_colors(length(unique(predator$variable)))
      
      p <- ggplot2::ggplot(data = predator, aes(x = Time, y = consum, fill = variable, width = 1))
      p <- p + geom_bar(stat = "identity", position = if(input$scaled_pred) 'fill' else 'stack', na.rm = TRUE)
      p <- p + scale_fill_manual(values = colors, name = 'Prey')
      p <- p + labs(title = paste('Predator -', input$fg_pred), x = 'Time step', 
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
      validate(
        need(length(prey_data()$eff.pred) != 0, 'This functional group has no predators.')
      )
      
      prey <- prey_data()
      colors <- get_predator_colors(length(unique(prey$Predator)))
      
      p <- ggplot2::ggplot(data = prey, aes(x = Time, y = eff.pred, fill = Predator, width = 1))
      p <- p + geom_bar(stat = "identity", position = if(input$scaled_pred) 'fill' else 'stack', na.rm = TRUE)
      p <- p + scale_fill_manual(values = colors, name = 'Predator')
      p <- p + labs(title = paste('Prey -', input$fg_pred), x = 'Time step',
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
      req(rv$diet.data, input$fg_age, input$stock_age, input$time_age, input$threshold_age)
      
      age.diet <- rv$diet.data[rv$diet.data$Predator == input$fg_age & 
                               rv$diet.data$Stock == as.numeric(input$stock_age) & 
                               rv$diet.data$Time == as.numeric(input$time_age), ]
      age.diet <- age.diet[, -which(names(age.diet) %in% c('Predator', 'Time', 'Stock'))]
      age.diet <- age.diet[, (colSums(age.diet, na.rm = TRUE) > input$threshold_age)]
      age.diet <- reshape2::melt(age.diet, id = 'Cohort')
      age.diet[age.diet$value > input$threshold_age, ]
    })
    
    age_diet_bio <- reactive({
      req(rv$new.bio, input$fg_age)
      rv$new.bio[rv$new.bio$Predator == input$fg_age, ]
    })
    
    output$plot_bio_pred <- renderPlot({
      req(age_diet_bio(), input$time_age)
      
      bio_data <- age_diet_bio()
      
      plot(bio_data$Time, bio_data$Biomass, ylab = 'Biomass (tons)', xlab = 'Time step', 
           bty = 'n', type = 'l', ylim = range(bio_data$Biomass), las = 1,
           main = paste0('Biomass - ', input$fg_age))
      points(bio_data$Time[bio_data$Time == input$time_age], 
            bio_data$Biomass[bio_data$Time == input$time_age], 
            pch = 19, col = 'firebrick3', cex = 1.3)
    })
    
    output$plot_pred_age <- renderPlot({
      req(age_diet_data(), rv$g.col)
      
      age.diet <- age_diet_data()
      color.pp <- as.character(rv$g.col$col[which(rv$g.col$grp %in% levels(age.diet$variable))])
      
      p <- ggplot2::ggplot(data = age.diet, aes(x = Cohort, y = value, fill = variable, width = .75))
      p <- p + geom_bar(stat = "identity", position = 'fill') + scale_fill_manual(values = color.pp)
      p <- p + labs(title = paste('Predator -', input$fg_age, 'on Time step:', input$time_age),
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
