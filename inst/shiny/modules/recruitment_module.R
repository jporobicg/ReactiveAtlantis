## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~         Recruitment Analysis Module        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

recruitment_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Recruitment and Primary Production"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(ns("ini_nc"), "Initial Conditions (.nc)", accept = ".nc"),
        fileInput(ns("out_nc"), "Output (.nc)", accept = ".nc"),
        fileInput(ns("yoy_file"), "YOY File", accept = ".txt"),
        fileInput(ns("grp_file"), "Groups CSV", accept = ".csv"),
        fileInput(ns("prm_file"), "Parameters File (.prm)", accept = ".prm"),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          h5("Analysis Options"),
          selectInput(ns("analysis_type"), "Analysis Type",
                     choices = c("Recruits and YOY" = "yoy",
                               "Growth Zoo and PPs" = "growth")),
          conditionalPanel(
            condition = "input.analysis_type == 'yoy'",
            ns = ns,
            selectInput(ns("fg_yoy"), "Functional Group:", choices = NULL),
            numericInput(ns("new_alpha"), "New Alpha:", value = 0, step = 0.1),
            numericInput(ns("new_beta"), "New Beta:", value = 0, step = 0.1),
            actionButton(ns("recalc"), "Recalculate", class = "btn-primary")
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'growth'",
            ns = ns,
            selectInput(ns("fg_growth"), "Functional Group:", choices = NULL),
            selectInput(ns("box"), "Box:", choices = NULL),
            checkboxInput(ns("layer_prop"), "Layer Proportion", TRUE),
            checkboxInput(ns("box_prop"), "Box Proportion", FALSE),
            checkboxInput(ns("log_scale"), "Logarithmic", FALSE)
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
            " Please load your recruitment data files to analyze recruitment parameters and YOY curves."
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

recruitment_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      data_loaded = FALSE,
      yoy_data = NULL,
      rec_params = NULL,
      pp_list = NULL,
      grp = NULL,
      time = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$ini_nc, input$out_nc, input$yoy_file, input$grp_file, input$prm_file)
      
      tryCatch({
        showNotification("Loading recruitment data...", type = "message", id = "load_rec", duration = NULL)
        
        nc.out <- ncdf4::nc_open(input$out_nc$datapath)
        yoy <- read.csv(input$yoy_file$datapath, sep = ' ')
        grp <- read.csv(input$grp_file$datapath)
        names(grp) <- tolower(names(grp))
        prm <- readLines(input$prm_file$datapath, warn = FALSE)
        
        sp.dat <- with(grp, which(isturnedon == 1 & numcohorts > 1))
        
        options(warn = -1)
        rec <- text2num(prm, '^flagrecruit', FG = 'look')
        options(warn = 0)
        rec <- rec[complete.cases(rec), ]
        
        rec <- cbind(rec, Alpha = NA, Beta = NA)
        sps <- gsub(pattern = '^flagrecruit', '', rec$FG)
        rec$FG <- sps
        
        for(fg.r in 1:length(sps)){
          if(!sps[fg.r] %in% grp$code[sp.dat]) next()
          if(rec$Value[fg.r] == 3){
            rec$Alpha[fg.r] <- text2num(prm, paste0('BHalpha_', sps[fg.r]), FG = 'look')[1, 2]
            rec$Beta[fg.r] <- text2num(prm, paste0('BHbeta_', sps[fg.r]), FG = 'look')[1, 2]
          }
        }
        
        pp.pos <- with(grp, which(grouptype %in% c('MED_ZOO', 'LG_ZOO', 'LG_PHY', 'SM_PHY', 'PHYTOBEN', 'DINOFLAG', "TURF") & isturnedon == 1))
        pp.fg <- grp$name[pp.pos]
        pp.cod <- as.character(grp$code[pp.pos])
        pp.list <- list()
        for(l.pp in 1:length(pp.fg)){
          pp.list[[l.pp]] <- ncdf4::ncvar_get(nc.out, paste0(pp.fg[l.pp], '_N'))
          names(pp.list)[l.pp] <- pp.cod[l.pp]
        }
        pp.list[['Light']] <- ncdf4::ncvar_get(nc.out, 'Light')
        pp.list[['Eddy']] <- ncdf4::ncvar_get(nc.out, 'eddy')
        
        time <- ncdf4::ncvar_get(nc.out, 't') / 86400
        
        rv$yoy_data <- yoy
        rv$rec_params <- rec
        rv$pp_list <- pp.list
        rv$grp <- grp
        rv$time <- time
        rv$data_loaded <- TRUE
        
        age_groups <- grp[grp$numcohorts > 1, ]
        updateSelectInput(session, "fg_yoy", choices = as.character(age_groups$code))
        updateSelectInput(session, "fg_growth", choices = pp.cod)
        updateSelectInput(session, "box", choices = 0:50)
        
        updateNumericInput(session, "new_alpha", 
                          value = rec$Alpha[1])
        updateNumericInput(session, "new_beta", 
                          value = rec$Beta[1])
        
        removeNotification("load_rec")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("load_rec")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    observeEvent(input$fg_yoy, {
      req(rv$rec_params, input$fg_yoy)
      rec_row <- which(rv$rec_params$FG == input$fg_yoy)
      if(length(rec_row) > 0){
        updateNumericInput(session, "new_alpha", value = rv$rec_params$Alpha[rec_row])
        updateNumericInput(session, "new_beta", value = rv$rec_params$Beta[rec_row])
      }
    })
    
    output$analysis_output <- renderUI({
      req(rv$data_loaded)
      ns <- session$ns
      
      if (input$analysis_type == "yoy") {
        tagList(
          plotOutput(ns("plot_yoy"), height = "350px"),
          plotOutput(ns("plot_relative_yoy"), height = "350px")
        )
      } else {
        plotOutput(ns("plot_pp_growth"), height = "700px")
      }
    })
    
    yoy_calculated <- reactive({
      req(rv$yoy_data, input$fg_yoy)
      
      yoy_col <- paste0(input$fg_yoy, '.0')
      if(!(yoy_col %in% names(rv$yoy_data))) return(NULL)
      
      list(
        time = rv$yoy_data$Time,
        original = rv$yoy_data[[yoy_col]],
        relative = rv$yoy_data[[yoy_col]] / rv$yoy_data[[yoy_col]][1]
      )
    })
    
    yoy_new <- eventReactive(input$recalc, {
      req(yoy_calculated(), input$new_alpha, input$new_beta)
      
      alpha <- input$new_alpha
      beta <- input$new_beta
      original <- yoy_calculated()$original
      
      new_yoy <- alpha * original / (1 + beta * original)
      
      list(
        time = yoy_calculated()$time,
        new = new_yoy,
        relative_new = new_yoy / new_yoy[1]
      )
    })
    
    output$plot_yoy <- renderPlot({
      req(yoy_calculated())
      
      colors <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
      
      plot(yoy_calculated()$time, yoy_calculated()$original, type = 'l', 
           col = colors[1], lwd = 2,
           xlab = "Time step", ylab = "Young of the Year",
           main = paste("YOY and Larvae -", input$fg_yoy))
      
      if(!is.null(yoy_new())){
        lines(yoy_new()$time, yoy_new()$new, col = colors[2], lwd = 2)
        legend("topleft", c("Original", "New parameters"), 
               col = colors[1:2], lwd = 2, bty = 'n')
      }
    })
    
    output$plot_relative_yoy <- renderPlot({
      req(yoy_calculated())
      
      colors <- RColorBrewer::brewer.pal(n = 8, name = "Set1")
      
      plot(yoy_calculated()$time, yoy_calculated()$relative, type = 'l', 
           col = colors[3], lwd = 2,
           xlab = "Time step", ylab = "Relative YOY",
           main = "Relative YOY (YOY/YOY0)")
      abline(h = 1, lty = 2, col = "gray50")
      
      if(!is.null(yoy_new())){
        lines(yoy_new()$time, yoy_new()$relative_new, col = colors[4], lwd = 2)
        legend("topright", c("Original", "New parameters"), 
               col = colors[3:4], lwd = 2, bty = 'n')
      }
    })
    
    output$plot_pp_growth <- renderPlot({
      req(rv$pp_list, input$fg_growth, input$box)
      
      colors <- get_pp_colors(length(rv$pp_list) + 2)
      
      par(mfrow = c(3, 1), mar = c(4, 4, 3, 2))
      
      fg_data <- rv$pp_list[[input$fg_growth]]
      if(is.null(fg_data)) return()
      
      if(length(dim(fg_data)) == 3){
        box_num <- as.numeric(input$box) + 1
        if(box_num > dim(fg_data)[2]) box_num <- 1
        
        n_layers <- dim(fg_data)[1]
        n_time <- dim(fg_data)[3]
        
        for(layer in 1:min(n_layers, 3)){
          layer_data <- fg_data[layer, box_num, ]
          
          if(input$log_scale) layer_data <- log(layer_data + 1)
          if(input$layer_prop || input$box_prop){
            layer_data <- layer_data / max(layer_data, na.rm = TRUE)
          }
          
          plot(1:n_time, layer_data, type = 'l', col = colors[layer], lwd = 2,
               xlab = "Time step", 
               ylab = if(input$log_scale) "log(Biomass)" else "Biomass",
               main = paste(input$fg_growth, "- Layer", layer, "- Box", input$box))
        }
      } else {
        plot(1:length(fg_data), fg_data, type = 'l', col = colors[1], lwd = 2,
             xlab = "Time step", ylab = "Biomass",
             main = paste(input$fg_growth, "- Box", input$box))
      }
    })
  })
}
