## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Catch Analysis Module             ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

catch_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Catch and Harvest Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(ns("grp_csv"), "Groups CSV", accept = ".csv"),
        fileInput(ns("fish_csv"), "Fisheries CSV", accept = ".csv"),
        fileInput(ns("catch_nc"), "Catch Output (.nc)", accept = ".nc"),
        fileInput(ns("ext_catch"), "External Catch [Optional]", accept = ".csv"),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          h5("Analysis Options"),
          selectInput(ns("analysis_type"), "Analysis Type",
                     choices = c("Biomass" = "biomass",
                               "Numbers" = "numbers",
                               "Skill Assessment" = "skill")),
          selectInput(ns("fishery"), "Fishery:", choices = NULL),
          selectInput(ns("fg"), "Functional Group:", choices = NULL),
          checkboxInput(ns("by_year"), "Aggregate by Year", TRUE)
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
            " Please load your catch data files to analyze harvest outputs and model skill."
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

catch_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      data_loaded = FALSE,
      nc_data = NULL,
      grp = NULL,
      fish = NULL,
      ext_catch = NULL,
      time = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$grp_csv, input$fish_csv, input$catch_nc)
      
      tryCatch({
        showNotification("Loading catch data...", type = "message", id = "load_catch", duration = NULL)
        
        nc.data <- ncdf4::nc_open(input$catch_nc$datapath)
        grp <- read.csv(input$grp_csv$datapath)
        names(grp) <- tolower(names(grp))
        fish <- read.csv(input$fish_csv$datapath)
        
        time <- ncdf4::ncvar_get(nc.data, 't')
        time <- as.Date(time / 86400, origin = '1970-01-01')
        
        ext_catch <- NULL
        if(!is.null(input$ext_catch)){
          ext_catch <- read.csv(input$ext_catch$datapath)
        }
        
        impacted_grp <- grp[grp$isimpacted == 1, ]
        
        rv$nc_data <- nc.data
        rv$grp <- grp
        rv$fish <- fish
        rv$ext_catch <- ext_catch
        rv$time <- time
        rv$data_loaded <- TRUE
        
        updateSelectInput(session, "fishery", choices = as.character(fish$Name))
        updateSelectInput(session, "fg", choices = as.character(impacted_grp$code))
        
        removeNotification("load_catch")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("load_catch")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    output$analysis_output <- renderUI({
      req(rv$data_loaded)
      ns <- session$ns
      
      if (input$analysis_type == "skill") {
        tagList(
          h4("Model Skill Assessment"),
          DT::dataTableOutput(ns("skill_table"))
        )
      } else {
        plotOutput(ns("catch_plot"), height = "700px")
      }
    })
    
    catch_data <- reactive({
      req(rv$nc_data, rv$grp, input$fg)
      
      pos <- which(rv$grp$code == input$fg)
      if(length(pos) == 0) return(NULL)
      
      is.C <- paste0('_', input$fishery, '_Catch')
      
      if(rv$grp$numcohorts[pos] > 1){
        Tcatch <- NULL
        for(coh in 1:rv$grp$numcohorts[pos]){
          name.fg <- paste0(rv$grp$name[pos], coh, is.C)
          tmp <- tryCatch({
            data <- ncdf4::ncvar_get(rv$nc_data, name.fg)
            catch_vals <- colSums(data, na.rm = TRUE)
            data.frame(years = rv$time, cohort = paste0('Cohort', coh), catch = catch_vals)
          }, error = function(e) NULL)
          
          if(!is.null(tmp)) Tcatch <- rbind(Tcatch, tmp)
        }
      } else {
        name.fg <- paste0(rv$grp$name[pos], is.C)
        tmp <- tryCatch({
          data <- ncdf4::ncvar_get(rv$nc_data, name.fg)
          catch_vals <- colSums(data, na.rm = TRUE)
          data.frame(years = rv$time, cohort = 'Biomass Pool', catch = catch_vals)
        }, error = function(e) NULL)
        Tcatch <- tmp
      }
      
      if(input$by_year && !is.null(Tcatch)){
        Tcatch$year <- format(Tcatch$years, '%Y')
        Tcatch <- aggregate(catch ~ year + cohort, data = Tcatch, sum)
        Tcatch$years <- as.Date(paste0(Tcatch$year, '-01-01'))
      }
      
      Tcatch
    })
    
    output$catch_plot <- renderPlot({
      req(catch_data())
      
      catch_df <- catch_data()
      colors <- get_catch_colors(length(unique(catch_df$cohort)))
      
      if(input$by_year){
        time_var <- as.Date(unique(format(catch_df$years, '%Y')), format = '%Y')
      } else {
        time_var <- catch_df$years
      }
      
      p <- ggplot2::ggplot(catch_df, aes(x = years, y = catch, color = cohort))
      p <- p + geom_line(linewidth = 1.2)
      p <- p + scale_color_manual(values = colors, name = 'Cohort')
      p <- p + theme_minimal()
      p <- p + labs(title = paste('Catch -', input$fishery, '-', input$fg),
                   x = if(input$by_year) 'Year' else 'Date',
                   y = 'Catch (tons)')
      p <- p + theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "right"
      )
      
      if(!is.null(rv$ext_catch) && input$fg %in% names(rv$ext_catch)){
        ext_df <- data.frame(
          years = as.Date(paste0(rv$ext_catch$Time, '-01-01')),
          catch = rv$ext_catch[[input$fg]]
        )
        p <- p + geom_point(data = ext_df, aes(x = years, y = catch), 
                           color = 'red', size = 3, inherit.aes = FALSE)
      }
      
      p
    })
    
    output$skill_table <- DT::renderDataTable({
      req(catch_data(), rv$ext_catch)
      
      if(is.null(rv$ext_catch)) {
        return(data.frame(Note = "External catch data required for skill assessment"))
      }
      
      catch_df <- catch_data()
      if(is.null(catch_df) || !(input$fg %in% names(rv$ext_catch))) {
        return(data.frame(Note = paste("No external data for", input$fg)))
      }
      
      catch_agg <- aggregate(catch ~ format(years, '%Y'), data = catch_df, sum)
      names(catch_agg) <- c('year', 'modeled')
      
      ext_df <- data.frame(
        year = as.character(rv$ext_catch$Time),
        observed = rv$ext_catch[[input$fg]]
      )
      
      merged <- merge(catch_agg, ext_df, by = 'year')
      merged <- merged[complete.cases(merged), ]
      
      if(nrow(merged) < 3){
        return(data.frame(Note = "Insufficient overlapping data for skill assessment"))
      }
      
      skill <- skill_assessment(merged$observed, merged$modeled, input$fg)
      
      DT::datatable(skill, options = list(pageLength = 10, dom = 't'), rownames = FALSE)
    })
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Helper Functions                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

skill_assessment <- function(obs, mod, FG){
  COR <- cor.test(obs, mod, method = 'spearman', use = "pairwise.complete.obs", exact = FALSE)
  AE <- mean(obs, na.rm = TRUE) - mean(mod, na.rm = TRUE)
  diff <- mod - obs
  AAE <- mean(abs(diff), na.rm = TRUE)
  RMSE <- sqrt(mean((diff) ^ 2, na.rm = TRUE))
  
  tmp <- log(obs / mod) ^ 2
  tmp[is.infinite(tmp)] <- NA
  RI <- exp(sqrt(mean(tmp, na.rm = TRUE)))
  
  ME <- 1 - (RMSE ^ 2) / var(obs, na.rm = TRUE)
  
  if(COR$p.value == 0) COR$p.value <- '< 2.2e-16'
  
  out <- data.frame(
    FunctionalGroup = c(FG, NA, NA, NA, NA, NA),
    Metric = c('Correlation (Spearman)', 'Average Error (AE)',
              'Average Absolute Error (AAE)', 'Root Mean Squared Error (RMSE)', 
              'Reliability Index (RI)', 'Model Efficiency (ME)'),
    Value = round(c(as.numeric(COR$estimate), AE, AAE, RMSE, RI, ME), 4),
    PValue = c(as.character(COR$p.value), NA, NA, NA, NA, NA)
  )
  
  return(out)
}
