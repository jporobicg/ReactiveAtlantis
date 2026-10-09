## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Mortality Analysis Module         ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

mortality_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Mortality Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(ns("grp_file"), "Groups CSV", accept = ".csv"),
        fileInput(ns("spe_mort"), "Specific Mortality (SpecificMort.txt)", accept = ".txt"),
        fileInput(ns("pred_mort"), "Predation Mortality (SpecificPredMort.txt)", accept = ".txt"),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          h5("Analysis Options"),
          selectInput(ns("fg"), "Functional Group:", choices = NULL)
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
            h4("Mortality Analysis"),
            p("This tool analyzes natural, fishing, and predation mortality from Atlantis model outputs."),
            hr(),
            h5("Required Files:"),
            tags$ul(
              tags$li(strong("SpecificMort.txt:"), " Specific mortality output file"),
              tags$li(strong("SpecificPredMort.txt:"), " Predation mortality output file"),
              tags$li(strong("Groups CSV:"), " Functional groups definition")
            ),
            hr(),
            h5("Generating Mortality Output Files:"),
            p("These files are optional Atlantis outputs. To generate them, add the following flags to your", 
              code("run.prm"), "file:"),
            tags$pre(style = "background-color: #f5f5f5; padding: 10px; border-radius: 4px;",
              "flagspecmort 1\nflagspecpredmort 1"
            ),
            p("Then run your Atlantis simulation. The mortality files will be created in the output directory.")
          )
        ),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          plotOutput(ns("mortality_plot"), height = "600px")
        )
      )
    )
  )
}

mortality_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      data_loaded = FALSE,
      SpeMort = NULL,
      PredMort = NULL,
      grp = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$grp_file, input$spe_mort, input$pred_mort)
      
      tryCatch({
        showNotification("Loading mortality data...", type = "message", id = "load_mort", duration = NULL)
        
        SpeMort <- read.csv(input$spe_mort$datapath, sep = ' ')
        PredMort <- read.csv(input$pred_mort$datapath, sep = ' ')
        grp <- read.csv(input$grp_file$datapath)
        names(grp) <- tolower(names(grp))
        
        rv$SpeMort <- SpeMort
        rv$PredMort <- PredMort
        rv$grp <- grp
        rv$data_loaded <- TRUE
        
        updateSelectInput(session, "fg", choices = as.character(grp$longname))
        
        removeNotification("load_mort")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("load_mort")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    mort_data <- reactive({
      req(rv$data_loaded, input$fg)
      FG <- rv$grp$code[which(rv$grp$longname %in% input$fg)]
      calc_mort(rv$SpeMort, rv$PredMort, FG)
    })
    
    output$mortality_plot <- renderPlot({
      req(mort_data())
      
      mort <- reshape2::melt(mort_data(), id.var = c('Time', 'Mtype'))
      colors <- get_mortality_colors(length(levels(mort$variable)))
      
      Title <- paste0('Mortalities for ', input$fg)
      
      p <- ggplot2::ggplot(mort, ggplot2::aes(Time, value, fill = variable))
      p <- p + ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge())
      p <- p + ggplot2::facet_wrap(~Mtype, ncol = 1, scales = "free_y") 
      p <- p + ggplot2::scale_fill_manual('Age', values = colors)
      p <- p + ggplot2::theme_minimal() 
      p <- p + ggplot2::labs(title = Title, x = 'Simulation Time', y = 'Mortality level')
      p <- p + theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 11, face = "bold"),
        legend.position = "right"
      )
      
      p
    })
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
  colnames(Fmort) <- c('Time', paste0('Age0', 1:(ncol(Fmort) - 1)))
  
  Nmort <- SpeMort[, c(1, which(colnames(SpeMort) %in% N.mort))]
  colnames(Nmort) <- c('Time', paste0('Age0', 1:(ncol(Nmort) - 1)))
  
  Pmort <- SpeMort[, c(1, which(colnames(SpeMort) %in% P.mort))]
  colnames(Pmort) <- c('Time', paste0('Age0', 1:(ncol(Pmort) - 1)))
  
  Fmort$Mtype <- 'Fishing Mortality'
  Nmort$Mtype <- 'Natural Mortality'
  Pmort$Mtype <- 'Predation Mortality'
  
  return(rbind(Fmort, Nmort, Pmort))
}
