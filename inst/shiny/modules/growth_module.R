## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   Primary Producer Growth Analysis Module  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

growth_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Growth Limitation Analysis for Primary Producers"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(ns("ini_nc"), "Initial Conditions (.nc)", accept = ".nc"),
        fileInput(ns("grp_file"), "Groups CSV", accept = ".csv"),
        fileInput(ns("prm_file"), "Parameters File (.prm)", accept = ".prm"),
        fileInput(ns("out_nc"), "Output (.nc)", accept = ".nc"),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = sprintf("output['%s']", ns("data_loaded")),
          ns = ns,
          h5("Analysis Options"),
          selectInput(ns("fg"), "Primary Producer:", choices = NULL),
          selectInput(ns("box"), "Box:", choices = NULL),
          checkboxInput(ns("log_scale"), "Logarithmic", FALSE)
        )
      ),
      
      mainPanel(
        width = 9,
        conditionalPanel(
          condition = sprintf("!output['%s']", ns("data_loaded")),
          ns = ns,
          div(
            class = "alert alert-info",
            style = "margin-top: 50px;",
            icon("info-circle"),
            " Please load your growth analysis files to analyze limitation factors (light, nutrients, eddies)."
          )
        ),
        conditionalPanel(
          condition = sprintf("output['%s']", ns("data_loaded")),
          ns = ns,
          plotOutput(ns("growth_plot"), height = "800px")
        )
      )
    )
  )
}

growth_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      data_loaded = FALSE,
      l_nut = NULL,
      l_light = NULL,
      l_eddy = NULL,
      cod_fg = NULL,
      n_box = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$ini_nc, input$grp_file, input$prm_file, input$out_nc)
      
      tryCatch({
        showNotification("Loading growth data...", type = "message", id = "load_growth", duration = NULL)
        
        nc.out <- ncdf4::nc_open(input$out_nc$datapath)
        grp <- read.csv(input$grp_file$datapath)
        names(grp) <- tolower(names(grp))
        prm <- readLines(input$prm_file$datapath, warn = FALSE)
        
        pp.grp <- with(grp, which(grouptype %in% c('PHYTOBEN', 'SM_PHY', 'LG_PHY', 'SEAGRASS', 'DINOFLAG', 'TURF','MICROPHTYBENTHOS') & isturnedon == 1))
        if(length(pp.grp) == 0){
          stop("No primary producers found in groups file")
        }
        
        cod.fg <- as.character(grp$code[pp.grp])
        nam.fg <- as.character(grp$name[pp.grp])
        
        options(warn = -1)
        flagnut <- text2num(prm, 'flagnut ', FG = 'look')
        if(nrow(flagnut) == 0) flagnut <- data.frame(FG = 'flagnut', Value = 0)
        
        KN <- KS <- KI <- NULL
        for(i in 1:length(pp.grp)) {
          KN <- rbind(KN, text2num(prm, paste0('KN_', cod.fg[i]), FG = cod.fg[i]))
          KS <- rbind(KS, text2num(prm, paste0('KS_', cod.fg[i]), FG = cod.fg[i]))
          KI <- rbind(KI, text2num(prm, paste0('KI_', cod.fg[i]), FG = cod.fg[i]))
        }
        
        ed.scl <- text2num(prm, 'eddy_scale', FG = 'look')
        if(nrow(ed.scl) > 0) ed.scl <- ed.scl[1, 2] else ed.scl <- 1
        options(warn = 0)
        
        DIN <- ncdf4::ncvar_get(nc.out, 'NO3') + ncdf4::ncvar_get(nc.out, 'NH3')
        Si <- ncdf4::ncvar_get(nc.out, 'Si')
        l.eddy <- ncdf4::ncvar_get(nc.out, 'eddy') * ed.scl
        IRR <- ncdf4::ncvar_get(nc.out, 'Light')
        
        l.light <- l.nut <- list()
        
        for(fg in 1:length(nam.fg)){
          if(flagnut$Value[1] == 0){
            l.nut[[fg]] <- pmin((DIN / (KN$Value[fg] + DIN)), (Si / (KS$Value[fg] + Si)), na.rm = TRUE)
          } else if(flagnut$Value[1] == 1){
            l.nut[[fg]] <- sqrt((DIN / (KN$Value[fg] + DIN)) * (Si / (KS$Value[fg] + Si)))
          } else if(flagnut$Value[1] == 2){
            l.nut[[fg]] <- 2 / ((DIN / (KN$Value[fg] + DIN)) + (Si / (KS$Value[fg] + Si)))
          } else {
            l.nut[[fg]] <- (DIN / (KN$Value[fg] + DIN))
          }
          
          l.light[[fg]] <- pmin((IRR / KI$Value[fg]), 1, na.rm = TRUE)
        }
        
        n.box <- dim(IRR)[2]
        
        rv$l_nut <- l.nut
        rv$l_light <- l.light
        rv$l_eddy <- l.eddy
        rv$cod_fg <- cod.fg
        rv$n_box <- n.box
        rv$data_loaded <- TRUE
        
        updateSelectInput(session, "fg", choices = cod.fg)
        updateSelectInput(session, "box", choices = 0:(n.box - 1))
        
        removeNotification("load_growth")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("load_growth")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    output$growth_plot <- renderPlot({
      req(rv$data_loaded, input$fg, input$box)
      
      fg_idx <- which(rv$cod_fg == input$fg)
      box_idx <- as.numeric(input$box) + 1
      
      if(length(fg_idx) == 0 || box_idx > rv$n_box) return()
      
      nut_data <- rv$l_nut[[fg_idx]]
      light_data <- rv$l_light[[fg_idx]]
      eddy_data <- rv$l_eddy
      
      n_layers <- dim(nut_data)[1]
      n_time <- dim(nut_data)[3]
      
      colors <- get_pp_colors(4)
      
      par(mfrow = c(min(n_layers, 4), 1), mar = c(3, 4, 2, 2))
      
      for(layer in 1:min(n_layers, 4)){
        nut_layer <- nut_data[layer, box_idx, ]
        light_layer <- light_data[layer, box_idx, ]
        eddy_layer <- eddy_data[box_idx, ]
        
        growth_layer <- nut_layer * light_layer
        
        if(input$log_scale){
          nut_layer <- log(nut_layer + 1e-10)
          light_layer <- log(light_layer + 1e-10)
          eddy_layer <- log(eddy_layer + 1e-10)
          growth_layer <- log(growth_layer + 1e-10)
        }
        
        plot(1:n_time, growth_layer, type = 'l', col = colors[1], lwd = 2,
             ylim = range(c(growth_layer, nut_layer, light_layer), na.rm = TRUE),
             xlab = if(layer == min(n_layers, 4)) "Time step" else "",
             ylab = if(input$log_scale) "log(Limitation)" else "Limitation",
             main = paste(input$fg, "- Layer", layer - 1, "- Box", input$box))
        
        lines(1:n_time, nut_layer, col = colors[2], lwd = 2)
        lines(1:n_time, light_layer, col = colors[3], lwd = 2)
        
        if(layer == 1){
          lines(1:n_time, rep(mean(eddy_layer, na.rm = TRUE), n_time), 
                col = colors[4], lwd = 2, lty = 2)
        }
        
        if(layer == 1){
          legend("topright", c("Growth", "Nutrients", "Light", "Eddy"), 
                 col = colors, lwd = 2, lty = c(1, 1, 1, 2), bty = 'n', cex = 0.8)
        }
      }
    })
  })
}
