## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~        Feeding Matrix Analysis Module      ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

feeding_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Predator-Prey Feeding Matrix"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(ns("prm_file"), "Parameters File (.prm)", accept = ".prm"),
        fileInput(ns("grp_file"), "Groups CSV", accept = ".csv"),
        fileInput(ns("nc_file"), "Initial Conditions (.nc)", accept = ".nc"),
        fileInput(ns("bgm_file"), "BGM File", accept = ".bgm"),
        textInput(ns("cum_depths"), "Cumulative Depths (comma-separated)",
                 value = "0,20,50,150,250,400,650,1000,4300"),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          h5("Matrix Options"),
          selectInput(ns("view_type"), "View Type",
                     choices = c("Availability Matrix" = "availability",
                               "Overlap Matrix" = "overlap",
                               "Effective Predation" = "effective")),
          selectInput(ns("predator"), "Predator:", choices = NULL),
          selectInput(ns("prey"), "Prey:", choices = NULL)
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
            " Please load your feeding matrix files to explore predator-prey availability and overlap."
          )
        ),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          plotOutput(ns("feeding_plot"), height = "700px")
        )
      )
    )
  )
}

feeding_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      data_loaded = FALSE,
      ava_mat = NULL,
      overlap_mat = NULL,
      biomass = NULL,
      grp = NULL,
      predators = NULL,
      prey_list = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$prm_file, input$grp_file, input$nc_file, input$bgm_file, input$cum_depths)
      
      tryCatch({
        showNotification("Loading feeding matrix data...", type = "message", id = "load_feed", duration = NULL)
        
        grp <- read.csv(input$grp_file$datapath)
        names(grp) <- tolower(names(grp))
        prm <- readLines(input$prm_file$datapath, warn = FALSE)
        nc.file <- ncdf4::nc_open(input$nc_file$datapath)
        
        predators <- grp[grp$ispredator > 0, ]
        
        options(warn = -1)
        pprey <- text2num(prm, '^pPREY', FG = 'look', Vector = TRUE, pprey = TRUE)
        options(warn = 0)
        
        if(is.null(pprey) || nrow(pprey) == 0){
          stop("Could not find pprey matrix in parameters file")
        }
        
        ava_mat <- pprey
        row_names <- rownames(ava_mat)
        col_names <- colnames(ava_mat)
        
        KUP <- KLP <- NULL
        for(i in 1:nrow(predators)){
          fg <- as.character(predators$code[i])
          kup_vals <- text2num(prm, paste0('KUP_', fg), FG = fg, Vector = TRUE)
          klp_vals <- text2num(prm, paste0('KLP_', fg), FG = fg, Vector = TRUE)
          KUP <- rbind(KUP, kup_vals)
          KLP <- rbind(KLP, klp_vals)
        }
        
        overlap_mat <- matrix(1, nrow = nrow(ava_mat), ncol = ncol(ava_mat))
        rownames(overlap_mat) <- row_names
        colnames(overlap_mat) <- col_names
        
        biomass <- list()
        for(i in 1:nrow(grp)){
          if(grp$isturnedon[i] == 0) next
          
          tryCatch({
            if(grp$numcohorts[i] > 1){
              bio_tmp <- NULL
              for(coh in 1:grp$numcohorts[i]){
                name_fg <- paste0(grp$name[i], coh)
                nums <- ncdf4::ncvar_get(nc.file, paste0(name_fg, '_Nums'))
                resn <- ncdf4::ncvar_get(nc.file, paste0(name_fg, '_ResN'))
                strn <- ncdf4::ncvar_get(nc.file, paste0(name_fg, '_StructN'))
                bio_coh <- (resn + strn) * nums
                bio_tmp <- c(bio_tmp, sum(bio_coh, na.rm = TRUE))
              }
              biomass[[as.character(grp$code[i])]] <- sum(bio_tmp, na.rm = TRUE)
            } else {
              name_fg <- paste0(grp$name[i], '_N')
              biom <- ncdf4::ncvar_get(nc.file, name_fg)
              biomass[[as.character(grp$code[i])]] <- sum(biom, na.rm = TRUE)
            }
          }, error = function(e) NULL)
        }
        
        rv$ava_mat <- ava_mat
        rv$overlap_mat <- overlap_mat
        rv$biomass <- biomass
        rv$grp <- grp
        rv$predators <- predators
        rv$prey_list <- col_names
        rv$data_loaded <- TRUE
        
        updateSelectInput(session, "predator", choices = as.character(predators$code))
        updateSelectInput(session, "prey", choices = col_names)
        
        removeNotification("load_feed")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("load_feed")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    output$feeding_plot <- renderPlot({
      req(rv$data_loaded, input$view_type)
      
      if(input$view_type == "availability"){
        plot_matrix(rv$ava_mat, "Availability Matrix (pprey)", 
                   input$predator, input$prey)
      } else if(input$view_type == "overlap"){
        plot_matrix(rv$overlap_mat, "Gape Overlap Matrix", 
                   input$predator, input$prey)
      } else if(input$view_type == "effective"){
        eff_mat <- rv$ava_mat * rv$overlap_mat
        
        for(prey in names(rv$biomass)){
          if(prey %in% colnames(eff_mat)){
            eff_mat[, prey] <- eff_mat[, prey] * rv$biomass[[prey]]
          }
        }
        
        eff_mat[eff_mat == 0] <- NA
        eff_mat_log <- log(eff_mat + 1e-10)
        eff_mat_log[!is.finite(eff_mat_log)] <- NA
        
        plot_matrix(eff_mat_log, "Effective Predation (log scale)", 
                   input$predator, input$prey, is_log = TRUE)
      }
    })
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Helper Functions                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

plot_matrix <- function(mat, title, highlight_pred = NULL, highlight_prey = NULL, is_log = FALSE){
  n_pred <- min(nrow(mat), 30)
  n_prey <- min(ncol(mat), 30)
  
  mat_subset <- mat[1:n_pred, 1:n_prey]
  
  mat_subset[!is.finite(mat_subset)] <- NA
  
  colors <- colorRampPalette(c("white", "lightblue", "blue", "darkblue"))(100)
  
  par(mar = c(8, 8, 3, 2))
  
  image(1:n_prey, 1:n_pred, t(mat_subset), 
        col = colors,
        xlab = "", ylab = "", 
        axes = FALSE,
        main = title)
  
  axis(1, at = 1:n_prey, labels = colnames(mat_subset), las = 2, cex.axis = 0.7)
  axis(2, at = 1:n_pred, labels = rownames(mat_subset), las = 1, cex.axis = 0.7)
  
  mtext("Prey", side = 1, line = 6, font = 2)
  mtext("Predator", side = 2, line = 6, font = 2)
  
  if(!is.null(highlight_pred) && !is.null(highlight_prey)){
    pred_idx <- which(rownames(mat_subset) == highlight_pred)
    prey_idx <- which(colnames(mat_subset) == highlight_prey)
    
    if(length(pred_idx) > 0 && length(prey_idx) > 0){
      points(prey_idx, pred_idx, pch = 21, col = "red", bg = "yellow", cex = 2, lwd = 2)
    }
  }
}
