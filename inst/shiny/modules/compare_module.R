## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Compare Outputs Module           ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

compare_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Compare Atlantis Outputs"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(ns("nc_current"), "Current Output (.nc)", 
                 accept = ".nc"),
        fileInput(ns("nc_old"), "Previous Output (.nc) [Optional]", 
                 accept = ".nc"),
        fileInput(ns("grp_csv"), "Groups CSV", 
                 accept = ".csv"),
        fileInput(ns("bgm_file"), "BGM File", 
                 accept = ".bgm"),
        textInput(ns("cum_depths"), "Cumulative Depths (comma-separated, optional)",
                 value = "",
                 placeholder = "Leave blank to read layers from the NetCDF"),
        actionButton(ns("load_data"), "Load Data", 
                    class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          h5("Analysis Options"),
          selectInput(ns("analysis_type"), "Analysis Type",
                     choices = c("Biomass" = "biomass",
                               "Total" = "total",
                               "By AgeClass" = "age")),
          conditionalPanel(
            condition = "input.analysis_type == 'total'",
            ns = ns,
            selectInput(ns("fg_total"), "Functional Group:", choices = NULL),
            checkboxInput(ns("by_polygon"), "By Polygon", FALSE),
            conditionalPanel(
              condition = "input.by_polygon",
              ns = ns,
              selectInput(ns("polygon_n"), "Polygon:", choices = NULL)
            ),
            hr(),
            h6("Variables to Display"),
            checkboxInput(ns("show_biomass"), "Biomass", TRUE),
            checkboxInput(ns("show_numbers"), "Numbers", FALSE),
            checkboxInput(ns("show_struct_n"), "Structural Nitrogen", FALSE),
            checkboxInput(ns("show_reserve_n"), "Reserve Nitrogen", FALSE),
            checkboxInput(ns("scaled"), "Scaled", TRUE),
            checkboxInput(ns("limit_axis"), "Limit Axis", TRUE)
          ),
          conditionalPanel(
            condition = "input.analysis_type == 'age'",
            ns = ns,
            selectInput(ns("fg_age"), "Functional Group:", choices = NULL),
            hr(),
            checkboxInput(ns("show_biomass_age"), "Biomass", TRUE),
            checkboxInput(ns("show_numbers_age"), "Numbers", FALSE),
            checkboxInput(ns("show_struct_n_age"), "Structural Nitrogen", FALSE),
            checkboxInput(ns("show_reserve_n_age"), "Reserve Nitrogen", FALSE),
            checkboxInput(ns("scaled_age"), "Scaled", TRUE),
            checkboxInput(ns("limit_axis_age"), "Limit Axis", TRUE)
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
            " Please load your Atlantis output files to begin analysis."
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

compare_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    rv <- reactiveValues(
      data_loaded = FALSE,
      t_biomass = NULL,
      rel_bio = NULL,
      nc_cur = NULL,
      nc_old = NULL,
      grp = NULL,
      inf_box = NULL,
      Time = NULL,
      Time_old = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$nc_current, input$grp_csv, input$bgm_file, input$cum_depths)
      
      tryCatch({
        showNotification("Loading data...", type = "message", duration = NULL, id = "load_compare")
        
        grp <- read.csv(input$grp_csv$datapath)
        names(grp) <- tolower(names(grp))
        grp <- grp[grp$isturnedon == 1, c('code', 'name', 'longname', 'grouptype', 'numcohorts')]
        
        nc_cur <- ncdf4::nc_open(input$nc_current$datapath)
        Time <- time_calc(nc_cur)
        
        inferred <- infer_cum_depths(nc_cur)
        user_raw <- trimws(input$cum_depths)
        if(nchar(user_raw) == 0){
          if(is.null(inferred)){
            stop("Could not read layer thicknesses from the NetCDF. Enter cumulative depths (surface first, include 0).")
          }
          cum_depths <- inferred
          updateTextInput(session, "cum_depths", value = paste(round(cum_depths, 3), collapse = ","))
        } else {
          cum_depths <- as.numeric(strsplit(user_raw, ",")[[1]])
          if(any(!is.finite(cum_depths))){
            stop("Cumulative depths must be a comma-separated list of numbers.")
          }
          nc_z <- if(!is.null(nc_cur$dim$z)) nc_cur$dim$z$len else NA
          user_n <- n_water_layers(cum_depths)
          if(is.finite(nc_z) && user_n != (nc_z - 1)){
            hint <- if(!is.null(inferred)) paste(round(inferred, 3), collapse = ",") else "the model layer thicknesses"
            stop(paste0("Cumulative depths describe ", user_n, " water layers but the NetCDF has ",
                        nc_z - 1, " water layers plus sediment (", nc_z, " in z). ",
                        "Use depths that match the model, for example: ", hint, "."))
          }
        }
        
        inf_box <- boxes.prop(input$bgm_file$datapath, cum_depths)
        
        vol_rows <- nrow(inf_box$Vol)
        nc_z <- if(!is.null(nc_cur$dim$z)) nc_cur$dim$z$len else NA
        if(is.finite(nc_z) && vol_rows != nc_z){
          stop(paste0("Box volumes have ", vol_rows, " layers but the NetCDF has ", nc_z,
                      ". Check the cumulative depth list."))
        }
        
        nc_old <- NULL
        Time_old <- NULL
        if (!is.null(input$nc_old)) {
          nc_old <- ncdf4::nc_open(input$nc_old$datapath)
          Time_old <- time_calc(nc_old)
        }
        
        mg2t <- 0.00000002
        x.cn <- 5.7
        
        age.grp <- grp[grp$numcohorts > 1 & 
                      !grp$grouptype %in% c('PWN', 'PRAWNS', 'PRAWN', 'CEP', 'MOB_EP_OTHER', 
                                           'SEAGRASS', 'CORAL', 'MANGROVE', 'MANGROVES', 'SPONGE'), ]
        pol.grp <- grp[grp$numcohorts == 1, ]
        pwn.grp <- grp[grp$numcohorts > 1 & 
                      grp$grouptype %in% c('PWN', 'PRAWNS', 'PRAWN', 'CEP', 'MOB_EP_OTHER', 
                                          'SEAGRASS', 'CORAL', 'MANGROVE', 'MANGROVES', 'SPONGE'), ]
        
        if(nrow(age.grp) > 0){
          age.cur.bio <- bio.age(age.grp, nc_cur, 'Current', mg2t, x.cn, inf_box, Time)
        } else {
          age.cur.bio <- NULL
        }
        
        pool.cur.bio <- bio.pool(pol.grp, nc_cur, 'Current', mg2t, x.cn, inf_box, Time)
        
        if (!is.null(nc_old)) {
          if(nrow(age.grp) > 0){
            age.old.bio <- bio.age(age.grp, nc_old, 'Previous', mg2t, x.cn, inf_box, Time_old)
          } else {
            age.old.bio <- NULL
          }
          pool.old.bio <- bio.pool(pol.grp, nc_old, 'Previous', mg2t, x.cn, inf_box, Time_old)
          old.bio <- rbind(pool.old.bio, age.old.bio)
        } else {
          old.bio <- NULL
        }
        
        if(nrow(pwn.grp) > 0){
          pwn.cur.bio <- bio.pwn(pwn.grp, nc_cur, 'Current', mg2t, x.cn, inf_box, Time)
          pwn.bio <- pwn.cur.bio
          if (!is.null(nc_old)) {
            pwn.old.bio <- bio.pwn(pwn.grp, nc_old, 'Previous', mg2t, x.cn, inf_box, Time_old)
            pwn.bio <- rbind(pwn.cur.bio, pwn.old.bio)
          }
        } else {
          pwn.bio <- NULL
        }
        
        t.biomass <- rbind(age.cur.bio, pool.cur.bio, old.bio, pwn.bio)
        
        rel.bio <- by(t.biomass, t.biomass$FG, relative)
        rel.bio <- do.call(rbind.data.frame, rel.bio)
        
        rv$data_loaded <- TRUE
        rv$t_biomass <- t.biomass
        rv$rel_bio <- rel.bio
        rv$nc_cur <- nc_cur
        rv$nc_old <- nc_old
        rv$grp <- grp
        rv$inf_box <- inf_box
        rv$Time <- Time
        rv$Time_old <- Time_old
        
        updateSelectInput(session, "fg_total", choices = as.character(grp$code))
        updateSelectInput(session, "fg_age", 
                         choices = as.character(grp$code[grp$numcohorts > 1 & 
                                                        !grp$grouptype %in% c('PWN', 'PRAWNS', 'PRAWN', 
                                                                             'CEP', 'MOB_EP_OTHER', 'SEAGRASS', 
                                                                             'CORAL', 'MANGROVE', 'MANGROVES', 'SPONGE')]))
        updateSelectInput(session, "polygon_n", choices = inf_box$info$Boxid)
        
        removeNotification("load_compare")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
        
      }, error = function(e) {
        removeNotification("load_compare")
        showNotification(paste("Error loading data:", e$message), type = "error", duration = 10)
      })
    })
    
    output$analysis_output <- renderUI({
      req(rv$data_loaded)
      ns <- session$ns
      
      if (input$analysis_type == "biomass") {
        tagList(
          tabsetPanel(
            tabPanel("Total Biomass",
                    plotOutput(ns("plot_biomass"), height = "800px"),
                    downloadButton(ns("dwn_bio"), "Download Data")
            ),
            tabPanel("Relative Biomass",
                    plotOutput(ns("plot_rel_biomass"), height = "800px"),
                    downloadButton(ns("dwn_rel"), "Download Data")
            )
          )
        )
      } else if (input$analysis_type == "total") {
        plotOutput(ns("plot_total"), height = "600px")
      } else if (input$analysis_type == "age") {
        n.coh <- rv$grp$numcohorts[rv$grp$code == input$fg_age]
        if(length(n.coh) == 0 || is.na(n.coh)) n.coh <- 4
        plot_h <- paste0(max(400, as.integer(ceiling(n.coh / 3) * 280)), "px")
        plotOutput(ns("plot_age"), height = plot_h)
      }
    })
    
    output$plot_biomass <- renderPlot({
      req(rv$t_biomass, rv$Time)
      
      colors <- get_comparison_colors()
      
      p <- ggplot2::ggplot(data = rv$t_biomass, 
                          aes(x = Time, y = Biomass, colour = Simulation)) +
        geom_line(linewidth = 0.8) + 
        facet_wrap(~ FG, ncol = 4, scale = 'free_y') + 
        theme_minimal() +
        scale_color_manual(values = colors) +
        labs(x = 'Date', y = 'Biomass (tons)', title = "Total Biomass by Functional Group") +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 10, face = "bold"),
          legend.position = "top"
        )
      
      p
    })
    
    output$plot_rel_biomass <- renderPlot({
      req(rv$rel_bio, rv$Time)
      
      colors <- get_comparison_colors()
      
      p <- ggplot2::ggplot(data = rv$rel_bio, 
                          aes(x = Time, y = Relative, colour = Simulation)) +
        geom_line(linewidth = 0.8) + 
        facet_wrap(~ FG, ncol = 4) + 
        ylim(0, 2) +
        theme_minimal() +
        scale_color_manual(values = colors) +
        labs(x = 'Date', y = 'Relative Biomass (Bt/B0)', 
            title = "Relative Biomass by Functional Group") +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          strip.text = element_text(size = 10, face = "bold"),
          legend.position = "top"
        )
      
      p
    })
    
    output$plot_total <- renderPlot({
      req(rv$data_loaded, input$fg_total, rv$nc_cur, rv$grp, rv$inf_box, rv$Time)
      
      mg2t <- 0.00000002
      x.cn <- 5.7
      
      total_data <- if(input$by_polygon) {
        nitro.weight(rv$nc_cur, rv$grp, input$fg_total, By = 'Poly', 
                    rv$inf_box, mg2t, x.cn, polnum = (as.numeric(input$polygon_n) + 1))
      } else {
        nitro.weight(rv$nc_cur, rv$grp, input$fg_total, By = 'Total', 
                    rv$inf_box, mg2t, x.cn)
      }
      
      if(input$scaled && !input$by_polygon){
        rmv <- which(sapply(total_data, function(x) length(x) == 0 || is.null(x) || is.character(x)))
        total.tmp <- total_data[-rmv]
        total.tmp <- lapply(total.tmp, function(x) x / x[1])
        total_data[-rmv] <- total.tmp
      }
      
      plot_total_data(total_data, rv$Time, input$show_reserve_n, input$show_struct_n,
                     input$show_numbers, input$show_biomass, input$scaled, 
                     input$limit_axis)
    })
    
    output$plot_age <- renderPlot({
      req(rv$data_loaded, input$fg_age, rv$nc_cur, rv$grp, rv$inf_box, rv$Time)
      
      mg2t <- 0.00000002
      x.cn <- 5.7
      
      coho <- nitro.weight(rv$nc_cur, rv$grp, FG = input$fg_age, By = 'Cohort', 
                          rv$inf_box, mg2t, x.cn)
      
      if(input$scaled_age){
        rmv <- which(sapply(coho, function(x) length(x) == 0 || is.null(x) || is.character(x)))
        coho.tmp <- coho[-rmv]
        coho.tmp <- lapply(coho.tmp, function(x) lapply(x, function(x) x / x[1]))
        coho[-rmv] <- coho.tmp
      }
      
      n.coh <- rv$grp$numcohorts[rv$grp$code == input$fg_age]
      
      plot_cohorts_ggplot(coho, rv$Time, input$show_reserve_n_age, input$show_struct_n_age,
                         input$show_numbers_age, input$show_biomass_age, input$scaled_age,
                         input$limit_axis_age, n.coh)
    })
    
    output$dwn_bio <- downloadHandler(
      filename = function(){
        paste0("biomass_", Sys.Date(), ".csv")
      },
      content = function(file) {
        biom.save <- list(Biomass = lapply(split(rv$t_biomass, paste(rv$t_biomass$FG, rv$t_biomass$Simulation, sep = '_')),
                                          function(x){x$Biomass}))
        out <- to.save.biomass(biom.save, rv$Time)
        write.csv(out, file, row.names = FALSE)
      }
    )
    
    output$dwn_rel <- downloadHandler(
      filename = function(){
        paste0("relative_biomass_", Sys.Date(), ".csv")
      },
      content = function(file) {
        biom.save <- list(Biomass = lapply(split(rv$rel_bio, paste(rv$rel_bio$FG, rv$rel_bio$Simulation, sep = '_')),
                                          function(x){x$Relative}))
        out <- to.save.biomass(biom.save, rv$Time)
        write.csv(out, file, row.names = FALSE)
      }
    )
    
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Helper Functions                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

bio.age <- function(age.grp, nc.out, ctg, mg2t, x.cn, inf.box, Time){
  bound <- ifelse(inf.box$info$Depth_Bound > 0, 1, 0)
  grp.bio <- NULL
  
  for(age in 1:nrow(age.grp)){
    cohort <- NULL
    for(coh in 1:age.grp[age, 'numcohorts']){
      name.fg <- paste0(age.grp$name[age], coh)
      b.coh <- (ncdf4::ncvar_get(nc.out, paste0(name.fg, '_ResN')) +
                ncdf4::ncvar_get(nc.out, paste0(name.fg, '_StructN'))) *
               ncdf4::ncvar_get(nc.out, paste0(name.fg, '_Nums')) * mg2t * x.cn
      b.coh <- colSums(apply(apply(b.coh, 2, colSums, na.rm = TRUE), 1, 
                            function(x) x * bound), na.rm = TRUE)
      cohort <- cbind(cohort, b.coh)
    }
    grp.bio <- rbind(grp.bio, data.frame(
      Time = Time, 
      FG = paste0(as.character(age.grp$longname[age]), ' (', as.character(age.grp$code[age]), ')'),
      Biomass = rowSums(cohort, na.rm = TRUE), 
      Simulation = ctg
    ))
  }
  return(grp.bio)
}

bio.pool <- function(pol.grp, nc.out, ctg, mg2t, x.cn, box.info, Time){
  grp.bio <- NULL
  
  for(pool in 1:nrow(pol.grp)){
    name.fg <- paste0(pol.grp$name[pool], '_N')
    biom <- ncdf4::ncvar_get(nc.out, name.fg)
    
    if(length(dim(biom)) == 3){
      if(pol.grp$grouptype[pool] == 'LG_INF'){
        biom <- apply(biom, 3, '*', box.info$VolInf)
      } else {
        biom <- apply(biom, 3, '*', box.info$Vol)
      }
      biom <- apply(biom, 2, sum, na.rm = TRUE)
    } else {
      biom <- apply(biom, 2, function(x) x * box.info$info$Area)
      biom <- apply(biom, 2, sum, na.rm = TRUE)
    }
    
    biom <- biom * mg2t * x.cn
    biom[1] <- biom[2]
    grp.bio <- rbind(grp.bio, data.frame(
      Time = Time, 
      FG = paste0(as.character(pol.grp$longname[pool]), ' (', as.character(pol.grp$code[pool]), ')'),
      Biomass = biom, 
      Simulation = ctg
    ))
  }
  return(grp.bio)
}

bio.pwn <- function(pwn.grp, nc.out, ctg, mg2t, x.cn, box.info, Time){
  grp.bio <- NULL
  
  for(pwn in 1:nrow(pwn.grp)){
    cohort <- NULL
    for(coh in 1:pwn.grp[pwn, 'numcohorts']){
      name.fg <- paste0(pwn.grp$name[pwn], '_N', coh)
      b.coh <- ncdf4::ncvar_get(nc.out, name.fg) * mg2t * x.cn
      
      if(length(dim(b.coh)) > 2){
        b.coh <- apply(b.coh, 3, '*', box.info$Vol)
      } else {
        b.coh <- apply(b.coh, 2, '*', box.info$info$Area)
      }
      b.coh <- apply(b.coh, 2, sum, na.rm = TRUE)
      cohort <- cbind(cohort, b.coh)
    }
    grp.bio <- rbind(grp.bio, data.frame(
      Time = Time,
      FG = paste0(as.character(pwn.grp$longname[pwn]), ' (', as.character(pwn.grp$code[pwn]), ')'),
      Biomass = rowSums(cohort, na.rm = TRUE), 
      Simulation = ctg
    ))
  }
  return(grp.bio)
}

relative <- function(df, biomass = TRUE, Vec = NULL){
  if(biomass) vector <- df$Biomass
  if(!is.null(Vec)) vector <- df[Vec]
  df$Relative <- vector / vector[1]
  return(df)
}

nitro.weight <- function(nc.out, grp, FG, By = 'Total', box.info, mg2t, x.cn, polnum = NULL){
  pos.fg <- which(grp$code == FG)
  Bio <- Num <- SN <- RN <- list()
  
  if(grp[pos.fg, 'numcohorts'] > 1 & !grp[pos.fg, 'grouptype'] %in% c('PWN', 'PRAWNS', 'PRAWN', 'CEP', 'MOB_EP_OTHER', 'SEAGRASS', 'CORAL', 'MANGROVE', 'MANGROVES', 'SPONGE')){
    n.coh <- grp[pos.fg, 'numcohorts']
    for(coh in 1:n.coh){
      name.fg <- paste0(grp$name[pos.fg], coh)
      nums <- ncdf4::ncvar_get(nc.out, paste0(name.fg, '_Nums'))
      resN <- ncdf4::ncvar_get(nc.out, paste0(name.fg, '_ResN'))
      strN <- ncdf4::ncvar_get(nc.out, paste0(name.fg, '_StructN'))
      
      resN[which(resN <= 1e-8, arr.ind = TRUE)] <- NA
      resN[which(nums <= 1e-8, arr.ind = TRUE)] <- NA
      strN[which(strN <= 1e-8, arr.ind = TRUE)] <- NA
      strN[which(nums <= 1e-8, arr.ind = TRUE)] <- NA
      nums[which(nums <= 1e-8, arr.ind = TRUE)] <- NA
      
      resN[, which(box.info$info$Depth_Bound == 0), ] <- NA
      strN[, which(box.info$info$Depth_Bound == 0), ] <- NA
      nums[, which(box.info$info$Depth_Bound == 0), ] <- NA
      
      if(By == 'Poly'){
        nums <- colSums(nums[, polnum, ], na.rm = TRUE)
        resN <- colSums(resN[, polnum, ], na.rm = TRUE)
        strN <- colSums(strN[, polnum, ], na.rm = TRUE)
      }
      
      b.coh <- (resN + strN) * nums * mg2t * x.cn
      
      if(By %in% c('Total', 'Cohort')){
        nums <- apply(nums, 3, sum, na.rm = TRUE)
        b.coh <- apply(b.coh, 3, sum, na.rm = TRUE)
        strN <- apply(strN, 3, mean, na.rm = TRUE)
        resN <- apply(resN, 3, mean, na.rm = TRUE)
      }
      
      RN[[coh]] <- resN
      SN[[coh]] <- strN
      Bio[[coh]] <- b.coh
      Num[[coh]] <- nums
    }
    
    if(By %in% c('Total', 'Poly')){
      RN <- rowSums(matrix(unlist(RN), ncol = n.coh))
      SN <- rowSums(matrix(unlist(SN), ncol = n.coh))
      Bio <- rowSums(matrix(unlist(Bio), ncol = n.coh))
      Num <- rowSums(matrix(unlist(Num), ncol = n.coh))
    }
    type <- 'AgeClass'
  } else if (grp[pos.fg, 'numcohorts'] == 1){
    name.fg <- paste0(grp$name[pos.fg], '_N')
    biom <- ncdf4::ncvar_get(nc.out, name.fg)
    
    if(length(dim(biom)) == 3){
      if(grp$grouptype[pos.fg] == 'LG_INF'){
        biom <- apply(biom, 3, '*', box.info$VolInf)
      } else {
        biom <- apply(biom, 3, '*', box.info$Vol)
      }
      if(By == 'Total'){
        biom <- apply(biom, 2, sum, na.rm = TRUE)
      }
    } else {
      biom <- apply(biom, 2, function(x) x * box.info$info$Area)
      if(By == 'Total'){
        biom <- apply(biom, 2, sum, na.rm = TRUE)
      }
    }
    Bio <- biom
    type <- 'BioPool'
  } else if(grp[pos.fg, 'numcohorts'] > 1 & grp[pos.fg, 'grouptype'] %in% c('PWN', 'PRAWNS', 'PRAWN', 'CEP', 'MOB_EP_OTHER', 'SEAGRASS', 'CORAL', 'MANGROVE', 'MANGROVES', 'SPONGE')){
    n.coh <- grp[pos.fg, 'numcohorts']
    for(coh in 1:n.coh){
      name.fg <- paste0(grp$name[pos.fg],'_N', coh)
      biom <- ncdf4::ncvar_get(nc.out, name.fg)
      if(length(dim(biom)) > 2){
        biom <- apply(biom, 3, '*', box.info$Vol)
      } else {
        biom <- apply(biom, 2, function(x) x * box.info$info$Area)
      }
      if(By == 'Total'){
        biom <- apply(biom, 2, sum, na.rm = TRUE)
      }
      Bio[[coh]] <- biom
    }
    if(By == 'Total'){
      Bio <- rowSums(matrix(unlist(Bio), ncol = n.coh))
    }
    type <- 'AgeBioPool'
  }
  
  return(list(Biomass = Bio, Numbers = Num, Structural = SN, Reserve = RN, Type = type))
}

plot_total_data <- function(total, Time, rn2, sn2, num2, bio2, scl2, limit2){
  colors <- get_predation_colors()
  
  if(bio2 && !is.null(total$Biomass)){
    bio_df <- data.frame(Time = Time, Value = unlist(total$Biomass), Variable = "Biomass")
    
    p <- ggplot2::ggplot(bio_df, aes(x = Time, y = Value)) +
      geom_line(color = colors[1], linewidth = 1) +
      theme_minimal() +
      labs(x = 'Date', 
          y = if(scl2) 'Relative Values (X_t/X_0)' else 'Biomass (tons)',
          title = "Total Biomass Over Time") +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 11)
      )
    
    if(scl2 && limit2){
      p <- p + ylim(0, 3)
    }
    
    return(p)
  }
  
  plot(Time, rep(1, length(Time)), type = 'n', 
       main = "Select variables to display", xlab = "", ylab = "")
}

plot_cohort <- function(coho, Time, rn3a, sn3a, num3a, bio3a, scl3a, limit3a, coh, max.coh){
  colors <- get_predation_colors()
  
  if(bio3a && !is.null(coho$Biomass)){
    bms <- unlist(coho$Biomass[[coh]])
    
    ylim <- if(limit3a && scl3a) c(0, 3) else range(bms, na.rm = TRUE)
    
    plot(Time, bms, type = 'l', xlab = '', ylab = '', 
         ylim = ylim, bty = 'n', lwd = 2, col = colors[1],
         main = paste0("Cohort ", coh))
    
    if(scl3a){
      mtext(2, text = 'Relative Values (X_t/X_0)', line = 2.5)
    } else {
      mtext(2, text = 'Biomass (tons)', line = 2.5)
    }
  }
}

plot_cohorts_ggplot <- function(coho, Time, rn3a, sn3a, num3a, bio3a, scl3a, limit3a, n.coh){
  colors <- get_predation_colors()
  rows <- list()
  
  add_series <- function(values, cohort, variable){
    vals <- unlist(values)
    if(length(vals) == 0) return()
    rows[[length(rows) + 1]] <<- data.frame(
      Time = Time[seq_along(vals)],
      Value = as.numeric(vals),
      Cohort = paste0("Cohort ", cohort),
      Variable = variable
    )
  }
  
  for(i in 1:n.coh){
    if(isTRUE(bio3a) && !is.null(coho$Biomass) && length(coho$Biomass) >= i){
      add_series(coho$Biomass[[i]], i, "Biomass")
    }
    if(isTRUE(num3a) && !is.null(coho$Numbers) && length(coho$Numbers) >= i){
      add_series(coho$Numbers[[i]], i, "Numbers")
    }
    if(isTRUE(sn3a) && !is.null(coho$Structural) && length(coho$Structural) >= i){
      add_series(coho$Structural[[i]], i, "Structural N")
    }
    if(isTRUE(rn3a) && !is.null(coho$Reserve) && length(coho$Reserve) >= i){
      add_series(coho$Reserve[[i]], i, "Reserve N")
    }
  }
  
  if(length(rows) == 0){
    plot(Time, rep(1, length(Time)), type = 'n',
         main = "Select variables to display", xlab = "", ylab = "")
    return(invisible(NULL))
  }
  
  plot_df <- do.call(rbind, rows)
  ylab <- if(isTRUE(scl3a)) "Relative Values (X_t/X_0)" else "Value"
  
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Time, y = Value, colour = Variable))
  p <- p + ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE)
  p <- p + ggplot2::facet_wrap(~ Cohort, ncol = min(3, n.coh), scales = "free_y")
  p <- p + ggplot2::theme_minimal()
  p <- p + ggplot2::labs(x = "Date", y = ylab, title = "Values by age class")
  p <- p + ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, face = "bold"),
    strip.text = ggplot2::element_text(size = 10, face = "bold"),
    legend.position = "top"
  )
  if(isTRUE(scl3a) && isTRUE(limit3a)){
    p <- p + ggplot2::coord_cartesian(ylim = c(0, 3))
  }
  p
}

to.save.biomass <- function(list_data, Time){
  n.c <- length(list_data$Biomass)
  biom <- matrix(unlist(list_data$Biomass, use.names = FALSE), ncol = n.c, byrow = FALSE)
  out <- cbind(Time, biom)
  colnames(out) <- c('Date', names(list_data$Biomass))
  return(data.frame(out))
}
