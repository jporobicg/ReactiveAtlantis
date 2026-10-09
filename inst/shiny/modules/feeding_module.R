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
        actionButton(ns("load_data"), "Load Data", class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          h5("Matrix Options"),
          selectInput(ns("view_type"), "View Type",
                     choices = c("Availability matrix" = "availability",
                               "Overlap matrix" = "overlap",
                               "Effective predation" = "effective",
                               "Predation pressure" = "pressure")),
          selectInput(ns("predator"), "Predator row:", choices = NULL),
          selectInput(ns("prey"), "Prey column:", choices = NULL)
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
            " Load the biology parameter file, groups CSV, and initial-conditions NetCDF to inspect the pPREY matrix, gape overlap, and effective predation."
          )
        ),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          div(
            style = "overflow: auto; width: 100%;",
            plotOutput(ns("feeding_plot"), height = "800px")
          )
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
      real_feed = NULL,
      pressure = NULL,
      grp = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$prm_file, input$grp_file, input$nc_file)
      
      tryCatch({
        showNotification("Loading feeding matrix data...", type = "message", id = "load_feed", duration = NULL)
        
        grp <- read.csv(input$grp_file$datapath)
        names(grp) <- tolower(names(grp))
        if(any(grepl('invertype', names(grp)))){
          names(grp)[which(grepl('invertype', names(grp)))] <- 'grouptype'
        }
        prm <- readLines(input$prm_file$datapath, warn = FALSE)
        
        options(warn = -1)
        ava_mat <- text2num(prm, 'pPREY', Vector = TRUE, pprey = TRUE)
        options(warn = 0)
        
        if(is.null(ava_mat) || !is.matrix(ava_mat) || nrow(ava_mat) == 0){
          stop("Could not find a pPREY matrix in the parameter file")
        }
        
        n_col <- ncol(ava_mat)
        expected <- c(as.character(grp$code), 'DLsed', 'DRsed', 'DCsed')
        if(n_col == length(expected)){
          colnames(ava_mat) <- expected
        } else if(n_col == length(grp$code)){
          colnames(ava_mat) <- as.character(grp$code)
        } else if(is.null(colnames(ava_mat))){
          colnames(ava_mat) <- paste0("Prey", seq_len(n_col))
        }
        
        bio <- feed_bio_struct(input$nc_file$datapath, grp)
        gape_out <- gape.func(grp, bio$Struct, bio$Biom.N, prm)
        overlap_mat <- Over.mat.func(ava_mat, gape_out[[1]])
        bio.a <- Bio.ages(bio$Biom.N, gape_out[[2]], overlap_mat)
        bio.juv <- bio.a[[1]]
        bio.adl <- bio.a[[2]]
        
        real.feed <- overlap_mat * NA
        pred <- row.names(overlap_mat)
        for(pd in seq_len(nrow(overlap_mat))){
          c.pred <- unlist(strsplit(pred[pd], 'pPREY'))[2]
          predator <- gsub(pattern = "[[:digit:]]+", '\\1', c.pred)
          a.pred.prey <- as.numeric(unlist(strsplit(c.pred, predator)))
          if(length(a.pred.prey) == 0 || is.na(a.pred.prey[2])) a.pred.prey[2] <- 2
          if(a.pred.prey[2] == 1){
            real.feed[pd, ] <- overlap_mat[pd, ] * as.numeric(bio.juv[, 2])
          } else {
            real.feed[pd, ] <- overlap_mat[pd, ] * as.numeric(bio.adl[, 2])
          }
        }
        real.feed <- real.feed * ava_mat
        pressure <- t((real.feed) / rowSums(real.feed, na.rm = TRUE)) * 100
        
        rv$ava_mat <- ava_mat
        rv$overlap_mat <- overlap_mat
        rv$real_feed <- real.feed
        rv$pressure <- pressure
        rv$grp <- grp
        rv$data_loaded <- TRUE
        
        updateSelectInput(session, "predator", choices = rownames(ava_mat))
        updateSelectInput(session, "prey", choices = colnames(ava_mat))
        
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
        plot_feed_tile(t(rv$ava_mat), "Availability matrix (pPREY)",
                      pal = "YlOrRd", legend = "Availability",
                      highlight_pred = input$predator, highlight_prey = input$prey)
      } else if(input$view_type == "overlap"){
        t.o.mat <- t(rv$overlap_mat * rv$ava_mat)
        t.o.mat[t.o.mat > 0] <- 1
        plot_feed_overlap(t.o.mat, input$predator, input$prey)
      } else if(input$view_type == "effective"){
        rff <- log(rv$real_feed)
        rff[!is.finite(rff)] <- NA
        plot_feed_tile(t(rff), "Effective predation (log scale)",
                      pal = "YlGnBu", legend = "Predation\nLn()",
                      highlight_pred = input$predator, highlight_prey = input$prey)
      } else {
        press <- rv$pressure
        press[!is.finite(press) | press == 0] <- NA
        plot_feed_tile(press, "Percentage of predation pressure",
                      pal = "RdPu", legend = "% of diet",
                      highlight_pred = input$predator, highlight_prey = input$prey,
                      limits = c(0, 100))
      }
    }, height = function(){
      req(rv$ava_mat)
      max(600, 18 * nrow(rv$ava_mat) + 160)
    })
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Feeding helpers (from feeding.mat)     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

feed_bio_struct <- function(nc.file, groups.csv){
  nc.out <- ncdf4::nc_open(nc.file)
  on.exit(ncdf4::nc_close(nc.out), add = TRUE)
  
  active <- which(groups.csv$isturnedon == 1)
  is.off <- which(groups.csv$isturnedon == 0)
  FG <- as.character(groups.csv$name)
  TY <- as.character(groups.csv$grouptype)
  Biom.N <- array(data = NA, dim = c(nrow(groups.csv), max(groups.csv$numcohorts)))
  Struct <- Biom.N
  special <- c('PWN', 'PRAWNS', 'PRAWN', 'CEP', 'MOB_EP_OTHER', 'SEAGRASS',
               'CORAL', 'MANGROVE', 'MANGROVES', 'SPONGE')
  
  for(code in active){
    if(TY[code] %in% special && groups.csv$numcohorts[code] > 1){
      sed <- ncdf4::ncatt_get(nc.out, varid = paste0(FG[code], "_N1"), attname = "insed")$value
      unit <- ncdf4::ncatt_get(nc.out, varid = paste0(FG[code], "_N1"), attname = "units")$value
      epi <- ncdf4::ncatt_get(nc.out, varid = paste0(FG[code], "_N1"), attname = "epibenthos")$value == 'epibenthos'
    } else {
      sed <- ncdf4::ncatt_get(nc.out, varid = paste0(FG[code], "_N"), attname = "insed")$value
      unit <- ncdf4::ncatt_get(nc.out, varid = paste0(FG[code], "_N"), attname = "units")$value
      epi <- ncdf4::ncatt_get(nc.out, varid = paste0(FG[code], "_N"), attname = "bmtype")$value == 'epibenthos'
    }
    
    if(groups.csv$numcohorts[code] == 1 || TY[code] %in% special){
      for(coh in 1:groups.csv$numcohorts[code]){
        if(TY[code] %in% special && groups.csv$numcohorts[code] > 1){
          N.tot <- ncdf4::ncvar_get(nc.out, paste0(FG[code], "_N", coh))
        } else {
          N.tot <- ncdf4::ncvar_get(nc.out, paste0(FG[code], "_N"))
        }
        if(all(is.na(N.tot)) || all(N.tot == 0) || sum(N.tot, na.rm = TRUE) == 0){
          water.t <- ncdf4::ncvar_get(nc.out, 'volume')
          w.depth <- ncdf4::ncvar_get(nc.out, 'nominal_dz')
          w.depth[is.na(w.depth)] <- 0
          w.m2 <- colSums(water.t, na.rm = TRUE) / apply(w.depth, 2, function(x) max(x, na.rm = TRUE))
          w.m2[is.infinite(w.m2)] <- NA
          w.m2 <- sum(w.m2, na.rm = TRUE)
          w.m3 <- sum(water.t, na.rm = TRUE)
          if(TY[code] %in% special){
            Biom.N[code, 1] <- ncdf4::ncatt_get(nc.out, varid = paste0(FG[code], "_N", coh), attname = "_FillValue")$value
          } else {
            Biom.N[code, 1] <- ncdf4::ncatt_get(nc.out, varid = paste0(FG[code], "_N"), attname = "_FillValue")$value
          }
          Biom.N[code, 1] <- ifelse(sed == 1 || epi, Biom.N[code, 1] * w.m2, Biom.N[code, 1] * w.m3)
        } else {
          if(length(dim(N.tot)) >= 3){
            N.tot <- N.tot[, , 1]
          } else if(!is.null(unit) && unit == "mg N m-3"){
            water.t <- ncdf4::ncvar_get(nc.out, 'volume')
            N.tot <- N.tot * water.t
          } else if(!is.null(unit) && unit == 'mg N m-2'){
            water.t <- ncdf4::ncvar_get(nc.out, 'volume')
            w.depth <- ncdf4::ncvar_get(nc.out, 'nominal_dz')
            w.depth[is.na(w.depth)] <- 0
            w.m2 <- colSums(water.t, na.rm = TRUE) / apply(w.depth, 2, function(x) max(x, na.rm = TRUE))
            w.m2[is.infinite(w.m2)] <- NA
            N.tot <- sum(N.tot * w.m2, na.rm = TRUE)
          }
          Biom.N[code, coh] <- sum(N.tot, na.rm = TRUE)
        }
      }
    } else if(groups.csv$numcohorts[code] > 1){
      for(cohort in 1:groups.csv$numcohorts[code]){
        StructN <- ncdf4::ncvar_get(nc.out, paste0(FG[code], cohort, "_StructN"))
        if(all(is.na(StructN))){
          StructN <- ncdf4::ncatt_get(nc.out, paste0(FG[code], cohort, "_StructN"), attname = "_FillValue")$value
        }
        ReservN <- ncdf4::ncvar_get(nc.out, paste0(FG[code], cohort, "_ResN"))
        if(all(is.na(ReservN))){
          ReservN <- ncdf4::ncatt_get(nc.out, paste0(FG[code], cohort, "_ResN"), attname = "_FillValue")$value
        }
        Numb <- ncdf4::ncvar_get(nc.out, paste0(FG[code], cohort, "_Nums"))
        if(length(dim(ReservN)) > 2){
          StructN <- StructN[, , 1]
          ReservN <- ReservN[, , 1]
        }
        if(length(dim(Numb)) > 2){
          Numb <- Numb[, , 1]
        }
        Biom.N[code, cohort] <- sum(StructN + ReservN * Numb, na.rm = TRUE)
        Struct[code, cohort] <- max(StructN, na.rm = TRUE)
      }
    }
  }
  
  row.names(Biom.N) <- as.character(groups.csv$code)
  row.names(Struct) <- as.character(groups.csv$code)
  if(length(is.off) > 0){
    Struct <- Struct[-is.off, , drop = FALSE]
    Biom.N <- Biom.N[-is.off, , drop = FALSE]
  }
  list(Struct = Struct, Biom.N = Biom.N)
}

gape.func <- function(groups.csv, Struct, Biom.N, prm){
  KLP <- text2num(prm, 'KLP', FG = as.character(groups.csv$code))
  KUP <- text2num(prm, 'KUP', FG = as.character(groups.csv$code))
  
  KLP <- KLP[complete.cases(KLP), , drop = FALSE]
  KUP <- KUP[complete.cases(KUP), , drop = FALSE]
  KLP <- KLP[!duplicated(KLP$FG), , drop = FALSE]
  KUP <- KUP[!duplicated(KUP$FG), , drop = FALSE]
  
  if(nrow(KLP) == 0 || nrow(KUP) == 0){
    stop("Could not read KLP_ and KUP_ values from the parameter file")
  }
  if(nrow(KUP) != nrow(KLP)){
    if(nrow(KUP) > nrow(KLP)){
      KUP <- KUP[KUP$FG %in% KLP$FG, , drop = FALSE]
    } else {
      KLP <- KLP[KLP$FG %in% KUP$FG, , drop = FALSE]
    }
  }
  
  age <- text2num(prm, '_age_mat', FG = as.character(groups.csv$code))
  names(age) <- c('FG', 'Age.Adult')
  Gape <- data.frame(FG = as.factor(KLP$FG), KLP = KLP$Value, KUP = KUP$Value,
                     adult.Min = NA, adult.Max = NA, juv.Min = NA,
                     juv.Max = NA, JminS = NA, JmaxS = NA, AminS = NA, AmaxS = NA)
  Gape <- dplyr::left_join(Gape, age, by = 'FG')
  Gape$Age.Young <- Gape$Age.Adult - 1
  Gape$Age.Young <- ifelse(Gape$Age.Young == 0, 1, Gape$Age.Young)
  Biom.N <- Biom.N[order(row.names(Biom.N)), , drop = FALSE]
  Struct <- data.frame(as.factor(row.names(Struct)), Struct)
  names(Struct) <- c('FG', paste0('Age_', 1:(ncol(Struct) - 1)))
  Gape <- dplyr::left_join(Gape, Struct, by = 'FG')
  ages.pos <- grep('Age_', colnames(Gape))
  Gape$juv.Min <- Gape[, ages.pos[1]] * Gape$KLP
  for(i in 1:nrow(Gape)){
    if(all(is.na(Gape[i, ages.pos]))) next()
    Gape$adult.Min[i] <- Gape[i, ages.pos[Gape$Age.Adult[i]]] * Gape$KLP[i]
    Gape$adult.Max[i] <- Gape[i, ages.pos[sum(!is.na(Gape[i, ages.pos]))]] * Gape$KUP[i]
    Gape$juv.Max[i] <- Gape[i, ages.pos[Gape$Age.Young[i]]] * Gape$KLP[i]
    Gape$JminS[i] <- Gape[i, ages.pos[1]]
    Gape$AminS[i] <- Gape[i, ages.pos[Gape$Age.Adult[i]]]
    Gape$JmaxS[i] <- Gape[i, ages.pos[Gape$Age.Young[i]]]
    Gape$AmaxS[i] <- Gape[i, ages.pos[sum(!is.na(Gape[i, ages.pos]))]]
  }
  list(Gape, age)
}

Over.mat.func <- function(Ava.mat, Gape){
  Over.mat <- Ava.mat * 0
  Prey <- colnames(Ava.mat)
  Pred <- row.names(Ava.mat)
  for(py in 1:length(Prey)){
    for(pd in 1:length(Pred)){
      c.pred <- unlist(strsplit(Pred[pd], 'pPREY'))[2]
      predator <- gsub(pattern = "[[:digit:]]+", '\\1', c.pred)
      a.pred.prey <- as.numeric(unlist(strsplit(c.pred, predator)))
      pry.loc <- which(Gape$FG %in% Prey[py])
      prd.loc <- which(Gape$FG %in% predator)
      if(length(pry.loc) == 0 || is.na(a.pred.prey[1])){
        Over.mat[pd, py] <- 1
      } else {
        if(a.pred.prey[1] == 1){
          if(!is.na(a.pred.prey[2]) && a.pred.prey[2] == 1){
            Over.mat[pd, py] <- ifelse(Gape$JminS[pry.loc] >= Gape$juv.Min[prd.loc],
                                ifelse(Gape$JminS[pry.loc] <= Gape$juv.Max[prd.loc], 1, 0),
                                ifelse(Gape$JmaxS[pry.loc] >= Gape$juv.Min[prd.loc],
                                ifelse(Gape$JmaxS[pry.loc] <= Gape$juv.Max[prd.loc], 1, 1), 0))
            if(is.na(Over.mat[pd, py])) Over.mat[pd, py] <- 1
          } else {
            Over.mat[pd, py] <- ifelse(Gape$AminS[pry.loc] >= Gape$juv.Min[prd.loc],
                                ifelse(Gape$AminS[pry.loc] <= Gape$juv.Max[prd.loc], 1, 0),
                                ifelse(Gape$AmaxS[pry.loc] >= Gape$juv.Min[prd.loc],
                                ifelse(Gape$AmaxS[pry.loc] <= Gape$juv.Max[prd.loc], 1, 1), 0))
            if(is.na(Over.mat[pd, py])) Over.mat[pd, py] <- 1
          }
        } else {
          if(!is.na(a.pred.prey[2]) && a.pred.prey[2] == 1){
            Over.mat[pd, py] <- ifelse(Gape$JminS[pry.loc] >= Gape$adult.Min[prd.loc],
                                ifelse(Gape$JminS[pry.loc] <= Gape$adult.Max[prd.loc], 1, 0),
                                ifelse(Gape$JmaxS[pry.loc] >= Gape$adult.Min[prd.loc],
                                ifelse(Gape$JmaxS[pry.loc] <= Gape$adult.Max[prd.loc], 1, 1), 0))
            if(is.na(Over.mat[pd, py])) Over.mat[pd, py] <- 1
          } else {
            Over.mat[pd, py] <- ifelse(Gape$AminS[pry.loc] >= Gape$adult.Min[prd.loc],
                                ifelse(Gape$AminS[pry.loc] <= Gape$adult.Max[prd.loc], 1, 0),
                                ifelse(Gape$AmaxS[pry.loc] >= Gape$adult.Min[prd.loc],
                                ifelse(Gape$AmaxS[pry.loc] <= Gape$adult.Max[prd.loc], 1, 1), 0))
            if(is.na(Over.mat[pd, py])) Over.mat[pd, py] <- 1
          }
        }
      }
    }
  }
  Over.mat
}

Bio.ages <- function(Biom.N, age, Over.mat){
  Biom.N <- Biom.N[order(row.names(Biom.N)), , drop = FALSE]
  fg <- row.names(Biom.N)
  bio.juv <- bio.adl <- matrix(NA, ncol = 2, nrow = nrow(Biom.N))
  for(i in 1:nrow(Biom.N)){
    l.age <- which(age$FG == fg[i])
    if(length(l.age) != 0){
      young_end <- max(1, age$Age.Adult[l.age] - 1)
      bio.juv[i, ] <- c(fg[i], sum(Biom.N[i, 1:young_end], na.rm = TRUE))
      bio.adl[i, ] <- c(fg[i], sum(Biom.N[i, age$Age.Adult[l.age]:ncol(Biom.N)], na.rm = TRUE))
    } else {
      bio.juv[i, ] <- c(fg[i], sum(Biom.N[i, 1], na.rm = TRUE))
      bio.adl[i, ] <- c(fg[i], sum(Biom.N[i, 1], na.rm = TRUE))
    }
  }
  or.prey <- match(colnames(Over.mat), bio.juv[, 1])
  bio.juv <- bio.juv[or.prey, , drop = FALSE]
  bio.adl <- bio.adl[or.prey, , drop = FALSE]
  list(bio.juv, bio.adl)
}

plot_feed_tile <- function(mat, title, pal, legend, highlight_pred, highlight_prey, limits = NULL){
  dat <- reshape2::melt(mat, value.name = 'value')
  dat$value[dat$value == 0] <- NA
  if(is.null(limits)){
    limits <- c(0, max(dat$value, na.rm = TRUE))
    if(!is.finite(limits[2]) || limits[2] == 0) limits[2] <- 1
  }
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = .data$value))
  p <- p + ggplot2::geom_tile(colour = "grey45", linewidth = 0.15)
  p <- p + ggplot2::scale_fill_distiller(palette = pal, limits = limits,
                                         name = legend, na.value = 'white', direction = 1)
  p <- p + ggplot2::theme(panel.background = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 7),
                          axis.text.y = ggplot2::element_text(size = 7))
  p <- p + ggplot2::labs(x = 'Prey', y = 'Predator', title = title)
  p <- p + ggplot2::scale_x_discrete(position = "top")
  if(!is.null(highlight_pred) && !is.null(highlight_prey)){
    p <- p + ggplot2::annotate("rect",
                               xmin = which(levels(factor(dat$Var1)) == highlight_prey) - 0.5,
                               xmax = which(levels(factor(dat$Var1)) == highlight_prey) + 0.5,
                               ymin = 0, ymax = length(unique(dat$Var2)) + 1,
                               alpha = 0.08, colour = 'goldenrod')
  }
  p
}

plot_feed_overlap <- function(t.o.mat, highlight_pred, highlight_prey){
  col.tmp <- RColorBrewer::brewer.pal(11, 'RdBu')[c(6, 11)]
  dat2 <- reshape2::melt(t.o.mat, value.name = 'value')
  p <- ggplot2::ggplot(dat2, ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = factor(.data$value)))
  p <- p + ggplot2::geom_tile(colour = 'grey45')
  p <- p + ggplot2::theme(panel.background = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 7),
                          axis.text.y = ggplot2::element_text(size = 7))
  p <- p + ggplot2::labs(x = 'Prey', y = 'Predator', title = "Gape overlap")
  p <- p + ggplot2::scale_x_discrete(position = "top")
  p <- p + ggplot2::scale_fill_manual(values = col.tmp, name = 'Gape overlap',
                                      labels = c('No', 'Yes'), na.value = 'white')
  p
}
