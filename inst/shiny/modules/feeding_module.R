## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~        Feeding Matrix Analysis Module      ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

feeding_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("view_type"), "View",
                    choices = c("Availability (pPREY)" = "availability",
                                "Gape overlap" = "overlap",
                                "Gape mismatch (pPREY>0, overlap=0)" = "mismatch",
                                "Effective predation" = "effective",
                                "Predation pressure" = "pressure")),
        textInput(ns("pred_search"), "Predator search", placeholder = "pPREY1COD…"),
        textInput(ns("prey_search"), "Prey search", placeholder = "HAD, COD…"),
        selectInput(ns("predator"), "Predator row", choices = "(all)"),
        selectInput(ns("prey"), "Prey column", choices = "(all)"),
        p(class = "ra-muted",
          "Choose a predator row to filter the matrix and to open a bar chart of availability, gape overlap and effective predation. Cells outlined in navy have pPREY > 0 with zero gape overlap.")
      ),
      plotOutput(ns("feeding_plot_gg"), height = "640px"),
      plotOutput(ns("detail_plot"), height = "280px"),
      DT::dataTableOutput(ns("bug_table"))
    )
  )
}

feeding_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      ava_mat = NULL, overlap_mat = NULL, real_feed = NULL,
      pressure = NULL, bugs = NULL, built = FALSE, token = NULL
    )

    output$banner <- renderUI(tool_banner(project, "feeding"))

    observe({
      req(rv$ava_mat)
      update_select_keep(session, "predator", c("(all)", rownames(rv$ava_mat)),
                         input$predator)
      update_select_keep(session, "prey", c("(all)", colnames(rv$ava_mat)),
                         input$prey)
    })

    observe({
      req(!is.null(project), isTRUE(project$ready), project$data)
      miss <- tool_missing(project$paths, "feeding", project)
      token <- paste(unlist(project$paths), collapse = "|")
      if (length(miss) || (isTRUE(rv$built) && identical(rv$token, token))) return()
      rv$token <- token
      dat <- project$data
      req(dat$grp, dat$prm_bio)
      paths <- tryCatch(normalize_paths(project$paths), error = function(e) empty_paths())
      if (!is.null(dat$paths)) {
        dp <- tryCatch(normalize_paths(dat$paths), error = function(e) NULL)
        if (!is.null(dp) && path_ok(dp$init_nc)) paths$init_nc <- dp$init_nc
      }
      init_path <- paths$init_nc
      grp <- dat$grp
      if (any(grepl("invertype", names(grp)))) {
        names(grp)[which(grepl("invertype", names(grp)))] <- "grouptype"
      }
      prm <- dat$prm_bio
      options(warn = -1)
      ava_mat <- text2num(prm, "pPREY", Vector = TRUE, pprey = TRUE)
      options(warn = 0)
      if (is.null(ava_mat) || !is.matrix(ava_mat) || nrow(ava_mat) == 0) {
        shiny::showNotification("Could not find a pPREY matrix in the biology parameter file.",
                                type = "error", duration = 10)
        return()
      }
      n_col <- ncol(ava_mat)
      expected <- c(as.character(grp$code), "DLsed", "DRsed", "DCsed")
      if (n_col == length(expected)) {
        colnames(ava_mat) <- expected
      } else if (n_col == length(grp$code)) {
        colnames(ava_mat) <- as.character(grp$code)
      } else if (is.null(colnames(ava_mat))) {
        colnames(ava_mat) <- paste0("Prey", seq_len(n_col))
      }

      built <- tryCatch({
        bio <- feed_bio_struct(init_path, grp)
        gape_out <- gape.func(grp, bio$Struct, bio$Biom.N, prm)
        overlap_mat <- Over.mat.func(ava_mat, gape_out[[1]])
        bio.a <- Bio.ages(bio$Biom.N, gape_out[[2]], overlap_mat)
        bio.juv <- bio.a[[1]]
        bio.adl <- bio.a[[2]]
        real.feed <- overlap_mat * NA
        pred <- row.names(overlap_mat)
        for (pd in seq_len(nrow(overlap_mat))) {
          c.pred <- unlist(strsplit(pred[pd], "pPREY"))[2]
          predator <- gsub(pattern = "[[:digit:]]+", "\\1", c.pred)
          a.pred.prey <- as.numeric(unlist(strsplit(c.pred, predator)))
          if (length(a.pred.prey) == 0 || is.na(a.pred.prey[2])) a.pred.prey[2] <- 2
          if (a.pred.prey[2] == 1) {
            real.feed[pd, ] <- overlap_mat[pd, ] * as.numeric(bio.juv[, 2])
          } else {
            real.feed[pd, ] <- overlap_mat[pd, ] * as.numeric(bio.adl[, 2])
          }
        }
        real.feed <- real.feed * ava_mat
        list(overlap = overlap_mat, real = real.feed,
             pressure = predation_pressure_matrix(real.feed))
      }, error = function(e) {
        shiny::showNotification(paste("Feeding matrix:", e$message), type = "error", duration = 10)
        NULL
      })
      rv$ava_mat <- ava_mat
      if (!is.null(built)) {
        rv$overlap_mat <- built$overlap
        rv$real_feed <- built$real
        rv$pressure <- built$pressure
        rv$bugs <- gape_mismatch_table(ava_mat, built$overlap)
      } else {
        ## Still render predation pressure from pPREY if gape overlap fails
        rv$pressure <- predation_pressure_matrix(ava_mat)
      }
      rv$built <- TRUE
    })

    filtered_mats <- reactive({
      req(rv$ava_mat)
      ava <- rv$ava_mat
      pred_keep <- seq_len(nrow(ava))
      prey_keep <- seq_len(ncol(ava))
      if (!is.null(input$pred_search) && nzchar(trimws(input$pred_search))) {
        pred_keep <- grep(trimws(input$pred_search), rownames(ava), ignore.case = TRUE)
      }
      if (!is.null(input$prey_search) && nzchar(trimws(input$prey_search))) {
        prey_keep <- grep(trimws(input$prey_search), colnames(ava), ignore.case = TRUE)
      }
      if (!is.null(input$predator) && nzchar(input$predator) && input$predator != "(all)") {
        hit <- which(rownames(ava) == input$predator)
        if (length(hit)) pred_keep <- intersect(pred_keep, hit)
      }
      if (!is.null(input$prey) && nzchar(input$prey) && input$prey != "(all)") {
        hit <- which(colnames(ava) == input$prey)
        if (length(hit)) prey_keep <- intersect(prey_keep, hit)
      }
      if (length(pred_keep) == 0) pred_keep <- seq_len(nrow(ava))
      if (length(prey_keep) == 0) prey_keep <- seq_len(ncol(ava))
      list(
        ava = ava[pred_keep, prey_keep, drop = FALSE],
        overlap = if (is.null(rv$overlap_mat)) NULL else rv$overlap_mat[pred_keep, prey_keep, drop = FALSE],
        real = if (is.null(rv$real_feed)) NULL else rv$real_feed[pred_keep, prey_keep, drop = FALSE],
        pressure = if (is.null(rv$pressure)) NULL else {
          rv$pressure[prey_keep, pred_keep, drop = FALSE]
        }
      )
    })

    make_feed_plot <- function() {
      mats <- filtered_mats()
      req(mats$ava, input$view_type)
      bugs <- if (is.null(mats$overlap)) NULL else gape_mismatch(mats$ava, mats$overlap)
      hl_pred <- if (is.null(input$predator) || input$predator %in% c("", "(all)")) NULL else input$predator
      hl_prey <- if (is.null(input$prey) || input$prey %in% c("", "(all)")) NULL else input$prey
      if (input$view_type == "availability") {
        plot_feed_tile(t(mats$ava), "Availability matrix (pPREY)",
                       pal = "YlOrRd", legend = "Availability",
                       highlight_pred = hl_pred, highlight_prey = hl_prey,
                       mismatch = if (is.null(bugs)) NULL else t(bugs))
      } else if (input$view_type == "overlap") {
        t.o.mat <- t(mats$overlap * mats$ava)
        t.o.mat[t.o.mat > 0] <- 1
        plot_feed_overlap(t.o.mat, hl_pred, hl_prey)
      } else if (input$view_type == "mismatch") {
        if (is.null(bugs)) {
          return(ggplot2::ggplot() + ggplot2::labs(title = "Gape overlap was not calculated") +
                   theme_reactiveatlantis())
        }
        plot_feed_mismatch(t(bugs))
      } else if (input$view_type == "effective") {
        rff <- log(mats$real)
        rff[!is.finite(rff)] <- NA
        plot_feed_tile(t(rff), "Effective predation (log scale)",
                       pal = "YlGnBu", legend = "Predation\nLn()",
                       highlight_pred = hl_pred, highlight_prey = hl_prey,
                       mismatch = if (is.null(bugs)) NULL else t(bugs))
      } else {
        press <- mats$pressure
        if (is.null(press) || !is.matrix(press)) {
          return(ggplot2::ggplot() +
                   ggplot2::labs(title = "Predation pressure needs gape overlap (initial.nc + KLP/KUP)") +
                   theme_reactiveatlantis())
        }
        press[!is.finite(press) | press == 0] <- NA
        plot_feed_tile(press, "Percentage of predation pressure",
                       pal = "RdPu", legend = "% of diet",
                       highlight_pred = hl_pred, highlight_prey = hl_prey,
                       limits = c(0, 100))
      }
    }

    output$feeding_plot_gg <- renderPlot({
      plot_or_message("Load biology.prm (and initial.nc) to plot the feeding matrix",
                      make_feed_plot())
    })

    output$detail_plot <- renderPlot({
      plot_or_message("Select a predator row for the detail chart", {
      req(rv$ava_mat)
      pred <- input$predator
      if (is.null(pred) || pred %in% c("", "(all)")) {
        plot.new()
        title("Select a predator row for availability, gape overlap and effective predation")
        return(invisible(NULL))
      }
      if (!pred %in% rownames(rv$ava_mat)) return()
      prey <- colnames(rv$ava_mat)
      ava <- as.numeric(rv$ava_mat[pred, ])
      ov <- if (is.null(rv$overlap_mat)) rep(NA_real_, length(prey)) else as.numeric(rv$overlap_mat[pred, ])
      eff <- if (is.null(rv$real_feed)) rep(NA_real_, length(prey)) else as.numeric(rv$real_feed[pred, ])
      df <- rbind(
        data.frame(Prey = prey, Value = ava, Metric = "Availability (pPREY)"),
        data.frame(Prey = prey, Value = ov, Metric = "Gape overlap"),
        data.frame(Prey = prey, Value = eff, Metric = "Effective predation")
      )
      keep <- prey[ava > 0 | (!is.na(ov) & ov > 0)]
      if (length(keep) == 0) keep <- prey
      df <- df[df$Prey %in% keep, ]
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Prey, y = Value, fill = Metric))
      p <- p + ggplot2::geom_col()
      p <- p + ggplot2::facet_wrap(~ Metric, ncol = 3, scales = "free_y")
      p <- p + ggplot2::labs(title = paste("Diet detail —", pred), x = "Prey", y = NULL)
      p <- p + theme_reactiveatlantis(base_size = 14)
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10),
                              legend.position = "none")
      p
      })
    })

    output$bug_table <- DT::renderDataTable({
      bugs <- rv$bugs
      if (is.null(bugs) || nrow(bugs) == 0) {
        return(ra_datatable(data.frame(Note = "No pPREY > 0 cells with zero gape overlap.")))
      }
      ra_datatable(bugs, options = list(pageLength = 8, dom = "tip"))
    })

    keep_outputs_alive(output, c("banner", "feeding_plot_gg", "detail_plot", "bug_table"))
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

predation_pressure_matrix <- function(real_feed) {
  if (is.null(real_feed) || !is.matrix(real_feed)) return(NULL)
  rf <- real_feed
  rf[!is.finite(rf)] <- 0
  rs <- rowSums(rf, na.rm = TRUE)
  press <- t(sweep(rf, 1, pmax(rs, 1e-12), "/")) * 100
  press[!is.finite(press)] <- 0
  press
}

gape_mismatch <- function(ava_mat, overlap_mat) {
  ava <- ava_mat > 0
  ov <- overlap_mat == 0
  ava & ov
}

gape_mismatch_table <- function(ava_mat, overlap_mat) {
  bug <- gape_mismatch(ava_mat, overlap_mat)
  if (!any(bug, na.rm = TRUE)) {
    return(data.frame(Predator = character(), Prey = character(), pPREY = numeric(),
                      stringsAsFactors = FALSE))
  }
  idx <- which(bug, arr.ind = TRUE)
  data.frame(
    Predator = rownames(ava_mat)[idx[, 1]],
    Prey = colnames(ava_mat)[idx[, 2]],
    pPREY = ava_mat[idx],
    stringsAsFactors = FALSE
  )
}

plot_feed_tile <- function(mat, title, pal, legend, highlight_pred, highlight_prey,
                           limits = NULL, mismatch = NULL){
  dat <- reshape2::melt(mat, value.name = 'value')
  dat$value[dat$value == 0] <- NA
  if(is.null(limits)){
    limits <- c(0, max(dat$value, na.rm = TRUE))
    if(!is.finite(limits[2]) || limits[2] == 0) limits[2] <- 1
  }
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = .data$value))
  p <- p + ggplot2::geom_tile(colour = "grey85", linewidth = 0.15)
  p <- p + ggplot2::scale_fill_distiller(palette = pal, limits = limits,
                                         name = legend, na.value = 'white', direction = 1)
  if (!is.null(mismatch) && any(mismatch, na.rm = TRUE)) {
    bug <- reshape2::melt(mismatch, value.name = "bug")
    bug <- bug[!is.na(bug$bug) & as.logical(bug$bug), , drop = FALSE]
    if (nrow(bug) > 0) {
      p <- p + ggplot2::geom_tile(
        data = bug, ggplot2::aes(x = .data$Var1, y = .data$Var2),
        fill = NA, colour = "#0B3C5D", linewidth = 0.55, inherit.aes = FALSE
      )
    }
  }
  p <- p + theme_reactiveatlantis()
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 8),
                          axis.text.y = ggplot2::element_text(size = 8),
                          legend.position = "right",
                          panel.grid = ggplot2::element_blank())
  p <- p + ggplot2::labs(x = 'Prey', y = 'Predator', title = title)
  p <- p + ggplot2::scale_x_discrete(position = "top")
  if (!is.null(highlight_pred) && nzchar(highlight_pred)) {
    hl <- dat[as.character(dat$Var2) == highlight_pred, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + ggplot2::geom_tile(
        data = hl, ggplot2::aes(x = .data$Var1, y = .data$Var2),
        fill = NA, colour = "#C4A35A", linewidth = 0.9, inherit.aes = FALSE
      )
    }
  }
  if (!is.null(highlight_prey) && nzchar(highlight_prey)) {
    hl <- dat[as.character(dat$Var1) == highlight_prey, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + ggplot2::geom_tile(
        data = hl, ggplot2::aes(x = .data$Var1, y = .data$Var2),
        fill = NA, colour = "#328CC1", linewidth = 0.9, inherit.aes = FALSE
      )
    }
  }
  p
}

plot_feed_mismatch <- function(bug_mat) {
  dat <- reshape2::melt(bug_mat, value.name = "bug")
  dat$Flag <- ifelse(!is.na(dat$bug) & as.logical(dat$bug), "pPREY>0, gape=0", "OK")
  pal <- c("OK" = "#F7F4EC", "pPREY>0, gape=0" = "#A33B20")
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = Flag))
  p <- p + ggplot2::geom_tile(colour = "grey85", linewidth = 0.15)
  p <- p + ggplot2::scale_fill_manual(values = pal)
  p <- p + theme_reactiveatlantis()
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 8),
                          axis.text.y = ggplot2::element_text(size = 8),
                          legend.position = "right",
                          panel.grid = ggplot2::element_blank())
  p <- p + ggplot2::labs(x = "Prey", y = "Predator",
                         title = "pPREY positive but gape overlap is zero")
  p <- p + ggplot2::scale_x_discrete(position = "top")
  p
}

plot_feed_overlap <- function(t.o.mat, highlight_pred, highlight_prey){
  col.tmp <- c("0" = "#F7F4EC", "1" = "#1D7874")
  dat2 <- reshape2::melt(t.o.mat, value.name = 'value')
  p <- ggplot2::ggplot(dat2, ggplot2::aes(x = .data$Var1, y = .data$Var2, fill = factor(.data$value)))
  p <- p + ggplot2::geom_tile(colour = 'grey85')
  p <- p + theme_reactiveatlantis()
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 8),
                          axis.text.y = ggplot2::element_text(size = 8),
                          legend.position = "right",
                          panel.grid = ggplot2::element_blank())
  p <- p + ggplot2::labs(x = 'Prey', y = 'Predator', title = "Gape overlap")
  p <- p + ggplot2::scale_x_discrete(position = "top")
  p <- p + ggplot2::scale_fill_manual(values = col.tmp, name = 'Gape overlap',
                                      labels = c('No', 'Yes'), na.value = 'white')
  if (!is.null(highlight_pred) && nzchar(highlight_pred)) {
    hl <- dat2[as.character(dat2$Var2) == highlight_pred, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + ggplot2::geom_tile(
        data = hl, ggplot2::aes(x = .data$Var1, y = .data$Var2),
        fill = NA, colour = "#C4A35A", linewidth = 0.9, inherit.aes = FALSE
      )
    }
  }
  if (!is.null(highlight_prey) && nzchar(highlight_prey)) {
    hl <- dat2[as.character(dat2$Var1) == highlight_prey, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + ggplot2::geom_tile(
        data = hl, ggplot2::aes(x = .data$Var1, y = .data$Var2),
        fill = NA, colour = "#328CC1", linewidth = 0.9, inherit.aes = FALSE
      )
    }
  }
  p
}
