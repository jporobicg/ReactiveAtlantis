## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Compare Outputs Module           ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

compare_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("body")))
}

compare_body_ui <- function(ns, grp) {
  types <- c("All", sort(unique(as.character(grp$grouptype))))
  codes <- as.character(grp$code[grp$numcohorts > 1 &
                                 !grp$grouptype %in% c("PWN", "PRAWNS", "PRAWN", "CEP",
                                                       "MOB_EP_OTHER", "SEAGRASS", "CORAL",
                                                       "MANGROVE", "MANGROVES", "SPONGE")])
  layout_sidebar(
    sidebar = sidebar(
      open = "open",
      width = 280,
      selectInput(ns("group_type"), "Group type", choices = types, selected = "All"),
      textInput(ns("search"), "Search code or name", placeholder = "COD, haddock…"),
      checkboxInput(ns("relative"), "Relative to initial (Bt/B0)", FALSE),
      hr(),
      selectInput(ns("view"), "View",
                  choices = c("Small multiples" = "facets",
                              "One group, totals" = "total",
                              "By age class" = "age")),
      conditionalPanel(
        "input.view == 'total'", ns = ns,
        selectInput(ns("fg_total"), "Functional group", choices = as.character(grp$code)),
        checkboxInput(ns("show_biomass"), "Biomass", TRUE),
        checkboxInput(ns("show_numbers"), "Numbers", FALSE),
        checkboxInput(ns("scaled"), "Scaled", TRUE),
        checkboxInput(ns("limit_axis"), "Limit axis", TRUE)
      ),
      conditionalPanel(
        "input.view == 'age'", ns = ns,
        selectInput(ns("fg_age"), "Functional group", choices = codes),
        checkboxInput(ns("show_biomass_age"), "Biomass", TRUE),
        checkboxInput(ns("show_numbers_age"), "Numbers", FALSE),
        checkboxInput(ns("scaled_age"), "Scaled", TRUE),
        checkboxInput(ns("limit_axis_age"), "Limit axis", TRUE)
      )
    ),
    uiOutput(ns("plot_host"))
  )
}

compare_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(t_biomass = NULL, rel_bio = NULL, built = FALSE, token = NULL)

    output$body <- renderUI({
      if (is.null(project) || !isTRUE(project$ready)) {
        return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS$compare], "Compare"))
      }
      miss <- tool_missing(project$paths, "compare")
      if (length(miss)) return(tool_missing_card(miss, "Compare"))
      compare_body_ui(session$ns, project$data$grp)
    })

    observe({
      req(!is.null(project), isTRUE(project$ready), project$data)
      miss <- tool_missing(project$paths, "compare")
      token <- paste(unlist(project$paths), collapse = "|")
      if (length(miss) || (isTRUE(rv$built) && identical(rv$token, token))) return()
      rv$token <- token
      dat <- project$data
      req(dat$nc_out, dat$grp, dat$inf_box, dat$time_dates)

      grp <- dat$grp[dat$grp$isturnedon == 1,
                     c("code", "name", "longname", "grouptype", "numcohorts")]
      mg2t <- 0.00000002
      x.cn <- 5.7
      Time <- dat$time_dates
      inf_box <- dat$inf_box
      nc_cur <- dat$nc_out

      age.grp <- grp[grp$numcohorts > 1 &
                     !grp$grouptype %in% c("PWN", "PRAWNS", "PRAWN", "CEP", "MOB_EP_OTHER",
                                           "SEAGRASS", "CORAL", "MANGROVE", "MANGROVES", "SPONGE"), ]
      pol.grp <- grp[grp$numcohorts == 1, ]
      pwn.grp <- grp[grp$numcohorts > 1 &
                     grp$grouptype %in% c("PWN", "PRAWNS", "PRAWN", "CEP", "MOB_EP_OTHER",
                                          "SEAGRASS", "CORAL", "MANGROVE", "MANGROVES", "SPONGE"), ]

      age.cur.bio <- if (nrow(age.grp) > 0) bio.age(age.grp, nc_cur, "Current", mg2t, x.cn, inf_box, Time) else NULL
      pool.cur.bio <- bio.pool(pol.grp, nc_cur, "Current", mg2t, x.cn, inf_box, Time)
      old.bio <- NULL
      Time_old <- NULL
      if (!is.null(dat$nc_old)) {
        Time_old <- tryCatch(time_calc(dat$nc_old), error = function(e) Time)
        age.old.bio <- if (nrow(age.grp) > 0) bio.age(age.grp, dat$nc_old, "Previous", mg2t, x.cn, inf_box, Time_old) else NULL
        pool.old.bio <- bio.pool(pol.grp, dat$nc_old, "Previous", mg2t, x.cn, inf_box, Time_old)
        old.bio <- rbind(pool.old.bio, age.old.bio)
      }
      pwn.bio <- NULL
      if (nrow(pwn.grp) > 0) {
        pwn.cur.bio <- bio.pwn(pwn.grp, nc_cur, "Current", mg2t, x.cn, inf_box, Time)
        pwn.bio <- pwn.cur.bio
        if (!is.null(dat$nc_old)) {
          pwn.bio <- rbind(pwn.cur.bio, bio.pwn(pwn.grp, dat$nc_old, "Previous", mg2t, x.cn, inf_box, Time_old))
        }
      }
      t.biomass <- rbind(age.cur.bio, pool.cur.bio, old.bio, pwn.bio)
      rel.bio <- do.call(rbind.data.frame, by(t.biomass, t.biomass$FG, relative))
      rv$t_biomass <- t.biomass
      rv$rel_bio <- rel.bio
      rv$built <- TRUE
    })

    filtered <- reactive({
      req(rv$t_biomass, project$data$grp)
      df <- if (isTRUE(input$relative)) rv$rel_bio else rv$t_biomass
      grp <- project$data$grp
      if (!is.null(input$group_type) && input$group_type != "All") {
        codes <- grp$code[grp$grouptype == input$group_type]
        keep <- grepl(paste0("\\((", paste(codes, collapse = "|"), ")\\)$"), df$FG)
        df <- df[keep, ]
      }
      if (!is.null(input$search) && nzchar(trimws(input$search))) {
        q <- trimws(input$search)
        df <- df[grepl(q, df$FG, ignore.case = TRUE), ]
      }
      df
    })

    output$plot_host <- renderUI({
      ns <- session$ns
      view <- input$view
      if (is.null(view) || view == "facets") {
        if (requireNamespace("plotly", quietly = TRUE)) plotly::plotlyOutput(ns("plot_facets"), height = "760px")
        else plotOutput(ns("plot_facets_gg"), height = "760px")
      } else if (view == "total") {
        plotOutput(ns("plot_total"), height = "560px")
      } else {
        plotOutput(ns("plot_age"), height = "640px")
      }
    })

    make_facet_plot <- function() {
      df <- filtered()
      req(nrow(df) > 0)
      ycol <- if ("Relative" %in% names(df) && isTRUE(input$relative)) "Relative" else "Biomass"
      ylab <- if (isTRUE(input$relative)) "Relative biomass (Bt/B0)" else "Biomass (tons)"
      colors <- get_comparison_colors()
      p <- ggplot2::ggplot(df, ggplot2::aes(x = Time, y = .data[[ycol]], colour = Simulation))
      p <- p + ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE)
      p <- p + ggplot2::facet_wrap(~ FG, ncol = 4, scales = "free_y")
      p <- p + ggplot2::scale_color_manual(values = colors)
      p <- p + ggplot2::labs(x = "Year", y = ylab, title = "Biomass by functional group")
      p <- p + theme_reactiveatlantis()
      if (isTRUE(input$relative)) {
        p <- p + ggplot2::coord_cartesian(ylim = relative_axis_ylim(df[[ycol]]))
      }
      p
    }

    output$plot_facets_gg <- renderPlot({ make_facet_plot() })
    if (requireNamespace("plotly", quietly = TRUE)) {
      output$plot_facets <- plotly::renderPlotly({
        p <- make_facet_plot()
        ra_ggplotly(p, n_series = length(unique(filtered()$FG)))
      })
    }

    output$plot_total <- renderPlot({
      dat <- project$data
      req(dat$nc_out, input$fg_total, dat$inf_box, dat$time_dates)
      mg2t <- 0.00000002
      x.cn <- 5.7
      total_data <- nitro.weight(dat$nc_out, dat$grp, input$fg_total, By = "Total",
                                dat$inf_box, mg2t, x.cn)
      if (isTRUE(input$scaled)) {
        rmv <- which(sapply(total_data, function(x) length(x) == 0 || is.null(x) || is.character(x)))
        total.tmp <- total_data[-rmv]
        total.tmp <- lapply(total.tmp, function(x) x / x[1])
        total_data[-rmv] <- total.tmp
      }
      p <- plot_total_data(total_data, dat$time_dates, FALSE, FALSE,
                           isTRUE(input$show_numbers), isTRUE(input$show_biomass),
                           isTRUE(input$scaled), isTRUE(input$limit_axis))
      if (inherits(p, "ggplot")) p + theme_reactiveatlantis() else p
    })

    output$plot_age <- renderPlot({
      dat <- project$data
      req(dat$nc_out, input$fg_age, dat$inf_box, dat$time_dates)
      mg2t <- 0.00000002
      x.cn <- 5.7
      coho <- nitro.weight(dat$nc_out, dat$grp, FG = input$fg_age, By = "Cohort",
                          dat$inf_box, mg2t, x.cn)
      if (isTRUE(input$scaled_age)) {
        rmv <- which(sapply(coho, function(x) length(x) == 0 || is.null(x) || is.character(x)))
        coho.tmp <- coho[-rmv]
        coho.tmp <- lapply(coho.tmp, function(x) lapply(x, function(x) x / x[1]))
        coho[-rmv] <- coho.tmp
      }
      n.coh <- dat$grp$numcohorts[dat$grp$code == input$fg_age]
      p <- plot_cohorts_ggplot(coho, dat$time_dates, FALSE, FALSE,
                              isTRUE(input$show_numbers_age), isTRUE(input$show_biomass_age),
                              isTRUE(input$scaled_age), isTRUE(input$limit_axis_age), n.coh)
      if (inherits(p, "ggplot")) p + theme_reactiveatlantis() else p
    })
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
      theme_reactiveatlantis() +
      labs(x = 'Year', 
          y = if(scl2) 'Relative Values (X_t/X_0)' else 'Biomass (tons)',
          title = "Total Biomass Over Time")
    
    if(scl2 && limit2){
      p <- p + ggplot2::coord_cartesian(ylim = relative_axis_ylim(bio_df$Value))
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
    
    ylim <- if(limit3a && scl3a) relative_axis_ylim(bms) else range(bms, na.rm = TRUE)
    
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
  p <- p + theme_reactiveatlantis()
  p <- p + ggplot2::labs(x = "Year", y = ylab, title = "Values by age class")
  if(isTRUE(scl3a) && isTRUE(limit3a)){
    p <- p + ggplot2::coord_cartesian(ylim = relative_axis_ylim(plot_df$Value))
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
