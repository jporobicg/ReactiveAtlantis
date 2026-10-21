## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~        Feeding Matrix Analysis Module      ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

feeding_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 300, open = "open",
        selectInput(
          ns("view_type"), "View",
          choices = list(
            "Matrix (existing)" = c(
              "Availability (pPREY)" = "availability",
              "Gape overlap" = "overlap",
              "Gape mismatch (pPREY>0, overlap=0)" = "mismatch",
              "Effective predation" = "effective",
              "Predation pressure" = "pressure"
            ),
            "One-group" = c(
              "One-group: predator or prey preferences" = "one_group"
            ),
            "Matrix filters" = c(
              "Matrix filters: threshold, interacting, cluster" = "matrix_filters"
            ),
            "Diagnostics" = c(
              "Diagnostics: sums, uneaten prey, DietCheck" = "diagnostics"
            ),
            "Compare" = c(
              "Compare edited vs original" = "compare"
            )
          ),
          selected = "availability"
        ),
        tags$hr(),
        tags$strong("Predator"),
        selectInput(ns("pred_group"), "Group name", choices = c("(all)" = "(all)")),
        selectInput(ns("pred_stage"), "Adult / juvenile",
                    choices = c("All" = "all", "Adult" = "2", "Juvenile" = "1",
                                "Biomass pool" = "pool")),
        tags$strong("Prey"),
        selectInput(ns("prey_group"), "Group name", choices = c("(all)" = "(all)")),
        selectInput(ns("prey_stage"), "Adult / juvenile",
                    choices = c("All" = "all", "Adult" = "2", "Juvenile" = "1")),
        conditionalPanel(
          "input.view_type == 'one_group'", ns = ns,
          tags$hr(),
          tags$strong("One-group view"),
          selectInput(
            ns("one_dir"), "Direction",
            choices = c("Predator diet (sorted prey)" = "predator",
                        "Prey eaten-by (sorted predators)" = "prey")
          ),
          p(class = "ra-muted",
            "Pick a predator group and adult/juvenile for a sorted prey bar chart, or reverse: pick a prey group and stage to list its predators.")
        ),
        conditionalPanel(
          "input.view_type == 'matrix_filters'", ns = ns,
          tags$hr(),
          tags$strong("Matrix filters"),
          sliderInput(ns("hide_below"), "Hide values below",
                      min = 0, max = 1, value = 0, step = 0.01),
          checkboxInput(ns("only_interacting"), "Show only interacting groups", FALSE),
          checkboxInput(ns("cluster_rc"), "Cluster rows and columns", FALSE)
        ),
        conditionalPanel(
          "input.view_type == 'diagnostics'", ns = ns,
          tags$hr(),
          tags$strong("Diagnostics"),
          numericInput(ns("diet_eps"), "DietCheck mismatch threshold",
                       0.001, min = 0, max = 1, step = 0.001),
          p(class = "ra-muted",
            "Flag predators with odd preference sums, prey nobody eats, and pPREY vs realised DietCheck mismatches.")
        ),
        conditionalPanel(
          "input.view_type == 'compare'", ns = ns,
          tags$hr(),
          tags$strong("Compare / edit"),
          numericInput(ns("edit_value"), "New pPREY value",
                       0, min = 0, max = 1, step = 0.0001),
          actionButton(ns("apply_edit"), "Apply edit", class = "btn-sm"),
          actionButton(ns("reset_edits"), "Reset to original", class = "btn-sm"),
          textOutput(ns("edit_status")),
          p(class = "ra-muted",
            "Changed cells only. The group-name and adult/juvenile selectors pick the cell.")
        ),
        p(class = "ra-muted",
          "Predator and prey search is group name plus adult/juvenile everywhere. Existing matrix views keep their plots.")
      ),
      plotOutput(ns("feeding_plot_gg"), height = "640px"),
      conditionalPanel(
        paste0("['availability','overlap','mismatch','effective','pressure']",
               ".indexOf(input.view_type) >= 0"),
        ns = ns,
        plotOutput(ns("detail_plot"), height = "280px")
      ),
      DT::dataTableOutput(ns("bug_table"))
    )
  )
}

feeding_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))
    rv <- reactiveValues(
      ava_mat = NULL, overlap_mat = NULL, real_feed = NULL,
      pressure = NULL, bugs = NULL, built = FALSE, token = NULL,
      edited = NULL, original_key = NULL, edit_msg = ""
    )

    output$banner <- renderUI(tool_banner(ps(), "feeding"))

    pprey_mat <- reactive({
      dat <- ps()$data
      req(isTRUE(ps()$ready), dat$grp, dat$prm_bio)
      grp <- dat$grp
      if (any(grepl("invertype", names(grp)))) {
        names(grp)[which(grepl("invertype", names(grp)))] <- "grouptype"
      }
      options(warn = -1)
      ava_mat <- text2num(dat$prm_bio, "pPREY", Vector = TRUE, pprey = TRUE)
      options(warn = 0)
      if (is.null(ava_mat) || !is.matrix(ava_mat) || nrow(ava_mat) == 0) return(NULL)
      n_col <- ncol(ava_mat)
      expected <- c(as.character(grp$code), "DLsed", "DRsed", "DCsed")
      if (n_col == length(expected)) {
        colnames(ava_mat) <- expected
      } else if (n_col == length(grp$code)) {
        colnames(ava_mat) <- as.character(grp$code)
      } else if (is.null(colnames(ava_mat))) {
        colnames(ava_mat) <- paste0("Prey", seq_len(n_col))
      }
      ava_mat
    })

    observe({
      orig <- pprey_mat()
      req(orig)
      key <- paste(c(rownames(orig), colnames(orig), dim(orig)), collapse = "|")
      if (!identical(rv$original_key, key)) {
        rv$edited <- orig
        rv$original_key <- key
        rv$edit_msg <- ""
      }
    })

    current_mat <- reactive({
      orig <- pprey_mat()
      ed <- rv$edited
      if (!is.null(ed) && !is.null(orig) &&
          identical(dim(ed), dim(orig)) &&
          identical(rownames(ed), rownames(orig)) &&
          identical(colnames(ed), colnames(orig))) {
        return(ed)
      }
      orig
    })

    gape_built <- reactive({
      ava_mat <- current_mat()
      req(ava_mat)
      dat <- ensure_project_data(ps(), "nc_init")
      grp <- dat$grp
      prm <- dat$prm_bio
      paths <- tryCatch(normalize_paths(ps()$paths), error = function(e) empty_paths())
      if (!is.null(dat$paths)) {
        dp <- tryCatch(normalize_paths(dat$paths), error = function(e) NULL)
        if (!is.null(dp) && path_ok(dp$init_nc)) paths$init_nc <- dp$init_nc
      }
      init_path <- paths$init_nc
      tryCatch({
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
             pressure = predation_pressure_matrix(real.feed),
             bugs = gape_mismatch_table(ava_mat, overlap_mat))
      }, error = function(e) {
        list(overlap = NULL, real = NULL,
             pressure = predation_pressure_matrix(ava_mat), bugs = NULL)
      })
    })

    observe({
      ava <- current_mat()
      req(ava)
      grp <- tryCatch(ps()$data$grp, error = function(e) NULL)
      meta <- parse_pprey_rownames(rownames(ava))
      pred_ch <- pprey_group_choices(meta$pred_code, grp)
      prey_ch <- pprey_group_choices(colnames(ava), grp)
      update_select_keep(session, "pred_group", pred_ch, input$pred_group)
      update_select_keep(session, "prey_group", prey_ch, input$prey_group)
    })

    filtered_mats <- reactive({
      ava <- current_mat()
      req(ava)
      hit <- pprey_filter_index(
        ava,
        pred_group = input$pred_group,
        pred_stage = input$pred_stage,
        prey_group = input$prey_group,
        prey_stage = input$prey_stage
      )
      searching <- isTRUE(hit$searching)
      pred_keep <- hit$pred_keep
      prey_keep <- hit$prey_keep
      if (!searching && length(pred_keep) == 0) pred_keep <- seq_len(nrow(ava))
      if (!searching && length(prey_keep) == 0) prey_keep <- seq_len(ncol(ava))
      list(
        ava = ava[pred_keep, prey_keep, drop = FALSE],
        need_gape = !input$view_type %in% c("availability", "one_group",
                                            "matrix_filters", "diagnostics",
                                            "compare"),
        pred_keep = pred_keep,
        prey_keep = prey_keep,
        empty_search = searching && (length(pred_keep) == 0 || length(prey_keep) == 0),
        hl_pred = if (is.null(input$pred_group) || input$pred_group %in% c("", "(all)")) {
          NULL
        } else rownames(ava)[hit$pred_all],
        hl_prey = if (is.null(input$prey_group) || input$prey_group %in% c("", "(all)")) {
          NULL
        } else input$prey_group
      )
    })

    make_feed_plot <- function() {
      if (identical(input$view_type, "one_group")) {
        return(make_one_group_plot())
      }
      if (identical(input$view_type, "diagnostics")) {
        return(make_diagnostics_plot())
      }
      if (identical(input$view_type, "compare")) {
        return(make_compare_plot())
      }
      mats <- filtered_mats()
      if (isTRUE(mats$empty_search) || is.null(mats$ava) || nrow(mats$ava) == 0 || ncol(mats$ava) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "No predators or prey match that group and stage") +
                 theme_reactiveatlantis())
      }
      req(mats$ava, input$view_type)
      ava_show <- mats$ava
      if (identical(input$view_type, "matrix_filters")) {
        ava_show <- filter_feed_matrix(
          ava_show,
          threshold = input$hide_below,
          interacting = isTRUE(input$only_interacting),
          cluster = isTRUE(input$cluster_rc)
        )
        if (is.null(ava_show) || nrow(ava_show) == 0 || ncol(ava_show) == 0) {
          return(ggplot2::ggplot() +
                   ggplot2::labs(title = "No interacting groups above that threshold") +
                   theme_reactiveatlantis())
        }
        return(plot_feed_tile(
          t(ava_show), "Filtered availability (pPREY)",
          pal = "YlOrRd", legend = "Availability",
          highlight_pred = mats$hl_pred, highlight_prey = mats$hl_prey
        ))
      }
      gape <- if (isTRUE(mats$need_gape)) gape_built() else NULL
      overlap <- if (is.null(gape) || is.null(gape$overlap) || length(mats$pred_keep) == 0) {
        NULL
      } else gape$overlap[mats$pred_keep, mats$prey_keep, drop = FALSE]
      real <- if (is.null(gape) || is.null(gape$real) || length(mats$pred_keep) == 0) {
        NULL
      } else gape$real[mats$pred_keep, mats$prey_keep, drop = FALSE]
      pressure <- if (is.null(gape) || is.null(gape$pressure) || length(mats$pred_keep) == 0) {
        NULL
      } else gape$pressure[mats$prey_keep, mats$pred_keep, drop = FALSE]
      bugs <- if (is.null(overlap)) NULL else gape_mismatch(mats$ava, overlap)
      hl_pred <- mats$hl_pred
      hl_prey <- mats$hl_prey
      if (input$view_type == "availability") {
        plot_feed_tile(t(mats$ava), "Availability matrix (pPREY)",
                       pal = "YlOrRd", legend = "Availability",
                       highlight_pred = hl_pred, highlight_prey = hl_prey,
                       mismatch = if (is.null(bugs)) NULL else t(bugs))
      } else if (input$view_type == "overlap") {
        t.o.mat <- t(overlap * mats$ava)
        t.o.mat[t.o.mat > 0] <- 1
        plot_feed_overlap(t.o.mat, hl_pred, hl_prey)
      } else if (input$view_type == "mismatch") {
        if (is.null(bugs)) {
          return(ggplot2::ggplot() + ggplot2::labs(title = "Gape overlap was not calculated") +
                   theme_reactiveatlantis())
        }
        plot_feed_mismatch(t(bugs))
      } else if (input$view_type == "effective") {
        rff <- log(real)
        rff[!is.finite(rff)] <- NA
        plot_feed_tile(t(rff), "Effective predation (log scale)",
                       pal = "YlGnBu", legend = "Predation\nLn()",
                       highlight_pred = hl_pred, highlight_prey = hl_prey,
                       mismatch = if (is.null(bugs)) NULL else t(bugs))
      } else {
        press <- pressure
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

    make_one_group_plot <- function() {
      ava <- current_mat()
      req(ava)
      grp <- tryCatch(ps()$data$grp, error = function(e) NULL)
      dirn <- if (is.null(input$one_dir)) "predator" else input$one_dir
      if (identical(dirn, "prey")) {
        if (is.null(input$prey_group) || input$prey_group %in% c("", "(all)")) {
          return(ggplot2::ggplot() +
                   ggplot2::labs(title = "Pick a prey group name, then adult or juvenile") +
                   theme_reactiveatlantis())
        }
        df <- pprey_one_group(ava, "prey", input$prey_group, input$prey_stage, grp)
      } else {
        if (is.null(input$pred_group) || input$pred_group %in% c("", "(all)")) {
          return(ggplot2::ggplot() +
                   ggplot2::labs(title = "Pick a predator group name, then adult or juvenile") +
                   theme_reactiveatlantis())
        }
        df <- pprey_one_group(ava, "predator", input$pred_group, input$pred_stage, grp)
      }
      plot_pprey_one_group(df)
    }

    make_diagnostics_plot <- function() {
      ava <- current_mat()
      req(ava)
      diag <- pprey_diagnostics(ava)
      plot_pprey_sum_flags(diag$sums)
    }

    make_compare_plot <- function() {
      orig <- pprey_mat()
      ed <- current_mat()
      req(orig, ed)
      plot_pprey_compare(orig, ed)
    }

    observeEvent(input$apply_edit, {
      orig <- pprey_mat()
      req(orig)
      if (is.null(rv$edited)) rv$edited <- orig
      hit <- pprey_apply_edit(
        rv$edited,
        pred_group = input$pred_group,
        pred_stage = input$pred_stage,
        prey_group = input$prey_group,
        prey_stage = input$prey_stage,
        value = input$edit_value
      )
      rv$edited <- hit$mat
      rv$edit_msg <- hit$message
    })

    observeEvent(input$reset_edits, {
      orig <- pprey_mat()
      req(orig)
      rv$edited <- orig
      rv$edit_msg <- "Reset to the original pPREY matrix."
    })

    output$edit_status <- renderText({
      msg <- rv$edit_msg
      if (is.null(msg) || !nzchar(msg)) return("")
      msg
    })

    output$feeding_plot_gg <- renderPlot({
      plot_or_message("Load biology.prm (and initial.nc) to plot the feeding matrix",
                      make_feed_plot())
    })

    output$detail_plot <- renderPlot({
      plot_or_message("Select a predator group and adult/juvenile for the detail chart", {
      ava_mat <- current_mat()
      req(ava_mat)
      if (is.null(input$pred_group) || input$pred_group %in% c("", "(all)")) {
        plot.new()
        title("Select a predator group and adult/juvenile for availability, gape overlap and effective predation")
        return(invisible(NULL))
      }
      meta <- parse_pprey_rownames(rownames(ava_mat))
      rows <- pprey_match_rows(meta, input$pred_group, input$pred_stage)
      if (!is.null(input$prey_stage) && !input$prey_stage %in% c("", "all", "(all)")) {
        rows <- intersect(rows, pprey_match_prey_stage(meta, input$prey_stage))
      }
      if (length(rows) == 0) {
        plot.new()
        title("No pPREY row for that predator group and stage")
        return(invisible(NULL))
      }
      pred <- rownames(ava_mat)[rows[1]]
      prey <- colnames(ava_mat)
      ava <- as.numeric(ava_mat[pred, ])
      gape <- tryCatch(gape_built(), error = function(e) NULL)
      ov <- if (is.null(gape) || is.null(gape$overlap) || !pred %in% rownames(gape$overlap)) {
        rep(NA_real_, length(prey))
      } else as.numeric(gape$overlap[pred, ])
      eff <- if (is.null(gape) || is.null(gape$real) || !pred %in% rownames(gape$real)) {
        rep(NA_real_, length(prey))
      } else as.numeric(gape$real[pred, ])
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
      extra <- if (length(rows) > 1) paste0(" (1 of ", length(rows), " matching rows)") else ""
      p <- p + ggplot2::labs(title = paste0("Diet detail — ", pred, extra), x = "Prey", y = NULL)
      p <- p + theme_reactiveatlantis(base_size = 14)
      p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 10),
                              legend.position = "none")
      p
      })
    })

    output$bug_table <- DT::renderDataTable({
      if (identical(input$view_type, "diagnostics")) {
        ava <- current_mat()
        if (is.null(ava)) {
          return(ra_datatable(data.frame(Note = "Load biology.prm to run feeding diagnostics.")))
        }
        diet <- NULL
        dat <- tryCatch(ensure_diet(ps()$data, ps()$paths), error = function(e) NULL)
        if (!is.null(dat)) {
          diet <- if (!is.null(dat$diet_full)) dat$diet_full else dat$diet_time
        }
        eps <- if (is.null(input$diet_eps)) 0.001 else as.numeric(input$diet_eps)[1]
        tab <- pprey_diagnostics_table(ava, diet, eps)
        if (is.null(tab) || nrow(tab) == 0) {
          return(ra_datatable(data.frame(Note = "No diagnostic flags.")))
        }
        return(ra_datatable(tab, options = list(pageLength = 10, dom = "tip")))
      }
      if (identical(input$view_type, "compare")) {
        ch <- pprey_changed_cells(pprey_mat(), current_mat())
        if (is.null(ch) || nrow(ch) == 0) {
          return(ra_datatable(data.frame(Note = "No edited cells. Apply an edit to compare.")))
        }
        return(ra_datatable(ch, options = list(pageLength = 10, dom = "tip")))
      }
      if (identical(input$view_type, "one_group") ||
          identical(input$view_type, "matrix_filters")) {
        return(ra_datatable(data.frame(
          Note = "This view uses the plot above. Switch to Diagnostics for flags."
        )))
      }
      if (identical(input$view_type, "availability")) {
        return(ra_datatable(data.frame(
          Note = "Switch to gape mismatch to list pPREY > 0 cells with zero overlap."
        )))
      }
      gape <- tryCatch(gape_built(), error = function(e) NULL)
      bugs <- if (is.null(gape)) NULL else gape$bugs
      if (is.null(bugs) || nrow(bugs) == 0) {
        return(ra_datatable(data.frame(Note = "No pPREY > 0 cells with zero gape overlap.")))
      }
      ra_datatable(bugs, options = list(pageLength = 8, dom = "tip"))
    })

    keep_outputs_alive(output, "banner")
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Feeding helpers (from feeding.mat)     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

feed_bio_struct <- function(nc.file, groups.csv){
  path <- as_path(nc.file)
  ck <- paste0("feedbio|", file_cache_key(path))
  hit <- ra_cache_get(ck)
  if (!is.null(hit)) return(hit)
  nc.out <- nc_handle(nc.file)
  if (is.null(nc.out)) stop("Could not open initial conditions NetCDF")
  
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
          N.tot <- nc_get(nc.out, paste0(FG[code], "_N", coh))
        } else {
          N.tot <- nc_get(nc.out, paste0(FG[code], "_N"))
        }
        if(all(is.na(N.tot)) || all(N.tot == 0) || sum(N.tot, na.rm = TRUE) == 0){
          water.t <- nc_get(nc.out, 'volume')
          w.depth <- nc_get(nc.out, 'nominal_dz')
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
            water.t <- nc_get(nc.out, 'volume')
            N.tot <- N.tot * water.t
          } else if(!is.null(unit) && unit == 'mg N m-2'){
            water.t <- nc_get(nc.out, 'volume')
            w.depth <- nc_get(nc.out, 'nominal_dz')
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
        StructN <- nc_get(nc.out, paste0(FG[code], cohort, "_StructN"))
        if(all(is.na(StructN))){
          StructN <- ncdf4::ncatt_get(nc.out, paste0(FG[code], cohort, "_StructN"), attname = "_FillValue")$value
        }
        ReservN <- nc_get(nc.out, paste0(FG[code], cohort, "_ResN"))
        if(all(is.na(ReservN))){
          ReservN <- ncdf4::ncatt_get(nc.out, paste0(FG[code], cohort, "_ResN"), attname = "_FillValue")$value
        }
        Numb <- nc_get(nc.out, paste0(FG[code], cohort, "_Nums"))
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
  out <- list(Struct = Struct, Biom.N = Biom.N)
  ra_cache_set(ck, out)
  out
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
  if (!is.null(highlight_pred) && length(highlight_pred) && any(nzchar(highlight_pred))) {
    hl <- dat[as.character(dat$Var2) %in% highlight_pred, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + ggplot2::geom_tile(
        data = hl, ggplot2::aes(x = .data$Var1, y = .data$Var2),
        fill = NA, colour = "#C4A35A", linewidth = 0.9, inherit.aes = FALSE
      )
    }
  }
  if (!is.null(highlight_prey) && length(highlight_prey) && any(nzchar(highlight_prey))) {
    hl <- dat[as.character(dat$Var1) %in% highlight_prey, , drop = FALSE]
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
  if (!is.null(highlight_pred) && length(highlight_pred) && any(nzchar(highlight_pred))) {
    hl <- dat2[as.character(dat2$Var2) %in% highlight_pred, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + ggplot2::geom_tile(
        data = hl, ggplot2::aes(x = .data$Var1, y = .data$Var2),
        fill = NA, colour = "#C4A35A", linewidth = 0.9, inherit.aes = FALSE
      )
    }
  }
  if (!is.null(highlight_prey) && length(highlight_prey) && any(nzchar(highlight_prey))) {
    hl <- dat2[as.character(dat2$Var1) %in% highlight_prey, , drop = FALSE]
    if (nrow(hl) > 0) {
      p <- p + ggplot2::geom_tile(
        data = hl, ggplot2::aes(x = .data$Var1, y = .data$Var2),
        fill = NA, colour = "#328CC1", linewidth = 0.9, inherit.aes = FALSE
      )
    }
  }
  p
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   pPREY group+stage, filters, diagnostics  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

pprey_stage_label <- function(x) {
  x <- as.integer(x)
  out <- rep("Biomass pool", length(x))
  out[!is.na(x) & x == 1L] <- "Juvenile"
  out[!is.na(x) & x == 2L] <- "Adult"
  out[!is.na(x) & !x %in% c(1L, 2L)] <- paste("Stage", x[!is.na(x) & !x %in% c(1L, 2L)])
  out
}

pprey_stage_title <- function(stage) {
  if (is.null(stage) || stage %in% c("", "all", "(all)")) return("(all stages)")
  if (identical(as.character(stage), "1")) return("juvenile")
  if (identical(as.character(stage), "2")) return("adult")
  if (identical(as.character(stage), "pool")) return("biomass pool")
  as.character(stage)
}

parse_pprey_rownames <- function(rnames) {
  rnames <- as.character(rnames)
  if (length(rnames) == 0) {
    return(data.frame(
      row = character(), pred_code = character(),
      pred_stage = integer(), prey_stage = integer(),
      stringsAsFactors = FALSE
    ))
  }
  rest <- sub("^pPREY", "", rnames, ignore.case = TRUE)
  n <- length(rest)
  prey_stage <- rep(NA_integer_, n)
  pred_code <- rep(NA_character_, n)
  pred_stage <- rep(NA_integer_, n)
  for (i in seq_len(n)) {
    tok <- rest[i]
    pred <- gsub("[[:digit:]]+", "", tok)
    if (!nzchar(pred)) {
      pred_code[i] <- tok
      next
    }
    pred_code[i] <- pred
    bits <- unlist(strsplit(tok, pred, fixed = TRUE))
    if (length(bits) == 0) next
    if (!nzchar(bits[1])) {
      n2 <- suppressWarnings(as.integer(bits[nzchar(bits)][1]))
      if (is.finite(n2)) pred_stage[i] <- n2
    } else {
      n1 <- suppressWarnings(as.integer(bits[1]))
      if (is.finite(n1)) prey_stage[i] <- n1
      if (length(bits) >= 2 && nzchar(bits[2])) {
        n2 <- suppressWarnings(as.integer(bits[2]))
        if (is.finite(n2)) pred_stage[i] <- n2
      }
    }
  }
  data.frame(
    row = rnames,
    pred_code = pred_code,
    pred_stage = pred_stage,
    prey_stage = prey_stage,
    stringsAsFactors = FALSE
  )
}

pprey_group_label <- function(code, grp = NULL) {
  code <- as.character(code)
  out <- code
  if (is.null(grp) || !"code" %in% names(grp) || length(code) == 0) return(out)
  nm <- if ("longname" %in% names(grp)) {
    as.character(grp$longname)
  } else if ("name" %in% names(grp)) {
    as.character(grp$name)
  } else {
    as.character(grp$code)
  }
  map <- stats::setNames(nm, as.character(grp$code))
  hit <- unname(map[code])
  ok <- !is.na(hit) & nzchar(hit) & hit != code
  out[ok] <- paste0(hit[ok], " (", code[ok], ")")
  out[!ok & !is.na(hit) & nzchar(hit)] <- hit[!ok & !is.na(hit) & nzchar(hit)]
  out
}

pprey_group_choices <- function(codes, grp = NULL) {
  codes <- unique(as.character(codes))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  if (length(codes) == 0) return(c("(all)" = "(all)"))
  labs <- pprey_group_label(codes, grp)
  c("(all)" = "(all)", stats::setNames(codes, labs))
}

pprey_stage_wanted <- function(stage) {
  if (is.null(stage)) return("all")
  s <- as.character(stage)[1]
  if (!nzchar(s) || s %in% c("all", "(all)")) return("all")
  s
}

pprey_match_stage <- function(stage_int, want) {
  want <- pprey_stage_wanted(want)
  if (identical(want, "all")) return(rep(TRUE, length(stage_int)))
  if (identical(want, "pool")) return(is.na(stage_int))
  w <- suppressWarnings(as.integer(want))
  !is.na(stage_int) & stage_int == w
}

pprey_match_rows <- function(meta, pred_group, pred_stage) {
  if (is.null(meta) || nrow(meta) == 0) return(integer(0))
  keep <- rep(TRUE, nrow(meta))
  if (!is.null(pred_group) && !pred_group %in% c("", "(all)")) {
    keep <- keep & meta$pred_code == as.character(pred_group)
  }
  keep <- keep & pprey_match_stage(meta$pred_stage, pred_stage)
  which(keep)
}

pprey_match_prey_stage <- function(meta, prey_stage) {
  if (is.null(meta) || nrow(meta) == 0) return(integer(0))
  want <- pprey_stage_wanted(prey_stage)
  if (identical(want, "all")) return(seq_len(nrow(meta)))
  which(pprey_match_stage(meta$prey_stage, prey_stage))
}

pprey_filter_index <- function(mat, pred_group, pred_stage, prey_group, prey_stage) {
  if (is.null(mat) || !is.matrix(mat)) {
    return(list(pred_keep = integer(0), prey_keep = integer(0),
                pred_all = integer(0), searching = FALSE))
  }
  meta <- parse_pprey_rownames(rownames(mat))
  pred_sel <- !is.null(pred_group) && !pred_group %in% c("", "(all)")
  pred_st <- !identical(pprey_stage_wanted(pred_stage), "all")
  prey_sel <- !is.null(prey_group) && !prey_group %in% c("", "(all)")
  prey_st <- !identical(pprey_stage_wanted(prey_stage), "all")
  pred_keep <- pprey_match_rows(meta, pred_group, pred_stage)
  if (prey_st) pred_keep <- intersect(pred_keep, pprey_match_prey_stage(meta, prey_stage))
  pred_all <- pprey_match_rows(meta, pred_group, pred_stage)
  prey_keep <- seq_len(ncol(mat))
  if (prey_sel) {
    prey_keep <- which(colnames(mat) == as.character(prey_group))
  }
  list(
    pred_keep = pred_keep,
    prey_keep = prey_keep,
    pred_all = pred_all,
    searching = pred_sel || pred_st || prey_sel || prey_st
  )
}

filter_feed_matrix <- function(mat, threshold = 0, interacting = FALSE, cluster = FALSE) {
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) return(mat)
  m <- mat
  m[!is.finite(m)] <- 0
  thr <- suppressWarnings(as.numeric(threshold)[1])
  if (is.finite(thr) && thr > 0) m[m < thr] <- 0
  if (isTRUE(interacting)) {
    rkeep <- rowSums(m > 0, na.rm = TRUE) > 0
    ckeep <- colSums(m > 0, na.rm = TRUE) > 0
    if (any(rkeep) && any(ckeep)) {
      m <- m[rkeep, ckeep, drop = FALSE]
    } else {
      return(m[FALSE, FALSE, drop = FALSE])
    }
  }
  if (isTRUE(cluster) && nrow(m) > 1 && ncol(m) > 1) {
    drow <- tryCatch(stats::dist(m), error = function(e) NULL)
    if (!is.null(drow) && length(drow) > 0 && all(is.finite(drow))) {
      m <- m[stats::hclust(drow)$order, , drop = FALSE]
    }
    dcol <- tryCatch(stats::dist(t(m)), error = function(e) NULL)
    if (!is.null(dcol) && length(dcol) > 0 && all(is.finite(dcol))) {
      m <- m[, stats::hclust(dcol)$order, drop = FALSE]
    }
  }
  m
}

pprey_one_group <- function(mat, direction, group, stage, grp = NULL) {
  empty <- data.frame(Label = character(), Value = numeric(), Stage = character(),
                      stringsAsFactors = FALSE)
  if (is.null(mat) || !is.matrix(mat) || is.null(group) || !nzchar(as.character(group)) ||
      group %in% c("(all)")) {
    return(empty)
  }
  meta <- parse_pprey_rownames(rownames(mat))
  if (identical(direction, "prey")) {
    if (!group %in% colnames(mat)) return(empty)
    rows <- pprey_match_prey_stage(meta, stage)
    if (length(rows) == 0) return(empty)
    vals <- as.numeric(mat[rows, group])
    labs <- paste0(
      pprey_group_label(meta$pred_code[rows], grp),
      " · ",
      tolower(pprey_stage_label(meta$pred_stage[rows]))
    )
    df <- data.frame(
      Label = labs, Value = vals,
      Stage = pprey_stage_label(meta$pred_stage[rows]),
      stringsAsFactors = FALSE
    )
    df <- df[is.finite(df$Value) & df$Value > 0, , drop = FALSE]
    if (nrow(df) == 0) return(empty)
    df <- df[order(-df$Value, df$Label), , drop = FALSE]
    df$Label <- factor(df$Label, levels = rev(unique(df$Label)))
    attr(df, "title") <- paste(
      "Predators of", pprey_group_label(group, grp), pprey_stage_title(stage)
    )
    attr(df, "xlab") <- "Predator"
    return(df)
  }
  rows <- pprey_match_rows(meta, group, stage)
  if (length(rows) == 0) return(empty)
  pieces <- lapply(rows, function(i) {
    data.frame(
      Prey = colnames(mat),
      Value = as.numeric(mat[i, ]),
      PreyStage = pprey_stage_label(meta$prey_stage[i]),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, pieces)
  df$Label <- ifelse(
    df$PreyStage %in% c("Juvenile", "Adult"),
    paste0(pprey_group_label(df$Prey, grp), " · ", tolower(df$PreyStage), " prey"),
    pprey_group_label(df$Prey, grp)
  )
  df$Stage <- df$PreyStage
  df <- df[is.finite(df$Value) & df$Value > 0, , drop = FALSE]
  if (nrow(df) == 0) return(empty)
  df <- df[order(-df$Value, df$Label), , drop = FALSE]
  df$Label <- factor(df$Label, levels = rev(unique(as.character(df$Label))))
  attr(df, "title") <- paste(
    "Prey preferences —", pprey_group_label(group, grp), pprey_stage_title(stage)
  )
  attr(df, "xlab") <- "Prey"
  df
}

plot_pprey_one_group <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No preferences for that group and stage") +
             theme_reactiveatlantis())
  }
  pal <- c(Juvenile = "#1D7874", Adult = "#0B3C5D",
           `Biomass pool` = "#C4A35A", All = "#328CC1")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Label, y = .data$Value, fill = .data$Stage))
  p <- p + ggplot2::geom_col()
  p <- p + ggplot2::coord_flip()
  p <- p + ggplot2::scale_fill_manual(values = pal, na.value = "#328CC1")
  ttl <- attr(df, "title")
  if (is.null(ttl) || !nzchar(as.character(ttl)[1])) ttl <- "One-group preferences"
  xl <- attr(df, "xlab")
  p <- p + ggplot2::labs(title = ttl, x = xl, y = "pPREY")
  p <- p + theme_reactiveatlantis()
  p
}

pprey_preference_sums <- function(mat) {
  if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0) {
    return(data.frame(
      Predator = character(), Pred_code = character(), Pred_stage = character(),
      Prey_stage = character(),
      Preference_sum = numeric(), N_prey = integer(), Max = numeric(),
      Flag = character(), stringsAsFactors = FALSE
    ))
  }
  meta <- parse_pprey_rownames(rownames(mat))
  rs <- rowSums(mat, na.rm = TRUE)
  nprey <- as.integer(rowSums(mat > 0, na.rm = TRUE))
  mx <- apply(mat, 1, function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    max(x)
  })
  mn <- apply(mat, 1, function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NA_real_)
    min(x)
  })
  flag <- rep("ok", nrow(mat))
  flag[is.finite(rs) & rs == 0] <- "zero preferences"
  flag[is.finite(mx) & mx > 1] <- "value above 1"
  flag[is.finite(mn) & mn < 0] <- "negative value"
  nz <- is.finite(rs) & rs > 0
  if (sum(nz) >= 4) {
    mu <- mean(rs[nz])
    sdv <- stats::sd(rs[nz])
    if (is.finite(sdv) && sdv > 0) {
      odd <- nz & abs(rs - mu) > 3 * sdv
      flag[odd & flag == "ok"] <- "sum outlier"
    }
  }
  data.frame(
    Predator = rownames(mat),
    Pred_code = meta$pred_code,
    Pred_stage = pprey_stage_label(meta$pred_stage),
    Prey_stage = pprey_stage_label(meta$prey_stage),
    Preference_sum = as.numeric(rs),
    N_prey = nprey,
    Max = as.numeric(mx),
    Flag = flag,
    stringsAsFactors = FALSE
  )
}

pprey_uneaten_prey <- function(mat) {
  empty <- data.frame(Prey = character(), Preference_sum = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(mat) || !is.matrix(mat) || ncol(mat) == 0) return(empty)
  cs <- colSums(mat, na.rm = TRUE)
  nms <- colnames(mat)
  if (is.null(nms)) nms <- paste0("Prey", seq_len(ncol(mat)))
  hit <- nms[!is.finite(cs) | cs == 0]
  if (length(hit) == 0) return(empty)
  data.frame(Prey = as.character(hit), Preference_sum = 0, stringsAsFactors = FALSE)
}

pprey_dietcheck_mismatch <- function(ava_mat, diet, eps = 1e-3) {
  empty <- data.frame(
    Predator = character(), Prey = character(), Stage = character(),
    pPREY = numeric(), DietCheck = numeric(), Flag = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(ava_mat) || !is.matrix(ava_mat) || is.null(diet) ||
      !is.data.frame(diet) && !data.table::is.data.table(diet) ||
      nrow(diet) == 0) {
    return(empty)
  }
  eps <- suppressWarnings(as.numeric(eps)[1])
  if (!is.finite(eps)) eps <- 1e-3
  dt <- data.table::as.data.table(diet)
  if ("Group" %in% names(dt) && !"Predator" %in% names(dt)) {
    data.table::setnames(dt, "Group", "Predator")
  }
  if (!"Predator" %in% names(dt)) return(empty)
  dt[, Predator := as.character(Predator)]
  meta_cols <- intersect(c("Time", "Predator", "Stock", "Cohort", "Updated"), names(dt))
  prey_cols <- setdiff(names(dt), meta_cols)
  if (length(prey_cols) == 0) return(empty)
  for (col in prey_cols) {
    data.table::set(dt, j = col, value = as.numeric(dt[[col]]))
  }
  by <- intersect(c("Predator", "Cohort"), names(dt))
  agg <- dt[, lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE)),
            by = by, .SDcols = prey_cols]
  meta <- parse_pprey_rownames(rownames(ava_mat))
  out <- list()
  if ("Cohort" %in% names(agg)) {
    agg$StageNum <- ifelse(as.numeric(agg$Cohort) <= 1, 1L, 2L)
    for (i in seq_len(nrow(agg))) {
      pd <- agg$Predator[i]
      st <- agg$StageNum[i]
      rows <- which(meta$pred_code == pd & pprey_match_stage(meta$pred_stage, as.character(st)))
      if (length(rows) == 0) rows <- which(meta$pred_code == pd)
      for (py in prey_cols) {
        pprey_val <- if (length(rows) && py %in% colnames(ava_mat)) {
          max(as.numeric(ava_mat[rows, py]), na.rm = TRUE)
        } else 0
        diet_val <- as.numeric(agg[[py]][i])
        if (!is.finite(pprey_val)) pprey_val <- 0
        if (!is.finite(diet_val)) diet_val <- 0
        flag <- NA_character_
        if (pprey_val > eps && diet_val <= eps) flag <- "pPREY > 0, DietCheck ~ 0"
        if (diet_val > eps && pprey_val <= eps) flag <- "DietCheck > 0, pPREY = 0"
        if (!is.na(flag)) {
          out[[length(out) + 1]] <- data.frame(
            Predator = pd, Prey = py,
            Stage = pprey_stage_label(st),
            pPREY = pprey_val, DietCheck = diet_val, Flag = flag,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  } else {
    preds <- unique(c(as.character(agg$Predator), unique(meta$pred_code)))
    preys <- unique(c(prey_cols, colnames(ava_mat)))
    for (pd in preds) {
      rows <- which(meta$pred_code == pd)
      diet_row <- agg[Predator == pd]
      for (py in preys) {
        pprey_val <- if (length(rows) && py %in% colnames(ava_mat)) {
          max(as.numeric(ava_mat[rows, py]), na.rm = TRUE)
        } else 0
        diet_val <- if (nrow(diet_row) && py %in% names(diet_row)) {
          max(as.numeric(diet_row[[py]]), na.rm = TRUE)
        } else 0
        if (!is.finite(pprey_val)) pprey_val <- 0
        if (!is.finite(diet_val)) diet_val <- 0
        flag <- NA_character_
        if (pprey_val > eps && diet_val <= eps) flag <- "pPREY > 0, DietCheck ~ 0"
        if (diet_val > eps && pprey_val <= eps) flag <- "DietCheck > 0, pPREY = 0"
        if (!is.na(flag)) {
          out[[length(out) + 1]] <- data.frame(
            Predator = pd, Prey = py, Stage = "group",
            pPREY = pprey_val, DietCheck = diet_val, Flag = flag,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (!length(out)) return(empty)
  do.call(rbind, out)
}

pprey_diagnostics <- function(mat) {
  list(sums = pprey_preference_sums(mat), uneaten = pprey_uneaten_prey(mat))
}

pprey_diagnostics_table <- function(mat, diet = NULL, eps = 1e-3) {
  sums <- pprey_preference_sums(mat)
  odd <- sums[sums$Flag != "ok", , drop = FALSE]
  rows <- list()
  if (nrow(odd) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      Type = "preference_sum",
      Group = odd$Pred_code,
      Stage = odd$Pred_stage,
      Detail = odd$Flag,
      Value = odd$Preference_sum,
      stringsAsFactors = FALSE
    )
  }
  uneaten <- pprey_uneaten_prey(mat)
  if (nrow(uneaten) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      Type = "uneaten_prey",
      Group = uneaten$Prey,
      Stage = "",
      Detail = "nobody eats this",
      Value = uneaten$Preference_sum,
      stringsAsFactors = FALSE
    )
  }
  mm <- pprey_dietcheck_mismatch(mat, diet, eps)
  if (nrow(mm) > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      Type = "dietcheck",
      Group = mm$Predator,
      Stage = mm$Stage,
      Detail = paste0(mm$Flag, " (prey ", mm$Prey, ")"),
      Value = mm$DietCheck,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) {
    return(data.frame(Type = character(), Group = character(), Stage = character(),
                      Detail = character(), Value = numeric(),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

plot_pprey_sum_flags <- function(sums) {
  if (is.null(sums) || nrow(sums) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No pPREY rows to diagnose") +
             theme_reactiveatlantis())
  }
  df <- sums
  df$Label <- paste0(
    df$Pred_code, " · ", tolower(df$Pred_stage),
    " eating ", tolower(df$Prey_stage)
  )
  df <- df[order(df$Preference_sum, df$Label), , drop = FALSE]
  df$Label <- factor(df$Label, levels = unique(df$Label))
  pal <- c(ok = "#1D7874", `zero preferences` = "#A33B20",
           `value above 1` = "#C4A35A", `negative value` = "#0B3C5D",
           `sum outlier` = "#328CC1")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Label, y = .data$Preference_sum,
                                        fill = .data$Flag))
  p <- p + ggplot2::geom_col()
  flagged <- df[df$Flag != "ok", , drop = FALSE]
  if (nrow(flagged) > 0) {
    p <- p + ggplot2::geom_text(
      data = flagged,
      ggplot2::aes(label = .data$Flag),
      hjust = -0.08, size = 3, colour = "#1B2833"
    )
  }
  p <- p + ggplot2::coord_flip(clip = "off")
  p <- p + ggplot2::scale_fill_manual(values = pal, na.value = "#5C6B73")
  p <- p + ggplot2::labs(
    title = "Predator preference sums",
    subtitle = "Flags: zero diet, values outside 0-1, or 3-sd sum outliers",
    x = "Predator", y = "Sum of pPREY"
  )
  p <- p + theme_reactiveatlantis()
  p
}

pprey_changed_cells <- function(original, edited) {
  empty <- data.frame(
    Predator = character(), Prey = character(),
    Original = numeric(), Edited = numeric(), Delta = numeric(),
    stringsAsFactors = FALSE
  )
  if (is.null(original) || is.null(edited) || !is.matrix(original) || !is.matrix(edited)) {
    return(empty)
  }
  if (!identical(dim(original), dim(edited)) ||
      !identical(rownames(original), rownames(edited)) ||
      !identical(colnames(original), colnames(edited))) {
    return(empty)
  }
  delta <- edited - original
  hit <- which(is.finite(delta) & delta != 0, arr.ind = TRUE)
  if (nrow(hit) == 0) return(empty)
  data.frame(
    Predator = rownames(original)[hit[, 1]],
    Prey = colnames(original)[hit[, 2]],
    Original = original[hit],
    Edited = edited[hit],
    Delta = delta[hit],
    stringsAsFactors = FALSE
  )
}

plot_pprey_compare <- function(original, edited) {
  ch <- pprey_changed_cells(original, edited)
  if (is.null(ch) || nrow(ch) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::labs(title = "No edited cells — original and current matrices match") +
             theme_reactiveatlantis())
  }
  dat <- ch
  dat$Predator <- factor(dat$Predator, levels = unique(dat$Predator))
  dat$Prey <- factor(dat$Prey, levels = unique(dat$Prey))
  lim <- max(abs(dat$Delta), na.rm = TRUE)
  if (!is.finite(lim) || lim == 0) lim <- 1
  dat$Lab <- paste0(signif(dat$Original, 3), " → ", signif(dat$Edited, 3))
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$Prey, y = .data$Predator, fill = .data$Delta))
  p <- p + ggplot2::geom_tile(colour = "grey85", linewidth = 0.15)
  p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$Lab), size = 3, colour = "#1B2833")
  p <- p + ggplot2::scale_fill_gradient2(
    low = "#A33B20", mid = "#F7F4EC", high = "#1D7874",
    midpoint = 0, limits = c(-lim, lim), name = "Edited − original"
  )
  p <- p + theme_reactiveatlantis()
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size = 8),
    axis.text.y = ggplot2::element_text(size = 8),
    legend.position = "right",
    panel.grid = ggplot2::element_blank()
  )
  p <- p + ggplot2::labs(
    x = "Prey", y = "Predator",
    title = "Changed pPREY cells only"
  )
  p <- p + ggplot2::scale_x_discrete(position = "top")
  p
}

pprey_apply_edit <- function(mat, pred_group, pred_stage, prey_group, prey_stage, value) {
  if (is.null(mat) || !is.matrix(mat)) {
    return(list(mat = mat, message = "No pPREY matrix to edit."))
  }
  val <- suppressWarnings(as.numeric(value)[1])
  if (!is.finite(val)) {
    return(list(mat = mat, message = "New value must be numeric."))
  }
  if (is.null(pred_group) || pred_group %in% c("", "(all)")) {
    return(list(mat = mat, message = "Pick a predator group name to edit."))
  }
  if (is.null(prey_group) || prey_group %in% c("", "(all)")) {
    return(list(mat = mat, message = "Pick a prey group name to edit."))
  }
  if (!prey_group %in% colnames(mat)) {
    return(list(mat = mat, message = paste("Prey", prey_group, "is not a matrix column.")))
  }
  meta <- parse_pprey_rownames(rownames(mat))
  rows <- pprey_match_rows(meta, pred_group, pred_stage)
  if (!identical(pprey_stage_wanted(prey_stage), "all")) {
    rows <- intersect(rows, pprey_match_prey_stage(meta, prey_stage))
  }
  if (length(rows) == 0) {
    return(list(mat = mat, message = "No pPREY row matches that predator and prey stage."))
  }
  col <- which(colnames(mat) == prey_group)[1]
  mat[rows, col] <- val
  list(
    mat = mat,
    message = paste0(
      "Set ", length(rows), " cell", if (length(rows) == 1) "" else "s",
      " to ", val, "."
    )
  )
}
