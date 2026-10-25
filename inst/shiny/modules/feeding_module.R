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
          choices = c(
            "pPREY matrix" = "matrix",
            "Predation pressure" = "pressure",
            "Diagnostics" = "diagnostics",
            "Compare edited vs original" = "compare"
          ),
          selected = "matrix"
        ),
        tags$hr(),
        tags$strong("Predator"),
        selectInput(ns("pred_group"), "Group name", choices = c("(none)" = "(none)")),
        selectInput(ns("pred_stage"), "Adult / juvenile",
                    choices = c("Juvenile" = "1", "Adult" = "2",
                                "Biomass pool" = "pool"),
                    selected = "1"),
        tags$strong("Prey"),
        selectInput(ns("prey_group"), "Group name", choices = c("(none)" = "(none)")),
        selectInput(ns("prey_stage"), "Adult / juvenile",
                    choices = c("Juvenile" = "1", "Adult" = "2"),
                    selected = "1"),
        textOutput(ns("cell_info")),
        p(class = "ra-muted",
          "The full matrix stays on screen. Hover a cell for the value and groups. Click a cell to set these dropdowns and highlight that row and column."),
        conditionalPanel(
          "input.view_type == 'pressure'", ns = ns,
          tags$hr(),
          tags$strong("Expected pressure"),
          selectInput(
            ns("pressure_metric"), "Cell value",
            choices = c(
              "Prey loss fraction / day" = "frac_day",
              "Consumption (t / day)" = "t_day",
              "Prey loss fraction / year" = "frac_year"
            ),
            selected = "frac_day"
          ),
          selectInput(
            ns("pressure_source"), "Biomass source",
            choices = c(
              "Initial conditions (init.nc)" = "init",
              "Output biomass (output.nc)" = "output"
            ),
            selected = "init"
          ),
          sliderInput(ns("pressure_time_i"), "Time step",
                      min = 1, max = 1, value = 1, step = 1),
          textOutput(ns("pressure_time_lab")),
          selectInput(ns("pressure_box"), "Boxes", choices = c("All boxes" = "all")),
          p(class = "ra-muted",
            "Holling Type I/II/III from biology.prm (C, mum, E, predcase, KLP/KUP).",
            "System-wide sums per-box calculations. Output uses biomass at the selected time step.")
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
            "Changed cells only. Use the group-name and adult/juvenile selectors to pick a cell.")
        )
      ),
      conditionalPanel(
        "input.view_type == 'matrix' || input.view_type == 'pressure'", ns = ns,
        uiOutput(ns("matrix_caption")),
        div(class = "ra-scroll-plot",
            plotly::plotlyOutput(ns("feeding_plot_gg"), height = "720px")),
        conditionalPanel(
          "input.view_type == 'pressure'", ns = ns,
          p(class = "ra-muted", textOutput(ns("pressure_note"), inline = TRUE))
        ),
        fluidRow(
          column(6, plotOutput(ns("diet_prop_plot"), height = "280px")),
          column(6, plotOutput(ns("consumed_plot"), height = "280px"))
        ),
        fluidRow(
          column(6, plotOutput(ns("pred_bars"), height = "280px")),
          column(6, plotOutput(ns("prey_bars"), height = "280px"))
        ),
        conditionalPanel(
          "input.view_type == 'pressure'", ns = ns,
          h5("Pressure per box"),
          p(class = "ra-muted",
            "Selected predator–prey pair. Colour matches the heatmap metric. Flags in the table mark loss above standing stock, 5% per day, or prey production."),
          plotly::plotlyOutput(ns("pressure_map"), height = "420px")
        )
      ),
      conditionalPanel(
        "input.view_type == 'diagnostics'", ns = ns,
        plotOutput(ns("diag_plot"), height = "480px")
      ),
      conditionalPanel(
        "input.view_type == 'compare'", ns = ns,
        plotOutput(ns("compare_plot"), height = "480px")
      ),
      DT::dataTableOutput(ns("bug_table"))
    )
  )
}

feeding_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ps <- shiny::reactive(project_snapshot(project))
    rv <- reactiveValues(
      ava_mat = NULL, overlap_mat = NULL, real_feed = NULL,
      pressure = NULL, bugs = NULL, built = FALSE, token = NULL,
      edited = NULL, original_key = NULL, edit_msg = ""
    )

    output$banner <- renderUI(tool_banner(ps(), "feeding"))

    output$matrix_caption <- renderUI({
      if (identical(input$view_type, "pressure")) {
        p(class = "ra-muted",
          "Screening index, not Atlantis intake: each cell is a simplified Holling calculation from pPREY, C, mum and biomass at the selected time (box totals, no Q10, satiation cap or refuge). Use it to rank links. On NESP it ran about 35x above the intake Atlantis realised, which is shown in the table for the selected pair.")
      } else {
        p(class = "ra-muted",
          "Every predator x prey cell is drawn, including zeros. Scroll or zoom if the matrix is large.")
      }
    })

    pprey_mat <- reactive({
      dat <- ps()$data
      req(isTRUE(ps()$ready), dat$grp, dat$prm_bio)
      grp <- dat$grp
      if (any(grepl("invertype", names(grp)))) {
        names(grp)[which(grepl("invertype", names(grp)))] <- "grouptype"
      }
      ava_mat <- tryCatch(read_pprey_matrix(dat$prm_bio), error = function(e) NULL)
      if (is.null(ava_mat)) {
        options(warn = -1)
        ava_mat <- tryCatch(
          text2num(dat$prm_bio, "pPREY", Vector = TRUE, pprey = TRUE),
          error = function(e) NULL
        )
        options(warn = 0)
      }
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

    pressure_nc_info <- reactive({
      req(identical(input$view_type, "pressure"))
      src <- if (is.null(input$pressure_source)) "init" else as.character(input$pressure_source)[1]
      need <- if (identical(src, "output")) "nc_out" else "nc_init"
      dat <- tryCatch(ensure_project_data(ps(), need), error = function(e) NULL)
      if (is.null(dat)) dat <- ps()$data
      paths <- tryCatch(normalize_paths(ps()$paths), error = function(e) empty_paths())
      if (!is.null(dat$paths)) {
        dp <- tryCatch(normalize_paths(dat$paths), error = function(e) NULL)
        if (!is.null(dp)) {
          if (path_ok(dp$init_nc)) paths$init_nc <- dp$init_nc
          if (path_ok(dp$output_nc)) paths$output_nc <- dp$output_nc
        }
      }
      nc <- NULL
      if (identical(src, "output")) {
        if (!is.null(dat$nc_out) && inherits(dat$nc_out, "ncdf4")) {
          nc <- dat$nc_out
        } else if (path_ok(paths$output_nc)) {
          nc <- tryCatch(nc_handle(paths$output_nc), error = function(e) NULL)
        }
      }
      if (is.null(nc)) {
        if (!is.null(dat$nc_init) && inherits(dat$nc_init, "ncdf4")) {
          nc <- dat$nc_init
        } else if (path_ok(paths$init_nc)) {
          nc <- tryCatch(nc_handle(paths$init_nc), error = function(e) NULL)
        }
        if (identical(src, "output") && !is.null(nc)) {
          src <- "init"
        }
      }
      list(nc = nc, src = src, paths = paths, dat = dat)
    })

    observe({
      info <- tryCatch(pressure_nc_info(), error = function(e) NULL)
      n_t <- if (is.null(info) || is.null(info$nc)) 1L else nc_n_times(info$nc)
      shiny::updateSliderInput(session, "pressure_time_i",
                               min = 1, max = max(1L, as.integer(n_t)))
    })

    output$pressure_time_lab <- renderText({
      if (!identical(input$view_type, "pressure")) return("")
      info <- tryCatch(pressure_nc_info(), error = function(e) NULL)
      src <- if (!is.null(info) && !is.null(info$src)) info$src else "init"
      ti <- if (is.null(input$pressure_time_i)) 1L else as.integer(input$pressure_time_i)[1]
      paste0(
        if (identical(src, "output")) "Output biomass" else "Initial conditions",
        ", time step ", ti
      )
    })

    pressure_stocks <- reactive({
      req(identical(input$view_type, "pressure"))
      ava <- current_mat()
      req(ava)
      info <- pressure_nc_info()
      dat <- info$dat
      grp <- dat$grp
      if (is.null(grp)) grp <- ps()$data$grp
      prm <- dat$prm_bio
      if (is.null(prm)) prm <- ps()$data$prm_bio
      time_i <- if (is.null(input$pressure_time_i)) 1L else as.integer(input$pressure_time_i)[1]
      if (!is.finite(time_i) || time_i < 1) time_i <- 1L
      if (is.null(info$nc)) return(NULL)
      ra_progress("Reading biomass for predation pressure", {
        init_stage_stocks(info$nc, grp, prm, time_i = time_i)
      })
    })

    pressure_built <- reactive({
      req(identical(input$view_type, "pressure"))
      ava <- current_mat()
      req(ava)
      info <- pressure_nc_info()
      dat <- info$dat
      grp <- dat$grp
      if (is.null(grp)) grp <- ps()$data$grp
      prm <- dat$prm_bio
      if (is.null(prm)) prm <- ps()$data$prm_bio
      time_i <- if (is.null(input$pressure_time_i)) 1L else as.integer(input$pressure_time_i)[1]
      if (!is.finite(time_i) || time_i < 1) time_i <- 1L
      box <- if (is.null(input$pressure_box)) "all" else input$pressure_box
      stocks <- tryCatch(pressure_stocks(), error = function(e) NULL)
      ra_progress("Expected predation pressure", {
        expected_predation_pressure(
          ava, grp, prm, info$nc, box = box, time_i = time_i,
          stocks = stocks
        )
      })
    })

    observe({
      press <- tryCatch(pressure_built(), error = function(e) NULL)
      n_box <- if (is.null(press)) 0L else as.integer(press$n_box)
      ch <- c("All boxes" = "all")
      if (is.finite(n_box) && n_box > 0) {
        ids <- as.character(seq_len(n_box) - 1L)
        ch <- c(ch, stats::setNames(ids, paste("Box", ids)))
      }
      update_select_keep(session, "pressure_box", ch, input$pressure_box)
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

    feed_grp <- reactive({
      tryCatch(ps()$data$grp, error = function(e) NULL)
    })

    selected_cell <- reactive({
      ava <- current_mat()
      req(ava)
      pprey_resolve_selection(
        ava,
        pred_group = input$pred_group,
        pred_stage = input$pred_stage,
        prey_group = input$prey_group,
        prey_stage = input$prey_stage
      )
    })

    output$cell_info <- renderText({
      ava <- current_mat()
      if (is.null(ava)) return("Load biology.prm to show the matrix.")
      sel <- tryCatch(selected_cell(), error = function(e) NULL)
      if (identical(input$view_type, "pressure")) {
        press <- tryCatch(pressure_built(), error = function(e) NULL)
        if (is.null(press) || is.null(press$prey_loss_frac_day)) {
          return("Load biology.prm, groups and a biomass NetCDF to compute expected predation pressure.")
        }
        return(pprey_cell_info_text(press$prey_loss_frac_day, sel, feed_grp(),
                                   value_label = "Pressure / day"))
      }
      pprey_cell_info_text(ava, sel, feed_grp())
    })

    output$pressure_note <- renderText({
      if (!identical(input$view_type, "pressure")) return("")
      press <- tryCatch(pressure_built(), error = function(e) NULL)
      if (is.null(press)) return(pressure_equation_note())
      paste(press$notes, pressure_equation_note())
    })

    observeEvent(plotly::event_data("plotly_click", source = ns("feed_mat")), {
      ev <- plotly::event_data("plotly_click", source = ns("feed_mat"))
      ava <- current_mat()
      if (is.null(ava) || is.null(ev)) return()
      hit <- pprey_selection_from_click(ava, ev$y, ev$x, feed_grp())
      if (is.null(hit)) return()
      shiny::updateSelectInput(session, "pred_group", selected = hit$pred_group)
      shiny::updateSelectInput(session, "pred_stage", selected = hit$pred_stage)
      shiny::updateSelectInput(session, "prey_group", selected = hit$prey_group)
      shiny::updateSelectInput(session, "prey_stage", selected = hit$prey_stage)
    }, ignoreNULL = TRUE)

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

    display_mat <- reactive({
      ava <- current_mat()
      if (!identical(input$view_type, "pressure")) return(ava)
      press <- tryCatch(pressure_built(), error = function(e) NULL)
      metric <- if (is.null(input$pressure_metric)) "frac_day" else input$pressure_metric
      mat <- pressure_metric_matrix(press, metric)
      if (is.null(mat)) ava else mat
    })

    output$feeding_plot_gg <- plotly::renderPlotly({
      ava <- current_mat()
      if (is.null(ava) || !is.matrix(ava) || nrow(ava) == 0) {
        p <- ggplot2::ggplot() +
          ggplot2::labs(title = "Load biology.prm to plot the feeding matrix") +
          theme_reactiveatlantis()
        plt <- ra_ggplotly(p, tooltip = "text")
        if (ra_is_plotly(plt)) plt$x$source <- ns("feed_mat")
        return(plt)
      }
      sel <- tryCatch(selected_cell(), error = function(e) NULL)
      if (identical(input$view_type, "pressure")) {
        press <- tryCatch(pressure_built(), error = function(e) NULL)
        metric <- if (is.null(input$pressure_metric)) "frac_day" else input$pressure_metric
        z <- pressure_metric_matrix(press, metric)
        if (is.null(z)) z <- ava * 0
        hover <- if (is.null(press)) NULL else pressure_hover_text(ava, press, feed_grp())
        plt <- plot_feed_matrix_plotly(
          z, sel, feed_grp(), hover = hover,
          title = paste0(pressure_metric_label(metric), " — ",
                         nrow(z), " predators x ", ncol(z), " prey"),
          colorbar = switch(as.character(metric),
                            t_day = "t / day",
                            frac_year = "/ year",
                            "/ day")
        )
      } else {
        plt <- plot_feed_matrix_plotly(ava, sel, feed_grp())
      }
      if (ra_is_plotly(plt)) {
        plt$x$source <- ns("feed_mat")
        tryCatch(plotly::event_register(plt, "plotly_click"), error = function(e) plt)
      }
      plt
    })

    output$pred_bars <- renderPlot({
      plot_or_message("Select a predator group and adult/juvenile", {
        mat <- display_mat()
        req(mat)
        if (pprey_is_none(input$pred_group)) {
          return(ggplot2::ggplot() +
                   ggplot2::labs(title = "Select a predator (or click a matrix cell)") +
                   theme_reactiveatlantis())
        }
        df <- pprey_one_group(mat, "predator", input$pred_group, input$pred_stage,
                              feed_grp())
        ylab <- if (identical(input$view_type, "pressure")) {
          pressure_metric_label(input$pressure_metric)
        } else {
          "pPREY"
        }
        if (identical(input$view_type, "pressure")) {
          ttl <- attr(df, "title")
          if (is.null(ttl)) ttl <- ""
          attr(df, "title") <- sub("Prey preferences", "Pressure from", ttl, fixed = TRUE)
          if (!nzchar(attr(df, "title"))) {
            attr(df, "title") <- "Pressure from the selected predator"
          }
        }
        plot_pprey_one_group(df, ylab = ylab)
      })
    })

    output$prey_bars <- renderPlot({
      plot_or_message("Select a prey group and adult/juvenile", {
        mat <- display_mat()
        req(mat)
        if (pprey_is_none(input$prey_group)) {
          return(ggplot2::ggplot() +
                   ggplot2::labs(title = "Select a prey (or click a matrix cell)") +
                   theme_reactiveatlantis())
        }
        df <- pprey_one_group(mat, "prey", input$prey_group, input$prey_stage,
                              feed_grp())
        ylab <- if (identical(input$view_type, "pressure")) {
          pressure_metric_label(input$pressure_metric)
        } else {
          "pPREY"
        }
        if (identical(input$view_type, "pressure")) {
          ttl <- attr(df, "title")
          if (is.null(ttl)) ttl <- ""
          attr(df, "title") <- sub("Predators of", "Pressure on", ttl, fixed = TRUE)
        }
        plot_pprey_one_group(df, ylab = ylab)
      })
    })

    output$diag_plot <- renderPlot({
      plot_or_message("Load biology.prm to run feeding diagnostics", {
        ava <- current_mat()
        req(ava)
        plot_pprey_sum_flags(pprey_preference_sums(ava))
      })
    })

    output$compare_plot <- renderPlot({
      plot_or_message("Load biology.prm to compare an edited matrix", {
        orig <- pprey_mat()
        ed <- current_mat()
        req(orig, ed)
        plot_pprey_compare(orig, ed)
      })
    })

    diet_pair <- reactive({
      pred <- input$pred_group
      prey <- input$prey_group
      if (pprey_is_none(pred) || pprey_is_none(prey)) {
        out <- data.frame(Time = numeric(), Proportion = numeric(),
                          stringsAsFactors = FALSE)
        attr(out, "reason") <- "Select a predator and prey (or click a matrix cell)."
        return(out)
      }
      dat <- tryCatch(ensure_project_data(ps(), "diet"), error = function(e) NULL)
      diet <- if (!is.null(dat$diet_time)) dat$diet_time else dat$diet_full
      dietcheck_pair_series(diet, pred, prey)
    })

    consumed_pair <- reactive({
      pred <- input$pred_group
      prey <- input$prey_group
      if (pprey_is_none(pred) || pprey_is_none(prey)) {
        out <- data.frame(Time = numeric(), Tonnes = numeric(),
                          stringsAsFactors = FALSE)
        attr(out, "reason") <- "Select a predator and prey (or click a matrix cell)."
        return(out)
      }
      dat <- tryCatch(ensure_project_data(ps(), c("nc_prod", "nc_out", "diet")),
                      error = function(e) NULL)
      if (is.null(dat) || is.null(dat$nc_prod)) {
        out <- data.frame(Time = numeric(), Tonnes = numeric(),
                          stringsAsFactors = FALSE)
        attr(out, "reason") <- "Load outputPROD.nc to plot consumed biomass."
        return(out)
      }
      tot <- tryCatch(consumed_tonnes(dat$nc_prod, dat$nc_out, dat$grp, pred,
                                      prm = dat$prm_bio),
                      error = function(e) NULL)
      diet <- if (!is.null(dat$diet_time)) dat$diet_time else dat$diet_full
      consumed_pair_series(tot, diet, pred, prey, diet_full = dat$diet_full)
    })

    output$diet_prop_plot <- renderPlot({
      plot_or_message("Load DietCheck.txt to plot realised diet", {
        df <- diet_pair()
        if (is.null(df) || nrow(df) == 0) {
          plot_message(attr(df, "reason") %||% "Load DietCheck.txt to plot realised diet")
          return(invisible(NULL))
        }
        origin <- tryCatch(ensure_project_data(ps(), "nc_out")$origin,
                           error = function(e) ps()$data$origin)
        df$Date <- days_as_date(df$Time, origin)
        p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Proportion))
        p <- p + ggplot2::geom_line(linewidth = 0.9, colour = "#0B3C5D", na.rm = TRUE)
        p <- p + ggplot2::geom_point(size = 1.6, colour = "#0B3C5D", na.rm = TRUE)
        p <- p + ggplot2::labs(
          title = paste("DietCheck —", input$pred_group, "eating", input$prey_group),
          x = "Year", y = "Diet proportion"
        )
        p <- p + theme_reactiveatlantis()
        ra_scale_x_year(p, df$Date)
      })
    })

    output$consumed_plot <- renderPlot({
      plot_or_message("Load outputPROD.nc to plot consumed biomass", {
        df <- consumed_pair()
        if (is.null(df) || nrow(df) == 0) {
          plot_message(attr(df, "reason") %||% "Load outputPROD.nc to plot consumed biomass")
          return(invisible(NULL))
        }
        origin <- tryCatch(ensure_project_data(ps(), "nc_out")$origin,
                           error = function(e) ps()$data$origin)
        df$Date <- days_as_date(df$Time, origin)
        pal <- tryCatch(get_fg_colors(input$prey_group, ps()$data$fg_palette),
                        error = function(e) c("#A33B20"))
        col <- if (length(pal)) unname(pal[1]) else "#A33B20"
        p <- ggplot2::ggplot(df, ggplot2::aes(x = Date, y = Tonnes))
        p <- p + ggplot2::geom_line(linewidth = 0.9, colour = col, na.rm = TRUE)
        p <- p + ggplot2::geom_point(size = 1.6, colour = col, na.rm = TRUE)
        p <- p + ggplot2::labs(
          title = paste("Consumed biomass —", input$pred_group, "on", input$prey_group),
          x = "Year", y = "Tonnes"
        )
        p <- p + theme_reactiveatlantis()
        ra_scale_x_year(p, df$Date)
      })
    })

    pressure_by_box <- reactive({
      req(identical(input$view_type, "pressure"))
      pred <- input$pred_group
      prey <- input$prey_group
      if (pprey_is_none(pred) || pprey_is_none(prey)) {
        return(NULL)
      }
      press <- tryCatch(pressure_built(), error = function(e) NULL)
      ava <- current_mat()
      req(ava)
      info <- tryCatch(pressure_nc_info(), error = function(e) NULL)
      dat <- if (!is.null(info)) info$dat else ps()$data
      time_i <- if (is.null(input$pressure_time_i)) 1L else as.integer(input$pressure_time_i)[1]
      if (!is.finite(time_i) || time_i < 1) time_i <- 1L
      pressure_pair_by_box(
        ava, dat$grp, dat$prm_bio,
        if (is.null(info)) NULL else info$nc,
        pred, prey,
        pred_stage = input$pred_stage,
        prey_stage = input$prey_stage,
        time_i = time_i,
        stocks = if (is.null(press)) NULL else press$stocks
      )
    })

    bgm_poly <- reactive({
      snap <- ps()
      dat <- snap$data
      path <- find_bgm_path(
        explicit = c(
          if (!is.null(dat)) dat$bgm_path else NA_character_,
          snap$paths$bgm
        ),
        config_dir = snap$config_dir,
        output_dir = snap$output_dir,
        prm_paths = c(snap$paths$run_prm, snap$paths$bio_prm, snap$paths$force_prm)
      )
      if (!path_ok(path)) return(NULL)
      read_bgm_polygons(path)
    })

    output$pressure_map <- plotly::renderPlotly({
      if (!identical(input$view_type, "pressure")) {
        return(spatial_empty_plotly("Switch to Predation pressure to map per-box loss."))
      }
      if (pprey_is_none(input$pred_group) || pprey_is_none(input$prey_group)) {
        return(spatial_empty_plotly(
          "Select a predator and prey (or click a matrix cell) to map pressure per box."
        ))
      }
      poly <- tryCatch(bgm_poly(), error = function(e) NULL)
      by_box <- tryCatch(pressure_by_box(), error = function(e) NULL)
      if (is.null(poly) || nrow(poly) == 0) {
        return(spatial_empty_plotly("Load a BGM file to map predation pressure per box."))
      }
      metric <- if (is.null(input$pressure_metric)) "frac_day" else input$pressure_metric
      poly <- pressure_map_polygons(poly, by_box, metric)
      sel <- input$pressure_box
      sel_box <- if (is.null(sel) || identical(as.character(sel)[1], "all")) {
        NULL
      } else {
        suppressWarnings(as.integer(sel)[1])
      }
      vals <- if ("Value" %in% names(poly)) poly$Value else numeric()
      fill_lim <- suppressWarnings(range(vals, na.rm = TRUE))
      if (!all(is.finite(fill_lim)) || !is.finite(diff(fill_lim)) ||
          diff(fill_lim) == 0) {
        mx <- if (length(vals) && any(is.finite(vals))) {
          max(vals[is.finite(vals)], na.rm = TRUE)
        } else {
          0
        }
        fill_lim <- c(0, if (is.finite(mx) && mx > 0) mx else 1)
      }
      plot_spatial_map_plotly(
        poly,
        fill_lim = fill_lim,
        fill_high = "#A33B20",
        selected_box = sel_box,
        title = paste0(
          pressure_metric_label(metric), " — ",
          input$pred_group, " on ", input$prey_group
        ),
        legend = switch(as.character(metric),
                        t_day = "t / day",
                        frac_year = "/ year",
                        "/ day")
      )
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
      if (identical(input$view_type, "pressure")) {
        press <- tryCatch(pressure_built(), error = function(e) NULL)
        if (is.null(press) || is.null(press$cells)) {
          return(ra_datatable(data.frame(
            Note = "Load biology.prm, groups and a biomass NetCDF to compute expected predation pressure."
          )))
        }
        sel <- tryCatch(selected_cell(), error = function(e) NULL)
        tab <- pressure_cell_detail(press, sel)
        if (is.null(tab) || nrow(tab) == 0) {
          tab <- pressure_flag_table(press)
        }
        cmp <- tryCatch({
          dat <- tryCatch(
            ensure_project_data(ps(), c("diet", "nc_prod", "nc_out")),
            error = function(e) NULL
          )
          if (is.null(dat)) dat <- tryCatch(ensure_diet(ps()$data, ps()$paths),
                                           error = function(e) ps()$data)
          diet <- if (!is.null(dat$diet_full)) dat$diet_full else dat$diet_time
          pred_mort <- dat$pred_mort
          if (is.null(pred_mort)) {
            pred_mort <- tryCatch(ps()$data$pred_mort, error = function(e) NULL)
          }
          time_i <- if (is.null(input$pressure_time_i)) 1L else {
            as.integer(input$pressure_time_i)[1]
          }
          if (!is.finite(time_i) || time_i < 1) time_i <- 1L
          prod <- tryCatch(
            prey_production_t_day(dat$nc_prod, dat$nc_out, dat$grp,
                                  input$prey_group, time_i),
            error = function(e) NA_real_
          )
          if (is.finite(prod) && is.data.frame(tab) && "Prey" %in% names(tab)) {
            tab <- pressure_flag_vs_production(tab, input$prey_group, prod)
          }
          real <- tryCatch({
            tot <- consumed_tonnes(dat$nc_prod, dat$nc_out, dat$grp, input$pred_group,
                                   prm = dat$prm_bio)
            t_days <- nc_t_days(dat$nc_out)
            t_sel <- if (length(t_days) >= time_i) t_days[time_i] else NA_real_
            i <- which.min(abs(tot$time - t_sel))
            pair <- consumed_pair_series(tot, dat$diet_time, input$pred_group,
                                         input$prey_group, diet_full = dat$diet_full)
            j <- if (nrow(pair)) which.min(abs(pair$Time - t_sel)) else integer()
            c(pred = if (length(i)) tot$tonnes[i] else NA_real_,
              pair = if (length(j)) pair$Tonnes[j] else NA_real_)
          }, error = function(e) c(pred = NA_real_, pair = NA_real_))
          pressure_vs_realised(
            press, diet, pred_mort, input$pred_group, input$prey_group, prod,
            realised_pred_t_day = real[["pred"]], realised_pair_t_day = real[["pair"]]
          )
        }, error = function(e) NULL)
        if (!is.null(cmp) && nrow(cmp) > 0 &&
            !pprey_is_none(input$pred_group) && !pprey_is_none(input$prey_group)) {
          extra <- data.frame(
            Predator_row = cmp$Source,
            Pred_code = input$pred_group,
            Pred_stage = "",
            Prey = input$prey_group,
            Prey_stage = "",
            pPREY = NA_real_,
            Gape = NA_real_,
            predcase = NA_integer_,
            Consumption_t_day = NA_real_,
            Prey_loss_frac_day = cmp$Value,
            Prey_loss_frac_year = NA_real_,
            Prey_tonnes = NA_real_,
            Flag = cmp$Note,
            stringsAsFactors = FALSE
          )
          extra <- extra[, intersect(names(tab), names(extra)), drop = FALSE]
          if (ncol(extra) && ncol(tab)) {
            missing <- setdiff(names(tab), names(extra))
            for (nm in missing) extra[[nm]] <- NA
            extra <- extra[, names(tab), drop = FALSE]
            tab <- rbind(tab, extra)
          }
        }
        return(ra_datatable(tab, options = list(pageLength = 10, dom = "tip")))
      }
      ra_datatable(data.frame(
        Note = "Hover a cell for pPREY and group names. Click to select that predator and prey."
      ))
    })

    keep_outputs_alive(output, "banner")
    keep_outputs_alive(output, "feeding_plot_gg")
    keep_outputs_alive(output, "matrix_caption")
    keep_outputs_alive(output, "pressure_map")
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
        Biom.N[code, cohort] <- sum((StructN + ReservN) * Numb, na.rm = TRUE)
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
  ## Atlantis: adult when the 0-based cohort >= age_mat, so the first adult

  ## cohort (1-based) is age_mat + 1 and the last juvenile is age_mat (D08).

  Gape$Age.Young <- pmax(1, Gape$Age.Adult)
  Gape$Age.Adult <- Gape$Age.Adult + 1
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
    Gape$juv.Max[i] <- Gape[i, ages.pos[Gape$Age.Young[i]]] * Gape$KUP[i]
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

pprey_is_none <- function(x) {
  is.null(x) || !nzchar(as.character(x)[1]) ||
    as.character(x)[1] %in% c("(all)", "(none)")
}

pprey_group_choices <- function(codes, grp = NULL) {
  codes <- unique(as.character(codes))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  if (length(codes) == 0) return(c("(none)" = "(none)"))
  labs <- pprey_group_label(codes, grp)
  c("(none)" = "(none)", stats::setNames(codes, labs))
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
  if (!pprey_is_none(pred_group)) {
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
  pred_sel <- !pprey_is_none(pred_group)
  pred_st <- !identical(pprey_stage_wanted(pred_stage), "all")
  prey_sel <- !pprey_is_none(prey_group)
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

pprey_axis_labels <- function(meta, grp = NULL) {
  if (is.null(meta) || nrow(meta) == 0) return(character())
  paste0(
    pprey_group_label(meta$pred_code, grp),
    " · ",
    tolower(pprey_stage_label(meta$pred_stage)),
    " / ",
    tolower(pprey_stage_label(meta$prey_stage)), " prey"
  )
}

pprey_hover_text <- function(mat, grp = NULL, value_label = "pPREY") {
  if (is.null(mat) || !is.matrix(mat)) {
    return(matrix(character(), 0, 0))
  }
  meta <- parse_pprey_rownames(rownames(mat))
  pred_lab <- pprey_group_label(meta$pred_code, grp)
  pred_st <- pprey_stage_label(meta$pred_stage)
  prey_st <- pprey_stage_label(meta$prey_stage)
  prey_lab <- pprey_group_label(colnames(mat), grp)
  out <- matrix("", nrow(mat), ncol(mat), dimnames = dimnames(mat))
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      val <- mat[i, j]
      lab <- if (is.finite(val)) as.character(signif(val, 4)) else "NA"
      out[i, j] <- paste0(
        value_label, ": ", lab,
        "<br>Predator: ", pred_lab[i], " - ", tolower(pred_st[i]),
        "<br>Prey: ", prey_lab[j], " - ", tolower(prey_st[i])
      )
    }
  }
  out
}

pprey_resolve_selection <- function(mat, pred_group, pred_stage, prey_group, prey_stage) {
  empty <- list(rows = character(), prey = character(), row_i = integer(0),
                col_i = integer(0))
  if (is.null(mat) || !is.matrix(mat)) return(empty)
  meta <- parse_pprey_rownames(rownames(mat))
  rows <- integer(0)
  if (!pprey_is_none(pred_group)) {
    rows <- pprey_match_rows(meta, pred_group, pred_stage)
    if (!identical(pprey_stage_wanted(prey_stage), "all") &&
        !pprey_is_none(prey_group)) {
      rows <- intersect(rows, pprey_match_prey_stage(meta, prey_stage))
    }
  }
  col_i <- integer(0)
  if (!pprey_is_none(prey_group)) {
    col_i <- which(colnames(mat) == as.character(prey_group))
  }
  list(
    rows = if (length(rows)) rownames(mat)[rows] else character(),
    prey = if (length(col_i)) colnames(mat)[col_i[1]] else character(),
    row_i = rows,
    col_i = col_i
  )
}

pprey_selection_from_click <- function(mat, y_label, x_label, grp = NULL) {
  if (is.null(mat) || !is.matrix(mat)) return(NULL)
  meta <- parse_pprey_rownames(rownames(mat))
  y_labs <- pprey_axis_labels(meta, grp)
  x_labs <- pprey_group_label(colnames(mat), grp)
  y_label <- as.character(y_label)[1]
  x_label <- as.character(x_label)[1]
  ri <- which(y_labs == y_label | rownames(mat) == y_label)[1]
  ci <- which(x_labs == x_label | colnames(mat) == x_label)[1]
  if (!is.finite(ri) || !is.finite(ci)) return(NULL)
  list(
    pred_group = meta$pred_code[ri],
    pred_stage = if (is.na(meta$pred_stage[ri])) "pool" else as.character(meta$pred_stage[ri]),
    prey_group = colnames(mat)[ci],
    prey_stage = if (is.na(meta$prey_stage[ri])) "1" else as.character(meta$prey_stage[ri])
  )
}

pprey_cell_info_text <- function(mat, sel, grp = NULL, value_label = "pPREY") {
  if (is.null(mat) || !is.matrix(mat)) return("Load biology.prm to show the pPREY matrix.")
  if (is.null(sel) || (length(sel$row_i) == 0 && length(sel$col_i) == 0)) {
    return("Hover a cell for its value. Click to select predator and prey.")
  }
  if (length(sel$row_i) == 1 && length(sel$col_i) >= 1) {
    txt <- pprey_hover_text(mat, grp, value_label = value_label)[sel$row_i[1], sel$col_i[1]]
    return(gsub("<br>", " | ", txt, fixed = TRUE))
  }
  bits <- character()
  if (length(sel$rows)) bits <- c(bits, paste("Predator row:", paste(sel$rows, collapse = ", ")))
  if (length(sel$prey)) bits <- c(bits, paste("Prey column:", sel$prey))
  paste(bits, collapse = " | ")
}

feed_matrix_plot_size <- function(mat) {
  nr <- if (is.null(mat) || !is.matrix(mat)) 1L else nrow(mat)
  nc <- if (is.null(mat) || !is.matrix(mat)) 1L else ncol(mat)
  list(
    height = max(520L, min(4200L, as.integer(nr * 18 + 160))),
    width = max(720L, min(4200L, as.integer(nc * 22 + 200)))
  )
}

##' Read every pPREY row; pad short rows with 0 instead of stopping.
read_pprey_matrix <- function(text) {
  if (is.null(text) || !length(text)) return(NULL)
  lines <- as.character(text)
  hits <- grep("^[[:space:]]*pPREY[A-Za-z0-9_]+", lines)
  if (!length(hits)) return(NULL)
  nms <- character()
  rows <- list()
  for (i in hits) {
    raw <- lines[i]
    if (grepl("^[[:space:]]*#", raw)) next
    ln <- trimws(sub("#.*$", "", raw))
    if (!nzchar(ln)) next
    toks <- unlist(strsplit(ln, "[[:space:]]+"), use.names = FALSE)
    nm <- toks[1]
    if (!grepl("^pPREY", nm)) next
    nums <- suppressWarnings(as.numeric(toks[-1]))
    nums <- nums[is.finite(nums)]
    if (length(nums) <= 1 && i < length(lines)) {
      nxt <- trimws(sub("#.*$", "", lines[i + 1]))
      more <- suppressWarnings(as.numeric(unlist(strsplit(nxt, "[[:space:]]+"))))
      more <- more[is.finite(more)]
      if (length(more) > length(nums)) nums <- more
    }
    nms <- c(nms, nm)
    rows[[length(rows) + 1]] <- nums
  }
  if (!length(rows)) return(NULL)
  nc <- max(vapply(rows, length, integer(1)))
  if (!is.finite(nc) || nc < 1) return(NULL)
  mat <- matrix(0, nrow = length(rows), ncol = nc)
  for (i in seq_along(rows)) {
    v <- rows[[i]]
    if (length(v)) mat[i, seq_len(min(length(v), nc))] <- v[seq_len(min(length(v), nc))]
  }
  rownames(mat) <- nms
  mat
}

plot_feed_matrix_plotly <- function(mat, sel = NULL, grp = NULL,
                                    hover = NULL, title = NULL,
                                    colorbar = "pPREY") {
  if (is.null(mat) || !is.matrix(mat) || !requireNamespace("plotly", quietly = TRUE)) {
    p <- ggplot2::ggplot() +
      ggplot2::labs(title = "pPREY matrix") +
      theme_reactiveatlantis()
    return(ra_ggplotly(p, tooltip = "text"))
  }
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste0("Prey", seq_len(ncol(mat)))
  }
  meta <- parse_pprey_rownames(rownames(mat))
  y_lab <- pprey_axis_labels(meta, grp)
  x_lab <- pprey_group_label(colnames(mat), grp)
  if (is.null(hover)) hover <- pprey_hover_text(mat, grp)
  z <- mat
  z[!is.finite(z)] <- NA
  ## Keep zeros. First predator row at the top (heatmap otherwise starts at bottom).
  y_ord <- rev(y_lab)
  z_ord <- z[rev(seq_len(nrow(z))), , drop = FALSE]
  h_ord <- hover[rev(seq_len(nrow(hover))), , drop = FALSE]
  sz <- feed_matrix_plot_size(mat)
  plt <- plotly::plot_ly(
    x = x_lab,
    y = y_ord,
    z = z_ord,
    type = "heatmap",
    colorscale = "YlOrRd",
    reversescale = TRUE,
    zmin = 0,
    hoverinfo = "text",
    text = h_ord,
    colorbar = list(title = colorbar),
    xgap = 1, ygap = 1,
    width = sz$width,
    height = sz$height
  )
  shapes <- list()
  if (!is.null(sel) && length(sel$row_i)) {
    for (ri in sel$row_i) {
      yl <- y_lab[ri]
      shapes[[length(shapes) + 1]] <- list(
        type = "rect",
        xref = "x", yref = "y",
        x0 = -0.5, x1 = ncol(mat) - 0.5,
        y0 = yl, y1 = yl,
        line = list(color = "#C4A35A", width = 3),
        fillcolor = "rgba(0,0,0,0)"
      )
    }
  }
  if (!is.null(sel) && length(sel$col_i)) {
    for (ci in sel$col_i) {
      xl <- x_lab[ci]
      shapes[[length(shapes) + 1]] <- list(
        type = "rect",
        xref = "x", yref = "y",
        x0 = xl, x1 = xl,
        y0 = -0.5, y1 = nrow(mat) - 0.5,
        line = list(color = "#328CC1", width = 3),
        fillcolor = "rgba(0,0,0,0)"
      )
    }
  }
  ann <- list()
  if (!is.null(sel) && length(sel$row_i) == 1 && length(sel$col_i) >= 1) {
    ri <- sel$row_i[1]
    ci <- sel$col_i[1]
    ann[[1]] <- list(
      x = x_lab[ci], y = y_lab[ri],
      text = gsub("<br>", "<br>", hover[ri, ci], fixed = TRUE),
      showarrow = TRUE, arrowhead = 2, arrowcolor = "#0B3C5D",
      bgcolor = "#F7F4EC", bordercolor = "#0B3C5D",
      font = list(size = 11, color = "#1B2833")
    )
  }
  plt <- plotly::layout(
    plt,
    title = list(
      text = if (!is.null(title) && nzchar(as.character(title)[1])) {
        as.character(title)[1]
      } else {
        paste0("Availability matrix (pPREY) — ",
               nrow(mat), " predators x ", ncol(mat), " prey")
      },
      font = list(size = 14)
    ),
    xaxis = list(
      title = "Prey", side = "top", tickangle = -60,
      type = "category", categoryarray = x_lab, categoryorder = "array",
      automargin = TRUE
    ),
    yaxis = list(
      title = "Predator", automargin = TRUE,
      type = "category", categoryarray = y_ord, categoryorder = "array"
    ),
    shapes = shapes,
    annotations = ann,
    margin = list(l = 160, t = 90, b = 40, r = 20)
  )
  plotly::config(plt, displaylogo = FALSE, scrollZoom = TRUE,
                 modeBarButtonsToRemove = c("lasso2d", "select2d"))
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
  if (is.null(mat) || !is.matrix(mat) || pprey_is_none(group)) {
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

plot_pprey_one_group <- function(df, ylab = "pPREY") {
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
  if (is.null(ylab) || !nzchar(as.character(ylab)[1])) ylab <- "pPREY"
  p <- p + ggplot2::labs(title = ttl, x = xl, y = ylab)
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
  if (pprey_is_none(pred_group)) {
    return(list(mat = mat, message = "Pick a predator group name to edit."))
  }
  if (pprey_is_none(prey_group)) {
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
