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
            " Catch analysis needs a groups CSV, a fisheries CSV, and a catch NetCDF (often a dedicated CATCH output, not the main biology output.nc). Load those files to plot harvest and skill metrics."
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
      nc_vars = NULL,
      grp = NULL,
      fish = NULL,
      ext_catch = NULL,
      time = NULL,
      fg_palette = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      if(is.null(input$grp_csv) || is.null(input$fish_csv) || is.null(input$catch_nc)){
        showNotification(
          "Catch analysis needs a groups CSV, a fisheries CSV, and a catch NetCDF. Those files were not provided.",
          type = "error", duration = 10
        )
        return()
      }
      
      tryCatch({
        showNotification("Loading catch data...", type = "message", id = "load_catch", duration = NULL)
        
        nc.data <- ncdf4::nc_open(input$catch_nc$datapath)
        grp <- normalise_groups_catch(utils::read.csv(input$grp_csv$datapath, stringsAsFactors = FALSE))
        fish <- normalise_fisheries(utils::read.csv(input$fish_csv$datapath, stringsAsFactors = FALSE))
        
        time <- ncdf4::ncvar_get(nc.data, 't')
        t_att <- ncdf4::ncatt_get(nc.data, 't')
        if (!is.null(t_att$units) && grepl('seconds', t_att$units, ignore.case = TRUE)) {
          time <- as.Date(time / 86400, origin = '1970-01-01')
        } else {
          time <- as.Date(time, origin = '1970-01-01')
        }
        
        ext_catch <- NULL
        if(!is.null(input$ext_catch)){
          ext_catch <- utils::read.csv(input$ext_catch$datapath, stringsAsFactors = FALSE)
        }
        
        impacted_grp <- grp
        if ("isimpacted" %in% names(grp)) {
          impacted_grp <- grp[grp$isimpacted == 1, ]
        }
        if (nrow(impacted_grp) == 0) impacted_grp <- grp
        
        rv$nc_data <- nc.data
        rv$nc_vars <- names(nc.data$var)
        rv$grp <- grp
        rv$fish <- fish
        rv$ext_catch <- ext_catch
        rv$time <- time
        rv$fg_palette <- build_fg_palette(grp$code)
        rv$data_loaded <- TRUE
        
        fish_labels <- if ("name" %in% names(fish)) {
          setNames(as.character(fish$code), as.character(fish$name))
        } else {
          as.character(fish$code)
        }
        updateSelectInput(session, "fishery", choices = fish_labels)
        updateSelectInput(session, "fg", choices = as.character(impacted_grp$code))
        
        removeNotification("load_catch")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("load_catch")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    catch_result <- reactive({
      req(rv$nc_data, rv$grp, rv$fish, rv$nc_vars, input$analysis_type)
      quantity <- if (identical(input$analysis_type, "numbers")) "numbers" else "biomass"
      fishery_code <- input$fishery
      fg_code <- input$fg
      if (identical(quantity, "biomass")) {
        req(fishery_code)
      } else {
        req(fg_code)
      }
      extract_catch_series(
        nc.data = rv$nc_data,
        nc_vars = rv$nc_vars,
        grp = rv$grp,
        fish = rv$fish,
        time = rv$time,
        quantity = quantity,
        fishery_code = fishery_code,
        fg_code = fg_code,
        by_year = isTRUE(input$by_year)
      )
    })
    
    output$analysis_output <- renderUI({
      req(rv$data_loaded)
      ns <- session$ns
      res <- catch_result()
      
      if (input$analysis_type == "skill") {
        if (!isTRUE(res$ok)) {
          return(catch_empty_message(res))
        }
        tagList(
          h4("Model Skill Assessment"),
          DT::dataTableOutput(ns("skill_table"))
        )
      } else if (!isTRUE(res$ok)) {
        catch_empty_message(res)
      } else {
        plotOutput(ns("catch_plot"), height = "700px")
      }
    })
    
    output$catch_plot <- renderPlot({
      res <- catch_result()
      req(isTRUE(res$ok), !is.null(res$data), nrow(res$data) > 0)
      
      catch_df <- res$data
      quantity <- res$quantity
      ylab <- if (identical(quantity, "numbers")) "Catch (numbers)" else "Catch (tons)"
      title_qty <- if (identical(quantity, "numbers")) "Catch numbers" else "Catch biomass"
      
      colour_by_group <- "Functional_group" %in% names(catch_df) &&
        length(unique(catch_df$Functional_group)) > 1 &&
        length(unique(catch_df$series)) == length(unique(catch_df$Functional_group))
      
      if (colour_by_group) {
        pal <- get_fg_colors(unique(as.character(catch_df$Functional_group)), rv$fg_palette)
        p <- ggplot2::ggplot(catch_df, ggplot2::aes(x = years, y = catch, color = Functional_group))
        p <- p + ggplot2::geom_line(linewidth = 1.1, na.rm = TRUE)
        p <- p + ggplot2::scale_color_manual(values = pal, name = "Functional group")
        p <- p + ggplot2::facet_wrap(~ Functional_group, scales = "free_y")
      } else {
        n_series <- length(unique(catch_df$series))
        colors <- get_catch_colors(n_series)
        names(colors) <- unique(as.character(catch_df$series))
        p <- ggplot2::ggplot(catch_df, ggplot2::aes(x = years, y = catch, color = series))
        p <- p + ggplot2::geom_line(linewidth = 1.1, na.rm = TRUE)
        p <- p + ggplot2::scale_color_manual(values = colors, name = "Cohort")
        if (n_series > 1) {
          p <- p + ggplot2::facet_wrap(~ series, scales = "free_y")
        }
      }
      
      p <- p + ggplot2::theme_minimal()
      p <- p + ggplot2::labs(
        title = paste(title_qty, "-", input$fishery, "-", input$fg),
        x = if (isTRUE(input$by_year)) "Year" else "Date",
        y = ylab
      )
      p <- p + ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        axis.title = ggplot2::element_text(size = 12),
        legend.position = "right"
      )
      
      if (!is.null(rv$ext_catch) && !is.null(input$fg) && input$fg %in% names(rv$ext_catch)) {
        time_col <- names(rv$ext_catch)[tolower(names(rv$ext_catch)) %in% c("time", "year", "years")][1]
        if (!is.na(time_col)) {
          ext_df <- data.frame(
            years = as.Date(paste0(rv$ext_catch[[time_col]], "-01-01")),
            catch = rv$ext_catch[[input$fg]]
          )
          p <- p + ggplot2::geom_point(
            data = ext_df, ggplot2::aes(x = years, y = catch),
            color = "red", size = 3, inherit.aes = FALSE
          )
        }
      }
      
      p
    })
    
    output$skill_table <- DT::renderDataTable({
      res <- catch_result()
      if (!isTRUE(res$ok) || is.null(rv$ext_catch)) {
        return(data.frame(Note = "External catch data required for skill assessment"))
      }
      if (is.null(input$fg) || !(input$fg %in% names(rv$ext_catch))) {
        return(data.frame(Note = paste("No external data for", input$fg)))
      }
      
      catch_df <- res$data
      catch_agg <- stats::aggregate(catch ~ format(years, "%Y"), data = catch_df, sum)
      names(catch_agg) <- c("year", "modeled")
      
      time_col <- names(rv$ext_catch)[tolower(names(rv$ext_catch)) %in% c("time", "year", "years")][1]
      ext_df <- data.frame(
        year = as.character(rv$ext_catch[[time_col]]),
        observed = rv$ext_catch[[input$fg]]
      )
      
      merged <- merge(catch_agg, ext_df, by = "year")
      merged <- merged[stats::complete.cases(merged), ]
      
      if (nrow(merged) < 3) {
        return(data.frame(Note = "Insufficient overlapping data for skill assessment"))
      }
      
      skill <- skill_assessment(merged$observed, merged$modeled, input$fg)
      DT::datatable(skill, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
    })
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Helper Functions                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

normalise_groups_catch <- function(grp) {
  names(grp) <- tolower(names(grp))
  if ("code" %in% names(grp)) grp$code <- trimws(as.character(grp$code))
  if ("name" %in% names(grp)) grp$name <- trimws(as.character(grp$name))
  grp
}

normalise_fisheries <- function(fish) {
  names(fish) <- tolower(names(fish))
  if ("code" %in% names(fish)) fish$code <- trimws(as.character(fish$code))
  if ("name" %in% names(fish)) fish$name <- trimws(as.character(fish$name))
  if (!"index" %in% names(fish)) {
    fish$index <- seq_len(nrow(fish))
  } else {
    fish$index <- as.integer(trimws(as.character(fish$index)))
  }
  fish
}

fishery_row <- function(fish, fishery_code) {
  fishery_code <- trimws(as.character(fishery_code))
  row <- fish[fish$code == fishery_code, , drop = FALSE]
  if (nrow(row) == 0 && "name" %in% names(fish)) {
    row <- fish[fish$name == fishery_code, , drop = FALSE]
  }
  if (nrow(row) == 0) {
    return(NULL)
  }
  row[1, , drop = FALSE]
}

catch_var_patterns <- function(grp_row, fish_row, quantity = "biomass") {
  code <- trimws(as.character(grp_row$code[1]))
  name <- trimws(as.character(grp_row$name[1]))
  n.coh <- as.integer(grp_row$numcohorts[1])
  if (!is.finite(n.coh) || n.coh < 1) n.coh <- 1
  fc <- if (is.null(fish_row)) NA_integer_ else as.integer(fish_row$index[1])
  
  patterns <- character()
  if (identical(quantity, "numbers")) {
    if (n.coh > 1) {
      coh <- seq_len(n.coh)
      patterns <- c(
        paste0(name, coh, "_Num"),
        paste0(name, coh, "_Catch_Num"),
        paste0(name, coh, "_Catch"),
        paste0(name, coh, "_Discard"),
        paste0(name, coh, "_Discards")
      )
    } else {
      patterns <- c(
        paste0(name, "_Num"),
        paste0(name, "_Catch_Num"),
        paste0(name, "_Catch"),
        paste0(name, "_Discard"),
        paste0(name, "_Discards")
      )
    }
    if (is.finite(fc)) {
      patterns <- c(patterns, paste0(code, "_Num_FC", fc), paste0(code, "_Catch_FC", fc))
    }
  } else {
    if (is.finite(fc)) {
      patterns <- c(
        paste0(code, "_Catch_FC", fc),
        paste0(code, "_Discard_FC", fc),
        paste0(code, "_Discards_FC", fc)
      )
    }
    if (n.coh > 1) {
      coh <- seq_len(n.coh)
      patterns <- c(patterns, paste0(name, coh, "_Catch"), paste0(name, coh, "_Discard"),
                    paste0(name, coh, "_Discards"))
    } else {
      patterns <- c(patterns, paste0(name, "_Catch"), paste0(name, "_Discard"),
                    paste0(name, "_Discards"))
    }
  }
  unique(patterns)
}

catch_var_regex <- function(grp_row, fish_row, quantity = "biomass") {
  code <- trimws(as.character(grp_row$code[1]))
  name <- trimws(as.character(grp_row$name[1]))
  n.coh <- as.integer(grp_row$numcohorts[1])
  if (!is.finite(n.coh) || n.coh < 1) n.coh <- 1
  fc <- if (is.null(fish_row)) NA_integer_ else as.integer(fish_row$index[1])
  code_esc <- gsub("([.|()\\^$*+?{}])", "\\\\\\1", code)
  name_esc <- gsub("([.|()\\^$*+?{}])", "\\\\\\1", name)
  
  rx <- character()
  if (identical(quantity, "numbers")) {
    if (n.coh > 1) {
      rx <- c(rx, paste0("^", name_esc, "[0-9]+_(Catch_Num|Num|Catch|Discard|Discards)$"))
    } else {
      rx <- c(rx, paste0("^", name_esc, "_(Catch_Num|Num|Catch|Discard|Discards)$"))
    }
    if (is.finite(fc)) {
      rx <- c(rx, paste0("^", code_esc, "_(Num|Catch|Discard|Discards)_FC", fc, "$"))
    }
  } else {
    if (is.finite(fc)) {
      rx <- c(rx, paste0("^", code_esc, "_(Catch|Discard|Discards)_FC", fc, "$"))
    }
    rx <- c(rx, paste0("^", code_esc, "_(Catch|Discard|Discards)_FC[0-9]+$"))
    if (n.coh > 1) {
      rx <- c(rx, paste0("^", name_esc, "[0-9]+_(Catch|Discard|Discards)$"))
    } else {
      rx <- c(rx, paste0("^", name_esc, "_(Catch|Discard|Discards)$"))
    }
  }
  unique(rx)
}

match_catch_vars <- function(nc_vars, grp_row, fish_row, quantity = "biomass") {
  patterns <- catch_var_patterns(grp_row, fish_row, quantity)
  regexes <- catch_var_regex(grp_row, fish_row, quantity)
  exact <- intersect(patterns, nc_vars)
  regex_hits <- unique(unlist(lapply(regexes, function(rx) {
    grep(rx, nc_vars, value = TRUE, ignore.case = TRUE)
  }), use.names = FALSE))
  matched <- unique(c(exact, regex_hits))
  matched <- prefer_catch_vars(matched, fish_row, quantity)
  list(
    matched = matched,
    patterns = patterns,
    regexes = regexes
  )
}

prefer_catch_vars <- function(matched, fish_row, quantity) {
  if (length(matched) == 0) return(matched)
  fc <- if (is.null(fish_row)) NA_integer_ else as.integer(fish_row$index[1])
  
  if (identical(quantity, "numbers")) {
    num_vars <- grep("(_Num|_Catch_Num)", matched, value = TRUE, ignore.case = TRUE)
    if (length(num_vars) > 0) return(num_vars)
    coh <- grep("_FC[0-9]+$", matched, value = TRUE, invert = TRUE)
    if (length(coh) > 0) return(coh)
    return(matched)
  }
  
  if (is.finite(fc)) {
    fc_match <- grep(paste0("_FC", fc, "$"), matched, value = TRUE, ignore.case = TRUE)
    catch_fc <- grep("_Catch_FC", fc_match, value = TRUE, ignore.case = TRUE)
    if (length(catch_fc) > 0) return(catch_fc)
    if (length(fc_match) > 0) return(fc_match)
  }
  fc_any <- grep("_Catch_FC[0-9]+$", matched, value = TRUE, ignore.case = TRUE)
  if (length(fc_any) > 0) return(fc_any)
  coh <- grep("_FC[0-9]+$", matched, value = TRUE, invert = TRUE)
  if (length(coh) > 0) return(coh)
  matched
}

sum_nc_var <- function(nc.data, varname) {
  data <- ncdf4::ncvar_get(nc.data, varname)
  d <- dim(data)
  if (is.null(d) || length(d) == 1) {
    as.numeric(data)
  } else {
    apply(data, length(d), sum, na.rm = TRUE)
  }
}

series_label_from_var <- function(varname, grp_row) {
  name <- trimws(as.character(grp_row$name[1]))
  coh <- sub(paste0("^", gsub("([.|()\\^$*+?{}])", "\\\\\\1", name), "([0-9]+)_.*$"), "\\1", varname)
  if (identical(coh, varname)) {
    return(varname)
  }
  n <- suppressWarnings(as.integer(coh))
  if (is.finite(n)) {
    return(ifelse(n < 10, paste0("Cohort 0", n), paste0("Cohort ", n)))
  }
  varname
}

catch_empty_result <- function(patterns, regexes, nc_vars, quantity, fishery_code, fg_code) {
  sample_vars <- nc_vars[!nc_vars %in% c("t", "b", "z", "time")]
  sample_vars <- utils::head(sample_vars, 25)
  list(
    ok = FALSE,
    data = NULL,
    quantity = quantity,
    patterns = patterns,
    regexes = regexes,
    sample_vars = sample_vars,
    fishery = fishery_code,
    fg = fg_code
  )
}

catch_empty_message <- function(res) {
  patterns <- unique(c(res$patterns, res$regexes))
  sample_vars <- res$sample_vars
  shiny::div(
    class = "alert alert-warning",
    style = "margin-top: 20px;",
    shiny::h4("No matching catch variables"),
    shiny::p(paste0(
      "Nothing was plotted for fishery '", res$fishery, "' and group '", res$fg,
      "' (", res$quantity, "). The NetCDF was searched with these names and patterns:"
    )),
    shiny::tags$ul(lapply(patterns, function(p) shiny::tags$li(shiny::code(p)))),
    shiny::p("Variables present in the file include:"),
    shiny::tags$p(if (length(sample_vars) == 0) "(none listed)" else paste(sample_vars, collapse = ", "))
  )
}

read_group_catch <- function(nc.data, nc_vars, grp_row, fish_row, quantity, time) {
  found <- match_catch_vars(nc_vars, grp_row, fish_row, quantity)
  rows <- list()
  for (vn in found$matched) {
    vals <- tryCatch(sum_nc_var(nc.data, vn), error = function(e) NULL)
    if (is.null(vals)) next
    n <- min(length(vals), length(time))
    rows[[length(rows) + 1]] <- data.frame(
      years = time[seq_len(n)],
      catch = as.numeric(vals[seq_len(n)]),
      series = series_label_from_var(vn, grp_row),
      Functional_group = grp_row$code[1],
      variable = vn,
      stringsAsFactors = FALSE
    )
  }
  list(rows = rows, patterns = found$patterns, regexes = found$regexes)
}

extract_catch_series <- function(nc.data, nc_vars, grp, fish, time, quantity,
                                 fishery_code, fg_code, by_year = TRUE) {
  fish_row <- fishery_row(fish, fishery_code)
  all_patterns <- character()
  all_regexes <- character()

  if (identical(quantity, "biomass") && is.null(fish_row)) {
    return(catch_empty_result(
      patterns = c(
        paste0("Fishery '", fishery_code, "' (trimmed Code/Name) in the fisheries CSV"),
        "{Code}_Catch_FC{Index} from the fisheries Index column",
        "{name}{cohort}_Catch totals"
      ),
      regexes = character(),
      nc_vars = nc_vars,
      quantity = quantity,
      fishery_code = fishery_code,
      fg_code = fg_code
    ))
  }
  
  target_groups <- grp
  if (identical(quantity, "numbers")) {
    target_groups <- grp[grp$code == fg_code, , drop = FALSE]
  } else if (!is.null(fg_code) && nzchar(as.character(fg_code)) && fg_code %in% grp$code) {
    target_groups <- grp[grp$code == fg_code, , drop = FALSE]
  }
  if ("isimpacted" %in% names(grp) && identical(quantity, "biomass") &&
      (nrow(target_groups) == 0 || is.null(fg_code) || !nzchar(as.character(fg_code)))) {
    impacted <- grp[grp$isimpacted == 1, , drop = FALSE]
    if (nrow(impacted) > 0) target_groups <- impacted
  }
  if (nrow(target_groups) == 0) target_groups <- grp
  
  rows <- list()
  used_groups <- character()
  for (i in seq_len(nrow(target_groups))) {
    grp_row <- target_groups[i, , drop = FALSE]
    found <- match_catch_vars(nc_vars, grp_row, fish_row, quantity)
    all_patterns <- c(all_patterns, found$patterns)
    all_regexes <- c(all_regexes, found$regexes)
    if (length(found$matched) == 0) next
    used_groups <- c(used_groups, grp_row$code[1])
    for (vn in found$matched) {
      vals <- tryCatch(sum_nc_var(nc.data, vn), error = function(e) NULL)
      if (is.null(vals)) next
      n <- min(length(vals), length(time))
      rows[[length(rows) + 1]] <- data.frame(
        years = time[seq_len(n)],
        catch = as.numeric(vals[seq_len(n)]),
        series = series_label_from_var(vn, grp_row),
        Functional_group = grp_row$code[1],
        variable = vn,
        stringsAsFactors = FALSE
      )
    }
  }
  
  ## Biomass fallback: if the selected group had no FC variable, try all groups
  if (length(rows) == 0 && identical(quantity, "biomass") &&
      !is.null(fg_code) && nrow(grp) > 1) {
    extra <- grp
    if ("isimpacted" %in% names(grp)) {
      extra <- grp[grp$isimpacted == 1, , drop = FALSE]
      if (nrow(extra) == 0) extra <- grp
    }
    extra <- extra[!extra$code %in% used_groups, , drop = FALSE]
    for (i in seq_len(nrow(extra))) {
      grp_row <- extra[i, , drop = FALSE]
      found <- match_catch_vars(nc_vars, grp_row, fish_row, quantity)
      all_patterns <- c(all_patterns, found$patterns)
      all_regexes <- c(all_regexes, found$regexes)
      if (length(found$matched) == 0) next
      for (vn in found$matched) {
        vals <- tryCatch(sum_nc_var(nc.data, vn), error = function(e) NULL)
        if (is.null(vals)) next
        n <- min(length(vals), length(time))
        rows[[length(rows) + 1]] <- data.frame(
          years = time[seq_len(n)],
          catch = as.numeric(vals[seq_len(n)]),
          series = series_label_from_var(vn, grp_row),
          Functional_group = grp_row$code[1],
          variable = vn,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(rows) == 0) {
    return(catch_empty_result(unique(all_patterns), unique(all_regexes),
                              nc_vars, quantity, fishery_code, fg_code))
  }
  
  catch_df <- do.call(rbind, rows)
  if (isTRUE(by_year)) {
    catch_df$year <- format(catch_df$years, "%Y")
    catch_df <- stats::aggregate(catch ~ year + series + Functional_group + variable,
                                 data = catch_df, sum)
    catch_df$years <- as.Date(paste0(catch_df$year, "-01-01"))
  }
  
  list(
    ok = TRUE,
    data = catch_df,
    quantity = quantity,
    patterns = unique(all_patterns),
    regexes = unique(all_regexes),
    fishery = fishery_code,
    fg = fg_code
  )
}

skill_assessment <- function(obs, mod, FG){
  COR <- stats::cor.test(obs, mod, method = 'spearman', use = "pairwise.complete.obs", exact = FALSE)
  AE <- mean(obs, na.rm = TRUE) - mean(mod, na.rm = TRUE)
  diff <- mod - obs
  AAE <- mean(abs(diff), na.rm = TRUE)
  RMSE <- sqrt(mean((diff) ^ 2, na.rm = TRUE))
  
  tmp <- log(obs / mod) ^ 2
  tmp[is.infinite(tmp)] <- NA
  RI <- exp(sqrt(mean(tmp, na.rm = TRUE)))
  
  ME <- 1 - (RMSE ^ 2) / stats::var(obs, na.rm = TRUE)
  
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
