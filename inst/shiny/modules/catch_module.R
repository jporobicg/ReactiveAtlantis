## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Catch Analysis Module             ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

catch_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("view"), "View",
                    choices = c("Stacked by group" = "by_group",
                                "Stacked by fleet" = "by_fleet",
                                "Numbers by age" = "numbers",
                                "Skill vs observed" = "skill")),
        selectInput(ns("fishery"), "Fishery", choices = character()),
        selectInput(ns("fg"), "Functional group", choices = "All"),
        checkboxInput(ns("by_year"), "Aggregate by year", TRUE)
      ),
      conditionalPanel("input.view != 'skill'", ns = ns,
                       plotOutput(ns("catch_plot_gg"), height = "640px")),
      conditionalPanel("input.view == 'skill'", ns = ns,
                       h4("Model skill assessment"),
                       DT::dataTableOutput(ns("skill_table")))
    )
  )
}

catch_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))

    output$banner <- renderUI(tool_banner(ps(), "catch"))

    fg_d <- ra_debounce(shiny::reactive(input$fg), 250)
    view_d <- ra_debounce(shiny::reactive(input$view), 250)

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      dat <- ps()$data
      grp <- dat$grp
      fish <- dat$fish
      if (is.null(fish) || NROW(fish) == 0) {
        fish <- tryCatch(fisheries_from_catch_nc(dat$nc_catch), error = function(e) NULL)
      }
      if (!is.null(fish) && NROW(fish) > 0) {
        fish_labels <- if ("name" %in% names(fish)) {
          stats::setNames(as.character(fish$code), paste(fish$code, fish$name, sep = " — "))
        } else {
          as.character(fish$code)
        }
        update_select_keep(session, "fishery", fish_labels, input$fishery)
      }
      impacted <- grp
      if ("isimpacted" %in% names(grp)) {
        on <- grp[grp$isimpacted == 1, , drop = FALSE]
        if (nrow(on) > 0) impacted <- on
      }
      update_select_keep(session, "fg", c("All", as.character(impacted$code)), input$fg)
    })

    catch_time <- reactive({
      dat <- ensure_project_data(ps(), "nc_catch")
      req(dat$nc_catch)
      t <- tryCatch(time_calc(dat$nc_catch), error = function(e) NULL)
      if (!is.null(t)) return(t)
      raw <- nc_get(dat$nc_catch, "t")
      as.Date(as.numeric(raw) / 86400, origin = "1970-01-01")
    })

    nc_vars <- reactive({
      dat <- ensure_project_data(ps(), "nc_catch")
      req(dat$nc_catch)
      names(dat$nc_catch$var)
    })

    catch_result <- reactive({
      dat <- ensure_project_data(ps(), "nc_catch")
      req(dat$nc_catch, dat$grp, view_d())
      if (is.null(dat$fish) || NROW(dat$fish) == 0) {
        dat$fish <- fisheries_from_catch_nc(dat$nc_catch)
      }
      req(dat$fish)
      quantity <- if (identical(view_d(), "numbers")) "numbers" else "biomass"
      fishery_code <- input$fishery
      fg_code <- fg_d()
      if (identical(fg_code, "All")) fg_code <- ""
      if (identical(quantity, "biomass")) {
        req(fishery_code)
      } else {
        req(fg_code)
      }
      extract_catch_series(
        nc.data = dat$nc_catch,
        nc_vars = nc_vars(),
        grp = dat$grp,
        fish = dat$fish,
        time = catch_time(),
        quantity = quantity,
        fishery_code = fishery_code,
        fg_code = fg_code,
        by_year = isTRUE(input$by_year)
      )
    })

    fleet_stack <- reactive({
      dat <- ensure_project_data(ps(), "nc_catch")
      req(dat$nc_catch, dat$grp, input$fg)
      if (is.null(dat$fish) || NROW(dat$fish) == 0) {
        dat$fish <- fisheries_from_catch_nc(dat$nc_catch)
      }
      req(dat$fish)
      fg_code <- fg_d()
      if (identical(fg_code, "All")) fg_code <- ""
      rows <- list()
      for (i in seq_len(nrow(dat$fish))) {
        res <- extract_catch_series(
          nc.data = dat$nc_catch,
          nc_vars = nc_vars(),
          grp = dat$grp,
          fish = dat$fish,
          time = catch_time(),
          quantity = "biomass",
          fishery_code = dat$fish$code[i],
          fg_code = fg_code,
          by_year = isTRUE(input$by_year)
        )
        if (isTRUE(res$ok) && !is.null(res$data)) {
          df <- res$data
          df$Fleet <- dat$fish$code[i]
          rows[[length(rows) + 1]] <- df
        }
      }
      if (length(rows) == 0) return(NULL)
      do.call(rbind, rows)
    })

    observed_overlay <- function(p, fg_code, time_col_df) {
      dat <- ps()$data
      ext <- dat$ext_catch
      if (is.null(ext) || is.null(fg_code) || !nzchar(fg_code) || !(fg_code %in% names(ext))) {
        return(p)
      }
      time_col <- names(ext)[tolower(names(ext)) %in% c("time", "year", "years")][1]
      if (is.na(time_col)) return(p)
      ext_df <- data.frame(
        years = as.Date(paste0(ext[[time_col]], "-01-01")),
        catch = ext[[fg_code]],
        stringsAsFactors = FALSE
      )
      p + ggplot2::geom_point(
        data = ext_df, ggplot2::aes(x = years, y = catch),
        color = "#A33B20", size = 2.4, inherit.aes = FALSE
      )
    }

    make_catch_plot <- function() {
      view <- view_d()
      pal <- ps()$data$fg_palette
      xlab <- if (isTRUE(input$by_year)) "Year" else "Date"

      if (identical(view, "by_fleet")) {
        df <- fleet_stack()
        req(!is.null(df), nrow(df) > 0)
        agg <- stats::aggregate(catch ~ years + Fleet, data = df, sum)
        n_series <- length(unique(agg$Fleet))
        colors <- get_catch_colors(n_series)
        names(colors) <- unique(as.character(agg$Fleet))
        p <- ggplot2::ggplot(agg, ggplot2::aes(x = years, y = catch, fill = Fleet))
        p <- p + ggplot2::geom_area(position = "stack", alpha = 0.92)
        p <- p + ggplot2::scale_fill_manual(values = colors)
        p <- p + ggplot2::labs(
          title = paste("Catch by fleet —", fg_d()),
          x = xlab, y = "Catch (tons)"
        )
        p <- p + theme_reactiveatlantis()
        p <- ra_scale_x_year(p, agg$years)
        return(observed_overlay(p, fg_d(), agg))
      }

      res <- catch_result()
      req(isTRUE(res$ok), !is.null(res$data), nrow(res$data) > 0)
      catch_df <- res$data
      quantity <- res$quantity
      ylab <- if (identical(quantity, "numbers")) "Catch (numbers)" else "Catch (tons)"

      if (identical(view, "by_group") || identical(quantity, "biomass")) {
        agg <- stats::aggregate(catch ~ years + Functional_group, data = catch_df, sum)
        pal_g <- get_fg_colors(unique(as.character(agg$Functional_group)), pal)
        p <- ggplot2::ggplot(agg, ggplot2::aes(x = years, y = catch, fill = Functional_group))
        p <- p + ggplot2::geom_area(position = "stack", alpha = 0.92)
        p <- p + ggplot2::scale_fill_manual(values = pal_g, name = "Group")
        p <- p + ggplot2::labs(
          title = paste("Catch by group —", input$fishery),
          x = xlab, y = ylab
        )
        p <- p + theme_reactiveatlantis()
        p <- ra_scale_x_year(p, agg$years)
        fg_one <- fg_d()
        if (!is.null(fg_one) && fg_one != "All") {
          p <- observed_overlay(p, fg_one, agg)
        }
        return(p)
      }

      n_series <- length(unique(catch_df$series))
      colors <- get_catch_colors(n_series)
      names(colors) <- unique(as.character(catch_df$series))
      p <- ggplot2::ggplot(catch_df, ggplot2::aes(x = years, y = catch, colour = series))
      p <- p + ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE)
      p <- p + ggplot2::scale_color_manual(values = colors, name = "Cohort")
      if (n_series > 1) {
        p <- p + ggplot2::facet_wrap(~ series, scales = "free_y")
      }
      p <- p + ggplot2::labs(
        title = paste("Catch numbers —", fg_d()),
        x = xlab, y = ylab
      )
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, catch_df$years)
    }

    output$catch_plot_gg <- renderPlot({
      plot_or_message("Load CATCH.nc to plot catch", make_catch_plot())
    })
    if (requireNamespace("plotly", quietly = TRUE)) {
      output$catch_plot <- plotly::renderPlotly({
        p <- make_catch_plot()
        n <- 8
        ra_ggplotly(p, n_series = n)
      })
    }

    output$skill_table <- DT::renderDataTable({
      dat <- ps()$data
      res <- tryCatch(catch_result(), error = function(e) NULL)
      if (!isTRUE(res$ok) || is.null(dat$ext_catch)) {
        return(ra_datatable(data.frame(Note = "Load an external catch CSV to score skill.")))
      }
      fg <- fg_d()
      if (is.null(fg) || identical(fg, "All") || !(fg %in% names(dat$ext_catch))) {
        return(ra_datatable(data.frame(Note = paste("No observed series for", fg))))
      }
      catch_df <- res$data
      catch_agg <- stats::aggregate(catch ~ format(years, "%Y"), data = catch_df, sum)
      names(catch_agg) <- c("year", "modeled")
      time_col <- names(dat$ext_catch)[tolower(names(dat$ext_catch)) %in% c("time", "year", "years")][1]
      ext_df <- data.frame(
        year = as.character(dat$ext_catch[[time_col]]),
        observed = dat$ext_catch[[fg]]
      )
      merged <- merge(catch_agg, ext_df, by = "year")
      merged <- merged[stats::complete.cases(merged), ]
      if (nrow(merged) < 3) {
        return(ra_datatable(data.frame(Note = "Not enough overlapping years for skill assessment")))
      }
      skill <- skill_assessment(merged$observed, merged$modeled, fg)
      ra_datatable(skill, options = list(pageLength = 10, dom = "t"))
    })

    keep_outputs_alive(output, "banner")
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

normalise_groups_catch <- function(grp) {
  names(grp) <- tolower(names(grp))
  if ("code" %in% names(grp)) grp$code <- trimws(as.character(grp$code))
  if ("name" %in% names(grp)) grp$name <- trimws(as.character(grp$name))
  grp
}

fisheries_from_catch_nc <- function(nc) {
  if (is.null(nc) || is.null(nc$var)) return(NULL)
  vars <- names(nc$var)
  hits <- grep("_FC[0-9]+$", vars, value = TRUE)
  if (length(hits) == 0) return(NULL)
  idx <- unique(as.integer(sub(".*_FC", "", hits)))
  idx <- idx[is.finite(idx)]
  if (length(idx) == 0) return(NULL)
  data.frame(
    index = idx,
    code = paste0("FC", idx),
    name = paste0("Fleet ", idx),
    stringsAsFactors = FALSE
  )
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
  data <- nc_get(nc.data, varname)
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
