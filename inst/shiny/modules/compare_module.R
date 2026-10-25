## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Compare Outputs Module           ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        open = "open",
        width = 280,
        selectInput(ns("group_type"), "Group type", choices = "All"),
        textInput(ns("search"), "Search code or name", placeholder = "COD, haddock…"),
        checkboxInput(ns("relative"), "Relative to initial (Bt/B0)", FALSE),
        checkboxInput(ns("show_band"), "0.5–1.5 acceptable band", TRUE),
        selectInput(ns("y_scale"), "Y axis",
                    choices = c("Free (each panel)" = "free",
                                "Log scale" = "log",
                                "Shared" = "shared"),
                    selected = "free"),
        hr(),
        selectInput(ns("view"), "View",
                    choices = c("Small multiples" = "facets",
                                "One group, totals" = "total",
                                "By age class" = "age",
                                "Age structure (bars)" = "age_bars")),
        conditionalPanel(
          "input.view == 'total'", ns = ns,
          selectInput(ns("fg_total"), "Functional group", choices = character()),
          checkboxInput(ns("show_biomass"), "Biomass", TRUE),
          checkboxInput(ns("show_numbers"), "Numbers", FALSE),
          checkboxInput(ns("scaled"), "Scaled", TRUE),
          checkboxInput(ns("limit_axis"), "Limit axis", TRUE)
        ),
        conditionalPanel(
          "input.view == 'age' || input.view == 'age_bars'", ns = ns,
          selectInput(ns("fg_age"), "Functional group", choices = character()),
          checkboxInput(ns("show_biomass_age"), "Biomass", TRUE),
          checkboxInput(ns("show_numbers_age"), "Numbers", FALSE),
          checkboxInput(ns("scaled_age"), "Scaled", TRUE),
          checkboxInput(ns("limit_axis_age"), "Limit axis", TRUE)
        )
      ),
      conditionalPanel("input.view == 'facets'", ns = ns,
                       plotOutput(ns("plot_facets_gg"), height = "2200px")),
      conditionalPanel("input.view == 'total'", ns = ns,
                       plotOutput(ns("plot_total"), height = "560px")),
      conditionalPanel("input.view == 'age'", ns = ns,
                       plotOutput(ns("plot_age"), height = "640px")),
      conditionalPanel("input.view == 'age_bars'", ns = ns,
                       plotOutput(ns("plot_age_bars"), height = "640px"))
    )
  )
}

compare_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))

    output$banner <- renderUI(tool_banner(ps(), "compare"))

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      grp <- ps()$data$grp
      types <- c("All", sort(unique(as.character(grp$grouptype))))
      update_select_keep(session, "group_type", types, input$group_type)
      update_select_keep(session, "fg_total", fg_select_choices(grp), input$fg_total)
      update_select_keep(session, "fg_age", age_select_choices(grp), input$fg_age)
    })

    search_d <- ra_debounce(shiny::reactive(input$search), 400)
    fg_total_d <- ra_debounce(shiny::reactive(input$fg_total), 250)
    fg_age_d <- ra_debounce(shiny::reactive(input$fg_age), 250)

    compare_tables <- reactive({
      dat <- ps()$data
      req(isTRUE(ps()$ready), dat$grp)
      t.biomass <- NULL
      if (!is.null(dat$biomindx) && NROW(dat$biomindx) > 0) {
        t.biomass <- tryCatch(
          biomindx_facet_df(dat$biomindx, dat$grp, dat$origin),
          error = function(e) NULL
        )
      }
      if (is.null(t.biomass) || nrow(t.biomass) == 0) {
        dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
        req(dat$nc_out, dat$inf_box, dat$time_dates)
        t.biomass <- nc_facet_biomass(dat)
      }
      req(!is.null(t.biomass), nrow(t.biomass) > 0)
      rel.bio <- do.call(rbind.data.frame, by(t.biomass, t.biomass$FG, relative))
      list(t_biomass = t.biomass, rel_bio = rel.bio)
    })

    filtered <- reactive({
      tabs <- compare_tables()
      req(tabs$t_biomass, ps()$data$grp)
      df <- if (isTRUE(input$relative)) tabs$rel_bio else tabs$t_biomass
      grp <- ps()$data$grp
      if (!is.null(input$group_type) && input$group_type != "All") {
        codes <- grp$code[grp$grouptype == input$group_type]
        keep <- grepl(paste0("\\((", paste(codes, collapse = "|"), ")\\)$"), df$FG)
        df <- df[keep, ]
      }
      q <- search_d()
      if (!is.null(q) && nzchar(trimws(q))) {
        df <- df[grepl(trimws(q), df$FG, ignore.case = TRUE), ]
      }
      df
    })

    make_facet_plot <- function() {
      df <- filtered()
      req(nrow(df) > 0)
      if (!"Code" %in% names(df)) {
        df$Code <- code_from_fg_label(as.character(df$FG))
      }
      ycol <- if ("Relative" %in% names(df) && isTRUE(input$relative)) "Relative" else "Biomass"
      ylab <- if (isTRUE(input$relative)) "Relative biomass (Bt/B0)" else "Biomass (tons)"
      y_scale <- if (is.null(input$y_scale) || !nzchar(input$y_scale)) "free" else input$y_scale
      pal <- get_fg_colors(unique(as.character(df$Code)), ps()$data$fg_palette)
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = Time, y = .data[[ycol]], colour = Code,
                     group = interaction(Code, Simulation, drop = TRUE))
      )
      p <- p + ggplot2::geom_line(ggplot2::aes(linetype = Simulation),
                                  linewidth = 0.7, na.rm = TRUE)
      facet_scales <- if (identical(y_scale, "shared")) "fixed" else "free_y"
      p <- p + ggplot2::facet_wrap(~ FG, ncol = 4, scales = facet_scales)
      p <- p + ggplot2::scale_color_manual(values = pal, guide = "none")
      if (length(unique(as.character(df$Simulation))) <= 1) {
        p <- p + ggplot2::scale_linetype_discrete(guide = "none")
      }
      p <- p + ggplot2::labs(
        x = "Year", y = ylab,
        title = "Biomass by functional group",
        subtitle = paste(length(unique(df$FG)), "groups")
      )
      p <- p + theme_reactiveatlantis()
      p <- p + ggplot2::theme(
        axis.text = ggplot2::element_text(size = 10, colour = RA_MUTED),
        axis.text.x = ggplot2::element_text(size = 10, colour = RA_MUTED),
        axis.text.y = ggplot2::element_text(size = 10, colour = RA_MUTED),
        strip.text = ggplot2::element_text(size = 10, face = "bold", colour = RA_NAVY),
        panel.spacing = ggplot2::unit(10, "pt")
      )
      xr <- range(df$Time, na.rm = TRUE)
      if (inherits(df$Time, "Date")) {
        br <- pretty(xr, n = 4)
        p <- p + ggplot2::scale_x_date(breaks = br, date_labels = "%Y")
      } else {
        p <- p + ggplot2::scale_x_continuous(breaks = pretty(as.numeric(xr), n = 4))
      }
      if (identical(y_scale, "log")) {
        p <- p + ggplot2::scale_y_log10()
      }
      if (isTRUE(input$relative) && isTRUE(input$show_band) && nrow(df) > 0) {
        p <- acceptable_band_layer(p, ACCEPT_BAND[1], ACCEPT_BAND[2],
                                   xmin = min(df$Time), xmax = max(df$Time))
      } else if (!isTRUE(input$relative)) {
        obs <- ps()$data$observations
        sub <- obs_for(obs, unique(df$Code), "Biomass")
        if (!is.null(sub) && nrow(sub) > 0) {
          lookup <- unique(data.frame(Code = df$Code, FG = df$FG, stringsAsFactors = FALSE))
          sub$FG <- lookup$FG[match(sub$Code, lookup$Code)]
          sub <- sub[!is.na(sub$FG), , drop = FALSE]
          if (nrow(sub) > 0 && any(is.finite(sub$Lower) & is.finite(sub$Upper))) {
            p <- p + ggplot2::geom_ribbon(
              data = sub, ggplot2::aes(x = Time, ymin = Lower, ymax = Upper),
              inherit.aes = FALSE, fill = "#C4A35A", alpha = 0.25
            )
          }
          if (nrow(sub) > 0) {
            p <- p + ggplot2::geom_point(
              data = sub, ggplot2::aes(x = Time, y = Value),
              inherit.aes = FALSE, colour = "#A33B20", size = 1.8
            )
          }
        }
      }
      p
    }

    output$plot_facets_gg <- renderPlot({
      tabs <- tryCatch(compare_tables(), error = function(e) NULL)
      if (!isTRUE(ps()$ready) || is.null(tabs) || is.null(tabs$t_biomass)) {
        plot_message("Load a run to compare biomass")
        return(invisible(NULL))
      }
      make_facet_plot()
    }, height = function() {
      n <- 8L
      df <- tryCatch(filtered(), error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) n <- length(unique(df$FG))
      as.integer(max(700, ceiling(max(n, 1) / 4) * 175))
    })

    output$plot_total <- renderPlot({
      plot_or_message("Load a run and pick a functional group", {
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out, fg_total_d(), dat$inf_box, dat$time_dates)
      mg2t <- 0.00000002
      x.cn <- 5.7
      total_data <- nitro.weight(dat$nc_out, dat$grp, fg_total_d(), By = "Total",
                                dat$inf_box, mg2t, x.cn)
      if (isTRUE(input$scaled)) {
        rmv <- which(sapply(total_data, function(x) length(x) == 0 || is.null(x) || is.character(x)))
        total.tmp <- total_data[-rmv]
        total.tmp <- lapply(total.tmp, ra_rel_to_first)
        total_data[-rmv] <- total.tmp
      }
      p <- plot_total_data(total_data, dat$time_dates, FALSE, FALSE,
                           isTRUE(input$show_numbers), isTRUE(input$show_biomass),
                           isTRUE(input$scaled), isTRUE(input$limit_axis))
      if (inherits(p, "ggplot")) {
        p <- p + theme_reactiveatlantis()
        if (isTRUE(input$scaled) && isTRUE(input$show_band)) {
          p <- acceptable_band_layer(p, ACCEPT_BAND[1], ACCEPT_BAND[2],
                                     xmin = min(dat$time_dates), xmax = max(dat$time_dates))
        }
        if (!isTRUE(input$scaled)) {
          p <- add_obs_overlay(p, dat$observations, fg_total_d(), "Biomass")
        }
        p
      } else p
      })
    })

    output$plot_age <- renderPlot({
      plot_or_message("Load a run and pick an age-structured group", {
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out, fg_age_d(), dat$inf_box, dat$time_dates)
      mg2t <- 0.00000002
      x.cn <- 5.7
      coho <- nitro.weight(dat$nc_out, dat$grp, FG = fg_age_d(), By = "Cohort",
                          dat$inf_box, mg2t, x.cn)
      if (isTRUE(input$scaled_age)) {
        rmv <- which(sapply(coho, function(x) length(x) == 0 || is.null(x) || is.character(x)))
        coho.tmp <- coho[-rmv]
        coho.tmp <- lapply(coho.tmp, function(x) lapply(x, ra_rel_to_first))
        coho[-rmv] <- coho.tmp
      }
      n.coh <- dat$grp$numcohorts[dat$grp$code == fg_age_d()]
      p <- plot_cohorts_ggplot(coho, dat$time_dates, FALSE, FALSE,
                              isTRUE(input$show_numbers_age), isTRUE(input$show_biomass_age),
                              isTRUE(input$scaled_age), isTRUE(input$limit_axis_age), n.coh)
      if (inherits(p, "ggplot")) {
        p <- p + theme_reactiveatlantis()
        if (isTRUE(input$scaled_age) && isTRUE(input$show_band)) {
          p <- acceptable_band_layer(p, ACCEPT_BAND[1], ACCEPT_BAND[2],
                                     xmin = min(dat$time_dates), xmax = max(dat$time_dates))
        }
        p
      } else p
      })
    })

    output$plot_age_bars <- renderPlot({
      plot_or_message("Load a run and pick an age-structured group", {
      dat <- ensure_project_data(ps(), c("nc_out", "inf_box"))
      req(dat$nc_out, fg_age_d(), dat$inf_box, dat$time_dates)
      mg2t <- 0.00000002
      x.cn <- 5.7
      coho <- nitro.weight(dat$nc_out, dat$grp, FG = fg_age_d(), By = "Cohort",
                          dat$inf_box, mg2t, x.cn)
      use_num <- isTRUE(input$show_numbers_age) && !isTRUE(input$show_biomass_age)
      src <- if (use_num) coho$Numbers else coho$Biomass
      req(!is.null(src), length(src) > 0)
      n.coh <- length(src)
      rows <- lapply(seq_len(n.coh), function(i) {
        vals <- as.numeric(unlist(src[[i]]))
        data.frame(
          Time = dat$time_dates[seq_along(vals)],
          Value = vals,
          Age = sprintf("Age%02d", i),
          stringsAsFactors = FALSE
        )
      })
      plot_df <- do.call(rbind, rows)
      ylab <- if (use_num) "Numbers" else "Biomass (tons)"
      n_t <- length(unique(plot_df$Time))
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Time, y = Value, fill = Age))
      if (n_t <= 40) {
        p <- p + ggplot2::geom_col(position = "stack", width = 20)
      } else {
        p <- p + ggplot2::geom_area(position = "stack")
      }
      pal <- get_mortality_colors(n.coh)
      names(pal) <- sprintf("Age%02d", seq_len(n.coh))
      p <- p + ggplot2::scale_fill_manual(values = pal)
      p <- p + ggplot2::labs(title = paste("Age structure —", fg_age_d()),
                             x = "Year", y = ylab)
      p <- p + theme_reactiveatlantis()
      ra_scale_x_year(p, plot_df$Time)
      })
    })

    keep_outputs_alive(output, "banner")
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Helper Functions                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' Small-multiples table from BiomIndx.txt (every group code, not just vertebrates)
biomindx_facet_df <- function(biomindx, grp, origin = NULL) {
  biom <- as.data.frame(biomindx)
  names(biom) <- trimws(as.character(names(biom)))
  skip <- c("Time", grep("^(Rel|Mig)", names(biom), value = TRUE),
            "PelDemRatio", "PiscivPlankRatio", "DivCount", "InfEpiRatio",
            "BSSslope", "HabitCover", "DIN")
  codes <- as.character(grp$code)
  if ("isturnedon" %in% names(grp)) {
    on <- grp$code[grp$isturnedon == 1]
    if (length(on) > 0) codes <- as.character(on)
  }
  codes <- codes[codes %in% names(biom)]
  if (length(codes) == 0) {
    codes <- setdiff(names(biom), skip)
  }
  time_raw <- as.numeric(biom[[if ("Time" %in% names(biom)) "Time" else names(biom)[1]]])
  if (length(time_raw) > 0 && is.finite(max(time_raw, na.rm = TRUE)) &&
      max(time_raw, na.rm = TRUE) > 1e6) {
    time_raw <- time_raw / 86400
  }
  Time <- days_as_date(time_raw, origin)
  longname <- if ("longname" %in% names(grp)) {
    grp$longname[match(codes, grp$code)]
  } else codes
  longname[is.na(longname) | !nzchar(as.character(longname))] <- codes
  rows <- lapply(seq_along(codes), function(i) {
    code <- codes[i]
    data.frame(
      Time = Time,
      FG = paste0(as.character(longname[i]), " (", code, ")"),
      Code = code,
      Biomass = as.numeric(biom[[code]]),
      Simulation = "Current",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

nc_facet_biomass <- function(dat) {
  grp <- dat$grp[dat$grp$isturnedon == 1,
                 c("code", "name", "longname", "grouptype", "numcohorts")]
  mg2t <- 0.00000002
  x.cn <- 5.7
  Time <- dat$time_dates
  inf_box <- dat$inf_box
  nc_cur <- dat$nc_out
  age.grp <- grp[grp$numcohorts > 1 & !grp$grouptype %in% AGE_SPECIAL, ]
  pol.grp <- grp[grp$numcohorts == 1, ]
  pwn.grp <- grp[grp$numcohorts > 1 & grp$grouptype %in% AGE_SPECIAL, ]
  age.cur.bio <- if (nrow(age.grp) > 0) {
    tryCatch(bio.age(age.grp, nc_cur, "Current", mg2t, x.cn, inf_box, Time),
             error = function(e) NULL)
  } else NULL
  pool.cur.bio <- if (nrow(pol.grp) > 0) {
    tryCatch(bio.pool(pol.grp, nc_cur, "Current", mg2t, x.cn, inf_box, Time),
             error = function(e) NULL)
  } else NULL
  old.bio <- NULL
  Time_old <- NULL
  if (!is.null(dat$nc_old)) {
    Time_old <- tryCatch(time_calc(dat$nc_old), error = function(e) Time)
    age.old.bio <- if (nrow(age.grp) > 0) {
      tryCatch(bio.age(age.grp, dat$nc_old, "Previous", mg2t, x.cn, inf_box, Time_old),
               error = function(e) NULL)
    } else NULL
    pool.old.bio <- tryCatch(
      bio.pool(pol.grp, dat$nc_old, "Previous", mg2t, x.cn, inf_box, Time_old),
      error = function(e) NULL
    )
    old.bio <- rbind(pool.old.bio, age.old.bio)
  }
  pwn.bio <- NULL
  if (nrow(pwn.grp) > 0) {
    pwn.cur.bio <- tryCatch(
      bio.pwn(pwn.grp, nc_cur, "Current", mg2t, x.cn, inf_box, Time),
      error = function(e) NULL
    )
    pwn.bio <- pwn.cur.bio
    if (!is.null(dat$nc_old)) {
      pwn.old <- tryCatch(
        bio.pwn(pwn.grp, dat$nc_old, "Previous", mg2t, x.cn, inf_box, Time_old),
        error = function(e) NULL
      )
      pwn.bio <- rbind(pwn.cur.bio, pwn.old)
    }
  }
  out <- rbind(age.cur.bio, pool.cur.bio, old.bio, pwn.bio)
  if (!is.null(out) && nrow(out) > 0 && !"Code" %in% names(out)) {
    out$Code <- code_from_fg_label(as.character(out$FG))
  }
  out
}

## Series relative to its first value; NA when that value is zero or not
## finite, so an undefined ratio is never drawn (validation D22).
ra_rel_to_first <- function(x) {
  x <- as.numeric(x)
  if (!length(x) || !is.finite(x[1]) || x[1] <= 0) return(rep(NA_real_, length(x)))
  x / x[1]
}

##' Pool biomass in tonnes per box and time [b, t] from one NetCDF variable:
##' 3-D (mg N m-3) x cell volume from the NetCDF, summed over layers;
##' 2-D epibenthic (mg N m-2) x box area. Uses the NetCDF geometry, not the
##' BGM-derived volumes (validation D19b, D31).
pool_mass_box_time <- function(nc.out, vname, mg2t, x.cn) {
  biom <- nc_get(nc.out, vname)
  if (length(dim(biom)) == 3) {
    vol <- nc_volume(nc.out)
    if (is.null(vol)) stop("output.nc has no volume variable for ", vname)
    if (length(dim(vol)) == 2) {
      m <- apply(sweep(biom, c(1, 2), vol, "*"), c(2, 3), sum, na.rm = TRUE)
    } else {
      n <- min(dim(biom)[3], dim(vol)[3])
      m <- apply(biom[, , seq_len(n), drop = FALSE] * vol[, , seq_len(n), drop = FALSE],
                 c(2, 3), sum, na.rm = TRUE)
    }
  } else {
    area <- nc_box_area(nc.out)
    if (is.null(area) || length(area) != nrow(biom)) stop("Box area is unavailable for ", vname)
    m <- sweep(biom, 1, area, "*")
  }
  m * mg2t * x.cn
}

## Boxes counted in system totals: BiomIndx excludes boundary boxes.
inner_boxes <- function(box.info, n_box) {
  db <- tryCatch(box.info$info$Depth_Bound, error = function(e) NULL)
  if (is.null(db) || length(db) != n_box) return(rep(TRUE, n_box))
  db > 0
}

bio.age <- function(age.grp, nc.out, ctg, mg2t, x.cn, inf.box, Time){
  bound <- ifelse(inf.box$info$Depth_Bound > 0, 1, 0)
  grp.bio <- NULL
  
  for(age in 1:nrow(age.grp)){
    cohort <- NULL
    for(coh in 1:age.grp[age, 'numcohorts']){
      name.fg <- paste0(age.grp$name[age], coh)
      b.coh <- (nc_get(nc.out, paste0(name.fg, '_ResN')) +
                nc_get(nc.out, paste0(name.fg, '_StructN'))) *
               nc_get(nc.out, paste0(name.fg, '_Nums')) * mg2t * x.cn
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
    m <- pool_mass_box_time(nc.out, paste0(pol.grp$name[pool], '_N'), mg2t, x.cn)
    keep <- inner_boxes(box.info, nrow(m))
    biom <- colSums(m[keep, , drop = FALSE], na.rm = TRUE)
    grp.bio <- rbind(grp.bio, data.frame(
      Time = Time, 
      FG = paste0(as.character(pol.grp$longname[pool]), ' (', as.character(pol.grp$code[pool]), ')'),
      Biomass = biom[seq_along(Time)], 
      Simulation = ctg
    ))
  }
  return(grp.bio)
}

bio.pwn <- function(pwn.grp, nc.out, ctg, mg2t, x.cn, box.info, Time){
  grp.bio <- NULL
  for(pwn in 1:nrow(pwn.grp)){
    tot <- 0
    for(coh in 1:pwn.grp[pwn, 'numcohorts']){
      vn <- paste0(pwn.grp$name[pwn], '_N', coh)
      if (!vn %in% names(nc.out$var)) next
      m <- pool_mass_box_time(nc.out, vn, mg2t, x.cn)
      tot <- tot + colSums(m[inner_boxes(box.info, nrow(m)), , drop = FALSE], na.rm = TRUE)
    }
    grp.bio <- rbind(grp.bio, data.frame(
      Time = Time,
      FG = paste0(as.character(pwn.grp$longname[pwn]), ' (', as.character(pwn.grp$code[pwn]), ')'),
      Biomass = tot[seq_along(Time)], 
      Simulation = ctg
    ))
  }
  return(grp.bio)
}

relative <- function(df, biomass = TRUE, Vec = NULL){
  if(biomass) vector <- df$Biomass
  if(!is.null(Vec)) vector <- df[Vec]
  base <- vector[1]
  ## B/B0 is undefined when B0 is zero or not finite (validation D22).
  df$Relative <- if (is.finite(base) && base > 0) vector / base else NA_real_
  return(df)
}

nitro.weight <- function(nc.out, grp, FG, By = 'Total', box.info, mg2t, x.cn, polnum = NULL){
  pos.fg <- which(grp$code == FG)
  Bio <- Num <- SN <- RN <- list()
  
  if(grp[pos.fg, 'numcohorts'] > 1 & !grp[pos.fg, 'grouptype'] %in% c('PWN', 'PRAWNS', 'PRAWN', 'CEP', 'MOB_EP_OTHER', 'SEAGRASS', 'CORAL', 'MANGROVE', 'MANGROVES', 'SPONGE')){
    n.coh <- grp[pos.fg, 'numcohorts']
    for(coh in 1:n.coh){
      name.fg <- paste0(grp$name[pos.fg], coh)
      nums <- nc_get(nc.out, paste0(name.fg, '_Nums'))
      resN <- nc_get(nc.out, paste0(name.fg, '_ResN'))
      strN <- nc_get(nc.out, paste0(name.fg, '_StructN'))
      
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
  } else {
    ## Pools (one cohort) and multi-cohort pools (PWN, CEP, ...): tonnes from
    ## NetCDF volume / area, boundary boxes excluded as in BiomIndx (D31, D19b).
    vnames <- pool_var_names(names(nc.out$var), grp$name[pos.fg],
                             grp[pos.fg, 'numcohorts'], grp$grouptype[pos.fg])
    if (!length(vnames)) stop("No biomass pool variable for ", FG)
    m <- NULL
    for (vn in vnames) {
      x <- pool_mass_box_time(nc.out, vn, mg2t, x.cn)
      m <- if (is.null(m)) x else m + x
    }
    keep <- inner_boxes(box.info, nrow(m))
    Bio <- if (By == 'Total') colSums(m[keep, , drop = FALSE], na.rm = TRUE) else m
    type <- if (length(vnames) > 1) 'AgeBioPool' else 'BioPool'
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
