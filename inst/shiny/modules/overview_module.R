## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~              Overview / health             ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("cards")),
    shiny::uiOutput(ns("run_window")),
    bslib::card(
      bslib::card_header("Group health (final / initial biomass)"),
      bslib::card_body(DT::dataTableOutput(ns("health_table")))
    ),
    bslib::card(
      bslib::card_header("Problem cohorts (StructN / ResN relative to t0)"),
      bslib::card_body(
        shiny::p(class = "ra-muted",
                 "Age classes outside the 0.5–1.5 band at the last time step. The same band is drawn on Compare relative views."),
        DT::dataTableOutput(ns("cohort_table"))
      )
    )
  )
}

overview_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {

    health <- shiny::reactive({
      dat <- project$data
      if (is.null(dat) || is.null(dat$grp) || is.null(dat$biomindx)) return(NULL)
      biomass_health(dat$biomindx, dat$grp)
    })

    cohorts <- shiny::reactive({
      dat <- project$data
      if (is.null(dat) || is.null(dat$grp) || is.null(dat$nc_out)) return(NULL)
      problem_cohorts(dat$nc_out, dat$grp, ACCEPT_BAND)
    })

    output$cards <- shiny::renderUI({
      if (!isTRUE(project$ready) || is.null(project$data)) {
        return(bslib::card(
          bslib::card_header("Load a run"),
          bslib::card_body(
            shiny::p("Point the left panel at an Atlantis output folder, or attach the individual files, then click Load run."),
            shiny::p(class = "ra-muted",
                     "A local folder path avoids browser upload limits for large NetCDF and DietCheck files.")
          )
        ))
      }
      dat <- project$data
      sm <- summarise_run(dat$grp, dat$nc_out, dat$biomindx, dat$time_dates)
      flags <- tryCatch(health(), error = function(e) NULL)
      n_ext <- if (is.null(flags)) NA else sum(flags$Flag == "Extinct (<1%)")
      n_exp <- if (is.null(flags)) NA else sum(flags$Flag == "Exploding (>10x)")
      coh <- tryCatch(cohorts(), error = function(e) NULL)
      n_coh <- if (is.null(coh)) NA else sum(coh$Flag == "Outside band")

      bslib::layout_column_wrap(
        width = 1/6,
        fill = FALSE,
        bslib::value_box(
          title = "Groups on",
          value = sm$n_groups,
          showcase = shiny::icon("layer-group"),
          theme = "primary"
        ),
        bslib::value_box(
          title = "Boxes",
          value = ifelse(is.na(sm$n_boxes), "—", sm$n_boxes),
          showcase = shiny::icon("border-all"),
          theme = "secondary"
        ),
        bslib::value_box(
          title = "Layers (z)",
          value = ifelse(is.na(sm$n_layers), "—", sm$n_layers),
          showcase = shiny::icon("water"),
          theme = "secondary"
        ),
        bslib::value_box(
          title = "Extinct (<1%)",
          value = ifelse(is.na(n_ext), "—", n_ext),
          theme = if (!is.na(n_ext) && n_ext > 0) "danger" else "success"
        ),
        bslib::value_box(
          title = "Exploding (>10x)",
          value = ifelse(is.na(n_exp), "—", n_exp),
          theme = if (!is.na(n_exp) && n_exp > 0) "warning" else "success"
        ),
        bslib::value_box(
          title = "Problem cohorts",
          value = ifelse(is.na(n_coh), "—", n_coh),
          theme = if (!is.na(n_coh) && n_coh > 0) "warning" else "success"
        )
      )
    })

    output$run_window <- shiny::renderUI({
      if (!isTRUE(project$ready)) return(NULL)
      dat <- project$data
      sm <- summarise_run(dat$grp, dat$nc_out, dat$biomindx, dat$time_dates)
      start_lab <- if (!is.na(sm$origin)) format(sm$origin, "%Y-%m-%d") else "not in NetCDF"
      bslib::card(
        bslib::card_header("Run window"),
        bslib::card_body(
          shiny::p(shiny::strong("Start date (from NetCDF time units): "), start_lab),
          shiny::p(shiny::strong("Time span: "), sm$time_span)
        )
      )
    })

    output$health_table <- DT::renderDataTable({
      tbl <- health()
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(ra_datatable(data.frame(Note = "Load BiomIndx.txt to flag groups that collapse or explode.")))
      }
      tbl$Ratio <- round(tbl$Ratio, 3)
      tbl$Initial <- signif(tbl$Initial, 4)
      tbl$Final <- signif(tbl$Final, 4)
      ra_datatable(tbl, filter = "top",
                   options = list(pageLength = 20, order = list(list(5, "desc"))))
    })

    output$cohort_table <- DT::renderDataTable({
      tbl <- cohorts()
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(ra_datatable(data.frame(Note = "Load the biology output NetCDF to flag StructN / ResN cohorts.")))
      }
      tbl$Relative <- round(tbl$Relative, 3)
      tbl$Initial <- signif(tbl$Initial, 4)
      tbl$Final <- signif(tbl$Final, 4)
      ra_datatable(tbl, filter = "top",
                   options = list(pageLength = 15, order = list(list(7, "desc"))))
    })

    keep_outputs_alive(output, c("cards", "run_window", "health_table", "cohort_table"))
  })
}
