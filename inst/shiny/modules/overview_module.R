## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~              Overview / health             ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

overview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("cards")),
    shiny::uiOutput(ns("health_panel"))
  )
}

overview_server <- function(id, project) {
  shiny::moduleServer(id, function(input, output, session) {

    health <- shiny::reactive({
      dat <- project$data
      shiny::req(dat, dat$grp, dat$biomindx)
      biomass_health(dat$biomindx, dat$grp)
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

      bslib::layout_column_wrap(
        width = 1/5,
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
        )
      )
    })

    output$health_panel <- shiny::renderUI({
      if (!isTRUE(project$ready)) return(NULL)
      dat <- project$data
      sm <- summarise_run(dat$grp, dat$nc_out, dat$biomindx, dat$time_dates)
      start_lab <- if (!is.na(sm$origin)) format(sm$origin, "%Y-%m-%d") else "not in NetCDF"
      ns <- session$ns
      shiny::tagList(
        bslib::card(
          bslib::card_header("Run window"),
          bslib::card_body(
            shiny::p(shiny::strong("Start date (from NetCDF time units): "), start_lab),
            shiny::p(shiny::strong("Time span: "), sm$time_span)
          )
        ),
        if (is.null(dat$biomindx)) {
          bslib::card(
            bslib::card_header("Group health"),
            bslib::card_body("Load BiomIndx.txt to flag groups that collapse below 1% of initial biomass or grow more than tenfold.")
          )
        } else {
          bslib::card(
            bslib::card_header("Group health (final / initial biomass)"),
            bslib::card_body(DT::dataTableOutput(ns("health_table")))
          )
        }
      )
    })

    output$health_table <- DT::renderDataTable({
      tbl <- health()
      tbl$Ratio <- round(tbl$Ratio, 3)
      tbl$Initial <- signif(tbl$Initial, 4)
      tbl$Final <- signif(tbl$Final, 4)
      DT::datatable(
        tbl,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 20, order = list(list(5, "desc")))
      )
    })
  })
}
