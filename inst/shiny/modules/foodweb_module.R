## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Food Web Analysis Module        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

foodweb_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    layout_sidebar(
      sidebar = sidebar(
        width = 280, open = "open",
        selectInput(ns("focal_fg"), "Focal group", choices = "All"),
        numericInput(ns("max_connections"), "Max trophic connections", 4, min = 1, max = 10),
        numericInput(ns("min_proportion"), "Min diet proportion", 0.01, min = 0.001, max = 1, step = 0.001),
        sliderInput(ns("time_step"), "Time (days)", min = 0, max = 1, value = 0, step = 1),
        textOutput(ns("time_step_label")),
        selectInput(ns("stock"), "Stock", choices = 0)
      ),
      plotOutput(ns("network_gg"), height = "640px"),
      h4("Trophic levels"),
      DT::dataTableOutput(ns("trophic_table"))
    )
  )
}

foodweb_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {
    ps <- shiny::reactive(project_snapshot(project))
    diet_dat <- shiny::reactive({
      req(ps()$data)
      ensure_diet(ps()$data, ps()$paths)
    })

    output$banner <- renderUI(tool_banner(ps(), "foodweb"))

    observe({
      req(isTRUE(ps()$ready), ps()$data$grp)
      grp <- ps()$data$grp
      code.fg <- grp$code[if ("ispredator" %in% names(grp)) grp$ispredator > 0 else TRUE]
      update_select_keep(session, "focal_fg", c("All", as.character(code.fg)), input$focal_fg)
    })

    output$time_step_label <- renderText({
      dat <- diet_dat()
      req(dat$diet_time, input$time_step)
      tstep <- nearest_time(as.numeric(input$time_step), sort(unique(dat$diet_time$Time)))
      origin <- ra_run_origin(ps)
      when <- days_as_date(tstep, origin)
      paste("Selected:", tstep, "days",
            if (inherits(when, "Date")) paste0("(", format(when, "%Y-%m-%d"), ")") else "")
    })

    time_prey_data <- reactive({
      dat <- diet_dat()
      req(dat$diet_time)
      times <- sort(unique(dat$diet_time$Time))
      stocks <- if ("Stock" %in% names(dat$diet_time)) unique(dat$diet_time$Stock) else 0
      update_select_keep(session, "stock", as.character(stocks), input$stock)
      step <- if (length(times) > 1) min(diff(times)) else 1
      shiny::updateSliderInput(session, "time_step",
                               min = min(times), max = max(times), step = step)
      req(input$time_step)
      tstep <- nearest_time(as.numeric(input$time_step), times)
      dt <- dat$diet_time[Time == tstep]
      if ("Stock" %in% names(dt) && !is.null(input$stock)) {
        dt <- dt[Stock == as.numeric(input$stock)]
      }
      drop <- intersect(c("Time", "Stock", "Cohort"), names(dt))
      as.data.frame(dt[, !drop, with = FALSE])
    })

    prey_data <- reactive({
      dat <- diet_dat()
      req(time_prey_data(), input$focal_fg, dat$grp)
      grp <- dat$grp
      if (!"Code" %in% names(grp) && "code" %in% names(grp)) {
        grp$Code <- grp$code
        grp$GroupType <- grp$grouptype
      }
      code.fg <- grp$code[if ("ispredator" %in% names(grp)) grp$ispredator > 0 else TRUE]
      tot.prey(input$focal_fg, input$max_connections, code.fg, time_prey_data(),
               input$min_proportion, grp)
    })

    trophic_data <- reactive({
      req(prey_data())
      dat <- diet_dat()
      grp <- dat$grp
      if (!"Code" %in% names(grp)) {
        grp$Code <- grp$code
        grp$GroupType <- grp$grouptype
      }
      trophic.lvl(prey_data(), grp, diet = time_prey_data())
    })

    output$network <- if (requireNamespace("visNetwork", quietly = TRUE)) {
      visNetwork::renderVisNetwork({
        TL <- trophic_data()
        edges_src <- prey_data()
        req(TL, edges_src)
        pal <- ps()$data$fg_palette
        nodes <- data.frame(
          id = TL$FG,
          label = TL$FG,
          level = pmax(1, 7 - round(TL$Tlevel)),
          value = 20,
          color = unname(get_fg_colors(as.character(TL$FG), pal)),
          title = paste0(TL$FG, "<br>TL ", round(TL$Tlevel, 2)),
          stringsAsFactors = FALSE
        )
        edges <- data.frame(
          from = edges_src$Pred,
          to = edges_src$Prey,
          value = edges_src$value,
          width = pmax(1, edges_src$value * 18),
          title = paste0(edges_src$Pred, " → ", edges_src$Prey, ": ",
                         round(100 * edges_src$value, 1), "%"),
          stringsAsFactors = FALSE
        )
        vn <- visNetwork::visNetwork(nodes, edges)
        vn <- visNetwork::visNodes(vn, font = list(size = 16))
        vn <- visNetwork::visEdges(vn, arrows = "to", color = list(opacity = 0.45))
        vn <- visNetwork::visHierarchicalLayout(vn, direction = "DU", levelSeparation = 90)
        vn <- visNetwork::visOptions(vn, highlightNearest = TRUE, nodesIdSelection = TRUE)
        visNetwork::visPhysics(vn, enabled = FALSE)
      })
    } else NULL

    output$network_gg <- renderPlot({
      plot_or_message("Load DietCheck.txt to draw the food web", {
        if (!isTRUE(ps()$ready) || is.null(diet_dat()$diet_time)) {
          plot_message("Load DietCheck.txt to draw the food web")
          return(invisible(NULL))
        }
        plot.tlvl(trophic_data(), prey_data(), input$focal_fg, NULL,
                  get_foodweb_colors(), ps()$data$fg_palette)
      })
    })

    output$trophic_table <- DT::renderDataTable({
      td <- tryCatch(trophic_data(), error = function(e) NULL)
      if (is.null(td) || nrow(td) == 0) {
        return(ra_datatable(data.frame(Note = "Load DietCheck.txt to list trophic levels.")))
      }
      table <- with(td, data.frame(
        `Functional Group` = FG,
        `Trophic Level` = round(Tlevel, 2)
      ))
      ra_datatable(table, options = list(pageLength = 15, dom = "tip"))
    })

    keep_outputs_alive(output, "banner")
  })
}

prey.pos <- function(FGs, grp.dat){
  code_col <- if ("Code" %in% names(grp.dat)) grp.dat$Code else grp.dat$code
  type_col <- if ("GroupType" %in% names(grp.dat)) grp.dat$GroupType else grp.dat$grouptype
  ctg <- match(FGs, code_col)
  typ <- type_col[ctg]
  TL.v <- ifelse(typ %in% c('SHARK', 'MAMMAL'), 4,
          ifelse(typ == 'BIRD', 3.5,
          ifelse(typ == 'FISH', 3.24,
          ifelse(typ == 'CEP', 3.2,
          ifelse(typ == 'LG_ZOO', 2.7,
          ifelse(typ == 'FISH_INVERT', 2.52,
          ifelse(typ %in% c('PWN', 'REPTILE', 'MOB_EP_OTHER', 'MED_ZOO'), 2.4,
          ifelse(typ == 'SM_ZOO', 2.4,
          ifelse(typ == 'SED_EP_FF', 2.3,
          ifelse(typ == 'CORAL', 2.2,
          ifelse(typ == 'SED_EP_OTHER', 2.1,
          ifelse(typ == 'LG_PHY', 1.5,
          ifelse(typ %in% c('MICROPHYTOBENTHOS', 'DINOFLAG'), 1.2, 1)))))))))))))
  return(TL.v)
}

Tlevel <- function(DC, TLp){
  tl <- 1 + (sum(DC * TLp, na.rm = TRUE) / sum(DC, na.rm = TRUE))
  return(tl)
}

ord <- function(vector){
  vec <- vector[order(vector)]
  or.v <- order(vector)
  vec.out <- NA * vec
  rev <- length(vec)
  fwd <- 1
  for(v in 1:length(vec)){
    if(v%%2 == 0){
      vec.out[rev] <- or.v[v]
      rev <- rev - 1
      next
    }
    vec.out[fwd] <- or.v[v]
    fwd <- fwd + 1
  }
  return(vec.out)
}

tot.prey <- function(foc.fg, connect, Fg.code, real.pred, min, grp.dat){
  if(foc.fg == 'All') foc.fg <- as.character(Fg.code)
  t.prey <- NULL
  stg <- 1
  while(stg <= connect){
    for(fg in foc.fg){
      diet <- colSums(real.pred[real.pred$Predator == fg, 2:ncol(real.pred)])
      diet <- diet[diet > 0] / sum(diet, na.rm = TRUE)
      if(length(diet) == 0) next
      diet <- diet[diet >= min]
      n.prey <- names(diet)
      m.prey <- data.frame(Pred = fg, Prey = n.prey, value = diet, Stage = stg)
      t.prey <- rbind(t.prey, m.prey)
    }
    foc.fgp <- foc.fg
    foc.fg <- as.character(unique(t.prey$Prey[t.prey$Stage == stg]))
    if(all(length(foc.fg) == length(foc.fgp), all(foc.fg %in% foc.fgp))) break
    stg <- stg + 1
  }
  if(!is.null(t.prey$Prey)){
    t.prey$TLprey <- prey.pos(t.prey$Prey, grp.dat)
    t.prey <- t.prey[!duplicated(t.prey[, c(1, 2)]), ]
    t.prey <- as.data.frame(t.prey)
    return(t.prey)
  } else {
    return(NULL)
  }
}

##' Trophic levels solved for the whole food web from a diet table.
##'
##' TL = 1 for groups that eat nothing (primary producers, detritus,
##' bacteria without diet rows); for consumers TL_i = 1 + sum_j p_ij TL_j,
##' with p_ij the diet proportions of predator i (rows normalised to 1).
##' Solved as the linear system (I - P) TL = 1 (validation N5: producers
##' were hard-coded at 1.5 / 1.2 and the system was never solved).
##' @param diet data.frame with a Predator column and one column per prey
##' @return named numeric vector of trophic levels
solve_trophic_levels <- function(diet) {
  if (is.null(diet) || !nrow(diet) || !"Predator" %in% names(diet)) return(NULL)
  prey_cols <- setdiff(names(diet), c("Time", "Predator", "Stock", "Cohort", "Updated"))
  agg <- stats::aggregate(as.data.frame(diet)[, prey_cols, drop = FALSE],
                          by = list(Predator = as.character(diet$Predator)),
                          FUN = function(x) sum(as.numeric(x), na.rm = TRUE))
  fgs <- union(as.character(agg$Predator), prey_cols)
  n <- length(fgs)
  P <- matrix(0, n, n, dimnames = list(fgs, fgs))
  for (i in seq_len(nrow(agg))) {
    row <- as.numeric(agg[i, prey_cols])
    tot <- sum(row, na.rm = TRUE)
    if (is.finite(tot) && tot > 0) P[agg$Predator[i], prey_cols] <- row / tot
  }
  A <- diag(n) - P
  tl <- tryCatch(solve(A, rep(1, n)), error = function(e) NULL)
  if (is.null(tl) || any(!is.finite(tl))) {
    ## Fall back to fixed-point iteration (e.g. a closed cannibalism loop).
    tl <- rep(1, n)
    for (k in 1:500) {
      new <- 1 + as.numeric(P %*% tl)
      if (max(abs(new - tl)) < 1e-9) { tl <- new; break }
      tl <- new
    }
  }
  stats::setNames(as.numeric(tl), fgs)
}

trophic.lvl <- function(rel.prey, grp.dat, diet = NULL){
  solved <- solve_trophic_levels(diet)
  fgs <- unique(c(as.character(rel.prey$Pred), as.character(rel.prey$Prey)))
  if (!is.null(solved)) {
    tl <- unname(solved[fgs])
    tl[!is.finite(tl)] <- 1
    TL <- data.frame(FG = fgs, Tlevel = tl)
    prey.f <- as.data.frame(rel.prey)
    prey.f$TLprey <- unname(solved[as.character(prey.f$Prey)])
    prey.f$TLprey[!is.finite(prey.f$TLprey)] <- 1
  } else {
    ## No diet table: previous approximation from group types.
    npred <- unique(rel.prey$Pred)
    TL <- NULL
    for(pred in npred){
      pospred <- which(rel.prey$Pred %in% pred)
      TL <- c(TL, Tlevel(rel.prey$value[pospred], rel.prey$TLprey[pospred]))
    }
    left <- rel.prey$Prey[!rel.prey$Prey %in% npred]
    pp.prey <- unique(left)
    pp.prey <- data.frame(FG = pp.prey, Tlevel = prey.pos(pp.prey, grp.dat))
    TL <- rbind(data.frame(FG = npred, Tlevel = TL), pp.prey)
    prey.f <- as.data.frame(rel.prey)
  }
  
  top <- max(6.5, ceiling(max(TL$Tlevel, na.rm = TRUE)) + 0.5)
  his <- hist(TL$Tlevel, breaks = seq(0.5, top, by = 1), plot = FALSE)
  v.lev <- max(his$counts)
  brk <- his$breaks
  h.lev <- max(his$mids[his$counts > 0]) + 1
  TL$v.lev <- v.lev
  TL$h.lev <- h.lev
  TL$vpos <- NA
  for(i in 1:(length(brk) - 1)){
    nfg.ly <- which(TL$Tlevel >= brk[i] & TL$Tlevel < brk[i + 1])
    tot.fg <- length(nfg.ly)
    vpos <- cumsum(rep(v.lev / tot.fg, tot.fg)) - (v.lev / tot.fg) * 0.5
    pos.or <- vector('numeric', length(TL$FG[nfg.ly]))
    for(j in 1:length(TL$FG[nfg.ly])){
      pos.or[j] <- sum(c(prey.f$Pred %in% TL$FG[nfg.ly][j], prey.f$Prey %in% TL$FG[nfg.ly][j]))
    }
    TL$vpos[nfg.ly] <- vpos[ord(pos.or)]
  }
  TL <- as.data.frame(TL)
  return(TL)
}

plot.tlvl <- function(T.lvl, rel.prey, foc.fg, pol = NULL, color.p, fg_colors = NULL){
  rad <- 0.25
  y.lab <- "Trophic-level"
  if(!is.null(pol)) y.lab <- paste0("Trophic-level at polygon ", as.numeric(pol))
  plot(1, type = "n", xlab = '', ylab = y.lab,
       xlim = c(0, unique(T.lvl$v.lev)), ylim = c(0.5, unique(T.lvl$h.lev)), axes = FALSE)
  axis(2, at = c(0.5:unique(T.lvl$h.lev)), las = 1)
  for(i in 1:nrow(rel.prey)){
    p.pred <- which(T.lvl$FG %in% rel.prey$Pred[i])
    p.prey <- which(T.lvl$FG %in% rel.prey$Prey[i])
    col <- ifelse(T.lvl$Tlevel[p.pred] < T.lvl$Tlevel[p.prey], color.p[1], color.p[2])
    y <- c(T.lvl$Tlevel[p.pred], T.lvl$Tlevel[p.prey])
    x <- c(T.lvl$vpos[p.pred], T.lvl$vpos[p.prey])
    lines(x, y, lty = 1, lwd = (rel.prey$value[i] * 3), col = col)
  }
  for(i in 1:nrow(T.lvl)){
    fg <- as.character(T.lvl$FG[i])
    node_col <- if (!is.null(fg_colors) && fg %in% names(fg_colors)) {
      unname(fg_colors[fg])
    } else if (i == 1 && foc.fg != 'All') {
      'steelblue'
    } else {
      'gray91'
    }
    rgb <- grDevices::col2rgb(node_col)
    lum <- 0.299 * rgb[1, 1] + 0.587 * rgb[2, 1] + 0.114 * rgb[3, 1]
    txt_col <- if (lum < 140) "white" else "black"
    plotrix::draw.circle(T.lvl$vpos[i], T.lvl$Tlevel[i], radius = rad, nv = 1500, border = NULL,
                        col = node_col, lty = 1, lwd = 1)
    text(T.lvl$vpos[i], T.lvl$Tlevel[i], T.lvl$FG[i], cex = .8, font = 2, col = txt_col)
  }
  legend('topright', c('DownTop', 'TopDown'), lty = 1, col = c(color.p[1], color.p[2]), lwd = 1.5, bty = 'n')
}
