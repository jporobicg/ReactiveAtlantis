## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Food Web Analysis Module        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

foodweb_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("body")))
}

foodweb_server <- function(id, project = NULL) {
  moduleServer(id, function(input, output, session) {

    output$body <- renderUI({
      if (is.null(project) || !isTRUE(project$ready)) {
        return(tool_missing_card(FILE_LABELS[TOOL_REQUIREMENTS$foodweb], "Food web"))
      }
      miss <- tool_missing(project$paths, "foodweb")
      if (length(miss)) return(tool_missing_card(miss, "Food web"))
      dat <- project$data
      times <- sort(unique(dat$diet_time$Time))
      stocks <- if ("Stock" %in% names(dat$diet_time)) unique(dat$diet_time$Stock) else 0
      grp <- dat$grp
      code.fg <- grp$code[if ("ispredator" %in% names(grp)) grp$ispredator > 0 else TRUE]
      ns <- session$ns
      layout_sidebar(
        sidebar = sidebar(
          width = 280, open = "open",
          selectInput(ns("focal_fg"), "Focal group", choices = c("All", as.character(code.fg))),
          numericInput(ns("max_connections"), "Max trophic connections", 4, min = 1, max = 10),
          numericInput(ns("min_proportion"), "Min diet proportion", 0.01, min = 0.001, max = 1, step = 0.001),
          sliderInput(ns("time_step"), "Time (days)",
                      min = min(times), max = max(times), value = times[1],
                      step = if (length(times) > 1) min(diff(times)) else 1),
          textOutput(ns("time_step_label")),
          selectInput(ns("stock"), "Stock", choices = stocks)
        ),
        uiOutput(ns("net_host")),
        h4("Trophic levels"),
        DT::dataTableOutput(ns("trophic_table"))
      )
    })

    output$time_step_label <- renderText({
      dat <- project$data
      req(dat$diet_time, input$time_step)
      tstep <- nearest_time(as.numeric(input$time_step), sort(unique(dat$diet_time$Time)))
      origin <- dat$origin
      when <- days_as_date(tstep, origin)
      paste("Selected:", tstep, "days",
            if (inherits(when, "Date")) paste0("(", format(when, "%Y-%m-%d"), ")") else "")
    })

    time_prey_data <- reactive({
      dat <- project$data
      req(dat$diet_time, input$time_step)
      tstep <- nearest_time(as.numeric(input$time_step), sort(unique(dat$diet_time$Time)))
      dt <- dat$diet_time[Time == tstep]
      if ("Stock" %in% names(dt) && !is.null(input$stock)) {
        dt <- dt[Stock == as.numeric(input$stock)]
      }
      drop <- intersect(c("Time", "Stock", "Cohort"), names(dt))
      as.data.frame(dt[, !drop, with = FALSE])
    })

    prey_data <- reactive({
      dat <- project$data
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
      dat <- project$data
      grp <- dat$grp
      if (!"Code" %in% names(grp)) {
        grp$Code <- grp$code
        grp$GroupType <- grp$grouptype
      }
      trophic.lvl(prey_data(), grp)
    })

    output$net_host <- renderUI({
      ns <- session$ns
      if (requireNamespace("visNetwork", quietly = TRUE)) {
        visNetwork::visNetworkOutput(ns("network"), height = "640px")
      } else {
        plotOutput(ns("network_gg"), height = "640px")
      }
    })

    output$network <- if (requireNamespace("visNetwork", quietly = TRUE)) {
      visNetwork::renderVisNetwork({
        TL <- trophic_data()
        edges_src <- prey_data()
        req(TL, edges_src)
        pal <- project$data$fg_palette
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
      plot.tlvl(trophic_data(), prey_data(), input$focal_fg, NULL,
                get_foodweb_colors(), project$data$fg_palette)
    })

    output$trophic_table <- DT::renderDataTable({
      req(trophic_data())
      table <- with(trophic_data(), data.frame(
        `Functional Group` = FG,
        `Trophic Level` = round(Tlevel, 2)
      ))
      DT::datatable(table, options = list(pageLength = 15, dom = "tip"), rownames = FALSE)
    })
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

trophic.lvl <- function(rel.prey, grp.dat){
  npred <- unique(rel.prey$Pred)
  TL <- NULL
  for(pred in npred){
    pospred <- which(rel.prey$Pred %in% pred)
    TL <- c(TL, Tlevel(rel.prey$value[pospred], rel.prey$TLprey[pospred]))
  }
  left <- rel.prey$Prey[-which(rel.prey$Prey %in% npred)]
  pp.prey <- unique(left)
  pp.prey <- data.frame(FG = pp.prey, Tlevel = prey.pos(pp.prey, grp.dat))
  TL <- data.frame(FG = npred, Tlevel = TL)
  TL <- rbind(TL, pp.prey)
  
  prey.f <- as.data.frame(rel.prey)
  for(fg in 1:nrow(TL)){
    pntl <- which(prey.f$Prey %in% TL$FG[fg])
    if(length(pntl) == 0) next
    prey.f$TLprey[pntl] <- TL$Tlevel[fg]
  }
  npred <- unique(prey.f$Pred)
  TL <- NULL
  for(pred in npred){
    pospred <- which(prey.f$Pred %in% pred)
    TL <- c(TL, Tlevel(prey.f$value[pospred], prey.f$TLprey[pospred]))
  }
  left <- prey.f$Prey[-which(prey.f$Prey %in% npred)]
  pp.prey <- unique(left)
  pp.prey <- data.frame(FG = pp.prey, Tlevel = prey.pos(pp.prey, grp.dat))
  TL <- data.frame(FG = npred, Tlevel = TL)
  TL <- rbind(TL, pp.prey)
  
  his <- hist(TL$Tlevel, breaks = c(0.5:6.5), plot = FALSE)
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
