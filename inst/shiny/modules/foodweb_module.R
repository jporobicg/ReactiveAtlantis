## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Food Web Analysis Module        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

foodweb_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    titlePanel("Atlantis Food Web and Trophic Levels"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput(ns("diet_file"), "Diet File (DietCheck.txt)", accept = ".txt"),
        fileInput(ns("grp_file"), "Groups CSV", accept = ".csv"),
        fileInput(ns("diet_bypol"), "Detailed Diet [Optional]", accept = ".txt"),
        actionButton(ns("load_data"), "Load Data", class = "btn-primary btn-block"),
        hr(),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          h5("Food Web Options"),
          selectInput(ns("focal_fg"), "Focal Functional Group:", choices = NULL),
          numericInput(ns("max_connections"), "Max Trophic Connections:", 
                      value = 4, min = 1, max = 10, step = 1),
          numericInput(ns("min_proportion"), "Min Proportion:", 
                      value = 0.01, min = 0.001, max = 1, step = 0.001),
          numericInput(ns("time_step"), "Time Step:", value = 0),
          selectInput(ns("stock"), "Stock:", choices = NULL),
          conditionalPanel(
            condition = "output.has_bypol",
            ns = ns,
            hr(),
            h6("By Polygon Analysis"),
            checkboxInput(ns("use_polygon"), "Analyze by Polygon", FALSE),
            conditionalPanel(
              condition = "input.use_polygon",
              ns = ns,
              selectInput(ns("polygon"), "Polygon:", choices = NULL)
            )
          )
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
            " Please load your diet files to visualize the food web."
          )
        ),
        conditionalPanel(
          condition = "output.data_loaded",
          ns = ns,
          plotOutput(ns("foodweb_plot"), height = "800px"),
          hr(),
          h4("Trophic Levels"),
          DT::dataTableOutput(ns("trophic_table"))
        )
      )
    )
  )
}

foodweb_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(
      data_loaded = FALSE,
      dat = NULL,
      dat.bp = NULL,
      grp.dat = NULL,
      code.fg = NULL,
      stk = NULL,
      pol = NULL,
      time.stp = NULL,
      lag = NULL
    )
    
    output$data_loaded <- reactive({ rv$data_loaded })
    outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
    
    output$has_bypol <- reactive({ !is.null(rv$dat.bp) })
    outputOptions(output, "has_bypol", suspendWhenHidden = FALSE)
    
    observeEvent(input$load_data, {
      req(input$diet_file, input$grp_file)
      
      tryCatch({
        showNotification("Loading food web data...", type = "message", id = "load_fw", duration = NULL)
        
        dat <- data.frame(data.table::fread(input$diet_file$datapath, header = TRUE, sep = ' ', showProgress = FALSE))
        
        dat.bp <- NULL
        pol <- NULL
        if(!is.null(input$diet_bypol)){
          dat.bp <- data.frame(data.table::fread(input$diet_bypol$datapath, header = TRUE, sep = ' ', showProgress = FALSE))
          pol <- unique(dat.bp$Box)
        }
        
        if(any(names(dat) == 'Group')) names(dat)[which(names(dat) == 'Group')] <- 'Predator'
        
        time.stp <- round(range(dat$Time), 0)
        lag <- diff(unique(dat$Time))[1]
        grp.dat <- utils::read.csv(input$grp_file$datapath)
        stk <- unique(dat$Stock)
        
        if(any(names(grp.dat) == 'InvertType')){
          names(grp.dat)[which(names(grp.dat) == 'InvertType')] <- 'GroupType'
        }
        if(any(names(grp.dat) == 'isPredator')){
          names(grp.dat)[which(names(grp.dat) == 'isPredator')] <- 'IsPredator'
        }
        
        code.fg <- grp.dat$Code[grp.dat$IsPredator > 0]
        
        rv$dat <- dat
        rv$dat.bp <- dat.bp
        rv$grp.dat <- grp.dat
        rv$code.fg <- code.fg
        rv$stk <- stk
        rv$pol <- pol
        rv$time.stp <- time.stp
        rv$lag <- lag
        rv$data_loaded <- TRUE
        
        updateSelectInput(session, "focal_fg", choices = c('All', as.character(code.fg)))
        updateSelectInput(session, "stock", choices = stk)
        updateNumericInput(session, "time_step", value = time.stp[1], min = time.stp[1], max = time.stp[2])
        
        if(!is.null(pol)){
          updateSelectInput(session, "polygon", choices = pol)
        }
        
        removeNotification("load_fw")
        showNotification("Data loaded successfully!", type = "message", duration = 3)
      }, error = function(e) {
        removeNotification("load_fw")
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
    
    time_prey_data <- reactive({
      req(rv$dat, input$time_step, input$stock)
      
      if(input$use_polygon && !is.null(rv$dat.bp)){
        req(input$polygon)
        rv$dat.bp[rv$dat.bp$Time == input$time_step & rv$dat.bp$Box == input$polygon, c(2, 6:ncol(rv$dat.bp))]
      } else {
        rv$dat[rv$dat$Time == input$time_step & rv$dat$Stock == input$stock, c(2, 6:ncol(rv$dat))]
      }
    })
    
    prey_data <- reactive({
      req(time_prey_data(), input$focal_fg, input$max_connections, input$min_proportion, rv$code.fg, rv$grp.dat)
      tot.prey(input$focal_fg, input$max_connections, rv$code.fg, time_prey_data(), input$min_proportion, rv$grp.dat)
    })
    
    trophic_data <- reactive({
      req(prey_data())
      validate(need(!is.null(prey_data()), 'No interaction between predators and prey.'))
      trophic.lvl(prey_data(), rv$grp.dat)
    })
    
    output$foodweb_plot <- renderPlot({
      req(trophic_data(), prey_data())
      
      color.p <- get_foodweb_colors()
      poly_label <- if(input$use_polygon && !is.null(input$polygon)) input$polygon else NULL
      
      plot.tlvl(trophic_data(), prey_data(), input$focal_fg, pol = poly_label, color.p)
    })
    
    output$trophic_table <- DT::renderDataTable({
      req(trophic_data())
      
      table <- with(trophic_data(), data.frame(
        `Functional Group` = FG,
        `Trophic Level` = round(Tlevel, 2)
      ))
      
      DT::datatable(table, options = list(pageLength = 15, dom = 'tip'), rownames = FALSE)
    })
  })
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~            Helper Functions                ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

prey.pos <- function(FGs, grp.dat){
  ctg <- vector('numeric')
  for(i in 1:length(FGs)){
    ctg[i] <- which(grp.dat$Code %in% FGs[i], arr.ind = TRUE)
  }
  typ <- grp.dat$GroupType[ctg]
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
  pp.prey <- unique(rel.prey$Prey[-which(rel.prey$Prey %in% npred)])
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
  pp.prey <- unique(prey.f$Prey[-which(prey.f$Prey %in% npred)])
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

plot.tlvl <- function(T.lvl, rel.prey, foc.fg, pol = NULL, color.p){
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
    plotrix::draw.circle(T.lvl$vpos[i], T.lvl$Tlevel[i], radius = rad, nv = 1500, border = NULL,
                        col = ifelse(i == 1 && foc.fg != 'All', 'steelblue', 'gray91'), lty = 1, lwd = 1)
    text(T.lvl$vpos[i], T.lvl$Tlevel[i], T.lvl$FG[i], cex = .8, font = 2, 
         col = ifelse(i == 1 && foc.fg != 'All', 'white', 1))
  }
  legend('topright', c('DownTop', 'TopDown'), lty = 1, col = c(color.p[1], color.p[2]), lwd = 1.5, bty = 'n')
}
