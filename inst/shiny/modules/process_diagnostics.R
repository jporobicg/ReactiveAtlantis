## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Independent process diagnostics             ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Recruits vs SSB, predation mortality by predator, realised diet
## next to pPREY, and biomass-based spatial overlap. Logic is written
## here from Atlantis file layouts (Mort.txt, SpecificMort.txt,
## SpecificPredMort.txt, YOY.txt, SSB.txt, DietCheck.txt, box biomass).
## atlantistools (GPL-3) describes similar products; this file is not
## a copy of that package.

mortality_output_roles <- function() {
  c("specific_mort", "specific_pred_mort", "mort")
}

mortality_file_status <- function(paths, data = NULL) {
  labs <- c(
    specific_mort = "SpecificMort.txt",
    specific_pred_mort = "SpecificPredMort.txt",
    mort = "Mort.txt"
  )
  roles <- names(labs)
  present <- vapply(roles, function(role) {
    p <- tryCatch(paths[[role]], error = function(e) NA_character_)
    loaded <- FALSE
    if (!is.null(data)) {
      loaded <- switch(
        role,
        specific_mort = !is.null(data$spe_mort) && NROW(data$spe_mort) > 0,
        specific_pred_mort = !is.null(data$pred_mort) && NROW(data$pred_mort) > 0,
        mort = !is.null(data$mort) && NROW(data$mort) > 0,
        FALSE
      )
    }
    path_ok(p) || isTRUE(loaded)
  }, logical(1))
  notes <- character(length(roles))
  names(notes) <- roles
  notes["specific_mort"] <- if (present[["specific_mort"]]) {
    "Fishing (F), natural (M1) and total predation (M2) by age."
  } else {
    "Missing. Needed for the fishing / natural / predation stack."
  }
  notes["specific_pred_mort"] <- if (present[["specific_pred_mort"]]) {
    pred <- tryCatch(data$pred_mort, error = function(e) NULL)
    if (pred_mort_is_specific_mort_style(pred)) {
      "Present, but columns are SpecificMort-style (M1/M2/F), not predator-by-prey."
    } else {
      "Predation mortality on a prey, split by predator."
    }
  } else {
    "Missing. Needed for predation mortality by predator."
  }
  notes["mort"] <- if (present[["mort"]]) {
    "Relative fishing (F) versus other mortality (M)."
  } else {
    "Missing. Optional relative F versus M (Atlantis documents this file as relative only)."
  }
  data.frame(
    role = roles,
    label = unname(labs),
    present = unname(present),
    note = unname(notes),
    stringsAsFactors = FALSE
  )
}

mortality_missing_message <- function(status, view = "source") {
  view <- as.character(view)[1]
  need <- switch(
    view,
    predator = "specific_pred_mort",
    mort = "mort",
    "specific_mort"
  )
  row <- status[status$role == need, , drop = FALSE]
  if (nrow(row) == 0) return("Mortality file is missing.")
  if (isTRUE(row$present[1]) && !grepl("SpecificMort-style", row$note[1], fixed = TRUE)) {
    return(NULL)
  }
  missing <- status[!status$present, "label"]
  extra <- if (length(missing)) {
    paste0(" Files not loaded: ", paste(missing, collapse = ", "), ".")
  } else {
    ""
  }
  paste0(row$label[1], ": ", row$note[1], extra)
}

pred_mort_is_trunk <- function(df) {
  if (is.null(df) || ncol(df) < 2) return(FALSE)
  nms <- tolower(names(df))
  any(nms %in% c("group", "predator", "pred")) &&
    any(nms %in% c("cohort", "agecl", "age"))
}

pred_mort_is_specific_mort_style <- function(df) {
  if (is.null(df) || ncol(df) < 2) return(FALSE)
  nms <- names(df)
  nms <- nms[tolower(nms) != "time"]
  any(grepl("[._-](M1|M2|F)$", nms, ignore.case = TRUE))
}

pred_mort_rename_ids <- function(df) {
  nms <- names(df)
  low <- tolower(nms)
  if (any(low == "group")) names(df)[low == "group"] <- "Predator"
  if (any(low == "predator")) names(df)[low == "predator"] <- "Predator"
  if (any(low == "pred")) names(df)[low == "pred"] <- "Predator"
  if (any(low == "cohort")) names(df)[low == "cohort"] <- "Cohort"
  if (any(low == "agecl")) names(df)[low == "agecl"] <- "Cohort"
  if (any(low == "age") && !"Cohort" %in% names(df)) {
    names(df)[low == "age"] <- "Cohort"
  }
  if (any(low == "time")) names(df)[low == "time"] <- "Time"
  if (any(low == "prey") && !"Prey" %in% names(df)) {
    names(df)[low == "prey"] <- "Prey"
  }
  if (any(low %in% c("atoutput", "value", "mort")) && !"Value" %in% names(df)) {
    names(df)[match(low[low %in% c("atoutput", "value", "mort")][1], low)] <- "Value"
  }
  df
}

##' Predation mortality on a prey group, by predator and age.
##'
##' Trunk SpecificPredMort.txt is Time, Group, Cohort, Stock plus one
##' column per prey. Wide SpecificMort-style files cannot be split by
##' predator; those return an empty table with a reason attribute.
parse_pred_mort_by_predator <- function(pred_mort, prey_code, pred_code = NULL) {
  empty <- data.frame(
    Time = numeric(), Predator = character(), Age = character(),
    Prey = character(), Value = numeric(), stringsAsFactors = FALSE
  )
  if (is.null(pred_mort) || ncol(pred_mort) < 2) {
    attr(empty, "reason") <- "SpecificPredMort.txt is missing or empty."
    return(empty)
  }
  prey_code <- as.character(prey_code)[1]
  df <- pred_mort_rename_ids(pred_mort)
  if (pred_mort_is_trunk(df)) {
    id <- intersect(c("Time", "Predator", "Cohort", "Stock", "Prey"), names(df))
    if ("Value" %in% names(df) && "Prey" %in% names(df)) {
      long <- df[, c(intersect(c("Time", "Predator", "Cohort", "Prey", "Value"),
                               names(df))), drop = FALSE]
    } else {
      prey_cols <- setdiff(names(df), c("Time", "Predator", "Cohort", "Stock"))
      if (length(prey_cols) == 0) {
        attr(empty, "reason") <- "SpecificPredMort.txt has no prey columns."
        return(empty)
      }
      long <- reshape2::melt(
        df[, c(intersect(c("Time", "Predator", "Cohort"), names(df)), prey_cols),
           drop = FALSE],
        id.vars = intersect(c("Time", "Predator", "Cohort"), names(df)),
        variable.name = "Prey", value.name = "Value",
        factorsAsStrings = TRUE
      )
    }
    long$Prey <- as.character(long$Prey)
    long$Value <- as.numeric(long$Value)
    long <- long[long$Prey == prey_code & is.finite(long$Value), , drop = FALSE]
    if (!is.null(pred_code) && nzchar(as.character(pred_code)[1])) {
      long <- long[long$Predator == as.character(pred_code)[1], , drop = FALSE]
    }
    if (nrow(long) == 0) {
      attr(empty, "reason") <- paste(
        "No predation mortality on", prey_code, "in SpecificPredMort.txt."
      )
      return(empty)
    }
    ages <- if ("Cohort" %in% names(long)) as.integer(long$Cohort) else 0L
    long$Age <- format_age_labels_from_index(ages)
    out <- data.frame(
      Time = as.numeric(long$Time),
      Predator = as.character(long$Predator),
      Age = as.character(long$Age),
      Prey = as.character(long$Prey),
      Value = as.numeric(long$Value),
      stringsAsFactors = FALSE
    )
    return(out)
  }
  if (pred_mort_is_specific_mort_style(df)) {
    attr(empty, "reason") <- paste(
      "SpecificPredMort.txt has SpecificMort-style columns (CODE.age.S0.M1/M2/F).",
      "Atlantis trunk writes Time, Group, Cohort, Stock and one column per prey."
    )
    return(empty)
  }
  attr(empty, "reason") <- paste(
    "Could not read predator-by-prey columns from SpecificPredMort.txt."
  )
  empty
}

format_age_labels_from_index <- function(idx) {
  idx <- as.integer(idx)
  idx[!is.finite(idx)] <- 0L
  n <- max(1L, max(idx, na.rm = TRUE) + 1L)
  labs <- format_age_labels(n)
  labs[pmin(pmax(idx + 1L, 1L), n)]
}

##' Mort.txt relative fishing (F) versus other mortality (M).
parse_mort_txt <- function(mort, fg) {
  empty <- data.frame(
    Time = numeric(), Source = character(), Value = numeric(),
    stringsAsFactors = FALSE
  )
  if (is.null(mort) || ncol(mort) < 2) {
    attr(empty, "reason") <- "Mort.txt is missing or empty."
    return(empty)
  }
  fg <- as.character(fg)[1]
  nms <- names(mort)
  tcol <- if ("Time" %in% nms) "Time" else nms[1]
  want <- nms[grepl(paste0("^", fg, "[._-](M|F)$"), nms, ignore.case = TRUE)]
  if (length(want) == 0) {
    attr(empty, "reason") <- paste("No Mort.txt columns for", fg, "(expected CODE.M / CODE.F).")
    return(empty)
  }
  long <- reshape2::melt(
    mort[, c(tcol, want), drop = FALSE],
    id.vars = tcol, variable.name = "code", value.name = "Value",
    factorsAsStrings = TRUE
  )
  src <- toupper(sub(paste0("^", fg, "[._-]"), "", as.character(long$code),
                     ignore.case = TRUE))
  long$Source <- ifelse(src == "F", "Fishing", "Natural")
  names(long)[names(long) == tcol] <- "Time"
  data.frame(
    Time = as.numeric(long$Time),
    Source = as.character(long$Source),
    Value = as.numeric(long$Value),
    stringsAsFactors = FALSE
  )
}

find_ssb_column <- function(ssb, code) {
  if (is.null(ssb) || ncol(ssb) == 0) return(NA_character_)
  nms <- trimws(names(ssb))
  nms <- nms[tolower(nms) != "time"]
  code <- trimws(as.character(code)[1])
  if (code %in% nms) return(code)
  ci <- nms[tolower(nms) == tolower(code)]
  if (length(ci) > 0) return(ci[1])
  NA_character_
}

##' Join YOY recruits with SSB for one age-structured group.
join_recruits_ssb <- function(yoy, ssb, code) {
  empty <- data.frame(
    Time = numeric(), Recruits = numeric(), SSB = numeric(),
    yoy_column = character(), ssb_column = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(yoy) || is.null(ssb)) {
    miss <- c(if (is.null(yoy)) "YOY.txt", if (is.null(ssb)) "SSB.txt")
    attr(empty, "reason") <- paste("Load", paste(miss, collapse = " and "),
                                   "to plot recruits versus spawning stock.")
    return(empty)
  }
  ycol <- find_yoy_column(yoy, code)
  scol <- find_ssb_column(ssb, code)
  if (is.na(ycol) || is.na(scol)) {
    attr(empty, "reason") <- paste(
      "No", if (is.na(ycol)) "YOY" else "SSB",
      "column for", code,
      if (is.na(ycol)) "(expected CODE.0 in YOY.txt)." else "(expected CODE in SSB.txt)."
    )
    return(empty)
  }
  yt <- if ("Time" %in% names(yoy)) as.numeric(yoy$Time) else seq_len(nrow(yoy)) - 1
  st <- if ("Time" %in% names(ssb)) as.numeric(ssb$Time) else seq_len(nrow(ssb)) - 1
  rec <- as.numeric(yoy[[ycol]])
  spawn <- as.numeric(ssb[[scol]])
  times <- sort(unique(c(yt[is.finite(yt)], st[is.finite(st)])))
  if (length(times) == 0) {
    attr(empty, "reason") <- paste("No overlapping times for", code, "in YOY and SSB.")
    return(empty)
  }
  rec_m <- rec[match(times, yt)]
  ssb_m <- spawn[match(times, st)]
  data.frame(
    Time = times,
    Recruits = rec_m,
    SSB = ssb_m,
    yoy_column = ycol,
    ssb_column = scol,
    stringsAsFactors = FALSE
  )
}

##' DietCheck proportion of one prey in one predator through time.
dietcheck_pair_series <- function(diet_time, pred, prey) {
  empty <- data.frame(Time = numeric(), Proportion = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(diet_time) || nrow(diet_time) == 0) {
    attr(empty, "reason") <- "Load DietCheck.txt to plot realised diet proportions."
    return(empty)
  }
  pred <- as.character(pred)[1]
  prey <- as.character(prey)[1]
  dt <- as.data.frame(diet_time, stringsAsFactors = FALSE)
  if (!"Predator" %in% names(dt) || !prey %in% names(dt)) {
    attr(empty, "reason") <- paste("DietCheck.txt has no", prey, "column for", pred, ".")
    return(empty)
  }
  dt <- dt[as.character(dt$Predator) == pred, , drop = FALSE]
  if (nrow(dt) == 0) {
    attr(empty, "reason") <- paste("DietCheck.txt has no rows for predator", pred, ".")
    return(empty)
  }
  dt$Time <- as.numeric(dt$Time)
  dt$Proportion <- as.numeric(dt[[prey]])
  agg <- stats::aggregate(Proportion ~ Time, data = dt, mean, na.rm = TRUE)
  agg
}

##' Consumed tonnes of one prey by one predator (Eat/Grazing x DietCheck).
consumed_pair_series <- function(tonnes, diet_time, pred, prey) {
  empty <- data.frame(Time = numeric(), Tonnes = numeric(),
                      stringsAsFactors = FALSE)
  if (is.null(tonnes) || !length(tonnes)) {
    attr(empty, "reason") <- "Load outputPROD.nc to plot consumed biomass."
    return(empty)
  }
  long <- consumption_by_prey(as.numeric(tonnes), diet_time, pred)
  if (nrow(long) == 0) {
    attr(empty, "reason") <- paste(
      "No DietCheck proportions for", pred, "to split consumed biomass."
    )
    return(empty)
  }
  prey <- as.character(prey)[1]
  hit <- long[as.character(long$Prey) == prey, , drop = FALSE]
  if (nrow(hit) == 0) {
    attr(empty, "reason") <- paste(pred, "has no DietCheck share for", prey, ".")
    return(empty)
  }
  stats::aggregate(Tonnes ~ Time, data = hit, sum, na.rm = TRUE)
}

##' Biomass-based predator-prey overlap per box and time.
##'
##' Per-box index is the local presence overlap
##' 2 * min(Bpred, Bprey) / (Bpred + Bprey) (0 if either is zero).
##' The time-series index is Schoener's D = sum min(p, q) where p and q
##' are each group's biomass proportions across boxes.
spatial_overlap_index <- function(pred_box, prey_box) {
  empty_box <- data.frame(Time = integer(), Box = integer(), Value = numeric(),
                          Variable = character(), stringsAsFactors = FALSE)
  empty_ts <- data.frame(Time = integer(), Schoener = numeric(),
                         stringsAsFactors = FALSE)
  empty <- list(by_box = empty_box, by_time = empty_ts)
  if (is.null(pred_box) || is.null(prey_box) ||
      nrow(pred_box) == 0 || nrow(prey_box) == 0) {
    attr(empty$by_box, "reason") <- "Need biomass by box for both predator and prey."
    return(empty)
  }
  pred_val <- overlap_box_value(pred_box)
  prey_val <- overlap_box_value(prey_box)
  keys <- unique(rbind(
    pred_val[, c("Time", "Box"), drop = FALSE],
    prey_val[, c("Time", "Box"), drop = FALSE]
  ))
  keys$Pred <- pred_val$Value[match(paste(keys$Time, keys$Box),
                                    paste(pred_val$Time, pred_val$Box))]
  keys$Prey <- prey_val$Value[match(paste(keys$Time, keys$Box),
                                    paste(prey_val$Time, prey_val$Box))]
  keys$Pred[!is.finite(keys$Pred)] <- 0
  keys$Prey[!is.finite(keys$Prey)] <- 0
  den <- keys$Pred + keys$Prey
  keys$Local <- ifelse(den > 0, 2 * pmin(keys$Pred, keys$Prey) / den, 0)
  pred_tot <- stats::aggregate(Pred ~ Time, data = keys, sum, na.rm = TRUE)
  prey_tot <- stats::aggregate(Prey ~ Time, data = keys, sum, na.rm = TRUE)
  keys$P <- keys$Pred / pred_tot$Pred[match(keys$Time, pred_tot$Time)]
  keys$Q <- keys$Prey / prey_tot$Prey[match(keys$Time, prey_tot$Time)]
  keys$P[!is.finite(keys$P)] <- 0
  keys$Q[!is.finite(keys$Q)] <- 0
  keys$SchoenerPart <- pmin(keys$P, keys$Q)
  by_box <- data.frame(
    Time = as.integer(keys$Time),
    Box = as.integer(keys$Box),
    Value = as.numeric(keys$Local),
    Variable = "Overlap",
    stringsAsFactors = FALSE
  )
  by_time <- stats::aggregate(SchoenerPart ~ Time, data = keys, sum, na.rm = TRUE)
  names(by_time)[2] <- "Schoener"
  by_time$Time <- as.integer(by_time$Time)
  list(by_box = by_box, by_time = by_time)
}

overlap_box_value <- function(df) {
  val_col <- if ("Value" %in% names(df)) "Value" else if ("Biomass" %in% names(df)) {
    "Biomass"
  } else {
    setdiff(names(df), c("Time", "Box", "Variable", "Code"))[1]
  }
  data.frame(
    Time = as.integer(df$Time),
    Box = as.integer(df$Box),
    Value = as.numeric(df[[val_col]]),
    stringsAsFactors = FALSE
  )
}
