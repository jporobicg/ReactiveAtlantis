## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Expected predation pressure (t0 diagnostic) ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## The calculation itself is atlantis_feeding_rates() at the end of this file
## (per predator cohort and water cell; validation D05). The older
## stage-level code below is kept for pressure_pair_by_box fallbacks only.
##
## Independent implementation of the Atlantis consumer-feeding
## calculation at initial conditions. Not copied from atlantistools.
##
## Equations follow Atlantis User Guide Part I, chapter 10.2
## (Consumer feeding) and 10.2.2 (C and mum), Table 18 (predcase),
## and 10.2.5 (size refuge). The same forms are named in
## atecology/atbiology.c (`box_bio_process`) in the User Guide.
## Murray and Parslow (1997) supply the modified Holling Type II
## that predcase = 0 uses: Gmax = mum / E.
##
## Available prey in a cell:
##   B*_j = pPREY_ij * B_j * delta_size * delta_overlap * delta_habitat
##
## Modified Holling Type II (predcase 0), per predator unit:
##   I = C * A / (1 + (C / mum) * sum_j (E_j * B*_j))
##   A = sum_j B*_j
##   Gr_j = I * pred_scale * (B*_j / A)
##
## Type I (predcase 1): I = min(C * A, mum / E_eff)
## Type III (predcase 2): same as Type II with B* squared.
##
## delta_size is the knife-edge gape window:
##   1 if KLP * SN_pred < SN_prey < KUP * SN_pred, else 0.
## Habitat refuge is 1 in this diagnostic (cover is not read).
## Spatial overlap is implicit when the sum is over boxes: prey and
## predator must share a box. KDENR is a recruitment parameter in
## the current User Guide, not a feeding term, so it is not applied.
##
## flagfishrates = 0: C is m3 ind-1 d-1, mum is mg N ind-1 d-1,
## pred_scale = numbers. flagfishrates = 1 (and biomass pools):
## C is m3 (mg N)-1 d-1, mum is d-1, pred_scale = predator mg N.

MG2T_DEFAULT <- 0.00000002
XCN_DEFAULT <- 5.7

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~                 prm tokens                  ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

prm_strip_comment <- function(line) {
  ln <- as.character(line)[1]
  if (!nzchar(ln)) return("")
  ln <- sub("#.*$", "", ln)
  trimws(ln)
}

##' Attach a first-token index to a biology.prm (one pass).
prm_with_index <- function(prm) {
  cached <- attr(prm, "ra_prm_index")
  if (is.list(cached) && length(cached)) return(prm)
  idx <- index_prm_tokens_build(prm)
  attr(prm, "ra_prm_index") <- idx
  prm
}

index_prm_tokens_build <- function(prm) {
  idx <- list()
  if (is.null(prm) || !length(prm)) return(idx)
  for (i in seq_along(prm)) {
    raw <- as.character(prm[i])
    if (grepl("^[[:space:]]*#", raw)) next
    ln <- prm_strip_comment(raw)
    if (!nzchar(ln)) next
    first <- unlist(strsplit(ln, "[[:space:]]+", perl = TRUE), use.names = FALSE)[1]
    if (!nzchar(first) || !is.null(idx[[first]])) next
    idx[[first]] <- i
  }
  idx
}

##' First-token index for a biology.prm (one pass).
index_prm_tokens <- function(prm) {
  cached <- attr(prm, "ra_prm_index")
  if (is.list(cached) && length(cached)) return(cached)
  index_prm_tokens_build(prm)
}

##' First non-comment line whose first token is exactly `token`.
prm_match_line <- function(prm, token) {
  tok <- as.character(token)[1]
  if (!nzchar(tok)) return(NA_integer_)
  idx <- index_prm_tokens(prm)
  hit <- idx[[tok]]
  if (is.null(hit)) return(NA_integer_)
  as.integer(hit)
}

prm_all_numbers <- function(line) {
  ln <- prm_strip_comment(line)
  if (!nzchar(ln)) return(numeric())
  bits <- unlist(strsplit(ln, "[[:space:]]+"), use.names = FALSE)
  nums <- suppressWarnings(as.numeric(bits))
  nums[is.finite(nums)]
}

prm_line_numbers <- function(line) {
  ln <- prm_strip_comment(line)
  if (!nzchar(ln)) return(numeric())
  bits <- unlist(strsplit(ln, "[[:space:]]+"), use.names = FALSE)
  if (!length(bits)) return(numeric())
  ## Header lines start with the token name; value-only lines do not.
  start <- if (grepl("^[A-Za-z_]", bits[1])) 2L else 1L
  if (start > length(bits)) return(numeric())
  nums <- suppressWarnings(as.numeric(bits[start:length(bits)]))
  nums[is.finite(nums)]
}

##' Read a biology.prm token. Vector parameters often have a count
##' on the header line and the values on the next line (C_XXX, mum_XXX).
read_prm_token <- function(prm, token, default = numeric()) {
  i <- prm_match_line(prm, token)
  if (!is.finite(i)) return(default)
  nums <- prm_line_numbers(prm[i])
  nums <- nums[is.finite(nums)]
  if (length(nums) == 1L && nums[1] >= 2 && nums[1] == as.integer(nums[1]) &&
      i < length(prm)) {
    nxt <- prm_line_numbers(prm[i + 1])
    nxt <- nxt[is.finite(nxt)]
    if (length(nxt) >= as.integer(nums[1])) {
      return(nxt[seq_len(as.integer(nums[1]))])
    }
    if (length(nxt) > 1L) return(nxt)
  }
  if (length(nums)) return(nums)
  if (i < length(prm)) {
    nxt <- prm_line_numbers(prm[i + 1])
    nxt <- nxt[is.finite(nxt)]
    if (length(nxt)) return(nxt)
  }
  default
}

read_prm_scalar <- function(prm, token, default = NA_real_) {
  v <- read_prm_token(prm, token, default = numeric())
  if (!length(v)) return(default)
  as.numeric(v[1])
}

prey_e_class <- function(code, grouptype = "") {
  code <- as.character(code)[1]
  gt <- toupper(as.character(grouptype)[1])
  if (code %in% c("DR", "DRsed") || grepl("REF_DET|REFRACT", gt)) {
    return("refractory")
  }
  ## Carrion is eaten as live prey with E (atprocess.c:519-522).
  if (code %in% c("DC", "DCsed") || grepl("CARRION", gt)) return("live")
  if (code %in% c("DL", "DLsed") || grepl("LAB_DET|DETRIT", gt)) {
    return("detritus")
  }
  ## Plant types use EPlant, including turf and microphytobenthos
  ## (atprocess.c:497-503; Atlantis spells MICROPHTYBENTHOS).
  if (grepl("PHY|PLANT|SEAGRASS|MACROAL|DINO|MICROPH|TURF|LG_PHY|SM_PHY", gt)) {
    return("plant")
  }
  "live"
}

gape_size_scalar <- function(sn_pred, sn_prey, klp, kup) {
  if (!is.finite(sn_pred) || !is.finite(sn_prey) || sn_pred <= 0 ||
      sn_prey <= 0) {
    return(1)
  }
  lo <- if (is.finite(klp)) klp * sn_pred else 0
  hi <- if (is.finite(kup)) kup * sn_pred else Inf
  ## Atlantis hard window is inclusive (atvertprocesses.c:557-560).
  if (sn_prey >= lo && sn_prey <= hi) 1 else 0
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~          Holling graze (one predator)       ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

##' Ingestion of each prey (mg N d-1) for one predator unit set.
##'
##' @param C clearance
##' @param mum maximum growth / consumption scalar
##' @param avail available prey (concentration, or squared for type III)
##' @param e_times_avail E_j * B*_j (un-squared)
##' @param pred_scale numbers (absolute) or predator mg N (mass-specific)
##' @param predcase 0 = Type II, 1 = Type I, 2 = Type III
holling_ingest_mgn_day <- function(C, mum, avail, e_times_avail, pred_scale,
                                   predcase = 0, e_live = NA_real_) {
  n <- length(avail)
  z <- rep(0, n)
  C <- as.numeric(C)[1]
  mum <- as.numeric(mum)[1]
  pred_scale <- as.numeric(pred_scale)[1]
  if (!is.finite(C) || C <= 0 || !is.finite(pred_scale) || pred_scale <= 0) {
    return(z)
  }
  avail <- as.numeric(avail)
  e_times_avail <- as.numeric(e_times_avail)
  if (length(e_times_avail) != n) e_times_avail <- rep(e_times_avail[1], n)
  predcase <- as.integer(predcase)[1]
  if (!is.finite(predcase)) predcase <- 0L

  if (identical(predcase, 2L)) {
    e_j <- ifelse(avail > 0, e_times_avail / avail, 0)
    avail_use <- avail * avail
    e_use <- e_j * avail_use
  } else {
    avail_use <- avail
    e_use <- e_times_avail
  }
  avail_use[!is.finite(avail_use)] <- 0
  e_use[!is.finite(e_use)] <- 0
  A <- sum(avail_use)
  if (!is.finite(A) || A <= 0) return(z)

  if (identical(predcase, 1L)) {
    ## Atlantis caps type I intake at mum / E of live prey (atprocess.c:840-845).
    e_eff <- as.numeric(e_live)[1]
    if (!is.finite(e_eff) || e_eff <= 0) e_eff <- sum(e_use) / A
    if (!is.finite(e_eff) || e_eff <= 0) e_eff <- 1
    gmax <- if (is.finite(mum) && mum > 0) mum / e_eff else Inf
    ingest_unit <- min(C * A, gmax)
  } else {
    if (!is.finite(mum) || mum <= 0) {
      ingest_unit <- C * A
    } else {
      ingest_unit <- C * A / (1 + C * sum(e_use) / mum)
    }
  }
  if (!is.finite(ingest_unit) || ingest_unit <= 0) return(z)
  (ingest_unit * pred_scale) * (avail_use / A)
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~         Predator / prey stocks at t0        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

empty_stage_stock <- function(n_box) {
  list(
    mass_mgn = rep(0, n_box),
    nums = rep(0, n_box),
    tonnes = rep(0, n_box),
    sn = NA_real_
  )
}

box_slice_t0 <- function(arr, time_i = 1L, n_t = NULL) {
  if (is.null(arr)) return(NULL)
  d <- dim(arr)
  if (is.null(d)) return(as.numeric(arr))
  t <- as.integer(time_i)[1]
  if (!is.finite(t) || t < 1) t <- 1L
  if (length(d) == 3L) {
    if (t > d[3]) t <- d[3]
    return(arr[, , t, drop = TRUE])
  }
  ## A 2-D [b, t] array (epibenthos in output.nc) is sliced on t; a 2-D
  ## [z, b] array (one time step, t dropped) is returned whole.
  if (length(d) == 2L) {
    if (!is.null(n_t) && n_t > 1L && d[2] == n_t) {
      if (t > d[2]) t <- d[2]
      return(as.numeric(arr[, t]))
    }
    return(arr)
  }
  as.numeric(arr)
}

sum_z_to_box <- function(arr2) {
  if (is.null(arr2)) return(numeric())
  d <- dim(arr2)
  if (is.null(d)) return(as.numeric(arr2))
  if (length(d) == 1L) return(as.numeric(arr2))
  if (length(d) == 2L) {
    return(as.numeric(colSums(arr2, na.rm = TRUE)))
  }
  as.numeric(arr2)
}

weighted_sn <- function(sn_arr, nums_arr) {
  sn <- as.numeric(sn_arr)
  nn <- as.numeric(nums_arr)
  ok <- is.finite(sn) & is.finite(nn) & nn > 0
  if (!any(ok)) return(NA_real_)
  sum(sn[ok] * nn[ok]) / sum(nn[ok])
}

##' Volume per box at t0 (m3).
init_box_volume <- function(nc, time_i = 1L) {
  n_t <- if (!is.null(nc$dim$t)) as.integer(nc$dim$t$len) else 1L
  vol <- tryCatch(nc_volume(nc), error = function(e) NULL)
  sl <- box_slice_t0(vol, time_i, n_t)
  out <- sum_z_to_box(sl)
  if (!length(out)) return(numeric())
  out[!is.finite(out)] <- 0
  out
}

add_stage_stock <- function(acc, mass, nums, sn_w) {
  n <- length(acc$mass_mgn)
  mass <- as.numeric(mass)
  nums <- as.numeric(nums)
  if (length(mass) < n) mass <- c(mass, rep(0, n - length(mass)))
  if (length(nums) < n) nums <- c(nums, rep(0, n - length(nums)))
  acc$mass_mgn <- acc$mass_mgn + mass[seq_len(n)]
  acc$nums <- acc$nums + nums[seq_len(n)]
  if (is.finite(sn_w) && sn_w > 0) {
    if (!is.finite(acc$sn) || acc$sn <= 0) {
      acc$sn <- sn_w
    } else {
      acc$sn <- (acc$sn + sn_w) / 2
    }
  }
  acc
}

finalise_stage_stock <- function(st, mg2t, x.cn) {
  st$mass_mgn[!is.finite(st$mass_mgn)] <- 0
  st$nums[!is.finite(st$nums)] <- 0
  st$tonnes <- st$mass_mgn * mg2t * x.cn
  st
}

##' Per-box juvenile / adult / pool stocks from the initial-conditions NetCDF.
init_stage_stocks <- function(nc, grp, prm, time_i = 1L,
                              mg2t = MG2T_DEFAULT, x.cn = XCN_DEFAULT) {
  empty <- list(n_box = 0L, volume = numeric(), groups = list(),
                notes = character())
  if (is.null(nc) || is.null(grp) || nrow(grp) < 1) return(empty)
  prm <- prm_with_index(prm)
  names(grp) <- tolower(names(grp))
  vars <- tryCatch(names(nc$var), error = function(e) character())
  vol <- init_box_volume(nc, time_i)
  n_box <- if (length(vol)) length(vol) else 1L
  if (!length(vol)) vol <- rep(NA_real_, n_box)

  out_groups <- list()
  notes <- character()
  n_t <- if (!is.null(nc$dim$t)) as.integer(nc$dim$t$len) else 1L
  box_area <- nc_box_area(nc)
  codes <- as.character(grp$code)
  for (i in seq_len(nrow(grp))) {
    code <- codes[i]
    name <- as.character(grp$name[i])
    gtype <- as.character(grp$grouptype[i])
    n_coh <- as.integer(grp$numcohorts[i])
    if (!is.finite(n_coh) || n_coh < 1) n_coh <- 1L
    age_mat <- read_prm_scalar(prm, paste0(code, "_age_mat"), default = NA_real_)
    if (!is.finite(age_mat) || age_mat < 1) age_mat <- max(1, ceiling(n_coh / 2))
    age_mat <- as.integer(age_mat)

    juv <- empty_stage_stock(n_box)
    adl <- empty_stage_stock(n_box)
    pool <- empty_stage_stock(n_box)
    found <- FALSE

    if (n_coh > 1L && !gtype %in% AGE_SPECIAL) {
      for (coh in seq_len(n_coh)) {
        n_var <- paste0(name, coh, "_Nums")
        r_var <- paste0(name, coh, "_ResN")
        s_var <- paste0(name, coh, "_StructN")
        if (!all(c(n_var, r_var, s_var) %in% vars)) next
        nums <- sum_z_to_box(box_slice_t0(nc_get(nc, n_var), time_i, n_t))
        resn <- box_slice_t0(nc_get(nc, r_var), time_i, n_t)
        strn <- box_slice_t0(nc_get(nc, s_var), time_i, n_t)
        mass <- sum_z_to_box((resn + strn) * box_slice_t0(nc_get(nc, n_var), time_i, n_t))
        sn_w <- weighted_sn(strn, box_slice_t0(nc_get(nc, n_var), time_i, n_t))
        if ((coh - 1L) < age_mat) {
          juv <- add_stage_stock(juv, mass, nums, sn_w)
        } else {
          adl <- add_stage_stock(adl, mass, nums, sn_w)
        }
        pool <- add_stage_stock(pool, mass, nums, sn_w)
        found <- TRUE
      }
    } else {
      vnames <- pool_var_names(vars, name, n_coh, gtype)
      if (length(vnames)) {
        mass <- rep(0, n_box)
        for (vname in vnames) {
          biom <- box_slice_t0(nc_get(nc, vname), time_i, n_t)
          vol2 <- box_slice_t0(tryCatch(nc_volume(nc), error = function(e) NULL),
                               time_i, n_t)
          if (!is.null(dim(biom)) && length(dim(biom)) == 2L) {
            ## water-column pool, mg N m-3 x cell volume
            if (!is.null(vol2) && length(dim(vol2)) == 2L && all(dim(biom) == dim(vol2))) {
              m <- sum_z_to_box(biom * vol2)
            } else {
              m <- sum_z_to_box(biom)
              if (length(vol) == length(m) && any(is.finite(vol) & vol > 0)) m <- m * vol
            }
          } else {
            ## epibenthic pool, mg N m-2 x box area (atbiolmessage.c:499)
            m <- as.numeric(biom)
            if (length(box_area) == length(m)) m <- m * box_area
          }
          m[!is.finite(m)] <- 0
          if (length(m) == n_box) mass <- mass + m
        }
        ## Atlantis feeds on pools as prey stage 0 only (pPREY1 rows;
        ## atprocess.c:372), so pools are not an adult-stage prey.
        juv <- add_stage_stock(juv, mass, rep(0, n_box), NA_real_)
        pool <- add_stage_stock(pool, mass, rep(0, n_box), NA_real_)
        found <- TRUE
      }
    }
    if (!found) {
      notes <- c(notes, code)
    }
    out_groups[[code]] <- list(
      code = code,
      name = name,
      grouptype = gtype,
      n_coh = n_coh,
      age_mat = age_mat,
      juvenile = finalise_stage_stock(juv, mg2t, x.cn),
      adult = finalise_stage_stock(adl, mg2t, x.cn),
      pool = finalise_stage_stock(pool, mg2t, x.cn)
    )
  }
  list(n_box = n_box, volume = vol, groups = out_groups, notes = unique(notes))
}

stage_key <- function(stage_int) {
  if (is.na(stage_int)) return("pool")
  if (as.integer(stage_int) == 1L) return("juvenile")
  if (as.integer(stage_int) == 2L) return("adult")
  "pool"
}

stock_stage <- function(stocks, code, stage_int) {
  g <- stocks$groups[[as.character(code)]]
  if (is.null(g)) return(NULL)
  key <- stage_key(stage_int)
  if (is.null(g[[key]])) g$pool else g[[key]]
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~         Predator feeding parameters         ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

read_predator_feed_params <- function(prm, grp) {
  prm <- prm_with_index(prm)
  names(grp) <- tolower(names(grp))
  flag_all <- read_prm_scalar(prm, "flagfishrates", default = NA_real_)
  rows <- lapply(seq_len(nrow(grp)), function(i) {
    code <- as.character(grp$code[i])
    n_coh <- as.integer(grp$numcohorts[i])
    if (!is.finite(n_coh) || n_coh < 1) n_coh <- 1L
    C <- read_prm_token(prm, paste0("C_", code))
    if (!length(C)) C <- read_prm_token(prm, paste0("C_", code, "_T15"))
    mum <- read_prm_token(prm, paste0("mum_", code))
    if (!length(mum)) mum <- read_prm_token(prm, paste0("mum_", code, "_T15"))
    predcase <- read_prm_scalar(prm, paste0("predcase_", code), default = 0)
    flag <- read_prm_scalar(prm, paste0("flagfishrates_", code), default = flag_all)
    if (!is.finite(flag)) {
      flag <- if (n_coh > 1) 0 else 1
    }
    list(
      code = code,
      C = C,
      mum = mum,
      E_live = read_prm_scalar(prm, paste0("E_", code), default = 1),
      E_plant = read_prm_scalar(prm, paste0("EPlant_", code), default = NA_real_),
      E_dl = read_prm_scalar(prm, paste0("EDL_", code), default = NA_real_),
      E_dr = read_prm_scalar(prm, paste0("EDR_", code), default = NA_real_),
      predcase = if (is.finite(predcase)) as.integer(predcase) else 0L,
      flagfishrates = as.integer(flag),
      klp = read_prm_scalar(prm, paste0("KLP_", code), default = NA_real_),
      kup = read_prm_scalar(prm, paste0("KUP_", code), default = NA_real_),
      age_mat = read_prm_scalar(prm, paste0(code, "_age_mat"), default = NA_real_),
      n_coh = n_coh
    )
  })
  names(rows) <- as.character(grp$code)
  rows
}

e_for_prey <- function(par, prey_code, prey_type) {
  cls <- prey_e_class(prey_code, prey_type)
  val <- switch(
    cls,
    plant = par$E_plant,
    detritus = par$E_dl,
    refractory = par$E_dr,
    par$E_live
  )
  if (!is.finite(val) || val <= 0) val <- par$E_live
  if (!is.finite(val) || val <= 0) val <- 1
  val
}

cohort_in_stage <- function(coh, age_mat, stage_int, n_coh) {
  if (is.na(stage_int)) return(TRUE)
  if (!is.finite(age_mat) || age_mat < 1) age_mat <- max(1, ceiling(n_coh / 2))
  ## Atlantis: adult when the 0-based cohort >= age_mat (atbiolsetup.c:542-562).
  ## coh here is 1-based.
  if (as.integer(stage_int) == 1L) return((coh - 1L) < age_mat)
  if (as.integer(stage_int) == 2L) return((coh - 1L) >= age_mat)
  TRUE
}

pick_rate <- function(vec, coh) {
  if (!length(vec)) return(NA_real_)
  i <- min(max(1L, as.integer(coh)), length(vec))
  as.numeric(vec[i])
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Expected pressure on a pPREY matrix     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

empty_pressure_result <- function(pprey) {
  z <- if (is.null(pprey) || !is.matrix(pprey)) {
    matrix(0, 0, 0)
  } else {
    pprey * 0
  }
  list(
    consumption_t_day = z,
    prey_loss_frac_day = z,
    prey_loss_frac_year = z,
    cells = data.frame(
      Predator_row = character(), Pred_code = character(),
      Pred_stage = character(), Prey = character(), Prey_stage = character(),
      pPREY = numeric(), Gape = numeric(), predcase = integer(),
      Consumption_t_day = numeric(), Prey_loss_frac_day = numeric(),
      Prey_loss_frac_year = numeric(), Prey_tonnes = numeric(),
      Flag = character(), stringsAsFactors = FALSE
    ),
    notes = "Load biology.prm, groups CSV and initial conditions to compute expected predation pressure.",
    n_box = 0L,
    boxes_used = integer(),
    flagfishrates = NA_integer_
  )
}

##' Expected predation pressure for every pPREY cell.
##'
##' System-wide is the sum of per-box calculations (true spatial
##' overlap: predator and prey must share a box). Set `box` to a
##' 0-based box id to restrict the sum. Pass `stocks` from
##' `init_stage_stocks` to reuse biomass when only the box changes.
##' `init_nc` may be init.nc or output.nc; `time_i` selects the step.
expected_predation_pressure <- function(pprey, grp, prm, init_nc,
                                        box = NULL, time_i = 1L,
                                        mg2t = MG2T_DEFAULT, x.cn = XCN_DEFAULT,
                                        habitat_refuge = 1, stocks = NULL) {
  if (is.null(pprey) || !is.matrix(pprey) || nrow(pprey) == 0) {
    return(empty_pressure_result(pprey))
  }
  prm <- prm_with_index(prm)
  if (!is.null(grp)) names(grp) <- tolower(names(grp))
  notes <- character()

  nc <- init_nc
  if (!inherits(nc, "ncdf4") && !is.null(init_nc)) {
    nc <- tryCatch(nc_handle(init_nc), error = function(e) NULL)
  }
  if (is.null(stocks)) {
    stocks <- if (is.null(nc)) {
      notes <- c(notes, "Biomass NetCDF could not be opened; pressure is zero.")
      list(n_box = 1L, volume = NA_real_, groups = list(), notes = notes)
    } else {
      init_stage_stocks(nc, grp, prm, time_i = time_i, mg2t = mg2t, x.cn = x.cn)
    }
  }
  if (length(stocks$notes)) {
    notes <- c(notes, paste0(
      length(stocks$notes), " groups have no t0 tracers in this init file (",
      paste(utils::head(stocks$notes, 8), collapse = ", "),
      if (length(stocks$notes) > 8) ", ..." else "",
      ")."
    ))
  }
  n_box <- max(1L, as.integer(stocks$n_box))
  vol <- stocks$volume
  if (length(vol) < n_box) vol <- c(vol, rep(NA_real_, n_box - length(vol)))
  use_conc <- any(is.finite(vol) & vol > 0)
  if (!use_conc) {
    notes <- c(notes, paste(
      "Box volume is missing. Prey is treated as a mass (mg N), not a",
      "concentration. Units of C then follow the prm as written."
    ))
  }

  boxes <- seq_len(n_box)
  if (!is.null(box) && length(box) && !identical(as.character(box)[1], "all")) {
    b0 <- suppressWarnings(as.integer(box))
    b0 <- b0[is.finite(b0)]
    hit <- b0 + 1L
    hit <- hit[hit >= 1L & hit <= n_box]
    if (length(hit)) boxes <- hit else {
      notes <- c(notes, "Requested box is outside the init grid; using all boxes.")
    }
  }

  params <- read_predator_feed_params(prm, grp)
  meta <- parse_pprey_rownames(rownames(pprey))
  prey_codes <- colnames(pprey)
  if (is.null(prey_codes)) prey_codes <- paste0("Prey", seq_len(ncol(pprey)))
  prey_type <- character(length(prey_codes))
  if (!is.null(grp) && "code" %in% names(grp)) {
    prey_type <- as.character(grp$grouptype[match(prey_codes, as.character(grp$code))])
  }
  prey_type[is.na(prey_type)] <- ""

  cons <- pprey * 0
  frac <- pprey * 0
  gape_mat <- pprey * 0
  cons[] <- 0
  frac[] <- 0
  gape_mat[] <- 0
  habitat_refuge <- as.numeric(habitat_refuge)[1]
  if (!is.finite(habitat_refuge)) habitat_refuge <- 1

  prey_mass_used <- matrix(0, nrow(pprey), ncol(pprey), dimnames = dimnames(pprey))
  unsupported <- character()

  if (is.null(nc)) return(empty_pressure_result(pprey))
  res <- cached_feeding_rates(pprey, grp, prm, nc, time_i = time_i)
  sel <- boxes[boxes <= res$n_box]
  cons_mg <- apply(res$cons_box[, , sel, drop = FALSE], c(1, 2), sum)
  dimnames(cons_mg) <- dimnames(pprey)
  pmass_sel <- apply(res$prey_mass_box[, , sel, drop = FALSE], c(1, 2), sum)
  for (ri in seq_len(nrow(pprey))) {
    st <- meta$prey_stage[ri]
    if (!is.finite(st)) st <- 1L
    prey_mass_used[ri, ] <- pmass_sel[st, ]
  }
  cons_row_mg <- cons_mg
  gape_mat[] <- res$window
  notes <- c(notes, res$notes)
  cons <- cons_row_mg * mg2t * x.cn
  frac <- ifelse(prey_mass_used > 0, cons_row_mg / prey_mass_used, 0)
  dim(frac) <- dim(cons); dimnames(frac) <- dimnames(cons)
  if (length(unsupported)) {
    notes <- c(notes, paste0(
      "Not computed (functional response predcase other than 0, 1 or 2): ",
      paste(unique(unsupported), collapse = ", "), "."
    ))
  }
  notes <- c(notes, paste(
    "Expected consumption from the Atlantis feeding equations, per predator cohort",
    "and water cell at this output time. Approximate: output snapshots miss",
    "day-night vertical movement and Q10 is not applied; on NESP it was within",
    "~10% of Atlantis's realised intake in most boxes and 0.3-4x system-wide.",
    "The realised intake is shown for the selected pair."
  ))
  frac[!is.finite(frac)] <- 0
  cons[!is.finite(cons)] <- 0
  year <- frac * 365
  cells <- pressure_cells_table(pprey, cons, frac, year, gape_mat, meta,
                                prey_codes, params, prey_mass_used, mg2t, x.cn)
  flag_n <- sum(cells$Flag != "OK", na.rm = TRUE)
  notes <- c(notes, paste0(
    "Holling functional response as in atprocess.c (User Guide Part I §10.2); ",
    "size window per atvertprocesses.c. Time step ", time_i,
    ". Boxes: ", if (length(boxes) == res$n_box) "all" else paste(boxes - 1L, collapse = ","), "."
  ))
  if (flag_n) {
    notes <- c(notes, paste0(flag_n, " cells flagged (loss > 5% of prey per day, or more than the standing stock)."))
  }

  list(
    consumption_t_day = cons,
    prey_loss_frac_day = frac,
    prey_loss_frac_year = year,
    gape = gape_mat,
    cells = cells,
    notes = paste(unique(notes), collapse = " "),
    n_box = n_box,
    boxes_used = boxes - 1L,
    flagfishrates = tryCatch(params[[1]]$flagfishrates, error = function(e) NA_integer_),
    stocks = stocks
  )
}

pressure_cells_table <- function(pprey, cons, frac, year, gape_mat, meta,
                                 prey_codes, params, prey_mass, mg2t, x.cn) {
  rows <- list()
  for (i in seq_len(nrow(pprey))) {
    par <- params[[meta$pred_code[i]]]
    pc <- if (is.null(par)) NA_integer_ else par$predcase
    for (j in seq_len(ncol(pprey))) {
      pv <- as.numeric(pprey[i, j])
      if (!is.finite(pv) && !is.finite(cons[i, j])) next
      if ((!is.finite(pv) || pv == 0) && (!is.finite(cons[i, j]) || cons[i, j] == 0)) {
        next
      }
      fd <- as.numeric(frac[i, j])
      flag <- "OK"
      if (is.finite(fd) && fd > 1) flag <- "loss > standing stock / day"
      else if (is.finite(fd) && fd > 0.05) flag <- "loss > 5% of prey / day"
      rows[[length(rows) + 1]] <- data.frame(
        Predator_row = rownames(pprey)[i],
        Pred_code = meta$pred_code[i],
        Pred_stage = pprey_stage_label(meta$pred_stage[i]),
        Prey = prey_codes[j],
        Prey_stage = pprey_stage_label(meta$prey_stage[i]),
        pPREY = pv,
        Gape = as.numeric(gape_mat[i, j]),
        predcase = pc,
        Consumption_t_day = as.numeric(cons[i, j]),
        Prey_loss_frac_day = fd,
        Prey_loss_frac_year = as.numeric(year[i, j]),
        Prey_tonnes = as.numeric(prey_mass[i, j]) * mg2t * x.cn,
        Flag = flag,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(empty_pressure_result(pprey)$cells)
  }
  out <- do.call(rbind, rows)
  out[order(-out$Prey_loss_frac_day, -out$Consumption_t_day), , drop = FALSE]
}

pressure_metric_matrix <- function(press, metric = "frac_day") {
  if (is.null(press)) return(NULL)
  switch(
    as.character(metric)[1],
    t_day = press$consumption_t_day,
    frac_year = press$prey_loss_frac_year,
    press$prey_loss_frac_day
  )
}

pressure_metric_label <- function(metric = "frac_day") {
  switch(
    as.character(metric)[1],
    t_day = "Expected consumption (t / day)",
    frac_year = "Expected prey loss (fraction / year)",
    "Expected predation pressure (prey loss / day)"
  )
}

pressure_hover_text <- function(pprey, press, grp = NULL) {
  if (is.null(pprey) || !is.matrix(pprey)) {
    return(matrix(character(), 0, 0))
  }
  cons <- press$consumption_t_day
  frac <- press$prey_loss_frac_day
  year <- press$prey_loss_frac_year
  gape <- press$gape
  meta <- parse_pprey_rownames(rownames(pprey))
  pred_lab <- pprey_group_label(meta$pred_code, grp)
  pred_st <- pprey_stage_label(meta$pred_stage)
  prey_st <- pprey_stage_label(meta$prey_stage)
  prey_lab <- pprey_group_label(colnames(pprey), grp)
  out <- matrix("", nrow(pprey), ncol(pprey), dimnames = dimnames(pprey))
  for (i in seq_len(nrow(pprey))) {
    for (j in seq_len(ncol(pprey))) {
      out[i, j] <- paste0(
        "Expected pressure: ", signif(frac[i, j], 4), " / day",
        "<br>Prey loss: ", signif(year[i, j], 4), " / year",
        "<br>Consumption: ", signif(cons[i, j], 4), " t / day",
        "<br>pPREY: ", signif(pprey[i, j], 4),
        "<br>Gape: ", signif(gape[i, j], 3),
        "<br>Predator: ", pred_lab[i], " - ", tolower(pred_st[i]),
        "<br>Prey: ", prey_lab[j], " - ", tolower(prey_st[i])
      )
    }
  }
  out
}

pressure_cell_detail <- function(press, sel) {
  cells <- press$cells
  if (is.null(cells) || nrow(cells) == 0 || is.null(sel)) {
    return(cells[0, , drop = FALSE])
  }
  keep <- rep(TRUE, nrow(cells))
  if (length(sel$rows)) {
    keep <- keep & cells$Predator_row %in% sel$rows
  }
  if (length(sel$prey)) {
    keep <- keep & cells$Prey %in% sel$prey
  }
  cells[keep, , drop = FALSE]
}

pressure_flag_table <- function(press) {
  cells <- press$cells
  if (is.null(cells) || nrow(cells) == 0) {
    return(data.frame(Note = "No predator-prey cells to flag."))
  }
  hit <- cells[cells$Flag != "OK", , drop = FALSE]
  if (nrow(hit) == 0) {
    return(data.frame(Note = "No cell exceeds 5% of prey biomass per day."))
  }
  hit
}

##' Compare expected consumption share with DietCheck for one pair.
pressure_vs_dietcheck <- function(press, diet, pred, prey) {
  empty <- data.frame(
    Source = character(), Value = numeric(), Note = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(press) || is.null(press$consumption_t_day)) return(empty)
  cons <- press$consumption_t_day
  meta <- parse_pprey_rownames(rownames(cons))
  rows <- which(meta$pred_code == as.character(pred))
  if (!length(rows) || !prey %in% colnames(cons)) return(empty)
  pred_tot <- sum(cons[rows, , drop = FALSE], na.rm = TRUE)
  pair <- sum(cons[rows, prey, drop = FALSE], na.rm = TRUE)
  share <- if (pred_tot > 0) pair / pred_tot else 0
  diet_val <- NA_real_
  note <- "DietCheck not loaded."
  if (!is.null(diet) && NROW(diet) > 0) {
    ser <- tryCatch(dietcheck_pair_series(diet, pred, prey), error = function(e) NULL)
    if (!is.null(ser) && nrow(ser) > 0) {
      diet_val <- mean(ser$Proportion, na.rm = TRUE)
      note <- "DietCheck mean proportion (all times) versus expected share of t0 consumption."
    }
  }
  data.frame(
    Source = c("Expected share of t0 consumption", "DietCheck mean proportion"),
    Value = c(share, diet_val),
    Note = c("Gr_prey / sum Gr from the Holling diagnostic.", note),
    stringsAsFactors = FALSE
  )
}

pressure_equation_note <- function() {
  paste(
    "Expected consumption from the Atlantis feeding equations at the selected time:",
    "each predator cohort in each water cell clears C / (1 + C sum(E a) / mum)",
    "per unit predator (capped at mum / E with flag_satiation), where",
    "a = pPREY x prey density x size window (hard, humped or bi-logistic as in",
    "the run's prm). Rates are per individual (flagfishrates 0) or per mg N.",
    "Prey loss is consumption / prey biomass in the selected boxes.",
    "Not included: Q10, habitat refuge, pSTOCK; output snapshots miss",
    "day-night vertical movement. Source: atprocess.c (Eat),",
    "atvertprocesses.c (prey window); User Guide Part I section 10.2."
  )
}

##' Flag cells whose expected consumption exceeds prey production (t/day).
pressure_flag_vs_production <- function(cells, prey, production_t_day) {
  if (is.null(cells) || !is.data.frame(cells) || nrow(cells) == 0) {
    return(cells)
  }
  if (!is.finite(production_t_day) || pprey_is_none(prey)) return(cells)
  if (!"Flag" %in% names(cells)) cells$Flag <- "OK"
  hit <- cells$Prey == as.character(prey)[1] &
    is.finite(cells$Consumption_t_day) &
    cells$Consumption_t_day > production_t_day
  cells$Flag[hit] <- "loss > prey production / day"
  cells
}

nc_n_times <- function(nc) {
  if (is.null(nc)) return(1L)
  if (inherits(nc, "ncdf4") && !is.null(nc$dim$t)) {
    n <- as.integer(nc$dim$t$len)
    if (is.finite(n) && n > 0) return(n)
  }
  1L
}

##' Per-box expected pressure for one predator–prey (adult/juvenile) pair.
##'
##' Uses the same Holling split as the full matrix: all prey of that
##' predator row enter the denominator; only the selected prey is reported.
pressure_pair_by_box <- function(pprey, grp, prm, nc,
                                 pred, prey,
                                 pred_stage = 1, prey_stage = 1,
                                 time_i = 1L,
                                 stocks = NULL,
                                 mg2t = MG2T_DEFAULT, x.cn = XCN_DEFAULT,
                                 habitat_refuge = 1) {
  empty <- data.frame(
    Box = integer(), Consumption_t_day = numeric(),
    Prey_loss_frac_day = numeric(), Prey_loss_frac_year = numeric(),
    Prey_tonnes = numeric(), Pred_tonnes = numeric(),
    Gape = numeric(), stringsAsFactors = FALSE
  )
  if (is.null(pprey) || !is.matrix(pprey)) return(empty)
  if (pprey_is_none(pred) || pprey_is_none(prey)) return(empty)
  handle <- nc
  if (!inherits(handle, "ncdf4")) handle <- tryCatch(nc_handle(nc), error = function(e) NULL)
  if (is.null(handle)) return(empty)
  if (!is.null(grp)) names(grp) <- tolower(names(grp))
  meta <- parse_pprey_rownames(rownames(pprey))
  rows <- pprey_match_rows(meta, pred, pred_stage)
  if (!identical(pprey_stage_wanted(prey_stage), "all")) {
    rows <- intersect(rows, pprey_match_prey_stage(meta, prey_stage))
  }
  pj <- match(as.character(prey), colnames(pprey))
  if (!length(rows) || is.na(pj)) return(empty)
  res <- tryCatch(cached_feeding_rates(pprey, grp, prm, handle, time_i = time_i),
                  error = function(e) NULL)
  if (is.null(res)) return(empty)
  n_b <- res$n_box
  cons_b <- colSums(matrix(res$cons_box[rows, pj, ], nrow = length(rows)))
  st <- meta$prey_stage[rows[1]]
  if (!is.finite(st)) st <- 1L
  prey_b <- res$prey_mass_box[st, pj, ]
  qj <- match(as.character(pred), colnames(pprey))
  pred_b <- if (is.na(qj)) rep(NA_real_, n_b) else colSums(res$prey_mass_box[, qj, , drop = FALSE][1:2, 1, ])
  frac <- ifelse(prey_b > 0, cons_b / prey_b, 0)
  data.frame(
    Box = seq_len(n_b) - 1L,
    Consumption_t_day = cons_b * mg2t * x.cn,
    Prey_loss_frac_day = frac,
    Prey_loss_frac_year = frac * 365,
    Prey_tonnes = prey_b * mg2t * x.cn,
    Pred_tonnes = pred_b * mg2t * x.cn,
    Gape = max(res$window[rows, pj]),
    stringsAsFactors = FALSE
  )
}

##' Prey production (t/day) at one time step.
##'
##' Age-structured prey: sum over cohorts and boxes of Nums x Growth, where
##' <Name><c>_Growth is mg N per individual per day (PROD units) and Nums
##' comes from output.nc. Pools: <Name>Prodn (mg N m-3 d-1) x cell volume.
prey_production_t_day <- function(nc_prod, nc_out, grp, prey_code,
                                  time_i = 1L, mg2t = MG2T_DEFAULT,
                                  x.cn = XCN_DEFAULT) {
  if (is.null(nc_prod) || is.null(grp) || pprey_is_none(prey_code)) {
    return(NA_real_)
  }
  names(grp) <- tolower(names(grp))
  row <- grp[as.character(grp$code) == as.character(prey_code)[1], , drop = FALSE]
  if (nrow(row) == 0) return(NA_real_)
  name <- as.character(row$name[1])
  n.coh <- as.integer(row$numcohorts[1])
  gtype <- as.character(row$grouptype[1])
  t <- max(1L, as.integer(time_i)[1])
  if (prod_is_age_structured(n.coh, gtype)) {
    if (is.null(nc_out)) return(NA_real_)
    tot <- 0
    found <- FALSE
    for (coh in seq_len(n.coh)) {
      g_var <- paste0(name, coh, "_Growth")
      n_var <- paste0(name, coh, "_Nums")
      if (!g_var %in% names(nc_prod$var) || !n_var %in% names(nc_out$var)) next
      g <- nc_get(nc_prod, g_var)
      n <- nc_get(nc_out, n_var)
      if (length(dim(g)) == 3) g <- apply(g, c(2, 3), sum, na.rm = TRUE)
      if (length(dim(n)) == 3) n <- apply(n, c(2, 3), sum, na.rm = TRUE)
      if (length(dim(g)) != 2 || length(dim(n)) != 2) next
      tt <- min(t, ncol(g), ncol(n))
      nb <- min(nrow(g), nrow(n))
      tot <- tot + sum(n[seq_len(nb), tt] * g[seq_len(nb), tt], na.rm = TRUE)
      found <- TRUE
    }
    if (!found) return(NA_real_)
    return(tot * mg2t * x.cn)
  }
  vn <- paste0(name, "Prodn")
  if (!vn %in% names(nc_prod$var)) return(NA_real_)
  arr <- nc_get(nc_prod, vn)
  vol <- tryCatch(nc_volume(nc_prod), error = function(e) NULL)
  if (is.null(vol) && !is.null(nc_out)) vol <- tryCatch(nc_volume(nc_out), error = function(e) NULL)
  if (length(dim(arr)) == 3 && !is.null(vol) && length(dim(vol)) == 3) {
    tt <- min(t, dim(arr)[3], dim(vol)[3])
    return(sum(arr[, , tt] * vol[, , tt], na.rm = TRUE) * mg2t * x.cn)
  }
  if (length(dim(arr)) == 2) {
    area <- if (!is.null(nc_out)) nc_box_area(nc_out) else nc_box_area(nc_prod)
    tt <- min(t, ncol(arr))
    if (length(area) == nrow(arr)) return(sum(arr[, tt] * area, na.rm = TRUE) * mg2t * x.cn)
  }
  NA_real_
}

##' DietCheck, SpecificPredMort and production next to expected pressure.
pressure_vs_realised <- function(press, diet, pred_mort, pred, prey,
                                 production_t_day = NA_real_,
                                 realised_pred_t_day = NA_real_,
                                 realised_pair_t_day = NA_real_) {
  empty <- data.frame(
    Source = character(), Value = numeric(), Note = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(press) || pprey_is_none(pred) || pprey_is_none(prey)) return(empty)
  cons <- press$consumption_t_day
  frac <- press$prey_loss_frac_day
  meta <- parse_pprey_rownames(rownames(cons))
  rows <- which(meta$pred_code == as.character(pred))
  if (!length(rows) || !prey %in% colnames(cons)) return(empty)
  pred_tot <- sum(cons[rows, , drop = FALSE], na.rm = TRUE)
  pair_t <- sum(cons[rows, prey, drop = FALSE], na.rm = TRUE)
  pair_f <- mean(frac[rows, prey], na.rm = TRUE)
  share <- if (pred_tot > 0) pair_t / pred_tot else 0
  diet_val <- NA_real_
  diet_note <- "DietCheck not loaded."
  if (!is.null(diet) && NROW(diet) > 0) {
    ser <- tryCatch(dietcheck_pair_series(diet, pred, prey), error = function(e) NULL)
    if (!is.null(ser) && nrow(ser) > 0) {
      diet_val <- mean(ser$Proportion, na.rm = TRUE)
      diet_note <- "DietCheck mean diet proportion (all times)."
    }
  }
  mort_val <- NA_real_
  mort_note <- "SpecificPredMort.txt not loaded."
  if (!is.null(pred_mort) && NROW(pred_mort) > 0) {
    pm <- tryCatch(
      parse_pred_mort_by_predator(pred_mort, prey, pred),
      error = function(e) NULL
    )
    if (!is.null(pm) && nrow(pm) > 0) {
      mort_val <- mean(pm$Value, na.rm = TRUE)
      mort_note <- paste("Mean over prey age classes and output periods of the fraction of",
                         "the prey's numbers killed by this predator per output period (not per day).")
    } else {
      mort_note <- attr(pm, "reason")
      if (is.null(mort_note) || !nzchar(mort_note)) {
        mort_note <- "No SpecificPredMort rows for this pair."
      }
    }
  }
  prod_note <- if (is.finite(production_t_day)) {
    if (is.finite(pair_t) && pair_t > production_t_day) {
      "Expected consumption exceeds prey production at this time step."
    } else {
      "Prey production (Growth/Prodn) at this time step."
    }
  } else {
    "outputPROD.nc production tracer not available."
  }
  data.frame(
    Source = c(
      "Expected consumption (t/day)",
      "Expected prey loss / day",
      "Expected share of predator consumption",
      "Atlantis realised intake of predator (t/day)",
      "Atlantis realised intake of this prey (t/day)",
      "DietCheck mean proportion",
      "SpecificPredMort mean (fraction per output period)",
      "Prey production (t/day)"
    ),
    Value = c(pair_t, pair_f, share, realised_pred_t_day, realised_pair_t_day,
              diet_val, mort_val, production_t_day),
    Note = c(
      "Atlantis feeding equations per cohort and cell, summed over the selected boxes (approximate; see notes).",
      "Gr_j / prey biomass.",
      "Gr_prey / sum Gr.",
      "Sum over cohorts and boxes of Nums x Growth / E from output.nc and outputPROD.nc.",
      "Cohort intake x that cohort's DietCheck proportion, summed.",
      diet_note,
      mort_note,
      prod_note
    ),
    stringsAsFactors = FALSE
  )
}

pressure_map_polygons <- function(poly, by_box, metric = "frac_day") {
  if (is.null(poly) || nrow(poly) == 0 || is.null(by_box) || nrow(by_box) == 0) {
    return(poly)
  }
  val <- switch(
    as.character(metric)[1],
    t_day = by_box$Consumption_t_day,
    frac_year = by_box$Prey_loss_frac_year,
    by_box$Prey_loss_frac_day
  )
  map <- stats::setNames(val, as.character(by_box$Box))
  poly$Value <- unname(map[as.character(poly$boxid)])
  poly
}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~   Atlantis-faithful feeding (per cohort, per cell)   ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##
## Rebuild of the expected predation pressure (validation D05) following the
## Atlantis feeding code (atprocess.c Eat, atvertprocesses.c prey window):
##  - each predator cohort in each water cell (box x layer) feeds on the prey
##    present in that cell, with sp = Nums / cell volume (flagfishrates = 0)
##    or C and mum scaled by the individual's mass (flagfishrates = 1);
##    biomass-pool predators use their concentration and mass-specific rates;
##  - available prey a_j = pPREY(prey stage, predator stage) x prey density x
##    size window; one Holling denominator over all prey of the cohort:
##    clearance = C / (1 + C sum_j(E_j a_j) / mum); with flag_satiation the
##    clearance is capped at mum / E (atprocess.c:896); type I and III as in
##    atprocess.c:840-848;
##  - size window on vertebrate prey only: hard (KLP SN <= prey SN <= KUP SN),
##    humped, or bi-logistic on length, as selected by UseHardFeedingWindow and
##    UseBiLogisticFeedingWindow (atvertprocesses.c:541-653);
##  - pools are stage-1 prey (pPREY1 rows); sediment and epibenthic prey are
##    available in each box's deepest water cell.
## Not included: Q10 (on NESP the realised temperature multiplier on rates was
## ~1, see validation phase 4), habitat refuge, pSTOCK, pH and turbidity.
## On NESP (adult FTM, box 94, day 930) the same calculation in Python gave
## 0.0052 t/day against 0.0050 t/day realised from Atlantis Growth / E.

ra_window_scalar <- function(mode, prey_sn, prey_w, pred_sn, pred_w, klp, kup,
                             kmax, li_a_prey, li_b_prey, li_a_pred, li_b_pred) {
  n <- length(prey_sn)
  if (identical(mode, "hard")) {
    return(as.numeric(prey_sn >= klp * pred_sn & prey_sn <= kup * pred_sn))
  }
  if (identical(mode, "humped")) {
    rel <- prey_sn / (kup * pred_sn)
    s <- rel * exp(kmax * (1 - rel))
  } else {
    ## bi-logistic on length; length (cm) from wet weight (g) = W mg N x 20 x X_CN / 1000
    len <- function(w, a, b) (pmax(w, 1e-30) * 20 * 5.7 / 1000 / a)^(1 / b)
    pl <- len(pred_w, li_a_pred, li_b_pred)
    ql <- len(prey_w, li_a_prey, li_b_prey)
    mx <- klp + (kup - klp) * 0.5
    lo <- (klp + (mx - klp) * 0.5) * pl
    hi <- (kup - (kup - mx) * 0.5) * pl
    s <- ifelse(ql <= mx * pl,
                1 / (1 + exp(pmin(700, pmax(-700, -kmax * (ql - lo))))),
                1 / (1 + exp(pmin(700, pmax(-700, kmax * (ql - hi))))))
  }
  s[!is.finite(s)] <- 0
  pmin(1, pmax(0, s))
}

##' Atlantis-faithful consumption for every pPREY row and prey column.
##' @return list(cons = mg N/day [row x prey], cons_box = [row x prey x box],
##'   window = max size scalar [row x prey], prey_mass = mg N by prey and
##'   stage, notes)
atlantis_feeding_rates <- function(pprey, grp, prm, nc, time_i = 1L, boxes = NULL,
                                   q10 = FALSE, exclude_boxes = NULL) {
  prm <- prm_with_index(prm)
  names(grp) <- tolower(names(grp))
  vars <- names(nc$var)
  notes <- character()
  t <- max(1L, as.integer(time_i)[1])
  slice <- function(vn) {
    a <- tryCatch(nc_get_at_t(nc, vn, t), error = function(e) NULL)
    if (is.null(a)) a <- tryCatch(nc_get(nc, vn), error = function(e) NULL)
    if (!is.null(a) && length(dim(a)) == 3) a <- a[, , min(t, dim(a)[3])]
    ## restore dimensions dropped by ncdf4 (e.g. a single box)
    sz <- nc$var[[vn]]$size
    has_t <- !is.null(nc$dim$t) && length(sz) && sz[length(sz)] == nc$dim$t$len &&
      any(vapply(nc$var[[vn]]$dim, function(d) identical(d$name, "t"), logical(1)))
    want <- if (has_t) sz[-length(sz)] else sz
    if (!is.null(a) && length(want) >= 2 && length(a) == prod(want)) a <- array(a, want)
    a
  }
  vol <- slice("volume")
  if (is.null(vol) || length(dim(vol)) != 2) stop("output.nc / init.nc has no volume variable")
  n_z <- nrow(vol); n_b <- ncol(vol)
  info <- tryCatch(atlantis_layer_info(nc), error = function(e) NULL)
  n_w <- if (!is.null(info)) info$n_water else rep(n_z - 1L, n_b)
  water <- matrix(FALSE, n_z, n_b)
  bottom <- rep(NA_integer_, n_b)
  for (b in seq_len(n_b)) {
    if (!is.finite(n_w[b]) || n_w[b] < 1) next
    zs <- (n_z - n_w[b]):(n_z - 1L)
    water[zs, b] <- vol[zs, b] > 0
    bottom[b] <- zs[1]
  }
  if (!is.null(boxes)) {
    keep_b <- seq_len(n_b) %in% (as.integer(boxes) + 1L)
    water[, !keep_b] <- FALSE
  }
  ## Atlantis does not run ecology in boundary boxes (their animals are held
  ## at fixed values), so nothing feeds there.
  if (length(exclude_boxes)) water[, as.integer(exclude_boxes) + 1L] <- FALSE
  cells <- which(water)
  cell_box <- col(water)[cells]
  vcell <- vol[cells]
  area <- nc_box_area(nc)
  ## put a box-level mass (mg N) into the deepest water cell as a density
  to_bottom <- function(mass_b) {
    d <- numeric(length(cells))
    for (b in unique(cell_box)) {
      zb <- bottom[b]
      if (!is.finite(zb)) next
      k <- which(cells == (b - 1L) * n_z + zb)
      if (length(k) && vcell[k] > 0) d[k] <- mass_b[b] / vcell[k]
    }
    d
  }
  hard <- isTRUE(read_prm_scalar(prm, "UseHardFeedingWindow", default = 1) == 1)
  bilog <- isTRUE(read_prm_scalar(prm, "UseBiLogisticFeedingWindow", default = 0) == 1)
  mode <- if (hard) "hard" else if (bilog) "bilogistic" else "humped"
  satiation <- isTRUE(read_prm_scalar(prm, "flag_satiation", default = 0) == 1)
  params <- read_predator_feed_params(prm, grp)
  codes <- as.character(grp$code)
  ## size and Q10 parameters, read once per group
  gpar <- lapply(codes, function(cd) list(
    li_a = read_prm_scalar(prm, paste0("li_a_", cd), default = NA_real_),
    li_b = read_prm_scalar(prm, paste0("li_b_", cd), default = NA_real_),
    kmax = read_prm_scalar(prm, paste0("Kmax_coefft_", cd), default = 1),
    age_mat = read_prm_scalar(prm, paste0(cd, "_age_mat"), default = NA_real_)))
  names(gpar) <- codes
  temp <- if (isTRUE(q10)) slice("Temp") else NULL
  tcorr <- if (!is.null(temp) && length(dim(temp)) == 2) 0.693 * 1.066^temp[cells] else rep(1, length(cells))
  tcorr[!is.finite(tcorr)] <- 1

  ## ---- prey items: one per prey cohort (age-structured) or pool ----
  prey_items <- list()
  is_vert <- function(g) !as.character(g$grouptype) %in% c(AGE_SPECIAL, "PWN", "PRAWN", "PRAWNS", "CEP")
  for (i in seq_len(nrow(grp))) {
    g <- grp[i, ]
    code <- as.character(g$code); name <- as.character(g$name)
    n_coh <- max(1L, as.integer(g$numcohorts))
    age_mat <- gpar[[code]]$age_mat
    if (!is.finite(age_mat)) age_mat <- max(1, ceiling(n_coh / 2))
    if (n_coh > 1 && !g$grouptype %in% AGE_SPECIAL && paste0(name, "1_Nums") %in% vars) {
      for (k in seq_len(n_coh)) {
        nv <- paste0(name, k, "_Nums")
        if (!nv %in% vars) next
        N <- slice(nv); SN <- slice(paste0(name, k, "_StructN")); RN <- slice(paste0(name, k, "_ResN"))
        if (is.null(N) || is.null(SN) || is.null(RN)) next
        Nc <- N[cells]; Nc[!is.finite(Nc)] <- 0
        if (!any(Nc > 0)) next
        prey_items[[length(prey_items) + 1]] <- list(
          code = code, col = code, stage = if ((k - 1) < age_mat) 1L else 2L,
          dens = Nc * (SN[cells] + RN[cells]) / vcell, sn = SN[cells],
          w = SN[cells] + RN[cells], vert = is_vert(g), type = as.character(g$grouptype),
          li_a = gpar[[code]]$li_a, li_b = gpar[[code]]$li_b)
      }
    } else {
      dens <- numeric(length(cells))
      found <- FALSE
      for (vn in pool_var_names(vars, name, n_coh, g$grouptype)) {
        x <- slice(vn)
        if (is.null(x)) next
        found <- TRUE
        if (length(dim(x)) == 2) {
          dens <- dens + x[cells]
          if (n_z >= 2) {  # sediment layer -> deepest water cell
            sed <- x[n_z, ] * vol[n_z, ]
            sed[!is.finite(sed)] <- 0
            dens <- dens + to_bottom(sed)
          }
        } else if (length(x) == n_b && length(area) == n_b) {
          m <- x * area; m[!is.finite(m)] <- 0
          dens <- dens + to_bottom(m)
        }
      }
      if (!found) next
      dens[!is.finite(dens)] <- 0
      prey_items[[length(prey_items) + 1]] <- list(
        code = code, col = code, stage = 1L, dens = dens, sn = NA, w = NA,
        vert = FALSE, type = as.character(g$grouptype), li_a = NA, li_b = NA)
      if (code %in% c("DL", "DR", "DC")) {   # sediment detritus columns
        x <- slice(pool_var_names(vars, name, 1L, g$grouptype)[1])
        if (!is.null(x) && length(dim(x)) == 2 && n_z >= 2) {
          sed <- x[n_z, ] * vol[n_z, ]; sed[!is.finite(sed)] <- 0
          prey_items[[length(prey_items) + 1]] <- list(
            code = code, col = paste0(code, "sed"), stage = 1L, dens = to_bottom(sed),
            sn = NA, w = NA, vert = FALSE, type = as.character(g$grouptype), li_a = NA, li_b = NA)
        }
      }
    }
  }
  prey_cols <- colnames(pprey)
  item_col <- match(vapply(prey_items, `[[`, "", "col"), prey_cols)

  ## ---- outputs ----
  meta <- parse_pprey_rownames(rownames(pprey))
  cons <- matrix(0, nrow(pprey), ncol(pprey), dimnames = dimnames(pprey))
  win <- cons
  cons_box <- array(0, c(nrow(pprey), ncol(pprey), n_b))
  prey_mass <- matrix(0, 2, ncol(pprey), dimnames = list(c("1", "2"), prey_cols))
  prey_mass_box <- array(0, c(2, ncol(pprey), n_b))
  for (it in seq_along(prey_items)) {
    j <- item_col[it]
    if (is.na(j)) next
    st <- prey_items[[it]]$stage
    m <- prey_items[[it]]$dens * vcell
    prey_mass[st, j] <- prey_mass[st, j] + sum(m)
    prey_mass_box[st, j, ] <- prey_mass_box[st, j, ] + tabulate_by(m, cell_box, n_b)
  }
  unsupported <- character()

  ## ---- predators ----
  for (pred in unique(meta$pred_code)) {
    par <- params[[pred]]
    if (is.null(par)) next
    if (!par$predcase %in% 0:2) { unsupported <- c(unsupported, pred); next }
    g <- grp[grp$code == pred, ][1, ]
    name <- as.character(g$name); n_coh <- max(1L, as.integer(g$numcohorts))
    rows <- which(meta$pred_code == pred)
    E1 <- if (is.finite(par$E_live) && par$E_live > 0) par$E_live else 1
    e_item <- vapply(prey_items, function(p) e_for_prey(par, p$col, p$type), numeric(1))
    age_mat <- if (is.finite(par$age_mat)) par$age_mat else max(1, ceiling(n_coh / 2))
    cohorts <- list()
    if (n_coh > 1 && !g$grouptype %in% AGE_SPECIAL && paste0(name, "1_Nums") %in% vars) {
      for (k in seq_len(n_coh)) {
        N <- slice(paste0(name, k, "_Nums"))
        if (is.null(N)) next
        SN <- slice(paste0(name, k, "_StructN")); RN <- slice(paste0(name, k, "_ResN"))
        Nc <- N[cells]; Nc[!is.finite(Nc)] <- 0
        if (!any(Nc > 0)) next
        w <- SN[cells] + RN[cells]
        C <- pick_rate(par$C, k); mum <- pick_rate(par$mum, k)
        if (identical(as.integer(par$flagfishrates), 1L)) { C <- C * w; mum <- mum * w }
        cohorts[[length(cohorts) + 1]] <- list(stage = if ((k - 1) < age_mat) 1L else 2L,
          sp = Nc / vcell, C = C, mum = mum, sn = SN[cells], w = w)
      }
    } else {
      dens <- numeric(length(cells))
      for (vn in pool_var_names(vars, name, n_coh, g$grouptype)) {
        x <- slice(vn)
        if (is.null(x)) next
        if (length(dim(x)) == 2) dens <- dens + x[cells]
        else if (length(x) == n_b && length(area) == n_b) {
          m <- x * area; m[!is.finite(m)] <- 0; dens <- dens + to_bottom(m)
        }
      }
      dens[!is.finite(dens)] <- 0
      if (any(dens > 0)) cohorts[[1]] <- list(stage = NA_integer_, sp = dens,
        C = pick_rate(par$C, 1), mum = pick_rate(par$mum, 1), sn = NA, w = NA)
    }
    for (ch in cohorts) {
      ## pPREY row for each prey stage, given this predator stage
      row_for <- function(js) {
        r <- rows[(is.na(meta$pred_stage[rows]) | meta$pred_stage[rows] %in% ch$stage) &
                  (is.na(meta$prey_stage[rows]) | meta$prey_stage[rows] == js)]
        if (length(r)) r[1] else NA_integer_
      }
      rmap <- c(row_for(1L), row_for(2L))
      if (all(is.na(rmap))) next
      avail <- matrix(0, length(cells), length(prey_items))
      for (it in seq_along(prey_items)) {
        j <- item_col[it]; p <- prey_items[[it]]
        r <- rmap[p$stage]
        if (is.na(j) || is.na(r)) next
        pp <- as.numeric(pprey[r, j])
        if (!is.finite(pp) || pp <= 0) next
        s <- if (isTRUE(p$vert) && length(ch$sn) == length(cells)) {
          ra_window_scalar(mode, p$sn, p$w, ch$sn, ch$w, par$klp, par$kup,
                           gpar[[pred]]$kmax, p$li_a, p$li_b,
                           gpar[[pred]]$li_a, gpar[[pred]]$li_b)
        } else 1
        if (length(s) == 1) s <- rep(s, length(cells))
        win[r, j] <- max(win[r, j], max(s[p$dens > 0], 0, na.rm = TRUE))
        avail[, it] <- pp * p$dens * s
      }
      avail[!is.finite(avail)] <- 0
      if (identical(as.integer(par$predcase), 2L)) avail <- avail^2
      A <- rowSums(avail)
      EA <- as.numeric(avail %*% e_item)
      C <- ch$C; mum <- ch$mum
      if (length(C) == 1) C <- rep(C, length(cells))
      if (length(mum) == 1) mum <- rep(mum, length(cells))
      C <- C * tcorr; mum <- mum * tcorr
      if (identical(as.integer(par$predcase), 1L)) {
        ingest <- pmin(C * A, mum / E1)               # per unit predator
        frac_j <- ifelse(A > 0, avail / A, 0)
        eaten <- ch$sp * ingest * vcell * frac_j
      } else {
        clear <- C / (1 + C * EA / mum)
        if (satiation) clear <- pmin(clear, mum / E1)
        clear[!is.finite(clear)] <- 0
        eaten <- ch$sp * clear * vcell * avail
      }
      eaten[!is.finite(eaten)] <- 0
      for (it in which(colSums(eaten) > 0)) {
        j <- item_col[it]; r <- rmap[prey_items[[it]]$stage]
        if (is.na(j) || is.na(r)) next
        cons[r, j] <- cons[r, j] + sum(eaten[, it])
        cons_box[r, j, ] <- cons_box[r, j, ] + tabulate_by(eaten[, it], cell_box, n_b)
      }
    }
  }
  if (length(unsupported)) notes <- c(notes, paste0(
    "Not computed (predcase other than 0, 1, 2): ", paste(unique(unsupported), collapse = ", "), "."))
  notes <- c(notes, paste0("Feeding window: ", mode, "; satiation cap: ",
                           if (satiation) "on" else "off", "; no Q10."))
  list(cons = cons, cons_box = cons_box, window = win, prey_mass = prey_mass,
       prey_mass_box = prey_mass_box, n_box = n_b, notes = notes)
}

tabulate_by <- function(x, g, n) {
  out <- numeric(n)
  s <- rowsum(x, g)
  out[as.integer(rownames(s))] <- s[, 1]
  out
}

##' atlantis_feeding_rates() for all boxes, cached per file, time step and
##' pPREY matrix (the view used to take ~2 min on every visit; PF2).
cached_feeding_rates <- function(pprey, grp, prm, nc, time_i = 1L) {
  path <- tryCatch(nc$filename, error = function(e) "anon")
  sig <- paste(dim(pprey), signif(sum(pprey * seq_along(pprey), na.rm = TRUE), 12),
               paste(rownames(pprey)[1:min(3, nrow(pprey))], collapse = ","), collapse = "|")
  key <- paste("feed", file_cache_key(path, paste("t", time_i)), sig, sep = "|")
  hit <- ra_cache_get(key)
  if (!is.null(hit)) return(hit)
  ra_cache_set(key, atlantis_feeding_rates(pprey, grp, prm, nc, time_i = time_i))
}
