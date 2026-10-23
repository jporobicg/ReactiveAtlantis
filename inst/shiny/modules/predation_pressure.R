## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Expected predation pressure (t0 diagnostic) ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
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
  if (code %in% c("DL", "DC", "DLsed", "DCsed") ||
      grepl("LAB_DET|CARRION|DETRIT", gt)) {
    return("detritus")
  }
  if (grepl("PHY|PLANT|SEAGRASS|MACROAL|DINO|MICROPHY|LG_PHY|SM_PHY", gt)) {
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
  if (sn_prey > lo && sn_prey < hi) 1 else 0
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
                                   predcase = 0) {
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
    e_eff <- sum(e_use) / A
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

box_slice_t0 <- function(arr, time_i = 1L) {
  if (is.null(arr)) return(NULL)
  d <- dim(arr)
  if (is.null(d)) return(as.numeric(arr))
  t <- as.integer(time_i)[1]
  if (!is.finite(t) || t < 1) t <- 1L
  if (length(d) == 3L) {
    if (t > d[3]) t <- d[3]
    return(arr[, , t, drop = TRUE])
  }
  if (length(d) == 2L) return(arr)
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
  vol <- tryCatch(nc_volume(nc), error = function(e) NULL)
  sl <- box_slice_t0(vol, time_i)
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
        nums <- sum_z_to_box(box_slice_t0(nc_get(nc, n_var), time_i))
        resn <- box_slice_t0(nc_get(nc, r_var), time_i)
        strn <- box_slice_t0(nc_get(nc, s_var), time_i)
        mass <- sum_z_to_box((resn + strn) * box_slice_t0(nc_get(nc, n_var), time_i))
        sn_w <- weighted_sn(strn, box_slice_t0(nc_get(nc, n_var), time_i))
        if (coh < age_mat) {
          juv <- add_stage_stock(juv, mass, nums, sn_w)
        } else {
          adl <- add_stage_stock(adl, mass, nums, sn_w)
        }
        pool <- add_stage_stock(pool, mass, nums, sn_w)
        found <- TRUE
      }
    } else {
      vname <- if (n_coh > 1L && gtype %in% AGE_SPECIAL) {
        paste0(name, "_N1")
      } else {
        paste0(name, "_N")
      }
      if (!vname %in% vars && paste0(name, "_N") %in% vars) {
        vname <- paste0(name, "_N")
      }
      if (vname %in% vars) {
        biom <- box_slice_t0(nc_get(nc, vname), time_i)
        vol2 <- box_slice_t0(tryCatch(nc_volume(nc), error = function(e) NULL),
                             time_i)
        if (!is.null(vol2) && !is.null(dim(biom)) && length(dim(biom)) == 2L &&
            length(dim(vol2)) == 2L && all(dim(biom) == dim(vol2))) {
          mass <- sum_z_to_box(biom * vol2)
        } else {
          mass <- sum_z_to_box(biom)
          if (length(vol) == length(mass) && any(is.finite(vol) & vol > 0)) {
            mass <- mass * vol
          }
        }
        juv <- add_stage_stock(juv, mass, rep(0, n_box), NA_real_)
        adl <- add_stage_stock(adl, mass, rep(0, n_box), NA_real_)
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
  if (as.integer(stage_int) == 1L) return(coh < age_mat)
  if (as.integer(stage_int) == 2L) return(coh >= age_mat)
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

##' Expected predation pressure for every pPREY cell at t0.
##'
##' System-wide is the sum of per-box calculations (true spatial
##' overlap: predator and prey must share a box). Set `box` to a
##' 0-based box id to restrict the sum.
expected_predation_pressure <- function(pprey, grp, prm, init_nc,
                                        box = NULL, time_i = 1L,
                                        mg2t = MG2T_DEFAULT, x.cn = XCN_DEFAULT,
                                        habitat_refuge = 1) {
  if (is.null(pprey) || !is.matrix(pprey) || nrow(pprey) == 0) {
    return(empty_pressure_result(pprey))
  }
  prm <- prm_with_index(prm)
  if (!is.null(grp)) names(grp) <- tolower(names(grp))
  notes <- character()

  nc <- init_nc
  if (!inherits(nc, "ncdf4")) {
    nc <- tryCatch(nc_handle(init_nc), error = function(e) NULL)
  }
  stocks <- if (is.null(nc)) {
    notes <- c(notes, "Initial-conditions NetCDF could not be opened; pressure is zero.")
    list(n_box = 1L, volume = NA_real_, groups = list(), notes = notes)
  } else {
    init_stage_stocks(nc, grp, prm, time_i = time_i, mg2t = mg2t, x.cn = x.cn)
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

  for (ri in seq_len(nrow(pprey))) {
    pred_code <- meta$pred_code[ri]
    pred_stage <- meta$pred_stage[ri]
    prey_stage <- meta$prey_stage[ri]
    par <- params[[pred_code]]
    if (is.null(par)) next
    pred_g <- stocks$groups[[pred_code]]
    if (is.null(pred_g)) next

    n_coh <- pred_g$n_coh
    age_mat <- pred_g$age_mat
    predcase <- par$predcase
    absolute <- identical(as.integer(par$flagfishrates), 0L) && n_coh > 1L

    cons_row <- rep(0, ncol(pprey))
    prey_mass_row <- rep(0, ncol(pprey))
    gape_row <- rep(0, ncol(pprey))
    gape_n <- rep(0, ncol(pprey))

    cohs <- if (n_coh > 1L && !pred_g$grouptype %in% AGE_SPECIAL) {
      seq_len(n_coh)
    } else {
      1L
    }
    cohs <- cohs[vapply(cohs, function(c) {
      cohort_in_stage(c, age_mat, pred_stage, n_coh)
    }, logical(1))]
    if (!length(cohs)) cohs <- 1L

    for (b in boxes) {
      pred_st <- stock_stage(stocks, pred_code, pred_stage)
      if (is.null(pred_st)) next
      if (absolute) {
        pred_scale <- pred_st$nums[b]
      } else {
        pred_scale <- pred_st$mass_mgn[b]
      }
      if (!is.finite(pred_scale) || pred_scale <= 0) next

      C_use <- if (length(par$C)) mean(par$C[pmin(cohs, length(par$C))]) else NA_real_
      mum_use <- if (length(par$mum)) mean(par$mum[pmin(cohs, length(par$mum))]) else NA_real_
      if (length(cohs) == 1L) {
        C_use <- pick_rate(par$C, cohs)
        mum_use <- pick_rate(par$mum, cohs)
      }
      sn_pred <- pred_st$sn
      if (!is.finite(sn_pred) || sn_pred <= 0) sn_pred <- NA_real_

      avail <- rep(0, ncol(pprey))
      e_av <- rep(0, ncol(pprey))
      gape_b <- rep(0, ncol(pprey))
      prey_mass_b <- rep(0, ncol(pprey))

      for (pj in seq_len(ncol(pprey))) {
        pval <- as.numeric(pprey[ri, pj])
        if (!is.finite(pval) || pval <= 0) next
        pry <- stock_stage(stocks, prey_codes[pj], prey_stage)
        if (is.null(pry)) {
          ## Detritus sediment columns have no group row.
          pry <- empty_stage_stock(n_box)
        }
        mass <- pry$mass_mgn[b]
        if (!is.finite(mass) || mass <= 0) next
        prey_mass_b[pj] <- mass
        conc <- if (use_conc && is.finite(vol[b]) && vol[b] > 0) {
          mass / vol[b]
        } else {
          mass
        }
        gape <- gape_size_scalar(sn_pred, pry$sn, par$klp, par$kup)
        gape_b[pj] <- gape
        avail[pj] <- pval * conc * gape * habitat_refuge
        e_av[pj] <- e_for_prey(par, prey_codes[pj], prey_type[pj]) * avail[pj]
      }
      eaten <- holling_ingest_mgn_day(
        C_use, mum_use, avail, e_av, pred_scale, predcase
      )
      cons_row <- cons_row + eaten
      prey_mass_row <- prey_mass_row + prey_mass_b
      gape_row <- gape_row + gape_b
      gape_n <- gape_n + as.numeric(gape_b > 0 | avail > 0 | prey_mass_b > 0)
    }

    cons[ri, ] <- cons_row * mg2t * x.cn
    prey_mass_used[ri, ] <- prey_mass_row
    frac[ri, ] <- ifelse(prey_mass_row > 0, cons_row / prey_mass_row, 0)
    gape_mat[ri, ] <- ifelse(gape_n > 0, gape_row / pmax(gape_n, 1), 0)
  }

  frac[!is.finite(frac)] <- 0
  cons[!is.finite(cons)] <- 0
  year <- frac * 365
  cells <- pressure_cells_table(pprey, cons, frac, year, gape_mat, meta,
                                prey_codes, params, prey_mass_used, mg2t, x.cn)
  flag_n <- sum(cells$Flag != "OK", na.rm = TRUE)
  notes <- c(notes, paste0(
    "Modified Holling from User Guide Part I §10.2 / Table 18; ",
    "gape knife-edge §10.2.5 (KLP, KUP). Boxes used: ",
    paste(boxes - 1L, collapse = ","), "."
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
    "Expected predation pressure is the implied prey-biomass loss rate",
    "from the Atlantis feeding equations at t0, not a DietCheck proportion.",
    "Availability: B* = pPREY x prey concentration x gape (KLP/KUP knife-edge).",
    "Type II (predcase 0): I = C A / (1 + C/mum x sum(E B*)), then",
    "Gr = I x predator scale x (B*_j / A).",
    "Cited: User Guide Part I §10.2, §10.2.2, Table 18, §10.2.5;",
    "atecology/atbiology.c box_bio_process; Murray and Parslow 1997.",
    "KDENR is recruitment in the current manuals and is not used here."
  )
}
