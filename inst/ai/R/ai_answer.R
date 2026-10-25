## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Ask Atlantis: answer pipeline and validation ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ra_ai_parse_json_object <- function(text) {
  if (!ra_ai_need_pkg("jsonlite")) return(NULL)
  txt <- trimws(as.character(text)[1])
  if (!nzchar(txt)) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.list(parsed) && !is.null(parsed$answer)) return(parsed)
  fenced <- sub("(?s).*```(?:json)?\\s*", "", txt, perl = TRUE)
  fenced <- sub("```.*", "", fenced, perl = TRUE)
  parsed <- tryCatch(jsonlite::fromJSON(trimws(fenced), simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.list(parsed) && !is.null(parsed$answer)) return(parsed)
  a <- regexpr("\\{", txt)
  b <- max(gregexpr("\\}", txt)[[1]])
  if (a[1] > 0 && b > a[1]) {
    parsed <- tryCatch(
      jsonlite::fromJSON(substring(txt, a[1], b), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.list(parsed) && !is.null(parsed$answer)) return(parsed)
  }
  NULL
}

ra_ai_as_list_of_lists <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (is.list(x) && !is.null(x$chunk_id) && !is.list(x$chunk_id)) {
    return(list(x))
  }
  if (is.list(x) && !is.null(x$id) && !is.list(x$id) && is.null(x[[1]])) {
    return(list(x))
  }
  lapply(x, function(item) {
    if (is.list(item)) item else list(id = as.character(item)[1])
  })
}

##' Keep citations that match indexed chunk or source ids.
ra_ai_validate_citations <- function(citations, chunks) {
  citations <- ra_ai_as_list_of_lists(citations)
  if (nrow(chunks) == 0 || length(citations) == 0) return(list())
  valid_chunk <- unique(as.character(chunks$chunk_id))
  valid_source <- unique(as.character(chunks$source_id))
  out <- list()
  seen <- character()
  for (cit in citations) {
    cid <- as.character(cit$chunk_id %||% "")[1]
    sid <- as.character(cit$source_id %||% "")[1]
    row <- NULL
    if (nzchar(cid) && cid %in% valid_chunk) {
      row <- chunks[chunks$chunk_id == cid, , drop = FALSE][1, ]
    } else if (nzchar(sid) && sid %in% valid_source) {
      row <- chunks[chunks$source_id == sid, , drop = FALSE][1, ]
    }
    if (is.null(row) || nrow(row) == 0) next
    key <- as.character(row$chunk_id)[1]
    if (key %in% seen) next
    seen <- c(seen, key)
    out[[length(out) + 1]] <- ra_ai_citation_from_row(row)
  }
  out
}

ra_ai_chr_list <- function(x) {
  if (is.null(x)) return(character())
  if (is.list(x)) x <- unlist(lapply(x, function(i) as.character(i)[1]))
  x <- as.character(x)
  unname(x[!is.na(x) & nzchar(trimws(x))])
}

ra_ai_validate_evidence_status <- function(x, default = "insufficient") {
  allowed <- c("supported", "partial", "insufficient", "out_of_scope")
  x <- tolower(as.character(x %||% default)[1])
  if (x %in% allowed) x else default
}

ra_ai_structured_fields <- function(parsed = list(),
                                    chunks = data.frame(),
                                    question = "",
                                    context = list(),
                                    default_status = "insufficient") {
  ranked <- if (exists("ra_ai_rank_tools", mode = "function")) {
    ra_ai_rank_tools(question, context)
  } else {
    list()
  }
  ranked_ids <- if (length(ranked) > 0) {
    vapply(ranked, function(x) x$id, character(1))
  } else {
    character()
  }
  model_ids <- if (exists("ra_ai_validate_tool_ids", mode = "function")) {
    ra_ai_validate_tool_ids(parsed$recommended_tool_ids)
  } else {
    character()
  }
  tool_ids <- unique(c(model_ids, ranked_ids))
  cases <- if (exists("ra_ai_match_cases", mode = "function")) {
    ra_ai_match_cases(question)
  } else {
    list()
  }
  explanations <- ra_ai_chr_list(parsed$possible_explanations)
  if (length(explanations) == 0 && exists("ra_ai_case_explanations",
                                          mode = "function")) {
    explanations <- ra_ai_case_explanations(cases)
  }
  checks <- ra_ai_chr_list(parsed$suggested_checks)
  if (length(checks) == 0 && exists("ra_ai_case_checks", mode = "function")) {
    checks <- ra_ai_case_checks(cases)
  }
  if (length(checks) == 0 && length(tool_ids) > 0) {
    checks <- paste("Inspect the", tool_ids, "tab")
  }
  known <- as.character(parsed$what_is_known %||% "")[1]
  if (!nzchar(known) && !is.null(chunks) && nrow(chunks) > 0) {
    known <- paste(
      "Local indexed passages were retrieved. They are not a diagnosis",
      "of the loaded run."
    )
  }
  limits <- as.character(parsed$limitations %||% "")[1]
  if (!nzchar(limits)) {
    limits <- paste(
      "Guidance only. The assistant cannot see numerical results, write",
      "files, or execute code. Do not treat one symptom as one cause."
    )
  }
  follow <- as.character(parsed$follow_up_question %||% "")[1]
  status <- ra_ai_validate_evidence_status(parsed$evidence_status, default_status)
  if ((is.null(chunks) || nrow(chunks) == 0) && identical(status, "supported")) {
    status <- "insufficient"
  }
  list(
    what_is_known = known,
    possible_explanations = explanations,
    suggested_checks = checks,
    recommended_tool_ids = tool_ids,
    limitations = limits,
    evidence_status = status,
    follow_up_question = follow
  )
}

ra_ai_with_structured <- function(result, extra) {
  for (nm in names(extra)) result[[nm]] <- extra[[nm]]
  if (length(extra$recommended_tool_ids) > 0 &&
      exists("ra_ai_tools_as_nav", mode = "function")) {
    result$nav_targets <- ra_ai_merge_nav(
      result$nav_targets %||% list(),
      ra_ai_tools_as_nav(extra$recommended_tool_ids)
    )
  }
  result
}

ra_ai_plain_answer <- function(text) {
  list(
    answer = as.character(text)[1],
    claim_kinds = "insufficient_evidence",
    citations = list(),
    nav_targets = list(),
    what_is_known = "",
    possible_explanations = character(),
    suggested_checks = character(),
    recommended_tool_ids = character(),
    limitations = "Guidance only.",
    evidence_status = "insufficient",
    follow_up_question = ""
  )
}

ra_ai_wants_simulation_numbers <- function(question) {
  q <- tolower(as.character(question)[1])
  grepl("what did this (particular )?simulation prove", q) ||
    grepl("what is the biomass of", q) ||
    grepl("how many tonnes", q) ||
    grepl("interpret (this|the) (plot|figure|output)", q) ||
    grepl("what does this plot", q)
}

ra_ai_expand_retrieval_query <- function(question, resolved = NULL) {
  extra <- character()
  if (!is.null(resolved)) {
    extra <- c(extra, resolved$codes, resolved$long_names, resolved$labels)
  }
  if (isTRUE(ra_ai_is_calibration_investigation(question))) {
    extra <- c(extra, "mortality", "recruitment", "predation", "growth",
               "fishing", "biomass", "mum", "pPREY", "KDENR", "ResN",
               "StructN", "consumption", "collapse")
  }
  extra <- unique(as.character(extra))
  extra <- extra[nzchar(extra)]
  if (length(extra) == 0) return(as.character(question)[1])
  paste(c(as.character(question)[1], extra), collapse = " ")
}

ra_ai_retrieval_is_weak <- function(chunks) {
  if (is.null(chunks) || nrow(chunks) == 0) return(TRUE)
  if ("score" %in% names(chunks)) {
    sc <- suppressWarnings(as.numeric(chunks$score))
    sc <- sc[is.finite(sc)]
    if (length(sc) == 0 || max(sc) < 0.05) return(TRUE)
  }
  FALSE
}

ra_ai_investigation_citations <- function(chunks) {
  if (is.null(chunks) || nrow(chunks) == 0) return(list())
  keys <- c("mortality", "mum", "pprey", "kdenr", "recruit", "predat",
            "growth", "resn", "structn", "clearance", "consumption",
            "collapse", "declin")
  keep <- vapply(seq_len(nrow(chunks)), function(i) {
    blob <- tolower(paste(
      chunks$text[i], chunks$section[i], chunks$title[i], sep = " "
    ))
    any(vapply(keys, function(k) grepl(k, blob, fixed = TRUE), logical(1)))
  }, logical(1))
  idx <- which(keep)
  if (length(idx) == 0) idx <- seq_len(min(3L, nrow(chunks)))
  idx <- idx[seq_len(min(6L, length(idx)))]
  lapply(idx, function(i) ra_ai_citation_from_row(chunks[i, ]))
}

ra_ai_calibration_guidance <- function(question, resolved = NULL) {
  label <- "the group"
  if (!is.null(resolved) && length(resolved$labels) > 0) {
    label <- paste(resolved$labels, collapse = ", ")
  }
  paste(
    paste0(
      "This is guidance, not a diagnosis. I cannot see the numerical results ",
      "of the loaded run. The notes below are hypotheses for why ",
      label, " might be collapsing or crashing — open the existing tabs to ",
      "check each process."
    ),
    "",
    "Likely mechanisms (hypotheses) and what to check:",
    "",
    "1. Predation — predators may be removing more biomass.",
    "   Check Predation (diet of predators and of this group) and Mortality (predation stack).",
    "",
    "2. Starvation / food limitation — intake too low (mum/C, pPREY, prey biomass).",
    "   Check Consumption, Feeding (pPREY and gape), and Predation (what the group eats).",
    "",
    "3. Recruitment failure — YOY, KDENR, or the Beverton-Holt curve.",
    "   Check Recruitment (YOY and SSB).",
    "",
    "4. Fishing — harvest or catchability increased.",
    "   Check Catch and Mortality (fishing).",
    "",
    "5. Growth / condition — ResN or StructN drop.",
    "   Check Overview (problem cohorts) and Compare (biomass and age structure).",
    "",
    "6. Oxygen / temperature / physics — habitat stress.",
    "   Check Physics.",
    "",
    "7. Spatial distribution — biomass moved into poor boxes or layers.",
    "   Check Spatial (by depth layer).",
    "",
    "The buttons below only switch to existing Reactive Atlantis tabs; they do not diagnose the run.",
    sep = "\n"
  )
}

ra_ai_investigation_answer <- function(question, chunks, resolved = NULL) {
  nav <- ra_ai_validate_nav(ra_ai_investigation_nav_ids())
  nav <- ra_ai_merge_nav(nav, ra_ai_validate_nav(ra_ai_suggest_nav(question)))
  citations <- ra_ai_investigation_citations(chunks)
  answer <- ra_ai_calibration_guidance(question, resolved)
  if (length(citations) > 0) {
    secs <- unique(vapply(citations, function(c) {
      paste(c$section %||% "", c$title %||% "")
    }, character(1)))
    secs <- trimws(secs)
    secs <- secs[nzchar(secs)]
    if (length(secs) > 0) {
      answer <- paste(
        answer,
        "",
        paste(
          "Local manuals mention these processes (mortality, mum/C, pPREY,",
          "KDENR/recruitment, growth). See sources:",
          paste0(paste(secs, collapse = "; "), ".")
        ),
        sep = "\n"
      )
    }
  }
  if (is.null(chunks)) chunks <- data.frame()
  extra <- ra_ai_structured_fields(
    list(), chunks, question, list(),
    default_status = if (length(citations) > 0) "partial" else "insufficient"
  )
  extra$possible_explanations <- unique(c(
    extra$possible_explanations,
    "Predation may have increased.",
    "Intake or gape/pPREY may be limiting.",
    "Recruitment (YOY/SSB/KDENR) may have failed.",
    "Fishing may have increased.",
    "Condition (ResN/StructN) may have dropped.",
    "Physics or spatial distribution may have changed."
  ))
  extra$suggested_checks <- unique(c(
    extra$suggested_checks,
    "Overview health flags",
    "Compare biomass",
    "Predation and Mortality",
    "Recruitment YOY/SSB",
    "Consumption and Feeding",
    "Spatial and Physics"
  ))
  extra$limitations <- paste(
    "This is guidance, not a diagnosis. One symptom does not identify",
    "one cause. The assistant cannot see numerical results of the loaded run."
  )
  extra$follow_up_question <- extra$follow_up_question
  if (!nzchar(extra$follow_up_question)) {
    extra$follow_up_question <- "Which of those processes looks most off on the plots?"
  }
  ra_ai_with_structured(
    list(
      ok = TRUE,
      blocked = FALSE,
      reason = "answered",
      answer = answer,
      claim_kinds = "hypothesis",
      citations = citations,
      nav_targets = nav,
      retrieved = chunks
    ),
    extra
  )
}

##' Full question pipeline. generator() may replace Ollama in tests.
ra_ai_answer <- function(question,
                         history = list(),
                         context = list(),
                         config = ra_ai_config(),
                         generator = NULL,
                         retrieved = NULL) {
  scope <- ra_ai_scope_check(question, context)
  if (!isTRUE(scope$ok)) {
    status <- if (identical(scope$reason, "out_of_scope")) {
      "out_of_scope"
    } else {
      "insufficient"
    }
    return(ra_ai_with_structured(
      list(
        ok = TRUE,
        blocked = TRUE,
        reason = scope$reason,
        answer = scope$message,
        claim_kinds = "insufficient_evidence",
        citations = list(),
        nav_targets = list(),
        retrieved = data.frame()
      ),
      list(
        what_is_known = "",
        possible_explanations = character(),
        suggested_checks = character(),
        recommended_tool_ids = character(),
        limitations = "The assistant stays read-only and in-domain.",
        evidence_status = status,
        follow_up_question = ""
      )
    ))
  }

  if (ra_ai_wants_simulation_numbers(question)) {
    nav <- ra_ai_validate_nav(ra_ai_suggest_nav(question))
    extra <- ra_ai_structured_fields(list(), data.frame(), question, context,
                                     default_status = "insufficient")
    extra$what_is_known <- "The assistant cannot see numerical results of the loaded run."
    extra$limitations <- "No plot pixels or NetCDF values are available to the drawer."
    extra$follow_up_question <- "Which tab are you looking at, and what do you observe?"
    return(ra_ai_with_structured(
      list(
        ok = TRUE,
        blocked = FALSE,
        reason = "no_simulation_data",
        answer = ra_ai_no_sim_data_message(),
        claim_kinds = "insufficient_evidence",
        citations = list(),
        nav_targets = nav,
        retrieved = data.frame()
      ),
      extra
    ))
  }

  groups <- context$groups %||% context$group_table
  resolved <- ra_ai_resolve_group_codes(question, groups)
  if (!is.null(context$selected_group) &&
      nzchar(as.character(context$selected_group)[1])) {
    extra <- ra_ai_resolve_group_codes(context$selected_group[1], groups)
    resolved$codes <- unique(c(resolved$codes, extra$codes))
    resolved$labels <- unique(c(resolved$labels, extra$labels))
    resolved$long_names <- unique(c(resolved$long_names, extra$long_names))
  }

  if (is.null(retrieved)) {
    q_ret <- ra_ai_expand_retrieval_query(question, resolved)
    retrieved <- ra_ai_retrieve(q_ret, config = config)
    chunks0 <- retrieved$chunks
    if (isTRUE(ra_ai_is_calibration_investigation(question)) &&
        (is.null(chunks0) || nrow(chunks0) == 0)) {
      retrieved <- ra_ai_retrieve(
        "mortality mum pPREY KDENR recruitment predation growth collapse",
        config = config
      )
    }
  }
  chunks <- retrieved$chunks
  if (is.null(chunks)) chunks <- data.frame()

  if (isTRUE(ra_ai_is_calibration_investigation(question))) {
    return(ra_ai_investigation_answer(question, chunks, resolved))
  }

  allowed_nav <- unname(lapply(ra_ai_nav_catalog(), function(x) {
    list(id = x$id, title = x$title)
  }))
  passages <- ra_ai_format_passages(chunks, max_chars = config$max_chunk_chars)
  if (!nzchar(passages)) {
    passages <- "(No passages were retrieved from the local index.)"
  }
  user_prompt <- ra_ai_build_user_prompt(question, passages, context, allowed_nav)

  messages <- list(list(role = "system", content = ra_ai_system_prompt()))
  if (length(history) > 0) {
    keep <- tail(history, config$max_history)
    for (turn in keep) {
      if (!is.null(turn$role) && !is.null(turn$content)) {
        messages[[length(messages) + 1]] <- list(
          role = as.character(turn$role)[1],
          content = as.character(turn$content)[1]
        )
      }
    }
  }
  messages[[length(messages) + 1]] <- list(role = "user", content = user_prompt)

  raw <- tryCatch({
    if (is.function(generator)) {
      generator(messages, config)
    } else {
      ra_ollama_chat(messages, config)
    }
  }, error = function(e) e)

  if (inherits(raw, "error")) {
    extra <- ra_ai_structured_fields(list(), chunks, question, context,
                                     default_status = "insufficient")
    extra$limitations <- "Local model unreachable. Existing tabs still work."
    return(ra_ai_with_structured(
      list(
        ok = FALSE,
        blocked = FALSE,
        reason = "ollama_error",
        answer = if (grepl("timeout|timed out", raw$message, ignore.case = TRUE)) {
          paste(
            "The local language model did not answer within",
            config$timeout_s, "seconds (RA_OLLAMA_TIMEOUT_S).",
            "It is reachable but slow; a smaller model or a longer timeout may help.",
            "Existing calibration tools are unaffected."
          )
        } else {
          paste(
            "I could not reach the local language model.",
            raw$message,
            "Existing calibration tools are unaffected."
          )
        },
        claim_kinds = "insufficient_evidence",
        citations = list(),
        nav_targets = ra_ai_validate_nav(ra_ai_suggest_nav(question)),
        retrieved = chunks
      ),
      extra
    ))
  }

  parsed <- ra_ai_parse_json_object(raw)
  if (is.null(parsed)) {
    parsed <- list(
      answer = as.character(raw)[1],
      claim_kinds = list(),
      citations = list(),
      nav_targets = list()
    )
  }

  citations <- ra_ai_validate_citations(parsed$citations, chunks)
  model_nav <- ra_ai_validate_nav(parsed$nav_targets)
  hint_nav <- ra_ai_validate_nav(ra_ai_suggest_nav(question))
  extra <- ra_ai_structured_fields(parsed, chunks, question, context,
                                   default_status = if (nrow(chunks) > 0)
                                     "partial" else "insufficient")
  tool_nav <- if (exists("ra_ai_tools_as_nav", mode = "function")) {
    ra_ai_tools_as_nav(extra$recommended_tool_ids)
  } else {
    list()
  }
  nav <- ra_ai_merge_nav(model_nav, hint_nav, tool_nav)

  kinds <- unlist(parsed$claim_kinds)
  if (length(kinds) == 0) kinds <- character()
  allowed_kinds <- c("documented_fact", "code_verified", "calibration_example",
                     "hypothesis", "insufficient_evidence")
  kinds <- intersect(as.character(kinds), allowed_kinds)
  if (nrow(chunks) > 0 && any(chunks$source_type == "calibration") &&
      "calibration_example" %in% kinds == FALSE &&
      grepl("calibrat", tolower(question))) {
    kinds <- unique(c(kinds, "calibration_example"))
  }
  if (length(kinds) == 0) kinds <- "documented_fact"

  answer_txt <- as.character(parsed$answer %||% "")[1]
  if (!nzchar(answer_txt)) {
    answer_txt <- "The local model returned an empty answer."
  }
  oos_mark <- "I only answer questions about the Atlantis ecosystem model"
  if (grepl(oos_mark, answer_txt, fixed = TRUE)) {
    answer_txt <- paste(
      "I do not have enough indexed material to be specific, but this is an",
      "Atlantis / Reactive Atlantis question. Use the existing tabs below or",
      "ask about a process, parameter, or tool."
    )
    kinds <- unique(c(kinds, "insufficient_evidence"))
  }

  ## If calibration evidence was used, make the example framing visible.
  if ("calibration_example" %in% kinds &&
      !grepl("example", answer_txt, ignore.case = TRUE)) {
    answer_txt <- paste(
      answer_txt,
      "Treat any calibration note above as an example from a particular case,",
      "not a universal parameter rule."
    )
  }

  ra_ai_with_structured(
    list(
      ok = TRUE,
      blocked = FALSE,
      reason = "answered",
      answer = answer_txt,
      claim_kinds = kinds,
      citations = citations,
      nav_targets = nav,
      retrieved = chunks,
      raw = raw
    ),
    extra
  )
}

ra_ai_merge_nav <- function(...) {
  items <- list(...)
  out <- list()
  seen <- character()
  for (group in items) {
    for (n in group) {
      id <- as.character(n$id)[1]
      if (!nzchar(id) || id %in% seen) next
      seen <- c(seen, id)
      out[[length(out) + 1]] <- n
    }
  }
  out
}

##' Escape model text for HTML; allow only simple newlines.
ra_ai_safe_html <- function(text) {
  txt <- as.character(text)[1]
  if (is.na(txt)) txt <- ""
  esc <- htmltools::htmlEscape(txt)
  esc <- gsub("\r\n", "\n", esc, fixed = TRUE)
  esc <- gsub("\n\n+", "</p><p>", esc)
  esc <- gsub("\n", "<br/>", esc, fixed = TRUE)
  paste0("<p>", esc, "</p>")
}
