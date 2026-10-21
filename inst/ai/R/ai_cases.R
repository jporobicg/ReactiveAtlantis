## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~ Scientific Guide: case cards and Q&A        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ra_ai_read_knowledge_json <- function(rel) {
  path <- ra_ai_ai_file("knowledge", rel)
  if (!file.exists(path) || !ra_ai_need_pkg("jsonlite")) return(list())
  spec <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                   error = function(e) NULL)
  if (is.null(spec)) return(list())
  spec
}

ra_ai_case_catalog <- function() {
  spec <- ra_ai_read_knowledge_json("cases.json")
  as.list(spec$cases %||% list())
}

ra_ai_qa_catalog <- function() {
  spec <- ra_ai_read_knowledge_json("qa.json")
  as.list(spec$items %||% list())
}

ra_ai_concept_catalog <- function() {
  spec <- ra_ai_read_knowledge_json("concepts.json")
  as.list(spec$concepts %||% list())
}

##' Rank reviewed case cards for a question.
ra_ai_match_cases <- function(question, cases = ra_ai_case_catalog()) {
  q <- tolower(as.character(question)[1])
  if (!nzchar(q) || length(cases) == 0) return(list())
  scored <- lapply(cases, function(cs) {
    kws <- as.character(cs$keywords %||% character())
    hits <- sum(vapply(kws, function(k) grepl(k, q, fixed = TRUE), logical(1)))
    if (hits <= 0) return(NULL)
    cs$score <- hits
    cs
  })
  scored <- Filter(Negate(is.null), scored)
  if (length(scored) == 0) return(list())
  scored[order(vapply(scored, function(x) -as.numeric(x$score), numeric(1)))]
}

ra_ai_case_explanations <- function(cases) {
  out <- character()
  for (cs in cases) {
    hyps <- cs$hypotheses %||% list()
    for (h in hyps) {
      lab <- as.character(h$id %||% "hypothesis")[1]
      exp <- as.character(h$expected %||% "")[1]
      if (nzchar(exp)) out <- c(out, paste0(lab, ": ", exp))
    }
  }
  unique(out)
}

ra_ai_case_checks <- function(cases) {
  unique(unlist(lapply(cases, function(cs) as.character(cs$checks %||% character()))))
}

ra_ai_source_registry <- function() {
  path <- ra_ai_ai_file("source_registry.json")
  if (!file.exists(path) || !ra_ai_need_pkg("jsonlite")) return(list())
  spec <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE),
                   error = function(e) NULL)
  as.list(spec$sources %||% list())
}

ra_ai_registry_counts <- function(reg = ra_ai_source_registry()) {
  st <- vapply(reg, function(s) as.character(s$ingest_status %||% "")[1],
               character(1))
  list(
    indexed = sum(st == "indexed"),
    linked_only = sum(st == "linked-only"),
    pending = sum(st == "pending"),
    excluded = sum(st == "excluded"),
    total = length(reg)
  )
}
