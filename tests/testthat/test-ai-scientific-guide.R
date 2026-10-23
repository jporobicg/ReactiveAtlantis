## Scientific Guide extensions of the existing Ask Atlantis stack.

root <- source_ask_atlantis()

test_that("tool registry matches nav catalog and rejects invented ids", {
  tools <- ra_ai_tool_catalog()
  nav <- ra_ai_nav_catalog()
  expect_identical(sort(names(tools)), sort(names(nav)))
  expect_true("predation" %in% ra_ai_validate_tool_ids(c("predation", "skill_assessment", "growth.pp")))
  expect_false("skill_assessment" %in% ra_ai_validate_tool_ids("skill_assessment"))
  expect_false("growth.pp" %in% ra_ai_validate_tool_ids("growth.pp"))
  expect_false("SuperCalibrator" %in% ra_ai_validate_tool_ids("SuperCalibrator"))
})

test_that("deterministic ranking hits expected tools before any LLM", {
  r1 <- ra_ai_rank_tools("Where do I inspect predation?")
  expect_equal(r1[[1]]$id, "predation")
  r2 <- ra_ai_rank_tools("What does the Atlantis parameter KLP represent?")
  ids <- vapply(r2, function(x) x$id, character(1))
  expect_true("feeding" %in% ids[seq_len(min(3L, length(ids)))])
  r3 <- ra_ai_rank_tools("Where are temperature and nitrate shown?")
  expect_equal(r3[[1]]$id, "physics")
})

test_that("session context is read-only metadata and isolated", {
  tmp <- tempfile("grp.csv")
  writeLines("code,name\nCOD,cod", tmp)
  project_a <- list(
    paths = list(groups = tmp, biomindx = tempfile()),
    run_labels = "run-A",
    grp = data.frame(code = "COD", name = "cod", stringsAsFactors = FALSE)
  )
  project_b <- list(
    paths = list(groups = tempfile()),
    run_labels = "run-B",
    grp = data.frame(code = "FDO", name = "dogfish", stringsAsFactors = FALSE)
  )
  ctx_a <- ra_ai_session_context(list(ra_page = "Compare"), project_a)
  ctx_b <- ra_ai_session_context(list(ra_page = "Spatial"), project_b)
  expect_true(isTRUE(ctx_a$file_availability$groups))
  expect_false(isTRUE(ctx_a$file_availability$dietcheck))
  expect_false(any(grepl("NetCDF|DietCheck matrix", paste(ctx_a, collapse = " "))))
  expect_identical(ctx_a$run_labels, "run-A")
  expect_identical(ctx_b$run_labels, "run-B")
  expect_false(identical(ctx_a$available_roles, ctx_b$available_roles) &&
                 identical(ctx_a$page, ctx_b$page))
  expect_identical(ctx_a$page, "Compare")
  expect_identical(ctx_b$page, "Spatial")
})

test_that("structured answers reject unknown tools and non-retrieved citations", {
  cfg <- build_fixture_index(embed = FALSE)
  with_mock_ollama({
    ans <- ra_ai_answer("What does the Atlantis parameter KLP represent?",
                        config = cfg)
    expect_true(ans$evidence_status %in% c("supported", "partial", "insufficient"))
    expect_true(all(ans$recommended_tool_ids %in% names(ra_ai_nav_catalog())))
    expect_false("skill_assessment" %in% ans$recommended_tool_ids)
    if (length(ans$citations) > 0) {
      expect_true(all(vapply(ans$citations, function(c) {
        c$chunk_id %in% ans$retrieved$chunk_id ||
          c$source_id %in% ans$retrieved$source_id
      }, logical(1))))
    }
  })
  forged <- ra_ai_validate_citations(
    list(list(chunk_id = "chk_not_real", source_id = "not-a-real-paper")),
    data.frame(chunk_id = "chk_abc", source_id = "fixture-manuals",
               stringsAsFactors = FALSE)
  )
  expect_equal(length(forged), 0)
})

test_that("answers include structured fields and never raw JSON in HTML", {
  cfg <- build_fixture_index(embed = FALSE)
  with_mock_ollama({
    ans <- ra_ai_answer("Where do I inspect predation?", config = cfg)
    expect_true(!is.null(ans$what_is_known))
    expect_true(!is.null(ans$possible_explanations))
    expect_true(!is.null(ans$suggested_checks))
    expect_true(!is.null(ans$limitations))
    expect_true(!is.null(ans$follow_up_question))
    html <- as.character(ra_ai_render_message(list(
      content = ans$answer,
      citations = ans$citations,
      nav_targets = ans$nav_targets,
      claim_kinds = ans$claim_kinds,
      what_is_known = ans$what_is_known,
      possible_explanations = ans$possible_explanations,
      suggested_checks = ans$suggested_checks,
      limitations = ans$limitations,
      evidence_status = ans$evidence_status,
      follow_up_question = ans$follow_up_question,
      failed = FALSE
    )))
    expect_false(grepl('"recommended_tool_ids"', html, fixed = TRUE))
    expect_true(grepl("Open Predation", html, fixed = TRUE))
  })
})

test_that("ten reviewed case cards have competing hypotheses and real tools", {
  cases <- ra_ai_case_catalog()
  expect_gte(length(cases), 10)
  known <- names(ra_ai_nav_catalog())
  for (cs in cases) {
    expect_gte(length(cs$hypotheses), 1)
    expect_true(nzchar(cs$what_not_to_conclude))
    expect_true(all(unlist(cs$tool_ids) %in% known))
    blob <- tolower(paste(cs$what_not_to_conclude, cs$interpretation, collapse = " "))
    expect_false(grepl("always increase|always decrease|must raise mum", blob))
  }
})

test_that("source registry has required fields and counts", {
  skip_if_not_installed("jsonlite")
  reg <- ra_ai_source_registry()
  expect_gte(length(reg), 10)
  need <- c("source_id", "title", "authors", "year", "type", "url", "doi",
            "local_path", "authority_tier", "rights", "licence",
            "access_method", "retrieved_at", "revision", "atlantis_version",
            "checksum", "ingest_status", "extraction_quality", "review_status")
  expect_true(all(need %in% names(reg[[1]])))
  counts <- ra_ai_registry_counts(reg)
  expect_gt(counts$indexed, 0)
  expect_gt(counts$linked_only, 0)
  expect_gt(counts$pending, 0)
  st <- vapply(reg, function(s) s$review_status, character(1))
  expect_true(all(st %in% c("verified", "pending", "excluded")))
})

test_that("retrieval returns at most six passages and knowledge is indexed", {
  cfg <- build_fixture_index(embed = FALSE)
  chunks <- ra_ai_retrieve("pPREY KLP biomass declining", config = cfg)$chunks
  expect_lte(nrow(chunks), 6)
  diag <- ra_ai_diagnose(cfg$index_dir)
  types <- as.character(diag$sources$source_type)
  expect_true("knowledge" %in% types)
})

test_that("embedding model change clears stored vectors", {
  skip_if_not_installed("RSQLite")
  cfg <- build_fixture_index(embed = TRUE)
  n0 <- ra_ai_diagnose(cfg$index_dir)$n_embeddings
  expect_gt(n0, 0)
  cfg$embed_model <- "other-embed-model"
  with_mock_ollama({
    ra_ai_index(sources_config = file.path(root, "inst", "ai", "sources.json"),
                index_dir = cfg$index_dir, embed = TRUE, config = cfg)
  })
  con <- ra_ai_store_open(cfg$index_dir, create = FALSE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_identical(ra_ai_store_meta_get(con, "embed_model"), "other-embed-model")
})

test_that("evaluation set covers sixty or more cases", {
  skip_if_not_installed("jsonlite")
  spec <- jsonlite::fromJSON(
    file.path(root, "inst", "ai", "evaluation", "questions.json"),
    simplifyVector = FALSE
  )
  expect_gte(length(spec$questions), 60)
  expect_lte(length(spec$questions), 100)
})

test_that("example questions and drawer title are the Scientific Guide", {
  qs <- ra_ai_example_questions()
  expect_gte(length(qs), 5)
  ui <- paste(as.character(assistant_ui("assistant")), collapse = " ")
  expect_true(grepl("Atlantis Scientific Guide", ui, fixed = TRUE))
  expect_true(grepl("ra-example-q", ui, fixed = TRUE))
  expect_false(grepl('"recommended_tool_ids"', ui, fixed = TRUE))
})
