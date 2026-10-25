# Audit plan and Phase 1 risk assessment

## Scope

- **Release candidate:** PR #4 head `92974ba`, compared against `master` `bb37888`.
- **What is audited:** all Shiny tabs, the legacy exported functions, packaging, the tests, and the optional Scientific Guide.
- **Also audited:** the visual design (see `visual_design/`).

## Working rules

- Scientific behaviour is not changed without the owner's approval. A proposed scientific fix goes in `defects.md` with its reference and the size of the discrepancy.
- Software-only fixes are made with a regression test, on `audit/pre-release-validation` only.
- Nothing is pushed or merged without approval. No NetCDF files are committed.

## Two reviewers per figure

Each plot gets two separate reviews:

1. **Science reviewer.** Says what the figure represents: the quantity, units, aggregation, and whether the number is right against the reference.
2. **Design reviewer.** Judges how well the figure communicates that quantity.

A redesign is approved only when both reviewers agree. If they disagree, the disagreement and its resolution are recorded in `visual_design/proposed_improvements.md`.

A plot that fails the science review fails the audit, whatever its design score.

## Phase 1 risk assessment (from static review)

| Risk | Rating | Basis |
|---|---|---|
| Predator attribution in mortality | **Critical** | D01, confirmed. Every predator-by-prey mortality view is transposed. |
| Expected predation pressure presented as Atlantis-equivalent | High | D05–D10. Several terms Atlantis applies are missing; the existing tests check only RA's own algebra. |
| Recruitment expectation | High | D03 (spawn sign) and D04 (SSB label, unit mixing). |
| Biomass of epibenthos and multi-cohort pools | High | D11, D12 (area factor; dropped cohorts). |
| Catch by fleet | High | D14 (fallback double-counting, numbers labelled as tonnes). Validation is also blocked by the lack of real fished data. |
| Spatial and depth aggregation | High | D18, D19 (intensive/extensive mixing; volume construction). The layer layout itself matches NESP (R-01 refuted). |
| Test independence | High | S02. The tests encode current behaviour, including D01, so they cannot detect scientific errors. |
| Install path | High | S01. The documented install gives master, not the release candidate. |
| Resource handling | Medium | S03. Handles are never closed and the cache has no bound; this matters with 1.5–2.7 GB NESP outputs. |
| Scientific Guide | Medium | Safety is regex only (S07, S12); the answer rubric is never scored. Needs a live Ollama to test. |

## Phases 2–10 (planned)

| Phase | Plan | Needs |
|---|---|---|
| 2 Test design | Build datasets C, D and E (`test_dataset_manifest.md`). Write Python reference calculations from Atlantis definitions, independent of RA, for biomass, pools, layers, box mapping, spawn, BH, Schoener and Holling. Acceptance: relative tolerance 1e-6 for exact re-computations, 1e-3 where the RA and reference float paths differ, and a stated tolerance for anything model-derived. | Owner approval of this plan |
| 3 Automated | Clean install from GitHub (master and the PR ref) and from local source; R CMD check; the existing testthat suite. | R (blocked) |
| 4 Scientific | Reference vs RA on sets A, B and C; cross-diagnostic checks (`cross_diagnostic_checks.md`). | R |
| 5 Interactive | shinytest2/chromote through every control; screenshots of every plot in normal and alternative states (`visual_design/`). | R + headless Chromium |
| 6 AI Guide | Ollama + models; 50+ question benchmark; injection and action tests. | Ollama (not installed) |
| 7 Performance | Load and render timings on NESP 1.5–2.7 GB outputs; memory over repeated interactions; file-handle counts. The 18 Atlantis runs currently on estigia will distort timings, so benchmarks wait until they finish or are reported with that load stated. | R |
| 8–10 | Fixes in priority order, full regression, release decision. | Approvals |
