# ReactiveAtlantis pre-release validation

Independent audit of ReactiveAtlantis before public release. It covers scientific correctness, software reliability, and visual communication.

| Item | Value |
|---|---|
| Release candidate | PR #4 `cursor/ask-atlantis-assistant-b4d2`, head `92974ba48371352576558f16907e6703db60a499` (open, **draft**, 20 commits, 99 files, +18,001 / −477; mergeable) |
| Default branch | `master` at `bb378884ef3c15d2a875bd30809a11d1f31aea05` |
| Audit branch | `audit/pre-release-validation` (created from PR #4 head; local only, not pushed) |
| Atlantis reference source | `~/whacamole_nesp/BethAtlantis_fix/atlantis` (git `16792e4`), the build that produced the NESP test outputs |
| Audit host | estigia: Dell Precision 7910, 2 × Xeon E5-2630 v4 (40 threads), 62 GB RAM, Ubuntu 26.04.1 |
| Started | 2026-10-25 |

## Status by phase

| Phase | Status |
|---|---|
| 1 Discovery | Done. `feature_inventory.md`, `defects.md` |
| 2 Test design | Done. `audit_plan.md`, `test_dataset_manifest.md`, `scientific_reference_matrix.md` |
| 3 Automated tests | Done. `phase3_automated_tests.md` |
| 4 Scientific validation | Done, except catch (no real fished run). `phase4_scientific_validation.md` |
| 5 Interactive and visual | Done. 35 states at 3 sizes; science and design reviews reconciled. `visual_design/` |
| 6 Scientific Guide | Done. `ai_guide_validation.md` |
| 7 Performance | Done, on a loaded machine. `performance_report.md` |
| 8 Remediation | Done for A–G and the software fixes. `phase8_remediation.md` |
| 9 Regression | Done. R CMD check: 1 NOTE; 1,319 tests pass; NESP checks re-run; "after" screenshots, performance and Guide benchmark |
| 10 Release decision | **NO-GO** until 10 open P1 items are fixed or accepted, then CONDITIONAL GO (`release_readiness.md`) |

## Environment

- R 4.5.2 and Google Chrome 156, installed system-wide by the owner.
- CRAN plotly 4.12.1, replacing Ubuntu's broken package (S16).
- Ollama 0.40.11 with `qwen3.5:4b` and `nomic-embed-text`.
- Large working data (NetCDF, index, synthetic files) is kept outside the repository, in `~/whacamole_nesp/ra_audit_data/`.

## Screenshots

The screenshots referenced in these reports, under `visual_design/screenshots/` and `evidence/phase3/launch/` (105 PNG, 26 MB), are **not in the repository**. They are kept on the audit machine in `~/whacamole_nesp/ra_audit_data/screenshots/` with the same subfolders. Their timing and app-log files are in `evidence/screenshot_runs/`.

## Files

- `audit_plan.md`: scope, phases, acceptance criteria, Phase 1 risk assessment
- `feature_inventory.md`: every tab, control, input file and computation, with code locations
- `scientific_reference_matrix.md`: each diagnostic against its Atlantis definition
- `test_dataset_manifest.md`: datasets A–F and where they come from
- `defects.md`: defect register (confirmed / suspected / refuted)
- `release_readiness.md`: final position and open items
- `phase3_automated_tests.md`, `phase4_scientific_validation.md`, `phase8_remediation.md`, `performance_report.md`, `ai_guide_validation.md`
- `visual_design/`: visualisation audit (inventory only; screenshots blocked)
- `evidence/`: command outputs supporting the findings
