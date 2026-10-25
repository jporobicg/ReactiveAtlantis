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
| 1 Discovery | **Done (static).** See `feature_inventory.md`, `defects.md` and the risk assessment in `audit_plan.md` |
| 2 Test design | Drafted: `audit_plan.md`, `test_dataset_manifest.md`, `scientific_reference_matrix.md`. Awaiting approval |
| 3–10 | Not started. **Blocked:** R and a browser are not installed on estigia (see below) |

## Environment blocker

estigia has no R, no Chrome/Chromium and no passwordless sudo, so nothing in this package has been executed yet. Every finding so far comes from reading code and the Atlantis source, or from inspecting real NESP output files with Python netCDF4. A finding is marked **CONFIRMED** only when it was checked against Atlantis source or real output.

## Files

- `audit_plan.md`: scope, phases, acceptance criteria, Phase 1 risk assessment
- `feature_inventory.md`: every tab, control, input file and computation, with code locations
- `scientific_reference_matrix.md`: each diagnostic against its Atlantis definition
- `test_dataset_manifest.md`: datasets A–F and where they come from
- `defects.md`: defect register (confirmed / suspected / refuted)
- `release_readiness.md`: interim position
- `visual_design/`: visualisation audit (inventory only; screenshots blocked)
- `evidence/`: command outputs supporting the findings
