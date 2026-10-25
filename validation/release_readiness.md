# Release readiness: final position (Phase 10, 2026-10-26)

## Update after round 2 (2026-10-26, branch `ea6de2d`)

The owner asked for the remaining P1 items to be fixed, accepted Catch as "not validated", and did not accept predation pressure as a screening index, so it was rebuilt.

**R CMD check:** 1 NOTE; **1,340 tests pass**, 0 failed, 0 skipped, browser tests included.

| Item | Status |
|---|---|
| D05 predation pressure | **Rebuilt** per cohort and cell from the Atlantis equations; exact against the reference case; 0.3–4× Atlantis realised system-wide (was 35×). Labelled approximate (vertical migration between snapshots, no Q10). Proposed: close it as a documented limitation, or investigate further (day/night distribution, Q10 variants). |
| D14 Catch | Fleet mapping and units fixed; **accepted by the owner as "not validated"** (the tab says so) |
| D17, D18, D19, N5, N6, N7 | **Fixed**, with tests |
| S02 tests | Partly fixed: 13 tests now use hand-computed references; most older tests still rely on synthetic fixtures |
| S25 Scientific Guide reliability | **Open, not accepted.** About 1 in 3 answers contains a false scientific claim. Options: ship it disabled (it is off by default) and documented as experimental, remove it from this release, or improve retrieval and prompts before release |

**Owner decisions (2026-10-26):**

- The Scientific Guide ships **off by default and marked experimental** (S25 accepted as a labelled limitation).
- Predation pressure is **closed as labelled approximate** (D05 accepted).

**Recommendation: CONDITIONAL GO.** The accepted limitations are:

- Catch is not validated;
- the Scientific Guide is experimental and off by default;
- expected predation pressure is approximate.

S02 (most older tests use synthetic fixtures) remains a test-quality gap, not a correctness defect.

---



**Recommendation: NO-GO for the release candidate as audited** (PR #4, `92974ba`).

**For the remediated branch** (`audit/pre-release-validation`, `ce1f9f6`), the recommendation is NO-GO until the open P1 items below are either fixed or **explicitly accepted by the owner** as labelled limitations. If they are accepted, the branch qualifies as a **CONDITIONAL GO**.

## What changed in Phase 8

All 7 P0 defects are resolved.

**Fixed and confirmed on NESP against independent references:**

| ID | Item | Result |
|---|---|---|
| D01 | Predator attribution | predators of FTM = SHW, FVL, as in Atlantis |
| D11 | Epibenthic biomass | all 101 groups exact per box |
| D16 | Consumption | equals Atlantis intake |
| D31, N1 | Compare pools | within 0.1–0.4 % of BiomIndx; the ZL crash is visible |
| N9 | D01 visible on screen | fixed with D01 |

**Mitigated:**

- D05, predation pressure: relabelled as a screening index, with Atlantis realised intake shown beside it.

**Also fixed:** 27 P1/P2/P3 items, including spawn (D03, now exact), diet weighting (D15), Multi-run run 1 (D26), the Overview (D21), health flags (D22), folder detection (L01), the Guide hang and timeout (S17, S23), the HTML manual download (S22), and NetCDF handles (S03). See `phase8_remediation.md`.

## Regression after Phase 8

- **R CMD check:** 1 NOTE. It was 1 ERROR, 1 WARNING and 5 NOTEs before. The tests now run under check: **1,319 passed, 0 failed, 0 skipped**, including every browser test.
- **NESP reference checks re-run** (`evidence/phase9/`):
  - biomass per box: 101/101 groups exact;
  - spawn: ratio 1.000 (FTM and FTB, 5 spawnings);
  - consumption: ratio 1.000 (4 groups, 3 times);
  - SpecificPredMort: identical to the file;
  - Compare totals: within 0.4 % of BiomIndx.
- **"After" screenshots:** 35 states in `visual_design/screenshots/after/`. Spot-checked: Overview, Consumption, Mortality by predator, Compare ZL, Multi-run YOY.

## Open P0/P1 items (block release unless accepted)

| ID | Sev | Item | Smallest way to close it |
|---|---|---|---|
| D05 | P1 (was P0) | Predation pressure is still ~35× Atlantis; it is now labelled as a screening index | Accept as labelled, or rebuild per cell and per cohort (option 1) |
| D14 | P1 | Catch by fleet: fallback double-counts and mixes units | Fix the fleet regex and units; **validate on a real fished run (none available)**, or label Catch "unvalidated" |
| D17 | P1 | Silent dummy geometry when the BGM is missing | Show a banner and stop reporting tonnes |
| D18 | P1 | Intensive vs extensive aggregation (Physics box mean, "integrated" tracers, problem cohorts) | Volume-weight or relabel |
| D19 | P1 | BGM-derived volumes (`boxes.prop`) still used outside Compare pools | Use NetCDF volume everywhere |
| N5 | P1 | Food-web trophic levels (producers hard-coded) | Solve TL properly, or hide the TL axis |
| N6, N7 | P1 | Recruitment display: what-if lines hide the Atlantis series; S–R pairs not lagged | Draw Atlantis on top; lag recruits to spawning |
| S02 | P1 | Most tests still rely on synthetic fixtures | Add small real-output regression fixtures (e.g. NESP subsets) |
| S25 | P1 | Guide answers are unreliable (about 1 in 3 has a false claim) | Accept as "experimental", or disable by default (it already is) |

## Coverage

| | Software tests | Scientific validation |
|---|---|---|
| Done | Install (3 methods), R CMD check, 1,319 tests incl. browser, 4 launch modes, 35 screenshot states × 3 sizes before and after, 55-question Guide benchmark before and after, performance before and after | Biomass (box and system, all group kinds), polygon and layer indexing, SpecificPredMort, spawn, consumption, diet aggregation, predation pressure (decomposed), Schoener's D, health thresholds, mortality stocks |
| Not done | Long-session stress beyond 51 steps; dark-mode pass; implementing the approved visual redesigns (`visual_design/proposed_improvements.md`) | **Catch (blocked: no real fished run)**; growth limitation (D23); Ages totals; the mortality vs abundance consistency check; AGE_SPECIAL pools (none in NESP) |
