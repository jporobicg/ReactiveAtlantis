# Release readiness: interim position, before remediation (end of Phases 1–7)

**Recommendation as of 2026-10-25: NO-GO.** This is not final; it will be re-issued after Phases 8–10.

## Why

- **7 P0 defects**, 6 of them confirmed by running the code on real NESP output:
  - D01: predator attribution is transposed.
  - D05: expected predation pressure is about 35× the realised intake.
  - D11: epibenthic biomass is about 1e-10 of the true value.
  - D16: the Consumption tab shows about 1e-11 of the realised intake.
  - D31: Compare pool totals are in mg N but labelled tonnes.
  - N1: the scaled zooplankton (ZL) total hides a 68 % crash.
  - N9: D01 is visible on screen (detritus listed as a predator of tuna).
- **26 P1 defects**, including:
  - D03: spawn has the wrong sign (2.6–12× too high in thin years).
  - D15: diet proportions are summed over cohorts.
  - D26: run 1 is missing from Multi-run.
  - S01: the documented install command installs master.
  - S13: no tests run under `R CMD check`.
  - S17 and S23: the Scientific Guide cannot answer.
  - S25: Guide answers are unreliable.
- **The existing test suite does not detect any of these.** It is green (1,095 expectations) and asserts some of the wrong behaviour.

## Coverage

| | Software test coverage | Scientific validation coverage |
|---|---|---|
| Executed | Install: 3 methods. R CMD check. Existing suite: 1,095 expectations, all 10 browser tests. 4 launch modes. 35 screenshot states at 3 sizes. 55-question Guide benchmark. Performance run of 51 steps. | Against an independent reference or Atlantis output:<br>- biomass for 101 groups per box, and Compare totals;<br>- polygon and layer indexing (all 147 boxes);<br>- SpecificPredMort attribution;<br>- spawn;<br>- predation pressure (one cell, decomposed);<br>- consumption (4 groups);<br>- DietCheck aggregation;<br>- Schoener's D (controlled cases);<br>- health thresholds;<br>- mortality stocks. |
| Not tested | Rapid-interaction and long-session stress beyond 51 steps; a dark-mode visual pass | Catch and fleets (**blocked: no real fished run**); growth limitation (D23); Ages totals; the BH curve plot end-to-end; mortality vs abundance consistency; AGE_SPECIAL pools (none in NESP) |

## What a release needs

1. Fix all P0 and P1 defects, or remove or relabel the affected views as "unverified".
2. Add a regression test for each, built on independent references like the ones in `evidence/phase4/`.
3. Make the documented install command install the release.
4. Get the test suite running under `R CMD check`.
5. Mark the Scientific Guide experimental.
6. Validate Catch on a real fished run, or label it unvalidated.
