# Phase 6: Atlantis Scientific Guide validation (2026-10-25)

## Setup

| Item | Value |
|---|---|
| Ollama | 0.40.11 |
| Chat model | `qwen3.5:4b`, the documented default (it exists in the Ollama library) |
| Embedding model | `nomic-embed-text` |
| Manuals | CSIRO User Guide Part I (11.7 MB) and Part II (2.3 MB), downloaded by `download_atlantis_manuals()` in 0.1 min |
| HTML manual | **Failed** (S22): the downloader deletes anything that is not a PDF |
| Index | Built by `index_ask_atlantis()` in 2.2 min: 1,291 chunks, all embedded (manual 890, package code 401). 4 manual pages unreadable. 2 configured sources missing in the installed package (S24) |
| Storage | Index and corpus in `~/whacamole_nesp/ra_audit_data/ai` (via `RA_AI_INDEX_DIR` and `RA_AI_CORPUS_DIR`); nothing was written into the package |
| Disabled mode | Covered by the existing tests (they pass) |
| Offline behaviour | Retrieval is local, and so are chat and embeddings; the only network use is the manual download |

## As shipped: **FAIL** (S23)

`qwen3.5:4b` writes out its reasoning before answering. A one-sentence answer took 306 s and 3,590 tokens; the Guide's timeout is 90 s. The first 3 benchmark questions each returned "I could not reach the local language model" after about 90 s. The message also misdiagnoses the problem: the model was reachable, it was just slow. The benchmark was stopped at that point.

Hardware: CPU only (2 × E5-2630 v4), with machine load about 11 during the test.

## Answer quality with an audit workaround

Reasoning was disabled (`think: false`) through the Guide's own documented hook, `options(reactiveatlantis.ollama_transport)`. **This is not shipped behaviour.**

**Benchmark:** 55 independent questions (`evidence/phase6/benchmark_questions.json`):

| Category | Questions |
|---|---|
| Fact | 25 |
| Reasoning | 10 |
| Navigation | 5 |
| False premise | 5 |
| Out of scope | 4 |
| Ambiguous | 2 |
| Prompt injection | 2 |
| Action request | 2 |

Median answer time: 61 s.

**Two scorings:**

1. **Regex key facts:** 40 / 55 pass. This scoring is unreliable: several passing answers contain false claims.
2. **Expert grading** of every answer against the Atlantis source, the User Guide and the NESP prm (`evidence/phase6/graded_answers.md`):

| Measure | Result |
|---|---|
| Mean accuracy (0–2) | **1.00** |
| Answers with accuracy 2 | **36 %** |
| Answers with at least one false claim | **18** |
| Fabricated parameter names | 2 (`clearance` in F03; `recruitXXX` in P01, the real name being `flagrecruitXXX`) |
| Legitimate in-scope questions refused as "out of scope" | **5** (F24 flagfishrates, R02 low reserve N, R07 thin tunas after spawning, N04 where to see ResN/SN by age, A02 "what does E mean") |
| Broken outputs | 2 (a 90 s timeout; raw JSON printed as the answer) |
| Documented items said to be "not in the documentation" | 4 (flag_satiation, pPREY digit meaning, UseHardFeedingWindow, YOY.txt) |
| Distinct questions given the same canned "collapsing group" reply | 6 (two of them about *exploding* groups) |
| Out-of-scope questions handled correctly | 4 / 4 |
| Action requests refused (no file edits, no runs) | 2 / 2 |
| Prompt injections refused | 2 / 2, but I02 then misdescribes pPREY |

### Most consequential false claims

1. KLP/KUP gape limits inverted.
2. pPREY described as assimilation efficiency, cumulative mortality, or the proportion eaten per step. It is the maximum availability of prey to the predator, 0–1.
3. Temperature effects put down to "pHCorr"; Q10/Tcorr is never mentioned.
4. Beverton–Holt (flagrecruit 3) said to depend on plankton.
5. M1 and M2 definitions swapped or mixed.

### Good answers worth noting

- R08 explains the satiation cap (mum/E) correctly.
- F17 says correctly that SpecificPredMort rows are prey and columns predators. That is the opposite of what the app's own code does (D01).

### Grader claim not confirmed

The grader reported that User Guide I p.157 has `UseHardFeedingWindow` inverted. The manual text I checked says the smooth window is `UseHardFeedingWindow=0`, which agrees with the code (atvertprocesses.c:557). The only mismatch is that the manual writes strict `<` and the code uses inclusive `≥`/`≤`.

## Assessment

| Area | Result |
|---|---|
| Safety (no file edits or simulation launches; injection refusal) | PASS on this benchmark (2/2 + 2/2). It is regex-based (S07, S12) and not adversarially exhaustive. |
| Scientific reliability | **FAIL.** 1 in 3 answers contains a false scientific claim, often with a citation attached, which makes it look supported. |
| Usability as shipped | **FAIL** (S23). |
| Navigation | Partial: the right tab is usually listed, but often among many. |

**Recommendation:** do not ship the Guide as enabled-by-default or as "validated". If it ships, it should be:

- labelled experimental;
- fixed for S23 (`think: false`) and S22 (HTML manual download);
- given a narrower refusal rule that admits in-scope questions;
- scored with the rubric automatically, on a reference set like this one, before each release.
