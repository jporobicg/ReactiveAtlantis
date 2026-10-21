# Source registry

Machine-readable copy: `inst/ai/source_registry.json`.
Ingest lists: `inst/ai/sources.json` (fixture + package + knowledge),
`inst/ai/sources.atlantis-manuals.json` (CSIRO PDFs after download).

Statuses: `verified` (checked against a live page or this repo),
`pending` (URL or rights not closed), `excluded` (do not ingest).

Uncommittable files (CSIRO PDFs, future Confluence PDF, optional HTML
mirror) are fetched by helpers into `RA_AI_CORPUS_DIR`. They are never
committed. Question time uses only the local index.

## Totals (this checkout)

| Class | Count | Notes |
| --- | --- | --- |
| Indexed with the default manifest | fixture manuals + fixture C + fixture calibration + README + `R/` + Shiny modules + knowledge products + live nav catalog | Always available in tests |
| Linked only | CSIRO Part I/II, maintained HTML manual, AtlantisTrunk, NOAA Pethybridge record, wiki/help pages, Google Group, GitHub example repos | Metadata + URL; download or clone locally |
| Pending | Appendix 1 HTML slug, Confluence 14 MB PDF, some repo licences, Tier C prm files | Do not ingest until reviewed |
| Excluded from git | Any PDF under `inst/ai/` except none — CSIRO and Confluence PDFs stay out | |

## Tier A

### Maintained Atlantis Manual (HTML)

- source_id: `atlantis-manual-html`
- title: Atlantis User Guide (HTML, August 2026)
- authors: Asta Audzijonyte, Rebecca Gorton, Isaac Kaplan, Jacob M. Kasper, Elizabeth A. Fulton
- year: 2026
- type: manual
- URL: https://atlantis-ecosystem-model.github.io/AtlantisManual/
- Part I index (verified 2026-10-21):
  https://atlantis-ecosystem-model.github.io/AtlantisManual/user_guides/quarto_site/part1/
- Appendix 1 is listed on that index as
  “16 Appendix 1: Tips for Calibrating the Biological Model: Notes from
  the 2015 Atlantis Summit”. Exact page slug is **pending** (tried
  `appendix1.html` and `16-appendix.html`; both 404).
- authority: A
- rights: public HTML; CSIRO / authors. Mirror locally; do not claim
  package copyright over the text.
- access: download HTML at install (`download_atlantis_html_manual()`),
  never at question time
- ingest: pending until a local mirror exists
- review: verified (site + Part I index)

### AtlantisTrunk

- source_id: `atlantis-trunk`
- title: AtlantisTrunk
- URL: https://github.com/Atlantis-Ecosystem-Model/AtlantisTrunk
- type: atlantis_code
- authority: A
- access: public GitHub (page reachable 2026-10-21). Clone locally to
  index definitions, flags, parameters, and CHANGELOG with commit links.
- ingest: pending (no clone in this package)
- review: verified (repository page). Do not commit the clone.

### This repository (precedence)

- source_id: `reactiveatlantis-local`
- title: ReactiveAtlantis (this checkout)
- type: reactive_code
- path: package root (`R/`, `inst/shiny/`, `README.md`)
- authority: A
- access: local files
- ingest: verified (default `sources.json`)
- review: verified
- Precedence: when this repo and an upstream copy disagree, this
  checkout wins.

### Upstream ReactiveAtlantis / rdrr man pages

- source_id: `reactiveatlantis-rdrr`
- URL: https://rdrr.io/github/jporobicg/ReactiveAtlantis/
- type: reactive_code
- authority: A (secondary to the local tree)
- ingest: excluded from the default index (local man/source already cover
  the same functions)
- review: pending (use only if local help is missing)

### Pethybridge et al. 2019

- source_id: `pethybridge-2019`
- title: Calibrating process-based marine ecosystem models: An example
  case using Atlantis
- authors: Pethybridge, Weijerman, Perrymann, Audzijonyte, Porobic,
  McGregor, Girardin, and others
- year: 2019
- type: paper
- DOI: 10.1016/j.ecolmodel.2019.108822
- URL: https://repository.library.noaa.gov/view/noaa/62642
- NOAA PDF: https://repository.library.noaa.gov/view/noaa/62642/noaa_62642_DS1.pdf
- checksum (NOAA, sha-512):
  `c9252c1ea656dc4f35a875cfe624f2fddfb86ac7668624b236b805d5f97473598ebf839ff40bdb0628f64c9e1949acd841f00885ce2f2c68e9467b24e6dc3932`
- rights: Elsevier journal article; NOAA holds an **accepted manuscript**.
  Publisher copyright is not a licence to redistribute the PDF in this
  package.
- access: citation + curated principles only until rights are reviewed
- ingest: excluded (PDF never committed). Curated principles live in
  `inst/ai/knowledge/` with this citation.
- review: verified (NOAA record 2026-10-21); rights **pending**

### Atlantis wiki FAQ / asking-for-help (historical)

- source_id: `atlantis-wiki-faq`
- type: wiki
- authority: A (historical)
- access: public wiki / CSIRO pages when reachable
- ingest: pending (pages are historical; record URL + retrieved_at
  before indexing)
- review: pending
- Google Group threads are **not** this source — see Tier B.

### CSIRO user-guide PDFs (kept)

- source_id: `csiro-user-guide-part-i` / `csiro-user-guide-part-ii`
- listed from https://research.csiro.au/atlantis/home/links/
- catalog: `inst/ai/atlantis-manuals.json`
- download: `download_atlantis_manuals()`
- authority: A
- rights: CSIRO copyright. Personal research/study download only.
  Do not redistribute with the package.
- ingest: after local download via `sources.atlantis-manuals.json`
- review: verified (catalog + helper). Files **not** in git.

### Confluence PDF (later)

- source_id: `atlantis-confluence-pdf`
- type: historical_manual
- size: ~14 MB (not yet supplied)
- access: local file only, never committed, never fetched from the
  internet at question time
- ingest: pending
- review: pending

## Tier B

| source_id | title | URL / DOI | licence note | ingest |
| --- | --- | --- | --- | --- |
| lecture-material | Atlantis lecture material page | pending URL | check before ingest | pending |
| atlantis-example-instructions | Atlantis_example_and_instructions | GitHub (search) | check | pending / explanation only |
| atlantistools | atlantistools | https://atlantis-ecosystem-model.github.io/atlantistools/ | R package; do not treat as this app | explanation only |
| morell-2026 | Morell et al. 2026 | doi 10.1016/j.ecolmodel.2026.111688 | journal; citation | linked only |
| atlantis2ls | alaiam/atlantis2ls | GitHub | check licence | explanation only |
| atlantis-calibration | alaiam/Atlantis_Calibration | GitHub | check licence | explanation only |
| psatlantismodel | atlantis-amps/psatlantismodel | GitHub (also historical hmorzaria/psatlantismodel) | model-specific; explanation only | excluded from default index |
| atlantis-google-group | Atlantis Google Group | public group URL | metadata and links only; **do not index posts** | excluded |

## Tier C (optional)

Public `.prm` files from other models may be listed after a licence
check. None are ingested in this checkout. Record licence, model, and
Atlantis version before adding a path.

## Fixture corpus (tests)

`inst/ai/fixtures/` is marked `FIXTURE-NOT-OFFICIAL`. It is the
retrieval source of truth for automated tests. Real manuals outrank it
only when a local official corpus has been downloaded.
