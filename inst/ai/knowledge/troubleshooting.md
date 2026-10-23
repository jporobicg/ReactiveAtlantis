# Troubleshooting Q&A

Each item has a status: **confirmed** (this package or fixture
materials) or **unconfirmed** (historical / community, not verified
here).

## Why is the Spatial map blank? (confirmed)

A blank map usually means the BGM was not found, the polygon parser
did not build rings, or box counts in the NetCDF and BGM disagree.
Reactive Atlantis searches nested folders for `.bgm` and supports
`boxN.vert`, count blocks, `vert id x y` plus index refs, and face
rings. Unused boxes render in a sand fill. This is not proof the
stock is zero. Status: confirmed against `spatial_module.R` and
`read_bgm_polygons()`.

## What is pPREY? (confirmed)

Availability, not a diet proportion. See Feeding for the matrix and
Predation for DietCheck. Status: confirmed (fixture parameters.md).

## Where is expected predation pressure? (confirmed)

Feeding tab, view Predation pressure. The heatmap is the same
pPREY layout; cells are implied prey-biomass loss per day from
the Holling equations at t0 (C, mum, E, predcase, KLP/KUP, init.nc).
KDENR is not used. Status: confirmed against `predation_pressure.R`.

## Is mL fishing mortality? (confirmed)

No. mL is linear natural mortality. Use the Mortality tab for
fishing, natural, and predation stacks. Status: confirmed.

## Can the Guide change pPREY for me? (confirmed)

No. The Guide is read-only. Feeding compare can highlight in-session
edits; it does not write the prm. Status: confirmed.

## Why does Ask Atlantis / the Guide say Offline? (confirmed)

Ollama is not reachable on `RA_OLLAMA_URL`, or `RA_AI_ENABLED` is
false so the drawer is absent. Scientific tabs still work. Status:
confirmed.

## Do I need the internet to ask a question? (confirmed)

No. Question time uses the local index and local Ollama only.
Downloads happen beforehand via helpers. Status: confirmed.

## Should I increase mum when biomass declines? (confirmed as what-not-to-do)

No universal direction. Declining biomass has several mechanisms.
Inspect Consumption, Feeding, Predation, Recruitment, Mortality,
and Spatial first. Status: confirmed (case declining-biomass).

## What is Appendix 1 of the HTML manual? (unconfirmed detail)

The Part I index lists “Appendix 1: Tips for Calibrating the
Biological Model: Notes from the 2015 Atlantis Summit” as chapter
16. The exact HTML slug was not resolved in this checkout. Status:
unconfirmed (page slug pending).

## Are Google Group answers reliable? (unconfirmed)

Community threads are historical and version-specific. This package
stores links only and does not index posts. Status: unconfirmed.

## Does Morell et al. 2026 change how I use this app? (unconfirmed)

The paper is registered as a citation (doi
10.1016/j.ecolmodel.2026.111688). It is not ingested. Status:
unconfirmed for this UI.
