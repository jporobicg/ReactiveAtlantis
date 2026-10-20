# Fixture corpus (not official Atlantis materials)

This directory is a **small, clearly marked stand-in** so Ask Atlantis can be
indexed and tested before the real manuals, Atlantis source, and calibration
notes are supplied.

It is **not** an Atlantis User Guide, **not** a copy of Atlantis C source, and
**not** a set of reviewed calibration procedures. Do not treat numbers, process
descriptions, or case studies here as authoritative.

Swap in the real corpus by pointing `RA_AI_SOURCES_CONFIG` at a manifest whose
`path` entries locate the actual manuals, source tree, and calibration notes.
See `inst/ai/sources.json` and `inst/ai/sources.example.json`.

ReactiveAtlantis's own R/Shiny source and help **are** real and are indexed
from the package via the default manifest (`ra-shiny-modules`, `ra-r-source`,
`README.md`, plus the navigation catalog derived from `inst/shiny/app.R`).
