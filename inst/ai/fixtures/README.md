# Fixture corpus (not official Atlantis materials)

This directory is a **small, clearly marked stand-in** so Ask Atlantis can be
indexed and tested before the real manuals, Atlantis source, and calibration
notes are supplied.

It is **not** an Atlantis User Guide, **not** a copy of Atlantis C source, and
**not** a set of reviewed calibration procedures. Do not treat numbers, process
descriptions, or case studies here as authoritative.

Swap in the CSIRO user-guide PDFs by running
`inst/ai/download_atlantis_manuals.R` and pointing `RA_AI_SOURCES_CONFIG` at
`inst/ai/sources.atlantis-manuals.json`. The PDFs stay in a local corpus
directory and are not committed. For Atlantis source or calibration notes,
copy `inst/ai/sources.example.json`. Tests continue to use this fixture tree.

ReactiveAtlantis's own R/Shiny source and help **are** real and are indexed
from the package via the default manifest (`ra-shiny-modules`, `ra-r-source`,
`README.md`, plus the navigation catalog derived from `inst/shiny/app.R`).
