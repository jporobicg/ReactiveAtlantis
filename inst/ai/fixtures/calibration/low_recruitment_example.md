# FIXTURE calibration note

This is a **worked example invented for tests**, not a reviewed procedure and
not a universal rule. It is labelled so the assistant must treat it as a case
from one model build, not as advice to copy blindly.

## Case: low YOY for COD in a shelf model (fixture)

- Symptom: COD YOY in `YOY.txt` fell for three years while adult biomass looked
  stable.
- Evidence inspected: Recruitment tab (YOY and SSB), Compare relative biomass,
  Predation diet of COD age 1, Physics surface temperature.
- Plausible mechanism in **that** model: SSB had not dropped, but the
  Beverton-Holt beta was large relative to SSB, so recruits sat on the steep
  part of the curve. DietCheck also showed age-1 COD eating almost no
  zooplankton after a prey shift.
- Action taken in that example: the calibrator lowered beta by about 20% and
  slightly increased zooplankton availability for age-1 COD. That action is
  **not** a recommended default.
- How effectiveness was checked: YOY recovered in a later run of the same
  model; adult biomass was re-checked on Compare so the change had not created
  an exploding stock.
- Caveats: Atlantis version in the example was "FIXTURE-code-build-X". A
  different build may apply extra larval mortality after the Beverton-Holt
  curve (see the fixture recruit source). Do not reuse the 20% beta change on
  another model.
