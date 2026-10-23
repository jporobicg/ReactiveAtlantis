# FIXTURE calibration note

Example only. Not a rule for pPREY editing.

## Case: flagged gape mismatch on the Feeding tab (fixture)

- Symptom: several pPREY cells for a large piscivore were positive while gape
  overlap was zero (navy outline in Reactive Atlantis Feeding).
- Evidence inspected: Feeding mismatch view, predator KLP/KUP in the biology
  `.prm`, prey size at age on Compare age structure.
- Action in that case: the calibrator did **not** raise pPREY. They lowered KLP
  slightly after confirming prey length-at-age was realistic, then re-checked
  the mismatch table.
- Caveat: increasing pPREY on a zero-overlap cell cannot create diet in the
  fixture consumption function, because intake is zero when gape_overlap <= 0.
  Always inspect Feeding before changing preferences.
