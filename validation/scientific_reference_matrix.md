# Scientific reference matrix

The reference is the Atlantis source `BethAtlantis_fix/atlantis` (16792e4), which is the build that produced the NESP datasets. Other Atlantis versions may differ, and each must be checked separately before results carry over.

Status meanings:

- **MATCH (source)**: the code agrees with the Atlantis source on reading; not yet run.
- **MISMATCH**: see the `defects.md` ID.
- **Pending**: the independent calculation has not been done.

| Diagnostic | RA implementation | Atlantis reference | Independent validation planned (Phase 4) | Status |
|---|---|---|---|---|
| mg N → t | `mg2t = 2e-8`, X_CN 5.7 hard-coded (predation_pressure.R:37-38) | `mg_2_tonne 2e-8` (atlantisboxmodel.h:448); X_CN from prm (atbiolmessage.c:1631) | Read X_CN from the NESP prm and compare | MATCH constant; D24 |
| Age-structured biomass | Σ(RN+SN)·Nums (analysis_helpers.R:1198-1212) | atbiolmessage.c:274 | Recompute from NESP output.nc in Python and compare with RA and BiomIndx, excluding boundary boxes | MATCH (source); Pending |
| Water-column pool biomass | `_N` × `volume` (1222-1229) | tr·dz·area (atbiolmessage.c:354) | Same as above | MATCH (source); Pending |
| Epibenthic pool biomass | no area term (1233) | epi × area (atbiolmessage.c:499) | Same as above | MISMATCH D11 |
| Multi-cohort pools | `_N1` only | all cohorts (atbiolmessage.c:351-354) | — | MISMATCH D12 |
| System totals vs BiomIndx | all boxes and layers | boundary boxes and some detritus excluded (atbiolsetup.c:1580; atbiolmessage.c:347,435,971) | Compare RA total and BiomIndx per group; the expected difference is boundary boxes only | D20; Pending |
| Layer layout | padding at low z, surface at nz−2, sediment at nz−1 (layers.R) | NESP output.nc shows the same (`evidence/E01`) | init.nc and other models | MATCH for NESP output; init Pending |
| Box ↔ polygon mapping | BGM box b → NetCDF b index b+1, 1-based (analysis_helpers.R:705-716) | BGM `box{b}` = NetCDF b | Plant a unique value per box in a copy of NESP output and check the rendered map | MATCH (source); Pending (critical) |
| Holling II | C·A/(1+C·ΣE·B*/mum) (predation_pressure.R:219-223) | atprocess.c:833-836, 924-926 | Closed form plus a single-cell reproduction from NESP prm + output | Algebra MATCH; D05 |
| Holling I | min(C·A, mum/E_eff) | mum/E1 (atprocess.c:840-845) | — | MISMATCH D10 |
| Holling III | B*² | atprocess.c:848,962 | — | MATCH (source) |
| Satiation cap, Tcorr, refuge, pSTOCK | absent | atprocess.c:896-898; atq10.c:493 | Quantify on NESP v8c (flag_satiation 1) vs v8d (0) | MISMATCH D05 |
| Gape | knife-edge, strict, SN-based | inclusive hard window or soft windows (atvertprocesses.c:541-653) | Hand cases at the boundaries | MISMATCH D09 |
| Juvenile/adult | 1-based cohort < age_mat | 0-based cohort ≥ age_mat (atbiolsetup.c:542-562) | Hand case age_mat = 2 | MISMATCH D08 |
| pPREY naming | `pPREY{j}{CODE}{i}` (feeding_module.R:1207-1250) | atBioltoXML.c:643-749 | Parse NESP prm; count non-zero entries vs grep | MATCH (source); D25 |
| SpecificMort | `CODE-age-S0-{F,M1,M2}` (mortality_module.R:215) | atbiolmessage.c:1863-1888 | NESP file | MATCH; D27 for S1+ |
| SpecificPredMort | Group = predator | Group = **prey**, columns = predators (atbiolmessage.c:2000-2016) | NESP header (`evidence/E02`) | **MISMATCH D01 (P0)** |
| Mort.txt | `CODE-M`/`CODE-F` annualised (process_diagnostics.R:249) | atbiolmessage.c:1753-1827 | NESP file | MATCH (source) |
| Spawn | subtracts surplus (recruitment_module.R:583-589) | subtracts deficit (atdemography.c:571-580) | Hand cases with SN 10 and RN 20 / 30 | **MISMATCH D03** |
| Beverton–Holt (cases 3, 10) | α·S/(β+B), B = total stock N (433-443) | atannualbiology.c:1198-1220 | Reproduce NESP tuna recruits from output (see the v8d BHalpha runs) | MATCH form; D04 label and fallback |
| YOY → t | recruits·(KWSR+KWRR)·X_CN·mg2t | atdemography.c:2123-2125 | NESP YOY.txt | MATCH (source) |
| Schoener's D | Σmin(p,q) on biomass shares | Schoener (1970) | Controlled distributions: complete, none, partial, zero | MATCH; D30 |
| Catch | FC vars (t), cohort `_Catch` (numbers) | atHarvestSetup.c:280-345 | North Sea fixture only; NESP has fishing off | MISMATCH D14; data-blocked |
| Consumption (PROD `_Eat`) | × volume | units attribute in PROD.nc | NESP PROD.nc | Pending (D16) |
| DietCheck aggregation | proportions summed over cohorts | DietCheck: per-cohort proportions | NESP DietCheck | Pending (D15) |
| PP limitation (flagnut 2) | 2/(din+si) | Atlantis nutrient limitation (to cite) | — | MISMATCH D23 |
| Health thresholds | < 1 %, > 10×, 0.5–1.5 band | Screening heuristics with no Atlantis counterpart | Boundary tests at exactly 0.01 and 10 | Design choice; label as screening |
