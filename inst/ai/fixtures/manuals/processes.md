# FIXTURE: ecological processes

Stand-in explanations for tests. Not an official Atlantis process chapter.

## Consumption and predation mortality

Atlantis consumption is limited by a maximum daily ration and by how much prey
is available after gape and preference filters. Predation mortality on a prey
group is the biomass removed by all predators, not a single parameter.

Documented sequence in this fixture:

1. Restrict prey by gape (KLP, KUP).
2. Apply pPREY availability.
3. Draw from the remaining prey biomass using clearance.
4. Cap intake at the maximum daily ration.

The Feeding tab Predation pressure view uses the modified Holling
forms from User Guide Part I chapter 10.2 (Consumer feeding),
section 10.2.2 (C and mum), Table 18 (predcase 0/1/2) and
section 10.2.5 (size refuge). Those names also appear with
`box_bio_process` in atecology/atbiology.c. The Type II term is

    B*_j = pPREY * prey * gape
    I = C * A / (1 + C/mum * sum(E * B*))
    Gr_j = I * predator_scale * (B*_j / A)

with predator_scale = numbers when flagfishrates = 0 and predator
mg N when flagfishrates = 1. KDENR is a recruitment parameter in
the current manuals and is not applied to feeding.

Where this fixture disagrees with versioned code, report both and do not
invent a reconciliation.

## Recruitment

Beverton-Holt recruitment uses an alpha (maximum recruits) and beta (SSB at
half of alpha). Reactive Atlantis draws YOY from `YOY.txt` (`CODE.0`) and can
overlay larvae from a Beverton-Holt curve on the Recruitment tab. Low
recruitment can come from low SSB, a steep stock-recruit slope, high larval
mortality, or prey shortage for young age classes. Those are hypotheses until
the corresponding outputs are inspected.

## Growth of primary producers

Primary-producer growth is limited by light, nutrients and eddy scaling.
The Growth tab shows those limitation terms by layer. Animal growth (StructN
and ResN) is inspected on Overview (problem cohorts) and Compare (age
structure), not on the Growth tab.

## Why biomass can decline while fishing looks stable

Possible processes, not a diagnosis:

- Predation mortality increased even if fishing mortality did not.
- Recruitment failed, so numbers drop with a lag.
- Growth declined (condition: StructN / ResN).
- Spatial distribution shifted into less productive boxes.
- A prey group collapsed, reducing consumption.

Inspect Mortality, Recruitment, Compare, Predation and Spatial rather than
asserting a single cause.
