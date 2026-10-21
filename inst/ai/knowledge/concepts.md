# Concepts and verified parameter names

These notes use only names verified in this package: the fixture
manual (`inst/ai/fixtures/manuals/parameters.md`), ReactiveAtlantis
tabs, and fixture C fragments. Unverified symbols are not listed.

Citation: fixture-manuals; reactiveatlantis-local. Official manuals
outrank this file when they are indexed locally.

## KLP

KLP is the lower gape limit used when a predator can swallow a prey
item. It is a dimensionless fraction of predator structural size. The
Feeding tab shows gape overlap from KLP and KUP together with pPREY.

Raising KLP can shrink the set of small prey inside the gape window.
That is a mechanism, not a recommended edit. Check Feeding cells
where pPREY is positive and overlap is zero before changing anything.

## KUP

KUP is the upper gape limit. Prey larger than this fraction of
predator size sit outside the feeding window even if pPREY is
positive.

## Clearance C

In the fixture manual, clearance C is cubic metres searched per day
(`m3 d-1`). The fixture Atlantis source may use a different time
unit. When documentation and code disagree, report both versions.
Reactive Atlantis does not pick a winner.

## mL

mL is a linear **natural** mortality rate (typically per day). It is
not fishing mortality. Inspect fishing, natural, and predation
mortality together on the Mortality tab.

## pPREY

pPREY is prey **availability** for a predator age class. It is not a
diet proportion. Realised diet also depends on prey biomass, gape
overlap, and spatial overlap. Inspect pPREY on Feeding; inspect
realised diet on Predation.

## mum

mum is a maximum consumption / growth-rate scalar. When mum or
clearance C is low relative to prey, intake can fall even if fishing
looks stable. Check Consumption and Feeding. Do not treat “increase
mum” as a universal fix.

## KDENR

KDENR is a recruitment-related density parameter used with the
stock-recruit relationship. If KDENR or Beverton-Holt alpha/beta
starve the curve, YOY can fall after a lag. Inspect Recruitment.
Do not treat a single KDENR direction as a rule.

## ResN and StructN

Reserve nitrogen (ResN) and structural nitrogen (StructN) describe
condition and size. Overview flags age classes outside 0.5–1.5 of
the initial value. Ages plots whole-system numbers-weighted means.
Spatial by-age shows one box and layer. Biomass on those views uses
`Nums * (ResN + StructN) * mg2t * x.cn` with mg2t `2e-8` and x.cn
`5.7` in this package.

## Beverton-Holt alpha and beta

Recruitment shows a side panel for alpha and beta. Recalculate is a
what-if on the displayed curve. It is not a write of a new official
run and not a request the Guide can execute.

## DietCheck versus pPREY

DietCheck is realised diet in the output. pPREY is a preference /
availability input. A mismatch is a reason to look, not proof that
one number is wrong.
