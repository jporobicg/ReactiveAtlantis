# FIXTURE: Atlantis parameter notes

These definitions are **stand-in text for tests**. They are not copied from an
official manual and may not match any particular Atlantis build.

## KLP

KLP is the lower gape limit used when Atlantis decides whether a predator can
swallow a prey item. It is a dimensionless fraction of predator structural
size. In Reactive Atlantis the Feeding tab shows gape overlap derived from
KLP and KUP together with the pPREY availability matrix.

If KLP is raised, fewer small prey remain inside the gape window. Check the
Feeding heatmap for cells where pPREY is positive but gape overlap is zero
before changing preferences.

## KUP

KUP is the upper gape limit. Prey larger than this fraction of predator size
are outside the feeding window even if pPREY is positive.

## Clearance C

In this fixture manual, the clearance rate C is described as cubic metres of
water searched per day (`m3 d-1`). Compare this wording with the fixture
Atlantis source, which may use a different time unit in a given build.

## mL

mL is a linear mortality rate applied to a functional group, typically with
units of per day. It is not fishing mortality. Fishing mortality should be
inspected on the Mortality tab together with natural and predation mortality.

## pPREY

pPREY is the availability of a prey group to a predator age class. It is not
a diet proportion. Realised diet also depends on prey biomass, gape overlap
and spatial overlap. Inspect pPREY on the Feeding tab; inspect realised diet
on the Predation tab.
