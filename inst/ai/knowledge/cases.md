# Diagnostic case cards

Reviewed hypothesis cards. Each observation has several mechanisms.
None is a diagnosis. None is a universal parameter direction.
Citation: pethybridge-2019 (principles: inspect biomass, age,
condition, realised diet, and space before editing); fixture-manuals;
reactiveatlantis-local.

## Case: declining-biomass

Observation: biomass of a group falls through time, or ends far
below initial.

Hypotheses (not mutually exclusive):

1. Predation increased. Expected: predator diet share or predation
   mortality stack rises. Counter: Predation and Mortality look
   stable while YOY or condition collapse.
2. Food limitation / starvation. Expected: consumption falls; pPREY
   or gape overlap looks ineffective; ResN drops. Counter: intake
   and condition stay healthy while catch or mL-like natural
   mortality rises.
3. Recruitment failure. Expected: YOY and SSB fall, perhaps with a
   lag. Counter: adults fall first while YOY stays high.
4. Fishing. Expected: Catch and fishing mortality rise. Counter:
   fishing stack is flat.
5. Spatial shift. Expected: biomass leaves productive boxes or
   layers. Counter: Spatial looks stable.

Checks: Overview health flags; Compare Bt/B0; Predation; Mortality;
Recruitment; Consumption; Feeding; Spatial; Physics.

What not to conclude: that fishing, pPREY, or mum is the cause from
the biomass plot alone. Do not “increase mum” or “cut mL” as a
universal step.

Tools: overview, compare, predation, mortality, recruitment,
consumption, feeding, spatial, physics, ages.

## Case: poor-growth-condition

Observation: ResN or StructN sit outside the 0.5–1.5 band, or
weight-at-age looks thin.

Hypotheses: low intake (mum/C, pPREY, prey biomass); high
metabolic or temperature stress; allocation / condition after a
diet shift; data or unit mismatch.

Expected: Consumption and Feeding show a constraint; Physics may
show an extreme tracer. Counter: intake looks adequate and
mortality stacks explain the biomass loss instead.

Checks: Overview problem cohorts; Ages; Spatial by-age; Consumption;
Feeding; Physics.

What not to conclude: that Growth (primary-producer limitation) is
the vertebrate condition tab. Do not treat a single mum direction
as a fix.

Tools: overview, ages, spatial, consumption, feeding, physics,
compare.

## Case: unexpected-increase

Observation: biomass grows far above initial (Overview exploding).

Hypotheses: weak predation; strong recruitment; unused fishing;
prey pulse; parameter or unit error that inflates growth.

Expected: Predation share on this group is small; YOY is high;
Catch is low. Counter: predators eat it heavily and Catch is high
while biomass still explodes (look at units and initial conditions).

Checks: Overview; Compare; Predation (as prey); Recruitment; Catch;
Mortality.

What not to conclude: that the group is “healthy”. An explosion can
be a parameter error.

Tools: overview, compare, predation, recruitment, catch, mortality.

## Case: poor-recruitment

Observation: YOY is low, missing, or lags SSB.

Hypotheses: SSB too low; Beverton-Holt alpha/beta or KDENR starve
the curve; larval food (Recruitment growth view); spatial mismatch
of spawners and nursery boxes.

Expected: Recruitment plots show a flat curve or empty YOY.
Counter: YOY is fine and adult mortality explains the later drop.

Checks: Recruitment (yoy, ssb); Compare; Spatial; Consumption for
young classes if PROD exists.

What not to conclude: a universal KDENR or beta direction. Recalculate
in the side panel is a what-if, not a saved run.

Tools: recruitment, compare, spatial, overview.

## Case: odd-diets

Observation: realised diet looks unexpected, or pPREY and DietCheck
disagree.

Hypotheses: pPREY availability is not the diet; gape overlap is
zero; prey biomass is scarce; spatial overlap is missing; DietCheck
threshold hides links.

Expected: Feeding diagnostics flag mismatch or uneaten prey;
Predation shows a different top prey. Counter: both views agree and
the surprise is ecological.

Checks: Feeding (matrix, predation pressure, diagnostics); Predation;
Food web; Consumption.

What not to conclude: that pPREY is a diet proportion, or that one
cell should be raised as a rule.

Tools: feeding, predation, foodweb, consumption.

## Case: excessive-predation

Observation: a prey group is heavily eaten, or predation mortality
dominates.

Hypotheses: predator biomass high; pPREY or gape window wide;
prey concentrated in predator boxes; fishing on the predator is
low.

Expected: Predation and Mortality (predation stack) rise together.
Counter: predation stack is small and fishing or recruitment
explains the prey drop.

Checks: Predation (predator and prey views); Mortality; Compare for
the predator; Feeding (predation pressure matrix, per-box map,
production flags); Spatial.

What not to conclude: that pPREY must be reduced. Check overlap and
biomass first.

Tools: predation, mortality, compare, feeding, spatial.

## Case: primary-producer-limitation

Observation: phytoplankton or other producers look weak, or
consumers starve while producers are low.

Hypotheses: light, nutrient, or eddy limitation; forcing; grazing
pressure; box/layer mismatch.

Expected: Growth tab shows a limitation; Physics shows nutrients
or light-related tracers. Counter: producers look healthy on Growth
and the consumer problem is diet or gape.

Checks: Growth; Physics; Consumption (grazing); Spatial.

What not to conclude: that the Growth tab diagnoses vertebrate
weight. Use Ages for ResN/StructN.

Tools: growth, physics, consumption, spatial, compare.

## Case: spatial-patterns

Observation: biomass is in unexpected boxes or layers, or the map
is empty.

Hypotheses: movement / habitat; layer padding (deep pads are not
real water); BGM/NetCDF box mismatch; true ecological shift.

Expected: Spatial map by layer plus series for the selected box.
Counter: whole-system Compare looks fine and the pattern is only
local.

Checks: Spatial (map and by-age); Physics; Compare.

What not to conclude: a blank map means the run is empty. Check BGM
detection and unused-box fill first.

Tools: spatial, physics, compare, ages.

## Case: skill-deterioration

Observation: catch or biomass skill versus observations gets worse.

Hypotheses: process drift (fishing, recruitment, diet); observation
coverage; unit or fleet mapping; a later period the model was not
tuned for.

Expected: Catch skill view plus Compare observed points move
together. Counter: skill is poor because the CSV is missing or
misaligned, while internal dynamics look stable.

Checks: Catch (view skill); Compare; Multi-run if another run
exists.

What not to conclude: that skill is a separate application. It is
Catch view `skill`. The Guide cannot compute a new skill table.

Tools: catch, compare, ensemble.

## Case: crashes-param-errors

Observation: a group hits zero, the run dies, or parameters look
inconsistent.

Hypotheses: numerical / initial-condition problem; recruitment
collapse; extreme mortality; pPREY or clearance units; missing
files so a tab looks “empty”.

Expected: Overview extinct flag; Compare goes to zero; Mortality or
Recruitment show an extreme. Counter: the tab is empty because a
file role is missing, not because the group crashed.

Checks: Overview; Compare; Mortality; Recruitment; Feeding
diagnostics; Physics; file-role availability in the Run sidebar.

What not to conclude: a crash from a single parameter name. Do not
ask the Guide to edit the prm or rerun Atlantis.

Tools: overview, compare, mortality, recruitment, feeding, physics.
