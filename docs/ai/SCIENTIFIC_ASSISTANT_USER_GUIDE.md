# Atlantis Scientific Guide — user guide

The Guide is an optional drawer in Reactive Atlantis. It explains
Atlantis ideas from a **local** index and points you at **existing**
tabs. It does not run the model, edit parameters, or read the numbers
off your plots.

## Open the Guide

1. Install [Ollama](https://ollama.com) and pull a chat model and
   embeddings (once, not at question time):

   ```bash
   ollama pull qwen3.5:4b
   ollama pull nomic-embed-text
   ```

2. Build the local index (from the package root, or after install):

   ```r
   library(ReactiveAtlantis)
   index_ask_atlantis()
   diagnose_ask_atlantis_index()
   ```

3. Enable the drawer and launch:

   ```r
   Sys.setenv(RA_AI_ENABLED = "true")
   launch_reactiveatlantis()
   ```

4. Click the speech-bubble button at the bottom right. The drawer
   titled **Atlantis Scientific Guide** opens from any tab. It stays
   closed until you open it and does not cover plots.

If Ollama or the index is missing, the badge shows Offline / Model
missing / a failure line. Every scientific tab still works.

## What it will and will not do

Will:

- Answer questions about processes, parameters, and where to look.
- List competing hypotheses for a symptom (never one cause).
- Offer Open-tab buttons for real tabs only.
- Show sources with type labels (manual, code, knowledge, …).

Will not:

- See biomass values, NetCDF arrays, or the plot you are looking at.
- Write files, change pPREY, or launch Atlantis.
- Invent a tab or a citation that was not retrieved.
- Call a cloud model or the internet while you ask.

## Example questions

Click one in the drawer, or type it:

1. What does the Atlantis parameter KLP represent?
2. What is pPREY? Is it a diet proportion?
3. Where do I inspect predation?
4. What should I check when biomass is declining?
5. What should I check when recruitment is low?

Further useful questions: Why can biomass fall when fishing looks
stable? Where are temperature and nitrate shown? How do I compare
catch to observations? Are primary producers limited?

## How to read an answer

- **Answer** — short guidance.
- **What is known** — facts the local materials support.
- **Possible explanations** — labelled hypotheses, not a diagnosis.
- **What to inspect** — checks plus Open-tab buttons.
- **Cautions** — what not to conclude.
- **Sources** — title, section, type. If the list is empty, treat the
  answer as weak.

Evidence status is one of supported, partial, insufficient, or
out of scope. Insufficient is normal when the index is thin or the
run numbers are not visible.

## Calibration questions

Questions such as “why is FDO collapsing?” stay in scope. The Guide
lists processes (predation, food, recruitment, fishing, condition,
physics, space) and the tabs that inspect them. That is **not** a
finding that FDO is collapsing for a stated reason.

## Privacy

Nothing is sent off the machine. Chat history is this browser session
only. Clear wipes it.
