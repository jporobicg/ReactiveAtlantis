# Atlantis Scientific Guide architecture

The Guide is the existing Ask Atlantis drawer, index, and Ollama client
with a stricter answer contract. It does not add a second chat stack
and it does not replace any scientific tab.

See also `docs/ai/IMPLEMENTATION_AUDIT.md` and `inst/ai/architecture.md`.

## Runtime shape

```
Shiny app (14 tabs)
  └─ assistant_module (one drawer, optional)
       ├─ ra_ai_safe_ui_context   session metadata only
       ├─ ra_ai_rank_tools        deterministic, before LLM
       ├─ ra_ai_retrieve          global SQLite index
       ├─ ra_ai_answer            scope → retrieve → generate → validate
       └─ render sections         never raw JSON
```

- Enabled only when `RA_AI_ENABLED` is true.
- LLM calls use `shiny::ExtendedTask` so other tabs stay usable.
- Transport is local Ollama HTTP. There is no cloud substitute.
- Retrieved passages are untrusted data. Injection patterns are
  blocked in R (`ra_ai_scope_check`).

## Two contexts

1. **Global corpus** — manuals, code, knowledge products, optional
   local downloads. Shared SQLite + FTS5 + embeddings under
   `RA_AI_INDEX_DIR` (default user cache `ask-atlantis`).
2. **Session context** — current tab, selected group, loaded group
   table, run pair labels, file-role availability. No NetCDF dumps,
   no DietCheck matrices, no plot pixels. Isolated per Shiny session.

## Retrieval

- Hybrid BM25 (FTS5) + embeddings when present.
- Authority weights: Atlantis code ≥ official manuals > this repo >
  knowledge cards > calibration examples.
- Dedup by source + leading text. Return 3–6 passages when that many
  exist (`max_chunks` default 6).
- Incremental rebuild uses file checksum + mtime.
- Changing `RA_OLLAMA_EMBED_MODEL` clears stored vectors.

## Answer contract

The model must return JSON. R validates and fills defaults:

| Field | Rule |
| --- | --- |
| answer | required string |
| what_is_known | string; empty allowed |
| possible_explanations | string list |
| suggested_checks | string list |
| recommended_tool_ids | must be in `tools.json` / nav catalog |
| citations | must match retrieved `chunk_id` or `source_id` |
| limitations | string |
| evidence_status | `supported` \| `partial` \| `insufficient` \| `out_of_scope` |
| follow_up_question | string |
| claim_kinds | existing enum (kept) |
| nav_targets | existing catalog (kept; merged with ranked tools) |

Unknown tool ids and non-retrieved citations are dropped. The UI
shows a short answer, alternatives, checks with Open-tab buttons,
cautions, and sources with type labels.

## Read-only invariants

Enforced in R, not only in the prompt:

- No file writes, no prm edits, no shell, no Atlantis launch.
- No internet at question time (index and Ollama are local).
- Missing index or Ollama degrades to a visible message; tabs still
  work.
- Calibration notes are examples, never universal parameter rules.

## Knowledge products

Stable ids in `inst/ai/knowledge/`:

- concepts (`concepts.md` / `concepts.json`) — verified parameter
  names only.
- case cards (`cases/*.md`, `cases.json`) — ~10 diagnostic topics.
- troubleshooting Q&A (`troubleshooting.md`, `qa.json`) with
  confirmed / unconfirmed.
- tool registry (`inst/ai/tools.json`).

These are indexed by the default manifest as source type `knowledge`.
