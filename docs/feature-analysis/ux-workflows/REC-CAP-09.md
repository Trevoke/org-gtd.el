# UX Workflow — Themed "what's true right now?" sweep

`REC-CAP-09` · a themed, trigger-list-driven current-reality sweep that empties the head into the inbox one prompt at a time · cluster: `A — guided review & sweep sessions`

---

> **Corpus note (2026-07-08).** Two contracts this doc builds on have been adjudicated and partly shipped (PR #294). (1) **Checklists**: a checklist is a **named top-level subtree in `checklists.org`** referenced by name (no `org-gtd-checklists` store, no manager, no `kind`). (2) **Guided-session engine**: what shipped is **lean** — profiles + typed steps + `n s p q` + pause/resume — **without** the stats block, back-step `b`, or `:allowed-actions`-generated action bars this doc assumes; the shipped `checklist` step walks item *strings*, not the org-heading `walk` this sweep needs. CAP-09 remains unimplemented; when built, inherit the **lean** engine, not the full console. See design doc §8.

## 1. The need (what & why)

GTD's mind sweep (WF-03) says: when spontaneous capture slows, walk an **incompletion trigger list** item by item so each prompt "jogs loose" commitments you're holding but haven't externalized. org-gtd has zero support for this today — no trigger list, no guided sweep (grep: zero checklist hits). REC-CAP-09 is the *themed* case: a present-tense **"what is true right now?"** sweep — a named trigger list whose prompts are current-reality questions ("what projects are active right now?", "who are you waiting on right now?"). The user hits this when their head still holds unrepresented commitments — during a weekly review, after a trip, or any time the inbox feels lighter than reality. It is pure **capture**: defer all judgment, go for quantity, route the batch afterward.

Source: `REC-CAP-09` (Not-implemented) · falls out of V-10 checklist support · book: B2-W-04 (mind sweep / trigger list).

## 2. Entry points & discovery

- **Invoke** — `M-x org-gtd-capture-sweep`, or from **`org-gtd-command-center`** [R] under a new **Reflect/Sweep** entry: `w` ("**w**hat's true now — run a sweep"). Cluster-A rule: entry is always the command center; a **profile picker** appears only when more than one sweep profile exists; an in-progress sweep offers **Resume**.
- **Discover** — the command-center row is the primary affordance. Secondarily, the mind-sweep is the natural "next thing" after capture: when `org-gtd-process-inbox` finds the inbox empty, its teaching message points at `org-gtd-capture-sweep` ("Head still full? Run a sweep."). Trigger-list *content* is discoverable in the CHK-01 manager (Cluster E) as a first-class named list.

## 3. Full-lifecycle walkthrough

The CRUD object here is the **trigger list / sweep profile** (owned by REC-CHK-01, Cluster E); the *session* is the run. Lifecycle verbs map to the session.

- **Create / start** — command center → `w`. One profile ("What's true right now?") ships bundled, so no picker; the **\*GTD Sweep\*** console takes the full frame (window config snapshotted). Phase 1 ("Professional") step 1 shows the first present-tense prompt.
- **See / preview** — three fixed Cluster-A widgets (identical to someday-review / REF-02): **(1)** phase/step tracker (`Phase 1/2 · Professional · Step 4/11`); **(2)** live **stats block** — for a capture sweep the load-bearing number is **Captured this run**, redrawn after every capture, alongside Inbox count and the shared X-15 `Next actions: 41 / ≥50` completeness readout; **(3)** header-line advertising the step's keys. Body shows the current prompt large, with the sweep coaching line ("dump each one; go for quantity").
- **Primary loop** — read prompt → press **`c`** → a one-line capture minibuffer opens; type the item, `RET` files it to `inbox.org` (stamped `ORG_GTD_CAPTURED_AT`) and **reopens immediately** so you can dump several per prompt (quantity over quality). Empty `RET` closes the capture line. `n`/`SPC` advances to the next prompt.
- **Edit / reconfigure** — `,` (comma) → customize this profile: reorder/enable phases, swap which trigger list drives it. The *prompt text itself* is edited in the CHK-01 checklist manager (each checklist item = one prompt step) — not re-authored here.
- **Save / name / recall** — `p` pauses: state persists to the shared session state-file and the window config restores; re-entry offers **Resume**. Profiles are **named defcustoms**, so a user recalls "Quick sweep" vs "Full sweep" vs "What's true right now?" by name from the picker. A completed run appends to the shared completion log.
- **Delete / back out** — `b` steps back a prompt; `s` skips this prompt for this run; `q` → **Pause / Abandon** prompt (abandon discards run state, keeps everything already captured — capture is never rolled back). Window config restores on every exit path.
- **Close with routing choice** (WF-03) — after the last prompt the console shows: **`Process now, or leave in inbox?`** — `p` hands off to `org-gtd-process-inbox`, `l` just ends. This is the one place the sweep leaves the capture idiom for the pipeline.
- **Repeat / recur** — a sweep is re-runnable any time; a profile can be surfaced on a cadence via a repeating tickler (REC-CHK-02 territory) — noted, not built here.

## 4. Interaction sketch

**Console + capture sub-prompt**

```
┌─ *GTD Sweep* ──────────────────────────────────────────────┐
│ Sweep: What's true right now?     Phase 1/2 · Professional  │
│ ▸ Step 4/11  ●●●○○○○○○○○                                    │
│─────────────────────────────────────────────────────────── │
│                                                             │
│    What projects are active right now —                     │
│    started, but not finished?                               │
│                                                             │
│    (dump each one into your inbox; go for quantity)         │
│                                                             │
│─────────────────────────────────────────────────────────── │
│  Captured this run: 7    Inbox: 12    Next actions: 41 /≥50 │
│─────────────────────────────────────────────────────────── │
│ c capture  s skip  b back  n/SPC next  p pause  q quit  , ⚙ │
└─────────────────────────────────────────────────────────────┘

  ↓ press c
Capture (Professional ▸ active projects) ▸ Q3 pricing revamp▮
[RET files to inbox + reopens for the next · empty RET closes]

  ↓ last step → n
Sweep complete — 23 captured this run.
  Process now, or leave in inbox?   p process   l leave
```

**Keymap**

| key | action |
|---|---|
| `c` | capture an item to `inbox.org` (reopens for the next) |
| `n` / `SPC` | advance to next prompt |
| `b` | back one prompt |
| `s` | skip this prompt for this run |
| `p` | pause (persist + restore windows; Resume later) |
| `q` | quit → Pause / Abandon |
| `,` | customize this sweep profile |

The action bar is **generated from each prompt step's `:allowed-actions`** (`'(capture skip)` for a pure sweep). The *same* bar renderer shows `defer`/`clarify` for someday-review — CAP-09 simply declares a capture-heavy allowed set, so `defer`/`clarify` don't appear.

**Live preview** — the **Captured this run** counter and **Inbox** count tick up the instant a capture minibuffer `RET`s, before the sub-prompt reopens; the step-tracker dots fill as you advance. No other panel recomputes mid-prompt.

## 5. Fit with org-gtd

- **Extends** — the **Cluster-A guided session engine** (`org-gtd-reflect-someday-review` idiom [U]) as a *profile*, and the **capture pipeline**: the `c` action reuses `org-gtd-capture` → `org-gtd-inbox-path`, with the `ORG_GTD_CAPTURED_AT` finalizer [R]. Content comes from the **CHK-01 checklist type** (each checkbox item → one prompt step). Home in **`org-gtd-command-center`** [R].
- **Shared surface / cluster** — Cluster A. The three console widgets, the `n/b/s/p/q/,` key vocabulary, the pause/resume state-file, and the completion log **must be pixel- and key-identical** to REC-REF-02, REC-REF-06, and someday-review. The per-item **action bar must feel identical** to someday-review and to Cluster B's accept/veto/skip — CAP-09 differs only in which actions are *allowed*. The X-15 completeness stat is the shared readout, surfaced, not owned.
- **Reuse vs. new** — reused: session engine, capture-to-inbox, window snapshot/restore, checklist store (CHK-01). Genuinely new: the **capture-centric prompt step** (a `:type prompt` step whose primary action is capture-into-inbox and *loops* rather than consuming one item), and the **closing routing choice** (process-now / leave).
- **Release tag** — leans on `[U]` session engine, `[R]` command center, `[U]` CHK-01 checklist type, and `[R]` capture/inbox. No `[R]` rework required.

### Type / extension-UX opportunities

1. **Checklist item → prompt step adapter.** CAP-09 shows a clean bridge: a CHK-01 checklist item wants an optional **`:prompt` slot** (present-tense phrasing) and an **`:allowed-actions` slot** so the *same* checklist can drive both a review step-list and a capture sweep. Formalizing "a checklist is a sequence of session steps" makes trigger lists, verb lists (PRJ-10), and maintenance lists (REF-06) one mechanism.
2. **Profile-as-defcustom = thin adapter over a checklist.** A sweep profile is little more than `(checklist-ref + phase grouping)`. This argues for the session engine reading its steps from a checklist object rather than a hand-authored phase list — reducing the "three-places-in-sync" authoring friction the primer flags for the organize transient.
3. **Capture as a first-class `:allowed-action`.** Making `capture` a declared step action (not special-cased) keeps the action bar fully registry-generated across all Cluster-A/B members.

## 6. Edge cases & failure modes

- **Empty state** — no sweep profile defined: command center still offers `w`; running it seeds the bundled "What's true right now?" list and teaches ("No custom sweeps yet — running the built-in current-reality list. Edit it in the checklist manager."). An empty checklist → the console shows one coaching card, not a blank frame.
- **Bad input** — capture minibuffer can't produce a "bad" item (that's the point: no judgment). A profile that references a deleted checklist fails **soft**: header-line message "Sweep 'X' points at a missing list — pick another or restore it," never a stack trace. Very long lists (100+ prompts) stay usable — the tracker shows `Step 4/117`; `p` lets you pause and resume across sessions (WF-03 notes a full sweep can take hours).
- **When it goes wrong** — a capture that fails to file (inbox unwritable) surfaces via `message`/header-line with the path; the prompt stays put so nothing is lost. Quit-during-capture keeps every item already filed.

## 7. Open questions & maintainer decisions

- **Command name** — `org-gtd-capture-sweep` vs `org-gtd-sweep` vs folding under a generic `org-gtd-reflect-sweep`. Naming convention favors `capture-` (the step it serves), but Cluster-A entry is the command center.
- **Phase grouping** — do bundled prompts split Professional/Personal (book default) or by the user's `org-gtd-areas-of-focus`? The latter is more native but requires areas to be configured.
- **Routing default** — should the closing choice default to `leave` (safer, matches "capture ≠ process") or `process`? Recommend `leave`.
- **Should the sweep counter feed X-15?** Captured-this-run is transient; the ≥50 readout counts filed next-actions. Confirm they stay distinct rows.

## 8. Provenance & links

`REC-CAP-09` · Not-implemented · `gap-implementation-strategies.md` §1 (Incompletion Trigger List artifact; the single most-referenced missing content) + §10 checklist umbrella (build route: CHK-01 checklist type; capture via `org-capture` templates) · workflows: `WF-03` (mind sweep w/ trigger lists), `WF-04` (higher-horizon sweep) in `capture.feature` · siblings: Cluster A `REC-REF-02` (session-engine contract, authoritative), `REC-REF-06`, `REC-X-15` (completeness stat); upstream dep `REC-CHK-01` (Cluster E, trigger-list data model).
