# UX Workflow — Trackable-channel guidance in the delegate prompt

`REC-DEL-05` · nudge the handoff toward the most *trackable* channel at the moment of delegating · cluster: `C — delegate-flow enrichments`

---

## 1. The need (what & why)

GTD's WF-09 says a handoff should use the most trackable channel available, in preference order: **e-mail → written note routed with the item → text/voicemail → an Agenda-list item for the next conversation → direct interruption (last resort)**. A less-trackable channel means a delegation you can't verify was received, which erodes trust in the Waiting-For list. Today org-gtd's delegate flow (`d`) captures *who* and *when to check in* but says nothing about *how* to hand off — so the methodology's single most actionable delegation tip is absent at the exact moment the user acts on it.

They hit this every time they choose delegate during clarify/organize — a per-item decision, not a rare one. Source: `REC-DEL-05` (Not-implemented; `methodology/may`, audit add, "docs-only candidate" — folded into the prompt per Cluster C, not left as a manual paragraph). Book anchor: WF-09 handoff hierarchy.

## 2. Entry points & discovery

- **Invoke** — unchanged: organize transient `d` (`org-gtd-delegate`), during a clarify/organize session or standalone (DWIM) on a heading/agenda item. Guidance and the optional channel capture appear *inside* the existing who/when prompt sequence — no new command, no new key.
- **Discover** — the guidance is self-revealing: it rides the `DELEGATED_TO` prompt's help line and the channel completion candidates are pre-ordered most-trackable-first, so the ranking teaches itself the first time the user delegates. No manual reading required. The captured value later shows up in the delegated follow-up view (§3), reinforcing it.

## 3. Full-lifecycle walkthrough

**Primary path** (delegate an item, capturing the channel):

1. **Create / start** — user hits `d` in the organize transient. Delegate flow prompts, in order (the Cluster-C canonical sequence): **who → when → channel**.
2. Prompt 1 — `Who will do this?` (`DELEGATED_TO`, unchanged, required).
3. Prompt 2 — `When to check in?` (`ORG_GTD_TIMESTAMP`, unchanged, required, `org-read-date`).
4. Prompt 3 (**new, optional**) — `How did you hand this off?` a `completing-read` whose candidates are the WF-09 hierarchy, **ordered most-trackable-first**, with an inline guidance annotation. The user picks one, types a free-form channel, or presses `RET` on empty to skip entirely. Skipping is first-class — the guidance was still surfaced.
5. **See / preview** — the choice is stored as `DELEGATED_VIA` on the entry and echoed in the minibuffer (`Delegated to Dana via e-mail; checking in <2026-07-11>`). In the delegated follow-up view it renders in the prefix/column so a quick scan shows which handoffs are trackable vs. shaky.
6. **Edit / reconfigure** — re-run `d` on the item to re-prompt (existing values pre-fill as defaults), or edit the `DELEGATED_VIA` property line directly in the WIP/org buffer. No bespoke editor.
7. **Save / name / recall** — persists as an org property; travels with the heading into `org-gtd-tasks.org`; recalled every time the item surfaces in the delegated view or engage's "delegated to check in on" block.
8. **Delete / undo / back out** — `C-c C-k` aborts the whole delegate/clarify step (universal). To drop just the channel, clear the `DELEGATED_VIA` property. Skipping prompt 3 leaves no property, which is indistinguishable from "not recorded."

**Variations** — DWIM/agenda invocation and non-interactive `(org-gtd-delegate delegated-to checkin-date)` behave identically; when a `:channel` value is passed in `config`, no prompt fires (same contract as `:who`/`:when`). Delegating a whole project (DEL-03) or plan-to-delegate (DEL-04) reuse the *same* three-prompt sequence — channel guidance appears once, on the collapsed Waiting-For.

## 4. Interaction sketch

**Mock** — the delegate prompt sequence (organize transient already dispatched via `d`):

```
Who will do this? > Dana
When to check in?  > +1w            ⇒ <2026-07-11 Sat>

How did you hand this off?  (most trackable first — RET to skip)
┌──────────────────────────────────────────────────────────────┐
│ e-mail                       ← leaves a written, searchable trail│
│ written note (routed w/ item)                                    │
│ text / voicemail                                                 │
│ agenda item for next talk    ← track on your Agenda list         │
│ real-time interruption       ← last resort, least trackable      │
└──────────────────────────────────────────────────────────────┘
> e-mail

  Delegated to Dana via e-mail; checking in <2026-07-11 Sat>.
```

Resulting entry:

```
** WAIT Ask Dana to review the Q3 draft
   :PROPERTIES:
   :DELEGATED_TO:  Dana
   :DELEGATED_VIA: e-mail
   :ORG_GTD_TIMESTAMP: <2026-07-11 Sat>
   :END:
```

**Keymap** — no new bindings; the flow rides existing ones.

| key | action |
|-----|--------|
| `d` | organize transient → delegate (opens the who/when/channel sequence) |
| `TAB` | complete against the ordered channel candidates |
| `RET` (empty) | skip channel capture (guidance already shown) |
| `C-c C-k` | abort the delegate/clarify step |

**Live preview** — n/a (a prompt sequence, not a live-preview manager). The closest analog is the minibuffer echo confirming who/when/channel after the sequence completes.

## 5. Fit with org-gtd

- **Extends** — the **organize transient** delegate disposition (`org-gtd-delegate` → `org-gtd--dispatch 'delegated`) and the **type property-descriptor** loop in `org-gtd-configure-as-type` (`org-gtd-configure.el`). The channel is a new `:channel` descriptor on the `delegated` type (`org-gtd-types.el`), org-property `DELEGATED_VIA`, with an `:input-fn` doing the ordered `completing-read`.
- **Shared surface / cluster (C)** — the delegate prompt sequence and `DELEGATED_TO`/WAIT vocabulary shared with `REC-DEL-03` (delegate whole project) and `REC-DEL-04` (plan-to-delegate). **Must feel identical:** the who/when/channel order, the prompt wording, and the follow-up view rendering are the same regardless of whether the user delegates an item, a project, or plans to. DEL-05 owns only the channel prompt + guidance text; DEL-03/04 inherit it unchanged. This confirms the Cluster-C build-route reunification from a UX standpoint.
- **Reuse vs. new** — reused: the descriptor prompt loop, `completing-read`, DWIM dispatch, the delegated follow-up view. Genuinely new: one property descriptor, the ordered candidate list with guidance annotations, and (see below) prompting a *non-required* property.
- **Release tag** — leans on `[R]` released surfaces: the delegate disposition, `:input-fn` and property descriptors (4.6.1). No `[R]` rework needed — the who/when prompts are untouched; channel is purely additive. The one genuinely new capability (prompting an optional property) is a small `[U]` refinement, justified below.

### Type / extension-UX opportunities

Designing this exposes a real gap: `org-gtd-configure-as-type` **only prompts `:required` properties** (`org-gtd-configure.el:107`). A trackable channel should be *offered, never forced* — so it can't be `:required t`, yet it needs to be prompted. Two v5-worth refinements fall out:

1. **A `:prompt` / `:optional-prompt` semantic on descriptors** — let a descriptor opt into interactive prompting without being mandatory (prompt fires, empty input = skip, no property written). This is the clean general fix and unblocks any future "offered but optional" field (energy, cost, location).
2. **Descriptor-level guidance text** — a `:guidance` slot rendered as the completion annotation / prompt help line, so methodology tips travel *with* the type definition rather than being hard-coded in a command. This makes user-defined types (`org-gtd-user-types`) able to carry their own coaching text — a small but real improvement to the extension model's expressiveness.

Both keep the transient/type registry as the single source of truth and avoid a bespoke delegate-only prompt branch.

## 6. Edge cases & failure modes

- **Empty state** — user skips channel (`RET`): no `DELEGATED_VIA` written, item delegates normally. Guidance was still shown; no nag, no error.
- **Bad / free-form input** — `completing-read` allows arbitrary text ("Slack DM"), stored verbatim; the ordered list is a nudge, not a whitelist. No validation error — matches the fail-soft voice.
- **Non-interactive / config-supplied** — `(:channel . "e-mail")` in the dispatch config sets it silently, no prompt (identical to `:who`/`:when`).
- **When it goes wrong** — if the descriptor-prompt refinement isn't in place, the fallback is guidance-only: the annotation still rides the *who* prompt help line and no property is captured — degrades to the original "docs-in-the-prompt" scope without breaking. Errors in `:input-fn` are logged via `message`, delegation still completes.

## 7. Open questions & maintainer decisions

- **Capture the channel, or guidance-only?** Cluster C's contract implies a real `channel` prompt; the original disposition was "docs-only." Recommend capturing `DELEGATED_VIA` (enables the trackable-vs-shaky scan in the delegated view) — but confirm the added property is wanted vs. pure inline guidance text.
- **Prompt every time vs. remember a per-person default?** Should the channel default to the last channel used for that `DELEGATED_TO`? (Nice, but adds state — likely a phase-2.)
- **Should `DELEGATED_VIA` surface in the engage delegated block by default,** or only in the reflect delegated-review view?

## 8. Provenance & links

`REC-DEL-05` · Not-implemented (`methodology/may`, audit add, docs-only candidate) · `gap-implementation-strategies.md` §5 (docs-only) / row "Trackable-channel guidance" — build-route hint is "one paragraph"; this doc folds it into the prompt per Cluster C · workflow `WF-09` (`clarify.feature:137` handoff hierarchy) · siblings `REC-DEL-03`, `REC-DEL-04` (Cluster C, shared delegate prompt sequence).
