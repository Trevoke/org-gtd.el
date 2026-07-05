# UX Workflow — Meeting lifecycle object

`REC-AGE-03` · "the meeting happened — now what came out of it?" · cluster: `G — activity-boundary harvest`

> The complete UX for treating a meeting as a *thing with a lifecycle* — scheduled, then occurred, then swept for outcomes — rather than a dead calendar entry you forget to mine.

---

## 1. The need (what & why)

A meeting isn't done when it ends — it's done when its outputs are *captured and clarified*. GTD is explicit (`capture.feature`: "annotated meeting agendas go into the in-basket after the meeting"): every call/meeting leaves action items, waiting-fors, and reference notes that must reach the in-tray or they leak. Today org-gtd has nothing meeting-shaped; a meeting is at best a bare Calendar item that silently expires. The gap: **no closure loop that, when a meeting reaches "occurred," asks "what came out of that?" and routes each answer to `inbox.org`.**

They hit it constantly — every scheduled conversation — and hardest at Weekly Review, when they try to reconstruct outcomes from memory of a meeting three days gone.

Source: `REC-AGE-03` (Not-implemented, `tool/may`, DA94-32/W06).

## 2. Entry points & discovery

- **Create** — `org-gtd-organize` transient, new **`m` meeting** disposition (sits beside `c` calendar). Also reachable standalone via `org-gtd-clarify-item` → `C-c C-o` → `m`.
- **Close (the harvest)** — two doors: `org-gtd-meetings-close` (guided session over all occurred-unharvested meetings) and, ad-hoc on point, `org-gtd-meeting-close`.
- **Discover** — a meeting whose time has passed surfaces automatically in `org-gtd-engage`'s **Today's Schedule** block with an inline hint `⟶ occurred · close to harvest`, and in `org-gtd-command-center` under a new **`m` Meetings to close** Reflect entry (badged with the count). No manual required — the moment you engage, ripe meetings advertise themselves.

## 3. Full-lifecycle walkthrough

**Create / start.** In the clarify WIP buffer, `C-c C-o` → `m`. Three prompts (calendar's `when`, plus meeting extras):
1. `Meeting when? (org date):` → appointment timestamp (same input as Calendar).
2. `Attendees (who, TAB completes):` → sets `WHO` / person-tags (shared vocabulary with REC-AGE-01/02).
3. Agenda body is just the WIP heading's text — type talking points freely, `C-c C-o` files it.
Files to `org-gtd-calendar.org` as a **Meeting** with `ORG_GTD_MEETING_STATE: upcoming`.

**See / preview.** Because it carries an appointment timestamp it renders in engage's `calendar-day` block like any calendar item. Once its time passes and it's still `upcoming`, the DSL computed state flips to *occurred* and the item gains the `⟶ occurred · close to harvest` prefix hint.

**Edit / reconfigure.** `C-u org-gtd-clarify-item` (update-in-place) on the meeting pulls it into a WIP buffer to change time, attendees, or agenda before it occurs — no refile, restores your window config.

**Save / name / recall.** It persists as a real heading; recall via the engage schedule, the `command-center → m` list, or a `(type . meeting)` DSL view.

**Delete / undo / back out.** A meeting that *won't* happen → in the close session press `x` **cancelled**: state → `cancelled`, **no harvest**, archived like any done item. During harvest, `C-c C-k` aborts the whole session leaving that meeting still open (nothing lost — it re-appears next time). Trash disposition (`t`) at create time drops it outright.

**Repeat / recur.** A standing meeting = a Habit-style repeating appointment; each occurrence closes independently. (Recurrence rides the existing repeating-timestamp descriptor — not re-invented here.)

### The harvest (occurred → outcomes to inbox) — the cluster-G core

On `close`, org-gtd asks the **identical question CAP-08 asks at clock-out**: *"Anything to capture?"* → a rapid multi-line capture loop; each line becomes an `inbox.org` heading stamped `ORG_GTD_CAPTURED_AT` plus a `ORG_GTD_MEETING_SOURCE` backlink. Meeting state → `closed`. The outcomes are now ordinary inbox items — `org-gtd-process-inbox` clarifies them like anything else. **No special outcome types**; the harvest only *feeds the funnel*.

## 4. Interaction sketch

**Create (organize transient) [R], `m` added:**
```
 Organize this item                      (C-c C-o)
 ─────────────────────────────────────────────────
  Actionable
   q quick   s next-action   d delegate
   c calendar   m meeting ◀ new   h habit
  Project        p new   a add-to-existing
  Non-actionable i tickler  y someday  k knowledge  t trash

 m ▸ Meeting when? (org date): 2026-07-08 14:00
     Attendees (WHO, TAB completes): dana, priya
     [agenda = WIP body]  C-c C-o to file
```

**Close session (`org-gtd-meetings-close`) — same session-engine chrome as `someday-review` [R]:**
```
 Meetings awaiting closure                         [1/2]
 ══════════════════════════════════════════════════════
   Q3 planning sync        2026-07-08 14:00  w/ dana, priya
 ──────────────────────────────────────────────────────
  c capture outcomes   x cancelled   s skip   q quit
                                          closed: 0  swept: 0
 › c

 Anything to capture from "Q3 planning sync"?
   outcome (RET on empty to finish):
   › Email Dana the revised budget            ⏎
   › Book follow-up room                       ⏎
   ›                                           ⏎
   2 items → inbox.org.  Meeting closed.       ▸ next [2/2]…
```

**Keymap**

| Key | Where | Action |
|---|---|---|
| `m` | organize transient | create meeting from item |
| `C-c C-o` | meeting WIP | file the meeting |
| `C-u M-x org-gtd-clarify-item` | on meeting | edit in place |
| `c` | close session | harvest outcomes → inbox |
| `x` | close session | mark cancelled, no harvest |
| `s` | close session | skip (stay open) |
| `q` / `C-c C-k` | close session | quit / abort |
| `m` | `org-gtd-command-center` | open Meetings-to-close |

**Live preview.** As you type each outcome line, a right-margin counter updates (`→ 2 items`), and on finish the engage schedule block re-renders that meeting from `⟶ occurred` to struck-through `closed`.

## 5. Fit with org-gtd

- **Extends** — organize transient (`org-gtd-organize`, new `m`) [R]; Calendar type / appointment timestamp + engage `calendar-day` block [R]; clarify + WIP for editing [R]; the guided one-at-a-time **session engine** (`org-gtd-reflect-someday-review`, IMPL-084) [R] for the close walk; **continuation + WIP infra** [R]; capture finalizer / `ORG_GTD_CAPTURED_AT` + inbox routing [R]; view DSL computed filters [R] extended with a `meeting` type and an `occurred` predicate [U].
- **Shared surface / cluster (G)** — the **harvest-to-inbox helper** is the shared contract with `REC-CAP-08`: both must call one function `org-gtd-capture--harvest-to-inbox` presenting the *identical* "Anything to capture?" multi-line loop and landing in `inbox.org`. AGE-03's entry is the **meeting-object transition** (occurred), CAP-08's is an **`org-clock-out` hook** — different triggers, byte-identical harvest moment, prompt idiom, and landing. If they drift, the cluster is broken. I confirm the §5/§8-vs-§9 build split but reunite the UX exactly as `_CLUSTERS.md` mandates.
- **Reuse vs. new** — reused: transient, calendar timestamp, WIP, session engine, inbox capture. Genuinely new: the **lifecycle state machine** (`upcoming → occurred → closed/cancelled`) and the shared harvest helper.
- **Release tags** — leans mostly on [R]; the `occurred` DSL predicate and the shared harvest helper are net-new [U] (fair for v5). The one [R] rework — adding `m` to the organize transient — is justified on **GTD fidelity**: a meeting is a bounded activity whose *closure loop* no existing disposition provides.

### Type / extension-UX opportunities

Strong one. Meetings expose that **types have no notion of a state lifecycle or a close event.** Propose two registry additions, designed here and reused by CAP-08 / REC-PRJ-12:
1. `:lifecycle` — a type may declare ordered states + a computed predicate (`occurred` = appointment past ∧ not closed), mirroring how `stuck-*` are computed in the DSL. This lets "Meetings to close" be a plain `(type . meeting) (when . past) (state . open)` view instead of bespoke code.
2. `:on-close` slot — a type-level closure action (here: fire the harvest). Generalizes the cluster-G harvest so *any* bounded type gets "what came out of that?" declaratively.
Both are the natural shape for the resurrected [X] `define-type`: `(org-gtd-define-type 'meeting :lifecycle '(upcoming occurred closed cancelled) :on-close #'org-gtd-capture--harvest-to-inbox …)`. And per the primer's hand-sync warning, the new `m` row is one more argument to **generate the transient from the registry** — flagged, not hand-maintained.

## 6. Edge cases & failure modes

- **Empty state** — no occurred meetings: `command-center → m` and `org-gtd-meetings-close` show `No meetings awaiting closure.` (teaching voice), then return you to whence you came.
- **Occurred, nothing came out** — press RET on the first empty outcome line → `Nothing captured. Meeting closed.` A blameless zero-outcome close, not an error.
- **Bad input** — malformed date reuses org's own date reader (re-prompts, never crashes); an attendee not in your person list is accepted as a free tag with a `+` hint.
- **Duplicate closes** — a meeting already `closed` is skipped by the session and shows `already closed` if targeted directly; the harvest is idempotent (backlinked items aren't re-created).
- **Aborted harvest** — `C-c C-k` mid-loop keeps items already sent, leaves the meeting `open`, restores window config; it simply re-surfaces next engage. Nothing is silently lost.

## 7. Open questions & maintainer decisions

- **New type vs. Calendar flavor?** I recommend a genuine `meeting` type for the lifecycle; alternative is a `MEETING` flag on Calendar items. Maintainer's call on whether the lifecycle justifies the new type (§3 primer bar).
- **Auto-detect occurrence** or require explicit close? Proposed: passive detect (time passed) surfaces it, user still consents to close — never auto-harvest. Confirm the no-auto-act stance.
- **Should the `:on-close` / `:lifecycle` slots land in v5's `define-type`** now, or be prototyped meeting-only first? This is the biggest scoping fork.

## 8. Provenance & links

`REC-AGE-03` · Not-implemented · `gap-implementation-strategies.md` §8 (New first-class objects: "Type registry + closure hooks; bespoke §4") · workflow `capture.feature` "Bookmark open threads in calls and meetings" · cluster **G** sibling `REC-CAP-08` (shared harvest helper) · adjacent `REC-AGE-01/02` (person/agenda vocabulary reused for attendees) · type-UX contribution reused by `REC-PRJ-12`.
