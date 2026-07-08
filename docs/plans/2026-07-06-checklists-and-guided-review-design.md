# Checklists and Guided Weekly Review — Design

**Date:** 2026-07-06
**Implements:** REC-CHK-01 (first-class checklists / trigger lists), REC-REF-02 (guided three-phase Weekly Review)
**Supersedes in part:** `docs/feature-analysis/ux-workflows/REC-CHK-01.md`, `REC-REF-02.md` — see "Divergences" below.

## Summary

Two features, designed together because a review step can walk a trigger list:

- **Checklists**: reusable named lists (Weekly Review trigger list, packing lists) stored as
  plain org headings in `checklists.org`. One file convention, one insert command, one
  checkbox-reset hook. No new GTD type, no manager UI, no new properties.
- **Guided review**: `M-x org-gtd-review` runs a customizable multi-phase session
  (Get Clear → Get Current → Get Creative by default) in a `*GTD Review*` buffer,
  with pause/resume. Step sequences ("profiles") live in a defcustom.
- **Installation**: `org-gtd-review-schedule` seeds a recurring Weekly Review habit;
  `org-gtd-init-system` is a thin idempotent concierge for first-time setup.

### Key decisions (divergences from the UX-workflow docs)

| Decision | UX doc said | This design says | Why |
|---|---|---|---|
| Checklist instances | New `checklist` type in registry, `ORG_GTD: Checklist` | Plain org subtrees, no type | YAGNI; avoids registry/organize-transient churn; reset hook works for any heading |
| Template store | `org-gtd-checklists` defcustom + transient CRUD manager | `checklists.org`, bare headings | Editing an org file is the ultimate no-elisp UX; syncs to mobile/git; deletes the whole manager surface |
| `CHECKLIST_KIND` property | kind: trigger-list/packing/verb-starter | Dropped | Nothing consumes it; consumers reference checklists by name |
| `RESET_CHECK_BOXES` property | Per-heading opt-in | Dropped | The repeater is the signal: reset fires only when a heading repeats |
| Recurrence mechanism | org-edna `RESET` | Plain org repeaters | Matches how habits already work; edna stays project-dependency-only |
| Manager UI (Cluster E CRUD) | Full list/builder/preview transient pair | Visit-file command + `org-gtd-checklist-insert` | Follows from org-file storage; decouples from the View Builder work |
| Command-center key | `k` "Checklists…" | `l` (and `w` for review) | `k` is already taken (Capture & Process group) |
| Engine scope | Full console: stats block, review log, back-step, someday-review generalized | Lean session + pause/resume | Stats/log/back-step layer on later; `someday-review` untouched |

## 1. Checklists

### 1.1 Template file: `checklists.org`

Lives in `org-gtd-directory`, created lazily via `org-gtd--ensure-file-exists`
(which already takes `initial-contents`). Seeded on first creation with starter
templates: **Weekly Review triggers** and **Mind sweep prompts** (content adapted
from the GTD trigger lists; editable, deletable).

Each top-level heading is a template:

```org
* Weekly Review triggers
- [ ] Boss?
- [ ] Car maintenance?
- [ ] Promises to family?

* Beach packing
- [ ] Sunscreen
- [ ] Swimsuit
- [ ] Beach towel
```

- Name = heading title; items = its `- [ ]` checkboxes.
- No TODO keywords, no properties, nothing org-gtd-specific.
- Not added to `org-agenda-files` (templates are content, not tasks).
- Users organize and extend the file with ordinary org editing — that *is* the
  customization UX.

### 1.2 `org-gtd-checklist-insert` (autoloaded)

`completing-read` over template headings, then copies the chosen subtree to point.
**Pure copy** — no repeater prompt, no properties added. A spawned instance is an
ordinary org subtree; org-gtd tracks nothing about it. (Prefix-arg refile-style
targeting was considered and deferred — see §7.)

### 1.3 Checkbox reset on repeat

In `org-gtd-hooks.el`, installed by `org-gtd-mode` like the existing hooks:
when a repeating heading re-arms (repeater fires on DONE, org resets it to TODO),
call org core's `org-reset-checkbox-state-subtree` on it. Fires **only** on
repeater re-arm, never on a plain DONE. Core org already handles nested boxes
and `[1/3]` statistics cookies. Applies to any repeating heading with checkboxes,
spawned or hand-written. ~20 lines.

### 1.4 Reading a checklist as data

Internal `org-gtd-checklist--items NAME` → ordered list of item strings parsed
from the named subtree. Consumed by the review engine's `checklist` step; future
consumers (e.g. project verb lists, PRJ-10) reference templates by name the same way.

### 1.5 Recurring checklist-as-task = composition

The workflow for "I want my trigger list as a recurring task in my system":
`org-gtd-checklist-insert` into the inbox (or any heading), then clarify and
organize it **as a habit**. The checkboxes ride along with the subtree, engage
shows it because it is a real habit, and the reset hook (1.3) clears the boxes
each time the repeater fires. Same pattern for a recurring packing list. The
manual documents this as *the* pattern; no new code.

## 2. Guided review engine

### 2.1 Profiles: `org-gtd-review-profiles`

A defcustom: alist of `profile-name → list of phases`; each phase is
`(phase-name . steps)`; each step a small plist. The **Weekly Review ships as
the default value**, so customization = editing the variable (setq or customize).

```elisp
(setq org-gtd-review-profiles
 '(("Weekly Review"
    ("Get Clear"
     (:title "Gather loose papers and materials" :type prompt
      :instruction "Collect everything loose into your inbox.")
     (:title "Mind sweep" :type checklist
      :checklist "Weekly Review triggers")
     (:title "Inbox to zero" :type command
      :command org-gtd-process-inbox))
    ("Get Current"
     (:title "Review calendar" :type view :view org-gtd-reflect-missed-items)
     (:title "Review Waiting-For" :type view :view org-gtd-reflect-upcoming-delegated)
     ...)
    ("Get Creative"
     (:title "Review Someday/Maybe" :type view :view org-gtd-reflect-someday-maybe)
     (:title "Any new ideas?" :type prompt
      :instruction "Capture any creative, risky, or fun ideas.")))))
```

Step types (v1):

| `:type` | keys used | behavior on `n` |
|---|---|---|
| `prompt` | shows `:instruction` | mark done, advance |
| `command` | — | `n` launches `:command`; on the user's next `n`, advance |
| `view` | — | show `:view` in other window; browse; `n` again advances |
| `checklist` | `c` capture, `n` next item | walk `:checklist` items one at a time; `c` captures to inbox mid-walk. Ephemeral — the template file is never modified |

### 2.2 Session

`M-x org-gtd-review` (autoloaded; optional profile arg; profile picker only when
more than one profile is configured). Snapshots the window configuration, opens
`*GTD Review*`: profile name, phase tracker, `Step 2/4`, current instruction.
Header line advertises the live keys — same idiom as `org-gtd-someday-review`.

Keys: `n` do/advance · `s` skip step · `p` pause · `q` quit (offers *Pause / Abandon*).

Phase boundaries show a one-line checkpoint message. Completion shows simple
counts (steps done/skipped), a one-time tip about `org-gtd-review-schedule` if
no reminder exists yet, and restores the window configuration.

### 2.3 Pause / resume

Session state (profile name, phase index, step index, position within a checklist
walk, counts) serializes to `review-state.eld` in `org-gtd-directory` (visible,
Emacs read-syntax). Next `org-gtd-review` offers *Resume / Start over*. Abandon
deletes the file. State saves at step boundaries plus walk position; pausing
mid-`command` resumes by re-offering that step (work already done — e.g. a
half-emptied inbox — is naturally reflected in the files themselves).

`org-gtd-someday-review` is untouched: the engine borrows its header-line /
read-only idioms, not its code. Generalizing it into a profile is deferred.

## 3. Installation workflows

### 3.1 `org-gtd-review-schedule`

Prompts for profile, day, and frequency; creates a habit via the existing
`org-gtd-configure-as-type` path — exactly what organizing a captured item as a
habit produces today:

```org
* Weekly Review
SCHEDULED: <2026-07-10 Fri .+1w>
:PROPERTIES:
:ORG_GTD: Habits
:STYLE: habit
:END:
Run M-x org-gtd-review when you sit down for this.
```

Not a command-center row of its own: it is reachable via `M-x`, hinted once on
the review completion screen (the one-time tip), offered by `org-gtd-init-system`,
and mentioned in the manual.

### 3.2 `org-gtd-init-system`

A thin, **idempotent** concierge for first-time setup; steps skip when already
satisfied. (As implemented, it reports one consolidated "✓ GTD files ready"
line rather than a per-step ✓ line.) Lazy init stays fully intact —
nothing ever requires having run this. Steps (deliberately minimal):

1. Ensure `org-gtd-directory`, `org-gtd-tasks.org`, and `checklists.org`
   (with starter content) exist — the same lazy-init calls, run eagerly.
2. Offer `org-gtd-review-schedule`: "Schedule a recurring Weekly Review? (y/n)".

Explicitly out of scope for now: `org-agenda-files` checks and the keywords
wizard (they remain documented manual steps / `org-gtd-setup-keywords-wizard`).

### 3.3 New-user story

Run `org-gtd-init-system` (or just `org-gtd-review` — checklists.org self-seeds
on first touch), accept the completion-screen tip, done: the system now reminds
them weekly, with a mind-sweep trigger list ready to walk.

## 4. Wiring

- **New modules:** `org-gtd-checklist.el` (insert command, items parser, seeding)
  and `org-gtd-review.el` (profiles defcustom + engine). The `org-gtd-review.el`
  filename is free — the old module became `org-gtd-reflect.el` in 4.0.0, and the
  ritual is properly called "Weekly Review" (Reflect is the phase name).
- **Autoloads:** `org-gtd-review`, `org-gtd-review-schedule`,
  `org-gtd-checklist-insert`, `org-gtd-init-system`.
- **Hooks:** reset-on-repeat in `org-gtd-hooks.el`, installed by `org-gtd-mode`.
- **Command center**, Reflect group (keys verified free):
  `w · Weekly Review (guided…)` → `org-gtd-review`;
  `l · Checklists` → visit `checklists.org`.
- **Unchanged:** type registry, organize transient + parity test,
  `org-gtd-someday-review`, org-edna usage, the View Builder yx tree (checklists
  no longer follow the Cluster-E CRUD idiom, so that work is decoupled).
- **Docs:** manual gains Checklists and Weekly Review sections (including the
  composition pattern, 1.5); the two feature-analysis UX docs get a divergence
  note pointing here.

## 5. Edge cases

All in the teaching voice — message + skip, never a stack trace.

- **Step references a missing checklist** (template renamed/deleted): show
  "No checklist named 'X' — edit checklists.org, or `s` to skip"; session continues.
- **Empty states auto-satisfy:** an empty checklist walk or a view step over zero
  items shows "Nothing here — nice"; `n` moves on. A brand-new user can run the
  full ritual.
- **Resume vs. changed profile:** if `org-gtd-review-profiles` changed since
  pausing (index out of range, profile gone), offer *Start over* instead of
  restoring into the wrong step.
- **Reset precision:** repeater re-arm only, never plain DONE.
- **Window config** restored on every exit path: complete, pause, abandon, error.

## 6. Testing

Via the `/test` skill, using existing builders and mock-gtd helpers.

- **Unit:** items parser; file seeding (content, idempotence); insert at point;
  reset hook (repeat vs. plain DONE; nested boxes); profile
  shape validation; state-file round-trip; init-system idempotence.
- **Integration:** full Weekly Review run with `with-simulated-input`, including
  a checklist walk with mid-walk capture; pause then resume across two sessions;
  `org-gtd-review-schedule` produces a working habit.

## 7. Deferred (explicitly not in this cut)

- Stats block, review-completion log, back-step (`b`), invariant guard on
  project walks (REF-02 §4/§6).
- Prefix-arg refile-style targeting for `org-gtd-checklist-insert` (v1 inserts
  at point only).
- Generalizing `org-gtd-someday-review` into a profile of this engine.
- Checklist manager transient / Cluster-E CRUD conformance.
- `CHECKLIST_KIND` (revisit if a consumer ever needs filtering by kind).
- Instance↔template linking / edit propagation.
- `org-agenda-files` and keywords steps in `org-gtd-init-system`.
- Daily/monthly/quarterly cadence profiles (ship as documented examples first).

## 8. Corpus impact — what to pull from the feature-analysis corpus later

Status of every corpus element this design touches, so future work can keep
pulling from `docs/feature-analysis/` without re-litigating. **Rejected** means
"do not implement later — the decision went the other way"; **still open** means
"valid future work; read it through this design's contracts."

### REC-CHK-01

- **Implemented (as specified):** named reusable checklists; insert command;
  bundled starter trigger lists; checkbox reset so lists are re-runnable.
- **Implemented (differently — read this design, not the corpus doc):**
  storage is `checklists.org`, not an `org-gtd-checklists` defcustom;
  reset keys off repeater re-arm, not a reset-policy field.
- **Rejected:** checklist type in the registry; `CHECKLIST_KIND` and
  `RESET_CHECK_BOXES` properties; org-edna `RESET` recurrence; the Cluster-E
  CRUD manager/builder transient pair for checklists; `ORG_GTD: Checklist`
  instances.
- **Still open:** a slim manager transient *over the file* (list/jump/insert)
  if discoverability warrants; instance↔template linking; `kind` filtering if
  a consumer ever needs it.
- **Contract for downstream corpus docs** (REC-PRJ-10, REC-CAP-09, REC-CHK-02,
  REC-CAP-06, REC-PRJ-07 all cite CHK-01's data model): a checklist is a named
  top-level subtree in `checklists.org`; consumers reference it **by name** and
  read items via `org-gtd-checklist--items`. Their references to `kind`,
  spec alists, or the manager surface are stale.

### REC-REF-02

- **Implemented (lean):** guided multi-phase session; configurable profiles
  (defcustom, Weekly default); step types `prompt`/`command`/`view`/`checklist`;
  session keys `n s p q`; phase checkpoints; pause/resume; completion counts;
  the REF-01 reminder rider via `org-gtd-review-schedule`.
- **Rejected:** org-edna involvement; hidden `.review-state.el`
  (now visible `review-state.eld`).
- **Still open (deferred, valid pulls):**
  - the **`walk` step type** — iterating *org headings* (projects, someday
    items) one at a time in WIP buffers with per-item actions (`c x d`) and the
    no-next-action **invariant guard**. Note our `checklist` step walks item
    *strings*, not headings; the org-item walk is the missing piece REF-06 and
    the someday-review generalization both need;
  - stats block (the X-15 completeness readout), review-completion log,
    back-step `b`, in-session `,` customize;
  - generalizing `org-gtd-someday-review` into a profile;
  - cadence-ladder profiles (daily/monthly/quarterly/annual, REF-05/WF-22);
  - action bars generated from `:allowed-actions` (the corpus's registry-parity
    idea) — steps currently declare behavior via `:type` only.
- **Contract for Cluster A siblings** (REC-REF-06, REC-CAP-09, REC-X-15): the
  engine they inherit is profiles + typed steps + `n s p q` + pause/resume —
  *not* the full console in the corpus doc. REF-06/CAP-09 are "just another
  profile" only once the `walk` step type lands; X-15 still needs the stats
  block.

When implementation lands, update `docs/feature-analysis/audit/` and
`gaps/recommended-not-implemented.md` from this section — the mapping above is
meant to make that mechanical.

## 9. Provenance

- `docs/feature-analysis/ux-workflows/REC-REF-02.md`, `REC-CHK-01.md` (UX corpus);
  adjudications 2026-06-05 #1 and V-10.
- Design decisions made interactively 2026-07-06 (brainstorming session):
  no-type instances → org-file templates → no manager → defcustom profiles →
  lean engine + pause/resume → habit-based reminder → minimal init-system.
