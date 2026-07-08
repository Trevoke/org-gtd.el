# UX Workflow — REC-REF-06

`REC-REF-06` · "My system has drifted — sweep out the cruft, archive the done, catch the broken references, and tell me it's trustworthy again." · cluster: `A — guided review & sweep sessions`

---

> **Corpus note (2026-07-08).** REC-REF-02's engine shipped **lean** (PR #294): profiles + typed steps (`prompt`/`command`/`view`/`checklist`) + `n s p q` + pause/resume. The **`walk` step type over org headings**, the per-item action bar generated from `:allowed-actions`, the stats block, and the back-step `b` that this maintenance profile assumes are **deferred, not built** — REF-06 becomes "just another profile" only once the `walk` step lands. REF-06 remains unimplemented. See `docs/plans/2026-07-06-checklists-and-guided-review-design.md` §8.

## 1. The need (what & why)

- GTD distinguishes the *Weekly Review* (bring lists current) from **system maintenance** — the periodic housekeeping that keeps the container itself trustworthy: purge dead someday items, archive completed project trees, and catch file-integrity rot (projects with no next action, DONE trees never archived, headings missing `ORG_GTD`, dangling org-edna triggers). Allen's rule: **cadence ≠ altitude** — maintenance runs on its own clock, not a horizon's.
- Users hit this when the system starts feeling heavy or untrustworthy — usually monthly, well outside the weekly cadence. Today `org-gtd-archive-completed-item` is single-shot (IMPL-061); there is no scaffold that walks the whole maintenance sweep, and nothing tracks *when* a subtree was last reviewed.
- Source: `REC-REF-06` (Not-implemented; Adjudicated 2026-06-05 #2 IMPLEMENT via opt-in recurring maintenance tasks) · GTD *Weekly Review / keeping the system current* · `org-review`'s `LAST_REVIEW`/`REVIEW_DELAY` convention.

## 2. Entry points & discovery

- **Invoke** — `M-x org-gtd-reflect-maintenance`, or from `org-gtd-command-center` **[R]** → **Review System** submenu → **`m` Maintenance sweep**. Because more than one session profile now exists (someday-review, REF-02 weekly, this), entry lands on the **profile picker** [U, shared engine]; picking *Maintenance* starts it (or offers **Resume** if paused).
- **Due nudge** — the review-due surface `org-gtd-reflect-due-for-review` (a view-DSL block, see §5) lists subtrees whose `LAST_REVIEW + REVIEW_DELAY` has elapsed; when maintenance itself is overdue a one-line banner appears at the top of the command center: *"System maintenance last run 47d ago — `m` to sweep."*
- **Discover** — the picker makes it a visible sibling of the reviews the user already runs; the overdue banner surfaces it without the manual. Opt-in: `org-gtd-reflect-maintenance-schedule` can inject a recurring `Habits`/calendar reminder ("Run system maintenance") into `org-gtd-tasks.org`.

## 3. Full-lifecycle walkthrough

Primary path — a full-frame `*GTD Review*` console (the shared Cluster-A engine), maintenance profile = three phases:

- **Create / start** — Command center → Review System → `m`. Window config is snapshotted. The console opens on **Phase 1 of 3 — Purge**. This is a **walk** step over flagged stale items (someday older than a threshold, `TRASH`-tagged leftovers, completed-not-archived actions). The per-item action bar offers **archive / trash / keep / skip** — nothing is removed unless the user presses the key.
- **See / preview** — Before any destructive phase acts, the first step of that phase is a **view** step: a read-only DSL block listing *exactly* what qualifies ("14 items would archive"). The running-stats widget redraws live after every action (`purged / archived / flagged / kept`), so the user always sees the sweep's effect. `p`review-only exists as a stats readout — the user can walk the whole session pressing only `skip` to see the report without touching anything.
- **Edit / reconfigure** — `,` (comma) opens the profile's customize buffer: thresholds (someday-stale age, archive scope), which integrity checks run, and `REVIEW_DELAY` defaults. Profiles are named `defcustom`s [U, shared], so edits persist across runs.
- **Save / name / recall** — `p` pauses: state file written, windows restored; re-entry offers **Resume**. On normal finish the engine stamps `LAST_REVIEW` = now on the GTD root (and on each subtree the user touched) and appends to the shared completion log. Phase 3 — **File integrity** — is a walk over structurally-broken items (project w/ no next action, all-DONE-but-open project, missing `ORG_GTD`); action bar **clarify / fix / skip**, where *clarify* routes the item into the organize dispatch [R].
- **Delete / undo / back out** — `q` → Pause/Abandon prompt [U, shared]. Abandon discards session state, restores windows, stamps nothing (so a bailed sweep does not falsely mark the system "reviewed"). Per-item actions are individually undoable via normal org `undo` before the phase closes; archive moves to `archive.org` (recoverable), never deletes.
- **Repeat / recur** — the injected recurring reminder (opt-in) resurfaces it on its own cadence; `LAST_REVIEW` tracking makes "overdue" computable.

## 4. Interaction sketch

```
┌ *GTD Review* — System Maintenance ───────────────────────────────┐
│ Phase 2 of 3 ▸ Archive        Step 2/2      [n]ext [b]ack [s]kip  │  ← tracker widget
│ swept: purged 3 · archived 8 · flagged 2 · kept 5                 │  ← stats widget (live)
│ completeness: 61 next-actions (≥50 ✓)                             │  ← X-15 readout (shared)
├──────────────────────────────────────────────────────────────────┤
│ DONE trees ready to archive (14):                                 │
│   ▸ Renew passport            ✓ closed 2026-05-11                  │
│     Ship v4 docs              ✓ closed 2026-04-02                  │
│     …                                                             │
├──────────────────────────────────────────────────────────────────┤
│ Item: “Renew passport”   [a]rchive  [t]rash  [k]eep  [.]skip      │  ← action bar (from :allowed-actions)
├──────────────────────────────────────────────────────────────────┤
│ n/SPC advance · b back · s skip step · p pause · q quit · , cfg   │  ← header-line (shared vocab)
└──────────────────────────────────────────────────────────────────┘
```

| key | action |
|-----|--------|
| `n` / `SPC` | advance step / next flagged item |
| `b` | back one step |
| `s` | skip this step this run |
| `p` | pause (persist + restore windows) |
| `q` | quit → Pause/Abandon |
| `,` | customize the maintenance profile |
| `a` | archive item (walk steps) |
| `t` | trash item |
| `k` | keep (dismiss the flag) |
| `c` | clarify → organize dispatch (integrity phase) |
| `.` | skip this item |

**Live preview:** in the Archive view step, toggling a threshold via `,` and returning re-renders the qualifying list and the "would archive N" count immediately — the DSL block is the preview surface.

## 5. Fit with org-gtd

- **Extends** — the **guided session engine [U]** (REF-02's Cluster-A contract, reshaped from `org-gtd-reflect-someday-review`, IMPL-084); the **view DSL [R]** for every view step and the due-for-review block; **organize dispatch [R]** for the integrity phase's *clarify*; **archive [R]** (IMPL-061) as the action a Purge/Archive step invokes; **command-center [R]** for its home.
- **Shared surface / cluster** — **Cluster A**. Its console (three widgets), key vocabulary (`n/b/s/p/q/,`), action bar, pause/resume, and window discipline must be **byte-for-byte identical** to REF-02, CAP-09, and someday-review; the action bar is generated from each step's `:allowed-actions`. The X-15 completeness readout is the same shared stat block. This confirms the cluster hypothesis from a UX standpoint — maintenance is *another profile*, not a bespoke flow.
- **Reuse vs. new** — reused: engine, widgets, persistence, profile-defcustom, archive command, DSL. Genuinely new: the **`LAST_REVIEW`/`REVIEW_DELAY` property convention**, the **`due-for-review` computed filter**, the **integrity-check predicates**, and the **opt-in recurring-reminder injector**.
- **Release tag** — leans on **[U]** surfaces (engine, DSL, command-center, hooks); the only **[R]** touch is `archive` (invoked as-is, no rework) and organize dispatch. No [R] rework needed.

### Type / extension-UX opportunities

Real leverage here. (1) `LAST_REVIEW`/`REVIEW_DELAY` should be **property descriptors [R]** any type can opt into, and `due-for-review` should join `org-gtd-view-lang--known-filter-keys` as a first-class computed filter + skip predicate — so review-due tracking is available to REC-REF-05 and the whole view DSL, not hardcoded in this session. (2) The file-integrity checks argue for a per-type **`:validate-fn` slot** (sibling to `:organize-fn` [U]) — "is a Projects heading well-formed?" is type knowledge; a registry of validators would let `define-type` [X] authors ship integrity rules with their type, and would generate the integrity phase's flag-list from the registry instead of a hand-written check list. Worth capturing for v5.

## 6. Edge cases & failure modes

- **Empty state** — nothing stale/archivable/broken: each phase's view step shows *"Nothing to purge — clean."* and auto-advances on `n`; the session still stamps `LAST_REVIEW` and logs a clean run.
- **Bad input / large data** — thousands of archivable items: the walk is lazy (one flagged item at a time) and the count is shown, so the user is never dumped a wall; a `keep-all`/`archive-all` bulk action on the view step is offered behind a confirm.
- **When it goes wrong** — archive target unwritable, or an integrity check errors: fail-soft per the contract — a header-line message (*"Couldn't archive 'Renew passport': archive.org read-only — skipped."*), the item is left flagged, the sweep continues; never a stack trace. Abandoning mid-sweep stamps nothing so the system is never falsely marked reviewed.

## 7. Open questions & maintainer decisions

- Should `LAST_REVIEW` stamp only the GTD root, or every subtree the user touched (finer-grained due-tracking, more property noise)?
- Does the recurring-reminder injector default **off** (opt-in per adjudication) — confirm; and does it land as a `Habits` item or a plain `Calendar` event?
- Is "archive" the right destructive default for stale someday items, or should Purge always route through *trash* and leave archiving to completed actions only?
- Command-center home: new `m` under **Review System**, or promoted into the top-level Reflect group beside the profile picker?

## 8. Provenance & links

`REC-REF-06` · Not-implemented (Adjudicated 2026-06-05 #2: IMPLEMENT) · `gap-implementation-strategies.md` build route: session-loop reshape (§ line 120) + review-due tracking `LAST_REVIEW`/`REVIEW_DELAY` (§ line 167) · workflows `WF-21` (Weekly Review), `WF-22` (cadence ladder) in `reflect.feature` · siblings: `REC-REF-02` (engine, Wave-0 contract), `REC-CAP-09`, `REC-X-15` (Cluster A); `REC-REF-05` (shares review-due tracking).
