# UX Workflow — REC-CHK-01 · first-class checklist / trigger-list object

`REC-CHK-01` · *Author, save, spawn, and recur a reusable checklist (packing list, weekly-review trigger list, project verb-list) without writing elisp* · cluster: `E — Named-object CRUD managers`

> The complete UX for turning a checklist from "a `- [ ]` list I retype every trip" into a **named object I manage once and instantiate on demand.** This is the Wave-0 **checklist data-model** contract: it defines what a checklist *is* so downstream v5 features can inherit it — PRJ-10's verb-starter list (project naming) and the guided-review trigger lists (REF-02 / CAP-09). Its *manager* UX conforms, key-for-key, to the Cluster-E exemplar `NEW-VIEW-MANAGER`.

---

## Implementation status (2026-07-08) — IMPLEMENTED (differently)

REC-CHK-01 was implemented in the checklists + guided-review work (PR #294,
unmerged at time of writing; not yet in a release). The adjudicated design is
`docs/plans/2026-07-06-checklists-and-guided-review-design.md` — **read its §8,
not this doc's mechanics**, for what actually shipped. The felt-need analysis
below (§1) stands; the concrete surface (§2–§7) was largely superseded.

- **Implemented as specified:** named reusable checklists; an insert command
  (`org-gtd-checklist-insert`, autoloaded, `completing-read` over template names);
  bundled starter trigger lists (Weekly Review triggers, Mind sweep prompts);
  checkbox reset so a list is re-runnable.
- **Implemented differently (the design doc supersedes this doc):**
  - **Storage is a plain org file, `checklists.org`** — each top-level heading is
    a template, items are its `- [ ]` boxes — **not** the `org-gtd-checklists`
    `defcustom` alist. Editing the org file directly *is* the authoring UX.
  - **There is no manager or builder transient.** The whole "manage my checklist
    interactively" surface (§2 `org-gtd-checklist-manager`, §3 builder lifecycle,
    §4 manager/builder mocks + keymap, the Cluster-E CRUD conformance) is **gone**;
    recall is by name via the insert command, discovery via a command-center
    `l · Checklists` row that visits the file.
  - **Reset keys off org repeater re-arm** (a hook calls
    `org-reset-checkbox-state-subtree` only when a repeating heading re-arms on
    DONE), **not** an `on-done`/`never` reset-policy field.
  - **Command-center key is `l`** (visit `checklists.org`), not `k`.
- **Rejected — do not re-litigate (design doc §8):** a `checklist` type in the
  registry and `ORG_GTD: Checklist` spawned instances; the `CHECKLIST_KIND` and
  `RESET_CHECK_BOXES` properties; org-edna `RESET` recurrence; the Cluster-E CRUD
  manager/builder transient pair. Everything in §2–§7 describing those (the
  manager, the builder, the `kind`/`reset`/`recur` infixes, the spec alist, the
  `org-gtd-checklists` store, the type-registry addition) is **rejected design**.
- **Still open (valid future pulls):** a *slim* manager transient over the file
  (list / jump / insert) if discoverability warrants; instance↔template linking
  (a `CHECKLIST_SOURCE` back-reference); `kind` filtering if a consumer ever
  needs it.
- **Contract for downstream corpus docs** (REC-PRJ-10, REC-CAP-09, REC-CHK-02,
  REC-CAP-06, REC-PRJ-07 all cite CHK-01's data model): **a checklist is a named
  top-level subtree in `checklists.org`; consumers reference it by name and read
  its items via `org-gtd-checklist--items`.** Any reference those docs make to
  `kind`, a spec alist, or the manager surface is stale.

---

## 1. The need (what & why)

- **Problem:** GTD leans on **reusable lists**: the Weekly Review *trigger list*, the *mind-sweep* prompts, a *packing* list per trip, a *verb-starter* list to name projects. Each is used many times, and each run should start fresh (all boxes unchecked). org-gtd has **zero checklist infrastructure** (verified by grep) — a user keeps these in a scratch file and copy-pastes, or retypes. That is the paper checklist that never got a digital drawer.
- **Who / when:** the practitioner mid-review who wants the same trigger list every Friday; the traveler who wants "Beach packing" to reappear each trip with every box reset; the project-namer who wants a menu of strong verbs. All want to **define the list once, run it many times, and have completion reset it.**
- **Source:** `REC-CHK-01` (deliverable #3, **Not-implemented**) · adjudication **V-10** (general checklist support + bundled trigger lists, near-term) · unblocks REC-CAP-06, REC-PRJ-07, REC-PRJ-10, REC-CAP-09, REC-CHK-02.

## 2. Entry points & discovery

> **Superseded (2026-07-08) — rejected design, see §8 of the design doc.** No `org-gtd-checklist-manager` and no `k`-in-hub entry shipped. Discovery is a command-center `l · Checklists` row that visits `checklists.org`; recall is `org-gtd-checklist-insert`.

- **Invoke:**
  - `M-x org-gtd-checklist-manager` — the top-level list transient (create/edit/delete/preview).
  - `org-gtd-command-center` gains a **`k` "Checklists…"** entry (`k` is free in the hub — the organize-transient's `k`=knowledge is a different surface) → opens the manager. This is the browse/discover surface.
  - `org-gtd-checklist-insert` (autoloaded) — `completing-read` over saved names, spawns a live **instance** at point. This is the recall-and-use command (parallels `org-gtd-view-run`); bind-it-yourself, no default key (primer §5). Callable from inside a clarify WIP buffer, so a checklist can be dropped into a project as it's shaped.
- **Discover:** the manager's empty state teaches itself (§6); the bundled starter checklists (weekly-review triggers, mind-sweep) ship pre-loaded so a new user *sees* worked examples the first time they open the manager, and every guided review that walks a trigger list is "one of these."

## 3. Full-lifecycle walkthrough

> **Superseded (2026-07-08) — rejected design, see §8 of the design doc.** The builder/manager lifecycle below (in-memory spec, `kind`/`reset`/`recur` infixes, save-to-`org-gtd-checklists`, spawned `ORG_GTD: Checklist` instances) did not ship. Checklists are plain top-level headings in `checklists.org`, authored with ordinary org editing.

Primary path — build "Beach packing," save, insert it into a trip project, run it, watch it reset.

- **Create / start.** In `org-gtd-checklist-manager`, press **`c`**. A fresh in-memory spec `((name . "Untitled") (kind . generic) (items) (reset . on-done) (recur))` opens the **builder** `org-gtd-checklist-manager--build`: a transient for the *metadata* fields beside an **editable checklist buffer** in the other window (a WIP-style, file-backed org buffer — window config snapshotted first, primer §5). The buffer holds the spawnable heading and its `- [ ]` items; you type items directly as `- [ ]` lines — no one-at-a-time prompt.
- **See / preview.** The right pane **is** the live preview: it always shows the checklist exactly as it will spawn (heading, `:Checklist:` tag, boxes, and — once you set a repeater — the timestamp/reset badge). Press **`RET` (Refresh)** after changing a *metadata* infix (kind, reset, recur) to re-render the heading; item text is live as you type. For a checklist, "preview" and "edit" are the same surface (a checklist's body is free-form text) — the one deliberate divergence from the view-manager's read-only agenda pane.
  1. Type three items in the pane: `- [ ] Sunscreen`, `- [ ] Swimsuit`, `- [ ] Beach towel`.
  2. `K` → kind → `packing` (completing-read: `trigger-list · packing · verb-starter · prompt · generic`).
  3. `R` → reset → `on-done` (each completed run clears the boxes).
  4. `c` → recur → `once` (a trip list re-arms manually; a Friday trigger list would be `+1w`).
- **Edit / reconfigure.** From the manager list, highlight a saved checklist and press **`e`** — re-opens the *same* builder on its stored spec. No difference between "new" and "edit" but the starting spec (mirrors clarify's open-then-edit, and view-manager exactly).
- **Save / name / recall.** In the builder press **`s`**: prompt *"Name this checklist:"* → "Beach packing." org-gtd parses the pane's `- [ ]` lines into the ordered `items` list and writes the spec to the **`org-gtd-checklists` defcustom** (a `name → spec` alist, persisted via `customize-save-variable`). Recall: `org-gtd-checklist-manager` lists it under **"Your saved checklists"**; or `org-gtd-checklist-insert RET Beach packing` spawns an instance wherever point is.
- **Insert / use (checklist-specific).** In the manager, highlight "Beach packing" and press **`i`** → prompts for a target (default: point in the last buffer, or a refile-style target) → a live subtree is spawned: `* Beach packing  :Checklist:` with the three boxes, `ORG_GTD: Checklist`. You check boxes as you pack.
- **Repeat / recur (the reset).** Mark the instance heading **DONE** → the in-house **checkbox-reset-on-DONE** hook walks the subtree and clears every `[X]` back to `[ ]`, so the list is instantly reusable. If `recur` is a repeater (`+1w`), org-edna's `RESET` re-arms the heading (TODO again, timestamp advanced) — the weekly trigger list reappears each Friday, fresh. `reset: never` leaves a spawned instance as a one-shot record.
- **Delete / back out.** In the list, highlight and press **`D`** → *"Delete checklist 'Beach packing'? (y/n)"* → removed from `org-gtd-checklists`. Deleting a *template* never touches already-spawned instances (they are ordinary org subtrees). Backing out of an unsaved build: **`C-c C-k`** discards, cleans the temp buffer, restores windows; if dirty, one guard prompt *"Discard unsaved checklist? (y/n)."*

## 4. Interaction sketch

> **Superseded (2026-07-08) — rejected design, see §8 of the design doc.** No manager or builder transient shipped; the mocks and keymap below describe rejected UI.

**Manager (list) transient** `[U]` home in command-center

```
╭─ org-gtd Checklist Manager ─────────────────────────────╮
│ Your saved checklists                                   │
│   RET Preview highlighted     i  Insert an instance     │
│   →  Weekly Review triggers  (trigger-list · 12 · +1w)  │
│      Mind-sweep prompts      (trigger-list · 40 · —)    │
│      Beach packing           (packing · 3 · once)       │
│      Project verbs           (verb-starter · 24 · —)    │
│                                                         │
│ Manage                                                  │
│   c  Create new          e  Edit highlighted            │
│   D  Delete highlighted  d  Duplicate highlighted       │
│ q  Quit                                                 │
╰─────────────────────────────────────────────────────────╯
     badge = (kind · item-count · recurrence)
```

**Builder transient + editable/live preview pane**

```
╭─ Build checklist: Beach packing ────╮   ┌ *Org GTD Checklist* (editable) ──┐
│ Spec  packing · 3 items · reset:done│   │ * Beach packing        :Checklist:│
│ n  Name    Beach packing            │   │   - [ ] Sunscreen                 │
│ K  Kind    packing                  │   │   - [ ] Swimsuit                  │
│ R  Reset   on-done                  │   │   - [ ] Beach towel               │
│ c  Recur   once (no repeater)       │   │   ▏ (type items as `- [ ]` lines; │
│                                     │   │      live — no re-render needed)  │
│ RET Refresh   s Save   C-c C-k Abort│   └───────────────────────────────────┘
╰─────────────────────────────────────╯   ▲ this is what `i` spawns verbatim
```

**Keymap**

| Surface | Key | Action | Tag |
|---|---|---|---|
| Manager | `RET` | Preview highlighted checklist in other window | `[U]` |
| Manager | `i` | Insert a live **instance** at a target (checklist-specific) | new |
| Manager | `c` / `e` | Create new / edit highlighted → builder | `[U]` |
| Manager | `d` / `D` | Duplicate / delete highlighted (D confirms) | `[U]` |
| Manager | `q` | Quit, restore window config | `[U]` |
| Builder | `n` `K` `R` `c` | Set Name / Kind / Reset-policy / Recurrence (infixes) | new |
| Builder | *(pane)* | Type `- [ ]` lines directly = the items (WIP buffer) | `[R]` WIP infra |
| Builder | `RET` | Refresh preview after a metadata change | `[U]` |
| Builder | `s` | Save (prompts name) → `org-gtd-checklists` | `[U]` |
| Builder | `C-c C-k` | Abort; dirty-guard prompt; restore windows | `[R]` |

**Live preview (before/after one keystroke):** with `Recur` = `once`, the pane heading is `* Beach packing  :Checklist:`. Press `c` → `+1w`, then `RET`: the heading re-renders as `* TODO Beach packing  :Checklist:` with `<2026-07-10 Fri +1w>` and a `reset↻` badge — you *see* it become recurring before you save. The manager keys (`RET c e d D q`) are identical to `NEW-VIEW-MANAGER`, so the two managers never drift; only `i` (instantiate) is added, because a checklist is *spawned into a file* where a view is merely *displayed*.

## 5. Fit with org-gtd

- **Extends:** the **type registry** (a lightweight `checklist` type in `org-gtd-types`, so a spawned instance carries `ORG_GTD: Checklist` and the reset/recur/skip behavior attaches to it) `[R]/[U]`; **WIP buffer infra** (`org-gtd-wip--get-buffer`) as the editable item pane `[R]`; **org-edna** `RESET` for heading-level re-arm `[R]` (already a declared dep); the **command-center** transient as the discovery home `[R]`; **transient.el** for both prefixes in the `org-gtd-<verb>` idiom. The per-checkbox reset-on-DONE is **net-new in-house** (~100 lines; the `org-checklist` *pattern*, no org-contrib dependency — `gap-implementation-strategies.md` §4).
- **Shared surface / cluster (E):** with **`NEW-VIEW-MANAGER`**, which is the Wave-0 exemplar this **conforms to**. Shared, must-feel-identical: (1) the **list → create → live-preview → edit → save/name → recall → delete** lifecycle; (2) the CRUD keys `c e d D RET q s`; (3) the "Your saved X" list layout with a highlighted-item model and a per-item badge; (4) `C-c C-k` abort with dirty-guard + window restore; (5) a name→spec `defcustom` store saved via `customize-save-variable`. **Confirmed from a UX standpoint** — despite different backing engines (view DSL vs checklist type), the folders-and-paper manager is one idiom. The *only* sanctioned per-feature variance: the **builder body** (checklist metadata + an *editable* item pane vs. the view-manager's DSL infixes + *read-only* agenda pane) and the extra **`i` insert** key (views aren't instantiated into files).
- **Downstream consumers (this doc is upstream):** PRJ-10 (Cluster D) reads a `kind: verb-starter` template's `items` to offer verbs during project naming — it *consumes the data model, not this manager's surface*. REF-02 / CAP-09 (Cluster A) read a `kind: trigger-list`/`prompt` template's `items` as the prompt sequence their session engine walks. So the item list must be a plain ordered list of strings usable **without** spawning an org instance.
- **Reuse vs. new:** *Reused as-is* — WIP infra, org-edna RESET, command-center, transient idiom, the view-manager's store+CRUD scaffold (see below). *Genuinely new* — the `org-gtd-checklists` store, the checklist type + its reset/recur behavior, the checkbox-reset hook, and `org-gtd-checklist-insert`.
- **Release tag of what you lean on:** WIP infra, org-edna, and command-center are `[R]`; the CRUD-manager store pattern is `[U]`. No `[R]` surface is *reworked* — the checklist type is an *addition* to the registry, so no GTD-fidelity justification is owed for churn (adding a reusable-list type is itself GTD-faithful: trigger/checklists are named GTD artifacts).

### Type / extension-UX opportunities

> **Superseded (2026-07-08) — rejected design, see §8 of the design doc.** The `checklist` type-registry addition discussed here was rejected (YAGNI); a checklist is a plain org subtree carrying no `ORG_GTD` type.

- **The CRUD-manager scaffold should be a shared primitive, not copy-pasted.** This is the *second* Cluster-E consumer of the "name→spec `defcustom` store + list-transient + builder-transient + dirty-guard" pattern that `NEW-VIEW-MANAGER` introduces. Building it twice proves it should be **factored generically** (`org-gtd-crud-manager` over a store symbol + a builder function). That same scaffold is exactly what the type registry lacks — it is the friendly front-end the primer wants for `org-gtd-customize-type` `[U]` and the removed `org-gtd-define-type` `[X]`. Recommend: extract the scaffold here so `define-type`'s eventual UI is a third client, not a fourth reimplementation.
- **Registering the `checklist` type touches the three-places-in-sync friction** the primer flags (type entry + organize-transient layout + help text hand-synced). This feature *deliberately does not add an organize disposition* (a checklist template is a reusable reference artifact, authored in the manager, not a per-item disposition — GTD-orthodox list-work, primer §4), so it sidesteps the friction — but it re-confirms the fix: **generate the organize transient from the registry.**

## 6. Edge cases & failure modes

- **Empty state:** if `org-gtd-checklists` is somehow empty, the manager shows a teaching line, not a blank pane: *"No saved checklists yet. Press `c` to build one, or try `e` on a bundled starter like Weekly Review triggers."* (Bundled starters ship pre-loaded, so this is rare.)
- **Bad input:** empty/duplicate name on save → *"A checklist named 'X' exists — overwrite? (y/n)."* A template with **zero items** → save is allowed but warns *"This checklist has no items — insert will spawn an empty heading."* Non-`- [ ]` lines the user types in the pane (a stray paragraph) are preserved as body text but excluded from `items` (a `message` notes how many lines were captured as items). Very large lists render fine (they are just org checkboxes).
- **Recurrence sanity:** a malformed repeater in `Recur` → the infix rejects it inline (*"Recur needs an org repeater like +1w, or 'once'"*), never a save-time crash. `reset: on-done` + `recur: +1w` is the normal recurring case; `reset: never` + a repeater warns (*"a never-reset recurring list will re-arm with boxes still checked — did you mean on-done?"*).
- **When it goes wrong:** spawning into a read-only or missing target buffer → fail-soft `message` (*"Can't insert here; pick a target with `i`."*), no partial write. A hand-corrupted stored spec (unknown key) → the offending key is skipped with a warning, the rest of the checklist still loads and renders. Window config is always restored on quit/abort. All in the header-line-hint teaching voice, never a stack trace.

## 7. Open questions & maintainer decisions

- **Store granularity:** one flat `org-gtd-checklists`, or namespace by `kind` so trigger-lists vs verb-lists vs packing lists list separately? (Leaning: one flat store, `kind` is a badge/filter — matches the single `org-gtd-saved-views` store.)
- **Bundled starters:** ship weekly-review-trigger + mind-sweep + a verb-starter list pre-loaded (editable, deletable), or offer them behind a "load starter checklists" command? (Leaning: pre-load, so REF-02/PRJ-10 have content to consume on day one; deletable so they're not sticky.)
- **Instance ↔ template link:** should a spawned instance remember which template it came from (a `CHECKLIST_SOURCE` property) so edits could propagate, or is spawn a pure one-way copy? (Leaning: one-way copy for v5 — an instance is a normal org subtree; propagation is a v6 want.)
- **`RET` semantics:** preview (assumed here, to stay identical to view-manager) vs. make `RET` the insert action since "use" is a checklist's primary verb. Needs a taste call against cluster consistency. (Leaning: keep `RET`=preview, `i`=insert.)
- **Recur mechanism ownership:** heading-level re-arm via org-edna `RESET` (assumed) vs. a plain org repeater on the timestamp. Confirm org-edna is the single re-arm path so tickler/habit/checklist recurrence stays consistent (`gap-implementation-strategies.md` §3 #3).

## 8. Provenance & links

- `REC-CHK-01` · deliverable-#3 status **Not-implemented** (zero checklist infra, grep-verified) · adjudication **V-10** · `gap-implementation-strategies.md` §3 #2 (checklist as first-class registry type — build route, hint only) + §4 (in-house per-checkbox reset; org-edna re-arm). Cluster **E** exemplar it conforms to: `NEW-VIEW-MANAGER` (sets the CRUD keys, the "Your saved X" list, the dirty-guard abort). **Downstream (this is upstream of them):** `REC-PRJ-10` (verb-starter list, Cluster D), `REC-REF-02` / `REC-CAP-09` (trigger lists, Cluster A), `REC-CHK-02` (recurring reflection prompts), `REC-CAP-06` (mind-sweep), `REC-PRJ-07` (recurring project checklist).
