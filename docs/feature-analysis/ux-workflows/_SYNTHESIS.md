# Synthesis — org-gtd v5 UX-Workflow Corpus (19 docs)

> One pass over all 19 UX-workflow docs. Verdict up front: the cluster contracts largely **held** — Cluster C (delegate) and Cluster E (CRUD managers) are near-airtight, and the guided-session engine is correctly shared across A, B, and (unexpectedly) G. The drift that remains is concrete and cheap to fix: a duplicated command-center key, three different buffer names for one engine, an unsettled `s`-key meaning, and inconsistent release-tags for `command-center`/view-DSL. The bigger payoff is that five type-system refactors, each flagged independently, are really **three** shared primitives that unblock a third of the feature set.

---

## 1. Idiom reconciliation — where siblings drifted, and how to align

### 1a. Cluster E — NEW-VIEW-MANAGER ↔ REC-CHK-01 (named-object CRUD manager)

**Conformance is strong.** Both ship a list transient (`org-gtd-view-manager` / `org-gtd-checklist-manager`) with byte-identical manager keys `RET c e d D q`, a "Your saved X" list with a highlighted-item model + per-item badge, a builder with `s`/`C-c C-k`, and a `name→spec` `defcustom` (`org-gtd-saved-views` / `org-gtd-checklists`) saved via `customize-save-variable`. CHK-01 correctly limits its variance to the two sanctioned points: an editable WIP pane instead of a read-only agenda pane, and the extra `i` (insert instance) key.

Residual drift, all minor, to align:

1. **Builder `RET` verb label.** NEW-VIEW-MANAGER: `RET` = **Preview** (renders spec via `org-gtd-view-show` in other window). CHK-01: `RET` = **Refresh** (re-render heading after a metadata infix; item text is already live). Same key, same intent ("re-render the preview pane"), two labels. Align the wording to one concept — "Preview / refresh the pane" — so the muscle-memory reads identically.
2. **Recall-command naming asymmetry.** `org-gtd-view-run` (view) vs `org-gtd-checklist-insert` (checklist). Justified (a view is *rendered*, a checklist is *spawned into a file*), so keep both — but document the rule so future E members (HOR-07 already reuses `org-gtd-view-run`) pick `-run` vs `-insert` by that test, not by taste.
3. **Manager `RET` primary verb.** View: `RET` renders/uses. Checklist: `RET` previews, `i` uses. CHK-01 §7 leaves this "open"; the synthesis should **close it**: keep `RET` = preview/render across both, `i` = instantiate only where instantiation exists. This is the sanctioned variance, not a drift — ratify it so it doesn't wobble.
4. **Command-center home keys:** `v` "Views…" (Engage) vs `k` "Checklists…" (hub). No collision (organize's `k`=knowledge is a different surface, as CHK-01 notes). Fine.

**Whichever ships first sets the idiom** (per Cluster E's rule): NEW-VIEW-MANAGER is Wave-0, so CHK-01 conforms to it, and both should be built on **one extracted `org-gtd-crud-manager` scaffold** (see §4B) rather than two hand-written transients — CHK-01 §5 already argues this.

### 1b. Clusters A & B — the guided-session-engine contract (REF-02 sets it; REF-06/CAP-09/X-15/CLA-10/UI-04 inherit)

The **generic session vocabulary held everywhere**: `n`/`SPC` advance, `b` back, `p` pause, `q` quit (→ Pause/Abandon), `,` customize; three-widget console (phase/step tracker · live stats block · header-line); window-config snapshot/restore; pause-to-state-file + Resume; shared completion log; X-15's completeness line as the one canonical stat. That is the contract, and it is respected in substance. Concrete divergences to reconcile:

1. **Buffer name is inconsistent — the clearest drift.** REF-02, REF-06, CLA-10, X-15 → `*GTD Review*`. CAP-09 → `*GTD Sweep*`. UI-04 → `*GTD Migrate Overdue Calendar*`. The contract says the console must feel identical; three names break "one engine." **Fix:** one engine buffer (e.g. `*GTD Session*`, or keep `*GTD Review*`) with the profile name in the header line — REF-02 already puts the profile name there (`— Weekly Review`, `— System Maintenance`), so the distinct buffer names are redundant *and* contradictory. Pick one.

2. **`s` means two different things.** In Cluster A `s` = **skip this step (this run)** (REF-02, CAP-09). In Cluster B `s` = **skip this item** (CLA-10, UI-04 — one item *is* the step). REF-06 tries to have both, splitting `s` = skip-step vs `.` = skip-item, which no one else does. **Fix:** define one rule — in a one-item-per-step walk (all of B, and A's walk steps) `s` skips the current item; a multi-step phase's "skip the whole step" is a *phase-level* action, not `s`. Drop REF-06's lone `.`; it is the only doc that invented it.

3. **Accept-default vs advance on `n`/`SPC`.** In A, `n` = advance *after* you've acted (REF-02's walk step blocks `n` via the invariant guard until you act). In UI-04, `n`/`SPC`/`RET` = **accept the default action (migrate) and advance** in one stroke; UI-04 flags this as a deliberate break, keeping `m` as the semantic key. This is reconcilable and worth stating as the rule: **in a triage walk, advancing == accepting the default**; the per-profile "accept" verb (`r` re-concretize, `m` migrate, `d` defer, `a` archive, `c` capture) is legitimately different because the action bar is **generated from each step's `:allowed-actions`**. What must stay identical — and does — is the veto/skip pair: `k` = keep/veto (REF-06, CLA-10, UI-04), `s` = skip (per fix #2). Ratify: keys differ by allowed-actions; the *renderer, chrome, veto/skip semantics, and default-fires-on-`n`* are fixed.

4. **Session command naming drifts from the `org-gtd-<step>-<action>` convention.** `org-gtd-review` (REF-02, no step prefix), `org-gtd-reflect-maintenance` (REF-06), `org-gtd-capture-sweep` (CAP-09, flagged unresolved in its §7), `org-gtd-reflect-vague-items` (CLA-10), `org-gtd-migrate-overdue-calendar` (UI-04, no step prefix), `org-gtd-completeness` (X-15). Since these are all one engine with different profiles, prefer **`org-gtd-review`/`org-gtd-session` as the engine entry with a profile arg**, and keep the specific commands as thin `(org-gtd-review 'maintenance)`-style aliases named consistently. At minimum, give UI-04's command a step prefix (`org-gtd-reflect-migrate-overdue-calendar`) so it doesn't read as a top-level verb.

---

## 2. Unify / streamline — one surface for overlapping UX

### 2a. One guided-session engine spans **three** clusters, not one
A (REF-02/06, CAP-09, X-15), B (CLA-10, UI-04), **and** G's AGE-03 meeting-close session all describe the same one-at-a-time walk with the same chrome — AGE-03 explicitly says its close session uses "the same session-engine chrome as `someday-review`." So the engine REF-02 designs is inherited by a Cluster-G member too. **Unify:** build a single `org-gtd-session` engine driven by a profile = an ordered list of typed steps, each carrying `:allowed-actions`, with the action bar and header-line **generated** from those actions (REF-02's own type-UX ask). Weekly review, maintenance, sweep, vague-triage, overdue-calendar-triage, someday-review, and meeting-close are all profiles. This is the single highest-leverage unification in the corpus.

### 2b. The `w` collision reveals a missing unification
REF-02 and CAP-09 **both** claim command-center Reflect key `w`. That is not a bug to renumber — it is the signal that they are one entry. **Unify:** a single `w` "Guided session…" command-center row opens the engine's **profile picker**, which lists Weekly Review, Maintenance, "What's true now" sweep, etc. Both docs already specify a profile picker "when >1 profile exists"; make that the front door and the conflict dissolves.

### 2c. `completing-read` over a named list is reinvented six times
It appears in NEW-VIEW-MANAGER (`org-gtd-view-run`), CHK-01 (`org-gtd-checklist-insert`), NXT-05 (someday-list prompt), PRJ-10 (verb starters), DEL-05 (channel hierarchy), X-04 (energy levels), and UI-07 (verb routes). PRJ-10 and NXT-05 both name the fix: **a descriptor `:input-fn` that references a named checklist by name.** Unify all "pick from a curated list" prompts onto *one* checklist-backed input mechanism (see §4C): project verbs, delegate channels, energy levels, someday sub-lists, and verb routes all become named lists in the CHK-01 store, consumed identically. This collapses Clusters C, D, F, and the UI-07 singleton onto one input idiom.

### 2d. Delegate is already one flow — keep it one command
Cluster C (DEL-03/04/05) correctly funnels through the single `d` disposition with the shared **who → when → channel** sequence and `DELEGATED_TO` + `WAIT` vocabulary. Streamline the implementation to match: one `org-gtd-delegate--organize` with a stage branch (DEL-04), a `:organize-project-fn` branch (DEL-03 collapse+snapshot), and the channel sub-prompt (DEL-05) — not three code paths. DEL-04 explicitly requires its stage-2 output to be **byte-identical** to DEL-03's collapsed Waiting-For; enforce that with a shared property-writer.

### 2e. Project shaping is already one transient — enforce it
Cluster D (PRJ-06 + PRJ-10) agree they are "one menu with two sections" on `C-c C-p` / the `p`-at-creation moment: PRJ-06 supplies the Natural-Planning phase toggles, PRJ-10 supplies the `t` verb-starter naming row. Ship them as a single `org-gtd-project-shape` transient (PRJ-06's mock already includes PRJ-10's `t` row), compose order name→plan, not two commands.

### 2f. Harvest-to-inbox is already one helper — keep it one
Cluster G (CAP-08 + AGE-03) both call the identical `org-gtd-capture--harvest-to-inbox` loop ("Anything to capture?" → `inbox.org`), differing only in trigger (clock-out hook vs meeting-occurred transition). Add CAP-08's proposed `org-gtd-harvest-triggers` registry so both *register* a boundary instead of hand-wiring hooks (see §4).

### 2g. Detection-pass → flagged-list → walk is a reusable scaffold
CLA-10 (vague lint), UI-04 (overdue calendar), REF-06's integrity phase, AGE-03's meetings-to-close, and X-15's count all run a detection pass over `org-map-entries`/skip-predicates producing a flagged set that the session engine then walks. Factor a `detect → flag → walk` helper so each feature supplies only its predicate and its accept verb.

---

## 3. Redundancy / conflict

- **Keybinding conflict — `w` (command-center Reflect):** REF-02 "Guided review" vs CAP-09 "run a sweep." Resolve per §2b (one `w` → profile picker).
- **Buffer-name divergence:** `*GTD Review*` / `*GTD Sweep*` / `*GTD Migrate Overdue Calendar*` for one engine (§1b#1).
- **`s`-key semantics:** skip-step (A) vs skip-item (B) vs REF-06's `s`+`.` split (§1b#2).
- **`m` overload:** organize disposition `m`=meeting (AGE-03); command-center `m`=Meetings-to-close (AGE-03, Reflect), `m`=Maintenance (REF-06, Review System submenu), `m`=Migrate (UI-04, inside the `M` Missed submenu). The three menu entries live in *different* submenus so they don't hard-collide, but three `m`s in the command-center neighborhood is confusing — assign distinct letters (e.g. maintenance `M`-submenu keeps `m`, meetings take a different Reflect letter since AGE-03's `m` sits in the top Reflect group).
- **`v` in two surfaces:** NEW-VIEW-MANAGER `v` "Views…" (command-center Engage) and UI-07 `v` "Enter by verb…" (organize transient). Different surfaces, no hard collision, but both `v` — acceptable; note it so neither migrates onto the other's surface later.
- **Command-naming inconsistency** across the session family (§1b#4) and the `-run`/`-insert` asymmetry (§1a#2).
- **Two hand-maintained dispatch maps:** the organize transient (type entry + layout + help, the primer's 3-places friction) and UI-07's `org-gtd-verb-routes`. UI-07 itself argues these are redundant and should generate from one `:verbs` registry slot (§4A).
- **Already-duplicated code the docs caught:** NXT-05 flags the someday list-prompt completer hand-copied in `org-gtd-someday.el:80-82` **and** `org-gtd-projects.el:1259-1261` — extract `org-gtd-someday--read-list` once (pre-existing drift, worth fixing while in the area).
- **Stat double-counting risk:** CAP-09 asks whether its "Captured this run" counter feeds X-15's ≥50 readout — keep them distinct rows (captured-this-run is transient; X-15 counts filed next-actions). Ratify: distinct.

---

## 4. Type / extension-UX opportunities, consolidated

Nearly every doc's §5 "Type / extension-UX opportunities" points at one of **six** primitives. Ranked by how many features benefit:

**A. Generate the organize transient (+ help text + verb router + session action bars) from the type registry.** The single most-cited item. Named by CHK-01, X-15, AGE-03 (its new `m` row), UI-07 (a `:verbs`/`:aliases` slot so the transient, help, and verb router share one source), and REF-02 (the same "generate the action bar from `:allowed-actions`, don't hand-sync" move for the session engine). Building the registry-generated action bar in the session engine first (REF-02) proves the pattern the organize transient then adopts. **Unblocks:** AGE-03's `m`, UI-07's `v`, and kills the primer's three-places-in-sync friction.

**B. A generic CRUD-manager scaffold (`org-gtd-crud-manager` over a store symbol + builder fn).** Named explicitly by NEW-VIEW-MANAGER and CHK-01 (as its "second consumer, prove it, factor it"), and reused by HOR-07 (a seeded object in the view store), REF-02's profiles (a named-object collection "E's manager could later wrap"), and UI-07's `org-gtd-verb-routes-edit`. Same scaffold is the friendly front-end the primer wants for `customize-type` **[U]** and the removed `define-type` **[X]**.

**C. Checklist-backed / named-list `:input-fn`, and a `:type choice` enum descriptor.** PRJ-10's flagship: "descriptors should reference a named checklist by name" as the completion source. Consumed by DEL-05 (channels), X-04 (energy), NXT-05 (someday lists), UI-07 (verbs), CAP-09 (checklist item → prompt step). DEL-04 adds the enum case (`:type choice :options (...)` for `ORG_GTD_DELEGATE_STAGE`). This turns "a checklist," "a field's allowed values," and "a guided prompt" into **one** mechanism — the structural form of Cluster F's "attaching optional metadata feels identical regardless of attribute."

**D. Optional / off-by-default / guidance descriptor slots.** DEL-05 (`org-gtd-configure-as-type` prompts *only* `:required` today — its whole channel-capture is blocked without a `:optional-prompt` slot; it degrades to guidance-only otherwise), X-04 (`:when-enabled` gating a descriptor on a defcustom; `:required nil` + skippable), NXT-05 (same), DEL-05 (`:guidance` slot so methodology tips travel with the type). One reusable "offered-but-optional, self-documenting field" primitive the whole Cluster F + DEL-05 needs.

**E. A uniform computed/property DSL-filter registration point.** Many features add a filter key: X-04 (implement the *reserved-but-unwired* generic `property` key, make `energy` sugar over it — turns X-04 into ~10 lines), NXT-05 (`someday-list`), CLA-10 (`vague`), REF-06 (`due-for-review` + a `:validate-fn`-driven integrity list), AGE-03 (`meeting` type + `occurred` predicate), X-15 (an `:actionable`/`:counts-as-next-action` type role flag that cleans up the `next-action` filter, the engage view, *and* the completeness count), HOR-07 (`horizon-text` block-type). Rather than each hand-adding to `org-gtd-view-lang--known-filter-keys`, expose **one registration API** (filter key + predicate + type-default), and implement the generic `property` filter as the backbone.

**F. Type lifecycle / snapshot / transition / validate / close slots.** DEL-03 (a shared `:snapshot t` marker + `:reactivate-fn` so someday/tickler/delegated share one freeze-restore capability instead of three), AGE-03 (`:lifecycle` ordered states + computed predicate, `:on-close` slot), REF-06 (`:validate-fn` for integrity), UI-04 (`org-gtd-retype` type-transition helper + `:overdue-default` policy), DEL-04 (`:states` intra-type progression). These converge on "types can declare state machines, transitions, and close hooks" — the natural payload for the resurrected `define-type`, and the biggest scoping fork (AGE-03 §7).

**Highest-leverage, build-these-first: A, B, C.** They are each named by 4–6 features and each unblock whole clusters. D and E are small and unblock Cluster F + DEL-05 cheaply. F is the deepest and should be *prototyped* on one feature (DEL-03's snapshot unification or AGE-03's meeting lifecycle) before being generalized into `define-type`.

---

## 5. Gaps / completeness

- **Release tags — RESOLVED (maintainer ruling, 2026-07-04):** `command-center` and the **view DSL** (`org-gtd-view-show`, filters, prefix-format) are **[R]** — released in 4.6.1 (git-verified). Docs that tagged them `[U]` are corrected. Consequence: a feature that merely *adds a layer on top* of these (a new command-center row, a manager/store around `org-gtd-view-show`) is **not** an `[R]` rework and owes no justification; a doc that claimed to *rework the DSL itself* (NEW-VIEW-MANAGER's earlier wording) is restated as 'adds a management layer, DSL unchanged'. Genuinely `[U]` remain `[U]`: the interactive view/checklist **managers**, the generalized **session engine**, new **types** (meeting), and the hook/slot/`customize-type`/`define-type` extension model.
- **DEL-05 is blocked on a type-system change and self-admits degradation.** Its channel *capture* needs the §4D optional-prompt slot (`org-gtd-configure-as-type` prompts only `:required`, per `org-gtd-configure.el:107`). Without it, DEL-05 falls back to guidance-only text on the who-prompt help line. The Cluster-C "who/when/channel" sequence assumes the channel prompt works — so C is not fully deliverable until D lands. Sequence D before C's channel piece.
- **X-15 cannot ship independently.** By design it is a stat line inside REF-02's stats widget — it has no standalone lifecycle. Only its `M-x org-gtd-completeness` one-shot is shippable before the engine exists. Not a defect (the demotion is per contract), but it means X-15 is gated on REF-02.
- **HOR-07 is doubly gated** — on NEW-VIEW-MANAGER (Cluster E must exist; it is a seeded object, not a manager) and on a net-new `horizon-text` block-type. It is the lowest-priority MAY, correctly scoped, but can't precede E.
- **REF-06 slightly breaks the "byte-for-byte identical" console contract** with its lone `.` skip-item key (§1b#2). Align it.
- **UI-07 hand-waves the context model.** It stamps `@calls`/`@errands` as plain org tags but admits "org-gtd has no first-class context object" and leaves whether verbs should tag at all as an open question. Its `:verbs` registry ambition (§4A) is sound, but the context-tagging half is underspecified — confirm plain-tag semantics or scope contexts out.
- **AGE-03 proposes a genuinely new top-level `meeting` type**, needing `define-type` resurrection (the corpus's biggest scoping fork, its §7). The primer says prefer a someday sub-list or customized type before minting a new type; AGE-03 justifies the new type on the lifecycle (which no existing type has). Legitimate, but it is the one feature that forces the §4F/define-type decision.
- Everything else is lifecycle-complete for what it is. The Cluster-F/G/PRJ/DEL "no preview at attach" notes are correct (a property write has no preview), not gaps.

---

## 6. Recommended build order (dependency-aware)

**Wave 0 — foundations everything inherits (build first, in parallel where independent):**
1. **Release tags — settled:** `command-center` and the view DSL are **[R]** (§5, maintainer ruling). Still worth a quick audit of any *other* surface's tag before build, but the two big ones are resolved.
2. **The guided-session engine** (REF-02) — one `org-gtd-session` with typed steps, generated action bars, pause/resume, three-widget console, one buffer name, the reconciled `n/b/s/p/q/,` + veto(`k`)/skip(`s`) vocabulary (§1b). Inherited by A, B, and AGE-03.
3. **The generic CRUD-manager scaffold** via NEW-VIEW-MANAGER (§4B) — sets the E idiom; also needs the generic `property`/computed-filter DSL registration point (§4E) since the builder generates infixes from `known-filter-keys`.
4. **The checklist data-model** (CHK-01) — conforms its manager to #3; provides the `items` list and the checklist-backed `:input-fn` (§4C) that feeds A's trigger lists, D's verbs, and F's/C's named lists.
5. **Descriptor refinements** (§4C/D): optional/skippable prompt, `:guidance`, `:type choice`, named-list `:input-fn`. Small; unblocks DEL-05, X-04, NXT-05, PRJ-10, DEL-04.

**Wave 1 — cluster inheritors (parallel), each handed its Wave-0 contract:**
- **A:** REF-06, CAP-09, X-15 (all profiles of the #2 engine; unify REF-02+CAP-09 onto the single `w` profile picker, §2b).
- **B:** CLA-10, UI-04 (triage profiles of the same engine + the detect→flag→walk helper, §2g).
- **C:** DEL-03, DEL-04, DEL-05 — as **one** `d` flow (§2d); DEL-05's capture rides Wave-0 #5.
- **D:** PRJ-06 + PRJ-10 as one `org-gtd-project-shape` transient (§2e), consuming CHK-01's verb list.
- **F:** X-04, NXT-05 — thin once #5 lands (X-04 ≈10 lines on the generic `property` filter).
- **G:** CAP-08 + AGE-03 sharing `org-gtd-capture--harvest-to-inbox` + the `org-gtd-harvest-triggers` registry (§2f).

**Wave 2 — after the registry-generated transient and E exist:**
- **UI-07** (better once the organize transient generates from a `:verbs` registry slot, §4A — otherwise it adds a *fourth* hand-synced dispatch map).
- **AGE-03's `meeting` type + `define-type`/`:lifecycle`/`:on-close`** (§4F) — the deepest type work; prototype meeting-only, then generalize.
- **HOR-07** (seeded object in the E store + the new `horizon-text` block-type) — lowest-priority MAY, last.

**Rationale for the ordering:** the three named Wave-0 primitives (session engine, CRUD scaffold, checklist model) plus the two small descriptor/DSL refinements unblock 12 of the 18 real features; the type-registry-generated transient (§4A) is the pivot that turns UI-07 and AGE-03 from "add another hand-synced map" into "register a slot," so it should precede them; the deep lifecycle/define-type work (§4F) is deliberately last and prototyped on one feature before generalizing.
