# UX-Workflow Clusters & Dispatch Plan

> Derived 2026-07-04 from a UX-surface clustering pass over the Not-implemented feature set. Groups features by the **interaction surface a user meets them on** — so features that should share a UX are designed against one **shared contract** instead of each inventing its own. Cluster by UX intent, *not* by build-route or release tag.
>
> **How to use.** If you're authoring a per-feature doc (`_TEMPLATE.md`), find your feature below, read your cluster's **shared design contract**, and make your feature conform to it (or, in your §5, argue explicitly why it should break from it). The `gap-implementation-strategies.md` §3/§10 clusters are *build-route* groupings — they may differ; see the divergence notes.

## Feature set

The 22 Not-implemented features, resolved: `REC-CAP-08, REC-CAP-09, REC-CLA-10, REC-PRJ-06, REC-PRJ-10, REC-NXT-05, REC-DEL-03, REC-DEL-04, REC-DEL-05, REC-KNO-03, REC-KNO-04, REC-KNO-05, REC-KNO-06, REC-CHK-01, REC-AGE-03, REC-REF-02, REC-REF-06, REC-HOR-07, REC-UI-04, REC-UI-07, REC-X-04, REC-X-15`.

- **Drop** `REC-KNO-03` (V-09 non-goal — no CRM).
- **Defer** `REC-KNO-04/05/06` → the reference-adapters brainstorm (not designed in this pass).
- **Add** the net-new exemplar `NEW-VIEW-MANAGER`.
- **Active set = 18 features + 1 exemplar = 19 docs.** (`REC-AGE-01/02` are *Partial*, not Not-implemented — they're phase-2, out of scope here; only `REC-AGE-03` is in scope.)

---

## Clusters

### A — Guided review & sweep sessions
**Surface:** the reflect guided one-at-a-time session engine (`org-gtd-reflect-someday-review`) + command-center home.
**Members:** `REC-REF-02`, `REC-REF-06`, `REC-CAP-09`, `REC-X-15` *(demoted — see note)*.
**Shared contract:** Entered from `org-gtd-command-center`; each is a full-frame session walking a *configured sequence* (REF-02's Get Clear/Current/Creative phases; REF-06's maintenance step-list; CAP-09's trigger-list prompt-walk), showing progress + running stats, offering per-step actions (defer/clarify/capture/quit), restoring the saved window config on finish/abort. They must feel like **one session engine with pluggable content**. The stats affordance must be identical across all.
> **X-15 is demoted:** it is a *lightweight completeness stat* (the ≥50-actions heuristic) shown **inside** a review, not a full session of its own. Design it as a small stat block the session engine can surface — in Cluster A's orbit, but scoped down, not a standalone walkthrough.

### B — Flag → decide → consent
**Surface:** a reflect triage loop over a *flagged list*, teaching-error voice.
**Members:** `REC-CLA-10`, `REC-UI-04`.
**Shared contract:** A detection pass flags items (vague/degraded phrasing for CLA-10; overdue calendar items for UI-04); org-gtd then walks them one at a time, presenting the decision with a clear default and an explicit veto/skip — **never auto-acting without consent**, always in the teaching voice. Runnable standalone *and* embeddable as a phase inside a Cluster-A review; the per-item accept/veto/skip must feel identical to Cluster A and to `someday-review`.

### C — Delegate-flow enrichments
**Surface:** the organize transient delegate disposition (`d`) + clarify WIP + the `DELEGATED_TO`/WAIT model.
**Members:** `REC-DEL-03`, `REC-DEL-04`, `REC-DEL-05`.
**Shared contract:** One delegate flow reached through `d`, sharing the same prompt sequence (who / when / channel), the `DELEGATED_TO`+WAIT vocabulary, and the follow-up view. DEL-04 inserts a *plan-to-delegate* pre-state before WAIT; DEL-03 adds a "delegate this whole project" variant collapsing a breakdown to one Waiting-For (lossless snapshot); DEL-05 folds trackable-channel guidance into the same prompt. Delegating an item, a project, or planning-to-delegate must read as **one coherent flow**, same keys and property names.

### D — Scaffolded project creation
**Surface:** organize-as-project (`p`) inside the clarify WIP buffer, at creation time.
**Members:** `REC-PRJ-06`, `REC-PRJ-10`.
**Shared contract:** Both intervene as a new project is shaped in the WIP buffer: PRJ-06 offers the Natural Planning scaffold (purpose→vision→brainstorm→organize→next-actions — helper-text now, structured schema later per V-04); PRJ-10 offers a verb-starter checklist for *naming* it. Same entry, same WIP surface, same "optional scaffold you can ignore" feel — one guided project-shaping affordance, not two bolt-ons. *(PRJ-10 consumes the Cluster-E checklist data model.)*

### E — Named-object CRUD managers (interactive, live-preview)
**Surface:** a *net-new* transient-driven manager with live preview + a persisted `defcustom` store — the primer's flagship "manage my X interactively" pattern.
**Members:** `NEW-VIEW-MANAGER`, `REC-CHK-01`.
**Shared contract:** Both manage a *named, persisted collection* — views for the view-manager, checklists/trigger-lists for CHK-01. Identical lifecycle idiom: **list existing → create → live-preview → edit → save/name → recall → delete**, driven by a transient over the object's fields, backed by a name→spec store. The CRUD keys, the preview-pane behavior, and the "your saved X" list must feel identical across both, even though the backing engines differ (view DSL vs checklist type). **Whichever ships first sets the idiom.** This is the digital folders-and-paper manager.

### F — Optional classification metadata
**Surface:** type property-descriptor / named someday-list, captured at organize, consumed via the view DSL / review — off by default, minimal new UI.
**Members:** `REC-X-04`, `REC-NXT-05`.
**Shared contract:** Both add an *optional* classification attached during organize and consumed later through existing surfaces: X-04 an `ENERGY` property (attach → filter in engage/DSL); NXT-05 a Read/Review someday sub-list (attach → consume in a named-list review). Off by default, no bespoke buffer; both reuse the existing property-prompt / someday-list-prompt idiom and the DSL filter families, so attaching optional metadata feels identical regardless of attribute.

### G — Activity-boundary harvest
**Surface:** the capture pipeline fed at the *close of a bounded activity*, via an org event or a lifecycle transition.
**Members:** `REC-CAP-08`, `REC-AGE-03`.
**Shared contract:** At the close of a bounded activity — a clocked call/session (CAP-08, on an `org-clock-out`-style boundary) or a meeting reaching "occurred" (AGE-03) — org-gtd asks "did anything come out of that?" and routes the outputs into `inbox.org` for normal processing. The harvest *moment*, its prompt idiom, and the landing (inbox) must be identical; entry differs (org hook vs meeting-object transition) but the closure-loop feel is shared.

### Singletons
- **`REC-UI-07`** (verb-first action entry) — a net-new *alternative front-door* routing an action by its leading verb to an existing organize disposition. Parallel dispatch UI to the organize transient's mnemonic keys; shares no lifecycle with a cluster.
- **`REC-HOR-07`** (cross-horizon "overview of my life" view) — a single bundled composite view (Ground→50k with projects folded in). It *consumes* the view DSL rather than managing it. Lowest-priority MAY (dropped-as-mandate). Natural *exemplar payload* that `NEW-VIEW-MANAGER` could produce.

### Deferred to adapter brainstorms
- **`REC-KNO-04/05/06`** — reference filing / browsable index / reference forms → the reference-adapters brainstorm (denote/org-roam/org-brain). Not designed here. *(If ever built in-house, KNO-06's forms manager would echo Cluster E's idiom.)*

---

## UX-vs-build-route divergences

Where UX clustering differs from `gap-implementation-strategies.md` §3/§10 (the highest-value findings; the synthesis pass should re-check these):

1. **E pairs `REC-CHK-01` with `NEW-VIEW-MANAGER`** — different engines entirely (checklist type vs view DSL), but the *same* net-new CRUD-manager UX. Design the manager idiom once; both inherit. **Biggest, highest-leverage divergence.**
2. **`REC-PRJ-10`: shared engine ≠ shared surface.** Build-route lumps it with checklist machinery; UX-wise its home is **project creation** (Cluster D). The checklist type is its *mechanism*, the WIP buffer is its *UX home*.
3. **`REC-CAP-09`: session-surface, not content-artifact.** Build-route treats it as authored trigger-list content; UX-wise its experience is the guided prompt-walk — the **same session engine as REF-02** (Cluster A).
4. **A vs B split what §10 lumped.** Ritual multi-phase review (A) and single-list consent triage (B) are distinct contracts despite sharing the engine. Build-route also scattered B's members (CLA-10 under Clarify §2, UI-04 under hooks §9); UX reunites them.
5. **G unifies two build lenses.** CAP-08 is a §9 hook, AGE-03 a §8 new object — maximally different engines, but the *same harvest moment*.
6. **C reunites what build-route split three ways** (DEL-03 §3#9, DEL-04 §7, DEL-05 §5) — one delegate flow, UX-wise.
7. **F agrees with build-route** (§7 schema additions) — honesty check: UX and build-route align on the "optional metadata via existing prompts + DSL filter" contract.

---

## Dispatch waves

**Wave 0 — foundational surfaces (design first; others inherit their contracts):**
1. `REC-REF-02` → the guided-**session-engine** contract.
2. `NEW-VIEW-MANAGER` → the named-object-**CRUD-manager** contract.
3. `REC-CHK-01` → the **checklist data-model** contract (upstream dep for A's trigger lists + D's verb list; its manager UX conforms to NEW-VIEW-MANAGER).

**Wave 1 — inheritors (parallel), each handed its cluster's Wave-0 contract:**
- A rem.: `REC-REF-06`, `REC-CAP-09`, `REC-X-15` (session)
- B: `REC-CLA-10`, `REC-UI-04` (session)
- C: `REC-DEL-03`, `REC-DEL-04`, `REC-DEL-05`
- D: `REC-PRJ-06`, `REC-PRJ-10` (checklist)
- F: `REC-X-04`, `REC-NXT-05`
- G: `REC-CAP-08`, `REC-AGE-03`
- singletons: `REC-UI-07`, `REC-HOR-07` (crud)

**Synthesis** — one pass over all 19 docs: reconcile idiom drift (CRUD idiom NEW-VIEW-MANAGER↔CHK-01; session contract A↔B), flag redundant UX, propose unified transients/commands, verify each shared contract held.
