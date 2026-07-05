# UX Workflow — Overview of My Life

`REC-HOR-07` · one scrolling view that stacks all six Horizons of Focus (Ground → 50k) with live projects folded in, so the user can see their whole life aligned in a single glance · cluster: `crud` (Cluster E, inherited)

---

## 1. The need (what & why)

GTD's Perspective half (Making It All Work, WF-26) says control decays without altitude: you must periodically see how today's runway lines up with your Areas, Goals, Vision, and Purpose. Today org-gtd scatters that across surfaces — `org-gtd-engage` shows Ground, `org-gtd-reflect-stuck-projects` shows 10k, and `horizons.org` (IMPL-103) holds the 20k–50k *prose* as a separate foldable file nobody composes in. There is no single view where runway and horizon sit together. This feature is the "look up from your desk and see the whole building" moment — a MAY, lowest-priority, but a natural payload once the Cluster-E view manager exists.

- **When hit:** during a Reflect session (weekly/monthly), or any time the user feels adrift and wants to re-anchor "what am I doing this for?"
- **Source:** REC-HOR-07 (deliverable #3, Not-implemented; DROP-as-mandate, reinstated MAY). Book: MIAW six-horizon conversations.

## 2. Entry points & discovery

- **Invoke:**
  - `M-x org-gtd-overview` — the companion `-run` that renders the seeded view immediately (the primer's `org-gtd-<thing>-run` idiom).
  - **Command center** [R]: a new `o` "Overview of my life" row in the *Reflect* group of `org-gtd-command-center`.
  - Via the **view manager** [U]: `M-x org-gtd-view-manager` → the "Overview of my life" entry is pre-seeded in the "Your saved views" list; `RET` recalls it.
- **Discover:** it ships **pre-populated** as a saved view in the manager's store, so the very first time a user opens `org-gtd-view-manager` they *see* it sitting there as a worked example — the exemplar payload NEW-VIEW-MANAGER produces. Plus the visible `o` row in the command center.

## 3. Full-lifecycle walkthrough

REC-HOR-07 owns **no bespoke lifecycle** — it is a single named spec living in the Cluster-E store, so every verb below is the view-manager's verb acting on one seeded object. What is genuinely new is only the *composite spec* and the `horizon-text` block it needs.

- **Create / start:** the user does not build it — it is delivered as a factory entry in `org-gtd-saved-views` named `"Overview of my life"`. (A user who deletes it recalls it with `M-x org-gtd-view-restore-defaults`.) A power user *can* fork it: manager → `d` duplicate → `e` edit.
- **See / preview:** `M-x org-gtd-overview` renders a single scrolling org-agenda buffer. Reading top→bottom walks **Ground → 50k**: each altitude is a **prose banner** (pulled from the matching `horizons.org` heading) immediately followed by its **live GTD block(s)**. Projects are folded in as the 10k block. Standard org outline folding (`TAB`/`S-TAB`) collapses any band.
- **Edit / reconfigure:** manager → `e` opens the shared **builder** transient. Because this is a stack-of-blocks spec, the builder exposes: which horizons to include (checkboxes), direction (bottom-up ↔ top-down, honoring WF-26's override), and an optional area-of-focus scope. `RET` live-previews into the other window after any change.
- **Save / name / recall:** `s` in the builder writes back to `org-gtd-saved-views` via `customize-save-variable`; recall is `org-gtd-view-run` completing-read-by-name, or manager `RET`. No init.el editing — that gap is exactly what Cluster E closes.
- **Delete / back out:** manager `D` deletes (confirms); builder `C-c C-k` aborts with the shared dirty-guard. Quitting the rendered buffer (`q`) restores the pre-view window configuration.
- **Repeat / recur:** not applicable — it is a read-only view, re-rendered on demand (`g` in the buffer refreshes); nothing recurs.

## 4. Interaction sketch

**Rendered composite (what `org-gtd-overview` shows):**

```
*Org Agenda: Overview of my life*───────────────────────────────────────────
  ╭─ GROUND · What do I need to DO? ───────────────────────────────╮
    Today's Schedule
      10:00  Dentist
    All actions ready
      Home    — Call plumber
      Career  — Draft Q3 memo
  ╭─ 10,000 ft · PROJECTS — What do I need to COMPLETE? ────────────╮
    Active projects
      Home    — Renovate kitchen          [2/5]
      Career  — Ship v5 release           [4/9]
  ╭─ 20,000 ft · AREAS OF FOCUS — What do I MAINTAIN? ──────────────╮
    « from horizons.org »  Home · Health · Family · Career · Finances
  ╭─ 30,000 ft · GOALS ────────────────────────────────────────────╮
    « from horizons.org »  Reach conversational Spanish by spring …
  ╭─ 40,000 ft · VISION ───────────────────────────────────────────╮
    « from horizons.org »  In five years I run my own studio …
  ╭─ 50,000 ft · PURPOSE & PRINCIPLES ─────────────────────────────╮
    « from horizons.org »  I exist to make useful things and …
─────────────────────────────────────────────────────────────────────────────
 TAB fold band · g refresh · q quit (restores windows)
```

**Builder transient (reached by `e` in the manager) — live preview redraws on every toggle:**

```
Edit view: Overview of my life
 Horizons   [x] Ground  [x] Projects  [x] Areas  [x] Goals  [x] Vision  [x] Purpose
 Order      (•) Bottom-up (Ground→50k)   ( ) Top-down (50k→Ground)
 Scope      Area of focus: <all>
 ──────────────────────────────────────────────
 RET preview   s save   C-c C-k abort
```

**Keymap**

| Surface | Key | Action |
|--------|-----|--------|
| Rendered buffer | `TAB` / `S-TAB` | Fold one band / all bands |
| Rendered buffer | `g` | Refresh live blocks |
| Rendered buffer | `q` | Quit, restore window config |
| Manager (shared) | `RET` | Render highlighted view |
| Manager (shared) | `c`/`e`/`d`/`D` | Create / edit / duplicate / delete |
| Builder (shared) | `RET` | Live-preview into other window |
| Builder (shared) | `s` / `C-c C-k` | Save (prompts name) / abort (dirty-guard) |

**Live preview:** unchecking `[x] Purpose` and choosing Order → Top-down, then `RET`: the preview window redraws instantly with the Purpose band gone and the stack inverted (50k banner now at top). Same keystroke, the real `org-gtd-view-show` engine — no bespoke renderer.

## 5. Fit with org-gtd

- **Extends:** the **view DSL** (`org-gtd-view-show`, `blocks`/`prefix`, IMPL-085/093) for the live Ground+Projects blocks; **horizons.org** (IMPL-103) as the source of the 20k–50k prose; the **per-area multi-block** shape (IMPL-073) as the stacking model; **command center** (IMPL-135) for the `o` entry. It *consumes* these; it does not manage anything itself.
- **Shared surface / cluster:** Cluster **E — Named-object CRUD managers**. REC-HOR-07 is a **seeded object inside `NEW-VIEW-MANAGER`**, not a parallel manager. It must feel **identical** to any user-created view: same "Your saved views" list row, same CRUD keymap, same builder dirty-guard, same live-preview pane, same fail-soft (a missing `horizons.org` heading renders an empty banner, never an error). The *only* thing unique to HOR-07 is its factory spec and the `horizon-text` block-type it relies on. This confirms the `_CLUSTERS.md` note ("exemplar payload NEW-VIEW-MANAGER could produce") from a UX standpoint — do not build a standalone manager.
- **Reuse vs. new:** reused as-is — the manager, builder, store, preview, command-center. Genuinely new — (a) one factory view spec, and (b) a `horizon-text` block-type that renders a named `horizons.org` heading body as a static read-only banner between live blocks.
- **Release tag:** view DSL, engage/reflect specs, `horizons.org`, areas-of-focus, and the command-center transient are [R] 4.6.1; the whole Cluster-E manager/builder/store is [U] HEAD-and-beyond. No [R] rework — HOR-07 only adds a spec and a block-type on top of [U] surfaces.

### Type / extension-UX opportunities

Real one: the view DSL today can only render **live query blocks**. To fold horizons in, add a **`horizon-text` (file-section) block-type** — `((block-type . horizon-text) (heading . "Vision"))` — that inserts a heading's body from `horizons.org` as a static banner. This is a clean, general DSL extension (any composite view can now interleave prose and queries) and it finally makes `horizons.org` a **first-class DSL citizen** instead of a sidecar file. Register it in `org-gtd-view-lang--known-filter-keys` / `--type-defaults` alongside the existing `native` escape hatch. Second, smaller: the six horizon-heading names are currently a bare template string in `org-gtd-file-horizons-template` and `org-gtd-areas-of-focus` is a separate defcustom — this feature is the nudge to expose the horizon set as a single named registry the block-type and the builder checkboxes both read from.

## 6. Edge cases & failure modes

- **Empty state:** fresh user with no projects and a stock `horizons.org` — bands render with their banner text and a quiet "Nothing here yet" line per live block (the DSL's normal empty-block behavior). Teaches the shape without scolding.
- **Missing/renamed horizon heading:** if `horizons.org` lacks a "Vision" heading, that band's banner renders empty with a one-line hint *"No Vision section in horizons.org — add one with M-x org-gtd-horizons-file"*; never an error (fail-soft, per Cluster-E contract).
- **Large data:** hundreds of next actions make the Ground band long; folding (`TAB`) and the optional area-of-focus scope in the builder are the release valves. Prefix-width keeps columns aligned.
- **Deleted the seed:** user `D`-deletes "Overview of my life" then wants it back — `M-x org-gtd-view-restore-defaults` re-seeds it; message teaches this at delete time.

## 7. Open questions & maintainer decisions

- Default reading direction: spec says "Ground→50k" (bottom-up), matching WF-26's default. Confirm bottom-up is the shipped default (builder still offers top-down).
- Should the 20k Areas band show **live per-area project rollups** (reusing IMPL-073) rather than only `horizons.org` prose? Richer, but heavier; proposed as an opt-in builder toggle, off by default given MAY status.
- Is `horizon-text` worth adding to the DSL for this lowest-priority MAY, or should HOR-07 wait until a second consumer wants prose bands? (Recommend adding it — it is the minimal general primitive.)

## 8. Provenance & links

- `REC-HOR-07` · deliverable #3 **Not-implemented** (DROP-as-mandate 2026-06-05 #3, reinstated MAY; ties REC-UI-16 per-horizon "sets") · `gap-implementation-strategies.md`: "Horizons outline (IMPL-103) + per-area multi-block review (IMPL-073) — compose Ground/Projects in" (build-route hint) · workflow `perspective.feature` WF-26…WF-33 · cluster **E** contract (inherited `crud`), parent `NEW-VIEW-MANAGER`, sibling `REC-CHK-01` · related `REC-UI-16`, `REC-HOR-03/04/05`.
