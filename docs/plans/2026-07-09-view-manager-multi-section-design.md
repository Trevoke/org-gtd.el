# View Manager — Multi-Section Views (eneg) — Design

**Goal:** Let a saved view contain multiple **sections** (blocks) — e.g. a Calendar
section, a Next-actions section, a Delegated section — built and previewed in the
builder without writing elisp. This is the "manage my whole GTD system view" gap
(yak `…compose-multi-section…eneg`).

**Status:** design agreed via QA Q&A 2026-07-09. Feeds a TDD implementation plan.

---

## 1. Decisions (from Q&A)

- **Builder flow:** *single builder + section switcher.* One builder edits the
  **active** section; dedicated keys add / switch / delete / reorder sections. The
  existing infixes (type + filters + prefix) apply to whichever section is active.
- **v1 scope:** *full* — add, edit, delete, **reorder** (order = block order in the
  rendered agenda).
- **Live preview:** *whole composite view* — every render shows all sections (the
  full multi-block agenda), so the section you're editing is seen in context.

## 2. DSL mapping (already supported — verified)

`org-gtd-view-show` renders a multi-block view from a single stored spec with a
`blocks` key; `org-gtd-view-lang--create-custom-commands` turns each block spec
into an agenda block. So a multi-section view is stored as ONE `name → spec`
entry:

```elisp
((name . "My Engage View")
 (blocks . ( ((type . calendar))
             ((type . next-action) (area-of-focus . "Work"))
             ((type . delegated)) )))
```

Each block is exactly what the builder builds today (type + filters + prefix,
**minus** the name). No DSL change is needed. `org-gtd-view-show` also still
renders a flat single-section spec, so **single-section views stay flat** (no
`blocks` wrapper) for full backward compatibility.

## 3. Builder state model

The current builder keeps the active section's spec in `org-gtd-view-manager--build-state`
(a `key → value` alist including `name`). We split the view name out and add a
section layer around the *unchanged* active-section alist, so all existing infix /
`--set-value` / `--compile-section` code keeps operating on `--build-state`:

- `org-gtd-view-manager--build-name` — the view's name (string). View-level, NOT per section.
- `org-gtd-view-manager--build-sections` — an ordered list of section alists (each a
  `key → value` alist of type+filters+prefix, **no** `name`).
- `org-gtd-view-manager--build-active` — index of the active section.
- `org-gtd-view-manager--build-state` — the **active** section's alist. On a section
  switch: write `--build-state` back into `(nth active sections)`, set the new
  `active`, load `--build-state` from the new section. (`--build-state` is always
  the live active-section object; keep it and `--build-sections` in sync.)

Rationale: the infixes and `--set-value` already edit `--build-state`; keeping
that as "the active section" means the infix layer is untouched. Only a thin
section-management layer + the name split are new.

### Section-management keys (constraint)

The builder already binds infix letters `t w D C o O N H A e W G P x X` and actions
`RET s C-c C-k`. Section keys MUST avoid those (notably `N` is `not-done`). Put them
in a dedicated **"Sections"** group. Proposed (final letters chosen in the plan):
add section, next/prev section (switch active), delete section, move section
up/down (reorder). Prefer meta-chorded or punctuation keys to stay clear of infix
letters, e.g. `M-a` add · `M-n`/`M-p` next/prev · `M-k` delete · `M-<up>`/`M-<down>`
reorder — the plan finalizes exact keys and verifies no collision against the
filter-spec table.

## 4. Compile

`org-gtd-view-manager--compile` (currently: state alist → flat spec) is refactored:

- Keep a section compiler `--compile-section` (today's `--compile` logic: allow-list
  ∩ non-nil, fresh cons per P1) producing one section's `key → value` alist (no name).
- `--compile-view` assembles the stored spec:
  - **one section** → `(cons (cons 'name NAME) (--compile-section (car sections)))`
    — a FLAT spec, identical to today (back-compat, no `blocks`).
  - **multiple sections** → `((name . NAME) (blocks . (S0 S1 …)))` where each `Sn`
    is `(--compile-section section-n)`. Empty sections (no keys) are dropped or
    rendered as their type default — plan decides; default: a section always has at
    least a `type`, so it's never empty.

The whole-view preview compiles via `--compile-view` and renders through the
existing `--preview-now`/`--render-preview` path (P1 fresh-cons + changed-p, P2
window binding, and preview-on-open all carry over unchanged — they now cache the
composite spec).

## 5. Load / edit an existing saved view

- Flat spec `((name . N) …filters…)` → `build-name = N`, `build-sections = (list
  (the spec minus name))`, `active = 0`. Opens as a **one-section** view (identical
  to today's edit).
- Blocks spec `((name . N) (blocks . (S0 S1 …)))` → `build-name = N`,
  `build-sections = (S0 S1 …)`, `active = 0`.
- A fresh create starts with `build-name = "Untitled"`, one default section
  `((type . next-action))`, `active = 0`.

## 6. Summary / badge

`--build-summary` becomes view-aware: show the view name as heading (iv4b), the
active section marker, and a compact list of section badges, e.g.

```
View: My Engage View
Section 2/3   [ calendar | ▸ next-action · Work | delegated ]
```

The list manager's row badge (`--badge` over a stored spec) must also handle a
`blocks` spec: summarize as `N sections: <b0> · <b1> · …` (or similar) instead of
treating `blocks` as an unknown key.

## 7. Save / store

Unchanged mechanism — `name → spec`, where `spec` is now flat (1 section) or a
`blocks` spec (≥2). Blank-name, overwrite, rename-move, duplicate-no-orphan guards
all operate on the view name and are unaffected. Migration of legacy custom views
is unaffected (they're single-section flat specs → one-section views).

## 8. Edge cases

- **Minimum one section:** deleting the last section is refused (a view needs ≥1
  section); deleting the active section moves `active` to a valid neighbor.
- **Reorder bounds:** move-up at index 0 / move-down at last index are no-ops.
- **Per-section fail-soft:** the whole-view preview must fail-soft — a section whose
  spec renders nothing shows org-agenda's "no matches" for that block; a section
  that errors is surfaced as the existing one-line teaching message, never a stack
  trace (design §8), and must not abort the other sections' render where possible.
- **Back-compat round-trip:** a one-section view saved, reloaded, and edited stays a
  flat spec; adding a second section then saving produces a `blocks` spec; deleting
  back down to one section saves flat again.

## 9. What is / isn't testable

- **Headless unit tests:** all state transitions — add / switch / delete / reorder /
  active-index bookkeeping; `--compile-view` producing a flat spec for one section
  and a `blocks` spec for many; load from flat vs blocks spec; the min-one-section
  and reorder-bounds guards; `--badge` summarizing a `blocks` spec; whole-view
  preview compiling the composite (render-count / captured-spec via stub).
- **Manual re-QA (visual/interactive):** the section-switcher keys in a live
  transient; the composite agenda actually rendering all blocks; the summary
  reading clearly; reorder reflecting in block order on screen.

## 10. Scope guardrails

- Do NOT change the view DSL / `org-gtd-view-show` (multi-block already works).
- Do NOT regress P1 (fresh-cons + force), P2 (window + effort-clear), the 3+2
  column layout, or iv4b (preview + summary on open).
- Out of scope for this feature: the remaining yaks `z15n` (rename Render) and
  `lk0a` (sample-data message spam).
