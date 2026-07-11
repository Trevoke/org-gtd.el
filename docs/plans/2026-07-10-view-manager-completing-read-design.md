# View Manager — completing-read picker — Design

**Goal:** Replace the view manager's in-transient full view list (a
newline-joined wall of text that breaks down at ~20 views) with a
`completing-read` picker, preserving at-a-glance section/badge visibility via
completion annotations.

**Status:** design agreed via brainstorming Q&A 2026-07-10. Feeds a TDD
implementation plan.

---

## 1. Problem

`org-gtd-view-manager` is currently a `transient-define-prefix` whose
`:description` calls `--rows`, rendering *every* saved view as a
newline-joined block with a `▸` cursor moved via up/down; the action keys
(Open/Create/Edit/Duplicate/Delete) act on the cursored row. This does not
scale — 20 views is a 20-line wall of text inside the transient.

`org-gtd-view-run` already provides a plain `completing-read` for the pure
"open a view" path, so recall-to-open already scales. The thing that does not
scale is the **manager** (browse + act), because a `completing-read` picks one
view and does one thing, whereas the manager offers many actions per view.

## 2. Decisions (from Q&A)

- **Action model:** *select-then-act.* `completing-read` picks ONE view
  (name + section-badge annotation), then a small action transient opens
  scoped to that view.
- **Action keys (rekeyed):** `o` Open · `e` Edit · `n` New · `c` Copy ·
  `d` Delete · `q` Quit. (`n` New ignores the selection and opens a fresh
  builder; `c` Copy is the former "duplicate"; `d` Delete confirms.)
- **Reorder:** *dropped.* Once the list is a fuzzy completing-read, manual
  ordering loses its point and "move the selected view up/down" is
  meaningless with no visible list. Candidates show in store (insertion)
  order.

## 3. Flow & entry point

`org-gtd-view-manager` stops being a transient and becomes a small command:

```
M-x org-gtd-view-manager
  ├─ migrate-once (unchanged)
  ├─ store empty? ──► open the builder fresh (nothing to manage)
  └─ else ─► completing-read picks ONE view (name + section badge annotation)
              └─► action transient opens, scoped to that view
```

The picked view's **name** is stashed in a new dynamic var
`org-gtd-view-manager--selected` (matching the codebase's dynamic-var style:
`--build-state`, `--highlight`). The action transient's suffixes read it; its
`:description` shows the selected view's name + badge.

```elisp
(defun org-gtd-view-manager ()
  (interactive)
  (org-gtd-view-manager--migrate-once)
  (let ((views (org-gtd-view-manager--store-read)))
    (if (null views)
        (org-gtd-view-manager--build)                 ; nothing to manage
      (setq org-gtd-view-manager--selected
            (org-gtd-view-manager--pick-view views))
      (org-gtd-view-manager--act))))
```

**Empty-store behavior change:** today an empty store shows teaching text and
`RET`→Engage. In the new model there is no list to host teaching text, and
invoking the *manager* with zero views means you want to make one — so an
empty store opens the builder directly. `org-gtd-engage` remains the separate
daily-view command; Engage access is not lost, just not routed from an empty
manager.

## 4. The annotated picker

A single shared helper builds the completing-read so the manager and
`org-gtd-view-run` render identically:

```elisp
(defun org-gtd-view-manager--annotate-view (name views)
  "Return the annotation string (dimmed badge) for view NAME within VIEWS."
  (let ((spec (cdr (assoc name views))))
    (concat "  " (propertize (org-gtd-view-manager--badge spec)
                             'face 'completions-annotations))))

(defun org-gtd-view-manager--pick-view (views &optional prompt)
  "Read a saved-view NAME from VIEWS, annotating each with its badge."
  (let* ((annotate (lambda (name)
                     (org-gtd-view-manager--annotate-view name views)))
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata (annotation-function . ,annotate)
                                 (category . org-gtd-view))
                    (complete-with-action action (mapcar #'car views) str pred)))))
    (completing-read (or prompt "View: ") table nil t)))
```

**What renders.** The candidate is the bare name; the badge trails as an
annotation. `--badge` already produces the summary the user liked — a flat
view reads `next-action · Home · <30m`; a multi-section view reads
`3 sections: calendar · next-action · delegated`. So "sections visible in the
completing-read" is literally reusing `--badge`; no new formatting.

```
View: ▏
  Weekend errands   next-action · Home · <30m
  Engage            3 sections: calendar · next-action · delegated
  Big rocks         project · not-done
```

**Cross-UI rendering.** `annotation-function` is the built-in mechanism, so
this works everywhere: vertico/marginalia render the badge in their annotation
column; default `*Completions*` appends it after the name. The
`completions-annotations` face reads as dimmed secondary text, not part of the
name typed.

**`annotation-function` over `affixation-function`:** annotation is simpler and
enough — the badge trails the name with a two-space gap. Hard-aligning badges
into a column later is a one-function swap, not a redesign.

**Category `org-gtd-view`** tags the completion so a user's marginalia/embark
config can target it — costs nothing.

## 5. The action transient

Internal `org-gtd-view-manager--act`, scoped to `--selected`:

```elisp
(defun org-gtd-view-manager--act-description ()
  (let* ((name org-gtd-view-manager--selected)
         (spec (org-gtd-view-manager--store-get name)))
    (format "%s\n%s"
            (propertize name 'face 'transient-heading)
            (org-gtd-view-manager--badge spec))))

(transient-define-prefix org-gtd-view-manager--act ()
  [:description org-gtd-view-manager--act-description
   [("o" "Open"   org-gtd-view-manager--act-open)
    ("e" "Edit"   org-gtd-view-manager--act-edit)
    ("n" "New"    org-gtd-view-manager--act-new)
    ("c" "Copy"   org-gtd-view-manager--act-copy)
    ("d" "Delete" org-gtd-view-manager--act-delete)
    ("q" "Quit"   transient-quit-one)]]
  (interactive)
  (transient-setup 'org-gtd-view-manager--act))
```

Each suffix reads `--selected` and calls `--store-get` directly — no cursor
indirection. `o/e/n/c` hand off (agenda or builder) and exit the transient:

- **Open** (`o`): `org-gtd-view-show` of the selected spec. Always has a real
  spec — you can only reach the transient with a picked view — so the old
  empty-store→Engage branch is gone.
- **Edit** (`e`): `--build` on the selected spec.
- **New** (`n`): `--build` fresh, ignoring the selection.
- **Copy** (`c`): `--build` on a `"<name> copy"` spec (former duplicate logic;
  copy is NOT pre-persisted — save creates it, abort leaves no orphan).
- **Delete** (`d`): `y-or-n` confirm, `--store-delete`, then **re-launch the
  picker** if views remain (clean up several in a row), else message
  `"No saved views remain."` — it does NOT pop the builder after deleting the
  last view.

The public autoload cookie moves from the old transient to the new plain
`org-gtd-view-manager` command; `--act` is internal.

## 6. `org-gtd-view-run` (recall)

Kept, and refactored to reuse `--pick-view` so its completion also shows the
section badges — DRY, and consistent visuals between "just open one" and
"manage." Behavior is otherwise unchanged (open the chosen spec via
`org-gtd-view-show`; `user-error` on empty store).

## 7. Deleted as dead code

- `--rows` (the wall-of-text description)
- `--highlight` defvar, `--list-up`, `--list-down`
- `--list-highlighted-name`, `--list-highlighted-spec`
- `--list-window-config`, `--list-quit` (no inline render to restore; Quit =
  `transient-quit-one`; Open hands to org-agenda exactly as `org-gtd-view-run`
  already does without restore, so parity holds)
- the old `transient-define-prefix org-gtd-view-manager`
- `--list-create`, `--list-edit`, `--list-render`, `--list-duplicate`,
  `--list-delete` (superseded by `--act-*`)

## 8. Testing

**Headless unit tests:**
- `--annotate-view (name views)` returns `"  <badge>"` (pure; dimmed face) for
  flat and multi-section specs.
- `--pick-view` metadata: the table returns `(metadata (annotation-function
  . …) (category . org-gtd-view))`; the annotation-function yields the badge.
- `--act-*` dispatch, with `--selected` bound and the store stubbed: Open →
  `org-gtd-view-show` with the right spec (spy); Edit → `--build` with the
  spec; New → `--build` with no spec; Copy → `--build` with a `"<name> copy"`
  spec; Delete → store no longer contains the name; Delete-of-last → messages
  cleanly, no error, no builder.
- Empty-store entry → `--build` invoked (spy).
- `org-gtd-view-run` still opens the chosen spec; empty store still
  `user-error`s.
- `--badge` is unchanged (already covered).

**Manual QA (visual/interactive):**
- The live completing-read annotation rendering under vertico and under
  default `*Completions*`.
- The action transient keys `o/e/n/c/d/q` and the delete→re-pick loop.

## 9. Scope guardrails

- Do NOT change the view DSL / `org-gtd-view-show`, `--badge`, `--build`,
  `--store-*`, or the builder — this is purely the browse/recall surface.
- Do NOT regress the builder work (P1 fresh-cons, P2 window/effort-clear, 3+2
  layout, iv4b preview-on-open, multi-section sections panel).
- Keep `org-gtd-view-run` behavior identical apart from the shared annotated
  picker.
