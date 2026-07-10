# View Manager: Sections Panel + Section Rename Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the builder's view summary + a discoverable section list actually render (fix), and let the user rename the active section's block header (feature) — three clustered problems in one code area, done as small TDD steps.

**Architecture:** All changes are confined to `org-gtd-view-manager.el` (the manager layer) and its unit tests. The display fix relocates the summary/section-list into a group that *has* suffixes (the "Sections" group), because transient 0.13.5 drops suffix-less groups at layout-init time so their `:description` never renders. The rename feature adds a per-section `title` key that round-trips through save/load and is used as the multi-block agenda header (falling back to the synthesized badge when unset).

**Tech Stack:** Emacs Lisp, `transient` 0.13.5 (vendored at `.eldev/30.2/packages/transient-0.13.5/transient.el`), e-unit test framework.

---

## Environment (read first, every task)

- **Work ONLY in** `/home/stag/src/projects/org-gtd.el/.claude/worktrees/view-manager-design`. Do NOT `cd` anywhere else. Stay on branch `view-manager-design`.
- **Run tests** with `.claude/skills/test/run-tests.sh <file-or-category>` — NOT the `/test` skill, NOT `eldev etest` directly. Examples:
  - Single file: `.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el`
  - Category: `.claude/skills/test/run-tests.sh unit`
  - The runner prints a one-line summary like `N tests: N passed, 0 failed` plus the seed. A failure prints the failing assertion.
- **Transient-key tests** use the helper `ogt--transient-suffix-plist` (in `test/helpers/utils.el`); it normalizes a suffix's plist across compiled/uncompiled shapes. Use it exactly like the existing `view-manager-build-test.el` tests do (`(require 'ogt-eunit-prelude "test/helpers/prelude.el")` at top).
- **Compile check** (do at the end, Task 4): `~/bin/eldev compile --warnings-as-errors` then immediately `git checkout -- dir`. NEVER `git add` `dir` or any `*.elc` — those are build artifacts. If compilation regenerated autoloads/`dir`, revert them.
- **Commit after every task** (each task ends with a commit step). Per-task commits only; do not squash.

### Do NOT regress prior branch work

These behaviors are already implemented and tested on this branch — do not break them:

- **P1 (fresh-cons / force):** `--compile-section` pushes a *fresh cons* per key (the compiled spec is cached; sharing cells corrupts `--preview-changed-p`). The explicit `RET` preview passes `force` to bypass the debounce guard.
- **P2 (window / effort):** preview renders with `org-agenda-window-setup` bound to `current-window`; a blank effort input clears the effort filter.
- **3+2 layout:** the five infix groups are laid out as two side-by-side rows (Type/Time/Structural, then Metadata/Prefix) via nested vectors classed `transient-columns`.
- **iv4b (preview-on-open + heading):** the builder force-renders a preview the moment it opens; the summary's `View:` label carries the `transient-heading` face.
- **x3jq (multi-section block headers):** every block in a multi-section (`blocks`) spec gets a `name` synthesized from its badge so org-agenda renders a meaningful header instead of the generic `Headlines with TAGS match: …`. `--build-load` strips the block `name` (sections stay canonical). `--badge-section`/`--compile-section` ignore/drop `name`.
- **Out of scope entirely:** z15n, lk0a. Do not touch them.

### Do NOT touch `org-gtd-view-language.el`

The DSL is released. Every change is on the manager side. Smallest robust change.

---

## Empirical findings from investigation (why the fix is shaped this way)

**A — the suffix-less summary group never renders. Root cause confirmed in the vendored source.**

The builder prefix currently has, as its FIRST group, a suffix-less description group:

```elisp
[:description (lambda () (org-gtd-view-manager--build-summary))]
```

In transient 0.13.5, `transient--init-group` (transient.el ~L2666) builds each group inside an `and-let*` whose LAST binding is:

```elisp
(suffixes (mapcan (##transient--init-child levels % obj)
                  (transient-setup-children obj children)))
```

For a group with **no children**, `suffixes` is `nil`. `and-let*` short-circuits on any nil binding, so `transient--init-group` returns `nil` and the group is **dropped from `transient--layout` entirely** — its `:description` is never inserted. That is why the summary has never been visible.

By contrast, `transient--insert-group :around ((group transient-group))` (transient.el ~L4796) DOES insert a group's description (via `transient-format-description`) — but only for groups that survived init, i.e. groups that HAVE at least one suffix. Every group whose heading currently renders (Type/Time/Structural/Metadata/Prefix/Sections/Actions) has suffixes. So the fix is: **put the summary into the description of a group that has suffixes, and delete the dead suffix-less group.** The "Sections" group is the natural home (its `M-a/M-n/…` keys are the suffixes that keep it alive).

**A group description may be a function and may be multi-line — both confirmed.**

- `transient--get-description` (transient.el ~L5140) funcalls a function description: `(funcall desc obj)`, falling back to `(funcall desc)` on `wrong-number-of-arguments`. So `:description (lambda () (org-gtd-view-manager--build-summary))` works (our function takes no args).
- `transient--insert-group :around` does `(insert desc ?\n)` — a `desc` containing embedded newlines renders as multiple lines above the group's key rows. So a dynamic, N-line section list is fine.
- `transient-format-description ((obj transient-group))` (transient.el ~L5004): if the description string already uses the `face` property anywhere, it is left as-is; otherwise the whole string is blanket-faced `transient-heading`. Our summary puts `transient-heading` on `"View: "` (position 0), so `text-property-not-all` finds a face and the string is used verbatim — only the label is a heading, the section lines render in default face. This preserves iv4b intent.

**Chosen mechanism (justified):** a **function `:description` on the "Sections" group** that returns the multi-line panel. Rejected alternative: `transient-information` info-suffixes (one static suffix per section) — a static suffix list can't grow/shrink with N sections without regenerating the layout, whereas a single function description recomputes the whole N-line panel on every redraw for free. The function description is strictly simpler for a dynamic-length list.

**C — title round-trip semantics (exact, chosen):**

- A section alist MAY carry a manager-level `(title . STRING)`. `title` is NOT a member of `org-gtd-view-manager--filter-specs`, so it is *already* excluded by `--compile-section`'s allow-list and *already* ignored by `--badge-section` (which only iterates filter-specs). We add tests to pin this; no code change is needed in those two functions.
- **Multi-section compile:** each block's `name` = the section's `title` when non-blank, else the synthesized badge (else type name, else the literal `"Section"`). Factored into a new helper `org-gtd-view-manager--section-label`.
- **Single-section compile:** a one-section view compiles to a FLAT spec whose `name` is the VIEW name (unchanged). A flat spec has no per-section header slot, so a `title` on a lone section is NOT persisted — the view name is the block header. This is intentional: titles only matter once there are 2+ blocks. (In-memory the title is kept, so if the user adds a second section before saving, it is used; but save→flat→reload of a single section drops it. Acceptable and stated.)
- **Load round-trip (the hard part):** on loading a `blocks` spec, each block's `name` becomes the section's `title` (and the `name` key is still stripped, keeping sections canonical). So a saved user title reloads as the editable title. Consequence (sticky badge): a block whose `name` was a *synthesized badge* (never explicitly titled) also reloads as a `title`. After one save+reload cycle a synthesized badge becomes a sticky title. This is accepted — state it in the docstring. A fresh in-memory section (never saved) has NO `title`, so its badge is not persisted as a title until save+reload.
- **Proof obligations (tests in Tasks 1–2):** set title → compile (block name = title) → the badge is unchanged → simulate save+load (block name → title) → compile again (still title, not doubled/lost); a section with NO title → compile uses badge → a fresh in-memory section carries no `title` key.

**Rename key:** `M-r`. Verified free. Infix letters are plain (`t w D C o O N H A e W G P x X`); actions are `RET s C-c C-k`; section keys are meta-chords `M-a M-n M-p M-k M-<up> M-<down>`. `M-r` does not collide.

**Macro-expansion caveat:** the builder prefix is generated by the macro `org-gtd-view-manager--define-builder-transient`, expanded by the trailing call `(org-gtd-view-manager--define-builder-transient)` (~L1027). The `[:description …]`, `["Sections" …]` and `["Actions" …]` groups are LITERAL vectors inside the macro's backquoted body (only the five infix columns are generated from the filter-specs table). Edits to those literal groups are ordinary edits, but they only take effect after the file is re-evaluated/recompiled — the transient object is (re)built when the macro call at the bottom re-runs. When testing interactively via an Emacs MCP server, `eval-buffer` the whole file (do not `load` it) so the macro re-expands.

**Test caveat (rendering is manual-QA):** the existing `view-manager-build/summary-label-is-a-heading` test checks the STRING returned by `--build-summary` (face + substrings). It stays valid and we keep it green, but it does NOT prove the string is rendered by transient — only that the content is correct. Actual on-screen rendering is verified in the Task 4 manual re-QA.

---

## Task 1: Section `title` in the model, compile, badge, and load round-trip

Introduces the `title` concept end-to-end EXCEPT the interactive rename command (Task 2) and the display (Task 3).

**Files:**
- Modify: `org-gtd-view-manager.el`
  - Add helper `org-gtd-view-manager--section-label` (new, after `--badge` ~L376).
  - Modify `org-gtd-view-manager--compile-view` multi-section branch (~L415-429) to use the helper.
  - Modify `org-gtd-view-manager--build-load` `blocks` branch (~L607-616) to map block `name` → section `title`.
- Test: `test/unit/view-manager-compile-test.el` (title-in-compile, title dropped from filters, badge ignores title), `test/unit/view-manager-sections-test.el` (load maps name→title; round-trip).

### Step 1: Write the failing tests

Add to the END of `test/unit/view-manager-compile-test.el`, before the `(provide …)` line:

```elisp
(deftest view-manager-compile-view/block-name-uses-title-when-set ()
  "A section's `title' becomes its block name in a multi-section spec."
  (let* ((sections '(((type . next-action) (title . "Today's focus"))
                     ((type . delegated))))
         (spec (org-gtd-view-manager--compile-view "V" sections))
         (blocks (cdr (assq 'blocks spec))))
    (assert-equal "Today's focus" (cdr (assq 'name (nth 0 blocks))))
    ;; No title on the second section -> falls back to its badge.
    (assert-equal (org-gtd-view-manager--badge-section (nth 1 sections))
                  (cdr (assq 'name (nth 1 blocks))))))

(deftest view-manager-compile-view/blank-title-falls-back-to-badge ()
  "A blank/whitespace title is ignored; the block name falls back to the badge."
  (let* ((sections '(((type . next-action) (area-of-focus . "Work") (title . "  "))
                     ((type . delegated))))
         (spec (org-gtd-view-manager--compile-view "V" sections))
         (blocks (cdr (assq 'blocks spec))))
    (assert-equal (org-gtd-view-manager--badge-section (nth 0 sections))
                  (cdr (assq 'name (nth 0 blocks))))))

(deftest view-manager-compile-section/drops-title ()
  "`title' is a manager-level key, never a DSL filter -- compile drops it."
  (let ((sec (org-gtd-view-manager--compile-section
              '((type . next-action) (title . "Focus")))))
    (assert-nil (assq 'title sec))
    (assert-equal 'next-action (cdr (assq 'type sec)))))

(deftest view-manager-badge-section/ignores-title ()
  "A section's `title' does not appear in its badge."
  (let ((with-title (org-gtd-view-manager--badge-section
                     '((type . next-action) (title . "Focus"))))
        (without    (org-gtd-view-manager--badge-section
                     '((type . next-action)))))
    (assert-equal without with-title)))

(deftest view-manager-compile-view/title-round-trips-without-doubling ()
  "Set title -> compile (block name = title) -> load compiled -> title reloaded
   -> compile again -> still the title, not doubled or lost."
  (let* ((sections '(((type . next-action) (title . "Focus"))
                     ((type . delegated))))
         (spec1 (org-gtd-view-manager--compile-view "V" sections)))
    (assert-equal "Focus" (cdr (assq 'name (nth 0 (cdr (assq 'blocks spec1))))))
    (org-gtd-view-manager--build-load spec1)
    (assert-equal "Focus" (cdr (assq 'title
                                      (nth 0 org-gtd-view-manager--build-sections))))
    (let* ((spec2 (org-gtd-view-manager--compile-view
                   org-gtd-view-manager--build-name
                   org-gtd-view-manager--build-sections))
           (blocks (cdr (assq 'blocks spec2))))
      (assert-equal "Focus" (cdr (assq 'name (nth 0 blocks))))
      ;; Not doubled: exactly one name key.
      (assert-equal 1 (length (seq-filter (lambda (c) (eq (car c) 'name))
                                          (nth 0 blocks)))))))
```

Add to the END of `test/unit/view-manager-sections-test.el`, before the `(provide …)`:

```elisp
(deftest view-manager-load/blocks-map-name-to-title ()
  "Loading a blocks spec maps each block's `name' to the section's `title'
   while stripping the raw `name' key (sections stay canonical)."
  (org-gtd-view-manager--build-load
   '((name . "Engage")
     (blocks . (((name . "Morning") (type . calendar))
                ((name . "next-action") (type . next-action))))))
  (assert-nil (assq 'name (nth 0 org-gtd-view-manager--build-sections)))
  (assert-equal "Morning"
                (cdr (assq 'title (nth 0 org-gtd-view-manager--build-sections))))
  (assert-equal "next-action"
                (cdr (assq 'title (nth 1 org-gtd-view-manager--build-sections)))))

(deftest view-manager-load/fresh-section-has-no-title ()
  "A fresh in-memory section carries no `title' (badge is not persisted as one)."
  (org-gtd-view-manager--build-load nil)
  (assert-nil (assq 'title (nth 0 org-gtd-view-manager--build-sections))))
```

### Step 2: Run the tests to verify they fail

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-sections-test.el
```
Expected: the five new compile tests and the two new load tests FAIL. The `--section-label`-dependent ones fail because compile-view does not yet honor `title`; the load ones fail because `--build-load` maps to nothing (no `title`). (`compile-section/drops-title` and `badge-section/ignores-title` may already PASS since `title` is not a filter key — that is fine; they are regression pins. If they pass immediately, keep them.)

### Step 3: Write the minimal implementation

In `org-gtd-view-manager.el`, add the helper immediately after `org-gtd-view-manager--badge` (after the `;;;; Compile` heading is fine; place it just above `--compile-section`):

```elisp
(defun org-gtd-view-manager--section-label (section)
  "Return SECTION's display/header label.
Its `title' when non-blank, else its filter badge, else its type name,
else the literal \"Section\".  Shared by `--compile-view' (the block
header) and the builder's section-list panel so the two never drift."
  (let ((title (alist-get 'title section)))
    (if (and title (not (string-blank-p title)))
        title
      (let ((badge (org-gtd-view-manager--badge-section section)))
        (if (string-empty-p badge)
            (if-let ((type (alist-get 'type section)))
                (symbol-name type)
              "Section")
          badge)))))
```

Replace the multi-section `lambda` inside `org-gtd-view-manager--compile-view` (the `mapcar` at ~L418-428) so the block name comes from the helper:

```elisp
    ;; Multi-section: give every block a `name' so org-agenda renders it as
    ;; the block header instead of falling back to the generic
    ;; `Headlines with TAGS match: …' default (the multi-section defect).
    ;; The header is the section's title when set, else its synthesized badge
    ;; (see `org-gtd-view-manager--section-label').
    (list (cons 'name name)
          (cons 'blocks
                (mapcar
                 (lambda (section)
                   (cons (cons 'name (org-gtd-view-manager--section-label section))
                         (org-gtd-view-manager--compile-section section)))
                 sections)))
```

Modify the `blocks` branch of `org-gtd-view-manager--build-load` (~L611-616) to carry the block `name` over as `title`:

```elisp
   ((assq 'blocks starting-spec)
    (setq org-gtd-view-manager--build-name
          (or (alist-get 'name starting-spec) "Untitled"))
    ;; Strip each block's synthesized `name' header but preserve it as the
    ;; editable per-section `title' so a saved header round-trips as the
    ;; user's title.  Sticky: a previously synthesized badge-name also
    ;; reloads as a title, which is acceptable (see design doc).
    (setq org-gtd-view-manager--build-sections
          (mapcar (lambda (block)
                    (let ((name (alist-get 'name block))
                          (sec (assq-delete-all 'name (copy-alist block))))
                      (if (and name (not (string-blank-p name)))
                          (cons (cons 'title name) sec)
                        sec)))
                  (alist-get 'blocks starting-spec))))
```

### Step 4: Run the tests to verify they pass

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-sections-test.el
```
Expected: all tests PASS, including the pre-existing `view-manager-compile-view/blocks-round-trip-stable`, `view-manager-compile-view/blocks-carry-badge-names`, `view-manager-load/blocks-spec-strips-synthesized-names` (these stay green: the block name for an untitled section is still exactly the badge, and the stored specs in those tests use block names equal to the badges, so mapping name→title yields a title equal to the badge and compile reproduces the same block name).

### Step 5: Commit

```bash
git add org-gtd-view-manager.el test/unit/view-manager-compile-test.el test/unit/view-manager-sections-test.el
git commit -m "feat(view-manager): per-section title round-trips as the block header

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: The `--section-rename` command wired to `M-r`

**Files:**
- Modify: `org-gtd-view-manager.el`
  - Add `org-gtd-view-manager--section-rename` in the `;;;; Section commands` section (after `--section-move-down` ~L922).
  - Add the `M-r` binding to the Sections group inside the builder macro (~L992-998).
- Test: `test/unit/view-manager-build-test.el` (rename sets/clears title; `M-r` is bound).

### Step 1: Write the failing tests

Add to the END of `test/unit/view-manager-build-test.el`, before the closing comment line:

```elisp
(deftest view-manager-build/section-rename-key-is-bound ()
  "The Sections group binds `M-r' to the rename command."
  (let ((plist (ogt--transient-suffix-plist
                'org-gtd-view-manager--build "M-r")))
    (assert-equal "M-r" (plist-get plist :key))))

(deftest view-manager-build/section-rename-sets-title ()
  "Rename stores the entered string as the active section's title."
  (org-gtd-view-manager--build-load '((name . "V") (type . next-action)))
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "My focus"))
            ((symbol-function 'org-gtd-view-manager--preview-schedule) #'ignore))
    (org-gtd-view-manager--section-rename))
  (assert-equal "My focus"
                (cdr (assq 'title (nth 0 org-gtd-view-manager--build-sections))))
  (assert-true org-gtd-view-manager--build-dirty))

(deftest view-manager-build/section-rename-blank-clears-title ()
  "A blank rename input removes the title (falls back to the badge)."
  (org-gtd-view-manager--build-load
   '((name . "V") (blocks . (((name . "Kept") (type . next-action))
                             ((name . "Other") (type . delegated))))))
  ;; Active is section 0, which loaded with title "Kept".
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   "))
            ((symbol-function 'org-gtd-view-manager--preview-schedule) #'ignore))
    (org-gtd-view-manager--section-rename))
  (assert-nil (assq 'title (nth 0 org-gtd-view-manager--build-sections))))
```

Note: these three tests need `cl-lib` for `cl-letf`; `view-manager-build-test.el` already loads the prelude which pulls it in (the existing `save-rejects-blank-name` test uses `cl-letf`), so no extra require.

### Step 2: Run the tests to verify they fail

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
```
Expected: the three new tests FAIL — `section-rename-key-is-bound` because `M-r` is unbound (`transient-get-suffix` returns nil), the other two because `org-gtd-view-manager--section-rename` is not `fboundp`.

### Step 3: Write the minimal implementation

Add the command in the `;;;; Section commands` section (after `--section-move-down`):

```elisp
(defun org-gtd-view-manager--section-rename ()
  "Prompt for and set the ACTIVE section's title (its block header).
The title is used as the section's agenda block header in a
multi-section view; a blank entry CLEARS it, falling back to the
synthesized badge.  Marks the builder dirty and refreshes the preview.

Note: in a one-section view the header is the view name (a flat spec
has no per-section slot), so a title set on a lone section only takes
effect once a second section exists."
  (interactive)
  ;; Sync first so the active slot reflects any in-flight edits, then edit
  ;; `--build-state' (the active section's alist) and sync back: setting a
  ;; NEW key via `alist-get' reassigns `--build-state' to a fresh list that
  ;; is no longer `eq' to its section slot, so the write-back is required
  ;; (the P1 fresh-cons pattern, see `;;;; Section state').
  (org-gtd-view-manager--build-sync-active)
  (let* ((current (alist-get 'title org-gtd-view-manager--build-state))
         (input (read-string "Section title: " current))
         (value (and (not (string-blank-p input)) input)))
    (if value
        (setf (alist-get 'title org-gtd-view-manager--build-state) value)
      (setq org-gtd-view-manager--build-state
            (assq-delete-all 'title org-gtd-view-manager--build-state)))
    (org-gtd-view-manager--build-sync-active)
    (setq org-gtd-view-manager--build-dirty t)
    (org-gtd-view-manager--preview-schedule)))
```

Add the `M-r` binding to the Sections group vector inside `org-gtd-view-manager--define-builder-transient` (the `["Sections" …]` literal). Insert after the `M-k` Delete row:

```elisp
         ["Sections"
          ("M-a"      "Add"       org-gtd-view-manager--section-add       :transient t)
          ("M-n"      "Next"      org-gtd-view-manager--section-next      :transient t)
          ("M-p"      "Prev"      org-gtd-view-manager--section-prev      :transient t)
          ("M-r"      "Rename"    org-gtd-view-manager--section-rename    :transient t)
          ("M-k"      "Delete"    org-gtd-view-manager--section-delete    :transient t)
          ("M-<up>"   "Move up"   org-gtd-view-manager--section-move-up   :transient t)
          ("M-<down>" "Move down" org-gtd-view-manager--section-move-down :transient t)]
```

(Task 3 replaces the literal `"Sections"` heading here with a function `:description`; leave it as `"Sections"` for now so this task's diff stays focused.)

### Step 4: Run the tests to verify they pass

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
```
Expected: all tests PASS, including the pre-existing `section-keys-are-bound` and `every-infix-and-action-key-is-bound` (unchanged keys still bound).

### Step 5: Commit

```bash
git add org-gtd-view-manager.el test/unit/view-manager-build-test.el
git commit -m "feat(view-manager): M-r renames the active section's block header

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Render the summary + section-list panel (the display fix)

Reshape `--build-summary` into a multi-line panel (header line + one `▸`-marked line per section), move it onto the Sections group's `:description`, and DELETE the dead suffix-less `[:description …]` group.

**Files:**
- Modify: `org-gtd-view-manager.el`
  - Reshape `org-gtd-view-manager--build-summary` (~L645-666).
  - In the builder macro: remove the `[:description (lambda () (org-gtd-view-manager--build-summary))]` group (~L990); change the Sections group heading from `"Sections"` to `:description (lambda () (org-gtd-view-manager--build-summary))` (~L992).
- Test: `test/unit/view-manager-build-test.el` (extend the summary-content test to assert the section list + `▸`; the builder still binds every key after the group removal).

### Step 1: Write the failing tests

Add to `test/unit/view-manager-build-test.el` (the existing `summary-label-is-a-heading` STAYS — do not delete it):

```elisp
(deftest view-manager-build/summary-lists-sections-with-active-marker ()
  "The summary lists each section on its own line with `▸' on the active one
   and shows the title (or badge) per section."
  (org-gtd-view-manager--build-load
   '((name . "Engage")
     (blocks . (((name . "Morning") (type . calendar))
                ((name . "Errands") (type . next-action))))))
  ;; Active is section 0.
  (let* ((summary (org-gtd-view-manager--build-summary))
         (plain (substring-no-properties summary))
         (lines (split-string plain "\n")))
    (assert-true (string-prefix-p "View: " plain))
    (assert-true (string-match-p "Section 1/2" plain))
    ;; One header line + two section lines.
    (assert-equal 3 (length lines))
    ;; Active marker on the first section, not the second.
    (assert-true (string-match-p "\\`  ▸" (nth 1 lines)))
    (assert-true (string-match-p "Morning" (nth 1 lines)))
    (assert-false (string-match-p "▸" (nth 2 lines)))
    (assert-true (string-match-p "Errands" (nth 2 lines)))))
```

The existing `every-infix-and-action-key-is-bound` and `section-keys-are-bound` tests already assert the full key set survives; after removing the description group they must still pass (they will — no keys are removed). No new key test is needed here.

### Step 2: Run the test to verify it fails

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
```
Expected: `summary-lists-sections-with-active-marker` FAILS — today's `--build-summary` returns a single line with `[ badge | badge ]`, so `(length lines)` is 1, not 3.

### Step 3: Write the minimal implementation

Reshape `org-gtd-view-manager--build-summary`:

```elisp
(defun org-gtd-view-manager--build-summary ()
  "Return the multi-line section panel shown atop the builder.
Line 1 is `View: <name>' (the label a `transient-heading', iv4b)
followed by the active-section marker `Section i/N'.  Each remaining
line lists a section: `▸' on the active one, its 1-based index, and its
title-or-badge (see `org-gtd-view-manager--section-label').

This STRING is content-tested in the unit suite; whether transient
RENDERS it is verified by manual QA (it is used as the Sections group's
`:description', a group that has suffixes and therefore renders --
suffix-less description groups are dropped by transient at init)."
  (org-gtd-view-manager--build-sync-active)
  (let* ((n (length org-gtd-view-manager--build-sections))
         (active org-gtd-view-manager--build-active)
         (lines
          (seq-map-indexed
           (lambda (sec i)
             (format "  %s %d  %s"
                     (if (= i active) "▸" " ")
                     (1+ i)
                     (org-gtd-view-manager--section-label sec)))
           org-gtd-view-manager--build-sections)))
    (concat
     (propertize "View: " 'face 'transient-heading)
     org-gtd-view-manager--build-name
     "  —  "
     (format "Section %d/%d" (1+ active) n)
     "\n"
     (string-join lines "\n"))))
```

In the builder macro, remove the dead group and move the summary onto the Sections group. Change:

```elisp
         [:description (lambda () (org-gtd-view-manager--build-summary))]
         ,@rows
         ["Sections"
          ("M-a"      "Add"       org-gtd-view-manager--section-add       :transient t)
```

to:

```elisp
         ,@rows
         [:description (lambda () (org-gtd-view-manager--build-summary))
          ("M-a"      "Add"       org-gtd-view-manager--section-add       :transient t)
```

(Delete the standalone `[:description …]` line entirely; the Sections group's leading `"Sections"` string is replaced by the `:description (lambda …)` pair. Keep the six/seven section key rows unchanged, including the `M-r` row from Task 2.)

### Step 4: Run the tests to verify they pass

Run:
```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
```
Expected: all PASS — the new list test, the kept `summary-label-is-a-heading` (line 1 still starts `View: ` with the heading face and contains `Untitled`/`Section 1/1` for a one-section load), and every key-binding test.

### Step 5: Commit

```bash
git add org-gtd-view-manager.el test/unit/view-manager-build-test.el
git commit -m "fix(view-manager): render the section panel via the Sections group description

A suffix-less description group is dropped by transient at layout init,
so the view summary never rendered.  Move it onto the Sections group
(which has suffixes) and reshape it into a per-section list with an
active marker.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 4: Regression sweep, compile check, and manual re-QA

**Files:** none modified (verification only). If the compile check surfaces a warning, fix it and re-run.

### Step 1: Full unit suite

Run:
```bash
.claude/skills/test/run-tests.sh unit
```
Expected: `N tests: N passed, 0 failed`. Pay attention to `view-manager-*` files. If any failure, capture the printed `Using seed: …` and re-run with `--seed=N` to reproduce before debugging (use superpowers:systematic-debugging).

### Step 2: Compile with warnings as errors

Run:
```bash
~/bin/eldev compile --warnings-as-errors
git checkout -- dir
git status --short
```
Expected: compilation succeeds with no warnings. `git status` must NOT show `dir` or any `*.elc` staged/modified as something you would commit. If autoloads or `dir` changed, `git checkout --` them. NEVER `git add dir` or `*.elc`.

### Step 3: Manual re-QA (visual — the rendering the unit tests cannot prove)

Start a clean Emacs with the branch loaded (or `eval-buffer` `org-gtd-view-manager.el` in a running Emacs after loading org-gtd — do NOT `load` the file, per the MEMORY note about Emacs caching). Then:

1. `M-x org-gtd-view-manager` → press `c` (Create). The builder opens.
   - **CONFIRM (A+B):** at the top of the Sections group you SEE a panel like:
     ```
     View: Untitled  —  Section 1/1
       ▸ 1  next-action
     ```
     with `View:` shown as a heading. (Before this branch, no summary line appeared at all.)
2. Press `M-a` (Add) once. The panel now shows two lines; `▸` marks the newly-active section 2:
   ```
   View: Untitled  —  Section 2/2
     1  next-action
   ▸ 2  next-action
   ```
   Press `M-p`/`M-n` and watch `▸` and `Section i/N` move.
3. Press `M-r` (Rename). Enter `Morning`. **CONFIRM (C):**
   - The active section's line now reads `▸ 2  Morning`.
   - The composite agenda preview's block header for that section updates to `Morning`.
4. Press `M-r` again, clear the input (empty RET). The line falls back to the badge (`next-action`), and the preview header reverts.
5. Press `s` (Save), name it `QA View`. Reopen `M-x org-gtd-view-manager`, `e` (Edit) it. **CONFIRM round-trip:** the renamed section still shows `Morning` in the panel and as its preview header (title survived save+reload).
6. Press `C-c C-k` (Abort) / `q` to confirm windows restore cleanly (P2 not regressed).

If every CONFIRM holds, the feature is complete. If not, do NOT edit code blindly — reproduce under a proper org-gtd setup (see the QA memory note) and debug with superpowers:systematic-debugging.

### Step 4: Commit (only if Step 2 required a fix)

If the compile step forced a source change, commit it:
```bash
git add org-gtd-view-manager.el
git commit -m "fix(view-manager): silence compile warning in section panel

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```
Otherwise no commit — this task is pure verification.

---

## Notes for the executor

- **Smallest robust change:** everything is in `org-gtd-view-manager.el` + its tests. Do NOT touch `org-gtd-view-language.el`.
- **`--section-label` is the single source of truth** for a section's header/label — both `--compile-view` (the block name) and `--build-summary` (the panel line) call it, so the on-screen list can never drift from the saved header.
- **Sticky-badge semantics are intentional.** A synthesized badge that round-trips through save→load becomes an editable `title`. This is documented in `--build-load` and acceptable per the design.
- **Rendering is manual-QA.** The unit tests pin the STRING content of `--build-summary` and the key bindings; only Task 4 Step 3 proves transient actually paints the panel.
- **Do not regress** P1, P2, 3+2, iv4b, x3jq (listed under Environment). The existing tests for those stay green throughout.

## Execution Handoff

Plan complete and saved to `docs/plans/2026-07-10-view-manager-sections-panel-and-rename-plan.md`. Two execution options:

1. **Subagent-Driven (this session)** — dispatch a fresh subagent per task with a code-review checkpoint between tasks (superpowers:subagent-driven-development).
2. **Parallel Session (separate)** — open a new session in this worktree and batch-execute with checkpoints (superpowers:executing-plans).
