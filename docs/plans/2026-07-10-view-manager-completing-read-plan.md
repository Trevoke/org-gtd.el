# View Manager completing-read Picker — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans (or
> superpowers:subagent-driven-development) to implement this plan task-by-task.

**Goal:** Replace the view manager's in-transient wall-of-text view list with a
`completing-read` picker (name + section-badge annotation) feeding a small
select-then-act transient (`o/e/n/c/d/q`).

**Architecture:** `org-gtd-view-manager` becomes a plain command: pick a view
via an annotated `completing-read`, stash its name in a dynamic var, then open
an action transient scoped to it. `org-gtd-view-run` reuses the same picker.
The old cursor/`▸`/reorder machinery is deleted.

**Tech Stack:** Emacs Lisp, transient.el, e-unit tests. Design doc:
`docs/plans/2026-07-10-view-manager-completing-read-design.md`.

**Conventions (read before starting):**
- Tests live in `test/unit/view-manager-*.el`; run with the **`/test` skill**
  (Skill tool), e.g. `.claude/skills/test/run-tests.sh
  test/unit/view-manager-list-test.el` — never call `eldev etest` directly.
- Test prelude pattern (copy from `view-manager-run-test.el`): `(require
  'ogt-eunit-prelude "test/helpers/prelude.el")`, `(e-unit-initialize)`, and an
  `around-each` wrapping `ogt-eunit-with-mock-gtd`.
- Spy/stub pattern: `cl-letf (((symbol-function 'FN) (lambda (&rest _) …)))`.
- Simulate minibuffer input: `with-simulated-input "text RET"`.
- Transient key assertions: `(ogt--transient-suffix-plist 'PREFIX "KEY")`.
- The MCP emacs server caches file loads — `eval-buffer`/redefine, don't
  file-load (see CLAUDE.md).
- Frequent commits; DRY; YAGNI; watch every test fail before implementing.

---

## Task 1: The annotated picker helper

Two pure-ish helpers: `--annotate-view` (name → dimmed badge string) and
`--pick-view` (the `completing-read` with annotation metadata). Shared by the
manager and `org-gtd-view-run`.

**Files:**
- Modify: `org-gtd-view-manager.el` (add near the Recall section, before
  `org-gtd-view-run` at ~`:1261`)
- Test: `test/unit/view-manager-pick-test.el` (create)

**Step 1: Write the failing tests**

Create `test/unit/view-manager-pick-test.el`:

```elisp
;;; view-manager-pick-test.el --- Tests for the annotated view picker -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for `org-gtd-view-manager--annotate-view' and `--pick-view':
;; the shared completing-read that annotates each saved view with its badge.
;;
;;; Code:

(require 'cl-lib)
(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest view-manager-pick/annotate-shows-badge ()
  "The annotation is the view's badge, dimmed, with a leading gap."
  (let* ((views '(("Errands" . ((name . "Errands") (type . next-action)
                                (area-of-focus . "Home")))))
         (s (org-gtd-view-manager--annotate-view "Errands" views)))
    (assert-true (string-match-p "next-action · Home" s))
    (assert-equal 'completions-annotations
                  (get-text-property (1- (length s)) 'face s))))

(deftest view-manager-pick/annotate-summarizes-sections ()
  "A multi-section view annotates with the `N sections: …' badge."
  (let* ((views '(("Engage" . ((name . "Engage")
                               (blocks . (((type . calendar))
                                          ((type . next-action))))))))
         (s (org-gtd-view-manager--annotate-view "Engage" views)))
    (assert-true (string-match-p "2 sections: calendar · next-action" s))))

(deftest view-manager-pick/table-exposes-annotation-metadata ()
  "The completion table advertises an annotation-function and category."
  (let* ((views '(("A" . ((name . "A") (type . next-action)))))
         (table (org-gtd-view-manager--completion-table views))
         (meta (funcall table "" nil 'metadata))
         (md (cdr meta)))
    (assert-equal 'org-gtd-view (cdr (assq 'category md)))
    (assert-true (functionp (cdr (assq 'annotation-function md))))
    (assert-true (string-match-p
                  "next-action"
                  (funcall (cdr (assq 'annotation-function md)) "A")))))

(deftest view-manager-pick/pick-view-returns-selected-name ()
  "`--pick-view' returns the chosen name from the annotated completion."
  (let ((views '(("A" . ((name . "A") (type . next-action)))
                 ("B" . ((name . "B") (type . project))))))
    (assert-equal "B"
                  (with-simulated-input "B RET"
                    (org-gtd-view-manager--pick-view views)))))

(provide 'view-manager-pick-test)
;;; view-manager-pick-test.el ends here
```

> Note: the design shows the table built inline inside `--pick-view`. To make
> the metadata unit-testable, extract the collection function as
> `--completion-table`; `--pick-view` just wraps it in `completing-read`.

**Step 2: Run the tests to verify they fail**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-pick-test.el`
Expected: FAIL — `--annotate-view` / `--completion-table` / `--pick-view` are
void functions.

**Step 3: Implement the helpers**

Add to `org-gtd-view-manager.el` just above the `;;;; Recall` section:

```elisp
(defun org-gtd-view-manager--annotate-view (name views)
  "Return the annotation string for view NAME within VIEWS.
The badge is dimmed with `completions-annotations' and prefixed by a
two-space gap so it reads as secondary text after the name."
  (let ((spec (cdr (assoc name views))))
    (concat "  " (propertize (org-gtd-view-manager--badge spec)
                             'face 'completions-annotations))))

(defun org-gtd-view-manager--completion-table (views)
  "Return a completion table over VIEWS' names annotated with their badges.
On `metadata' it advertises an `annotation-function' (the badge) and the
`org-gtd-view' category; otherwise it completes over the view names."
  (lambda (str pred action)
    (if (eq action 'metadata)
        `(metadata
          (annotation-function
           . ,(lambda (name)
                (org-gtd-view-manager--annotate-view name views)))
          (category . org-gtd-view))
      (complete-with-action action (mapcar #'car views) str pred))))

(defun org-gtd-view-manager--pick-view (views &optional prompt)
  "Read a saved-view name from VIEWS via an annotated `completing-read'.
PROMPT defaults to \"View: \"."
  (completing-read (or prompt "View: ")
                   (org-gtd-view-manager--completion-table views)
                   nil t))
```

**Step 4: Run the tests to verify they pass**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-pick-test.el`
Expected: PASS (4 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-pick-test.el
git commit -m "feat: annotated completing-read picker for saved views"
```

---

## Task 2: Reuse the picker in `org-gtd-view-run`

`org-gtd-view-run` (`:1261`) currently builds its own `completing-read` over
bare names. Refactor it to call `--pick-view` so recall also shows badges.
Behavior is otherwise unchanged.

**Files:**
- Modify: `org-gtd-view-manager.el:1261-1269`
- Test: `test/unit/view-manager-run-test.el` (add one test)

**Step 1: Write the failing test**

Append to `test/unit/view-manager-run-test.el` (before the `provide`):

```elisp
(deftest view-manager-run/uses-annotated-picker ()
  "Recall routes through the shared annotated picker helper."
  (org-gtd-view-manager--store-upsert
   "Errands" '((name . "Errands") (type . next-action)))
  (let (picked-views)
    (cl-letf (((symbol-function 'org-gtd-view-manager--pick-view)
               (lambda (views &rest _) (setq picked-views views) "Errands"))
              ((symbol-function 'org-gtd-view-show) #'ignore))
      (org-gtd-view-run))
    (assert-true (assoc "Errands" picked-views))))
```

**Step 2: Run to verify it fails**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-run-test.el`
Expected: FAIL — `--pick-view` is not called (old inline `completing-read`).

**Step 3: Refactor `org-gtd-view-run`**

Replace the body at `:1261-1269`:

```elisp
;;;###autoload
(defun org-gtd-view-run ()
  "Prompt for a saved view by name and render it via `org-gtd-view-show'."
  (interactive)
  (let ((views (org-gtd-view-manager--store-read)))
    (unless views
      (user-error "No saved views yet — build one with M-x org-gtd-view-manager"))
    (let* ((name (org-gtd-view-manager--pick-view views))
           (spec (cdr (assoc name views))))
      (org-gtd-view-show spec))))
```

**Step 4: Run to verify pass**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-run-test.el`
Expected: PASS (all 3 tests — the two originals still green).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-run-test.el
git commit -m "refactor: org-gtd-view-run reuses the annotated picker"
```

---

## Task 3: The `--selected` var and the `--act-*` dispatch functions

The action transient's suffixes, each reading the dynamic var `--selected`.

**Files:**
- Modify: `org-gtd-view-manager.el` (replace the `--list-*` block; add near the
  old list functions ~`:1147`)
- Test: `test/unit/view-manager-act-test.el` (create)

**Step 1: Write the failing tests**

Create `test/unit/view-manager-act-test.el`:

```elisp
;;; view-manager-act-test.el --- Tests for the per-view action dispatch -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the select-then-act dispatch (`--act-open/edit/new/copy/
;; delete') scoped to `org-gtd-view-manager--selected'.
;;
;;; Code:

(require 'cl-lib)
(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defun ogt--seed-view (name spec)
  (org-gtd-view-manager--store-upsert name spec)
  (setq org-gtd-view-manager--selected name))

(deftest view-manager-act/open-shows-selected-spec ()
  "Open renders the selected view's stored spec."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (let (captured)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (spec &rest _) (setq captured spec))))
      (org-gtd-view-manager--act-open))
    (assert-equal 'next-action (alist-get 'type captured))))

(deftest view-manager-act/edit-builds-selected-spec ()
  "Edit opens the builder on the selected spec."
  (ogt--seed-view "E" '((name . "E") (type . project)))
  (let (captured)
    (cl-letf (((symbol-function 'org-gtd-view-manager--build)
               (lambda (&optional spec &rest _) (setq captured (or spec 'none)))))
      (org-gtd-view-manager--act-edit))
    (assert-equal 'project (alist-get 'type captured))))

(deftest view-manager-act/new-builds-fresh-ignoring-selection ()
  "New opens the builder with NO starting spec, even with a selection."
  (ogt--seed-view "E" '((name . "E") (type . project)))
  (let ((captured 'unset))
    (cl-letf (((symbol-function 'org-gtd-view-manager--build)
               (lambda (&optional spec &rest _) (setq captured spec))))
      (org-gtd-view-manager--act-new))
    (assert-nil captured)))

(deftest view-manager-act/copy-builds-named-copy ()
  "Copy opens the builder on a `<name> copy' spec."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (let (captured)
    (cl-letf (((symbol-function 'org-gtd-view-manager--build)
               (lambda (&optional spec &rest _) (setq captured spec))))
      (org-gtd-view-manager--act-copy))
    (assert-equal "E copy" (alist-get 'name captured))))

(deftest view-manager-act/delete-removes-and-repicks ()
  "Delete (confirmed) removes the view, then re-enters the manager."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (org-gtd-view-manager--store-upsert "F" '((name . "F") (type . project)))
  (let (repicked)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'org-gtd-view-manager)
               (lambda (&rest _) (setq repicked t))))
      (org-gtd-view-manager--act-delete))
    (assert-nil (assoc "E" (org-gtd-view-manager--store-read)))
    (assert-true repicked)))

(deftest view-manager-act/delete-last-messages-no-repick ()
  "Deleting the only view messages cleanly and does NOT re-pick or build."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (let (repicked built)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'org-gtd-view-manager)
               (lambda (&rest _) (setq repicked t)))
              ((symbol-function 'org-gtd-view-manager--build)
               (lambda (&rest _) (setq built t))))
      (org-gtd-view-manager--act-delete))
    (assert-nil (assoc "E" (org-gtd-view-manager--store-read)))
    (assert-nil repicked)
    (assert-nil built)))

(deftest view-manager-act/delete-declined-keeps-view ()
  "Declining the confirm leaves the view in the store."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
    (org-gtd-view-manager--act-delete))
  (assert-true (assoc "E" (org-gtd-view-manager--store-read))))

(provide 'view-manager-act-test)
;;; view-manager-act-test.el ends here
```

**Step 2: Run to verify failure**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-act-test.el`
Expected: FAIL — `--selected` and the `--act-*` functions are void.

**Step 3: Implement the var + dispatch functions**

Add the defvar near the other manager state (replace the `--highlight` defvar
at `:1123`; `--highlight` is going away in Task 5):

```elisp
(defvar org-gtd-view-manager--selected nil
  "Name of the view the action transient is currently scoped to.")
```

Add the dispatch functions (these REPLACE `--list-render/edit/create/
duplicate/delete`):

```elisp
(defun org-gtd-view-manager--act-open ()
  "Render the selected view via `org-gtd-view-show'."
  (interactive)
  (org-gtd-view-show
   (org-gtd-view-manager--store-get org-gtd-view-manager--selected)))

(defun org-gtd-view-manager--act-edit ()
  "Open the builder on the selected view's stored spec."
  (interactive)
  (org-gtd-view-manager--build
   (org-gtd-view-manager--store-get org-gtd-view-manager--selected)))

(defun org-gtd-view-manager--act-new ()
  "Open the builder on a fresh spec, ignoring the current selection."
  (interactive)
  (org-gtd-view-manager--build))

(defun org-gtd-view-manager--act-copy ()
  "Open the builder on a copy of the selected view named \"<name> copy\".
The copy is NOT pre-persisted: `--build' seeds `--build-original-name'
to the copy name, so `--save' creates it on save and aborting leaves no
orphan behind."
  (interactive)
  (let* ((spec (org-gtd-view-manager--store-get org-gtd-view-manager--selected))
         (copy-name (concat (alist-get 'name spec) " copy"))
         (copy-spec (cons (cons 'name copy-name)
                          (assq-delete-all 'name (copy-alist spec)))))
    (org-gtd-view-manager--build copy-spec)))

(defun org-gtd-view-manager--act-delete ()
  "Delete the selected view after a `y/n' confirm.
If views remain, re-enter the manager (pick another); otherwise message
that none remain -- never pop the builder after deleting the last view."
  (interactive)
  (let ((name org-gtd-view-manager--selected))
    (when (y-or-n-p (format "Delete view '%s'? " name))
      (org-gtd-view-manager--store-delete name)
      (if (org-gtd-view-manager--store-read)
          (org-gtd-view-manager)
        (message "No saved views remain.")))))
```

**Step 4: Run to verify pass**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-act-test.el`
Expected: PASS (7 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-act-test.el
git commit -m "feat: per-view action dispatch scoped to a selected view"
```

---

## Task 4: The action transient

The `--act` transient prefix wiring the rekeyed suffixes `o/e/n/c/d/q` and a
`:description` showing the selected view.

**Files:**
- Modify: `org-gtd-view-manager.el` (add after the `--act-*` functions)
- Test: `test/unit/view-manager-act-test.el` (add transient-wiring tests)

**Step 1: Write the failing tests**

Append to `test/unit/view-manager-act-test.el` (before `provide`):

```elisp
(deftest view-manager-act/transient-exists ()
  "The action transient prefix is defined."
  (assert-true (fboundp 'org-gtd-view-manager--act)))

(deftest view-manager-act/transient-keys ()
  "The action transient binds o/e/n/c/d/q."
  (dolist (key '("o" "e" "n" "c" "d" "q"))
    (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager--act key)))
      (assert-equal key (plist-get plist :key)))))

(deftest view-manager-act/description-shows-selection ()
  "The transient description names the selected view and its badge."
  (org-gtd-view-manager--store-upsert
   "E" '((name . "E") (type . next-action) (area-of-focus . "Home")))
  (setq org-gtd-view-manager--selected "E")
  (let ((desc (org-gtd-view-manager--act-description)))
    (assert-true (string-match-p "E" desc))
    (assert-true (string-match-p "next-action · Home" desc))))
```

**Step 2: Run to verify failure**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-act-test.el`
Expected: FAIL — `--act` and `--act-description` are void.

**Step 3: Implement the description + transient**

```elisp
(defun org-gtd-view-manager--act-description ()
  "Return the action transient's heading: selected name + its badge."
  (let* ((name org-gtd-view-manager--selected)
         (spec (org-gtd-view-manager--store-get name)))
    (format "%s\n%s"
            (propertize (or name "") 'face 'transient-heading)
            (org-gtd-view-manager--badge spec))))

(transient-define-prefix org-gtd-view-manager--act ()
  "Act on the selected saved view."
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

**Step 4: Run to verify pass**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-act-test.el`
Expected: PASS (10 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-act-test.el
git commit -m "feat: action transient (o/e/n/c/d/q) for the selected view"
```

---

## Task 5: Rewrite the entry command; delete dead code

`org-gtd-view-manager` becomes a plain command (pick → act; empty → builder),
carrying the autoload cookie. Delete the old transient and all cursor/list/
reorder machinery.

**Files:**
- Modify: `org-gtd-view-manager.el` — replace `:1234-1256` (old transient) and
  delete `--rows` (`:1128-1145`), `--highlight` (already replaced in Task 3 by
  `--selected` — remove the old defvar if still present), `--list-up`,
  `--list-down`, `--list-highlighted-name`, `--list-highlighted-spec`,
  `--list-window-config`, `--list-quit`.
- Test: `test/unit/view-manager-list-test.el` (rewrite for the new entry)

**Step 1: Write the failing tests**

Replace the transient-wiring tests in `view-manager-list-test.el` (keep the
save/round-trip tests — they exercise `--save`, unaffected). Swap the header
comment and the first three `deftest`s for:

```elisp
(deftest view-manager-list/entry-command-exists ()
  "The manager entry point is a command."
  (assert-true (commandp 'org-gtd-view-manager)))

(deftest view-manager-list/empty-store-opens-builder ()
  "Invoking the manager with no saved views opens the builder directly."
  (let (built)
    (cl-letf (((symbol-function 'org-gtd-view-manager--migrate-once) #'ignore)
              ((symbol-function 'org-gtd-view-manager--build)
               (lambda (&rest _) (setq built t))))
      (org-gtd-view-manager))
    (assert-true built)))

(deftest view-manager-list/nonempty-picks-then-acts ()
  "With views present, the manager picks a name then opens the act transient."
  (org-gtd-view-manager--store-upsert
   "E" '((name . "E") (type . next-action)))
  (let (acted)
    (cl-letf (((symbol-function 'org-gtd-view-manager--migrate-once) #'ignore)
              ((symbol-function 'org-gtd-view-manager--pick-view)
               (lambda (&rest _) "E"))
              ((symbol-function 'org-gtd-view-manager--act)
               (lambda (&rest _) (setq acted t))))
      (org-gtd-view-manager))
    (assert-equal "E" org-gtd-view-manager--selected)
    (assert-true acted)))
```

Also delete `view-manager-list/has-create-key` and
`view-manager-list/has-delete-key` (those keys no longer exist on this prefix).

**Step 2: Run to verify failure**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el`
Expected: FAIL — `org-gtd-view-manager` is still the old transient (no
`--pick-view`/`--act` path; `commandp` may pass but the empty/pick tests fail).

**Step 3: Rewrite the entry command and delete dead code**

Replace the old `transient-define-prefix org-gtd-view-manager` (`:1234-1256`)
with:

```elisp
;;;###autoload
(defun org-gtd-view-manager ()
  "Browse and manage saved GTD views.
With no saved views, open the builder directly (nothing to manage).
Otherwise pick a view via an annotated `completing-read', then open the
action transient scoped to it."
  (interactive)
  (org-gtd-view-manager--migrate-once)
  (let ((views (org-gtd-view-manager--store-read)))
    (if (null views)
        (org-gtd-view-manager--build)
      (setq org-gtd-view-manager--selected
            (org-gtd-view-manager--pick-view views))
      (org-gtd-view-manager--act))))
```

Then DELETE (now dead):
- `--rows` (`:1128-1145`)
- the `--highlight` defvar (`:1123`) if not already replaced
- `--list-highlighted-name`, `--list-highlighted-spec`
- `--list-up`, `--list-down`
- `--list-window-config` defvar and `--list-quit`
- `--list-create`, `--list-edit`, `--list-render`, `--list-duplicate`,
  `--list-delete` (superseded by `--act-*` in Task 3 — confirm none remain)

Keep the `--migrate-once` KNOWN-LIMITATION comment by moving it above the new
command (the session-scoped migration behavior is unchanged).

**Step 4: Run to verify pass, then the whole view-manager suite**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el`
Expected: PASS.

Run each touched file, then confirm nothing else references the deleted
symbols:

```bash
grep -n "org-gtd-view-manager--\(rows\|highlight\|list-\)" org-gtd-view-manager.el test/
```

Expected: no matches outside the (now-updated) tests.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-list-test.el
git commit -m "feat: view manager entry becomes a completing-read picker

Drop the in-transient view list, cursor navigation, and reorder; the
manager now picks a view via an annotated completing-read then opens the
action transient. Empty store opens the builder directly."
```

---

## Task 6: Full-suite green, compile, lint, docs

**Files:**
- Modify: `doc/using-org-gtd.org` (if it documents the manager's list keys /
  reorder — update to the new picker + `o/e/n/c/d/q`), `CHANGELOG.org`
- Regenerate: `org-gtd.info` (SEPARATE commit — see the
  split-generated-text-into-own-commit memory)

**Step 1: Run the whole unit suite**

Run: `.claude/skills/test/run-tests.sh unit`
Expected: PASS. (Pre-existing unrelated flakes may appear:
`project-task-commands/cancel-from-graph-view` and `reactivate-test.el` "State
WAIT not valid" — confirm they fail independently of this change; do not treat
as regressions.)

**Step 2: Compile clean**

Run: `~/bin/eldev clean && ~/bin/eldev compile --warnings-as-errors`
Expected: no warnings/errors. (Watch for references to deleted functions and an
unused `--selected` — it IS used by the transient.)

**Step 3: Lint**

Run: `~/bin/eldev lint --file="org-gtd-view-manager.el"`
Expected: clean (checkdoc docstrings on every new defun/defvar — they are
provided above).

**Step 4: Update prose docs**

Grep the manual for stale references:

```bash
grep -rn "reorder\|▸\|Duplicate\|manager.*list" doc/using-org-gtd.org
```

Update any section describing the manager's list/cursor/reorder to the new
model: a `completing-read` picker (badges shown as annotations) → action
transient (`o` Open, `e` Edit, `n` New, `c` Copy, `d` Delete, `q` Quit).
Add a CHANGELOG.org entry under the unreleased heading.

**Step 5: Commit (source + prose), then info separately**

```bash
git add org-gtd-view-manager.el doc/using-org-gtd.org CHANGELOG.org
git commit -m "docs: document the completing-read view manager"
# regenerate the info manual, then:
git add org-gtd.info
git commit -m "docs: regenerate org-gtd.info"
```

---

## Final review

After all tasks: dispatch a code-reviewer over the whole diff
(`git diff master...HEAD -- org-gtd-view-manager.el`), then use
superpowers:finishing-a-development-branch. Do NOT push master, force-push, or
merge — the branch-landing decision remains the user's.
