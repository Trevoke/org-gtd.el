# Project-Heading Filter Inheritance

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the view DSL's tags, area-of-focus, and priority filters inherit values from parent project headings, so project tasks match filters set on their project.

**Architecture:** Add a reusable helper `org-gtd-pred--any-project-satisfies` to `org-gtd-skip.el` that looks up parent projects via `ORG_GTD_PROJECT_IDS` (same pattern as the existing `org-gtd-pred--task-has-active-project`). Modify the three specific predicates to use this helper as a fallback when the item itself doesn't match. The item's own property always takes precedence.

**Tech Stack:** Emacs Lisp, org-mode, e-unit test framework, mock-fs virtual filesystem

---

## Context for Implementer

### How project tasks link to projects

Project tasks have an `ORG_GTD_PROJECT_IDS` property containing space-separated org IDs of their parent project(s). Use `(org-entry-get-multivalued-property (point) org-gtd-prop-project-ids)` to read them, and `(org-id-find pid 'marker)` to resolve each ID to a buffer position.

### Prior art

- **Display inheritance:** `org-gtd-agenda.el:71-89` already resolves CATEGORY from parent projects for prefix display using this exact pattern.
- **Active-project filter:** `org-gtd-skip.el:379-398` iterates `ORG_GTD_PROJECT_IDS` to check project status. This is the closest model for the new helper.

### Predicate pattern

Predicates in `org-gtd-skip.el` are factory functions returning closures. The closure is called with point at the org entry being filtered. Returns `t` = include, `nil` = skip.

### Test infrastructure

- Unit tests use `with-temp-buffer` + `org-mode` + inline org content (no mock-fs needed for predicate tests).
- `make-project` and `make-task` builders in `test/helpers/builders.el` create project structures with `:tags`, `:properties`, `:project-ids` options.
- Run tests: use `/test` skill (never run eldev directly).

### Constants

- `org-gtd-prop-project-ids` = `"ORG_GTD_PROJECT_IDS"` (defined in `org-gtd-core.el:156`)
- `org-gtd-prop-area-of-focus` = `"CATEGORY"` (defined in `org-gtd-core.el:141`)

---

## Task 1: Add `org-gtd-pred--any-project-satisfies` helper

**Files:**
- Modify: `org-gtd-skip.el` (after line 398, after `org-gtd-pred--task-has-active-project`)
- Test: `test/unit/skip-predicates-test.el`

**Step 1: Write the failing test**

Add to `test/unit/skip-predicates-test.el`:

```elisp
;;;; Project inheritance helper tests

(deftest skip-pred/any-project-satisfies-finds-matching-project ()
  "Helper returns non-nil when a parent project satisfies the predicate."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "Build app"
                          :tags '("@work")
                          :tasks '("Write code"))))
      (goto-char (plist-get (car (plist-get project-info :task-markers)) 'marker))
      ;; Point is now at project task "Write code"
      (org-back-to-heading t)
      (assert-true
       (org-gtd-pred--any-project-satisfies
        (lambda () (member "@work" (org-get-tags nil t))))))))

(deftest skip-pred/any-project-satisfies-nil-when-no-match ()
  "Helper returns nil when no parent project satisfies the predicate."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "Build app"
                          :tasks '("Write code"))))
      (goto-char (plist-get (car (plist-get project-info :task-markers)) 'marker))
      (org-back-to-heading t)
      (assert-nil
       (org-gtd-pred--any-project-satisfies
        (lambda () (member "@work" (org-get-tags nil t))))))))

(deftest skip-pred/any-project-satisfies-nil-for-standalone-task ()
  "Helper returns nil for tasks without ORG_GTD_PROJECT_IDS."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Standalone task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (assert-nil
     (org-gtd-pred--any-project-satisfies
      (lambda () t)))))
```

**Important:** The `make-project` builder auto-generates IDs and sets `ORG_GTD_PROJECT_IDS` on child tasks. Check that the task markers from the builder are actual markers (they may be point-markers). Navigate to them with `goto-char`. If the builder returns markers differently than shown, adapt — read `test/helpers/builders.el:152-250` to understand the return format.

**Step 2: Run tests to verify they fail**

Run: `/test test/unit/skip-predicates-test.el`
Expected: FAIL — `org-gtd-pred--any-project-satisfies` is not defined

**Step 3: Write the implementation**

Add to `org-gtd-skip.el` after line 398 (after `org-gtd-pred--task-has-active-project`):

```elisp
(defun org-gtd-pred--any-project-satisfies (pred-fn)
  "Return non-nil if PRED-FN returns non-nil at any parent project heading.
PRED-FN is a zero-arg function called with point at each project heading.
Looks up parent projects via ORG_GTD_PROJECT_IDS property.
Returns nil for standalone items (no ORG_GTD_PROJECT_IDS)."
  (let ((project-ids (org-entry-get-multivalued-property
                      (point) org-gtd-prop-project-ids)))
    (cl-some (lambda (pid)
               (when-let ((marker (org-id-find pid 'marker)))
                 (org-with-point-at marker
                   (funcall pred-fn))))
             project-ids)))
```

**Step 4: Run tests to verify they pass**

Run: `/test test/unit/skip-predicates-test.el`
Expected: PASS

**Step 5: Commit**

```
feat: add org-gtd-pred--any-project-satisfies helper

Generic predicate helper that checks if any parent project heading
satisfies a given predicate function. Looks up projects via
ORG_GTD_PROJECT_IDS, following the same pattern as the existing
active-project filter.
```

---

## Task 2: Add project inheritance to tag filtering

**Files:**
- Modify: `org-gtd-skip.el:155-161` (`org-gtd-pred--tags-matches`)
- Test: `test/unit/tags-filter-test.el`

**Step 1: Write the failing tests**

Add to `test/unit/tags-filter-test.el` before the `(provide)` line:

```elisp
;;;; Project Tag Inheritance

(deftest tags-pred/inherits-tag-from-project-heading ()
  "Predicate matches when parent project has the tag but task does not."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "Build app"
                          :tags '("@work")
                          :tasks '("Write code"))))
      (goto-char (car (plist-get project-info :task-markers)))
      (org-back-to-heading t)
      (let ((pred (org-gtd-pred--tags-matches '("@work"))))
        (assert-true (funcall pred))))))

(deftest tags-pred/task-own-tag-takes-precedence ()
  "Task with its own tag matches without needing project lookup."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "Build app"
                          :tags '("@home")
                          :tasks '((:description "Write code" :tags ("@work"))))))
      (goto-char (car (plist-get project-info :task-markers)))
      (org-back-to-heading t)
      (let ((pred (org-gtd-pred--tags-matches '("@work"))))
        (assert-true (funcall pred))))))

(deftest tags-pred/no-inheritance-for-standalone-task ()
  "Standalone task without project does not inherit anything."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work"))))
      (assert-nil (funcall pred)))))

(deftest tags-pred/inherits-from-any-project-in-multi-project ()
  "Task matches if ANY parent project has the tag."
  (with-temp-buffer
    (org-mode)
    (let* ((proj-a (make-project "Project A" :tags '("@work")))
           (proj-b (make-project "Project B" :tags '("@home")))
           (proj-a-id (plist-get proj-a :id))
           (proj-b-id (plist-get proj-b :id)))
      (make-task "Shared task" :project-ids (list proj-a-id proj-b-id))
      ;; Go to the task we just created
      (search-backward "Shared task")
      (org-back-to-heading t)
      (let ((pred (org-gtd-pred--tags-matches '("@home"))))
        (assert-true (funcall pred))))))
```

**Important:** The `make-task` builder's `:tags` param takes a list of strings. The `make-project` builder's `:tasks` param accepts either strings or plists — use plists to specify task tags. Check the builder return format for `:task-markers` — it may be a list of markers directly or wrapped in plists. Read `test/helpers/builders.el:66-248` to confirm.

**Step 2: Run tests to verify they fail**

Run: `/test test/unit/tags-filter-test.el`
Expected: FAIL — inheritance tests fail (predicate doesn't check projects yet)

**Step 3: Modify the implementation**

Replace `org-gtd-pred--tags-matches` in `org-gtd-skip.el:155-161`:

```elisp
(defun org-gtd-pred--tags-matches (tags)
  "Return predicate checking if item has any of TAGS.
TAGS is a list of tag strings (e.g., (\"@work\" \"@home\")).
Uses OR semantics: returns t if entry has ANY of the specified tags.
Falls back to checking parent project headings via ORG_GTD_PROJECT_IDS."
  (lambda ()
    (let ((entry-tags (org-get-tags nil t)))  ; nil=current, t=local only
      (or (cl-some (lambda (tag) (member tag entry-tags)) tags)
          (org-gtd-pred--any-project-satisfies
           (lambda ()
             (let ((proj-tags (org-get-tags nil t)))
               (cl-some (lambda (tag) (member tag proj-tags)) tags))))))))
```

**Step 4: Run tests to verify they pass**

Run: `/test test/unit/tags-filter-test.el`
Expected: PASS (all existing + new tests)

**Step 5: Commit**

```
feat: inherit tags from project headings in view DSL filters

When filtering by tags, project tasks now inherit tags from their
parent project heading(s) via ORG_GTD_PROJECT_IDS. The task's own
tags take precedence. For multi-project tasks, matching ANY parent
project's tags is sufficient.
```

---

## Task 3: Add project inheritance to area-of-focus filtering

**Files:**
- Create: `test/unit/area-of-focus-filter-test.el`
- Modify: `org-gtd-skip.el` (add new predicate after `org-gtd-pred--tags-matches`)
- Modify: `org-gtd-view-language.el:797-798` (swap predicate)

**Step 1: Write the failing tests**

Create `test/unit/area-of-focus-filter-test.el`:

```elisp
;;; area-of-focus-filter-test.el --- Tests for area-of-focus filter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for area-of-focus predicate with project inheritance.

;;; Code:

(require 'e-unit)
(require 'org)

(e-unit-initialize)

;;;; Direct matching

(deftest aof-pred/matches-direct-category ()
  "Predicate matches when item has CATEGORY set directly."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:CATEGORY: Work\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
      (assert-true (funcall pred)))))

(deftest aof-pred/no-match-different-category ()
  "Predicate returns nil when CATEGORY differs."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:CATEGORY: Health\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
      (assert-nil (funcall pred)))))

;;;; Project inheritance

(deftest aof-pred/inherits-category-from-project ()
  "Predicate matches when parent project has CATEGORY but task does not."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "Build app"
                          :properties '(("CATEGORY" . "Work"))
                          :tasks '("Write code"))))
      (goto-char (car (plist-get project-info :task-markers)))
      (org-back-to-heading t)
      (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
        (assert-true (funcall pred))))))

(deftest aof-pred/task-category-wins-over-project ()
  "Task's own CATEGORY takes precedence over project's."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "Build app"
                          :properties '(("CATEGORY" . "Work"))
                          :tasks '((:description "Write code"
                                    :properties (("CATEGORY" . "Health")))))))
      (goto-char (car (plist-get project-info :task-markers)))
      (org-back-to-heading t)
      (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
        (assert-nil (funcall pred))))))

(deftest aof-pred/standalone-task-no-inheritance ()
  "Standalone task without project has no inheritance."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
      (assert-nil (funcall pred)))))

(provide 'area-of-focus-filter-test)
;;; area-of-focus-filter-test.el ends here
```

**Step 2: Run tests to verify they fail**

Run: `/test test/unit/area-of-focus-filter-test.el`
Expected: FAIL — `org-gtd-pred--area-of-focus-matches` is not defined

**Step 3: Write the predicate**

Add to `org-gtd-skip.el` after `org-gtd-pred--tags-matches` (after line 161):

```elisp
(defun org-gtd-pred--area-of-focus-matches (value)
  "Return predicate checking if item's area of focus matches VALUE.
Checks item's own CATEGORY property first.  If not set, falls back to
parent project's CATEGORY via ORG_GTD_PROJECT_IDS."
  (lambda ()
    (let ((aof (or (org-entry-get (point) org-gtd-prop-area-of-focus)
                   (org-gtd-pred--any-project-satisfies
                    (lambda ()
                      (org-entry-get (point) org-gtd-prop-area-of-focus))))))
      (equal aof value))))
```

**Step 4: Swap the predicate in the view language**

In `org-gtd-view-language.el:796-798`, change:

```elisp
        ;; Add area-of-focus predicate (uses CATEGORY property)
        (when area-filter
          (push (org-gtd-pred--property-equals org-gtd-prop-area-of-focus area-filter) predicates))
```

To:

```elisp
        ;; Add area-of-focus predicate (inherits from parent project)
        (when area-filter
          (push (org-gtd-pred--area-of-focus-matches area-filter) predicates))
```

**Step 5: Run tests to verify they pass**

Run: `/test test/unit/area-of-focus-filter-test.el`
Expected: PASS

Then run the full suite to check for regressions:

Run: `/test`
Expected: PASS (all tests)

**Step 6: Commit**

```
feat: inherit area-of-focus from project headings in filters

New org-gtd-pred--area-of-focus-matches predicate checks the item's
own CATEGORY first, then falls back to parent project CATEGORY via
ORG_GTD_PROJECT_IDS. This mirrors the existing display-level
inheritance in org-gtd-agenda--resolve-area-of-focus but extends
it to filtering.
```

---

## Task 4: Add project inheritance to priority filtering

**Files:**
- Modify: `org-gtd-skip.el:109-151` (`org-gtd-pred--priority-matches`)
- Test: `test/unit/skip-predicates-test.el`

**Step 1: Write the failing tests**

Add to `test/unit/skip-predicates-test.el`:

```elisp
;;;; Priority inheritance tests

(deftest skip-pred/priority-inherits-from-project ()
  "Priority predicate matches when project has priority but task does not."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "[#A] Important project"
                          :tasks '("Do something"))))
      (goto-char (car (plist-get project-info :task-markers)))
      (org-back-to-heading t)
      (let ((pred (org-gtd-pred--priority-matches 'A)))
        (assert-true (funcall pred))))))

(deftest skip-pred/priority-task-own-wins ()
  "Task's own priority takes precedence over project's."
  (with-temp-buffer
    (org-mode)
    ;; Project has [#A], but task has [#C]
    (insert "* [#A] Important project\n:PROPERTIES:\n:ID: proj-1\n:ORG_GTD: Projects\n:END:\n")
    (insert "** TODO [#C] Low prio task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: proj-1\n:END:\n")
    (goto-char (point-min))
    (search-forward "Low prio task")
    (org-back-to-heading t)
    (let ((pred (org-gtd-pred--priority-matches 'A)))
      (assert-nil (funcall pred)))))

(deftest skip-pred/priority-standalone-no-inheritance ()
  "Standalone task does not inherit priority."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task without priority\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--priority-matches 'A)))
      (assert-nil (funcall pred)))))
```

**Important:** Priority is parsed from the heading text `[#A]`, not from a property. The `make-project` builder passes the name directly to the heading. To get `[#A]` in the heading, include it in the project name string. Verify this works by checking that `make-project` outputs the heading correctly — if it doesn't, use raw `insert` instead.

**Step 2: Run tests to verify they fail**

Run: `/test test/unit/skip-predicates-test.el`
Expected: FAIL — priority predicate doesn't check projects yet

**Step 3: Modify the implementation**

The key change: when `item-priority` is nil AND `value` is non-nil (looking for a specific priority), check parent projects as fallback.

In `org-gtd-pred--priority-matches` at `org-gtd-skip.el:109-151`, the current logic:
1. Extracts `item-priority` from the heading `[#X]` cookie
2. Matches against `value`

Add a project fallback. Replace the function body. The tricky part is extracting priority from a project heading at a marker. Reuse the same heading-parse logic.

```elisp
(defun org-gtd-pred--priority-matches (value)
  "Return predicate checking if item priority matches VALUE.
VALUE can be:
  - A symbol/string like A (single priority)
  - A list like (A B) (any of these priorities)
  - A comparison like (>= B)
  - nil (no priority set)
Falls back to parent project priority via ORG_GTD_PROJECT_IDS
when the item has no priority set."
  (lambda ()
    (let ((item-priority (org-gtd-pred--extract-priority)))
      (or (org-gtd-pred--priority-value-matches item-priority value)
          ;; Inherit from project if task has no priority and we're
          ;; looking for a specific priority (not nil)
          (when (and (null item-priority) value)
            (org-gtd-pred--any-project-satisfies
             (lambda ()
               (org-gtd-pred--priority-value-matches
                (org-gtd-pred--extract-priority) value))))))))
```

Extract two helper functions from the existing code:

```elisp
(defun org-gtd-pred--extract-priority ()
  "Extract priority letter from heading at point, or nil."
  (save-excursion
    (org-back-to-heading t)
    (when (looking-at org-complex-heading-regexp)
      (let ((cookie (match-string-no-properties 3)))
        (when (and cookie (string-match "\\[#\\(.\\)\\]" cookie))
          (match-string 1 cookie))))))

(defun org-gtd-pred--priority-value-matches (item-priority value)
  "Check if ITEM-PRIORITY matches VALUE.
VALUE can be nil, a symbol/string, a list, or a comparison."
  (cond
   ((null value)
    (or (null item-priority)
        (string-empty-p item-priority)))
   ((and (listp value) (memq (car value) '(< > <= >=)))
    (when item-priority
      (let* ((op (car value))
             (ref (cadr value))
             (highest (or org-priority-highest ?A))
             (item-num (1+ (- (aref item-priority 0) highest)))
             (ref-num (1+ (- (aref (symbol-name ref) 0) highest))))
        (pcase op
          ('< (< item-num ref-num))
          ('> (> item-num ref-num))
          ('<= (<= item-num ref-num))
          ('>= (<= item-num ref-num))))))
   ((listp value)
    (when item-priority
      (member item-priority
              (mapcar (lambda (p) (if (symbolp p) (symbol-name p) p)) value))))
   (t
    (when item-priority
      (equal item-priority
             (if (symbolp value) (symbol-name value) value))))))
```

Place the two helpers BEFORE `org-gtd-pred--priority-matches` (before line 109), then replace the main function.

**Step 4: Run tests to verify they pass**

Run: `/test test/unit/skip-predicates-test.el`
Expected: PASS

Then full suite:

Run: `/test`
Expected: PASS

**Step 5: Commit**

```
feat: inherit priority from project headings in filters

When filtering by priority, project tasks without their own priority
now inherit from their parent project heading. Task's own priority
always takes precedence. Refactored priority matching into extract
and match helpers to enable reuse at project heading locations.
```

---

## Task 5: Full integration verification

**Files:**
- Test: `test/unit/gtd-view-language-test.el` (add integration tests)

**Step 1: Write integration tests**

Add integration tests that verify the full path: DSL spec → skip function → correct filtering. Add to `test/unit/gtd-view-language-test.el` (find the right location near existing filter tests):

```elisp
;;;; Project Inheritance Integration

(deftest view-lang/tags-filter-inherits-from-project ()
  "View DSL tags filter includes project tasks via project tag inheritance."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "Build app"
                          :tags '("@work")
                          :tasks '("Write code"))))
      (goto-char (car (plist-get project-info :task-markers)))
      (org-back-to-heading t)
      (let* ((spec '((type . next-action) (tags . ("@work"))))
             (skip-fn (org-gtd-view-lang--build-skip-function spec)))
        ;; Task should NOT be skipped (included via project inheritance)
        (assert-nil (funcall skip-fn))))))

(deftest view-lang/area-of-focus-filter-inherits-from-project ()
  "View DSL area-of-focus filter includes project tasks via inheritance."
  (with-temp-buffer
    (org-mode)
    (let ((project-info (make-project "Build app"
                          :properties '(("CATEGORY" . "Work"))
                          :tasks '("Write code"))))
      (goto-char (car (plist-get project-info :task-markers)))
      (org-back-to-heading t)
      (let* ((spec '((type . next-action) (area-of-focus . "Work")))
             (skip-fn (org-gtd-view-lang--build-skip-function spec)))
        (assert-nil (funcall skip-fn))))))
```

**Step 2: Run tests**

Run: `/test test/unit/gtd-view-language-test.el`
Expected: PASS

Then full suite:

Run: `/test`
Expected: PASS (all tests, no regressions)

**Step 3: Commit**

```
test: add integration tests for project filter inheritance
```

---

Plan complete and saved to `docs/plans/2026-03-03-project-heading-filter-inheritance.md`. Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?