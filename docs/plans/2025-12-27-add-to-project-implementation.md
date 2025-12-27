# Add to Project Flow Redesign - Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task.

**Goal:** Fix the "add to project" organize flow to set `ORG_GTD_PROJECT_IDS` explicitly rather than deriving it from refile hierarchy.

**Architecture:** Project selection happens first, then properties are set explicitly on the task, then refile respects user settings. This matches the graph view's cleaner approach.

**Tech Stack:** Emacs Lisp, org-mode, e-unit testing framework

**Bead:** TBD (create when starting implementation)

---

## Task 1: Create project-extend unit test file

**Files:**
- Create: `test/unit/project-extend-test.el`

**Step 1: Create test file with basic structure**

```elisp
;;; project-extend-test.el --- Tests for add-to-project flow -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the add-to-project (project-extend) flow.
;; Tests the new flow: select project first, set ORG_GTD_PROJECT_IDS explicitly.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-projects)

;; Initialize e-unit short syntax
(e-unit-initialize)

(provide 'project-extend-test)
;;; project-extend-test.el ends here
```

**Step 2: Run test to verify file loads**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: 0 tests, no errors

**Step 3: Commit**

```bash
git add test/unit/project-extend-test.el
git commit -m "test: create project-extend test file"
```

---

## Task 2: Add project selection helper with test

**Files:**
- Modify: `test/unit/project-extend-test.el`
- Modify: `org-gtd-projects.el`

**Step 1: Write the failing test**

Add to `test/unit/project-extend-test.el`:

```elisp
(deftest project-extend/select-project-returns-id-and-marker ()
  "Selection helper returns cons of (project-id . project-marker)."
  (with-temp-buffer
    (org-mode)
    (insert "* Test Project\n:PROPERTIES:\n:ORG_GTD: Projects\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((project-id (org-id-get-create))
           (expected-marker (point-marker)))
      ;; Mock completing-read to return "Test Project"
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Test Project"))
                ((symbol-function 'org-map-entries)
                 (lambda (fn match scope)
                   (when (string= match "+ORG_GTD=\"Projects\"")
                     (save-excursion
                       (goto-char (point-min))
                       (org-next-visible-heading 1)
                       (funcall fn))))))
        (let ((result (org-gtd-project-extend--select-project)))
          (assert-equal project-id (car result))
          (assert-true (markerp (cdr result))))))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: FAIL with "Symbol's function definition is void: org-gtd-project-extend--select-project"

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` after line 149 (after `org-gtd-project-extend`):

```elisp
(defun org-gtd-project-extend--select-project ()
  "Prompt user to select a project.
Returns cons of (project-id . project-marker)."
  (let ((projects '()))
    ;; Collect all projects
    (org-map-entries
     (lambda ()
       (let ((id (org-id-get-create))
             (name (org-get-heading t t t t)))
         (push (cons name id) projects)))
     "+ORG_GTD=\"Projects\""
     'agenda)
    ;; Prompt for selection
    (let* ((project-names (mapcar #'car projects))
           (chosen-name (completing-read "Add to project: " project-names nil t))
           (project-id (cdr (assoc chosen-name projects)))
           (project-marker (org-id-find project-id t)))
      (cons project-id project-marker))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add test/unit/project-extend-test.el org-gtd-projects.el
git commit -m "feat: add project selection helper for add-to-project flow"
```

---

## Task 3: Add move-to-project helper with test

**Files:**
- Modify: `test/unit/project-extend-test.el`
- Modify: `org-gtd-projects.el`

**Step 1: Write the failing test**

Add to `test/unit/project-extend-test.el`:

```elisp
(deftest project-extend/move-to-project-refiles-under-project ()
  "Move helper refiles task under project heading."
  (with-temp-buffer
    (org-mode)
    (insert "* Project A\n:PROPERTIES:\n:ORG_GTD: Projects\n:END:\n")
    (insert "* Task to move\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((project-marker (point-marker)))
      ;; Go to task
      (org-next-visible-heading 1)
      (assert-equal "Task to move" (org-get-heading t t t t))
      ;; Move it
      (org-gtd-project-extend--move-to-project project-marker)
      ;; Verify task is now under project
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (assert-equal "Project A" (org-get-heading t t t t))
      (org-next-visible-heading 1)
      (assert-equal "Task to move" (org-get-heading t t t t))
      (assert-equal 2 (org-current-level)))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: FAIL with "Symbol's function definition is void: org-gtd-project-extend--move-to-project"

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` after the select-project function:

```elisp
(defun org-gtd-project-extend--move-to-project (project-marker)
  "Move current heading under PROJECT-MARKER using programmatic refile."
  (let* ((project-file (buffer-file-name (marker-buffer project-marker)))
         (project-pos (marker-position project-marker))
         (rfloc (list nil project-file nil project-pos)))
    (org-refile nil nil rfloc)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add test/unit/project-extend-test.el org-gtd-projects.el
git commit -m "feat: add move-to-project helper for programmatic refile"
```

---

## Task 4: Add refile-with-user-targets helper

**Files:**
- Modify: `test/unit/refiling-test.el`
- Modify: `org-gtd-refile.el`

**Step 1: Write the failing test**

Add to `test/unit/refiling-test.el`:

```elisp
(deftest refile/with-user-targets-uses-only-user-config ()
  "Refile with user targets does not merge org-gtd targets."
  ;; This test verifies the function exists and uses correct settings
  (let ((org-refile-use-outline-path nil)
        (org-outline-path-complete-in-steps t)
        (refile-called nil)
        (captured-use-outline-path nil)
        (captured-complete-in-steps nil))
    (cl-letf (((symbol-function 'org-refile)
               (lambda (&rest _)
                 (setq refile-called t
                       captured-use-outline-path org-refile-use-outline-path
                       captured-complete-in-steps org-outline-path-complete-in-steps))))
      (org-gtd-refile--with-user-targets)
      (assert-true refile-called)
      (assert-true captured-use-outline-path)
      (assert-nil captured-complete-in-steps))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev -p -dtT etest test/unit/refiling-test.el`
Expected: FAIL with "Symbol's function definition is void: org-gtd-refile--with-user-targets"

**Step 3: Write minimal implementation**

Add to `org-gtd-refile.el` after line 161 (after `org-gtd-refile--do-project-task`):

```elisp
(defun org-gtd-refile--with-user-targets ()
  "Refile current heading using user's org-refile-targets.
Does not merge with org-gtd targets—uses only what user configured."
  (let ((org-refile-use-outline-path t)
        (org-outline-path-complete-in-steps nil))
    (org-refile)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev -p -dtT etest test/unit/refiling-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add test/unit/refiling-test.el org-gtd-refile.el
git commit -m "feat: add refile helper that uses only user targets"
```

---

## Task 5: Rewrite org-gtd-project-extend--apply

**Files:**
- Modify: `test/unit/project-extend-test.el`
- Modify: `org-gtd-projects.el`

**Step 1: Write test for new flow - ORG_GTD_PROJECT_IDS set before refile**

Add to `test/unit/project-extend-test.el`:

```elisp
(deftest project-extend/apply-sets-project-ids-before-refile ()
  "The new flow sets ORG_GTD_PROJECT_IDS explicitly before any refile."
  (let ((project-ids-set-at nil)
        (refile-called-at nil)
        (call-order 0))
    ;; Track when ORG_GTD_PROJECT_IDS is set vs when refile happens
    (cl-letf (((symbol-function 'org-gtd-project-extend--select-project)
               (lambda ()
                 (cons "fake-project-id" (point-marker))))
              ((symbol-function 'org-gtd-project--configure-single-task)
               #'ignore)
              ((symbol-function 'org-entry-put)
               (lambda (pom prop val)
                 (when (string= prop "ORG_GTD_PROJECT_IDS")
                   (setq project-ids-set-at (cl-incf call-order)))))
              ((symbol-function 'org-entry-add-to-multivalued-property)
               #'ignore)
              ((symbol-function 'org-gtd-project-extend--move-to-project)
               (lambda (_)
                 (setq refile-called-at (cl-incf call-order))))
              ((symbol-function 'org-gtd-projects-fix-todo-keywords)
               #'ignore)
              ((symbol-function 'org-id-get-create)
               (lambda () "fake-task-id"))
              ((symbol-function 'org-with-point-at)
               (lambda (marker &rest body) nil))
              (org-gtd-clarify--skip-refile nil)
              (org-gtd-refile-prompt-for-types nil))
      (with-temp-buffer
        (org-mode)
        (insert "* Task\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd-project-extend--apply)))
    ;; ORG_GTD_PROJECT_IDS must be set BEFORE refile
    (assert-true project-ids-set-at)
    (assert-true refile-called-at)
    (assert-true (< project-ids-set-at refile-called-at))))
```

**Step 2: Run test to verify current implementation fails**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: FAIL (current flow sets PROJECT_IDS after refile)

**Step 3: Rewrite the function**

Replace `org-gtd-project-extend--apply` in `org-gtd-projects.el` (around line 1026):

```elisp
(defun org-gtd-project-extend--apply ()
  "Add current task to an existing project.

Flow:
1. Prompt for project selection
2. Configure task for project membership
3. Set ORG_GTD_PROJECT_IDS explicitly
4. Add to project's FIRST_TASKS
5. Refile based on settings:
   - skip-refile: leave in place
   - project-task in refile-prompt-for-types: prompt with user targets
   - otherwise: move under project heading
6. Fix TODO keywords"

  ;; Step 1: Select project FIRST
  (let* ((selection (org-gtd-project-extend--select-project))
         (project-id (car selection))
         (project-marker (cdr selection))
         (task-id (org-id-get-create)))

    ;; Step 2: Configure task
    (org-gtd-project--configure-single-task)

    ;; Step 3: Set project membership explicitly
    (org-entry-put (point) org-gtd-prop-project-ids project-id)

    ;; Step 4: Add to FIRST_TASKS (no dependencies when added this way)
    (org-with-point-at project-marker
      (org-entry-add-to-multivalued-property
       (point) "ORG_GTD_FIRST_TASKS" task-id))

    ;; Step 5: Handle refile
    (unless org-gtd-clarify--skip-refile
      (if (memq 'project-task org-gtd-refile-prompt-for-types)
          (org-gtd-refile--with-user-targets)
        (org-gtd-project-extend--move-to-project project-marker)))

    ;; Step 6: Fix keywords
    (org-gtd-projects-fix-todo-keywords project-marker)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: PASS

**Step 5: Run full test suite to check for regressions**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests pass

**Step 6: Commit**

```bash
git add test/unit/project-extend-test.el org-gtd-projects.el
git commit -m "fix: rewrite add-to-project to set ORG_GTD_PROJECT_IDS explicitly"
```

---

## Task 6: Add test for skip-refile behavior

**Files:**
- Modify: `test/unit/project-extend-test.el`

**Step 1: Write the test**

Add to `test/unit/project-extend-test.el`:

```elisp
(deftest project-extend/apply-respects-skip-refile ()
  "When skip-refile is set, task stays in place with correct properties."
  (let ((refile-called nil))
    (cl-letf (((symbol-function 'org-gtd-project-extend--select-project)
               (lambda ()
                 (cons "fake-project-id" (point-marker))))
              ((symbol-function 'org-gtd-project--configure-single-task)
               #'ignore)
              ((symbol-function 'org-entry-put)
               #'ignore)
              ((symbol-function 'org-entry-add-to-multivalued-property)
               #'ignore)
              ((symbol-function 'org-gtd-project-extend--move-to-project)
               (lambda (_) (setq refile-called t)))
              ((symbol-function 'org-gtd-refile--with-user-targets)
               (lambda () (setq refile-called t)))
              ((symbol-function 'org-gtd-projects-fix-todo-keywords)
               #'ignore)
              ((symbol-function 'org-id-get-create)
               (lambda () "fake-task-id"))
              ((symbol-function 'org-with-point-at)
               (lambda (marker &rest body) nil))
              (org-gtd-clarify--skip-refile t))  ; Skip refile enabled
      (with-temp-buffer
        (org-mode)
        (insert "* Task\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd-project-extend--apply)))
    ;; Refile should NOT have been called
    (assert-nil refile-called)))
```

**Step 2: Run test to verify it passes**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: PASS (function already implements this)

**Step 3: Commit**

```bash
git add test/unit/project-extend-test.el
git commit -m "test: add test for skip-refile behavior in add-to-project"
```

---

## Task 7: Add test for refile-prompt-for-types behavior

**Files:**
- Modify: `test/unit/project-extend-test.el`

**Step 1: Write the test**

Add to `test/unit/project-extend-test.el`:

```elisp
(deftest project-extend/apply-uses-user-targets-when-configured ()
  "When project-task in refile-prompt-for-types, uses user's targets."
  (let ((user-refile-called nil)
        (auto-refile-called nil))
    (cl-letf (((symbol-function 'org-gtd-project-extend--select-project)
               (lambda ()
                 (cons "fake-project-id" (point-marker))))
              ((symbol-function 'org-gtd-project--configure-single-task)
               #'ignore)
              ((symbol-function 'org-entry-put)
               #'ignore)
              ((symbol-function 'org-entry-add-to-multivalued-property)
               #'ignore)
              ((symbol-function 'org-gtd-project-extend--move-to-project)
               (lambda (_) (setq auto-refile-called t)))
              ((symbol-function 'org-gtd-refile--with-user-targets)
               (lambda () (setq user-refile-called t)))
              ((symbol-function 'org-gtd-projects-fix-todo-keywords)
               #'ignore)
              ((symbol-function 'org-id-get-create)
               (lambda () "fake-task-id"))
              ((symbol-function 'org-with-point-at)
               (lambda (marker &rest body) nil))
              (org-gtd-clarify--skip-refile nil)
              (org-gtd-refile-prompt-for-types '(project-task)))  ; Include project-task
      (with-temp-buffer
        (org-mode)
        (insert "* Task\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd-project-extend--apply)))
    ;; User refile should have been called, not auto
    (assert-true user-refile-called)
    (assert-nil auto-refile-called)))
```

**Step 2: Run test to verify it passes**

Run: `~/bin/eldev -p -dtT etest test/unit/project-extend-test.el`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/project-extend-test.el
git commit -m "test: add test for refile-prompt-for-types behavior"
```

---

## Task 8: Clean up dead code

**Files:**
- Modify: `org-gtd-projects.el`

**Step 1: Review org-gtd-project--update-after-task-addition**

Read the function and its usages. After the rewrite, this function is no longer called by `org-gtd-project-extend--apply`.

**Step 2: Check if function is used elsewhere**

Run: `grep -r "org-gtd-project--update-after-task-addition" --include="*.el"`

If only used in the old flow, mark for deletion or simplification.

**Step 3: Remove or simplify dead code**

If `org-gtd-project--update-after-task-addition` is unused:
```elisp
;; Delete the function entirely
```

If partially used, simplify to only what's needed.

**Step 4: Run full test suite**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests pass

**Step 5: Commit**

```bash
git add org-gtd-projects.el
git commit -m "refactor: remove dead code from old add-to-project flow"
```

---

## Task 9: Run full test suite and verify

**Step 1: Run all tests**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests pass

**Step 2: Manual verification**

1. Open Emacs with org-gtd loaded
2. Create an inbox item
3. Process inbox, clarify the item
4. Choose "Add to project" (a)
5. Verify: Project selection prompt appears FIRST
6. Select a project
7. Verify: Task has `ORG_GTD_PROJECT_IDS` property set
8. Verify: Task is in project's `FIRST_TASKS`
9. Verify: Task was refiled under project

**Step 3: Commit any final fixes**

```bash
git add -A
git commit -m "fix: final adjustments after manual testing"
```

---

## Summary

| Task | Description | Tests |
|------|-------------|-------|
| 1 | Create test file | 0 |
| 2 | Project selection helper | 1 |
| 3 | Move-to-project helper | 1 |
| 4 | Refile-with-user-targets | 1 |
| 5 | Rewrite main function | 1 |
| 6 | Skip-refile test | 1 |
| 7 | Refile-prompt-for-types test | 1 |
| 8 | Clean up dead code | 0 |
| 9 | Full verification | 0 |

**Total new tests:** 6
