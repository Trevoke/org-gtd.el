# Project Someday/Maybe Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `org-gtd-someday` handle projects the same way `org-gtd-tickler` does — cascading state saves to all child tasks, with smart dispatch from project headings, project tasks, and the graph view.

**Architecture:** Refactor `org-gtd-project--save-state` to be generic (save only, no type assignment). Add `org-gtd-project-someday` parallel to `org-gtd-project-incubate`. Add smart dispatcher to `org-gtd-someday`. Add `S` keybinding in graph view.

**Tech Stack:** Emacs Lisp, org-mode, e-unit test framework, `with-simulated-input` for interactive prompts

**Design doc:** `docs/plans/2026-02-11-project-someday-support.md`

---

### Task 1: Refactor `org-gtd-project--save-state` to be Generic

**Files:**
- Modify: `org-gtd-projects.el:968-995`
- Test: `test/unit/project-tickler-test.el`

**Step 1: Update the existing test to match new generic behavior**

In `test/unit/project-tickler-test.el`, the test `tickler/saves-state-to-previous-properties` (line 34) currently asserts that `org-gtd-project--save-state` sets `ORG_GTD` to "Tickler". Update it to assert that `ORG_GTD` is unchanged (save-state no longer sets the type).

Replace lines 50-57 of the test with:

```elisp
    ;; Verify state was saved
    (assert-equal "Actions" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
    (assert-equal "NEXT" (org-entry-get (point) "PREVIOUS_TODO"))

    ;; Verify ORG_GTD is NOT changed (generic save-state doesn't set type)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

    ;; Verify TODO keyword was cleared
    (assert-nil (org-entry-get (point) "TODO"))))
```

**Step 2: Run the test to verify it fails**

Run: `~/bin/eldev etest -r dot -s tickler/saves-state`
Expected: FAIL — the test expects ORG_GTD="Actions" but gets "Tickler" because the function still hardcodes it.

**Step 3: Refactor `org-gtd-project--save-state`**

In `org-gtd-projects.el`, replace lines 968-995 with:

```elisp
(defun org-gtd-project--save-state (marker)
  "Save ORG_GTD and TODO state at MARKER to PREVIOUS_* properties.

Saves current ORG_GTD value to PREVIOUS_ORG_GTD property.
Saves current TODO keyword to PREVIOUS_TODO property.
Clears the TODO keyword.

Does NOT set ORG_GTD to any value -- callers are responsible for
setting the target type after calling this function.

Skips tasks belonging to multiple projects (identified by
multiple IDs in ORG_GTD_PROJECT_IDS property)."
  (org-with-point-at marker
    ;; Check if this is a multi-project task
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (if (> (length project-ids) 1)
          ;; Skip multi-project tasks
          (message "Skipping multi-project task: %s" (org-get-heading t t t t))
        ;; Save state
        (let ((current-org-gtd (org-entry-get (point) "ORG_GTD"))
              (current-todo (org-entry-get (point) "TODO")))
          ;; Save current state
          (when current-org-gtd
            (org-entry-put (point) "PREVIOUS_ORG_GTD" current-org-gtd))
          (when current-todo
            (org-entry-put (point) "PREVIOUS_TODO" current-todo))
          ;; Clear TODO keyword
          (org-todo 'none))))))
```

**Step 4: Run the test to verify it passes**

Run: `~/bin/eldev etest -r dot -s tickler/saves-state`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/unit/project-tickler-test.el
git commit -m "refactor: make project save-state generic (no type assignment)"
```

---

### Task 2: Update `org-gtd-project-incubate` to Set Type After Generic Save

**Files:**
- Modify: `org-gtd-projects.el:1103-1146`
- Test: `test/unit/project-tickler-test.el`

**Step 1: Run the existing tickler incubate test to see it fail**

Run: `~/bin/eldev etest -r dot -s tickler/incubates-project`
Expected: FAIL — after Task 1's refactor, `org-gtd-project-incubate` calls `org-gtd-project--save-state` which no longer sets ORG_GTD to "Tickler", so the assertions on line 135 and 143 will fail.

**Step 2: Update `org-gtd-project-incubate` to set ORG_GTD after save-state**

In `org-gtd-projects.el`, replace the body of `org-gtd-project-incubate` (lines 1120-1146) with:

```elisp
  (org-with-point-at project-marker
    ;; Check for external dependencies
    (let ((external-deps (org-gtd-project--check-external-dependencies project-marker)))
      (when external-deps
        (let ((dep-names (mapcar (lambda (m)
                                   (org-with-point-at m
                                     (org-get-heading t t t t)))
                                 external-deps)))
          (unless (yes-or-no-p
                   (format "External tasks depend on this project:\n%s\n\nContinue ticklering? "
                           (mapconcat (lambda (name) (format "  - %s" name))
                                      dep-names "\n")))
            (user-error "Tickler cancelled")))))

    ;; Save state and set type for project heading
    (org-gtd-project--save-state project-marker)
    (org-entry-put (point) "ORG_GTD" org-gtd-tickler)

    ;; Set review date
    (org-entry-put (point) org-gtd-timestamp (format "<%s>" review-date))

    ;; Save state and set type for all tasks
    (let ((task-markers (org-gtd-project--get-all-tasks project-marker)))
      (dolist (task-marker task-markers)
        (org-gtd-project--save-state task-marker)
        (org-with-point-at task-marker
          (org-entry-put (point) "ORG_GTD" org-gtd-tickler))))

    ;; Save changes to disk
    (save-buffer)))
```

**Step 3: Run the tickler tests to verify they pass**

Run: `~/bin/eldev etest -r dot -s tickler/`
Expected: ALL PASS — incubate test passes because we now explicitly set ORG_GTD to Tickler. Reactivate test passes unchanged. Save-state test passes with updated assertions from Task 1.

**Step 4: Commit**

```bash
git add org-gtd-projects.el
git commit -m "fix: set ORG_GTD type explicitly after generic save-state in project-incubate"
```

---

### Task 3: Add `org-gtd-project-someday` Function

**Files:**
- Modify: `org-gtd-projects.el` (add after `org-gtd-project-incubate`, around line 1147)
- Create: `test/unit/project-someday-test.el`

**Step 1: Write the failing test for project someday (basic)**

Create `test/unit/project-someday-test.el`:

```elisp
;;; project-someday-test.el --- Unit tests for project someday -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025, 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for project someday functionality including state preservation
;; and reactivation.

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Project Someday

(deftest project-someday/sets-heading-and-tasks-to-someday ()
  "Someday-ing a project sets ORG_GTD to Someday on heading and all tasks."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker)))

      ;; Someday the project
      (org-gtd-project-someday project-marker)

      ;; Verify project heading
      (org-with-point-at project-marker
        (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))
        (assert-equal "Projects" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
        (assert-nil (org-entry-get (point) "ORG_GTD_TIMESTAMP")))

      ;; Verify tasks
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))
      (assert-equal "Actions" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
      (assert-nil (org-entry-get (point) "TODO"))
      (assert-equal "NEXT" (org-entry-get (point) "PREVIOUS_TODO")))))

(deftest project-someday/reactivates-from-someday ()
  "Reactivating a someday project restores all states."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker)))

      ;; Someday then reactivate
      (org-gtd-project-someday project-marker)
      (org-gtd-project-reactivate project-marker)

      ;; Verify project heading restored
      (org-with-point-at project-marker
        (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))
        (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD")))

      ;; Verify tasks restored
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
      (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))
      (assert-true (org-entry-get (point) "TODO")))))

(deftest project-someday/sets-someday-list-on-heading ()
  "Someday-ing a project with lists configured sets ORG_GTD_SOMEDAY_LIST on heading."
  (let ((org-gtd-someday-lists '("Work Ideas" "Personal")))
    (create-project "Test project")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Test project")
      (org-back-to-heading t)
      (let ((project-marker (point-marker)))

        ;; Someday with list selection
        (with-simulated-input "Work SPC Ideas TAB RET"
          (org-gtd-project-someday project-marker))

        ;; Verify list is set on heading
        (org-with-point-at project-marker
          (assert-equal "Work Ideas" (org-entry-get (point) "ORG_GTD_SOMEDAY_LIST")))

        ;; Verify list is NOT set on tasks
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (assert-nil (org-entry-get (point) "ORG_GTD_SOMEDAY_LIST"))))))

(deftest project-someday/checks-external-dependencies ()
  "Someday-ing a project with external deps warns the user."
  ;; This test just verifies the function calls the dependency check.
  ;; Detailed dependency testing is in project-tickler-advanced-test.el.
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker)))
      ;; No external deps, so it should succeed without prompting
      (org-gtd-project-someday project-marker)
      (org-with-point-at project-marker
        (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))))))

(provide 'project-someday-test)

;;; project-someday-test.el ends here
```

**Step 2: Run the tests to verify they fail**

Run: `~/bin/eldev etest -r dot -s project-someday/`
Expected: FAIL — `org-gtd-project-someday` is not defined.

**Step 3: Implement `org-gtd-project-someday`**

In `org-gtd-projects.el`, add after `org-gtd-project-incubate` (after line 1146). You will also need to add `(require 'org-gtd-someday)` near the top of the file, or use `(require 'org-gtd-core)` which already provides `org-gtd-someday` as a constant. Check that `org-gtd-someday` (the constant string "Someday") and `org-gtd-prop-someday-list` are available from `org-gtd-core.el`.

```elisp
(defun org-gtd-project-someday (project-marker)
  "Put project at PROJECT-MARKER into someday/maybe.

PROJECT-MARKER is a marker pointing to the project heading.

Puts the project into someday/maybe by:
1. Saving state for project heading and all tasks
2. Setting ORG_GTD to Someday on everything
3. Prompting for someday list if configured"
  (interactive (list (point-marker)))

  (org-with-point-at project-marker
    ;; Check for external dependencies
    (let ((external-deps (org-gtd-project--check-external-dependencies project-marker)))
      (when external-deps
        (let ((dep-names (mapcar (lambda (m)
                                   (org-with-point-at m
                                     (org-get-heading t t t t)))
                                 external-deps)))
          (unless (yes-or-no-p
                   (format "External tasks depend on this project:\n%s\n\nContinue with someday? "
                           (mapconcat (lambda (name) (format "  - %s" name))
                                      dep-names "\n")))
            (user-error "Someday cancelled")))))

    ;; Save state and set type for project heading
    (org-gtd-project--save-state project-marker)
    (org-entry-put (point) "ORG_GTD" org-gtd-someday)

    ;; Prompt for someday list if configured
    (when org-gtd-someday-lists
      (let ((list (completing-read "Someday list: " org-gtd-someday-lists nil t)))
        (org-entry-put (point) org-gtd-prop-someday-list list)))

    ;; Save state and set type for all tasks
    (let ((task-markers (org-gtd-project--get-all-tasks project-marker)))
      (dolist (task-marker task-markers)
        (org-gtd-project--save-state task-marker)
        (org-with-point-at task-marker
          (org-entry-put (point) "ORG_GTD" org-gtd-someday))))

    ;; Save changes to disk
    (save-buffer)))
```

Note: `org-gtd-someday-lists` and `org-gtd-prop-someday-list` are defined in `org-gtd-someday.el` and `org-gtd-core.el` respectively. You may need `(require 'org-gtd-someday)` at the top of `org-gtd-projects.el`. Check for circular dependency — if `org-gtd-someday.el` already requires `org-gtd-projects.el`, use `(defvar org-gtd-someday-lists)` as a forward declaration instead, and `(require 'org-gtd-someday)` inside the function body.

**Step 4: Run the tests to verify they pass**

Run: `~/bin/eldev etest -r dot -s project-someday/`
Expected: ALL PASS

**Step 5: Run full test suite to verify no regressions**

Run: `~/bin/eldev etest -r dot`
Expected: ALL PASS

**Step 6: Commit**

```bash
git add org-gtd-projects.el test/unit/project-someday-test.el
git commit -m "feat: add org-gtd-project-someday for project-level someday/maybe"
```

---

### Task 4: Add `org-gtd-project-someday-from-context`

**Files:**
- Modify: `org-gtd-projects.el` (add after `org-gtd-project-incubate-from-context`, around line 300)

**Step 1: Implement `org-gtd-project-someday-from-context`**

In `org-gtd-projects.el`, add after `org-gtd-project-incubate-from-context` (after line 300):

```elisp
;;;###autoload
(defun org-gtd-project-someday-from-context ()
  "Put the current project into someday/maybe.
Works from graph view or agenda context."
  (interactive)
  (require 'org-gtd-someday)
  (let ((project-marker (org-gtd-project--get-marker-from-context)))
    (org-with-point-at project-marker
      (call-interactively #'org-gtd-someday))
    (message "Project moved to someday/maybe")))
```

**Step 2: Run full test suite to verify no regressions**

Run: `~/bin/eldev etest -r dot`
Expected: ALL PASS

**Step 3: Commit**

```bash
git add org-gtd-projects.el
git commit -m "feat: add org-gtd-project-someday-from-context for graph/agenda dispatch"
```

---

### Task 5: Add Smart Dispatcher to `org-gtd-someday`

**Files:**
- Modify: `org-gtd-someday.el:63-71`
- Create: `test/unit/someday-dispatcher-test.el`

**Step 1: Write failing test for project heading dispatch**

Create `test/unit/someday-dispatcher-test.el`:

```elisp
;;; someday-dispatcher-test.el --- Tests for org-gtd-someday smart dispatch -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-someday smart dispatcher detecting project headings
;; and project tasks.

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Dispatcher Tests

(deftest someday-dispatch/on-project-heading-somedays-whole-project ()
  "Calling org-gtd-someday on a project heading somedays the whole project."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)

    ;; Call org-gtd-someday on project heading
    (org-gtd-someday)

    ;; Verify project heading is someday
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "Projects" (org-entry-get (point) "PREVIOUS_ORG_GTD"))

    ;; Verify all tasks are someday
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))))

(deftest someday-dispatch/on-project-task-somedays-whole-project ()
  "Calling org-gtd-someday on a project task somedays the whole project."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    ;; Navigate to a task, not the project heading
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)

    ;; Call org-gtd-someday on a project task
    (org-gtd-someday)

    ;; Verify the entire project is someday
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))

    ;; Verify all tasks are someday
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))))

(provide 'someday-dispatcher-test)

;;; someday-dispatcher-test.el ends here
```

**Step 2: Run the tests to verify they fail**

Run: `~/bin/eldev etest -r dot -s someday-dispatch/`
Expected: FAIL — `org-gtd-someday` doesn't dispatch to project-level logic yet.

**Step 3: Implement the smart dispatcher**

In `org-gtd-someday.el`, replace the `org-gtd-someday` function (lines 63-71) with:

```elisp
(defun org-gtd-someday ()
  "Decorate, organize and refile item at point as someday/maybe.

Smart dispatcher that detects context:
- On project heading (ORG_GTD: Projects): someday entire project
- On project task (has ORG_GTD_PROJECT_IDS): someday project(s)
- On single item: use existing single-item someday logic

Someday/maybe items are for things you might want to do eventually,
but with no specific timeframe."
  (interactive)

  ;; Get the actual marker - works from both org buffers and agenda buffers
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (point-marker))))
    (org-with-point-at marker
      ;; Detect context
      (let* ((org-gtd-value (org-entry-get (point) "ORG_GTD"))
             (project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))
             (is-project-heading (string= org-gtd-value "Projects"))
             (is-project-task (> (length project-ids) 0)))

        (cond
         ;; Case 1: On project heading - someday the project
         (is-project-heading
          (require 'org-gtd-projects)
          (org-gtd-project-someday (point-marker)))

         ;; Case 2: On project task - someday the project(s)
         (is-project-task
          (require 'org-gtd-projects)
          (let ((project-marker (org-gtd-project--get-marker-at-point
                                 "Which project to put on someday? ")))
            (org-gtd-project-someday project-marker)))

         ;; Case 3: Single item - use existing logic
         (t
          (org-gtd-organize--call
           (lambda () (org-gtd-someday--apply)))))))))
```

Also add `(require 'org-gtd-projects)` to the requires section at the top of `org-gtd-someday.el` (after line 36). If this creates a circular dependency, remove it and rely on the `(require 'org-gtd-projects)` calls inside the cond branches instead.

**Step 4: Run the dispatcher tests to verify they pass**

Run: `~/bin/eldev etest -r dot -s someday-dispatch/`
Expected: ALL PASS

**Step 5: Run full test suite to verify no regressions**

Run: `~/bin/eldev etest -r dot`
Expected: ALL PASS

**Step 6: Commit**

```bash
git add org-gtd-someday.el test/unit/someday-dispatcher-test.el
git commit -m "feat: add smart dispatcher to org-gtd-someday for project detection"
```

---

### Task 6: Add Graph View Keybinding

**Files:**
- Modify: `org-gtd-graph-transient.el:110-111` (add `S` entry)
- Modify: `org-gtd-graph-transient.el` (add function after line 891)
- Modify: `org-gtd-graph-mode.el:97-98` (add `S` keybinding)

**Step 1: Add `org-gtd-graph-someday-project` function**

In `org-gtd-graph-transient.el`, add after `org-gtd-graph-incubate-project` (after line 891):

```elisp
(defun org-gtd-graph-someday-project ()
  "Put the current project on someday/maybe from graph mode.
Prompts for someday list if configured, then closes the graph view."
  (interactive)
  (org-gtd-project-someday-from-context)
  ;; Clean up details pane before quitting
  (org-gtd-graph-ui-cleanup-windows)
  (quit-window))
```

**Step 2: Add `S` to the transient menu**

In `org-gtd-graph-transient.el`, in the `["Session"` group (around line 109), add the `S` entry after `I`:

```elisp
   ["Session"
    ("I" "Incubate project" org-gtd-graph-incubate-project :transient nil)
    ("S" "Someday/Maybe" org-gtd-graph-someday-project :transient nil)
    ("C" "Cancel project" org-gtd-graph-cancel-project :transient nil)
    ("q" "Quit menu" transient-quit-one :transient nil)
    ("Q" "Quit and kill buffer" org-gtd-graph-quit-and-kill :transient nil)
    (org-gtd-graph-transient--sticky)]])
```

**Step 3: Add `S` keybinding in mode map**

In `org-gtd-graph-mode.el`, after line 98 (`define-key` for `I`), add:

```elisp
    (define-key map (kbd "S") #'org-gtd-graph-someday-project)
```

**Step 4: Run full test suite to verify no regressions**

Run: `~/bin/eldev etest -r dot`
Expected: ALL PASS

**Step 5: Compile to check for warnings**

Run: `~/bin/eldev clean && ~/bin/eldev compile`
Expected: No errors. Warnings about undefined functions are OK if they're autoloaded.

**Step 6: Commit**

```bash
git add org-gtd-graph-transient.el org-gtd-graph-mode.el
git commit -m "feat: add S keybinding for someday/maybe in graph view"
```

---

### Task 7: Final Verification

**Step 1: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: ALL PASS

**Step 2: Clean compile**

Run: `~/bin/eldev clean && ~/bin/eldev compile`
Expected: No errors.

**Step 3: Verify no circular dependencies**

Run: `~/bin/eldev compile --warnings-as-errors`
Expected: No errors related to circular requires.
