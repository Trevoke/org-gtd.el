# Expand org-gtd-show-project-graph Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `org-gtd-show-project-graph` work on task headings (navigating to their parent project) and from org-agenda items.

**Architecture:** Expand the existing `org-gtd-graph-mode--find-project-at-point` to detect task headings via `ORG_GTD_PROJECT_IDS`, resolve project IDs to markers, and handle multi-project selection via `completing-read`. Add agenda detection at the start of `org-gtd-show-project-graph`.

**Tech Stack:** Emacs Lisp, org-mode, org-agenda, e-unit test framework

---

## Task 1: Test - Task in Single Project Shows That Project's Graph

**Files:**
- Create: `test/unit/graph-mode-entry-test.el`
- Reference: `org-gtd-graph-mode.el:163-169` (current `--find-project-at-point`)

**Step 1: Create test file with setup**

```elisp
;;; graph-mode-entry-test.el --- Tests for graph entry point expansion -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:
;; Tests for org-gtd-show-project-graph working on task headings and agenda items.

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

(require 'org-gtd-graph-mode)

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Tests

(deftest graph-mode-entry/task-in-single-project-finds-project ()
  "When point is on a task belonging to one project, find that project."
  (with-current-buffer (org-gtd--default-file)
    (let* ((project-info (make-project "Test Project"
                                       :id "proj-1"
                                       :tasks '("Task One")))
           (project-marker (plist-get project-info :marker))
           (task-markers (plist-get project-info :task-markers))
           (task-marker (car task-markers)))
      ;; Go to task heading
      (goto-char (marker-position task-marker))
      ;; Find project should return project marker
      (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
        (should found-marker)
        (should (equal (marker-position found-marker)
                       (marker-position project-marker)))))))

(provide 'graph-mode-entry-test)
;;; graph-mode-entry-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: FAIL - currently `--find-project-at-point` only works on project headings

**Step 3: Commit test file**

```bash
git add test/unit/graph-mode-entry-test.el
git commit -m "test: add failing test for graph entry from task heading"
```

---

## Task 2: Implement - Task in Single Project

**Files:**
- Modify: `org-gtd-graph-mode.el:163-169`

**Step 1: Expand `org-gtd-graph-mode--find-project-at-point`**

Replace the current function with:

```elisp
(defun org-gtd-graph-mode--find-project-at-point ()
  "Find project for heading at point.
Works on:
- Project headings (ORG_GTD=Projects): returns that project
- Task headings with ORG_GTD_PROJECT_IDS: returns the project (or prompts if multiple)
- Other headings: returns nil with appropriate message"
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading t)
      (let ((org-gtd-type (org-entry-get nil "ORG_GTD")))
        (cond
         ;; Project heading - return it directly
         ((string= org-gtd-type "Projects")
          (point-marker))

         ;; Has ORG_GTD property - check for project membership
         (org-gtd-type
          (org-gtd-graph-mode--find-project-for-task))

         ;; No ORG_GTD property
         (t
          (message "Heading not a GTD item")
          nil))))))

(defun org-gtd-graph-mode--find-project-for-task ()
  "Find project marker for task at point.
If task belongs to multiple projects, prompt user to select.
Returns nil with message if task not in any project."
  (let ((project-ids (org-entry-get-multivalued-property (point) org-gtd-prop-project-ids)))
    (cond
     ;; No projects
     ((null project-ids)
      (message "Task not in a project")
      nil)

     ;; Single project
     ((= (length project-ids) 1)
      (org-id-find (car project-ids) 'marker))

     ;; Multiple projects - prompt
     (t
      (org-gtd-graph-mode--select-project-for-task project-ids)))))

(defun org-gtd-graph-mode--select-project-for-task (project-ids)
  "Prompt user to select from PROJECT-IDS and return marker.
Shows project titles in completion."
  (let* ((projects (org-gtd-graph-mode--resolve-project-ids project-ids))
         (titles (mapcar #'car projects)))
    (if (null projects)
        (progn
          (message "Task not in a project")
          nil)
      (let ((choice (completing-read
                     (format "Task is in %d projects, select: " (length projects))
                     titles nil t)))
        (cdr (assoc choice projects))))))

(defun org-gtd-graph-mode--resolve-project-ids (project-ids)
  "Resolve PROJECT-IDS to alist of (title . marker).
Filters out IDs that can't be resolved (deleted projects)."
  (let (results)
    (dolist (id project-ids)
      (when-let ((marker (org-id-find id 'marker)))
        (org-with-point-at marker
          (push (cons (org-get-heading t t t t) marker) results))))
    (nreverse results)))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: PASS

**Step 3: Commit**

```bash
git add org-gtd-graph-mode.el
git commit -m "feat: graph entry point works on task headings in single project"
```

---

## Task 3: Test - Task in Multiple Projects Prompts Selection

**Files:**
- Modify: `test/unit/graph-mode-entry-test.el`

**Step 1: Add test for multi-project task**

```elisp
(deftest graph-mode-entry/task-in-multiple-projects-prompts-selection ()
  "When task is in multiple projects, prompt user to select."
  (with-current-buffer (org-gtd--default-file)
    ;; Create two projects
    (let* ((proj1-info (make-project "Project Alpha" :id "proj-alpha"))
           (proj2-info (make-project "Project Beta" :id "proj-beta"))
           (proj1-marker (plist-get proj1-info :marker))
           (proj2-marker (plist-get proj2-info :marker)))

      ;; Create a task that belongs to both projects
      (goto-char (point-max))
      (let ((task-marker (make-task "Shared Task"
                                    :id "shared-task"
                                    :project-ids '("proj-alpha" "proj-beta")
                                    :level 1)))
        ;; Go to task
        (goto-char (marker-position task-marker))

        ;; Simulate selecting "Project Beta"
        (with-simulated-input "Project Beta RET"
          (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
            (should found-marker)
            (should (equal (marker-position found-marker)
                           (marker-position proj2-marker)))))))))
```

**Step 2: Run test**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: PASS (implementation already handles this)

**Step 3: Commit**

```bash
git add test/unit/graph-mode-entry-test.el
git commit -m "test: add test for multi-project task selection"
```

---

## Task 4: Test - Task Not in Any Project Shows Message

**Files:**
- Modify: `test/unit/graph-mode-entry-test.el`

**Step 1: Add test for orphan task**

```elisp
(deftest graph-mode-entry/task-not-in-project-shows-message ()
  "When task has no ORG_GTD_PROJECT_IDS, show message and return nil."
  (with-current-buffer (org-gtd--default-file)
    ;; Create a standalone single action (no project)
    (goto-char (point-max))
    (let ((task-marker (make-task "Standalone Action"
                                  :id "standalone-task"
                                  :level 1)))
      ;; Set ORG_GTD but no PROJECT_IDS
      (goto-char (marker-position task-marker))
      (org-entry-put (point) "ORG_GTD" "Actions")

      ;; Find project should return nil
      (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
        (should-not found-marker)))))
```

**Step 2: Run test**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-mode-entry-test.el
git commit -m "test: add test for task not in project"
```

---

## Task 5: Test - Heading Without ORG_GTD Property Shows Message

**Files:**
- Modify: `test/unit/graph-mode-entry-test.el`

**Step 1: Add test for non-GTD heading**

```elisp
(deftest graph-mode-entry/non-gtd-heading-shows-message ()
  "When heading has no ORG_GTD property, show message and return nil."
  (with-current-buffer (org-gtd--default-file)
    ;; Create a plain org heading (no GTD properties)
    (goto-char (point-max))
    (insert "* Regular Heading\n")
    (forward-line -1)
    (org-back-to-heading t)

    ;; Find project should return nil
    (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
      (should-not found-marker))))
```

**Step 2: Run test**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-mode-entry-test.el
git commit -m "test: add test for non-GTD heading"
```

---

## Task 6: Test - Project Heading Still Works (Regression)

**Files:**
- Modify: `test/unit/graph-mode-entry-test.el`

**Step 1: Add regression test**

```elisp
(deftest graph-mode-entry/project-heading-returns-itself ()
  "When point is on a project heading, return that project (regression test)."
  (with-current-buffer (org-gtd--default-file)
    (let* ((project-info (make-project "Direct Project" :id "direct-proj"))
           (project-marker (plist-get project-info :marker)))
      ;; Go to project heading
      (goto-char (marker-position project-marker))
      ;; Find project should return the same marker
      (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
        (should found-marker)
        (should (equal (marker-position found-marker)
                       (marker-position project-marker)))))))
```

**Step 2: Run test**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-mode-entry-test.el
git commit -m "test: add regression test for project heading"
```

---

## Task 7: Test - From Org-Agenda Item

**Files:**
- Modify: `test/unit/graph-mode-entry-test.el`
- Modify: `org-gtd-graph-mode.el`

**Step 1: Add test for agenda entry**

```elisp
(deftest graph-mode-entry/from-agenda-finds-project ()
  "When called from org-agenda on a task, find its project."
  (with-current-buffer (org-gtd--default-file)
    (let* ((project-info (make-project "Agenda Project"
                                       :id "agenda-proj"
                                       :tasks '((:description "Agenda Task"
                                                 :id "agenda-task"
                                                 :status next))))
           (project-marker (plist-get project-info :marker))
           (task-ids (plist-get project-info :task-ids))
           (task-id (car task-ids)))
      (basic-save-buffer)

      ;; Build agenda
      (let ((org-agenda-files (list (buffer-file-name))))
        (org-agenda-list nil nil 'day)

        (with-current-buffer org-agenda-buffer-name
          ;; Find our task in agenda
          (goto-char (point-min))
          (search-forward "Agenda Task")
          (beginning-of-line)

          ;; Should find project from agenda context
          (let ((found-marker (org-gtd-graph-mode--find-project-at-point-or-agenda)))
            (should found-marker)
            (should (equal (marker-position found-marker)
                           (marker-position project-marker)))))

        ;; Clean up agenda buffer
        (when (get-buffer org-agenda-buffer-name)
          (kill-buffer org-agenda-buffer-name))))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: FAIL - function `org-gtd-graph-mode--find-project-at-point-or-agenda` doesn't exist

**Step 3: Implement agenda support**

Add to `org-gtd-graph-mode.el`:

```elisp
(defun org-gtd-graph-mode--find-project-at-point-or-agenda ()
  "Find project from point in org-mode or from agenda item.
In org-agenda-mode, gets the marker for the item at point and
delegates to the org-mode logic."
  (cond
   ;; In agenda - get marker and process
   ((derived-mode-p 'org-agenda-mode)
    (if-let ((marker (org-get-at-bol 'org-marker)))
        (org-with-point-at marker
          (org-gtd-graph-mode--find-project-at-point))
      (message "No task at point")
      nil))

   ;; In org-mode - direct processing
   ((derived-mode-p 'org-mode)
    (org-gtd-graph-mode--find-project-at-point))

   ;; Other modes
   (t
    (message "Not in org-mode or org-agenda")
    nil)))
```

**Step 4: Update `org-gtd-show-project-graph` to use new function**

Replace in `org-gtd-show-project-graph`:

```elisp
;;;###autoload
(defun org-gtd-show-project-graph (&optional project-marker)
  "Display an interactive visual graph of project task dependencies.
Shows tasks as nodes and dependencies as edges in an SVG visualization.

When called interactively:
- On a project heading: shows that project's graph
- On a task heading: shows the project it belongs to (prompts if multiple)
- From org-agenda: shows the project for the task at point
- Otherwise: prompts to select a project

If PROJECT-MARKER is provided, shows that project's graph directly."
  (interactive)
  (let ((marker (or project-marker
                    (org-gtd-graph-mode--find-project-at-point-or-agenda)
                    (org-gtd-graph-mode--prompt-for-project))))
    (when marker
      (org-gtd-graph-view-create marker))))
```

**Step 5: Run test**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: PASS

**Step 6: Commit**

```bash
git add org-gtd-graph-mode.el test/unit/graph-mode-entry-test.el
git commit -m "feat: graph entry point works from org-agenda"
```

---

## Task 8: Test - Agenda Item Not in Project Shows Message

**Files:**
- Modify: `test/unit/graph-mode-entry-test.el`

**Step 1: Add test**

```elisp
(deftest graph-mode-entry/agenda-item-not-in-project-shows-message ()
  "When agenda item is not in a project, show message."
  (with-current-buffer (org-gtd--default-file)
    ;; Create standalone action (next, so it appears in agenda)
    (goto-char (point-max))
    (let ((task-marker (make-task "Standalone Next"
                                  :id "standalone-next"
                                  :status 'next
                                  :level 1)))
      (goto-char (marker-position task-marker))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer)

      ;; Build agenda
      (let ((org-agenda-files (list (buffer-file-name))))
        (org-agenda-list nil nil 'day)

        (with-current-buffer org-agenda-buffer-name
          ;; Find our task in agenda
          (goto-char (point-min))
          (search-forward "Standalone Next")
          (beginning-of-line)

          ;; Should return nil
          (let ((found-marker (org-gtd-graph-mode--find-project-at-point-or-agenda)))
            (should-not found-marker)))

        ;; Clean up
        (when (get-buffer org-agenda-buffer-name)
          (kill-buffer org-agenda-buffer-name))))))
```

**Step 2: Run test**

Run: `~/.local/bin/eldev etest -r dot test/unit/graph-mode-entry-test.el`

Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-mode-entry-test.el
git commit -m "test: add test for agenda item not in project"
```

---

## Task 9: Run Full Test Suite

**Step 1: Run all tests to ensure no regressions**

Run: `~/.local/bin/eldev etest -r dot`

Expected: All tests pass (1080+ tests)

**Step 2: Final commit if any cleanup needed**

```bash
git status
# If clean, skip this step
```

---

## Task 10: Update Autoloads

**Step 1: Regenerate autoloads**

Run: `~/.local/bin/eldev compile`

**Step 2: Verify lazy loading still works**

Run:
```bash
~/.local/bin/eldev emacs -- --batch -q -L . -L .eldev/30.2/packages/org-edna-1.1.2 -L .eldev/30.2/packages/transient-0.8.4 -L .eldev/30.2/packages/f-0.21.0 -L .eldev/30.2/packages/s-1.13.1 -L .eldev/30.2/packages/dash-2.19.1 -L .eldev/30.2/packages/org-agenda-property-1.3.1 -L .eldev/30.2/packages/dag-draw-1.0.4 -L .eldev/30.2/packages/ht-2.4 --eval "(setq org-gtd-update-ack \"4.0.0\")" --eval "(require 'org-gtd)" --eval "(message \"graph-mode loaded: %s\" (featurep 'org-gtd-graph-mode))"
```

Expected: `graph-mode loaded: nil`

**Step 3: Commit if autoloads changed**

```bash
git add org-gtd-autoloads.el
git commit -m "chore: regenerate autoloads"
```

---

## Summary

| Task | Description | Tests |
|------|-------------|-------|
| 1 | Test: task in single project | 1 |
| 2 | Implement: expand `--find-project-at-point` | - |
| 3 | Test: task in multiple projects | 1 |
| 4 | Test: task not in project | 1 |
| 5 | Test: non-GTD heading | 1 |
| 6 | Test: project heading regression | 1 |
| 7 | Test + Implement: agenda support | 1 |
| 8 | Test: agenda item not in project | 1 |
| 9 | Full test suite | 1080+ |
| 10 | Autoloads verification | - |

**Total new tests:** 7
