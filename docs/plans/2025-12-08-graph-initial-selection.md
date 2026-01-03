# Graph Initial Selection: First Actionable Task Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** When opening the project graph view, automatically select the first actionable task (NEXT, WAIT, etc.) instead of the project heading, using breadth-first search.

**Architecture:** Add a BFS helper function to `org-gtd-graph-data.el` that searches from root tasks for the first node with an actionable state (not DONE, CNCL, TODO, or nil). Update `org-gtd-graph-view-create` to use this function, falling back to project heading if no actionable task exists.

**Tech Stack:** Emacs Lisp, e-unit testing framework, org-gtd graph data structures

---

## Task 1: Create Test File with Basic Structure

**Files:**
- Create: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the test file skeleton**

```elisp
;;; graph-initial-selection-test.el --- Tests for graph initial selection -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-graph-data-find-first-actionable function.
;; Tests BFS traversal to find first actionable task (NEXT, WAIT, etc.)
;; for initial selection when opening project graph view.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-graph-data)

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (setq org-id-locations nil)
    (setq org-id-files nil)
    (unwind-protect
        (funcall proceed context)
      (setq org-id-locations nil)
      (setq org-id-files nil)
      (org-id-locations-save))))

;;; Tests go here

(provide 'graph-initial-selection-test)

;;; graph-initial-selection-test.el ends here
```

**Step 2: Verify file loads without errors**

Run: `~/.local/bin/eldev compile test/unit/graph-initial-selection-test.el`
Expected: No errors

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add skeleton for graph initial selection tests"
```

---

## Task 2: Write Test - Function Exists

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

Add after the `around-each` block:

```elisp
(deftest graph-initial/function-exists ()
  "Test that the function exists."
  (assert-true (fboundp 'org-gtd-graph-data-find-first-actionable)))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "graph-initial/function-exists"`
Expected: FAIL - function not defined

**Step 3: Write minimal implementation**

In `org-gtd-graph-data.el`, add before the `;;;; Footer` section (around line 462):

```elisp
;;;; First Actionable Task

(defun org-gtd-graph-data-find-first-actionable (graph)
  "Find the first actionable task in GRAPH using breadth-first search.

An actionable task has a TODO state that is NOT:
- DONE (completed)
- CNCL (cancelled)
- TODO (not yet activated)
- nil (no state)

This means NEXT, WAIT, or other active workflow states.

Searches from root tasks (ORG_GTD_FIRST_TASKS) in BFS order.

Returns the node ID of the first actionable task, or nil if none found."
  nil)
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/function-exists"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-graph-data.el test/unit/graph-initial-selection-test.el
git commit -m "feat: add stub for org-gtd-graph-data-find-first-actionable"
```

---

## Task 3: Write Test - Returns nil When No Tasks

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/returns-nil-when-no-tasks ()
  "Returns nil when graph has only project heading (no tasks)."
  (let* ((temp-file (make-temp-file "graph-initial-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with no tasks
          (make-project "Empty Project" :id "empty-proj-id")
          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Empty Project")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-nil result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes (stub returns nil)**

Run: `~/.local/bin/eldev test -B "graph-initial/returns-nil-when-no-tasks"`
Expected: PASS (stub already returns nil)

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for empty graph returns nil"
```

---

## Task 4: Write Test - Finds NEXT Task in Root

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/finds-next-task-in-root ()
  "Finds NEXT task when it's a root task."
  (let* ((temp-file (make-temp-file "graph-initial-next" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project with NEXT"
                        :id "proj-next-id"
                        :first-tasks '("task-next-id"))

          (make-task "Task with NEXT state"
                     :id "task-next-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-next-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project with NEXT")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-next-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev test -B "graph-initial/finds-next-task-in-root"`
Expected: FAIL - returns nil instead of "task-next-id"

**Step 3: Write implementation to make test pass**

Update `org-gtd-graph-data-find-first-actionable` in `org-gtd-graph-data.el`:

```elisp
(defun org-gtd-graph-data-find-first-actionable (graph)
  "Find the first actionable task in GRAPH using breadth-first search.

An actionable task has a TODO state that is NOT:
- DONE (completed)
- CNCL (cancelled)
- TODO (not yet activated)
- nil (no state)

This means NEXT, WAIT, or other active workflow states.

Searches from root tasks (ORG_GTD_FIRST_TASKS) in BFS order.

Returns the node ID of the first actionable task, or nil if none found."
  (let* ((nodes (org-gtd-graph-nodes graph))
         (root-ids (org-gtd-graph-root-ids graph))
         (project-id (org-gtd-graph-project-id graph))
         (non-actionable-states '("DONE" "CNCL" "TODO"))
         (visited (make-hash-table :test 'equal))
         (queue (copy-sequence root-ids)))

    (while queue
      (let ((node-id (pop queue)))
        (unless (or (gethash node-id visited)
                    (equal node-id project-id))
          (puthash node-id t visited)
          (when-let ((node (gethash node-id nodes)))
            (let ((state (org-gtd-graph-node-state node)))
              ;; Actionable = has a state that's not in non-actionable list
              (when (and state
                         (not (member state non-actionable-states)))
                (cl-return node-id)))
            ;; Enqueue successors for BFS
            (dolist (succ-id (org-gtd-graph-data-get-successors graph node-id))
              (unless (gethash succ-id visited)
                (setq queue (append queue (list succ-id)))))))))
    nil))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/finds-next-task-in-root"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-graph-data.el test/unit/graph-initial-selection-test.el
git commit -m "feat: implement BFS search for first actionable task"
```

---

## Task 5: Write Test - Finds WAIT Task in Root

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/finds-wait-task-in-root ()
  "Finds WAIT task when it's a root task."
  (let* ((temp-file (make-temp-file "graph-initial-wait" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project with WAIT"
                        :id "proj-wait-id"
                        :first-tasks '("task-wait-id"))

          (make-task "Task with WAIT state"
                     :id "task-wait-id"
                     :status 'wait
                     :level 2
                     :project-ids '("proj-wait-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project with WAIT")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-wait-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes (implementation already handles WAIT)**

Run: `~/.local/bin/eldev test -B "graph-initial/finds-wait-task-in-root"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for WAIT task found in root"
```

---

## Task 6: Write Test - Skips TODO Tasks

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/skips-todo-tasks ()
  "Skips tasks in TODO state, finds NEXT."
  (let* ((temp-file (make-temp-file "graph-initial-skip-todo" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project skip TODO"
                        :id "proj-skip-todo-id"
                        :first-tasks '("task-todo-id" "task-next-id"))

          ;; TODO task comes first in first-tasks
          (make-task "Task with TODO state"
                     :id "task-todo-id"
                     :status 'todo
                     :level 2
                     :project-ids '("proj-skip-todo-id"))

          ;; NEXT task comes second
          (make-task "Task with NEXT state"
                     :id "task-next-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-skip-todo-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project skip TODO")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            ;; Should find NEXT task, not TODO
            (assert-equal "task-next-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/skips-todo-tasks"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for skipping TODO tasks"
```

---

## Task 7: Write Test - Skips DONE Tasks

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/skips-done-tasks ()
  "Skips tasks in DONE state."
  (let* ((temp-file (make-temp-file "graph-initial-skip-done" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project skip DONE"
                        :id "proj-skip-done-id"
                        :first-tasks '("task-done-id" "task-next-id"))

          (make-task "Task with DONE state"
                     :id "task-done-id"
                     :status 'done
                     :level 2
                     :project-ids '("proj-skip-done-id"))

          (make-task "Task with NEXT state"
                     :id "task-next-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-skip-done-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project skip DONE")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-next-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/skips-done-tasks"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for skipping DONE tasks"
```

---

## Task 8: Write Test - Skips CNCL Tasks

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/skips-cncl-tasks ()
  "Skips tasks in CNCL state."
  (let* ((temp-file (make-temp-file "graph-initial-skip-cncl" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project skip CNCL"
                        :id "proj-skip-cncl-id"
                        :first-tasks '("task-cncl-id" "task-next-id"))

          (make-task "Task with CNCL state"
                     :id "task-cncl-id"
                     :status 'cncl
                     :level 2
                     :project-ids '("proj-skip-cncl-id"))

          (make-task "Task with NEXT state"
                     :id "task-next-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-skip-cncl-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project skip CNCL")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-equal "task-next-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/skips-cncl-tasks"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for skipping CNCL tasks"
```

---

## Task 9: Write Test - BFS Finds Actionable in Successors

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/bfs-finds-actionable-in-successors ()
  "BFS traverses to successors when roots are not actionable."
  (let* ((temp-file (make-temp-file "graph-initial-bfs-succ" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Task A (DONE) -> Task B (NEXT)
          (make-project "Project BFS successors"
                        :id "proj-bfs-succ-id"
                        :first-tasks '("task-a-id"))

          (make-task "Task A - done root"
                     :id "task-a-id"
                     :status 'done
                     :level 2
                     :project-ids '("proj-bfs-succ-id")
                     :blocks '("task-b-id"))

          (make-task "Task B - next successor"
                     :id "task-b-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-bfs-succ-id")
                     :depends-on '("task-a-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project BFS successors")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            ;; Should traverse from DONE Task A to find NEXT Task B
            (assert-equal "task-b-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/bfs-finds-actionable-in-successors"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for BFS finding actionable in successors"
```

---

## Task 10: Write Test - BFS Order is Breadth-First

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/bfs-searches-breadth-first ()
  "Searches all nodes at current depth before going deeper."
  (let* ((temp-file (make-temp-file "graph-initial-bfs-order" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Structure:
          ;;   Task A (TODO) -> Task C (NEXT)  [depth 2]
          ;;   Task B (NEXT)                   [depth 1]
          ;; BFS should find Task B first (same depth as A, but NEXT)
          (make-project "Project BFS order"
                        :id "proj-bfs-order-id"
                        :first-tasks '("task-a-id" "task-b-id"))

          (make-task "Task A - TODO root"
                     :id "task-a-id"
                     :status 'todo
                     :level 2
                     :project-ids '("proj-bfs-order-id")
                     :blocks '("task-c-id"))

          (make-task "Task B - NEXT root"
                     :id "task-b-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-bfs-order-id"))

          (make-task "Task C - NEXT at depth 2"
                     :id "task-c-id"
                     :status 'next
                     :level 2
                     :project-ids '("proj-bfs-order-id")
                     :depends-on '("task-a-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project BFS order")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            ;; Should find Task B (depth 1, NEXT) before Task C (depth 2, NEXT)
            (assert-equal "task-b-id" result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/bfs-searches-breadth-first"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for BFS breadth-first order"
```

---

## Task 11: Write Test - Returns nil When All Non-Actionable

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/returns-nil-when-all-non-actionable ()
  "Returns nil when all tasks are DONE, CNCL, or TODO."
  (let* ((temp-file (make-temp-file "graph-initial-all-non-action" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (make-project "Project all non-actionable"
                        :id "proj-all-na-id"
                        :first-tasks '("task-todo-id" "task-done-id" "task-cncl-id"))

          (make-task "Task TODO"
                     :id "task-todo-id"
                     :status 'todo
                     :level 2
                     :project-ids '("proj-all-na-id"))

          (make-task "Task DONE"
                     :id "task-done-id"
                     :status 'done
                     :level 2
                     :project-ids '("proj-all-na-id"))

          (make-task "Task CNCL"
                     :id "task-cncl-id"
                     :status 'cncl
                     :level 2
                     :project-ids '("proj-all-na-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project all non-actionable")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            (assert-nil result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/returns-nil-when-all-non-actionable"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for nil when all non-actionable"
```

---

## Task 12: Write Test - Skips Project Heading

**Files:**
- Modify: `test/unit/graph-initial-selection-test.el`

**Step 1: Write the failing test**

```elisp
(deftest graph-initial/skips-project-heading ()
  "Does not return project heading even if it has actionable state."
  (let* ((temp-file (make-temp-file "graph-initial-skip-proj" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with NEXT state (unusual but possible)
          ;; Only has TODO tasks
          (insert "* NEXT Project with actionable state\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: proj-actionable-id\n")
          (insert ":ORG_GTD: Projects\n")
          (insert ":ORG_GTD_FIRST_TASKS: task-todo-id\n")
          (insert ":END:\n")

          (make-task "Task TODO only"
                     :id "task-todo-id"
                     :status 'todo
                     :level 2
                     :project-ids '("proj-actionable-id"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          (goto-char (point-min))
          (search-forward "Project with actionable state")
          (org-back-to-heading t)
          (let* ((graph (org-gtd-graph-data--extract-from-project (point-marker)))
                 (result (org-gtd-graph-data-find-first-actionable graph)))
            ;; Should return nil - project heading excluded, no other actionable
            (assert-nil result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev test -B "graph-initial/skips-project-heading"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/graph-initial-selection-test.el
git commit -m "test: add test for skipping project heading"
```

---

## Task 13: Run All New Tests Together

**Step 1: Run all graph-initial tests**

Run: `~/.local/bin/eldev test -B "graph-initial/"`
Expected: All 11 tests PASS

**Step 2: Run full test suite**

Run: `~/.local/bin/eldev test -B`
Expected: All tests PASS (714+ tests)

**Step 3: Commit if any adjustments were needed**

---

## Task 14: Update Graph View to Use New Function

**Files:**
- Modify: `org-gtd-graph-view.el:110-113`

**Step 1: Read current implementation**

Current code at lines 110-113:
```elisp
;; Auto-select project heading as initial node (after window setup)
(when-let* ((graph org-gtd-graph-view--graph)
            (project-id (org-gtd-graph-project-id graph)))
  (org-gtd-graph-ui-select-node project-id t))  ; t = no history
```

**Step 2: Update to use new function**

Replace with:
```elisp
;; Auto-select first actionable task (or project heading if none)
(when org-gtd-graph-view--graph
  (let ((initial-node (or (org-gtd-graph-data-find-first-actionable
                           org-gtd-graph-view--graph)
                          (org-gtd-graph-project-id org-gtd-graph-view--graph))))
    (org-gtd-graph-ui-select-node initial-node t)))  ; t = no history
```

**Step 3: Run tests to verify no regression**

Run: `~/.local/bin/eldev test -B`
Expected: All tests PASS

**Step 4: Commit**

```bash
git add org-gtd-graph-view.el
git commit -m "feat: auto-select first actionable task in graph view"
```

---

## Task 15: Final Verification and Cleanup

**Step 1: Run full test suite**

Run: `~/.local/bin/eldev test -B`
Expected: All tests PASS

**Step 2: Compile to check for warnings**

Run: `~/.local/bin/eldev clean && ~/.local/bin/eldev compile`
Expected: No errors or warnings

**Step 3: Manual test (optional)**

1. Open a project with multiple tasks (some NEXT, some TODO)
2. Run `M-x org-gtd-show-project-graph`
3. Verify the first NEXT task is selected, not the project heading

**Step 4: Final commit if needed**

```bash
git add -A
git commit -m "chore: cleanup after graph initial selection feature"
```

---

## Success Criteria

1. All existing tests pass (714+ tests)
2. All 11 new tests pass
3. Graph opens with first actionable task (NEXT/WAIT) selected
4. Falls back to project heading when no actionable tasks
5. BFS order ensures closest actionable task is found first
6. No compilation warnings
