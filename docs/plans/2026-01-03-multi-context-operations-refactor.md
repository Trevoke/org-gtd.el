# Multi-Context Operations Refactor Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor project operations to work cleanly from org buffers, agenda, and graph view with proper transient state management.

**Architecture:** Create a layered architecture with (1) internal pure functions, (2) shared context resolution, (3) clean transient state via `:scope`, (4) context-specific UX variants, and (5) DWIM dispatcher commands. Eliminate global variables, use buffer-local with cleanup for mutable state.

**Tech Stack:** Emacs Lisp, transient.el, org-mode, org-agenda

---

## Background

### The Problem

Current implementation uses dynamic `let` bindings to pass `org-gtd-graph-view--project-marker` to transient operations. This breaks because:

1. User calls command from agenda
2. `let` binds the marker
3. Transient UI displays, function returns
4. `let` binding unwinds
5. User interacts with transient
6. Transient suffix tries to access marker → nil → ERROR

Additionally, state is passed via global `defvar`s that are never cleaned up.

### The Solution

1. **Context struct** - Single source of truth for "where am I"
2. **Transient `:scope`** - Pass immutable context properly
3. **Buffer-local with cleanup** - For mutable checkbox state
4. **UX variants** - Simple prompts for org/agenda, transient for graph view
5. **DWIM commands** - Dispatch based on detected context

---

## Task 1: Create Context Resolution Infrastructure

**Files:**
- Create: `org-gtd-context.el`
- Test: `test/unit/context-test.el`

### Step 1: Write failing tests for context struct

```elisp
;;; context-test.el --- Tests for org-gtd-context -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-context)

(e-unit-initialize)

(deftest context/struct-accessors-work ()
  "Context struct has all expected accessors."
  (let ((ctx (org-gtd-context-create
              :mode 'graph-view
              :project-marker (point-marker)
              :project-id "proj-123"
              :task-id "task-456"
              :task-marker nil)))
    (assert-equal 'graph-view (org-gtd-context-mode ctx))
    (assert-equal "proj-123" (org-gtd-context-project-id ctx))
    (assert-equal "task-456" (org-gtd-context-task-id ctx))))

(deftest context/from-graph-view-requires-marker ()
  "Context from graph view errors without project marker."
  (with-temp-buffer
    (setq major-mode 'org-gtd-graph-view-mode)
    (setq-local org-gtd-graph-view--project-marker nil)
    (assert-raises 'user-error
      (org-gtd-context-at-point))))

(provide 'context-test)
;;; context-test.el ends here
```

### Step 2: Run test to verify it fails

Run: `~/.local/bin/eldev etest 2>&1 | grep -E "context/|Cannot open load file"`
Expected: FAIL with "Cannot open load file: org-gtd-context"

### Step 3: Create org-gtd-context.el with struct definition

```elisp
;;; org-gtd-context.el --- Context resolution for multi-context operations -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Provides context resolution for operations that work across
;; org buffers, agenda, and graph view.
;;
;;; Code:

;;;; Requirements

(require 'cl-lib)

;;;; External Variable Declarations

(defvar org-gtd-graph-view--project-marker)
(defvar org-gtd-graph-ui--selected-node-id)

;;;; Context Struct

(cl-defstruct (org-gtd-context
               (:constructor org-gtd-context-create)
               (:copier nil))
  "Context for multi-context project operations.

Fields:
  mode           - Symbol: \\='graph-view, \\='agenda, or \\='org
  project-marker - Marker to project heading
  project-id     - ID string of project
  task-id        - ID string of task at point (nil if on project heading)
  task-marker    - Marker to task (nil if on project heading)"
  mode
  project-marker
  project-id
  task-id
  task-marker)

;;;; Footer

(provide 'org-gtd-context)

;;; org-gtd-context.el ends here
```

### Step 4: Run test to verify struct works

Run: `~/.local/bin/eldev etest 2>&1 | grep -E "context/struct"`
Expected: PASS

### Step 5: Commit

```bash
git add org-gtd-context.el test/unit/context-test.el
git commit -m "feat(context): add org-gtd-context struct for multi-context operations"
```

---

## Task 2: Implement Context Resolution Functions

**Files:**
- Modify: `org-gtd-context.el`
- Test: `test/unit/context-test.el`

### Step 1: Add tests for context resolution

```elisp
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest context/from-graph-view-returns-context ()
  "Context from graph view populates all fields."
  (let* ((result (ogt-create-project-with-task "Test Project" "Test Task"))
         (project-marker (car result))
         (task-id (cdr result)))
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)
      (setq-local org-gtd-graph-ui--selected-node-id task-id)
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'graph-view (org-gtd-context-mode ctx))
        (assert-true (markerp (org-gtd-context-project-marker ctx)))
        (assert-equal task-id (org-gtd-context-task-id ctx))))))

(deftest context/from-agenda-returns-context ()
  "Context from agenda resolves project from task."
  (let* ((result (ogt-create-project-with-task "Agenda Project" "Agenda Task"))
         (project-marker (car result))
         (task-id (cdr result))
         (task-marker (org-id-find task-id t)))
    (with-temp-buffer
      (org-agenda-mode)
      (insert "  agenda:  Agenda Task")
      (put-text-property (point-min) (point-max) 'org-marker task-marker)
      (goto-char (point-min))
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'agenda (org-gtd-context-mode ctx))
        (assert-equal task-id (org-gtd-context-task-id ctx))))))

(deftest context/from-org-buffer-on-task ()
  "Context from org buffer on task heading."
  (let* ((result (ogt-create-project-with-task "Org Project" "Org Task"))
         (task-id (cdr result))
         (task-marker (org-id-find task-id t)))
    (org-with-point-at task-marker
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'org (org-gtd-context-mode ctx))
        (assert-equal task-id (org-gtd-context-task-id ctx))))))

(deftest context/from-org-buffer-on-project-heading ()
  "Context from org buffer on project heading has nil task-id."
  (let* ((result (ogt-create-project-with-task "Direct Project" "Direct Task"))
         (project-marker (car result)))
    (org-with-point-at project-marker
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'org (org-gtd-context-mode ctx))
        (assert-nil (org-gtd-context-task-id ctx))))))

(deftest context/errors-in-invalid-buffer ()
  "Context resolution errors in non-supported buffer."
  (with-temp-buffer
    (fundamental-mode)
    (assert-raises 'user-error
      (org-gtd-context-at-point))))
```

### Step 2: Run tests to verify they fail

Run: `~/.local/bin/eldev etest 2>&1 | grep -E "context/from"`
Expected: FAIL (functions not defined)

### Step 3: Implement context resolution functions

Add to `org-gtd-context.el` before footer:

```elisp
;;;; Context Resolution

(declare-function org-gtd-project--get-marker-at-point "org-gtd-projects")

(defun org-gtd-context-at-point ()
  "Resolve current context for multi-context operations.
Returns `org-gtd-context' struct with mode, markers, and IDs.
Signals `user-error' if not in a supported context."
  (cond
   ((eq major-mode 'org-gtd-graph-view-mode)
    (org-gtd-context--from-graph-view))
   ((derived-mode-p 'org-agenda-mode)
    (org-gtd-context--from-agenda))
   ((derived-mode-p 'org-mode)
    (org-gtd-context--from-org-buffer))
   (t
    (user-error "Must be in org buffer, agenda, or graph view"))))

(defun org-gtd-context--from-graph-view ()
  "Build context from graph view buffer-locals."
  (unless (bound-and-true-p org-gtd-graph-view--project-marker)
    (user-error "No project in graph view"))
  (let* ((project-marker org-gtd-graph-view--project-marker)
         (project-id (org-with-point-at project-marker (org-id-get)))
         (selected-id (bound-and-true-p org-gtd-graph-ui--selected-node-id))
         ;; Task ID is nil if selected node is the project itself
         (task-id (unless (equal selected-id project-id) selected-id))
         (task-marker (when task-id (org-id-find task-id t))))
    (org-gtd-context-create
     :mode 'graph-view
     :project-marker project-marker
     :project-id project-id
     :task-id task-id
     :task-marker task-marker)))

(defun org-gtd-context--from-agenda ()
  "Build context from agenda item at point."
  (let ((item-marker (org-get-at-bol 'org-marker)))
    (unless item-marker
      (user-error "No item at agenda point"))
    (let* ((task-id (org-with-point-at item-marker (org-id-get)))
           (project-marker (org-with-point-at item-marker
                             (require 'org-gtd-projects)
                             (org-gtd-project--get-marker-at-point)))
           (project-id (org-with-point-at project-marker (org-id-get))))
      (org-gtd-context-create
       :mode 'agenda
       :project-marker project-marker
       :project-id project-id
       :task-id task-id
       :task-marker item-marker))))

(defun org-gtd-context--from-org-buffer ()
  "Build context from org heading at point."
  (org-back-to-heading t)
  (let* ((here-id (org-id-get))
         (org-gtd-val (org-entry-get (point) "ORG_GTD"))
         (on-project-heading (equal org-gtd-val "Projects"))
         (project-marker (if on-project-heading
                             (point-marker)
                           (require 'org-gtd-projects)
                           (org-gtd-project--get-marker-at-point)))
         (project-id (org-with-point-at project-marker (org-id-get))))
    (org-gtd-context-create
     :mode 'org
     :project-marker project-marker
     :project-id project-id
     :task-id (unless on-project-heading here-id)
     :task-marker (unless on-project-heading (point-marker)))))
```

### Step 4: Run tests to verify they pass

Run: `~/.local/bin/eldev etest 2>&1 | grep -E "context/"`
Expected: All context tests PASS

### Step 5: Commit

```bash
git add org-gtd-context.el test/unit/context-test.el
git commit -m "feat(context): implement context resolution for graph-view, agenda, org"
```

---

## Task 3: Add Context Module to Build System

**Files:**
- Modify: `org-gtd.el` (add require)
- Modify: `Eldev` (if needed for load order)

### Step 1: Add require to org-gtd.el

Find the requires section in `org-gtd.el` and add:

```elisp
(require 'org-gtd-context)
```

### Step 2: Run full test suite

Run: `~/.local/bin/eldev etest 2>&1 | tail -5`
Expected: All tests pass

### Step 3: Run compile to check for warnings

Run: `~/.local/bin/eldev compile 2>&1 | grep -i warning`
Expected: No new warnings

### Step 4: Commit

```bash
git add org-gtd.el
git commit -m "build: add org-gtd-context to package requires"
```

---

## Task 4: Refactor Transient State to Use :scope

**Files:**
- Modify: `org-gtd-graph-transient.el`
- Test: `test/unit/graph-transient-test.el` (create if needed)

### Step 1: Create buffer-local for edge selection with cleanup

Replace the global defvars in `org-gtd-graph-transient.el`:

```elisp
;; REMOVE these lines (around line 864-871):
;; (defvar org-gtd-graph--add-task-id nil ...)
;; (defvar org-gtd-graph--add-task-title nil ...)
;; (defvar org-gtd-graph--add-edge-state nil ...)

;; ADD this buffer-local:
(defvar-local org-gtd-graph--edge-selection nil
  "Alist of (TASK-ID . SELECTED-P) for multi-select operations.
Buffer-local to graph view, cleaned up after operation.")

(defun org-gtd-graph--cleanup-edge-selection ()
  "Clean up edge selection state after operation."
  (when (eq major-mode 'org-gtd-graph-view-mode)
    (setq org-gtd-graph--edge-selection nil)))
```

### Step 2: Update org-gtd-graph-add-successor to use :scope

Rewrite the function to build scope and pass to transient:

```elisp
(defun org-gtd-graph-add-successor ()
  "Add a successor task that blocks project task(s).
Step 1: Select existing task or create new one.
Step 2: Select which project tasks the new task should block."
  (interactive)
  (require 'org-gtd-graph-view)
  (require 'org-gtd-graph-data)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (unless (eq (org-gtd-context-mode ctx) 'graph-view)
      (user-error "This command requires graph view for multi-select"))

    ;; Step 1: Select or create task
    (let* ((choices (org-gtd-graph--select-or-create-task-prioritizing-current
                     "Select successor task: "
                     (org-gtd-context-project-marker ctx)))
           (selected (completing-read "Select or create successor task: " choices nil nil))
           (match (assoc-string selected choices))
           (existing-id (when match (cdr match)))
           (title (if match selected selected)))

      ;; Build edge selection state (buffer-local)
      (setq org-gtd-graph--edge-selection
            (org-gtd-graph--build-predecessor-selection ctx))

      ;; Invoke transient with scope
      (transient-setup 'org-gtd-graph-add-successor-menu nil nil
                       :scope (list :ctx ctx
                                    :task-id existing-id
                                    :task-title title)))))

(defun org-gtd-graph--build-predecessor-selection (ctx)
  "Build edge selection alist for predecessor selection.
Pre-selects the currently selected task if any."
  (let* ((project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (graph (org-gtd-graph-data--extract-from-project project-marker))
         (all-task-ids (hash-table-keys (org-gtd-graph-nodes graph)))
         (valid-candidates (seq-remove (lambda (id) (string= id project-id)) all-task-ids))
         (selected-id (org-gtd-context-task-id ctx)))
    (mapcar (lambda (task-id)
              (cons task-id (and selected-id (string= task-id selected-id))))
            valid-candidates)))
```

### Step 3: Update the transient menu to read from scope

```elisp
(transient-define-prefix org-gtd-graph-add-successor-menu ()
  "Select which tasks should block the new successor (predecessors)."
  :refresh-suffixes t
  [:description
   (lambda ()
     (let* ((scope (oref transient-current-prefix scope))
            (title (plist-get scope :task-title)))
       (format "Adding successor: %s\nSelect predecessors (tasks that must complete first):"
               (propertize title 'face 'bold))))
   :class transient-columns
   :setup-children org-gtd-graph--add-successor-setup-children]
  [["Actions"
    ("RET" "Confirm" org-gtd-graph--add-successor-apply)
    ("q" "Cancel" transient-quit-one)]])
```

### Step 4: Update apply function to read from scope

```elisp
(defun org-gtd-graph--add-successor-apply ()
  "Apply add-successor using transient scope."
  (interactive)
  (let* ((scope (oref transient-current-prefix scope))
         (ctx (plist-get scope :ctx))
         (task-id (plist-get scope :task-id))
         (task-title (plist-get scope :task-title))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx))
         (predecessor-ids (mapcar #'car (seq-filter #'cdr org-gtd-graph--edge-selection)))
         (new-task-id task-id))

    ;; Create new task if needed
    (unless new-task-id
      (org-with-point-at project-marker
        (org-end-of-subtree t t)
        (unless (bolp) (insert "\n"))
        (insert "** " task-title "\n")
        (forward-line -1)
        (org-back-to-heading t)
        (setq new-task-id (org-id-get-create))
        (org-todo "TODO")
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (save-buffer)))

    ;; Create dependencies
    (dolist (predecessor-id predecessor-ids)
      (org-gtd-dependencies-create predecessor-id new-task-id))

    ;; Link existing external task to project
    (when task-id
      (org-gtd-add-to-multivalued-property new-task-id "ORG_GTD_PROJECT_IDS" project-id)
      (org-with-point-at (org-id-find new-task-id t)
        (save-buffer)))

    ;; Cleanup and refresh
    (org-gtd-graph--cleanup-edge-selection)
    (message "Added successor '%s' after %d task(s)" task-title (length predecessor-ids))
    (org-gtd-projects-fix-todo-keywords project-marker)
    (org-gtd-graph-view-refresh)
    (transient-quit-one)))
```

### Step 5: Run tests

Run: `~/.local/bin/eldev etest 2>&1 | tail -5`
Expected: All tests pass

### Step 6: Commit

```bash
git add org-gtd-graph-transient.el
git commit -m "refactor(transient): use :scope instead of global variables for add-successor"
```

---

## Task 5: Apply Same Pattern to Add-Blocker

**Files:**
- Modify: `org-gtd-graph-transient.el`

### Step 1: Update org-gtd-graph-add-blocker to use :scope

Follow the same pattern as add-successor:
- Build context with `org-gtd-context-at-point`
- Build edge selection for blocked tasks
- Pass scope to transient
- Apply function reads from scope
- Cleanup after

### Step 2: Update apply function

Similar to add-successor-apply but creates blocker dependencies.

### Step 3: Run tests

Run: `~/.local/bin/eldev etest 2>&1 | tail -5`
Expected: All tests pass

### Step 4: Commit

```bash
git add org-gtd-graph-transient.el
git commit -m "refactor(transient): use :scope for add-blocker"
```

---

## Task 6: Apply Pattern to Modify-Blockers and Modify-Successors

**Files:**
- Modify: `org-gtd-graph-transient.el`

Same pattern as Tasks 4-5 for:
- `org-gtd-graph-modify-blockers`
- `org-gtd-graph-modify-successors`

Commit after each.

---

## Task 7: Create Simple UX Functions for Org/Agenda

**Files:**
- Create: `org-gtd-project-operations.el`
- Test: `test/unit/project-operations-test.el`

### Step 1: Write tests for simple UX

```elisp
(deftest project-ops/add-successor-simple-creates-task ()
  "Simple add-successor creates task with dependency."
  (let* ((result (ogt-create-project-with-task "Simple Project" "Predecessor"))
         (project-marker (car result))
         (predecessor-id (cdr result)))
    (org-with-point-at (org-id-find predecessor-id t)
      (with-simulated-input "New Successor RET"
        (org-gtd-add-successor--simple (org-gtd-context-at-point))))
    ;; Verify task created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "New Successor" nil t)))))
```

### Step 2: Implement simple UX functions

```elisp
(defun org-gtd-add-successor--simple (ctx)
  "Simple prompt-based UX for adding successor from org/agenda.
Uses task at point as the predecessor."
  (let* ((predecessor-id (org-gtd-context-task-id ctx))
         (project-marker (org-gtd-context-project-marker ctx))
         (project-id (org-gtd-context-project-id ctx)))
    (unless predecessor-id
      (user-error "No task at point to add successor to"))
    (let ((title (read-string "Successor task title: ")))
      (when (string-empty-p title)
        (user-error "Task title required"))
      (org-gtd-project--add-successor-internal
       title (list predecessor-id) project-marker)
      (message "Added successor: %s" title))))

(defun org-gtd-add-blocker--simple (ctx)
  "Simple prompt-based UX for adding blocker from org/agenda.
Creates new task that blocks the task at point."
  (let* ((blocked-id (org-gtd-context-task-id ctx))
         (project-marker (org-gtd-context-project-marker ctx)))
    (unless blocked-id
      (user-error "No task at point to add blocker for"))
    (let ((title (read-string "Blocker task title: ")))
      (when (string-empty-p title)
        (user-error "Task title required"))
      (org-gtd-project--add-blocker-internal
       title (list blocked-id) project-marker)
      (message "Added blocker: %s" title))))

(defun org-gtd-add-root--simple (ctx)
  "Simple prompt-based UX for adding root task from org/agenda."
  (let* ((project-marker (org-gtd-context-project-marker ctx)))
    (let ((title (read-string "Root task title: ")))
      (when (string-empty-p title)
        (user-error "Task title required"))
      (org-gtd-project--add-root-internal title project-marker)
      (message "Added root task: %s" title))))
```

### Step 3: Run tests, commit

---

## Task 8: Create DWIM Dispatcher Commands

**Files:**
- Modify: `org-gtd-projects.el`

### Step 1: Refactor existing commands to use DWIM pattern

```elisp
;;;###autoload
(defun org-gtd-project-add-successor ()
  "Add a successor task to current project.
Works from org buffer, agenda, or graph view.

In graph view: opens multi-select transient for choosing predecessors.
In org/agenda: task at point becomes the predecessor, prompts for title."
  (interactive)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (pcase (org-gtd-context-mode ctx)
      ('graph-view
       (require 'org-gtd-graph-transient)
       (org-gtd-graph-add-successor))
      (_
       (org-gtd-add-successor--simple ctx)))))

;;;###autoload
(defun org-gtd-project-add-blocker ()
  "Add a blocker task to current project.
Works from org buffer, agenda, or graph view."
  (interactive)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (pcase (org-gtd-context-mode ctx)
      ('graph-view
       (require 'org-gtd-graph-transient)
       (org-gtd-graph-add-blocker))
      (_
       (org-gtd-add-blocker--simple ctx)))))

;;;###autoload
(defun org-gtd-project-add-root-task ()
  "Add a root task (no dependencies) to current project.
Works from org buffer, agenda, or graph view."
  (interactive)
  (require 'org-gtd-context)
  (let ((ctx (org-gtd-context-at-point)))
    (pcase (org-gtd-context-mode ctx)
      ('graph-view
       (require 'org-gtd-graph-transient)
       (org-gtd-graph-transient-add-root))
      (_
       (org-gtd-add-root--simple ctx)))))
```

### Step 2: Run full test suite

Run: `~/.local/bin/eldev etest 2>&1 | tail -5`
Expected: All tests pass

### Step 3: Commit

```bash
git add org-gtd-projects.el
git commit -m "refactor(projects): DWIM dispatchers for add-successor/blocker/root"
```

---

## Task 9: Apply Pattern to Remaining Operations

**Files:**
- Modify: `org-gtd-graph-transient.el`
- Modify: `org-gtd-projects.el`

Apply the same DWIM pattern to:
- Remove task from project
- Trash task
- Change TODO state
- Incubate project
- Cancel project

Each needs:
1. Simple UX function (for org/agenda)
2. DWIM dispatcher
3. Graph-specific function uses context properly

---

## Task 10: Remove Old Global Variables

**Files:**
- Modify: `org-gtd-graph-transient.el`

### Step 1: Search for remaining uses of old globals

Run: `grep -n "org-gtd-graph--add-task-id\|org-gtd-graph--add-task-title\|org-gtd-graph--add-edge-state" org-gtd-graph-transient.el`

### Step 2: Remove any remaining references

Ensure all code paths use scope or buffer-local.

### Step 3: Remove defvar declarations

Delete the old global defvars.

### Step 4: Run full test suite

Run: `~/.local/bin/eldev etest 2>&1 | tail -5`
Expected: All tests pass

### Step 5: Commit

```bash
git add org-gtd-graph-transient.el
git commit -m "refactor(transient): remove global state variables"
```

---

## Task 11: Update Documentation

**Files:**
- Modify: `doc/org-gtd.org`

Update any documentation that references the old command structure.

---

## Task 12: Manual Testing

Follow the manual testing script from earlier conversation:
1. Test add-successor from agenda → stays in agenda
2. Test add-blocker from agenda → stays in agenda
3. Test add-root from agenda → stays in agenda
4. Test all operations from graph view → transient works
5. Test all operations from org buffer → simple prompts work

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Create context struct | org-gtd-context.el |
| 2 | Implement context resolution | org-gtd-context.el |
| 3 | Add to build system | org-gtd.el |
| 4 | Refactor add-successor transient | org-gtd-graph-transient.el |
| 5 | Refactor add-blocker transient | org-gtd-graph-transient.el |
| 6 | Refactor modify-blockers/successors | org-gtd-graph-transient.el |
| 7 | Create simple UX functions | org-gtd-project-operations.el |
| 8 | Create DWIM dispatchers | org-gtd-projects.el |
| 9 | Apply to remaining operations | multiple |
| 10 | Remove old globals | org-gtd-graph-transient.el |
| 11 | Update docs | doc/org-gtd.org |
| 12 | Manual testing | - |

**Estimated commits:** 12-15
