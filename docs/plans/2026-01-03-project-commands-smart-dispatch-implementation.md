# Project Commands Smart Dispatch Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable project operations (add-successor, add-blocker, add-root, incubate, cancel) to work from both graph view and agenda contexts via smart dispatching.

**Architecture:** Core commands live in `org-gtd-projects.el` using shared helpers for context detection and project marker resolution. UI wrappers in `org-gtd-graph-transient.el` handle graph-specific concerns (refresh/quit). No circular dependencies via `bound-and-true-p` for graph view detection.

**Tech Stack:** Emacs Lisp, org-mode, e-unit test framework, with-simulated-input for interactive testing.

---

## Task 1: Helper Function - Get Project Marker at Point

**Files:**
- Modify: `org-gtd-projects.el`
- Test: `test/unit/project-context-test.el` (create)

**Step 1: Write the failing test for single-project case**

Create `test/unit/project-context-test.el`:

```elisp
;;; project-context-test.el --- Tests for project context helpers -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-projects)

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

(deftest project-context/get-marker-at-point-single-project ()
  "Returns project marker when task belongs to single project."
  (let (project-id task-marker)
    ;; Create project with task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Test Project\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")
      (org-end-of-subtree t t)
      (insert "** TODO Task 1\n")
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (setq task-marker (point-marker)))
      (basic-save-buffer))

    ;; At task point, should get project marker
    (org-with-point-at task-marker
      (let ((result (org-gtd-project--get-marker-at-point)))
        (assert-true (markerp result))
        (org-with-point-at result
          (assert-equal "Test Project" (org-get-heading t t t t)))))))

(provide 'project-context-test)
;;; project-context-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest "project-context/get-marker-at-point-single-project"`
Expected: FAIL with "org-gtd-project--get-marker-at-point" not defined

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` (after the requires, before existing functions):

```elisp
;;;; Context Helpers

(defun org-gtd-project--get-marker-at-point (&optional prompt)
  "Get project marker for task at point.
If task belongs to multiple projects, prompt user to choose.
PROMPT is the completing-read prompt (default: \"Which project? \").
Returns project marker, or signals error if not a project task."
  (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
    (cond
     ;; No projects - error
     ((null project-ids)
      (user-error "This is not a project task"))
     ;; Single project - use it
     ((= (length project-ids) 1)
      (or (org-id-find (car project-ids) t)
          (user-error "Cannot find project with ID: %s" (car project-ids))))
     ;; Multiple projects - prompt
     (t
      (let* ((prompt-text (or prompt "Which project? "))
             (project-names
              (mapcar (lambda (id)
                        (cons (org-with-point-at (org-id-find id t)
                                (org-get-heading t t t t))
                              id))
                      project-ids))
             (chosen-name (completing-read prompt-text
                                           (mapcar #'car project-names)
                                           nil t))
             (chosen-id (cdr (assoc chosen-name project-names))))
        (or (org-id-find chosen-id t)
            (user-error "Cannot find project with ID: %s" chosen-id)))))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest "project-context/get-marker-at-point-single-project"`
Expected: PASS

**Step 5: Commit**

```bash
git add test/unit/project-context-test.el org-gtd-projects.el
git commit -m "feat: add org-gtd-project--get-marker-at-point helper"
```

---

## Task 2: Helper Function - Multi-Project Prompt

**Files:**
- Modify: `org-gtd-projects.el` (already modified)
- Modify: `test/unit/project-context-test.el`

**Step 1: Write failing test for multi-project prompt**

Add to `test/unit/project-context-test.el`:

```elisp
(deftest project-context/get-marker-at-point-multi-project-prompts ()
  "Prompts user when task belongs to multiple projects."
  (let (project-a-id project-b-id task-marker)
    ;; Create two projects
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Project Alpha\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")

      (goto-char (point-max))
      (insert "* Project Beta\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-b-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")

      ;; Create task belonging to both
      (goto-char (point-max))
      (insert "* TODO Shared Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" (concat project-a-id " " project-b-id))
        (setq task-marker (point-marker)))
      (basic-save-buffer))

    ;; At task point, should prompt and return chosen project
    (org-with-point-at task-marker
      (with-simulated-input "Project SPC Beta RET"
        (let ((result (org-gtd-project--get-marker-at-point)))
          (assert-true (markerp result))
          (org-with-point-at result
            (assert-equal "Project Beta" (org-get-heading t t t t))))))))
```

**Step 2: Run test to verify it passes (already implemented)**

Run: `~/.local/bin/eldev etest "project-context/get-marker-at-point-multi-project"`
Expected: PASS

**Step 3: Write failing test for error case**

Add to `test/unit/project-context-test.el`:

```elisp
(deftest project-context/get-marker-at-point-errors-on-non-project-task ()
  "Errors when task has no project IDs."
  (let (task-marker)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* TODO Single Action\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      ;; No ORG_GTD_PROJECT_IDS
      (setq task-marker (point-marker))
      (basic-save-buffer))

    (org-with-point-at task-marker
      (assert-raises 'user-error
        (org-gtd-project--get-marker-at-point)))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest "project-context/get-marker-at-point-errors"`
Expected: PASS

**Step 5: Commit**

```bash
git add test/unit/project-context-test.el
git commit -m "test: add multi-project and error case tests for get-marker-at-point"
```

---

## Task 3: Helper Function - Get Project Marker from Context

**Files:**
- Modify: `org-gtd-projects.el`
- Modify: `test/unit/project-context-test.el`

**Step 1: Write failing test for graph view context**

Add to `test/unit/project-context-test.el`:

```elisp
(deftest project-context/get-marker-from-context-graph-view ()
  "Returns buffer-local marker when in graph-view-mode."
  (let (project-marker)
    ;; Create a project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Graph Project\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Projects")
      (setq project-marker (point-marker))
      (basic-save-buffer))

    ;; Simulate being in graph view
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)
      (let ((result (org-gtd-project--get-marker-from-context)))
        (assert-eq project-marker result)))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest "project-context/get-marker-from-context-graph-view"`
Expected: FAIL with "org-gtd-project--get-marker-from-context" not defined

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` after `org-gtd-project--get-marker-at-point`:

```elisp
(defun org-gtd-project--get-marker-from-context (&optional prompt)
  "Get project marker from current context.
In graph-view-mode: return buffer-local project marker.
In agenda-mode: get marker from agenda item, then resolve project.
Otherwise: signal error.
PROMPT is passed to `org-gtd-project--get-marker-at-point' for multi-project selection."
  (cond
   ;; Graph view - use buffer-local marker
   ((and (eq major-mode 'org-gtd-graph-view-mode)
         (bound-and-true-p org-gtd-graph-view--project-marker))
    org-gtd-graph-view--project-marker)

   ;; Agenda - get from agenda item
   ((derived-mode-p 'org-agenda-mode)
    (org-gtd-project--get-marker-from-agenda-item prompt))

   (t (user-error "Must be in graph view or agenda"))))

(defun org-gtd-project--get-marker-from-agenda-item (&optional prompt)
  "Get project marker from agenda item at point.
PROMPT is passed to `org-gtd-project--get-marker-at-point'."
  (let ((marker (org-get-at-bol 'org-marker)))
    (unless marker
      (user-error "No task at point"))
    (org-with-point-at marker
      (org-gtd-project--get-marker-at-point prompt))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest "project-context/get-marker-from-context-graph-view"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/unit/project-context-test.el
git commit -m "feat: add org-gtd-project--get-marker-from-context helper"
```

---

## Task 4: Context Helper - Agenda Mode Test

**Files:**
- Modify: `test/unit/project-context-test.el`

**Step 1: Write failing test for agenda context**

Add to `test/unit/project-context-test.el`:

```elisp
(deftest project-context/get-marker-from-context-agenda ()
  "Returns project marker when called from agenda on project task."
  (let (project-id)
    ;; Create project with task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Agenda Project\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")
      (org-end-of-subtree t t)
      (insert "** NEXT Agenda Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (basic-save-buffer))

    ;; Open agenda and navigate to task
    (org-gtd-engage)
    (with-current-buffer org-agenda-buffer
      (goto-char (point-min))
      (search-forward "Agenda Task")
      (let ((result (org-gtd-project--get-marker-from-context)))
        (assert-true (markerp result))
        (org-with-point-at result
          (assert-equal "Agenda Project" (org-get-heading t t t t)))))))
```

**Step 2: Run test to verify it passes**

Run: `~/.local/bin/eldev etest "project-context/get-marker-from-context-agenda"`
Expected: PASS (implementation already handles this)

**Step 3: Write failing test for error in wrong context**

Add to `test/unit/project-context-test.el`:

```elisp
(deftest project-context/get-marker-from-context-errors-outside-valid-context ()
  "Errors when not in graph view or agenda."
  (with-temp-buffer
    (fundamental-mode)
    (assert-raises 'user-error
      (org-gtd-project--get-marker-from-context))))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest "project-context/get-marker-from-context-errors"`
Expected: PASS

**Step 5: Commit**

```bash
git add test/unit/project-context-test.el
git commit -m "test: add agenda and error context tests"
```

---

## Task 5: Core Command - org-gtd-project-task-add-successor

**Files:**
- Modify: `org-gtd-projects.el`
- Create: `test/unit/project-task-commands-test.el`

**Step 1: Write failing test**

Create `test/unit/project-task-commands-test.el`:

```elisp
;;; project-task-commands-test.el --- Tests for project task commands -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-projects)
(require 'org-gtd-graph-data)
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

(defun ogt-create-project-with-task (project-name task-name)
  "Create project with one task. Return (project-marker . task-id)."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* %s\n" project-name))
    (forward-line -1)
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create))
          (project-marker (point-marker)))
      (org-entry-put (point) "ORG_GTD" "Projects")
      (org-end-of-subtree t t)
      (insert (format "** NEXT %s\n" task-name))
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (org-entry-add-to-multivalued-property project-marker "ORG_GTD_FIRST_TASKS" task-id)
        (basic-save-buffer)
        (cons project-marker task-id)))))

(deftest project-task-commands/add-successor-from-graph-view ()
  "Add successor works from graph view context."
  (let* ((result (ogt-create-project-with-task "Successor Project" "First Task"))
         (project-marker (car result))
         (first-task-id (cdr result)))

    ;; Simulate graph view context
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)
      (setq-local org-gtd-graph-view--graph
                  (org-gtd-graph-data--extract-from-project project-marker))
      (setq-local org-gtd-graph-ui--selected-node-id first-task-id)

      ;; Add successor - select "New Successor" as new task, then select First Task as predecessor
      (with-simulated-input "New SPC Successor RET a RET"
        (org-gtd-project-task-add-successor)))

    ;; Verify successor was created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "New Successor" nil t)))))

(provide 'project-task-commands-test)
;;; project-task-commands-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest "project-task-commands/add-successor-from-graph-view"`
Expected: FAIL with function not defined

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el`:

```elisp
;;;; Project Task Commands

(defun org-gtd-project-task-add-successor ()
  "Add a successor task to the current project.
Works from graph view or agenda context.
Step 1: Select existing task or create new one.
Step 2: Select which project tasks the new task should block."
  (interactive)
  (require 'org-gtd-graph-data)
  (let ((project-marker (org-gtd-project--get-marker-from-context)))
    (org-gtd-project-task--add-successor-impl project-marker)))

(defun org-gtd-project-task--add-successor-impl (project-marker)
  "Implementation for adding successor to project at PROJECT-MARKER."
  ;; This delegates to the existing transient implementation
  ;; We'll refactor the transient code to use this in a later task
  (let ((org-gtd-graph-view--project-marker project-marker))
    (require 'org-gtd-graph-transient)
    (funcall 'org-gtd-graph-add-successor)))
```

Note: This is a temporary shim. We'll properly refactor in Task 9.

**Step 4: Run test to verify behavior**

Run: `~/.local/bin/eldev etest "project-task-commands/add-successor-from-graph-view"`
Expected: May need adjustment based on existing transient implementation

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/unit/project-task-commands-test.el
git commit -m "feat: add org-gtd-project-task-add-successor command (shim)"
```

---

## Task 6: Core Command - org-gtd-project-task-add-blocker

**Files:**
- Modify: `org-gtd-projects.el`
- Modify: `test/unit/project-task-commands-test.el`

**Step 1: Write failing test**

Add to `test/unit/project-task-commands-test.el`:

```elisp
(deftest project-task-commands/add-blocker-from-graph-view ()
  "Add blocker works from graph view context."
  (let* ((result (ogt-create-project-with-task "Blocker Project" "First Task"))
         (project-marker (car result))
         (first-task-id (cdr result)))

    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)
      (setq-local org-gtd-graph-view--graph
                  (org-gtd-graph-data--extract-from-project project-marker))
      (setq-local org-gtd-graph-ui--selected-node-id first-task-id)

      (with-simulated-input "New SPC Blocker RET a RET"
        (org-gtd-project-task-add-blocker)))

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "New Blocker" nil t)))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest "project-task-commands/add-blocker-from-graph-view"`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el`:

```elisp
(defun org-gtd-project-task-add-blocker ()
  "Add a blocker task to the current project.
Works from graph view or agenda context."
  (interactive)
  (require 'org-gtd-graph-data)
  (let ((project-marker (org-gtd-project--get-marker-from-context)))
    (org-gtd-project-task--add-blocker-impl project-marker)))

(defun org-gtd-project-task--add-blocker-impl (project-marker)
  "Implementation for adding blocker to project at PROJECT-MARKER."
  (let ((org-gtd-graph-view--project-marker project-marker))
    (require 'org-gtd-graph-transient)
    (funcall 'org-gtd-graph-add-blocker)))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest "project-task-commands/add-blocker-from-graph-view"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/unit/project-task-commands-test.el
git commit -m "feat: add org-gtd-project-task-add-blocker command"
```

---

## Task 7: Core Command - org-gtd-project-add-root-task

**Files:**
- Modify: `org-gtd-projects.el`
- Modify: `test/unit/project-task-commands-test.el`

**Step 1: Write failing test**

Add to `test/unit/project-task-commands-test.el`:

```elisp
(deftest project-task-commands/add-root-from-graph-view ()
  "Add root task works from graph view context."
  (let* ((result (ogt-create-project-with-task "Root Project" "First Task"))
         (project-marker (car result)))

    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)

      (with-simulated-input "New SPC Root RET"
        (org-gtd-project-add-root-task)))

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "New Root" nil t)))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest "project-task-commands/add-root-from-graph-view"`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el`:

```elisp
(defun org-gtd-project-add-root-task ()
  "Add a root task (no dependencies) to the current project.
Works from graph view or agenda context."
  (interactive)
  (let ((project-marker (org-gtd-project--get-marker-from-context)))
    (org-gtd-project--add-root-task-impl project-marker)))

(defun org-gtd-project--add-root-task-impl (project-marker)
  "Implementation for adding root task to project at PROJECT-MARKER."
  (let ((org-gtd-graph-view--project-marker project-marker))
    (require 'org-gtd-graph-transient)
    (funcall 'org-gtd-graph-transient-add-root)))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest "project-task-commands/add-root-from-graph-view"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/unit/project-task-commands-test.el
git commit -m "feat: add org-gtd-project-add-root-task command"
```

---

## Task 8: Core Command - org-gtd-project-incubate and org-gtd-project-cancel

**Files:**
- Modify: `org-gtd-projects.el`
- Modify: `test/unit/project-task-commands-test.el`

**Step 1: Write failing tests**

Add to `test/unit/project-task-commands-test.el`:

```elisp
(deftest project-task-commands/incubate-from-graph-view ()
  "Incubate project works from graph view context."
  (let* ((result (ogt-create-project-with-task "Incubate Project" "Task"))
         (project-marker (car result)))

    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)

      (with-simulated-input "2025-12-25 RET"
        (org-gtd-project-incubate)))

    ;; Verify project is incubated
    (org-with-point-at project-marker
      (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD")))))

(deftest project-task-commands/cancel-from-graph-view ()
  "Cancel project works from graph view context."
  (let* ((result (ogt-create-project-with-task "Cancel Project" "Task"))
         (project-marker (car result)))

    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)

      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Verify project is canceled
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))))
```

**Step 2: Run tests to verify they fail**

Run: `~/.local/bin/eldev etest "project-task-commands/incubate"`
Expected: FAIL

**Step 3: Write minimal implementations**

Add to `org-gtd-projects.el`:

```elisp
(defun org-gtd-project-incubate (&optional review-date)
  "Incubate the current project (put on tickler).
Works from graph view or agenda context.
REVIEW-DATE is the date to review, prompted if not provided."
  (interactive)
  (let ((project-marker (org-gtd-project--get-marker-from-context)))
    (let ((date (or review-date
                    (org-read-date nil nil nil "Review date: "))))
      (org-gtd-project-incubate project-marker date)
      (message "Project incubated until %s" date))))

(defun org-gtd-project-cancel ()
  "Cancel the current project.
Works from graph view or agenda context."
  (interactive)
  (let ((project-marker (org-gtd-project--get-marker-from-context)))
    (when (yes-or-no-p "Really cancel this project? ")
      (org-with-point-at project-marker
        (org-gtd-project-cancel))
      (message "Project canceled"))))
```

Note: These call existing functions. The naming collision with `org-gtd-project-cancel` (already exists) needs resolution - we may need to rename the existing one to `org-gtd-project-cancel--internal` or similar.

**Step 4: Run tests and adjust**

Run: `~/.local/bin/eldev etest "project-task-commands/incubate"`
Expected: Need to resolve naming

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/unit/project-task-commands-test.el
git commit -m "feat: add org-gtd-project-incubate and cancel commands"
```

---

## Task 9: Refactor Transient to Use Core Commands

**Files:**
- Modify: `org-gtd-graph-transient.el`

**Step 1: Update wrapper functions**

Replace the existing implementations with wrappers:

```elisp
(defun org-gtd-graph-add-successor ()
  "Add successor task from graph view."
  (interactive)
  (org-gtd-project-task-add-successor)
  (org-gtd-graph-view-refresh))

(defun org-gtd-graph-add-blocker ()
  "Add blocker task from graph view."
  (interactive)
  (org-gtd-project-task-add-blocker)
  (org-gtd-graph-view-refresh))

(defun org-gtd-graph-add-root ()
  "Add root task from graph view."
  (interactive)
  (org-gtd-project-add-root-task)
  (org-gtd-graph-view-refresh))

(defun org-gtd-graph-incubate-project ()
  "Incubate project from graph view."
  (interactive)
  (org-gtd-project-incubate)
  (quit-window))

(defun org-gtd-graph-cancel-project ()
  "Cancel project from graph view."
  (interactive)
  (org-gtd-project-cancel)
  (quit-window))
```

**Step 2: Run existing graph transient tests**

Run: `~/.local/bin/eldev etest "graph-transient"`
Expected: PASS

**Step 3: Commit**

```bash
git add org-gtd-graph-transient.el
git commit -m "refactor: graph transient uses core project commands"
```

---

## Task 10: Add Cancel Project to Keymap and Transient

**Files:**
- Modify: `org-gtd-graph-mode.el`
- Modify: `org-gtd-graph-transient.el`
- Modify: `test/unit/graph-mode-test.el`

**Step 1: Write failing test**

Add to `test/unit/graph-mode-test.el`:

```elisp
(deftest graph-mode/keymap-has-cancel-binding ()
  "Graph view mode keymap should have C for cancel project."
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "C"))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest "graph-mode/keymap-has-cancel-binding"`
Expected: FAIL

**Step 3: Add keybinding**

In `org-gtd-graph-mode.el`, add to keymap:

```elisp
(define-key map (kbd "C") #'org-gtd-graph-cancel-project)
```

In `org-gtd-graph-transient.el`, add to Project Operations group:

```elisp
("C" "Cancel project" org-gtd-graph-cancel-project :transient nil)
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest "graph-mode/keymap-has-cancel-binding"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-graph-mode.el org-gtd-graph-transient.el test/unit/graph-mode-test.el
git commit -m "feat: add C keybinding for cancel project in graph view"
```

---

## Task 11: Refactor Existing Code to Use Helpers

**Files:**
- Modify: `org-gtd-areas-of-focus.el`
- Modify: `org-gtd-tickler.el`
- Modify: `org-gtd-projects.el` (cancel-from-agenda)

**Step 1: Refactor areas-of-focus**

Replace lines 98-119 in `org-gtd-areas-of-focus.el`:

```elisp
(defun org-gtd-areas-of-focus--set-on-project-tasks ()
  "Set area of focus on all tasks in the project containing the task at point."
  (require 'org-gtd-projects)
  (let* ((project-marker (org-gtd-project--get-marker-at-point
                          "Which project to set area of focus on? "))
         (task-markers (org-gtd-projects--collect-tasks-by-graph project-marker))
         (chosen-area (completing-read
                       "Which area of focus does this project belong to? "
                       org-gtd-areas-of-focus
                       nil t)))
    (dolist (task-marker task-markers)
      (org-with-point-at task-marker
        (org-entry-put (point) "CATEGORY" chosen-area)))
    (org-with-point-at project-marker
      (org-entry-put (point) "CATEGORY" chosen-area)
      (save-buffer))))
```

**Step 2: Run existing tests**

Run: `~/.local/bin/eldev etest "areas-of-focus"`
Expected: PASS

**Step 3: Refactor tickler multi-project**

Update `org-gtd-tickler.el` lines 84-92 to use helper and prompt:

```elisp
(is-project-task
 (require 'org-gtd-projects)
 (let* ((project-marker (org-gtd-project--get-marker-at-point
                         "Which project to incubate? "))
        (review-date (or reminder-date
                         (org-read-date nil nil nil "Review date: "))))
   (org-gtd-project-incubate project-marker review-date)))
```

**Step 4: Run tickler tests**

Run: `~/.local/bin/eldev etest "tickler"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-areas-of-focus.el org-gtd-tickler.el org-gtd-projects.el
git commit -m "refactor: use shared project context helpers"
```

---

## Task 12: Integration Tests for Agenda Context

**Files:**
- Create: `test/integration/project-commands-agenda-test.el`

**Step 1: Write integration tests**

Create `test/integration/project-commands-agenda-test.el`:

```elisp
;;; project-commands-agenda-test.el --- Integration tests for project commands from agenda -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-projects)
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

(deftest project-commands-agenda/add-successor-from-agenda ()
  "Add successor works from agenda on project task."
  ;; Create project with task
  (let (project-id)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Agenda Test Project\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")
      (org-end-of-subtree t t)
      (insert "** NEXT Agenda Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (org-entry-add-to-multivalued-property
         (org-id-find project-id t) "ORG_GTD_FIRST_TASKS" task-id))
      (basic-save-buffer))

    ;; Open agenda, navigate to task, add successor
    (org-gtd-engage)
    (with-current-buffer org-agenda-buffer
      (goto-char (point-min))
      (search-forward "Agenda Task")
      (with-simulated-input "New SPC Successor RET a RET"
        (org-gtd-project-task-add-successor)))

    ;; Verify successor created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "New Successor" nil t)))))

(deftest project-commands-agenda/errors-on-non-project-task ()
  "Errors when trying to add successor on non-project task from agenda."
  ;; Create single action (not project task)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* NEXT Single Action\n")
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-entry-put (point) "ORG_GTD" "Actions")
    ;; No ORG_GTD_PROJECT_IDS
    (basic-save-buffer))

  (org-gtd-engage)
  (with-current-buffer org-agenda-buffer
    (goto-char (point-min))
    (search-forward "Single Action")
    (assert-raises 'user-error
      (org-gtd-project-task-add-successor))))

(provide 'project-commands-agenda-test)
;;; project-commands-agenda-test.el ends here
```

**Step 2: Run tests**

Run: `~/.local/bin/eldev etest "project-commands-agenda"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/integration/project-commands-agenda-test.el
git commit -m "test: add integration tests for project commands from agenda"
```

---

## Task 13: Update Documentation

**Files:**
- Modify: `doc/org-gtd.org`

**Step 1: Use writing-documentation skill**

Invoke skill: `@writing-documentation`

Update the following sections in `doc/org-gtd.org`:

1. **Graph View section**: Add `C` keybinding for cancel project
2. **Project Operations section**: Document that add-successor, add-blocker, add-root, incubate, and cancel now work from agenda
3. **Agenda Operations section**: Document the new smart dispatch behavior
4. **Keybinding reference**: Update tables with new bindings

Key additions:
- Explain that project task operations now work from both graph view and agenda
- Document the multi-project prompt behavior
- Add `C` to the keybinding table

**Step 2: Verify documentation builds**

Run: `makeinfo doc/org-gtd.org -o doc/org-gtd.info`
Expected: No errors

**Step 3: Commit**

```bash
git add doc/org-gtd.org
git commit -m "docs: update for smart dispatch project commands"
```

---

## Task 14: Final Verification

**Step 1: Run full test suite**

Run: `~/.local/bin/eldev etest`
Expected: All tests pass (or only pre-existing failures)

**Step 2: Compile with warnings**

Run: `~/.local/bin/eldev compile`
Expected: No new warnings

**Step 3: Manual testing checklist**

- [ ] `s` in graph view adds successor
- [ ] `s` in agenda on project task adds successor
- [ ] `s` in agenda on multi-project task prompts for project
- [ ] `s` in agenda on non-project task errors gracefully
- [ ] `b` works same as above for blocker
- [ ] `r` works same as above for root
- [ ] `I` incubates from both contexts
- [ ] `C` cancels from graph view
- [ ] Cancel from agenda works via project cancel command

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: complete smart dispatch for project commands"
```

---

## Summary

This plan implements:

1. **Shared helpers** in `org-gtd-projects.el`:
   - `org-gtd-project--get-marker-at-point`
   - `org-gtd-project--get-marker-from-context`

2. **Core commands** in `org-gtd-projects.el`:
   - `org-gtd-project-task-add-successor`
   - `org-gtd-project-task-add-blocker`
   - `org-gtd-project-add-root-task`
   - `org-gtd-project-incubate`
   - `org-gtd-project-cancel`

3. **UI wrappers** in `org-gtd-graph-transient.el` that call core + refresh

4. **New keybinding** `C` for cancel project

5. **Refactored code** in areas-of-focus, tickler using new helpers

6. **Documentation updates** via @writing-documentation skill
