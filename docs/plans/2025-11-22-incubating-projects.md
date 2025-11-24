# Incubating Projects Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add ability to pause/hibernate entire projects with state preservation and scheduled reactivation.

**Architecture:** Property-based incubation (ORG_GTD: Incubated) with state preservation (PREVIOUS_ORG_GTD, PREVIOUS_TODO), smart command dispatch, and natural view filtering via cleared TODO keywords.

**Tech Stack:** Emacs Lisp, org-mode, org-gtd (projects, incubate, view-language, graph-transient modules), Buttercup (testing)

**Design Doc:** See `docs/plans/2025-11-22-incubating-projects-design.md` for complete design rationale.

---

## Task 1: State Management - Save State Helper

**Files:**
- Modify: `org-gtd-projects.el` (add new function around line 650, after other project helpers)
- Test: `test/project-test.el` (add new describe block at end)

**Step 1: Write the failing test**

Add to `test/project-test.el` before final closing paren:

```elisp
 (describe "State preservation for incubation"
   (it "saves ORG_GTD and TODO state to PREVIOUS_* properties"
       (create-project "Test project")
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Task 1")
         (org-back-to-heading t)

         ;; Set up initial state
         (org-entry-put (point) "ORG_GTD" "Actions")
         (org-entry-put (point) "TODO" "NEXT")

         ;; Save state
         (org-gtd-project--save-state (point))

         ;; Verify state was saved
         (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Actions")
         (expect (org-entry-get (point) "PREVIOUS_TODO") :to-equal "NEXT")

         ;; Verify ORG_GTD changed to Incubated
         (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated")

         ;; Verify TODO keyword was cleared
         (expect (org-entry-get (point) "TODO") :to-be nil))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "saves ORG_GTD and TODO state"`

Expected: FAIL with "Symbol's function definition is void: org-gtd-project--save-state"

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` around line 650 (after other helper functions):

```elisp
(defun org-gtd-project--save-state (marker)
  "Save ORG_GTD and TODO state at MARKER to PREVIOUS_* properties for incubation.

Saves current ORG_GTD value to PREVIOUS_ORG_GTD property.
Saves current TODO keyword to PREVIOUS_TODO property.
Sets ORG_GTD to 'Incubated'.
Clears the TODO keyword."
  (org-with-point-at marker
    (let ((current-org-gtd (org-entry-get (point) "ORG_GTD"))
          (current-todo (org-entry-get (point) "TODO")))
      ;; Save current state
      (when current-org-gtd
        (org-entry-put (point) "PREVIOUS_ORG_GTD" current-org-gtd))
      (when current-todo
        (org-entry-put (point) "PREVIOUS_TODO" current-todo))
      ;; Set incubated state
      (org-entry-put (point) "ORG_GTD" "Incubated")
      ;; Clear TODO keyword
      (org-todo 'none))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "saves ORG_GTD and TODO state"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/project-test.el
git commit -m "feat: add state preservation helper for project incubation

Implements org-gtd-project--save-state which:
- Saves ORG_GTD to PREVIOUS_ORG_GTD property
- Saves TODO keyword to PREVIOUS_TODO property
- Sets ORG_GTD to Incubated
- Clears TODO keyword

This is foundational for incubating projects while preserving
their original state for later reactivation.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 2: State Management - Restore State Helper

**Files:**
- Modify: `org-gtd-projects.el` (add function after org-gtd-project--save-state)
- Test: `test/project-test.el` (add to same describe block)

**Step 1: Write the failing test**

Add to `test/project-test.el` in the "State preservation" describe block:

```elisp
   (it "restores ORG_GTD and TODO state from PREVIOUS_* properties"
       (create-project "Test project")
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Task 1")
         (org-back-to-heading t)

         ;; Set up incubated state with saved previous values
         (org-entry-put (point) "ORG_GTD" "Incubated")
         (org-entry-put (point) "PREVIOUS_ORG_GTD" "Actions")
         (org-entry-put (point) "PREVIOUS_TODO" "NEXT")
         (org-todo 'none)  ; Clear TODO keyword

         ;; Restore state
         (org-gtd-project--restore-state (point))

         ;; Verify state was restored
         (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
         (expect (org-entry-get (point) "TODO") :to-equal "NEXT")

         ;; Verify PREVIOUS_* properties were removed
         (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)
         (expect (org-entry-get (point) "PREVIOUS_TODO") :to-be nil)))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "restores ORG_GTD and TODO state"`

Expected: FAIL with "Symbol's function definition is void: org-gtd-project--restore-state"

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` after org-gtd-project--save-state:

```elisp
(defun org-gtd-project--restore-state (marker)
  "Restore ORG_GTD and TODO state at MARKER from PREVIOUS_* properties.

Restores PREVIOUS_ORG_GTD to ORG_GTD property.
Restores PREVIOUS_TODO to TODO keyword.
Removes PREVIOUS_* properties."
  (org-with-point-at marker
    (let ((previous-org-gtd (org-entry-get (point) "PREVIOUS_ORG_GTD"))
          (previous-todo (org-entry-get (point) "PREVIOUS_TODO")))
      ;; Restore ORG_GTD
      (when previous-org-gtd
        (org-entry-put (point) "ORG_GTD" previous-org-gtd)
        (org-entry-delete (point) "PREVIOUS_ORG_GTD"))
      ;; Restore TODO keyword
      (when previous-todo
        (org-todo previous-todo)
        (org-entry-delete (point) "PREVIOUS_TODO")))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "restores ORG_GTD and TODO state"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/project-test.el
git commit -m "feat: add state restoration helper for project reactivation

Implements org-gtd-project--restore-state which:
- Restores PREVIOUS_ORG_GTD to ORG_GTD property
- Restores PREVIOUS_TODO to TODO keyword
- Removes PREVIOUS_* properties after restoration

Paired with save-state, this enables full state preservation cycle
for incubating and reactivating projects.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 3: Project Incubation - Get All Tasks Helper

**Files:**
- Modify: `org-gtd-projects.el` (add function after state helpers)
- Test: `test/project-test.el` (add new test to describe block)

**Step 1: Write the failing test**

Add to `test/project-test.el` in the "State preservation" describe block:

```elisp
   (it "collects all tasks in a project by graph traversal"
       (create-project "Test project")
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Test project")
         (org-back-to-heading t)
         (let ((project-marker (point-marker))
               (task-ids '()))

           ;; Get all task IDs
           (goto-char (point-min))
           (search-forward "Task 1")
           (org-back-to-heading t)
           (push (org-id-get-create) task-ids)
           (goto-char (point-min))
           (search-forward "Task 2")
           (org-back-to-heading t)
           (push (org-id-get-create) task-ids)
           (goto-char (point-min))
           (search-forward "Task 3")
           (org-back-to-heading t)
           (push (org-id-get-create) task-ids)

           (setq task-ids (nreverse task-ids))

           ;; Get tasks via helper
           (let ((task-markers (org-gtd-project--get-all-tasks project-marker)))
             ;; Should return 3 task markers
             (expect (length task-markers) :to-equal 3)
             ;; Each marker should point to a task with matching ID
             (dolist (marker task-markers)
               (org-with-point-at marker
                 (expect (member (org-id-get) task-ids) :to-be-truthy)))))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "collects all tasks in a project"`

Expected: FAIL with "Symbol's function definition is void: org-gtd-project--get-all-tasks"

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` after restore-state function:

```elisp
(defun org-gtd-project--get-all-tasks (project-marker)
  "Return list of markers for all tasks in project at PROJECT-MARKER.

Uses existing org-gtd-projects--collect-tasks-by-graph to traverse
the project's dependency graph and collect all task markers."
  (org-gtd-projects--collect-tasks-by-graph project-marker))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "collects all tasks in a project"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/project-test.el
git commit -m "feat: add helper to get all tasks in a project

Implements org-gtd-project--get-all-tasks which delegates to
existing collect-tasks-by-graph for consistent task traversal.

This is used during incubation to find all tasks that need
their state saved.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 4: Project Incubation - Core Function (Basic)

**Files:**
- Modify: `org-gtd-projects.el` (add main incubation function)
- Test: `test/project-test.el` (add new describe block)

**Step 1: Write the failing test**

Add to `test/project-test.el` before final closing paren:

```elisp
 (describe "Project incubation"
   (it "incubates a project with review date"
       (create-project "Test project")
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Test project")
         (org-back-to-heading t)
         (let ((project-marker (point-marker)))

           ;; Incubate with review date
           (org-gtd-project-incubate project-marker "2025-12-01")

           ;; Verify project heading is incubated
           (org-with-point-at project-marker
             (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated")
             (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Projects")
             (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2025-12-01>"))

           ;; Verify all tasks are incubated
           (goto-char (point-min))
           (search-forward "Task 1")
           (org-back-to-heading t)
           (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated")
           (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Actions")
           (expect (org-entry-get (point) "TODO") :to-be nil)
           (expect (org-entry-get (point) "PREVIOUS_TODO") :to-equal "TODO")))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "incubates a project with review date"`

Expected: FAIL with "Symbol's function definition is void: org-gtd-project-incubate"

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` after helper functions (around line 700):

```elisp
;;;###autoload
(defun org-gtd-project-incubate (project-marker review-date)
  "Incubate project at PROJECT-MARKER with REVIEW-DATE.

PROJECT-MARKER is a marker pointing to the project heading.
REVIEW-DATE is a string in YYYY-MM-DD format.

Incubates the project by:
1. Saving state for project heading and all tasks
2. Setting ORG_GTD_TIMESTAMP for review date
3. Marking everything as Incubated

Does not check for external dependencies or multi-project tasks yet
(those validations added in later tasks)."
  (interactive
   (list (point-marker)
         (org-read-date nil nil nil "Review date: ")))

  (org-with-point-at project-marker
    ;; Save and incubate project heading
    (org-gtd-project--save-state project-marker)

    ;; Set review date
    (org-entry-put (point) "ORG_GTD_TIMESTAMP" (format "<%s>" review-date))

    ;; Incubate all tasks
    (let ((task-markers (org-gtd-project--get-all-tasks project-marker)))
      (dolist (task-marker task-markers)
        (org-gtd-project--save-state task-marker)))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "incubates a project with review date"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/project-test.el
git commit -m "feat: implement basic project incubation

Adds org-gtd-project-incubate which:
- Saves state for project heading and all tasks
- Sets ORG_GTD_TIMESTAMP for review date
- Marks everything as Incubated

This is the core incubation function. Validation logic
(external deps, multi-project tasks) added in later commits.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 5: Project Reactivation - Core Function

**Files:**
- Modify: `org-gtd-projects.el` (add reactivation function)
- Test: `test/project-test.el` (add to incubation describe block)

**Step 1: Write the failing test**

Add to `test/project-test.el` in "Project incubation" describe block:

```elisp
   (it "reactivates an incubated project"
       (create-project "Test project")
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Test project")
         (org-back-to-heading t)
         (let ((project-marker (point-marker)))

           ;; First incubate it
           (org-gtd-project-incubate project-marker "2025-12-01")

           ;; Verify it's incubated
           (org-with-point-at project-marker
             (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated"))

           ;; Now reactivate it
           (org-gtd-project-reactivate project-marker)

           ;; Verify project heading is restored
           (org-with-point-at project-marker
             (expect (org-entry-get (point) "ORG_GTD") :to-equal "Projects")
             (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)
             (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-be nil))

           ;; Verify tasks are restored
           (goto-char (point-min))
           (search-forward "Task 1")
           (org-back-to-heading t)
           (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
           (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)
           ;; TODO keyword should be restored (will be NEXT after recalculation)
           (expect (org-entry-get (point) "TODO") :not :to-be nil))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "reactivates an incubated project"`

Expected: FAIL with "Symbol's function definition is void: org-gtd-project-reactivate"

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` after org-gtd-project-incubate:

```elisp
;;;###autoload
(defun org-gtd-project-reactivate (project-marker)
  "Reactivate incubated project at PROJECT-MARKER.

PROJECT-MARKER is a marker pointing to the project heading.

Reactivates the project by:
1. Restoring state for project heading and all tasks
2. Removing ORG_GTD_TIMESTAMP property
3. Recalculating NEXT/TODO states based on dependencies
4. Opening graph view for user review (when called interactively)

Note: Graph view opening will be added in later task."
  (interactive (list (point-marker)))

  (org-with-point-at project-marker
    ;; Restore project heading state
    (org-gtd-project--restore-state project-marker)

    ;; Remove review date
    (org-entry-delete (point) "ORG_GTD_TIMESTAMP")

    ;; Restore all tasks
    (let ((task-markers (org-gtd-project--get-all-tasks project-marker)))
      (dolist (task-marker task-markers)
        (org-gtd-project--restore-state task-marker)))

    ;; Recalculate NEXT/TODO states based on dependencies
    (org-gtd-projects-fix-todo-keywords project-marker)

    ;; TODO: Open graph view when called interactively (added in later task)
    ))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "reactivates an incubated project"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/project-test.el
git commit -m "feat: implement project reactivation

Adds org-gtd-project-reactivate which:
- Restores state for project heading and all tasks
- Removes ORG_GTD_TIMESTAMP property
- Recalculates NEXT/TODO states via fix-todo-keywords

Graph view opening (for interactive use) will be added
in a later commit.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 6: Smart Dispatcher - Update org-gtd-incubate

**Files:**
- Modify: `org-gtd-incubate.el` (modify org-gtd-incubate function)
- Test: `test/incubate-test.el` (add new tests)

**Step 1: Write the failing test**

Add to `test/incubate-test.el` (find existing tests and add new describe block):

```elisp
(describe "Smart incubation dispatcher"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "detects project heading and calls org-gtd-project-incubate"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        (org-back-to-heading t)

        ;; Call org-gtd-incubate with review date override
        (let ((org-gtd-incubate-review-date "2025-12-01"))
          (org-gtd-incubate))

        ;; Verify project was incubated
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated")
        (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Projects")))

  (it "detects single item and uses existing incubation logic"
      (create-single-action "Test action")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "Test action")

        ;; Call org-gtd-incubate with date override
        (let ((org-gtd-incubate-review-date "2025-12-01"))
          (org-gtd-incubate))

        ;; Verify it was filed to incubate file
        ;; (This uses existing org-gtd-incubate.el logic)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated"))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "Smart incubation dispatcher"`

Expected: FAIL - tests will fail because org-gtd-incubate doesn't detect context yet

**Step 3: Write minimal implementation**

Modify `org-gtd-incubate.el`, replace the `org-gtd-incubate` function:

```elisp
(defun org-gtd-incubate (&optional reminder-date)
  "Decorate, organize and refile item at point as incubated.

Smart dispatcher that detects context:
- On project heading (ORG_GTD: Projects): incubate entire project
- On project task (has ORG_GTD_PROJECT_IDS): incubate project(s)
- On single item: use existing single-item incubation logic

If you want to call this non-interactively,
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."
  (interactive)

  ;; Detect context
  (let* ((org-gtd-value (org-entry-get (point) "ORG_GTD"))
         (project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))
         (is-project-heading (string= org-gtd-value "Projects"))
         (is-project-task (> (length project-ids) 0)))

    (cond
     ;; Case 1: On project heading - incubate the project
     (is-project-heading
      (require 'org-gtd-projects)
      (let ((review-date (or reminder-date
                             (org-read-date nil nil nil "Review date: "))))
        (org-gtd-project-incubate (point-marker) review-date)))

     ;; Case 2: On project task - incubate the project(s)
     ;; For now, just incubate the first project (multi-project selection added later)
     (is-project-task
      (require 'org-gtd-projects)
      (let* ((project-id (car project-ids))
             (project-marker (org-id-find project-id t))
             (review-date (or reminder-date
                              (org-read-date nil nil nil "Review date: "))))
        (if project-marker
            (org-gtd-project-incubate project-marker review-date)
          (user-error "Cannot find project with ID: %s" project-id))))

     ;; Case 3: Single item - use existing logic
     (t
      (let ((config-override (when reminder-date
                               `(('active-timestamp . ,(lambda (x) (format "<%s>" reminder-date)))))))
        (org-gtd-organize--call
         (lambda () (org-gtd-incubate--apply config-override))))))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "Smart incubation dispatcher"`

Expected: PASS (2 specs, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-incubate.el test/incubate-test.el
git commit -m "feat: make org-gtd-incubate a smart context dispatcher

Updates org-gtd-incubate to detect context:
- Project heading → incubate entire project
- Project task → incubate project(s)
- Single item → existing incubation logic

Multi-project task selection (transient menu) will be added
in a later commit. For now, incubates first project only.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 7: Smart Dispatcher - Add org-gtd-reactivate

**Files:**
- Modify: `org-gtd-incubate.el` (add new function)
- Test: `test/incubate-test.el` (add tests)

**Step 1: Write the failing test**

Add to `test/incubate-test.el`:

```elisp
(describe "Smart reactivation dispatcher"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "detects incubated project and calls org-gtd-project-reactivate"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        (org-back-to-heading t)

        ;; Incubate it first
        (let ((org-gtd-incubate-review-date "2025-12-01"))
          (org-gtd-incubate))

        ;; Verify it's incubated
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated")

        ;; Reactivate it
        (org-gtd-reactivate)

        ;; Verify it's reactivated
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Projects")
        (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "Smart reactivation dispatcher"`

Expected: FAIL with "Symbol's function definition is void: org-gtd-reactivate"

**Step 3: Write minimal implementation**

Add to `org-gtd-incubate.el` after org-gtd-incubate function:

```elisp
;;;###autoload
(defun org-gtd-reactivate ()
  "Reactivate incubated item at point.

Smart dispatcher that detects context:
- On incubated project heading: reactivate entire project
- On incubated single item: reactivate that item (future enhancement)"
  (interactive)

  ;; Check if item is incubated
  (let ((org-gtd-value (org-entry-get (point) "ORG_GTD")))
    (unless (string= org-gtd-value "Incubated")
      (user-error "Item at point is not incubated (ORG_GTD: %s)" org-gtd-value))

    ;; Detect if this is a project heading by checking PREVIOUS_ORG_GTD
    (let ((previous-org-gtd (org-entry-get (point) "PREVIOUS_ORG_GTD")))
      (cond
       ;; Case 1: Was a project heading - reactivate project
       ((string= previous-org-gtd "Projects")
        (require 'org-gtd-projects)
        (org-gtd-project-reactivate (point-marker)))

       ;; Case 2: Was a single item - reactivate that item
       ;; TODO: Implement single item reactivation logic (future enhancement)
       (t
        (user-error "Single item reactivation not yet implemented"))))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "Smart reactivation dispatcher"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-incubate.el test/incubate-test.el
git commit -m "feat: add smart reactivation dispatcher

Adds org-gtd-reactivate which detects context:
- Incubated project → reactivate entire project
- Incubated single item → error for now (future enhancement)

Works from both project heading and agenda views.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 8: View Language - Add Incubated Category Filters

**Files:**
- Modify: `org-gtd-view-language.el` (add filter handlers)
- Test: `test/org-gtd-view-language-test.el` (add tests)

**Step 1: Write the failing test**

Check if test file exists, if not we'll add tests to existing view test file. Add to appropriate test file:

```elisp
(describe "Incubated category filters"
  (it "filters for incubated projects only"
      ;; Test that (category . incubated-projects) returns only projects with ORG_GTD: Incubated
      (let ((view-spec '((name . "Incubated Projects")
                         (filters . ((category . incubated-projects))))))
        ;; Create test data: active project + incubated project
        (create-project "Active project")
        (create-project "Incubated project")

        (with-current-buffer (org-gtd--default-file)
          ;; Incubate the second project
          (goto-char (point-min))
          (search-forward "Incubated project")
          (org-back-to-heading t)
          (org-gtd-incubate "2025-12-01"))

        ;; Run the view and check results
        (org-gtd-view-show view-spec)
        (with-current-buffer org-agenda-buffer
          ;; Should show "Incubated project" but not "Active project"
          (expect (buffer-string) :to-match "Incubated project")
          (expect (buffer-string) :not :to-match "Active project"))))

  (it "filters for all incubated items"
      (let ((view-spec '((name . "All Incubated")
                         (filters . ((category . incubated))))))
        ;; Create incubated project + incubated single action
        (create-project "Incubated project")
        (create-single-action "Incubated action")

        ;; Incubate both
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Incubated project")
          (org-back-to-heading t)
          (org-gtd-incubate "2025-12-01")

          (goto-char (point-min))
          (search-forward "Incubated action")
          (org-back-to-heading t)
          (org-gtd-incubate "2025-12-01"))

        ;; Run the view
        (org-gtd-view-show view-spec)
        (with-current-buffer org-agenda-buffer
          (expect (buffer-string) :to-match "Incubated project")
          (expect (buffer-string) :to-match "Incubated action")))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "Incubated category filters"`

Expected: FAIL - filters not recognized yet

**Step 3: Write minimal implementation**

Find the category filter handler in `org-gtd-view-language.el` and add new cases. Look for `org-gtd-view-lang--filter-category` or similar function and add:

```elisp
;; In org-gtd-view-language.el, find the category filter handler
;; and add these cases to the cond:

((eq category 'incubated-projects)
 ;; Match headings with ORG_GTD: Incubated AND PREVIOUS_ORG_GTD: Projects
 '(and (property "ORG_GTD" "Incubated")
       (property "PREVIOUS_ORG_GTD" "Projects")))

((eq category 'incubated)
 ;; Match any item with ORG_GTD: Incubated
 '(property "ORG_GTD" "Incubated"))
```

Note: The exact location and syntax depends on how category filters are currently implemented. If the structure is different, adapt the pattern to match existing category handlers.

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "Incubated category filters"`

Expected: PASS (2 specs, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/org-gtd-view-language-test.el
git commit -m "feat: add incubated category filters to view language

Adds two new category filter types:
- incubated-projects: shows projects with ORG_GTD: Incubated
- incubated: shows any item with ORG_GTD: Incubated

Enables creating custom views for reviewing incubated items.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 9: External Dependencies Check

**Files:**
- Modify: `org-gtd-projects.el` (add validation function)
- Modify: `org-gtd-projects.el` (integrate into incubate function)
- Test: `test/project-test.el` (add tests)

**Step 1: Write the failing test**

Add to `test/project-test.el` in "Project incubation" describe block:

```elisp
   (it "warns about external dependencies before incubating"
       ;; Create two projects: Project A and Project B
       ;; Task in Project B depends on task in Project A
       ;; Incubating Project A should warn about Project B task

       (create-project "Project A")
       (create-project "Project B")

       (with-current-buffer (org-gtd--default-file)
         ;; Get IDs for tasks
         (goto-char (point-min))
         (re-search-forward "Project A")
         (org-next-visible-heading 1)  ; Task 1 of Project A
         (let ((task-a1-id (org-id-get-create)))

           (goto-char (point-min))
           (re-search-forward "Project B")
           (org-next-visible-heading 1)  ; Task 1 of Project B
           (let ((task-b1-id (org-id-get-create)))

             ;; Make Task B1 depend on Task A1 (external dependency)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a1-id)
             (goto-char (point-min))
             (re-search-forward "Project A")
             (org-next-visible-heading 1)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b1-id)

             ;; Check for external dependencies on Project A
             (goto-char (point-min))
             (re-search-forward "Project A")
             (org-back-to-heading t)
             (let ((external-deps (org-gtd-project--check-external-dependencies (point-marker))))
               ;; Should find Task B1 as external dependency
               (expect (length external-deps) :to-equal 1)
               (expect (org-with-point-at (car external-deps)
                         (org-id-get))
                       :to-equal task-b1-id))))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "warns about external dependencies"`

Expected: FAIL with "Symbol's function definition is void: org-gtd-project--check-external-dependencies"

**Step 3: Write minimal implementation**

Add to `org-gtd-projects.el` after the get-all-tasks function:

```elisp
(defun org-gtd-project--check-external-dependencies (project-marker)
  "Check for external tasks depending on PROJECT-MARKER's tasks.

Returns list of markers to external tasks (tasks not in this project)
that have DEPENDS_ON pointing to tasks in this project.

Used to warn user before incubating a project."
  (let ((project-tasks (org-gtd-project--get-all-tasks project-marker))
        (project-task-ids (mapcar (lambda (m)
                                     (org-with-point-at m (org-id-get)))
                                   (org-gtd-project--get-all-tasks project-marker)))
        (external-deps '()))

    ;; For each task in project, check what blocks it
    (dolist (task-marker project-tasks)
      (org-with-point-at task-marker
        (let ((task-id (org-id-get))
              (blocks-ids (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          ;; For each task this blocks, check if it's external
          (dolist (blocked-id blocks-ids)
            (unless (member blocked-id project-task-ids)
              ;; This is an external dependency
              (when-let ((blocked-marker (org-id-find blocked-id t)))
                (push blocked-marker external-deps)))))))

    (delete-dups external-deps)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "warns about external dependencies"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Integrate into incubate function**

Modify `org-gtd-project-incubate` to check and warn:

```elisp
;; In org-gtd-project-incubate, before saving state:

  ;; Check for external dependencies
  (let ((external-deps (org-gtd-project--check-external-dependencies project-marker)))
    (when external-deps
      (let ((dep-names (mapcar (lambda (m)
                                 (org-with-point-at m
                                   (org-get-heading t t t t)))
                               external-deps)))
        (unless (yes-or-no-p
                 (format "External tasks depend on this project:\n%s\n\nContinue incubating? "
                         (mapconcat (lambda (name) (format "  - %s" name))
                                    dep-names "\n")))
          (user-error "Incubation cancelled")))))
```

**Step 6: Commit**

```bash
git add org-gtd-projects.el test/project-test.el
git commit -m "feat: check for external dependencies before incubating

Adds org-gtd-project--check-external-dependencies which finds
external tasks that depend on the project being incubated.

Integrates check into org-gtd-project-incubate to warn user
before proceeding. User can cancel or continue.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 10: Multi-Project Task Handling

**Files:**
- Modify: `org-gtd-projects.el` (modify save-state to skip multi-project tasks)
- Test: `test/project-test.el` (add test)

**Step 1: Write the failing test**

Add to `test/project-test.el`:

```elisp
   (it "skips multi-project tasks during incubation"
       ;; Create project with a task that belongs to multiple projects
       (create-project "Project A")
       (create-project "Project B")

       (with-current-buffer (org-gtd--default-file)
         ;; Make Task 1 belong to both projects
         (goto-char (point-min))
         (re-search-forward "Project A")
         (let ((project-a-id (org-id-get-create)))
           (goto-char (point-min))
           (re-search-forward "Project B")
           (let ((project-b-id (org-id-get-create)))

             ;; Add both project IDs to Task 1 of Project A
             (goto-char (point-min))
             (re-search-forward "Project A")
             (org-next-visible-heading 1)  ; Task 1
             (org-entry-put (point) "ORG_GTD_PROJECT_IDS" (format "%s %s" project-a-id project-b-id))
             (let ((task-1-todo (org-entry-get (point) "TODO")))

               ;; Incubate Project A
               (goto-char (point-min))
               (re-search-forward "Project A")
               (org-back-to-heading t)
               (org-gtd-project-incubate (point-marker) "2025-12-01")

               ;; Verify Task 1 was NOT incubated (it belongs to multiple projects)
               (goto-char (point-min))
               (re-search-forward "Project A")
               (org-next-visible-heading 1)
               (expect (org-entry-get (point) "ORG_GTD") :not :to-equal "Incubated")
               (expect (org-entry-get (point) "TODO") :to-equal task-1-todo))))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "skips multi-project tasks"`

Expected: FAIL - task is incorrectly incubated

**Step 3: Write minimal implementation**

Modify `org-gtd-project--save-state` to check for multi-project tasks:

```elisp
(defun org-gtd-project--save-state (marker)
  "Save ORG_GTD and TODO state at MARKER to PREVIOUS_* properties for incubation.

Saves current ORG_GTD value to PREVIOUS_ORG_GTD property.
Saves current TODO keyword to PREVIOUS_TODO property.
Sets ORG_GTD to 'Incubated'.
Clears the TODO keyword.

Skips tasks that belong to multiple projects (identified by multiple
IDs in ORG_GTD_PROJECT_IDS property)."
  (org-with-point-at marker
    ;; Check if this is a multi-project task
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (if (> (length project-ids) 1)
          ;; Skip multi-project tasks
          (message "Skipping multi-project task: %s" (org-get-heading t t t t))
        ;; Normal incubation
        (let ((current-org-gtd (org-entry-get (point) "ORG_GTD"))
              (current-todo (org-entry-get (point) "TODO")))
          ;; Save current state
          (when current-org-gtd
            (org-entry-put (point) "PREVIOUS_ORG_GTD" current-org-gtd))
          (when current-todo
            (org-entry-put (point) "PREVIOUS_TODO" current-todo))
          ;; Set incubated state
          (org-entry-put (point) "ORG_GTD" "Incubated")
          ;; Clear TODO keyword
          (org-todo 'none))))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "skips multi-project tasks"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-projects.el test/project-test.el
git commit -m "feat: skip multi-project tasks during incubation

Updates org-gtd-project--save-state to check for tasks belonging
to multiple projects (via ORG_GTD_PROJECT_IDS).

Multi-project tasks are skipped during incubation - they remain
active with their TODO keywords intact. User is notified via
message.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 11: Area of Focus Review Integration

**Files:**
- Modify: `org-gtd-review.el` (update area-of-focus view specs)
- Test: `test/reviews-test.el` (add test)

**Step 1: Write the failing test**

Add to `test/reviews-test.el`:

```elisp
(describe "Area of focus review with incubated projects"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs)
               (setq org-gtd-areas-of-focus '("Work" "Personal")))
  (after-each (ogt--close-and-delete-files))

  (it "shows incubated projects in area review"
      ;; Create active and incubated projects in Work area
      (create-project "Active work project")
      (create-project "Incubated work project")

      (with-current-buffer (org-gtd--default-file)
        ;; Tag both with Work area
        (goto-char (point-min))
        (search-forward "Active work project")
        (org-back-to-heading t)
        (org-set-tags ":Work:")

        (goto-char (point-min))
        (search-forward "Incubated work project")
        (org-back-to-heading t)
        (org-set-tags ":Work:")

        ;; Incubate the second project
        (org-gtd-incubate "2025-12-01"))

      ;; Run area of focus review for Work
      (org-gtd-review-area-of-focus "Work")

      (with-current-buffer org-agenda-buffer
        ;; Should show both projects
        (expect (buffer-string) :to-match "Active work project")
        (expect (buffer-string) :to-match "Incubated work project")
        ;; Incubated should show review date
        (expect (buffer-string) :to-match "2025-12-01"))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev test -B "shows incubated projects in area review"`

Expected: FAIL - incubated projects not shown yet

**Step 3: Write minimal implementation**

Find `org-gtd-review--area-of-focus-view-specs` in `org-gtd-review.el` and add a new section for incubated projects:

```elisp
;; In org-gtd-review.el, in org-gtd-review--area-of-focus-view-specs function:
;; Add a new view spec for incubated projects after the projects spec:

((name . ,(format "Incubated Projects (%s)" area))
 (filters . ((category . incubated-projects)
             (area-of-focus . ,area)))
 (prefix-format . "  Incubated: "))
```

Note: The exact location depends on current structure. Add it as a separate section in the view specs list.

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev test -B "shows incubated projects in area review"`

Expected: PASS (1 spec, 0 failed)

**Step 5: Commit**

```bash
git add org-gtd-review.el test/reviews-test.el
git commit -m "feat: show incubated projects in area-of-focus review

Updates org-gtd-review--area-of-focus-view-specs to include
a separate section for incubated projects.

When reviewing an area of focus, users now see both active
and incubated projects, with review dates visible for
incubated projects.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 12: Graph View Integration - Incubate Option

**Files:**
- Modify: `org-gtd-graph-transient.el` (add incubate option to menu)
- Test: Test manually (transient menu testing is complex, manual verification sufficient)

**Step 1: No test needed**

Transient menu integration is best tested manually. Skip to implementation.

**Step 2: Write implementation**

Add to `org-gtd-graph-transient.el` in the main transient menu definition (find `org-gtd-graph-view-menu` or similar):

```elisp
;; In org-gtd-graph-transient.el, add to the transient menu:
;; Find the menu definition and add a new suffix:

("i" "Incubate this project" org-gtd-graph-incubate-project)
```

Then add the function:

```elisp
(defun org-gtd-graph-incubate-project ()
  "Incubate the current project being viewed in graph mode.

Calls org-gtd-incubate which will detect it's on a project heading
and incubate the entire project with all its tasks."
  (interactive)
  (org-with-point-at org-gtd-graph-view--project-marker
    (call-interactively #'org-gtd-incubate)))
```

**Step 3: Manual testing**

Test manually:
1. Create a project: `M-x org-gtd-capture` → add project
2. Open graph view: `M-x org-gtd-show-project-graph`
3. Press `?` to see help
4. Verify `i` shows "Incubate this project"
5. Press `i` and verify incubation prompts for date
6. Verify project is incubated

**Step 4: Commit**

```bash
git add org-gtd-graph-transient.el
git commit -m "feat: add incubate option to project graph view

Adds 'i' key binding in graph view transient menu to incubate
the current project.

Calls org-gtd-incubate which detects context and incubates
the entire project.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 13: Graph View Integration - Open on Reactivation

**Files:**
- Modify: `org-gtd-projects.el` (update reactivate function)
- Test: Manual testing (graph view opening is interactive)

**Step 1: No test needed**

Graph view opening is interactive behavior, manual testing sufficient.

**Step 2: Write implementation**

Modify `org-gtd-project-reactivate` to open graph view when called interactively:

```elisp
;; In org-gtd-project-reactivate, at the end:

  ;; ... existing restoration and recalculation code ...

  ;; Open graph view when called interactively
  (when (called-interactively-p 'any)
    (require 'org-gtd-graph-view)
    (org-gtd-show-project-graph project-marker)))
```

**Step 3: Manual testing**

Test manually:
1. Create and incubate a project
2. Navigate to the incubated project heading
3. Run `M-x org-gtd-reactivate`
4. Verify graph view opens automatically
5. Verify tasks have correct NEXT/TODO states

**Step 4: Commit**

```bash
git add org-gtd-projects.el
git commit -m "feat: open graph view after reactivating project

Updates org-gtd-project-reactivate to automatically open the
project graph view when called interactively.

This allows users to review and adjust the reactivated project's
state immediately after reactivation.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 14: Documentation - Update Commands Reference

**Files:**
- Modify: `doc/org-gtd.org` (add commands to reference section)

**Step 1: Find commands reference section**

Locate the commands reference in `doc/org-gtd.org` (around line 3000-4000).

**Step 2: Add documentation**

Add entries for the new commands using the writing-documentation skill's "Teach, Don't Tell" approach:

```org
**** ~org-gtd-incubate~

**What it is:** Smart command to pause items you're not ready to work on yet.

**Why you'd use it:** You have a project or action that's a good idea, but not right now. You want to set it aside with a reminder to review it later, without losing your planning work.

**Who needs it:** Anyone managing "someday/maybe" items or projects they want to defer.

**Your first use (30 seconds):**

1. Navigate to a project or action
2. Run: ~M-x org-gtd-incubate~
3. Enter review date: ~2025-12-01~
4. Done - it disappears from your engage view until that date

**What it does:**

Detects what you're on:
- Project heading → incubates entire project with all tasks
- Project task → incubates the project it belongs to
- Single action → incubates just that action

For projects:
- Preserves all task states and dependencies
- Hides everything from engage/NEXT views
- Adds review date to your agenda
- Skips multi-project tasks (tasks shared between projects)

**When to use:**

- Projects you want to pursue "someday" but not now
- Ideas that need to marinate before you're ready
- Seasonal projects (start each year)
- Projects blocked by external factors beyond your control

**** ~org-gtd-reactivate~

**What it is:** Bring back incubated projects and actions.

**Why you'd use it:** The review date arrived, you're ready to work on it again.

**Your first use (30 seconds):**

1. When you see an incubated item in agenda (on its review date)
2. Run: ~M-x org-gtd-reactivate~
3. Graph view opens - review the project structure
4. Adjust if needed and close

**What it does:**

- Restores original item states (project → Projects, actions → Actions, etc.)
- Restores task TODO keywords
- Recalculates NEXT tasks based on current dependencies
- Opens project graph view (for projects) so you can review and adjust
- Removes the review date

**When to use:**

- When an incubated item appears in your agenda on its review date
- When you're ready to reactivate something earlier than scheduled
- During weekly review when scanning incubated items
```

**Step 3: Commit**

```bash
git add doc/org-gtd.org
git commit -m "docs: add incubation commands to reference

Documents org-gtd-incubate and org-gtd-reactivate following
Teach, Don't Tell methodology:
- What/why/who (First Contact)
- 30-second quick start (Black Triangle)
- Details on behavior (Hairball)

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 15: Documentation - Add Troubleshooting Section

**Files:**
- Modify: `doc/org-gtd.org` (add troubleshooting for common issues)

**Step 1: Find troubleshooting section**

Locate troubleshooting section in `doc/org-gtd.org` (around line 4000-5000).

**Step 2: Add troubleshooting entry**

```org
**** Tasks Still Showing After Incubating Project

*Symptom*: Incubated a project but some tasks still appear in engage view.

*Diagnosis*:
- Multi-project tasks (tasks shared between projects) are not incubated
- If task belongs to Project A and Project B, and you only incubate Project A, the task stays visible

*Solution*:
- Check task's ORG_GTD_PROJECT_IDS property to see which projects it belongs to
- Either incubate all projects the task belongs to, or remove it from one project first
- During incubation, watch for "Skipping multi-project task" messages

*Recommended workflow*:
1. Before incubating: review the project for multi-project tasks
2. Decide: should this task stay active (part of other project) or be incubated too?
3. If incubating: incubate all projects it belongs to
4. If staying active: accept that it will remain visible
```

**Step 3: Commit**

```bash
git add doc/org-gtd.org
git commit -m "docs: add troubleshooting for multi-project task incubation

Explains why multi-project tasks may still appear after
incubating a project, with diagnosis and solutions.

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 16: End-to-End Integration Test

**Files:**
- Test: `test/end-to-end-test.el` (add comprehensive test)

**Step 1: Write the test**

Add to `test/end-to-end-test.el`:

```elisp
(describe "Incubating and reactivating projects (end-to-end)"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "full incubation → reactivation cycle preserves project state"
      ;; Create a project with dependencies
      (create-project "Future project")

      (with-current-buffer (org-gtd--default-file)
        ;; Verify initial state: Task 1 is NEXT
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal "NEXT")

        ;; Incubate the project
        (goto-char (point-min))
        (search-forward "Future project")
        (org-back-to-heading t)
        (org-gtd-incubate "2025-12-01")

        ;; Verify project is incubated
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated")
        (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2025-12-01>")

        ;; Verify tasks are incubated (no TODO keywords)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Incubated")
        (expect (org-entry-get (point) "TODO") :to-be nil)
        (expect (org-entry-get (point) "PREVIOUS_TODO") :to-equal "NEXT"))

      ;; Verify it doesn't appear in engage view
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (expect (buffer-string) :not :to-match "Task 1"))

      ;; Reactivate the project
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Future project")
        (org-back-to-heading t)
        (org-gtd-reactivate)

        ;; Verify project is reactivated
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Projects")
        (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-be nil)

        ;; Verify tasks are reactivated with TODO keywords
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
        (expect (org-entry-get (point) "TODO") :to-equal "NEXT"))

      ;; Verify it appears in engage view again
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (expect (buffer-string) :to-match "Task 1"))))
```

**Step 2: Run test to verify it passes**

Run: `~/bin/eldev test -B "full incubation.*reactivation cycle"`

Expected: PASS (1 spec, 0 failed)

**Step 3: Commit**

```bash
git add test/end-to-end-test.el
git commit -m "test: add end-to-end incubation/reactivation test

Comprehensive test covering:
- Project incubation with review date
- State preservation (ORG_GTD, TODO keywords)
- Visibility in engage view (hidden when incubated)
- Project reactivation
- State restoration
- Visibility after reactivation

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Task 17: Run Full Test Suite

**Step 1: Run all tests**

Run: `~/bin/eldev test -B`

Expected: All tests pass

**Step 2: Fix any failures**

If any tests fail, investigate and fix. Common issues:
- Test isolation problems (leftover state)
- Missing requires
- Incorrect assumptions about default state

**Step 3: Commit fixes if needed**

```bash
git add [files]
git commit -m "fix: resolve test failures

[Description of what was fixed]

🤖 Generated with Claude Code

Co-Authored-By: Claude <noreply@anthropic.com>"
```

---

## Future Enhancements (Not in This Plan)

The following features are documented in the design but explicitly out of scope for this implementation:

1. **Multi-project task selection UI**: Currently incubates first project only when on a multi-project task. Future: transient menu with checkboxes to select which projects to incubate.

2. **Perma-blocked children check**: Warning when incubating would permanently block a task (all its dependencies are being incubated). Currently not implemented.

3. **Single item reactivation**: org-gtd-reactivate currently only handles projects. Future: support reactivating individual incubated actions/items.

4. **Batch incubation**: Incubate multiple projects at once from a list view.

5. **Incubation templates**: Quick options like "review in 1 week", "review in 1 month".

6. **Statistics tracking**: How long projects stay incubated, reactivation rates, etc.

These can be added in future iterations based on user feedback and needs.

---

## Execution Options

**Plan complete and saved to `docs/plans/2025-11-22-incubating-projects.md`.**

Two execution options:

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration with quality gates

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with review checkpoints

Which approach would you like?
