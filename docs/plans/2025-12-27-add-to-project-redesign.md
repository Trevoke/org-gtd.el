# Add to Project Flow Redesign

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan.

**Goal:** Fix the "add to project" organize flow to set `ORG_GTD_PROJECT_IDS` explicitly rather than deriving it from refile hierarchy.

**Architecture:** Project membership is determined by `ORG_GTD_PROJECT_IDS`, not outline hierarchy. The current flow is backwards—it refiles first, then tries to derive project membership. The new flow selects the project first, sets properties explicitly, then handles refile.

**Bead:** TBD (create when starting implementation)

---

## Problem Statement

**Current broken flow:**
```
1. Configure task (ORG_GTD, TRIGGER)
2. Refile via org-refile → user picks project here
3. After refile, derive project-id from hierarchy
4. Set ORG_GTD_PROJECT_IDS (too late!)
5. Update FIRST_TASKS, fix keywords
```

Issues:
- Project membership derived from hierarchy instead of set explicitly
- `ORG_GTD_PROJECT_IDS` set after refile, not before
- Doesn't match graph view's cleaner approach
- Confuses physical location with logical membership

**Graph view does it right:**
```elisp
;; From org-gtd-graph-transient.el:244-247
(org-with-point-at org-gtd-graph-view--project-marker
  (let ((project-id (org-entry-get (point) "ID")))
    (org-with-point-at (org-id-find existing-id t)
      (org-gtd-add-to-multivalued-property existing-id org-gtd-prop-project-ids project-id)
      (save-buffer))))
```

---

## New Flow

```
1. Prompt: "Select project" → get project-id and project-marker
2. Configure task (ORG_GTD, TRIGGER)
3. Set ORG_GTD_PROJECT_IDS = project-id
4. Add task-id to project's FIRST_TASKS
5. Handle refile:
   - If skip-refile: leave task in place
   - If 'project-task in org-gtd-refile-prompt-for-types: prompt with user's targets
   - Otherwise: move under project heading
6. Fix TODO keywords
```

Key changes:
- Project selection moves to **step 1** (before any configuration)
- `ORG_GTD_PROJECT_IDS` set **explicitly** (not derived from hierarchy)
- `FIRST_TASKS` updated immediately (task has no dependencies when added this way)
- Refile respects both `skip-refile` and `org-gtd-refile-prompt-for-types`

---

## Implementation

### Task 1: Add project selection helper

**Files:**
- Modify: `org-gtd-projects.el`

**Implementation:**
```elisp
(defun org-gtd-project-extend--select-project ()
  "Prompt user to select a project.
Returns cons of (project-id . project-marker)."
  (let* ((project-alist (org-gtd-projects--get-all-projects))
         (project-names (mapcar #'car project-alist))
         (chosen-name (completing-read "Add to project: " project-names nil t))
         (project-id (cdr (assoc chosen-name project-alist)))
         (project-marker (org-id-find project-id t)))
    (cons project-id project-marker)))
```

**Commit:** `feat: add project selection helper for add-to-project flow`

---

### Task 2: Add move-to-project helper

**Files:**
- Modify: `org-gtd-projects.el`

**Implementation:**
```elisp
(defun org-gtd-project-extend--move-to-project (project-marker)
  "Move current heading under PROJECT-MARKER using programmatic refile."
  (let* ((project-file (buffer-file-name (marker-buffer project-marker)))
         (project-pos (marker-position project-marker))
         (rfloc (list nil project-file nil project-pos)))
    (org-refile nil nil rfloc)))
```

**Commit:** `feat: add move-to-project helper for programmatic refile`

---

### Task 3: Add refile-with-user-targets helper

**Files:**
- Modify: `org-gtd-refile.el`

**Implementation:**
```elisp
(defun org-gtd-refile--with-user-targets ()
  "Refile current heading using user's org-refile-targets.
Does not merge with org-gtd targets—uses only what user configured."
  (let ((org-refile-use-outline-path t)
        (org-outline-path-complete-in-steps nil))
    (org-refile)))
```

**Commit:** `feat: add refile helper that uses only user targets`

---

### Task 4: Rewrite org-gtd-project-extend--apply

**Files:**
- Modify: `org-gtd-projects.el`

**Implementation:**
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

    ;; Step 4: Add to FIRST_TASKS (no dependencies)
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

**Commit:** `fix: rewrite add-to-project to set ORG_GTD_PROJECT_IDS explicitly`

---

### Task 5: Remove dead code

**Files:**
- Modify: `org-gtd-projects.el`
- Modify: `org-gtd-refile.el`

**Changes:**
- Remove or simplify `org-gtd-project--update-after-task-addition` (no longer needed in current form)
- Review `org-gtd-refile--do-project-task` - may be unused after this change

**Commit:** `refactor: remove dead code from old add-to-project flow`

---

### Task 6: Update tests

**Files:**
- Modify: `test/acceptance/project-task-operations-test.el`
- Possibly create: `test/unit/project-extend-test.el`

**Tests to add/update:**
- Test that `ORG_GTD_PROJECT_IDS` is set before refile
- Test skip-refile leaves task in place with correct properties
- Test refile-prompt-for-types behavior
- Test automatic refile moves under project

**Commit:** `test: update tests for new add-to-project flow`

---

## Summary

| Aspect | Before | After |
|--------|--------|-------|
| Project selection | During org-refile | Step 1, before configuration |
| ORG_GTD_PROJECT_IDS | Derived from hierarchy after refile | Set explicitly before refile |
| FIRST_TASKS | Set after refile | Set immediately after selection |
| Refile behavior | Always prompts with merged targets | Respects skip-refile and prompt-for-types |
| Matches graph view | No | Yes |
