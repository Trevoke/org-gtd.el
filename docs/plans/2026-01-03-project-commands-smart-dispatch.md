# Project Commands Smart Dispatch Design

## Overview

Enable project operations (add-successor, add-blocker, add-root, incubate, cancel) to work from both graph view and agenda contexts via smart dispatching.

## Problem

Currently, graph commands like `org-gtd-graph-add-successor` only work from within a graph view buffer. Users want to perform these operations directly from the agenda on project tasks.

Additionally, there's duplicated logic across the codebase for:
- Getting project marker from a task's `ORG_GTD_PROJECT_IDS`
- Prompting user when task belongs to multiple projects

## Architecture

### Layer 1: Core Commands in `org-gtd-projects.el`

Pure commands with no UI concerns. Work from any context via smart dispatch.

#### Shared Helpers

```elisp
(defun org-gtd-project--get-marker-at-point (&optional prompt)
  "Get project marker for task at point.
If task belongs to multiple projects, prompt user to choose.
PROMPT is the completing-read prompt (default: \"Which project? \").
Returns project marker, or signals error if not a project task.")

(defun org-gtd-project--get-marker-from-context (&optional prompt)
  "Get project marker from current context.
In graph-view-mode: return buffer-local project marker (via bound-and-true-p)
In agenda-mode: get marker from agenda item, then call get-marker-at-point
Otherwise: signal error."
  (cond
   ((and (eq major-mode 'org-gtd-graph-view-mode)
         (bound-and-true-p org-gtd-graph-view--project-marker))
    org-gtd-graph-view--project-marker)

   ((derived-mode-p 'org-agenda-mode)
    (org-gtd-project--get-marker-from-agenda-item prompt))

   (t (user-error "Must be in graph view or agenda"))))
```

#### Public Commands

| Command | Description |
|---------|-------------|
| `org-gtd-project-task-add-successor` | Add successor task to project |
| `org-gtd-project-task-add-blocker` | Add blocker task to project |
| `org-gtd-project-add-root-task` | Add root task (no dependencies) |
| `org-gtd-project-incubate` | Incubate/tickler the project |
| `org-gtd-project-cancel` | Cancel the project |

These commands:
1. Call `org-gtd-project--get-marker-from-context` to get project marker
2. Perform the operation
3. Show confirmation message
4. Return (no refresh - UI layer handles that)

### Layer 2: UI Wrappers in `org-gtd-graph-transient.el`

Simple wrappers that call core commands then refresh the graph.

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

Transient menu binds to these wrappers.

### Layer 3: Keymap in `org-gtd-graph-mode.el`

Keymap binds to the wrapper functions:

```elisp
(define-key map (kbd "s") #'org-gtd-graph-add-successor)
(define-key map (kbd "b") #'org-gtd-graph-add-blocker)
(define-key map (kbd "r") #'org-gtd-graph-add-root)
(define-key map (kbd "I") #'org-gtd-graph-incubate-project)
(define-key map (kbd "C") #'org-gtd-graph-cancel-project)
```

## Behavior Summary

| Command | From Graph View | From Agenda | Post-op (graph) | Post-op (agenda) |
|---------|----------------|-------------|-----------------|------------------|
| add-successor | use project-marker | prompt if multi-project | refresh | message |
| add-blocker | use project-marker | prompt if multi-project | refresh | message |
| add-root | use project-marker | prompt if multi-project | refresh | message |
| incubate | use project-marker | prompt if multi-project | quit view | message |
| cancel | use project-marker | prompt if multi-project | quit view | message |

## Refactoring Opportunities

The new `org-gtd-project--get-marker-at-point` helper can simplify:

1. **`org-gtd-areas-of-focus--set-on-project-tasks`** (areas-of-focus.el:98-119)
   - Currently has inline "get IDs -> prompt if multiple -> get marker" logic

2. **`org-gtd-project-cancel-from-agenda-item`** (projects.el:122-144)
   - Same pattern, can use shared helper

3. **`org-gtd-tickler`** (tickler.el:84-92)
   - Has TODO comment: "multi-project selection added later"
   - Currently uses first project without prompting

## Dependency Management

**No circular dependencies** because:
- `org-gtd-graph-view.el` requires `org-gtd-projects.el` (existing)
- `org-gtd-projects.el` does NOT require `org-gtd-graph-view.el`
- Context detection uses `(eq major-mode 'org-gtd-graph-view-mode)` and `(bound-and-true-p org-gtd-graph-view--project-marker)` - no require needed

## Implementation Steps

1. Add `org-gtd-project--get-marker-at-point` helper to `org-gtd-projects.el`
2. Add `org-gtd-project--get-marker-from-context` helper to `org-gtd-projects.el`
3. Create core commands in `org-gtd-projects.el`:
   - `org-gtd-project-task-add-successor`
   - `org-gtd-project-task-add-blocker`
   - `org-gtd-project-add-root-task`
   - `org-gtd-project-incubate`
   - `org-gtd-project-cancel`
4. Update UI wrappers in `org-gtd-graph-transient.el`
5. Add `C` keybinding for cancel in keymap and transient
6. Refactor existing code to use new helpers:
   - `org-gtd-areas-of-focus--set-on-project-tasks`
   - `org-gtd-tickler`
   - Existing cancel-from-agenda code
7. Update documentation

## Testing

- Test each command from graph view
- Test each command from agenda on single-project task
- Test each command from agenda on multi-project task (verify prompt)
- Test error case: command from agenda on non-project task
- Test error case: command from neither graph view nor agenda
