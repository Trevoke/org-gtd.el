# Detailed Implementation Plan: Graph View UX Cleanup

## Overview
Clean up the project graph view by removing unused features, adding configurable sibling navigation, and improving legend clarity.

---

## Task 1: Remove Zoom Feature from org-gtd-graph-filter.el
**Estimate:** 15 min
**File:** `org-gtd-graph-filter.el`

### What to Remove:
1. **Line 58** - Remove `zoom-node-id` field from `org-gtd-graph-filter` struct:
   ```elisp
   ;; DELETE this line:
   (zoom-node-id nil)
   ```

2. **Lines 53-54** - Update docstring to remove zoom reference:
   ```elisp
   ;; DELETE these lines from docstring:
   ;;   scheduled    - Scheduled filter: 'overdue, 'today, 'week, 'unscheduled, or nil
   ;;   zoom-node-id - Node ID for zoom focus, or nil for full view
   ```

3. **Lines 76-79** - Remove zoom filter application in `org-gtd-graph-filter-apply`:
   ```elisp
   ;; DELETE this block:
   (when (org-gtd-graph-filter-zoom-node-id filter)
     (setq all-node-ids (org-gtd-graph-filter--get-subtree-ids
                         graph
                         (org-gtd-graph-filter-zoom-node-id filter))))
   ```

4. **Lines 249-277** - Remove both zoom command functions:
   ```elisp
   ;; DELETE: org-gtd-graph-zoom-to-subtree (lines 249-264)
   ;; DELETE: org-gtd-graph-zoom-out-full (lines 266-277)
   ```

### Verification:
- Run `eldev compile org-gtd-graph-filter.el` - no errors
- Grep for `zoom` in file - should only find filter-field accessor (auto-generated)

---

## Task 2: Remove Zoom from org-gtd-graph-transient.el
**Estimate:** 10 min
**File:** `org-gtd-graph-transient.el`

### What to Remove:
1. **Line 68** - Remove zoom entry from main transient menu:
   ```elisp
   ;; DELETE this line:
   ("z" "Zoom" org-gtd-graph-transient-zoom)
   ```

2. **Lines 279-285** - Remove zoom transient prefix:
   ```elisp
   ;; DELETE entire transient-define-prefix:
   (transient-define-prefix org-gtd-graph-transient-zoom ()
     "Zoom graph view focus."
     ["Zoom Graph View"
      ["Zoom"
       ("z" "zoom to selected subtree" org-gtd-graph-zoom-to-subtree)
       ("o" "zoom out to full view" org-gtd-graph-zoom-out-full)]])
   ```

### Verification:
- Run `eldev compile org-gtd-graph-transient.el` - no errors
- Check transient menu no longer has "z - Zoom"

---

## Task 3: Remove Navigation History from org-gtd-graph-ui.el
**Estimate:** 20 min
**File:** `org-gtd-graph-ui.el`

### What to Remove:
1. **Lines 56-63** - Remove history variables:
   ```elisp
   ;; DELETE these three defvar-local forms:
   (defvar-local org-gtd-graph-ui--navigation-history nil ...)
   (defvar-local org-gtd-graph-ui--navigation-future nil ...)
   (defvar-local org-gtd-graph-ui--traversal-order nil ...)
   ```
   Note: Keep `org-gtd-graph-ui--traversal-order` - it's used by layer navigation

2. **Lines 121-154** - Remove history functions:
   ```elisp
   ;; DELETE entire "Navigation History" section:
   ;; - org-gtd-graph-ui-push-history
   ;; - org-gtd-graph-ui-back
   ;; - org-gtd-graph-ui-forward
   ```

3. **Lines 158-170** - Update `org-gtd-graph-ui-select-node` to remove history logic:
   ```elisp
   ;; BEFORE:
   (defun org-gtd-graph-ui-select-node (node-id &optional no-history)
     "Select NODE-ID and update details panel.
   Updates the details buffer to show task information.
   If NO-HISTORY is non-nil, don't add to navigation history."
     (unless no-history
       (org-gtd-graph-ui-push-history node-id)
       ;; Clear future stack when navigating normally (not via back/forward)
       (setq org-gtd-graph-ui--navigation-future nil))
     (setq org-gtd-graph-ui--selected-node-id node-id)
     ...)

   ;; AFTER:
   (defun org-gtd-graph-ui-select-node (node-id)
     "Select NODE-ID and update details panel.
   Updates the details buffer to show task information."
     (setq org-gtd-graph-ui--selected-node-id node-id)
     ...)
   ```

4. **Update call sites** - Find calls to `org-gtd-graph-ui-select-node` with 2nd arg:
   - `org-gtd-graph-view.el:120` - remove `t` argument

### Verification:
- Run `eldev compile org-gtd-graph-ui.el` - no errors
- Grep for `history` and `future` - no matches

---

## Task 4: Remove Keybindings from org-gtd-graph-mode.el
**Estimate:** 10 min
**File:** `org-gtd-graph-mode.el`

### What to Remove:
1. **Lines 64-65** - Remove first/last layer bindings:
   ```elisp
   ;; DELETE:
   (define-key map (kbd "<") #'org-gtd-graph-nav-first-in-layer)
   (define-key map (kbd ">") #'org-gtd-graph-nav-last-in-layer)
   ```

2. **Lines 71-72** - Remove history navigation bindings:
   ```elisp
   ;; DELETE:
   (define-key map (kbd "[") #'org-gtd-graph-ui-back)
   (define-key map (kbd "]") #'org-gtd-graph-ui-forward)
   ```

3. **Lines 99-101** - Remove history documentation from mode docstring:
   ```elisp
   ;; DELETE these lines from docstring:
   ;; Navigation history:
   ;; - Press \\[org-gtd-graph-ui-back] to go back to previously selected node
   ;; - Press \\[org-gtd-graph-ui-forward] to go forward after going back
   ```

### Verification:
- Run `eldev compile org-gtd-graph-mode.el` - no errors
- Keys `<`, `>`, `[`, `]` no longer bound

---

## Task 5: Remove First/Last Layer Functions from org-gtd-graph-navigation.el
**Estimate:** 10 min
**File:** `org-gtd-graph-navigation.el`

### What to Remove:
1. **Lines 142-160** - Remove both functions:
   ```elisp
   ;; DELETE: org-gtd-graph-nav-first-in-layer
   ;; DELETE: org-gtd-graph-nav-last-in-layer
   ```

### Verification:
- Run `eldev compile org-gtd-graph-navigation.el` - no errors
- Grep for `first-in-layer` and `last-in-layer` - no matches

---

## Task 6: Add Sibling Mode Configuration to org-gtd-graph-navigation.el
**Estimate:** 45 min
**File:** `org-gtd-graph-navigation.el`

### What to Add:

1. **Add new customization variable** after `;;;; Requirements`:
   ```elisp
   ;;;; Customization

   (defcustom org-gtd-graph-sibling-mode 'any-same-blocker
     "How to determine sibling tasks for TAB/S-TAB navigation.

   - `any-same-blocker': Tasks sharing at least one blocker (default).
     Two tasks are siblings if they have any blocker in common.

   - `all-same-blockers': Tasks with exactly the same set of blockers.
     Two tasks are siblings only if their blocker sets are identical.

   - `dag-level': Tasks at the same depth in the dependency graph.
     Two tasks are siblings if they have the same layer number,
     regardless of their actual blockers."
     :type '(choice (const :tag "Any same blocker (default)" any-same-blocker)
                    (const :tag "All same blockers" all-same-blockers)
                    (const :tag "DAG level (depth)" dag-level))
     :group 'org-gtd)
   ```

2. **Add new sibling calculation functions** after layer-based navigation section:
   ```elisp
   ;;;; Sibling Calculation by Mode

   (defun org-gtd-graph-nav--get-current-blockers ()
     "Get list of blocker IDs for currently selected node."
     (when org-gtd-graph-ui--selected-node-id
       (org-gtd-graph-data-get-predecessors
        org-gtd-graph-view--graph
        org-gtd-graph-ui--selected-node-id)))

   (defun org-gtd-graph-nav--get-siblings-any-same-blocker ()
     "Get sibling nodes that share at least one blocker with current node.
   Returns list of node IDs in traversal order, excluding current node."
     (when-let* ((current-blockers (org-gtd-graph-nav--get-current-blockers))
                 ((not (null current-blockers))))
       (let ((current-id org-gtd-graph-ui--selected-node-id)
             (current-blocker-set (make-hash-table :test 'equal))
             (siblings '()))
         ;; Build set of current node's blockers
         (dolist (blocker current-blockers)
           (puthash blocker t current-blocker-set))
         ;; Find all nodes sharing any blocker
         (maphash
          (lambda (node-id _node)
            (unless (string= node-id current-id)
              (let ((node-blockers (org-gtd-graph-data-get-predecessors
                                    org-gtd-graph-view--graph node-id)))
                (when (cl-some (lambda (b) (gethash b current-blocker-set))
                               node-blockers)
                  (push node-id siblings)))))
          (org-gtd-graph-nodes org-gtd-graph-view--graph))
         ;; Return in traversal order
         (let ((order (org-gtd-graph-nav--get-traversal-order)))
           (seq-filter (lambda (id) (member id siblings)) order)))))

   (defun org-gtd-graph-nav--get-siblings-all-same-blockers ()
     "Get sibling nodes that have exactly the same blockers as current node.
   Returns list of node IDs in traversal order, excluding current node."
     (when-let* ((current-blockers (org-gtd-graph-nav--get-current-blockers)))
       (let ((current-id org-gtd-graph-ui--selected-node-id)
             (current-blocker-set (sort (copy-sequence current-blockers) #'string<))
             (siblings '()))
         ;; Find all nodes with identical blocker set
         (maphash
          (lambda (node-id _node)
            (unless (string= node-id current-id)
              (let* ((node-blockers (org-gtd-graph-data-get-predecessors
                                     org-gtd-graph-view--graph node-id))
                     (node-blocker-set (sort (copy-sequence node-blockers) #'string<)))
                (when (equal current-blocker-set node-blocker-set)
                  (push node-id siblings)))))
          (org-gtd-graph-nodes org-gtd-graph-view--graph))
         ;; Return in traversal order
         (let ((order (org-gtd-graph-nav--get-traversal-order)))
           (seq-filter (lambda (id) (member id siblings)) order)))))

   (defun org-gtd-graph-nav--get-siblings ()
     "Get sibling nodes based on `org-gtd-graph-sibling-mode'.
   Returns list of node IDs in traversal order."
     (pcase org-gtd-graph-sibling-mode
       ('any-same-blocker (org-gtd-graph-nav--get-siblings-any-same-blocker))
       ('all-same-blockers (org-gtd-graph-nav--get-siblings-all-same-blockers))
       ('dag-level (org-gtd-graph-nav--get-nodes-in-layer
                    (org-gtd-graph-nav--get-current-layer)))
       (_ (org-gtd-graph-nav--get-siblings-any-same-blocker))))
   ```

3. **Update next-sibling to use new dispatch**:
   ```elisp
   (defun org-gtd-graph-nav-next-sibling ()
     "Move to next sibling node based on `org-gtd-graph-sibling-mode'."
     (interactive)
     (when-let* ((sibling-nodes (org-gtd-graph-nav--get-siblings))
                 ((> (length sibling-nodes) 0)))
       (let* ((current-pos (cl-position org-gtd-graph-ui--selected-node-id
                                        sibling-nodes :test 'equal))
              (next-pos (if current-pos (1+ current-pos) 0)))
         (if (< next-pos (length sibling-nodes))
             (let ((next-node-id (nth next-pos sibling-nodes)))
               (org-gtd-graph-ui-select-node next-node-id))
           (message "No next sibling")))))
   ```

4. **Update previous-sibling similarly**:
   ```elisp
   (defun org-gtd-graph-nav-previous-sibling ()
     "Move to previous sibling node based on `org-gtd-graph-sibling-mode'."
     (interactive)
     (when-let* ((sibling-nodes (org-gtd-graph-nav--get-siblings))
                 ((> (length sibling-nodes) 0)))
       (let* ((current-pos (cl-position org-gtd-graph-ui--selected-node-id
                                        sibling-nodes :test 'equal))
              (prev-pos (if current-pos (1- current-pos) -1)))
         (if (>= prev-pos 0)
             (let ((prev-node-id (nth prev-pos sibling-nodes)))
               (org-gtd-graph-ui-select-node prev-node-id))
           (message "No previous sibling")))))
   ```

### Verification:
- Run `eldev compile org-gtd-graph-navigation.el` - no errors
- Test each sibling mode manually

---

## Task 7: Update Legends in org-gtd-graph-view.el
**Estimate:** 15 min
**File:** `org-gtd-graph-view.el`

### What to Change:

1. **Lines 201-210** - Update `--display-svg` legend:
   ```elisp
   ;; BEFORE:
   (insert (propertize "Graph View Controls:\n" 'face 'bold))
   (insert "  Quick Keys:      ? - Show all commands (transient menu) v - Toggle ASCII/SVG\n")
   (insert "  Navigation:      n/p - Down/up dependency | TAB - Next sibling | g - Go to node\n")
   (insert "  View:            z - Zoom\n")
   (insert "  Actions:         r - Refresh | q - Quit\n")
   (insert "\n")
   (insert "  Press ? to see all commands including: add child/root/blocker tasks\n")
   (insert (format "\n  Render mode: SVG (press 'v' to toggle)\n"))

   ;; AFTER:
   (insert (propertize "Graph View Controls:\n" 'face 'bold))
   (insert "  ? - All commands    v - Toggle ASCII/SVG    r - Refresh    q - Quit\n")
   (insert "\n")
   (insert "  Navigation:\n")
   (insert "    n - A task this blocks       p - A task that blocks this\n")
   (insert "    TAB - Next sibling task      S-TAB - Previous sibling task\n")
   (insert "    g - Go to task by name\n")
   (insert "\n")
   (insert "  Press ? to see all commands including: add/remove tasks, modify relationships\n")
   (insert (format "\n  Render mode: SVG (press 'v' to toggle)\n"))
   ```

2. **Lines 221-230** - Update `--display-ascii` legend identically:
   ```elisp
   ;; Same changes as above, just different line numbers
   ```

### Verification:
- Run `eldev compile org-gtd-graph-view.el` - no errors
- Open graph view and verify legend displays correctly

---

## Task 8: Run Full Test Suite
**Estimate:** 10 min

### Commands:
```bash
eldev compile
eldev etest -B
```

### Expected:
- All compilation succeeds
- All tests pass (or document any that need updating)

---

## Summary

| Task | File | Estimate | Type |
|------|------|----------|------|
| 1 | org-gtd-graph-filter.el | 15 min | Remove |
| 2 | org-gtd-graph-transient.el | 10 min | Remove |
| 3 | org-gtd-graph-ui.el | 20 min | Remove |
| 4 | org-gtd-graph-mode.el | 10 min | Remove |
| 5 | org-gtd-graph-navigation.el | 10 min | Remove |
| 6 | org-gtd-graph-navigation.el | 45 min | Add |
| 7 | org-gtd-graph-view.el | 15 min | Update |
| 8 | Full test suite | 10 min | Verify |
| **Total** | | **135 min** | |

## Dependencies

```
Task 1 (filter) ──┐
Task 2 (transient)├── Task 8 (tests)
Task 3 (ui) ──────┤
Task 4 (mode) ────┤
Task 5 (nav rm) ──┼── Task 6 (nav add) ──┘
Task 7 (legends) ─┘
```

Tasks 1-5 and 7 are independent.
Task 6 depends on Task 5 (same file, remove before add).
Task 8 depends on all others.
