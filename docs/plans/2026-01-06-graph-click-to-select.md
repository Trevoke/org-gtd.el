# Graph Click-to-Select Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable clicking on nodes in the org-gtd project graph view to select them as the current node.

**Architecture:** Add mouse-1 handler that extracts click coordinates, performs hit-testing against stored node bounds, and calls existing `org-gtd-graph-ui-select-node`. Coordinates differ by mode: ASCII uses buffer (col, row), SVG uses viewport pixels + viewBox offset.

**Tech Stack:** Emacs Lisp, org-gtd, dag-draw, e-unit for testing

---

## Task 1: Add Node Bounds Storage Variables

**Files:**
- Modify: `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/org-gtd-graph-view.el:66-78`

**Step 1: Write the buffer-local variables**

Add after `org-gtd-graph-view--file-watch-descriptor` (line 70):

```elisp
(defvar-local org-gtd-graph-view--node-bounds nil
  "Alist of (node-id . (left top width height)) for click hit-testing.
Coordinates are render-mode-specific:
- ASCII: buffer character coordinates
- SVG: SVG viewport pixel coordinates")

(defvar-local org-gtd-graph-view--svg-viewbox-offset nil
  "Cons cell (view-x . view-y) for SVG coordinate transformation.")
```

**Step 2: Verify elisp compiles**

Run: `emacs -Q --batch -f batch-byte-compile /home/stag/.emacs.d/elpa/org-gtd-4.0.7/org-gtd-graph-view.el 2>&1 | head -20`
Expected: No errors (warnings OK)

**Step 3: Commit**

```bash
cd /home/stag/.emacs.d/elpa/org-gtd-4.0.7
git add org-gtd-graph-view.el
git commit -m "feat(graph): add buffer-local vars for click hit-testing"
```

---

## Task 2: Add Hit-Testing Function (with TDD)

**Files:**
- Create: `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/test/org-gtd-graph-view-test.el`
- Modify: `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/org-gtd-graph-view.el`

**Step 1: Write the failing test**

Create test file:

```elisp
;;; org-gtd-graph-view-test.el --- Tests for graph view click handling -*- lexical-binding: t -*-

(require 'e-unit)
(require 'org-gtd-graph-view)

(describe "org-gtd-graph-view--node-at-position"
  (it "returns node-id when click is inside node bounds"
    (let ((org-gtd-graph-view--node-bounds
           '(("node-1" . (0 0 10 3))
             ("node-2" . (15 0 12 3)))))
      (assert-equal "node-1" (org-gtd-graph-view--node-at-position 5 1))))

  (it "returns correct node when multiple nodes exist"
    (let ((org-gtd-graph-view--node-bounds
           '(("node-1" . (0 0 10 3))
             ("node-2" . (15 0 12 3)))))
      (assert-equal "node-2" (org-gtd-graph-view--node-at-position 20 2))))

  (it "returns nil when click is in empty space"
    (let ((org-gtd-graph-view--node-bounds
           '(("node-1" . (0 0 10 3)))))
      (assert-nil (org-gtd-graph-view--node-at-position 50 50))))

  (it "returns nil when node-bounds is nil"
    (let ((org-gtd-graph-view--node-bounds nil))
      (assert-nil (org-gtd-graph-view--node-at-position 5 5)))))

(provide 'org-gtd-graph-view-test)
;;; org-gtd-graph-view-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `emacs -Q --batch -l e-unit -l org-gtd-graph-view -l test/org-gtd-graph-view-test.el -f e-unit-run-all-tests 2>&1`
Expected: FAIL - function `org-gtd-graph-view--node-at-position` is void

**Step 3: Write minimal implementation**

Add to `org-gtd-graph-view.el` after the variable definitions:

```elisp
(defun org-gtd-graph-view--node-at-position (x y)
  "Return node ID at position (X, Y) or nil if none.
X and Y are in render-mode-appropriate coordinates."
  (cl-loop for (node-id . bounds) in org-gtd-graph-view--node-bounds
           for left = (nth 0 bounds)
           for top = (nth 1 bounds)
           for width = (nth 2 bounds)
           for height = (nth 3 bounds)
           when (and (>= x left) (< x (+ left width))
                     (>= y top) (< y (+ top height)))
           return node-id))
```

**Step 4: Run test to verify it passes**

Run: `emacs -Q --batch -l e-unit -l org-gtd-graph-view -l test/org-gtd-graph-view-test.el -f e-unit-run-all-tests 2>&1`
Expected: All tests PASS

**Step 5: Commit**

```bash
git add org-gtd-graph-view.el test/org-gtd-graph-view-test.el
git commit -m "feat(graph): add hit-testing for click-to-select"
```

---

## Task 3: Add Coordinate Extraction Functions

**Files:**
- Modify: `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/org-gtd-graph-view.el`

**Step 1: Write coordinate extraction for both modes**

Add after the hit-testing function:

```elisp
(defun org-gtd-graph-view--extract-click-coords (pos)
  "Extract coordinates from POS based on current render mode.
Returns (x . y) in appropriate coordinate space, or nil if invalid."
  (pcase org-gtd-graph-view--render-mode
    ('ascii
     (posn-col-row pos))
    ('svg
     (when-let ((obj-xy (posn-object-x-y pos)))
       (if org-gtd-graph-view--svg-viewbox-offset
           (cons (+ (car obj-xy) (car org-gtd-graph-view--svg-viewbox-offset))
                 (+ (cdr obj-xy) (cdr org-gtd-graph-view--svg-viewbox-offset)))
         obj-xy)))))
```

**Step 2: Verify elisp compiles**

Run: `emacs -Q --batch -f batch-byte-compile org-gtd-graph-view.el 2>&1 | head -20`
Expected: No errors

**Step 3: Commit**

```bash
git add org-gtd-graph-view.el
git commit -m "feat(graph): add coordinate extraction for click handling"
```

---

## Task 4: Add Click Handler (with TDD)

**Files:**
- Modify: `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/test/org-gtd-graph-view-test.el`
- Modify: `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/org-gtd-graph-view.el`

**Step 1: Write the failing test**

Add to test file:

```elisp
(describe "org-gtd-graph-view-click-select"
  (it "calls select-node when clicking inside a node"
    (with-stub org-gtd-graph-view--extract-click-coords '(5 . 1)
      (with-spy org-gtd-graph-ui-select-node calls
        (let ((org-gtd-graph-view--node-bounds '(("node-1" . (0 0 10 3))))
              (org-gtd-graph-view--render-mode 'ascii))
          (org-gtd-graph-view-click-select 'ignored-event)
          (assert-true (spy-called-p calls))
          (assert-equal '("node-1") (spy-last-call calls))))))

  (it "does nothing when clicking empty space"
    (with-stub org-gtd-graph-view--extract-click-coords '(50 . 50)
      (with-spy org-gtd-graph-ui-select-node calls
        (let ((org-gtd-graph-view--node-bounds '(("node-1" . (0 0 10 3))))
              (org-gtd-graph-view--render-mode 'ascii))
          (org-gtd-graph-view-click-select 'ignored-event)
          (assert-false (spy-called-p calls)))))))
```

**Step 2: Run test to verify it fails**

Run: `emacs -Q --batch -l e-unit -l e-unit-mock -l org-gtd-graph-view -l org-gtd-graph-ui -l test/org-gtd-graph-view-test.el -f e-unit-run-all-tests 2>&1`
Expected: FAIL - function `org-gtd-graph-view-click-select` is void

**Step 3: Write minimal implementation**

Add to `org-gtd-graph-view.el`:

```elisp
(defun org-gtd-graph-view-click-select (event)
  "Select node at mouse click position.
If click is not on a node, do nothing."
  (interactive "e")
  (when-let* ((pos (event-start event))
              (coords (org-gtd-graph-view--extract-click-coords pos))
              (node-id (org-gtd-graph-view--node-at-position
                        (car coords) (cdr coords))))
    (org-gtd-graph-ui-select-node node-id)))
```

**Step 4: Run test to verify it passes**

Run: `emacs -Q --batch -l e-unit -l e-unit-mock -l org-gtd-graph-view -l org-gtd-graph-ui -l test/org-gtd-graph-view-test.el -f e-unit-run-all-tests 2>&1`
Expected: All tests PASS

**Step 5: Commit**

```bash
git add org-gtd-graph-view.el test/org-gtd-graph-view-test.el
git commit -m "feat(graph): add click handler for node selection"
```

---

## Task 5: Add Mouse Keybinding

**Files:**
- Modify: `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/org-gtd-graph-mode.el:53-75`

**Step 1: Add keybinding to mode map**

Find `org-gtd-graph-view-mode-map` definition and add after line 65 (after navigation bindings):

```elisp
    ;; Mouse selection
    (define-key map [mouse-1] #'org-gtd-graph-view-click-select)
```

**Step 2: Verify elisp compiles**

Run: `emacs -Q --batch -f batch-byte-compile org-gtd-graph-mode.el 2>&1 | head -20`
Expected: No errors

**Step 3: Commit**

```bash
git add org-gtd-graph-mode.el
git commit -m "feat(graph): bind mouse-1 to click-select"
```

---

## Task 6: Store Node Bounds After Rendering

**Files:**
- Modify: `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/org-gtd-graph-view.el`

**Step 1: Add bounds extraction helper**

Add after the click handler:

```elisp
(defun org-gtd-graph-view--extract-node-bounds (dd-graph scale)
  "Extract node bounding boxes from DD-GRAPH after layout.
SCALE is the coordinate scale factor (1.0 for ASCII, 10.0 for SVG).
Returns alist of (org-id-string . (left top width height))."
  (let ((bounds nil))
    (ht-each
     (lambda (node-id node)
       (let* ((x (* scale (or (dag-draw-node-x-coord node) 0)))
              (y (* scale (or (dag-draw-node-y-coord node) 0)))
              (w (* scale (dag-draw-node-x-size node)))
              (h (* scale (dag-draw-node-y-size node)))
              ;; Convert center coords to top-left
              (left (- x (/ w 2.0)))
              (top (- y (/ h 2.0))))
         (push (cons (symbol-name node-id) (list left top w h)) bounds)))
     (dag-draw-graph-nodes dd-graph))
    bounds))
```

**Step 2: Modify refresh to store bounds**

In `org-gtd-graph-view-refresh`, after `(dag-draw-layout-graph dd-graph)` is called (this happens inside `org-gtd-dag-draw-render`), we need to extract bounds.

Actually, looking at the code flow, `org-gtd-dag-draw-render` creates the dd-graph internally. We need to refactor slightly.

Modify `org-gtd-graph-view-refresh` to:

```elisp
(defun org-gtd-graph-view-refresh ()
  "Refresh the graph display in the current buffer."
  (interactive)
  (when org-gtd-graph-view--project-marker
    (let* ((full-graph (org-gtd-graph-data--extract-from-project
                        org-gtd-graph-view--project-marker))
           (issues (org-gtd-graph-data-validate full-graph))
           ;; Apply filter if active, otherwise use full graph
           (graph (if org-gtd-graph-view--filter
                      (org-gtd-graph-filter-create-filtered-graph
                       full-graph org-gtd-graph-view--filter)
                    full-graph)))

      ;; Warn about validation issues
      (when issues
        (message "Graph validation issues: %s"
                 (mapconcat #'identity issues "; ")))

      ;; Store the graph before rendering
      (setq org-gtd-graph-view--graph full-graph)

      ;; Render and store bounds
      (if (eq org-gtd-graph-view--render-mode 'ascii)
          (let* ((result (org-gtd-dag-draw-render-with-bounds graph 'ascii org-gtd-graph-ui--selected-node-id))
                 (ascii-text (plist-get result :output))
                 (dd-graph (plist-get result :graph)))
            (setq org-gtd-graph-view--node-bounds
                  (org-gtd-graph-view--extract-node-bounds dd-graph 1.0))
            (setq org-gtd-graph-view--svg-viewbox-offset nil)
            (org-gtd-graph-view--display-ascii ascii-text graph))
        ;; SVG mode
        (let* ((result (org-gtd-dag-draw-render-with-bounds graph 'svg org-gtd-graph-ui--selected-node-id))
               (svg (plist-get result :output))
               (dd-graph (plist-get result :graph))
               (scale (if (require 'dag-draw-svg nil t) dag-draw-svg-coordinate-scale 10.0))
               (bounds (dag-draw-get-graph-bounds dd-graph))
               (margin 20)
               (view-x (- (* scale (nth 0 bounds)) margin))
               (view-y (- (* scale (nth 1 bounds)) margin)))
          (setq org-gtd-graph-view--node-bounds
                (org-gtd-graph-view--extract-node-bounds dd-graph scale))
          (setq org-gtd-graph-view--svg-viewbox-offset (cons view-x view-y))
          (org-gtd-graph-view--display-svg svg graph))))))
```

**Step 3: Add render-with-bounds to org-gtd-dag-draw.el**

Modify `/home/stag/.emacs.d/elpa/org-gtd-4.0.7/org-gtd-dag-draw.el`:

```elisp
(defun org-gtd-dag-draw-render-with-bounds (org-gtd-graph format &optional selected-node-id)
  "Render ORG-GTD-GRAPH and return both output and positioned graph.
FORMAT should be \\='svg or \\='ascii.
SELECTED-NODE-ID, if provided, highlights that node.

Returns plist (:output STRING :graph DAG-DRAW-GRAPH)."
  (let* ((spec (org-gtd-dag-draw-translate org-gtd-graph selected-node-id))
         (dd-graph (apply #'dag-draw-create-from-spec spec)))
    (dag-draw-layout-graph dd-graph)
    (list :output (dag-draw-render-graph dd-graph format)
          :graph dd-graph)))
```

**Step 4: Verify elisp compiles**

Run: `emacs -Q --batch -f batch-byte-compile org-gtd-dag-draw.el org-gtd-graph-view.el 2>&1 | head -30`
Expected: No errors

**Step 5: Commit**

```bash
git add org-gtd-dag-draw.el org-gtd-graph-view.el
git commit -m "feat(graph): store node bounds after rendering for hit-testing"
```

---

## Task 7: Manual Testing

**No code changes - verification only**

**Step 1: Test ASCII mode click selection**

1. Open Emacs: `emacs -Q -l ~/.emacs.d/init.el`
2. Open an org-gtd project file
3. Run: `M-x org-gtd-show-project-graph`
4. Press `v` to ensure ASCII mode
5. Click on a node with mouse
6. Verify: Details panel updates to show clicked node

**Step 2: Test SVG mode click selection**

1. In same graph buffer, press `v` to toggle to SVG mode
2. Click on a node with mouse
3. Verify: Details panel updates, node gets highlight

**Step 3: Test empty space click**

1. Click on empty space between nodes
2. Verify: No change to selection

**Step 4: Commit docs**

```bash
git add docs/plans/2026-01-06-graph-click-to-select.md
git commit -m "docs: add click-to-select implementation plan"
```

---

## Summary

| Task | Description | Files Changed |
|------|-------------|---------------|
| 1 | Add buffer-local vars | org-gtd-graph-view.el |
| 2 | Add hit-testing (TDD) | org-gtd-graph-view.el, test file |
| 3 | Add coord extraction | org-gtd-graph-view.el |
| 4 | Add click handler (TDD) | org-gtd-graph-view.el, test file |
| 5 | Add keybinding | org-gtd-graph-mode.el |
| 6 | Store bounds after render | org-gtd-dag-draw.el, org-gtd-graph-view.el |
| 7 | Manual testing | none |

**Estimated time:** 2-3 hours
