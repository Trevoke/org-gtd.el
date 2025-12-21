# Someday/Maybe Review Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Enable iterative review of someday/maybe items with defer/clarify options.

**Architecture:** New major mode (`org-gtd-someday-review-mode`) derived from `org-mode`, using the existing WIP buffer infrastructure (`org-gtd-wip.el`) for buffer management. Buffer is read-only with single-key actions. Session state stored in global variable for persistence across organize flow. Uses existing `org-gtd-reactivate` for clarify action.

**Tech Stack:** Emacs Lisp, e-unit testing framework, org-mode LOGBOOK drawer API

---

## Task 1: Add Configuration Variable

**Files:**
- Modify: `org-gtd-someday.el`
- Test: `test/unit/someday-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-test.el`:

```elisp
;;; Someday Lists Configuration Tests

(deftest someday/lists-variable-exists ()
  "org-gtd-someday-lists variable exists and defaults to nil."
  (assert-nil org-gtd-someday-lists))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday/lists-variable-exists"`
Expected: FAIL with "Symbol's value as variable is void: org-gtd-someday-lists"

**Step 3: Write minimal implementation**

Add to `org-gtd-someday.el` after the Requirements section:

```elisp
;;;; Customization

(defcustom org-gtd-someday-lists nil
  "List of someday/maybe list names for review grouping.
When nil, all someday items are reviewed together without prompting.
When populated, user is prompted to select which list to review."
  :group 'org-gtd
  :package-version '(org-gtd . "4.0")
  :type '(repeat string))
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest -B "someday/lists-variable-exists"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-someday.el test/unit/someday-test.el
git commit -m "$(cat <<'EOF'
feat(someday): add org-gtd-someday-lists configuration variable

Adds customization variable for grouping someday items into lists.
Defaults to nil for backwards compatibility (review all items together).
EOF
)"
```

---

## Task 2: Add ORG_GTD_SOMEDAY_LIST Property Constant

**Files:**
- Modify: `org-gtd-core.el`
- Test: `test/unit/core-test.el`

**Step 1: Write the failing test**

Add to `test/unit/core-test.el`:

```elisp
(deftest core/someday-list-property-constant-exists ()
  "org-gtd-prop-someday-list constant exists."
  (assert-equal "ORG_GTD_SOMEDAY_LIST" org-gtd-prop-someday-list))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "core/someday-list-property-constant"`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-core.el` in the Constants section (near other property constants):

```elisp
(defconst org-gtd-prop-someday-list "ORG_GTD_SOMEDAY_LIST"
  "Property for categorizing someday/maybe items into lists.")
```

**Step 4: Run test to verify it passes**

Run: `~/.local/bin/eldev etest -B "core/someday-list-property-constant"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-core.el test/unit/core-test.el
git commit -m "$(cat <<'EOF'
feat(core): add ORG_GTD_SOMEDAY_LIST property constant

Property used to categorize someday/maybe items into named lists
for filtered review.
EOF
)"
```

---

## Task 3: Add List Prompt to Someday Organization

**Files:**
- Modify: `org-gtd-someday.el`
- Test: `test/unit/someday-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-test.el`:

```elisp
(deftest someday/prompts-for-list-when-lists-configured ()
  "Prompts for list selection when org-gtd-someday-lists is configured."
  (let ((org-gtd-someday-lists '("Work Ideas" "Personal")))
    (ogt--with-temp-org-buffer
     "* Test item"
     (org-back-to-heading t)
     (with-simulated-input "Work Ideas RET"
       (org-gtd-someday--configure))
     (assert-equal "Work Ideas" (org-entry-get (point) "ORG_GTD_SOMEDAY_LIST")))))

(deftest someday/skips-list-prompt-when-no-lists-configured ()
  "Skips list prompt when org-gtd-someday-lists is nil."
  (let ((org-gtd-someday-lists nil))
    (ogt--with-temp-org-buffer
     "* Test item"
     (org-back-to-heading t)
     (org-gtd-someday--configure)
     (assert-nil (org-entry-get (point) "ORG_GTD_SOMEDAY_LIST")))))
```

**Step 2: Run tests to verify they fail**

Run: `~/.local/bin/eldev etest -B "someday/prompts-for-list"`
Expected: FAIL (no prompt implemented)

**Step 3: Write minimal implementation**

Modify `org-gtd-someday--configure` in `org-gtd-someday.el`:

```elisp
(defun org-gtd-someday--configure ()
  "Configure item at point as someday/maybe.

Saves current state to PREVIOUS_* properties, then sets ORG_GTD
property to Someday, clears TODO keyword, and removes any timestamp properties.
If `org-gtd-someday-lists' is configured, prompts for list selection."
  ;; Save current state before changing type
  (org-gtd-save-state)

  ;; Configure as someday type (no properties needed - no timestamps!)
  (org-gtd-configure-as-type 'someday)

  ;; Prompt for list if configured
  (when org-gtd-someday-lists
    (let ((list (completing-read "Someday list: " org-gtd-someday-lists nil t)))
      (org-entry-put nil org-gtd-prop-someday-list list)))

  ;; Clear TODO keyword - someday items are not actionable
  (org-todo "")

  ;; Explicitly remove any timestamp properties that might exist
  (org-entry-delete (point) org-gtd-timestamp)
  (org-entry-delete (point) "SCHEDULED")
  (org-entry-delete (point) "DEADLINE"))
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday/prompts-for-list" && ~/.local/bin/eldev etest -B "someday/skips-list-prompt"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-someday.el test/unit/someday-test.el
git commit -m "$(cat <<'EOF'
feat(someday): prompt for list during organization

When org-gtd-someday-lists is configured, prompts user to select
which list the someday item belongs to. Skips prompt when lists
are not configured.
EOF
)"
```

---

## Task 4: Create LOGBOOK Entry Helper Function

**Files:**
- Create: `org-gtd-someday-review.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Create `test/unit/someday-review-test.el`:

```elisp
;;; someday-review-test.el --- Tests for someday review -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;;; Commentary:
;;
;; Tests for someday/maybe review functionality.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

;;; LOGBOOK Entry Tests

(deftest someday-review/adds-reviewed-entry-to-logbook ()
  "Adds 'Reviewed' entry to LOGBOOK drawer."
  (ogt--with-temp-org-buffer
   "* Test item
:PROPERTIES:
:ORG_GTD: Someday
:END:"
   (org-back-to-heading t)
   (org-gtd-someday-review--add-reviewed-entry)
   (let ((content (buffer-string)))
     (assert-match ":LOGBOOK:" content)
     (assert-match "- Reviewed \\[" content))))

(deftest someday-review/preserves-existing-logbook-entries ()
  "Preserves existing LOGBOOK entries when adding new one."
  (ogt--with-temp-org-buffer
   "* Test item
:PROPERTIES:
:ORG_GTD: Someday
:END:
:LOGBOOK:
- Previous note [2025-01-01 Wed]
:END:"
   (org-back-to-heading t)
   (org-gtd-someday-review--add-reviewed-entry)
   (let ((content (buffer-string)))
     (assert-match "Previous note" content)
     (assert-match "- Reviewed \\[" content))))

(provide 'someday-review-test)

;;; someday-review-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/adds-reviewed"`
Expected: FAIL with "Symbol's function definition is void"

**Step 3: Write minimal implementation**

Create `org-gtd-someday-review.el`:

```elisp
;;; org-gtd-someday-review.el --- Review someday/maybe items -*- lexical-binding: t; coding: utf-8 -*-
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
;; Iterative review of someday/maybe items.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-gtd-core)

;;;; Functions

;;;;; Private

(defun org-gtd-someday-review--add-reviewed-entry ()
  "Add a 'Reviewed' entry to the LOGBOOK drawer at point."
  (save-excursion
    (org-back-to-heading t)
    (let ((log-pos (org-log-beginning t)))
      (goto-char log-pos)
      (insert (format "- Reviewed %s\n"
                      (format-time-string "[%Y-%m-%d %a %H:%M]"))))))

;;;; Footer

(provide 'org-gtd-someday-review)

;;; org-gtd-someday-review.el ends here
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/adds-reviewed" && ~/.local/bin/eldev etest -B "someday-review/preserves-existing"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): add LOGBOOK entry helper function

Creates org-gtd-someday-review.el with function to add timestamped
"Reviewed" entries to the LOGBOOK drawer.
EOF
)"
```

---

## Task 5: Create Function to Find Someday Items

**Files:**
- Modify: `org-gtd-someday-review.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-review-test.el`:

```elisp
;;; Finding Someday Items Tests

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest someday-review/finds-all-someday-items ()
  "Finds all items with ORG_GTD: Someday."
  (org-gtd-someday-create "Item one")
  (org-gtd-someday-create "Item two")
  (let ((items (org-gtd-someday-review--find-items nil)))
    (assert-equal 2 (length items))))

(deftest someday-review/filters-by-list-property ()
  "Filters items by ORG_GTD_SOMEDAY_LIST property."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    ;; Create items with different lists
    (with-simulated-input "Work RET"
      (org-gtd-someday-create "Work idea"))
    (with-simulated-input "Personal RET"
      (org-gtd-someday-create "Personal idea")))
  (let ((work-items (org-gtd-someday-review--find-items "Work")))
    (assert-equal 1 (length work-items))))

(deftest someday-review/finds-unassigned-items ()
  "Finds items without ORG_GTD_SOMEDAY_LIST when filtering for unassigned."
  (let ((org-gtd-someday-lists nil))
    (org-gtd-someday-create "Unassigned item"))
  (let ((org-gtd-someday-lists '("Work")))
    (with-simulated-input "Work RET"
      (org-gtd-someday-create "Work item")))
  (let ((unassigned (org-gtd-someday-review--find-items 'unassigned)))
    (assert-equal 1 (length unassigned))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/finds-all-someday"`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-someday-review.el`:

```elisp
(defun org-gtd-someday-review--find-items (list-filter)
  "Find someday items, optionally filtered by LIST-FILTER.
LIST-FILTER can be:
  - nil: find all someday items
  - a string: find items with matching ORG_GTD_SOMEDAY_LIST
  - symbol `unassigned': find items without ORG_GTD_SOMEDAY_LIST"
  (let ((items '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (when (string= (org-entry-get (point) "ORG_GTD") org-gtd-someday)
               (let ((item-list (org-entry-get (point) org-gtd-prop-someday-list)))
                 (when (org-gtd-someday-review--item-matches-filter-p item-list list-filter)
                   (push (org-id-get-create) items)))))))))
    (nreverse items)))

(defun org-gtd-someday-review--item-matches-filter-p (item-list list-filter)
  "Return t if ITEM-LIST matches LIST-FILTER.
ITEM-LIST is the value of ORG_GTD_SOMEDAY_LIST property (or nil).
LIST-FILTER is nil (match all), a string (match exact), or `unassigned'."
  (cond
   ((null list-filter) t)
   ((eq list-filter 'unassigned) (null item-list))
   ((stringp list-filter) (equal item-list list-filter))
   (t nil)))
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/finds"`
Expected: PASS (all 3 tests)

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): add function to find someday items

Finds items with ORG_GTD: Someday across all agenda files.
Supports filtering by list name or finding unassigned items.
EOF
)"
```

---

## Task 6: Create Review Session State

**Files:**
- Modify: `org-gtd-someday-review.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-review-test.el`:

```elisp
;;; Review Session State Tests

(deftest someday-review/initializes-session-state ()
  "Initializes review session state with item queue."
  (org-gtd-someday-create "Item one")
  (org-gtd-someday-create "Item two")
  (org-gtd-someday-review--start-session nil)
  (assert-true org-gtd-someday-review--session-active)
  (assert-equal 2 (length (plist-get org-gtd-someday-review--state :queue)))
  (assert-equal 0 (plist-get org-gtd-someday-review--state :position))
  ;; Cleanup
  (org-gtd-someday-review--end-session))

(deftest someday-review/tracks-statistics ()
  "Tracks review statistics (reviewed count, clarified count)."
  (org-gtd-someday-create "Item one")
  (org-gtd-someday-review--start-session nil)
  (assert-equal 0 (plist-get org-gtd-someday-review--state :reviewed))
  (assert-equal 0 (plist-get org-gtd-someday-review--state :clarified))
  ;; Cleanup
  (org-gtd-someday-review--end-session))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/initializes-session"`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-someday-review.el`:

```elisp
;;;; Variables

(defvar org-gtd-someday-review--session-active nil
  "Non-nil when a someday review session is active.")

(defvar org-gtd-someday-review--state nil
  "State for active someday review session.
Plist with :queue (list of org-ids), :position (current index),
:list-name (which list being reviewed), :reviewed (count),
:clarified (count).")

;;;; Functions

;;;;; Session Management

(defun org-gtd-someday-review--start-session (list-filter)
  "Start a review session for items matching LIST-FILTER."
  (let ((items (org-gtd-someday-review--find-items list-filter)))
    (setq org-gtd-someday-review--session-active t
          org-gtd-someday-review--state
          (list :queue items
                :position 0
                :list-name list-filter
                :reviewed 0
                :clarified 0))))

(defun org-gtd-someday-review--end-session ()
  "End the current review session."
  (let ((reviewed (plist-get org-gtd-someday-review--state :reviewed))
        (clarified (plist-get org-gtd-someday-review--state :clarified)))
    (setq org-gtd-someday-review--session-active nil
          org-gtd-someday-review--state nil)
    (message "Review complete. %d items reviewed, %d clarified." reviewed clarified)))
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/initializes-session" && ~/.local/bin/eldev etest -B "someday-review/tracks-statistics"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): add review session state management

Tracks active review session with item queue, position,
and statistics (reviewed/clarified counts).
EOF
)"
```

---

## Task 7: Create Review Buffer Display (Using WIP Infrastructure)

**Files:**
- Modify: `org-gtd-someday-review.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-review-test.el`:

```elisp
;;; Review Buffer Tests

(deftest someday-review/creates-wip-buffer-with-review-mode ()
  "Creates WIP buffer with review mode active."
  (org-gtd-someday-create "Review me")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  ;; Should use WIP buffer infrastructure
  (let ((bufs (org-gtd-wip--get-buffers)))
    (assert-true (> (length bufs) 0))
    (with-current-buffer (car bufs)
      (assert-true (eq major-mode 'org-gtd-someday-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Review me" (buffer-string))))
  ;; Cleanup
  (org-gtd-someday-review--cleanup-current-buffer)
  (org-gtd-someday-review--end-session))

(deftest someday-review/shows-keybindings-in-header-line ()
  "Shows available keybindings in header-line."
  (org-gtd-someday-create "Review me")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  (let ((bufs (org-gtd-wip--get-buffers)))
    (with-current-buffer (car bufs)
      (assert-match "\\[d\\]" header-line-format)
      (assert-match "\\[c\\]" header-line-format)
      (assert-match "\\[q\\]" header-line-format)))
  ;; Cleanup
  (org-gtd-someday-review--cleanup-current-buffer)
  (org-gtd-someday-review--end-session))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/creates-wip-buffer"`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-someday-review.el`:

```elisp
(require 'org-gtd-wip)

;;;; Variables (add to existing section)

(defvar-local org-gtd-someday-review--current-item-id nil
  "ID of the item currently being reviewed in this buffer.")

;;;; Functions

;;;;; Buffer Display

(defun org-gtd-someday-review--display-current-item ()
  "Display the current item in a WIP review buffer."
  (let* ((queue (plist-get org-gtd-someday-review--state :queue))
         (pos (plist-get org-gtd-someday-review--state :position))
         (item-id (nth pos queue))
         (marker (org-id-find item-id 'marker)))
    (when marker
      ;; Clean up previous review buffer if exists
      (org-gtd-someday-review--cleanup-current-buffer)
      ;; Get WIP buffer for this item
      (let ((buf (org-gtd-wip--get-buffer item-id)))
        (org-gtd-someday-review--initialize-buffer marker buf)
        (with-current-buffer buf
          (org-gtd-someday-review-mode)
          (setq org-gtd-someday-review--current-item-id item-id)
          (setq buffer-read-only t)
          (setq header-line-format
                (format "[d] Defer  [c] Clarify  [q] Quit  (%d/%d)"
                        (1+ pos) (length queue))))
        (pop-to-buffer buf)))))

(defun org-gtd-someday-review--initialize-buffer (marker buffer)
  "Initialize BUFFER with content from item at MARKER."
  (when (= (buffer-size buffer) 0)
    (let ((last-command nil))
      (org-with-point-at marker
        (org-copy-subtree))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (org-paste-subtree)
          (goto-char (point-min)))))))

(defun org-gtd-someday-review--cleanup-current-buffer ()
  "Clean up the current review WIP buffer."
  (when-let ((item-id (plist-get org-gtd-someday-review--state :current-buffer-id)))
    (org-gtd-wip--cleanup-temp-file item-id)
    (plist-put org-gtd-someday-review--state :current-buffer-id nil)))
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/creates-read-only" && ~/.local/bin/eldev etest -B "someday-review/shows-keybindings"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): add review buffer display

Creates read-only buffer showing current item content with
header-line displaying available keybindings and position.
EOF
)"
```

---

## Task 8: Create Review Major Mode with Keybindings

**Files:**
- Modify: `org-gtd-someday-review.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-review-test.el`:

```elisp
;;; Review Mode Keybinding Tests

(deftest someday-review/mode-has-defer-keybinding ()
  "Review mode has 'd' bound to defer command."
  (assert-equal 'org-gtd-someday-review-defer
                (lookup-key org-gtd-someday-review-mode-map (kbd "d"))))

(deftest someday-review/mode-has-clarify-keybinding ()
  "Review mode has 'c' bound to clarify command."
  (assert-equal 'org-gtd-someday-review-clarify
                (lookup-key org-gtd-someday-review-mode-map (kbd "c"))))

(deftest someday-review/mode-has-quit-keybinding ()
  "Review mode has 'q' bound to quit command."
  (assert-equal 'org-gtd-someday-review-quit
                (lookup-key org-gtd-someday-review-mode-map (kbd "q"))))

(deftest someday-review/mode-is-derived-from-org-mode ()
  "Review mode is derived from org-mode."
  (with-temp-buffer
    (org-gtd-someday-review-mode)
    (assert-true (derived-mode-p 'org-mode))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/mode-has-defer"`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-someday-review.el`:

```elisp
;;;; Keymaps

(defvar org-gtd-someday-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "d") #'org-gtd-someday-review-defer)
    (define-key map (kbd "c") #'org-gtd-someday-review-clarify)
    (define-key map (kbd "q") #'org-gtd-someday-review-quit)
    map)
  "Keymap for `org-gtd-someday-review-mode'.")

;;;; Modes

;;;###autoload
(define-derived-mode org-gtd-someday-review-mode org-mode "GTD-Review"
  "Major mode for reviewing someday/maybe items.
Derived from `org-mode' and provides read-only review interface
with keybindings for defer, clarify, and quit actions.

\\{org-gtd-someday-review-mode-map}"
  :group 'org-gtd
  (setq buffer-read-only t))

;;;; Commands

(defun org-gtd-someday-review-defer ()
  "Defer the current item and advance to next."
  (interactive)
  ;; TODO: implement
  (message "Defer not yet implemented"))

(defun org-gtd-someday-review-clarify ()
  "Clarify (reactivate) the current item and advance to next."
  (interactive)
  ;; TODO: implement
  (message "Clarify not yet implemented"))

(defun org-gtd-someday-review-quit ()
  "Quit the review session."
  (interactive)
  ;; TODO: implement
  (message "Quit not yet implemented"))
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/mode-has"`
Expected: PASS (all 3 tests)

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): add minor mode with keybindings

Creates org-gtd-someday-review-mode with d/c/q keybindings
for defer, clarify, and quit actions.
EOF
)"
```

---

## Task 9: Implement Defer Command

**Files:**
- Modify: `org-gtd-someday-review.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-review-test.el`:

```elisp
;;; Defer Command Tests

(deftest someday-review/defer-adds-logbook-entry ()
  "Defer command adds reviewed entry to item's LOGBOOK."
  (org-gtd-someday-create "Defer me")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  (org-gtd-someday-review-defer)
  ;; Check the source item has LOGBOOK entry
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Defer me")
    (assert-match ":LOGBOOK:" (buffer-substring (point-at-bol) (org-end-of-subtree))))
  ;; Cleanup
  (org-gtd-someday-review--cleanup-current-buffer)
  (org-gtd-someday-review--end-session))

(deftest someday-review/defer-advances-to-next-item ()
  "Defer command advances to the next item."
  (org-gtd-someday-create "First item")
  (org-gtd-someday-create "Second item")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  (org-gtd-someday-review-defer)
  (assert-equal 1 (plist-get org-gtd-someday-review--state :position))
  (assert-equal 1 (plist-get org-gtd-someday-review--state :reviewed))
  ;; Cleanup
  (org-gtd-someday-review--cleanup-current-buffer)
  (org-gtd-someday-review--end-session))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/defer-adds-logbook"`
Expected: FAIL

**Step 3: Write minimal implementation**

Update `org-gtd-someday-review-defer` in `org-gtd-someday-review.el`:

```elisp
(defun org-gtd-someday-review-defer ()
  "Defer the current item and advance to next."
  (interactive)
  (let* ((queue (plist-get org-gtd-someday-review--state :queue))
         (pos (plist-get org-gtd-someday-review--state :position))
         (item-id (nth pos queue))
         (marker (org-id-find item-id 'marker)))
    ;; Add LOGBOOK entry to source item
    (when marker
      (org-with-point-at marker
        (org-gtd-someday-review--add-reviewed-entry)
        (save-buffer)))
    ;; Update statistics
    (plist-put org-gtd-someday-review--state :reviewed
               (1+ (plist-get org-gtd-someday-review--state :reviewed)))
    ;; Advance to next
    (org-gtd-someday-review--advance)))

(defun org-gtd-someday-review--advance ()
  "Advance to the next item or end session if done."
  (let* ((queue (plist-get org-gtd-someday-review--state :queue))
         (pos (plist-get org-gtd-someday-review--state :position))
         (next-pos (1+ pos)))
    (if (< next-pos (length queue))
        (progn
          (plist-put org-gtd-someday-review--state :position next-pos)
          (org-gtd-someday-review--display-current-item))
      ;; No more items
      (org-gtd-someday-review--cleanup-and-end))))

(defun org-gtd-someday-review--cleanup-and-end ()
  "Clean up review buffer and end session."
  (org-gtd-someday-review--cleanup-current-buffer)
  (org-gtd-someday-review--end-session))
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/defer"`
Expected: PASS (both tests)

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): implement defer command

Defer adds LOGBOOK entry to source item, updates statistics,
and advances to next item in queue.
EOF
)"
```

---

## Task 10: Implement Clarify Command

**Files:**
- Modify: `org-gtd-someday-review.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-review-test.el`:

```elisp
;;; Clarify Command Tests

(deftest someday-review/clarify-calls-reactivate ()
  "Clarify command calls org-gtd-reactivate on the item."
  (ogt--with-temp-org-buffer
   "* Test item
:PROPERTIES:
:ID: clarify-test-id
:ORG_GTD: Someday
:PREVIOUS_ORG_GTD: Actions
:PREVIOUS_TODO: NEXT
:END:"
   (org-back-to-heading t)
   (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
     ;; Set up minimal review state
     (setq org-gtd-someday-review--session-active t
           org-gtd-someday-review--state
           (list :queue (list "clarify-test-id")
                 :position 0
                 :reviewed 0
                 :clarified 0))
     ;; Spy on reactivate
     (with-spy org-gtd-reactivate spy
       (with-simulated-input "RET"
         (org-gtd-someday-review--clarify-current-item))
       (assert-true (spy-called-p spy)))))
  ;; Cleanup
  (setq org-gtd-someday-review--session-active nil
        org-gtd-someday-review--state nil))

(deftest someday-review/clarify-increments-clarified-count ()
  "Clarify command increments the clarified count."
  (org-gtd-someday-create "Clarify me")
  (org-gtd-someday-review--start-session nil)
  ;; Set up so reactivate triggers clarify (no PREVIOUS_ORG_GTD)
  (with-spy org-gtd-clarify-item ignored
    (org-gtd-someday-review--clarify-current-item))
  (assert-equal 1 (plist-get org-gtd-someday-review--state :clarified))
  ;; Cleanup
  (org-gtd-someday-review--end-session))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/clarify-calls"`
Expected: FAIL

**Step 3: Write minimal implementation**

Update `org-gtd-someday-review-clarify` and add helper in `org-gtd-someday-review.el`:

```elisp
(require 'org-gtd-reactivate)

(defun org-gtd-someday-review-clarify ()
  "Clarify (reactivate) the current item and advance to next."
  (interactive)
  (org-gtd-someday-review--clarify-current-item)
  ;; Update statistics
  (plist-put org-gtd-someday-review--state :clarified
             (1+ (plist-get org-gtd-someday-review--state :clarified)))
  ;; Advance to next
  (org-gtd-someday-review--advance))

(defun org-gtd-someday-review--clarify-current-item ()
  "Call org-gtd-reactivate on the current item."
  (let* ((queue (plist-get org-gtd-someday-review--state :queue))
         (pos (plist-get org-gtd-someday-review--state :position))
         (item-id (nth pos queue))
         (marker (org-id-find item-id 'marker)))
    (when marker
      (org-with-point-at marker
        (org-gtd-reactivate)))))
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/clarify"`
Expected: PASS (both tests)

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): implement clarify command

Clarify calls org-gtd-reactivate on the current item,
increments clarified count, and advances to next item.
EOF
)"
```

---

## Task 11: Implement Quit Command

**Files:**
- Modify: `org-gtd-someday-review.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-review-test.el`:

```elisp
;;; Quit Command Tests

(deftest someday-review/quit-ends-session ()
  "Quit command ends the review session."
  (org-gtd-someday-create "Item")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  (let ((wip-count-before (length (org-gtd-wip--get-buffers))))
    (org-gtd-someday-review-quit)
    (assert-nil org-gtd-someday-review--session-active)
    ;; WIP buffer should be cleaned up
    (assert-true (<= (length (org-gtd-wip--get-buffers)) wip-count-before))))

(deftest someday-review/quit-shows-partial-stats ()
  "Quit shows statistics for items reviewed before quitting."
  (org-gtd-someday-create "First")
  (org-gtd-someday-create "Second")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  (org-gtd-someday-review-defer) ; Review one item
  ;; Now quit - should show 1 reviewed
  (with-spy message spy
    (org-gtd-someday-review-quit)
    (assert-match "1 items reviewed" (car (spy-last-call-args spy)))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/quit-ends"`
Expected: FAIL

**Step 3: Write minimal implementation**

Update `org-gtd-someday-review-quit` in `org-gtd-someday-review.el`:

```elisp
(defun org-gtd-someday-review-quit ()
  "Quit the review session."
  (interactive)
  (org-gtd-someday-review--cleanup-and-end))
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/quit"`
Expected: PASS (both tests)

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): implement quit command

Quit ends session, cleans up buffer, and shows statistics
for items reviewed before quitting.
EOF
)"
```

---

## Task 12: Create Main Entry Point Command

**Files:**
- Modify: `org-gtd-someday-review.el`
- Modify: `org-gtd-reflect.el`
- Test: `test/unit/someday-review-test.el`

**Step 1: Write the failing test**

Add to `test/unit/someday-review-test.el`:

```elisp
;;; Entry Point Tests

(deftest someday-review/entry-point-starts-session ()
  "org-gtd-reflect-someday-review starts a review session."
  (org-gtd-someday-create "Item")
  (org-gtd-reflect-someday-review)
  (assert-true org-gtd-someday-review--session-active)
  ;; Should create a WIP buffer
  (assert-true (> (length (org-gtd-wip--get-buffers)) 0))
  ;; Cleanup
  (org-gtd-someday-review-quit))

(deftest someday-review/entry-point-prompts-for-list-when-configured ()
  "Entry point prompts for list when org-gtd-someday-lists is configured."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    (with-simulated-input "Work RET"
      (org-gtd-someday-create "Work item"))
    (with-simulated-input "Work RET"
      (org-gtd-reflect-someday-review))
    (assert-equal "Work" (plist-get org-gtd-someday-review--state :list-name)))
  ;; Cleanup
  (org-gtd-someday-review-quit))

(deftest someday-review/entry-point-accepts-list-argument ()
  "Entry point accepts optional list argument to skip prompt."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    (with-simulated-input "Work RET"
      (org-gtd-someday-create "Work item"))
    (org-gtd-reflect-someday-review "Work")
    (assert-equal "Work" (plist-get org-gtd-someday-review--state :list-name)))
  ;; Cleanup
  (org-gtd-someday-review-quit))

(deftest someday-review/shows-message-when-no-items ()
  "Shows message when no items to review."
  ;; No items created
  (with-spy message spy
    (org-gtd-reflect-someday-review)
    (assert-match "No someday" (car (spy-last-call-args spy)))))
```

**Step 2: Run test to verify it fails**

Run: `~/.local/bin/eldev etest -B "someday-review/entry-point-starts"`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-someday-review.el`:

```elisp
;;;; Commands

;;;###autoload
(defun org-gtd-reflect-someday-review (&optional list)
  "Review someday/maybe items one at a time.
With optional LIST argument, review only items in that list.
When `org-gtd-someday-lists' is configured, prompts for list selection.
Adds 'Unassigned' option for items without a list."
  (interactive
   (list (when org-gtd-someday-lists
           (completing-read "Review which list? "
                            (append org-gtd-someday-lists '("Unassigned"))
                            nil t))))
  (let ((list-filter (cond
                      ((equal list "Unassigned") 'unassigned)
                      ((and list (not (string-empty-p list))) list)
                      (t nil))))
    (org-gtd-someday-review--start-session list-filter)
    (if (zerop (length (plist-get org-gtd-someday-review--state :queue)))
        (progn
          (org-gtd-someday-review--end-session)
          (message "No someday items to review."))
      (org-gtd-someday-review--display-current-item))))
```

Also add autoload to `org-gtd-reflect.el` requires section:

```elisp
(require 'org-gtd-someday-review)
```

**Step 4: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "someday-review/entry-point"`
Expected: PASS (all 4 tests)

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el org-gtd-reflect.el test/unit/someday-review-test.el
git commit -m "$(cat <<'EOF'
feat(someday-review): add main entry point command

org-gtd-reflect-someday-review starts iterative review.
Prompts for list when configured, accepts optional list argument,
shows message when no items to review.
EOF
)"
```

---

## Task 13: Add org-gtd-someday-review.el to Package

**Files:**
- Modify: `org-gtd.el`

**Step 1: Verify current package loading**

Run: `~/.local/bin/eldev etest -B`
Expected: All tests pass (verify no regressions)

**Step 2: Add require to org-gtd.el**

Add to the requires section in `org-gtd.el`:

```elisp
(require 'org-gtd-someday-review)
```

**Step 3: Run full test suite**

Run: `~/.local/bin/eldev etest -B`
Expected: All tests pass

**Step 4: Commit**

```bash
git add org-gtd.el
git commit -m "$(cat <<'EOF'
feat: integrate someday review into main package

Adds require for org-gtd-someday-review to main package file.
EOF
)"
```

---

## Task 14: Run Full Test Suite and Verify

**Step 1: Run full test suite**

Run: `~/.local/bin/eldev clean && ~/.local/bin/eldev compile && ~/.local/bin/eldev etest -B`
Expected: All tests pass, no compilation warnings

**Step 2: Manual verification**

Create a test GTD setup and verify:
1. `M-x org-gtd-reflect-someday-review` works
2. `d` defers and adds LOGBOOK entry
3. `c` clarifies (calls reactivate)
4. `q` quits cleanly
5. Statistics shown on completion

**Step 3: Final commit if any fixes needed**

---

## Summary

13 implementation tasks covering:
1. Configuration variable (`org-gtd-someday-lists`)
2. Property constant (`ORG_GTD_SOMEDAY_LIST`)
3. List prompt during organize
4. LOGBOOK entry helper
5. Find someday items function
6. Review session state
7. Review buffer display
8. Minor mode with keybindings
9. Defer command
10. Clarify command
11. Quit command
12. Main entry point
13. Package integration
14. Final verification
