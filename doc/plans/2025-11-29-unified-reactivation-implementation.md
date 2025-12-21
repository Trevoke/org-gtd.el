# Unified Reactivation System Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement unified save/restore state for all GTD item types when moving to/from someday/tickler.

**Architecture:** Create `org-gtd-reactivate.el` with generic `org-gtd-save-state` and `org-gtd-restore-state` functions that use the type system to determine which properties to preserve. Integrate with someday/tickler modules and refactor project code to use the generic functions.

**Tech Stack:** Emacs Lisp, Buttercup testing framework, eldev build tool

---

## Task 1: Create org-gtd-reactivate.el with save-state

**Files:**
- Create: `org-gtd-reactivate.el`
- Test: `test/reactivate-test.el`

**Step 1: Write failing test for save-state with delegated item**

Create `test/reactivate-test.el`:

```elisp
;;; reactivate-test.el --- Tests for reactivation -*- lexical-binding: t; -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

(describe "org-gtd-save-state"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "saves delegated item state to PREVIOUS_* properties"
    (ogt--with-temp-org-buffer
     "* Test task
:PROPERTIES:
:ID: test-id
:ORG_GTD: Delegated
:DELEGATED_TO: John Doe
:ORG_GTD_TIMESTAMP: <2024-06-15>
:END:"
     (org-back-to-heading t)
     (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
       (org-todo "WAIT"))
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Delegated")
     (expect (org-entry-get (point) "PREVIOUS_TODO") :to-equal "WAIT")
     (expect (org-entry-get (point) "PREVIOUS_DELEGATED_TO") :to-equal "John Doe")
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP") :to-equal "<2024-06-15>"))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "saves delegated item state"`
Expected: FAIL - org-gtd-save-state not defined

**Step 3: Create org-gtd-reactivate.el with save-state implementation**

Create `org-gtd-reactivate.el`:

```elisp
;;; org-gtd-reactivate.el --- Save and restore GTD state -*- lexical-binding: t; coding: utf-8 -*-
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
;; Save and restore GTD item state when moving to/from someday/tickler.
;; Uses the type system to determine which properties to preserve.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-types)

;;;; Functions

(defun org-gtd-save-state ()
  "Save current GTD state to PREVIOUS_* properties.

Only saves if item has ORG_GTD that is not someday/tickler.
Saves ORG_GTD, TODO state, and all type-specific properties."
  (let* ((current-org-gtd (org-entry-get (point) "ORG_GTD"))
         (current-type (when current-org-gtd
                         (org-gtd-type-from-org-gtd-value current-org-gtd))))
    ;; Only save if not already someday/tickler
    (when (and current-org-gtd
               (not (member current-type '(someday tickler))))
      ;; Save ORG_GTD
      (org-entry-put (point) "PREVIOUS_ORG_GTD" current-org-gtd)
      ;; Save TODO state
      (when-let ((todo (org-entry-get (point) "TODO")))
        (org-entry-put (point) "PREVIOUS_TODO" todo))
      ;; Save type-specific properties
      (dolist (prop (org-gtd-type-properties current-type))
        (let ((org-prop (plist-get (cdr prop) :org-property)))
          (when-let ((val (org-entry-get (point) org-prop)))
            (org-entry-put (point) (concat "PREVIOUS_" org-prop) val)))))))

;;;; Footer

(provide 'org-gtd-reactivate)

;;; org-gtd-reactivate.el ends here
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "saves delegated item state"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-reactivate.el test/reactivate-test.el
git commit -m "feat: add org-gtd-save-state for preserving GTD state"
```

---

## Task 2: Add test for save-state with calendar item

**Files:**
- Modify: `test/reactivate-test.el`

**Step 1: Write failing test for calendar item**

Add to `test/reactivate-test.el`:

```elisp
  (it "saves calendar item state to PREVIOUS_* properties"
    (ogt--with-temp-org-buffer
     "* Appointment
:PROPERTIES:
:ID: cal-id
:ORG_GTD: Calendar
:ORG_GTD_TIMESTAMP: <2024-07-20>
:END:"
     (org-back-to-heading t)
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Calendar")
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP") :to-equal "<2024-07-20>")))
```

**Step 2: Run test to verify it passes**

Run: `eldev test -B "saves calendar item state"`
Expected: PASS (implementation already handles this)

**Step 3: Commit**

```bash
git add test/reactivate-test.el
git commit -m "test: add calendar item save-state test"
```

---

## Task 3: Add test for save-state skipping someday/tickler items

**Files:**
- Modify: `test/reactivate-test.el`

**Step 1: Write test for no-op on someday item**

Add to `test/reactivate-test.el`:

```elisp
  (it "does not save state for someday items"
    (ogt--with-temp-org-buffer
     "* Someday item
:PROPERTIES:
:ID: someday-id
:ORG_GTD: Someday
:END:"
     (org-back-to-heading t)
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)))

  (it "does not save state for tickler items"
    (ogt--with-temp-org-buffer
     "* Tickler item
:PROPERTIES:
:ID: tickler-id
:ORG_GTD: Tickler
:ORG_GTD_TIMESTAMP: <2024-08-01>
:END:"
     (org-back-to-heading t)
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)))

  (it "does not save state for items without ORG_GTD"
    (ogt--with-temp-org-buffer
     "* Inbox item
:PROPERTIES:
:ID: inbox-id
:END:"
     (org-back-to-heading t)
     (org-gtd-save-state)
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)))
```

**Step 2: Run tests to verify they pass**

Run: `eldev test -B "does not save state"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/reactivate-test.el
git commit -m "test: verify save-state skips someday/tickler/inbox items"
```

---

## Task 4: Implement restore-state

**Files:**
- Modify: `org-gtd-reactivate.el`
- Modify: `test/reactivate-test.el`

**Step 1: Write failing test for restore-state**

Add to `test/reactivate-test.el`:

```elisp
(describe "org-gtd-restore-state"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "restores delegated item from PREVIOUS_* properties"
    (ogt--with-temp-org-buffer
     "* Test task
:PROPERTIES:
:ID: test-id
:ORG_GTD: Someday
:PREVIOUS_ORG_GTD: Delegated
:PREVIOUS_TODO: WAIT
:PREVIOUS_DELEGATED_TO: John Doe
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-06-15>
:END:"
     (org-back-to-heading t)
     (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
       ;; Mock read-string to return previous values
       (spy-on 'read-string :and-call-fake
               (lambda (prompt &optional initial-input history default-value)
                 default-value))
       (org-gtd-restore-state)
       ;; Verify restored
       (expect (org-entry-get (point) "ORG_GTD") :to-equal "Delegated")
       (expect (org-entry-get (point) "TODO") :to-equal "WAIT")
       (expect (org-entry-get (point) "DELEGATED_TO") :to-equal "John Doe")
       (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2024-06-15>")
       ;; Verify PREVIOUS_* cleaned up
       (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)
       (expect (org-entry-get (point) "PREVIOUS_TODO") :to-be nil)
       (expect (org-entry-get (point) "PREVIOUS_DELEGATED_TO") :to-be nil)
       (expect (org-entry-get (point) "PREVIOUS_ORG_GTD_TIMESTAMP") :to-be nil)))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "restores delegated item"`
Expected: FAIL - org-gtd-restore-state not defined

**Step 3: Add restore-state implementation**

Add to `org-gtd-reactivate.el` before the Footer section:

```elisp
(declare-function org-gtd-clarify-item "org-gtd-clarify")

(defun org-gtd-restore-state ()
  "Restore GTD state from PREVIOUS_* properties.

If PREVIOUS_ORG_GTD is nil, clears ORG_GTD and calls `org-gtd-clarify-item'.
Otherwise, restores ORG_GTD, TODO, and type-specific properties.
Prompts user to confirm/update each type-specific property."
  (let ((previous-org-gtd (org-entry-get (point) "PREVIOUS_ORG_GTD")))

    ;; Clear someday/tickler-specific properties
    (org-entry-delete (point) "ORG_GTD_TIMESTAMP")

    (if (null previous-org-gtd)
        ;; No saved state - re-clarify
        (progn
          (org-entry-delete (point) "ORG_GTD")
          (require 'org-gtd-clarify)
          (org-gtd-clarify-item))

      ;; Has saved state - restore with prompts
      (let ((previous-type (org-gtd-type-from-org-gtd-value previous-org-gtd)))
        ;; Restore ORG_GTD
        (org-entry-put (point) "ORG_GTD" previous-org-gtd)
        (org-entry-delete (point) "PREVIOUS_ORG_GTD")

        ;; Restore TODO state
        (when-let ((previous-todo (org-entry-get (point) "PREVIOUS_TODO")))
          (org-todo previous-todo)
          (org-entry-delete (point) "PREVIOUS_TODO"))

        ;; Restore type-specific properties with prompts
        (dolist (prop (org-gtd-type-properties previous-type))
          (let* ((prop-def (cdr prop))
                 (org-prop (plist-get prop-def :org-property))
                 (prompt (plist-get prop-def :prompt))
                 (previous-key (concat "PREVIOUS_" org-prop))
                 (previous-val (org-entry-get (point) previous-key)))
            (when previous-val
              (let ((new-val (read-string
                              (format "%s [%s]: " prompt previous-val)
                              nil nil previous-val)))
                (org-entry-put (point) org-prop new-val))
              (org-entry-delete (point) previous-key))))))))
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "restores delegated item"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-reactivate.el test/reactivate-test.el
git commit -m "feat: add org-gtd-restore-state for restoring GTD state"
```

---

## Task 5: Add test for restore-state with no previous state

**Files:**
- Modify: `test/reactivate-test.el`

**Step 1: Write test for clarify fallback**

Add to the `org-gtd-restore-state` describe block:

```elisp
  (it "calls clarify when no PREVIOUS_ORG_GTD exists"
    (ogt--with-temp-org-buffer
     "* Inbox item went to someday
:PROPERTIES:
:ID: direct-someday-id
:ORG_GTD: Someday
:END:"
     (org-back-to-heading t)
     (spy-on 'org-gtd-clarify-item)
     (org-gtd-restore-state)
     ;; Verify ORG_GTD cleared and clarify called
     (expect (org-entry-get (point) "ORG_GTD") :to-be nil)
     (expect 'org-gtd-clarify-item :to-have-been-called)))
```

**Step 2: Run test to verify it passes**

Run: `eldev test -B "calls clarify when no PREVIOUS"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/reactivate-test.el
git commit -m "test: verify restore-state falls back to clarify"
```

---

## Task 6: Implement interactive org-gtd-reactivate command

**Files:**
- Modify: `org-gtd-reactivate.el`
- Modify: `test/reactivate-test.el`

**Step 1: Write failing test for reactivate command**

Add to `test/reactivate-test.el`:

```elisp
(describe "org-gtd-reactivate"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "reactivates a someday item"
    (ogt--with-temp-org-buffer
     "* Delegated then someday'd
:PROPERTIES:
:ID: reactivate-id
:ORG_GTD: Someday
:PREVIOUS_ORG_GTD: Delegated
:PREVIOUS_TODO: WAIT
:PREVIOUS_DELEGATED_TO: Jane
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-09-01>
:END:"
     (org-back-to-heading t)
     (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
       (spy-on 'read-string :and-call-fake
               (lambda (prompt &optional initial-input history default-value)
                 default-value))
       (org-gtd-reactivate)
       (expect (org-entry-get (point) "ORG_GTD") :to-equal "Delegated"))))

  (it "reactivates a tickler item"
    (ogt--with-temp-org-buffer
     "* Calendar then tickler'd
:PROPERTIES:
:ID: tickler-reactivate-id
:ORG_GTD: Tickler
:ORG_GTD_TIMESTAMP: <2024-12-01>
:PREVIOUS_ORG_GTD: Calendar
:PREVIOUS_ORG_GTD_TIMESTAMP: <2024-10-15>
:END:"
     (org-back-to-heading t)
     (spy-on 'read-string :and-call-fake
             (lambda (prompt &optional initial-input history default-value)
               default-value))
     (org-gtd-reactivate)
     (expect (org-entry-get (point) "ORG_GTD") :to-equal "Calendar")
     (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2024-10-15>")))

  (it "errors on non-someday/tickler item"
    (ogt--with-temp-org-buffer
     "* Active item
:PROPERTIES:
:ID: active-id
:ORG_GTD: Delegated
:END:"
     (org-back-to-heading t)
     (expect (org-gtd-reactivate) :to-throw 'user-error))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "reactivates a someday item"`
Expected: FAIL - org-gtd-reactivate not defined

**Step 3: Add reactivate command**

Add to `org-gtd-reactivate.el` before Footer:

```elisp
;;;###autoload
(defun org-gtd-reactivate ()
  "Reactivate a someday/tickler item at point.

Restores the item to its previous GTD state, prompting to confirm
or update each type-specific property (dates, delegated-to, etc.)."
  (interactive)
  (let ((org-gtd-value (org-entry-get (point) "ORG_GTD")))
    (unless (member org-gtd-value (list org-gtd-someday org-gtd-tickler))
      (user-error "Item is not someday/tickler (ORG_GTD: %s)" org-gtd-value))
    (org-gtd-restore-state)))
```

**Step 4: Run tests to verify they pass**

Run: `eldev test -B "org-gtd-reactivate"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-reactivate.el test/reactivate-test.el
git commit -m "feat: add org-gtd-reactivate interactive command"
```

---

## Task 7: Integrate save-state with org-gtd-someday

**Files:**
- Modify: `org-gtd-someday.el`
- Modify: `test/reactivate-test.el`

**Step 1: Write failing integration test**

Add to `test/reactivate-test.el`:

```elisp
(describe "Integration: someday then reactivate"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "preserves delegated state through someday cycle"
    (ogt--with-temp-org-buffer
     "* Delegated task
:PROPERTIES:
:ID: cycle-test-id
:ORG_GTD: Delegated
:DELEGATED_TO: Bob
:ORG_GTD_TIMESTAMP: <2024-05-01>
:END:"
     (org-back-to-heading t)
     (let ((org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))))
       (org-todo "WAIT")
       ;; Someday the item
       (org-gtd-someday--configure)
       ;; Verify someday state
       (expect (org-entry-get (point) "ORG_GTD") :to-equal "Someday")
       (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Delegated")
       ;; Reactivate with mocked prompts
       (spy-on 'read-string :and-call-fake
               (lambda (prompt &optional initial-input history default-value)
                 default-value))
       (org-gtd-reactivate)
       ;; Verify restored
       (expect (org-entry-get (point) "ORG_GTD") :to-equal "Delegated")
       (expect (org-entry-get (point) "DELEGATED_TO") :to-equal "Bob")))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "preserves delegated state through someday"`
Expected: FAIL - PREVIOUS_ORG_GTD not set

**Step 3: Modify org-gtd-someday--configure to call save-state**

In `org-gtd-someday.el`, add require and modify `org-gtd-someday--configure`:

```elisp
;; Add to Requirements section:
(require 'org-gtd-reactivate)

;; Replace org-gtd-someday--configure:
(defun org-gtd-someday--configure ()
  "Configure item at point as someday/maybe.

Saves current state to PREVIOUS_* properties, then sets ORG_GTD
property to Someday and removes any timestamp properties."
  ;; Save current state before changing type
  (org-gtd-save-state)
  ;; Configure as someday type
  (org-gtd-configure-as-type 'someday)
  ;; Explicitly remove any timestamp properties
  (org-entry-delete (point) org-gtd-timestamp)
  (org-entry-delete (point) "SCHEDULED")
  (org-entry-delete (point) "DEADLINE"))
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "preserves delegated state through someday"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-someday.el test/reactivate-test.el
git commit -m "feat: integrate save-state with org-gtd-someday"
```

---

## Task 8: Integrate save-state with org-gtd-tickler

**Files:**
- Modify: `org-gtd-tickler.el`
- Modify: `test/reactivate-test.el`

**Step 1: Write failing integration test**

Add to `test/reactivate-test.el`:

```elisp
  (it "preserves calendar state through tickler cycle"
    (ogt--with-temp-org-buffer
     "* Calendar event
:PROPERTIES:
:ID: tickler-cycle-id
:ORG_GTD: Calendar
:ORG_GTD_TIMESTAMP: <2024-06-20>
:END:"
     (org-back-to-heading t)
     ;; Tickler the item (mock the date prompt)
     (spy-on 'org-gtd-prompt-for-active-date :and-return-value "<2025-01-01>")
     (org-gtd-tickler--configure)
     ;; Verify tickler state
     (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")
     (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Calendar")
     ;; Reactivate with mocked prompts
     (spy-on 'read-string :and-call-fake
             (lambda (prompt &optional initial-input history default-value)
               default-value))
     (org-gtd-reactivate)
     ;; Verify restored
     (expect (org-entry-get (point) "ORG_GTD") :to-equal "Calendar")
     (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2024-06-20>")))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "preserves calendar state through tickler"`
Expected: FAIL - PREVIOUS_ORG_GTD not set

**Step 3: Modify org-gtd-tickler--configure to call save-state**

In `org-gtd-tickler.el`, add require and modify `org-gtd-tickler--configure`:

```elisp
;; Add to Requirements section:
(require 'org-gtd-reactivate)

;; Replace org-gtd-tickler--configure:
(defun org-gtd-tickler--configure (&optional config-override)
  "Configure item at point as tickler.

Saves current state to PREVIOUS_* properties before setting type.
CONFIG-OVERRIDE can provide input configuration to override default
prompting behavior."
  ;; Save current state before changing type
  (org-gtd-save-state)
  ;; Configure as tickler type
  (org-gtd-configure-as-type 'tickler
                             (when config-override
                               `((:when . ,(funcall (alist-get '(quote active-timestamp) config-override nil nil #'equal) nil))))))
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "preserves calendar state through tickler"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-tickler.el test/reactivate-test.el
git commit -m "feat: integrate save-state with org-gtd-tickler"
```

---

## Task 9: Remove old reactivation code from tickler

**Files:**
- Modify: `org-gtd-tickler.el`

**Step 1: Remove org-gtd-reactivate from org-gtd-tickler.el**

Delete the entire `org-gtd-reactivate` function (lines ~103-127) from `org-gtd-tickler.el`.

**Step 2: Run full test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 3: Commit**

```bash
git add org-gtd-tickler.el
git commit -m "refactor: remove old org-gtd-reactivate from tickler module"
```

---

## Task 10: Remove org-gtd-someday-activate

**Files:**
- Modify: `org-gtd-someday.el`

**Step 1: Remove org-gtd-someday-activate from org-gtd-someday.el**

Delete the entire `org-gtd-someday-activate` function (lines ~65-81) from `org-gtd-someday.el`.

**Step 2: Run full test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 3: Commit**

```bash
git add org-gtd-someday.el
git commit -m "refactor: remove org-gtd-someday-activate (replaced by org-gtd-reactivate)"
```

---

## Task 11: Refactor project save/restore to use generic functions

**Files:**
- Modify: `org-gtd-projects.el`
- Modify: `test/project-test.el`

**Step 1: Verify existing project tests pass**

Run: `eldev test -B "saves ORG_GTD and TODO state\|restores ORG_GTD and TODO state"`
Expected: PASS (baseline)

**Step 2: Refactor org-gtd-project--save-state**

Replace `org-gtd-project--save-state` in `org-gtd-projects.el`:

```elisp
(require 'org-gtd-reactivate)

(defun org-gtd-project--save-state (marker)
  "Save GTD state at MARKER to PREVIOUS_* properties.

Uses `org-gtd-save-state' for the actual property saving.
Skips tasks belonging to multiple projects."
  (org-with-point-at marker
    ;; Check if this is a multi-project task
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (if (> (length project-ids) 1)
          ;; Skip multi-project tasks
          (message "Skipping multi-project task: %s" (org-get-heading t t t t))
        ;; Save state and set tickler
        (org-gtd-save-state)
        (org-entry-put (point) "ORG_GTD" org-gtd-tickler)
        (org-todo 'none)))))
```

**Step 3: Refactor org-gtd-project--restore-state**

Replace `org-gtd-project--restore-state` in `org-gtd-projects.el`:

```elisp
(defun org-gtd-project--restore-state (marker)
  "Restore GTD state at MARKER from PREVIOUS_* properties.

Uses `org-gtd-restore-state' but without prompting for project tasks
since they don't have user-facing properties to update."
  (org-with-point-at marker
    (let ((previous-org-gtd (org-entry-get (point) "PREVIOUS_ORG_GTD"))
          (previous-todo (org-entry-get (point) "PREVIOUS_TODO")))
      ;; Clear tickler timestamp
      (org-entry-delete (point) "ORG_GTD_TIMESTAMP")
      ;; Restore ORG_GTD
      (when previous-org-gtd
        (org-entry-put (point) "ORG_GTD" previous-org-gtd)
        (org-entry-delete (point) "PREVIOUS_ORG_GTD"))
      ;; Restore TODO keyword
      (when previous-todo
        (org-todo previous-todo)
        (org-entry-delete (point) "PREVIOUS_TODO")))))
```

**Step 4: Run project tests**

Run: `eldev test -B "saves ORG_GTD and TODO state\|restores ORG_GTD and TODO state\|reactivates a tickler project"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-projects.el
git commit -m "refactor: project save/restore uses org-gtd-save-state"
```

---

## Task 12: Add org-gtd-reactivate to org-gtd.el requires

**Files:**
- Modify: `org-gtd.el`

**Step 1: Add require**

Add to the requires section of `org-gtd.el`:

```elisp
(require 'org-gtd-reactivate)
```

**Step 2: Run full test suite**

Run: `eldev test -B`
Expected: All tests pass

**Step 3: Compile to check for warnings**

Run: `eldev clean && eldev compile`
Expected: No errors

**Step 4: Commit**

```bash
git add org-gtd.el
git commit -m "chore: add org-gtd-reactivate to package requires"
```

---

## Task 13: Final verification

**Step 1: Run full test suite**

Run: `eldev test -B`
Expected: All 750+ tests pass

**Step 2: Check for compilation warnings**

Run: `eldev clean && eldev compile`
Expected: No errors, minimal warnings

**Step 3: Manual smoke test**

1. Create a delegated item with org-gtd
2. Someday it
3. Verify PREVIOUS_* properties are set
4. Reactivate it
5. Verify properties are restored with prompts
