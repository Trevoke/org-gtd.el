# Timestamped Types Use Agenda Blocks Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make delegated and tickler items respond to date navigation by using agenda blocks instead of tags-todo blocks.

**Architecture:** Timestamped types (those with `:when` property) without an explicit `when` filter get routed to agenda blocks. Types with a `when` filter continue to use tags-todo blocks for list-style views. The presence of `when` filter determines block type: no filter = calendar-style (agenda), with filter = list-style (tags-todo).

**Tech Stack:** Emacs Lisp, org-agenda, e-unit testing framework

---

### Task 1: Write failing integration test for delegated items responding to date navigation

**Files:**
- Create: `test/integration/engage-date-navigation-test.el`

**Step 1: Create the test file with user behavior tests**

Create `test/integration/engage-date-navigation-test.el`:

```elisp
;;; engage-date-navigation-test.el --- Tests for date navigation in engage view -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;;; Commentary:
;;
;; Integration tests verifying that delegated and tickler items respond
;; to date navigation in the engage view.
;;
;; User behaviors tested:
;; - Delegated item due Friday appears when jumping to Friday
;; - Delegated item due Friday does NOT appear when viewing today
;; - Tickler item due Friday appears when jumping to Friday
;; - Reflect views with (when . future) still show all future items

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-engage)
(require 'org-gtd-delegate)
(require 'org-gtd-reflect)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defun test--create-delegated-item (title person date-string)
  "Create a delegated item with TITLE assigned to PERSON due on DATE-STRING."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* WAIT %s\n" title))
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-entry-put (point) "ORG_GTD" "Delegated")
    (org-entry-put (point) "DELEGATED_TO" person)
    (org-entry-put (point) org-gtd-timestamp (format "<%s>" date-string))
    (basic-save-buffer)))

(defun test--create-tickler-item (title date-string)
  "Create a tickler item with TITLE due on DATE-STRING."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* %s\n" title))
    (forward-line -1)
    (org-back-to-heading t)
    (org-id-get-create)
    (org-entry-put (point) "ORG_GTD" "Tickler")
    (org-entry-put (point) org-gtd-timestamp (format "<%s>" date-string))
    (basic-save-buffer)))

(defun test--engage-view-contains-p (text)
  "Return non-nil if engage view buffer contains TEXT."
  (with-current-buffer "*Org Agenda*"
    (goto-char (point-min))
    (search-forward text nil t)))

(defun test--cleanup-agenda ()
  "Kill agenda buffer if it exists."
  (when (get-buffer "*Org Agenda*")
    (kill-buffer "*Org Agenda*")))

;;; Delegated Item Tests

(deftest engage/delegated-item-appears-on-its-date ()
  "Delegated item appears in engage view when jumping to its due date."
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60)))))
    (test--create-delegated-item "Call Bob about project" "Bob" future-date)
    (org-gtd-engage)
    ;; Jump to the future date
    (org-agenda-goto-date future-date)
    (assert-true (test--engage-view-contains-p "Call Bob about project"))
    (test--cleanup-agenda)))

(deftest engage/delegated-item-not-shown-on-other-dates ()
  "Delegated item does NOT appear in engage view when viewing a different date."
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60)))))
    (test--create-delegated-item "Call Bob about project" "Bob" future-date)
    (org-gtd-engage)
    ;; Viewing today - item should NOT appear
    (assert-nil (test--engage-view-contains-p "Call Bob about project"))
    (test--cleanup-agenda)))

;;; Tickler Item Tests

(deftest engage/tickler-item-appears-on-its-date ()
  "Tickler item appears in engage view when jumping to its due date."
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60)))))
    (test--create-tickler-item "Review insurance options" future-date)
    (org-gtd-engage)
    ;; Jump to the future date
    (org-agenda-goto-date future-date)
    (assert-true (test--engage-view-contains-p "Review insurance options"))
    (test--cleanup-agenda)))

(deftest engage/tickler-item-not-shown-on-other-dates ()
  "Tickler item does NOT appear in engage view when viewing a different date."
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60)))))
    (test--create-tickler-item "Review insurance options" future-date)
    (org-gtd-engage)
    ;; Viewing today - item should NOT appear
    (assert-nil (test--engage-view-contains-p "Review insurance options"))
    (test--cleanup-agenda)))

;;; Reflect View Tests (when filter preserved)

(deftest reflect/upcoming-delegated-shows-all-future-items ()
  "Reflect upcoming delegated view shows all future delegated items."
  (let ((future-date-1 (format-time-string "%Y-%m-%d" (time-add nil (* 3 24 60 60))))
        (future-date-2 (format-time-string "%Y-%m-%d" (time-add nil (* 10 24 60 60)))))
    (test--create-delegated-item "Call Bob" "Bob" future-date-1)
    (test--create-delegated-item "Call Alice" "Alice" future-date-2)
    (org-gtd-reflect-upcoming-delegated)
    ;; Both items should appear (list view, not date-specific)
    (assert-true (test--engage-view-contains-p "Call Bob"))
    (assert-true (test--engage-view-contains-p "Call Alice"))
    (test--cleanup-agenda)))

(provide 'engage-date-navigation-test)

;;; engage-date-navigation-test.el ends here
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev etest test/integration/engage-date-navigation-test.el -r dot`
Expected: FAIL - delegated/tickler items don't appear when jumping to dates (current behavior)

**Step 3: Commit the failing tests**

```bash
git add test/integration/engage-date-navigation-test.el
git commit -m "test(engage): add failing tests for date navigation

User behaviors tested:
- Delegated item due Friday appears when jumping to Friday
- Delegated item due Friday does NOT appear when viewing today
- Tickler item due Friday appears when jumping to Friday
- Tickler item due Friday does NOT appear when viewing today
- Reflect upcoming-delegated still shows all future items (list view)"
```

---

### Task 2: Add timestamped type agenda block creator

**Files:**
- Modify: `org-gtd-view-language.el:302` (add new function after `org-gtd-view-lang--create-calendar-day-block`)

**Step 1: Add the new function**

Add to `org-gtd-view-language.el` after `org-gtd-view-lang--create-calendar-day-block` (around line 302):

```elisp
(defun org-gtd-view-lang--create-timestamped-type-block (gtd-view-spec)
  "Create agenda block for a timestamped type (delegated, tickler, etc.).
Items appear on their ORG_GTD_TIMESTAMP date and respond to date navigation.
Time grid is disabled (only calendar-day block shows the time grid)."
  (let* ((skip-fn (org-gtd-view-lang--build-skip-function gtd-view-spec))
         (settings `((org-agenda-span 1)
                     (org-agenda-start-day nil)
                     (org-agenda-use-time-grid nil)
                     (org-agenda-skip-function ',skip-fn))))
    `(agenda "" ,settings)))
```

**Step 2: Run tests (still failing)**

Run: `~/bin/eldev etest test/integration/engage-date-navigation-test.el -r dot`
Expected: Still FAIL - function exists but not wired up yet

---

### Task 3: Update routing logic and verify tests pass

**Files:**
- Modify: `org-gtd-view-language.el:274` (add routing condition)

**Step 1: Add routing condition**

In `org-gtd-view-lang--create-agenda-block`, add BEFORE the tags-todo fallback (before the line with `org-gtd-view-lang--native-type-p`):

```elisp
         ;; Timestamped types without 'when' filter use agenda blocks for date navigation
         ((and type-filter
               (org-gtd-type-property type-filter :when)
               (not (alist-get 'when gtd-view-spec)))
          (org-gtd-view-lang--create-timestamped-type-block gtd-view-spec))
```

**Step 2: Run integration tests (still failing)**

Run: `~/bin/eldev etest test/integration/engage-date-navigation-test.el -r dot`
Expected: Still FAIL - engage view still has `(when . today)` filters

---

### Task 4: Update engage view spec and verify tests pass

**Files:**
- Modify: `org-gtd-engage.el:59-64`

**Step 1: Remove `(when . today)` from delegated and tickler blocks**

Change from:
```elisp
               ((name . "Tickler items ready for today")
                (type . tickler)
                (when . today))
               ((name . "Delegated items to check in on today")
                (type . delegated)
                (when . today))
```

To:
```elisp
               ((name . "Tickler items")
                (type . tickler))
               ((name . "Delegated items to check in on")
                (type . delegated))
```

**Step 2: Run integration tests to verify they pass**

Run: `~/bin/eldev etest test/integration/engage-date-navigation-test.el -r dot`
Expected: PASS - all 5 user behavior tests pass

**Step 3: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 4: Commit**

```bash
git add org-gtd-view-language.el org-gtd-engage.el
git commit -m "feat(engage): delegated/tickler respond to date navigation

Timestamped types (delegated, tickler) without a 'when' filter now use
agenda blocks instead of tags-todo. This enables date navigation (j/f/b)
to show items on their ORG_GTD_TIMESTAMP date.

Types WITH a 'when' filter continue to use tags-todo for list-style views
(e.g., reflect-upcoming-delegated uses (when . future) to show all future
items as a list).

Fixes user-reported issue where delegated items didn't appear when
jumping to future dates in the engage view."
```

---

### Task 5: Add unit test for routing logic

**Files:**
- Modify: `test/unit/gtd-view-language-test.el`

**Step 1: Add unit tests for routing logic**

Add to `test/unit/gtd-view-language-test.el` before `(provide 'gtd-view-language-test)`:

```elisp
;;; Timestamped Type Routing Tests

(deftest view-lang/timestamped-type-without-when-uses-agenda-block ()
  "Timestamped types without 'when' filter use agenda blocks for date navigation."
  (let ((delegated-spec '((type . delegated))))
    (let ((block (org-gtd-view-lang--create-agenda-block delegated-spec)))
      (assert-equal 'agenda (car block)))))

(deftest view-lang/timestamped-type-with-when-uses-tags-todo ()
  "Timestamped types WITH 'when' filter use tags-todo for list-style views."
  (let ((delegated-future-spec '((type . delegated) (when . future))))
    (let ((block (org-gtd-view-lang--create-agenda-block delegated-future-spec)))
      (assert-equal 'tags-todo (car block)))))

(deftest view-lang/tickler-without-when-uses-agenda-block ()
  "Tickler type without 'when' filter uses agenda block."
  (let ((tickler-spec '((type . tickler))))
    (let ((block (org-gtd-view-lang--create-agenda-block tickler-spec)))
      (assert-equal 'agenda (car block)))))

(deftest view-lang/next-action-still-uses-tags-todo ()
  "Non-timestamped types still use tags-todo blocks."
  (let ((action-spec '((type . next-action))))
    (let ((block (org-gtd-view-lang--create-agenda-block action-spec)))
      (assert-equal 'tags-todo (car block)))))
```

**Step 2: Run unit tests to verify they pass**

Run: `~/bin/eldev etest test/unit/gtd-view-language-test.el -r dot`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/gtd-view-language-test.el
git commit -m "test(view-lang): add unit tests for timestamped type routing"
```

---

### Task 6: Add future architecture comment

**Files:**
- Modify: `org-gtd-view-language.el` (after Commentary section, before Code section)

**Step 1: Read the file header**

Run: Read `org-gtd-view-language.el` lines 1-40

**Step 2: Add the architecture comment**

Insert after `;;; Commentary:` section, before `;;; Code:`:

```elisp
;; FUTURE ARCHITECTURE NOTE:
;;
;; The view DSL should evolve toward:
;;
;; 1. ONE agenda block per view (combines all timestamped types)
;;    - Calendar, Delegated, Tickler shown together on their dates
;;    - Single date navigation affects all items
;;
;; 2. MANY non-agenda blocks (tags-todo for non-timestamped types)
;;    - Next actions, projects, etc.
;;
;; 3. View properties with hierarchy/precedence:
;;    - calendar: (time-grid . t)
;;    - delegated: (time-grid . nil)
;;    - tickler: (time-grid . nil)
;;    - If calendar + delegated in same view -> calendar properties win
;;
;; 4. Simplified DSL example:
;;    ((types . (calendar delegated tickler))  ; combined agenda block
;;     (time-grid . from-calendar))            ; inherit from highest-priority type
;;
;; Current implementation (2026-01): Each timestamped type gets its own
;; agenda block. This works but creates multiple agenda blocks where one
;; would suffice.
```

**Step 3: Commit**

```bash
git add org-gtd-view-language.el
git commit -m "docs(view-lang): add future architecture vision comment"
```

---

### Task 7: Final verification

**Step 1: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 2: Compile with warnings**

Run: `~/bin/eldev clean && ~/bin/eldev compile`
Expected: No errors

**Step 3: Manual end-to-end test**

1. Create a delegated item with future date:
   ```elisp
   (org-gtd-delegate-create "Test delegation for Friday" "Bob" "2026-01-24")
   ```
2. Open engage view: `C-c d e`
3. Press `j` and enter `2026-01-24`
4. Verify: "Test delegation for Friday" appears in "Delegated items to check in on" section
5. Press `b` (back) to return to today
6. Verify: The delegated item no longer appears (it's not due today)

---

### Summary

**User behaviors tested:**
1. Delegated item due Friday appears when jumping to Friday ✓
2. Delegated item due Friday does NOT appear when viewing today ✓
3. Tickler item due Friday appears when jumping to Friday ✓
4. Tickler item due Friday does NOT appear when viewing today ✓
5. Reflect upcoming-delegated still shows all future items (list view) ✓

**Implementation:**
- `(type . delegated)` without `when` → agenda block (responds to date nav)
- `(type . delegated) (when . future)` → tags-todo block (list of all future)
- Engage view delegated/tickler blocks respond to j/f/b navigation
- Reflect views (with `when` filters) unchanged
- Future architecture documented
