# Deadline Filter Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the `deadline` filter for native agenda skip functions so users can filter items by deadline timing (past, today, future).

**Architecture:** Create `org-gtd-pred--deadline-matches` predicate that uses `org-get-deadline-time` to access the DEADLINE planning line and compare with today's date. Integrate into skip function builder.

**Tech Stack:** Emacs Lisp, org-mode deadline API (`org-get-deadline-time`), date comparison

**Bead:** orggtd-bvu

---

### Task 1: Unit Tests for Deadline Predicate

**Files:**
- Create: `test/unit/deadline-filter-test.el`

**Step 1: Create the test file**

```elisp
;;; deadline-filter-test.el --- Tests for deadline filter -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Predicate Unit Tests

(deftest deadline-pred/past-matches-overdue ()
  "Deadline predicate matches items with deadline in the past."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Overdue task\n")
    (insert "DEADLINE: <2020-01-01 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'past)))
      (assert-true (funcall pred)))))

(deftest deadline-pred/past-no-match-future-deadline ()
  "Deadline predicate does not match items with future deadline."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Future task\n")
    (insert "DEADLINE: <2099-12-31 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'past)))
      (assert-nil (funcall pred)))))

(deftest deadline-pred/past-no-match-no-deadline ()
  "Deadline predicate does not match items without deadline."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO No deadline task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'past)))
      (assert-nil (funcall pred)))))

(deftest deadline-pred/future-matches-upcoming ()
  "Future deadline predicate matches items with future deadline."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Future task\n")
    (insert "DEADLINE: <2099-12-31 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'future)))
      (assert-true (funcall pred)))))

(deftest deadline-pred/future-no-match-past-deadline ()
  "Future deadline predicate does not match past deadlines."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Overdue task\n")
    (insert "DEADLINE: <2020-01-01 Wed>\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--deadline-matches 'future)))
      (assert-nil (funcall pred)))))

(provide 'deadline-filter-test)
;;; deadline-filter-test.el ends here
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev -p -dtT etest test/unit/deadline-filter-test.el`
Expected: FAIL with "Symbol's function definition is void: org-gtd-pred--deadline-matches"

**Step 3: Commit**

```bash
git add test/unit/deadline-filter-test.el
git commit -m "test: add failing tests for deadline predicate"
```

---

### Task 2: Implement Deadline Predicate

**Files:**
- Modify: `org-gtd-skip.el` (after Tag Predicates section)

**Step 1: Add deadline predicate**

Add after the `org-gtd-pred--tags-matches` function:

```elisp
;;;; Deadline/Scheduled Predicates

(defun org-gtd-pred--deadline-matches (time-spec)
  "Return predicate checking if item's deadline matches TIME-SPEC.
TIME-SPEC can be:
  - \\='past - deadline is before today
  - \\='today - deadline is today
  - \\='future - deadline is after today
Returns nil if item has no deadline."
  (lambda ()
    (when-let ((deadline-time (org-get-deadline-time (point))))
      (let* ((today (org-today))
             (deadline-day (time-to-days deadline-time)))
        (pcase time-spec
          ('past (< deadline-day today))
          ('today (= deadline-day today))
          ('future (> deadline-day today))
          (_ nil))))))
```

**Step 2: Run tests**

Run: `~/bin/eldev -p -dtT etest test/unit/deadline-filter-test.el`
Expected: All 5 tests PASS

**Step 3: Commit**

```bash
git add org-gtd-skip.el
git commit -m "feat: add org-gtd-pred--deadline-matches predicate"
```

---

### Task 3: Integrate Deadline Filter into Skip Function Builder

**Files:**
- Modify: `org-gtd-view-language.el`

**Step 1: Add deadline handling to skip function**

In `org-gtd-view-lang--build-skip-function`, after the who predicate handling, add:

```elisp
        ;; Add deadline predicate
        (when-let ((deadline-filter (alist-get 'deadline gtd-view-spec)))
          (push (org-gtd-pred--deadline-matches deadline-filter) predicates))
```

**Step 2: Run all tests**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS

**Step 3: Commit**

```bash
git add org-gtd-view-language.el
git commit -m "feat: integrate deadline filter into skip function builder"
```

---

### Task 4: End-to-End Integration Test

**Files:**
- Modify: `test/integration/gtd-view-language-test.el`

**Step 1: Add integration test**

```elisp
;;; Deadline Filter Integration Tests

(deftest view-lang-int/deadline-filter-shows-overdue-items ()
  "View with deadline=past filter shows items with overdue deadlines."
  (with-current-buffer (org-gtd--default-file)
    (insert "* TODO Overdue task
DEADLINE: <2020-01-01 Wed>
:PROPERTIES:
:ORG_GTD: Actions
:END:
* TODO Future task
DEADLINE: <2099-12-31 Wed>
:PROPERTIES:
:ORG_GTD: Actions
:END:
* TODO No deadline task
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Overdue Tasks")
     (type . next-action)
     (deadline . past)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include overdue task
        (assert-match "Overdue task" content)
        ;; Should NOT include future or no-deadline tasks
        (assert-no-match "Future task" content)
        (assert-no-match "No deadline task" content)))))
```

**Step 2: Run tests**

Run: `~/bin/eldev -p -dtT etest test/integration/gtd-view-language-test.el`
Expected: All tests PASS

**Step 3: Commit**

```bash
git add test/integration/gtd-view-language-test.el
git commit -m "test: add end-to-end test for deadline filter"
```

---

### Task 5: Close the Bead

```bash
bd close orggtd-bvu --reason="Implemented deadline predicate and integrated into skip function. Includes unit and integration tests for past/today/future filtering."
bd sync
```

---

## Summary

1. **New predicate**: `org-gtd-pred--deadline-matches` in `org-gtd-skip.el`
2. **Skip function integration**: Deadline filter handled in builder
3. **Unit tests**: 5 tests covering past, future, today scenarios
4. **Integration test**: 1 end-to-end test

**Usage:**
```elisp
(org-gtd-view-show
 '((name . "Overdue Tasks")
   (type . next-action)
   (deadline . past)))
```
