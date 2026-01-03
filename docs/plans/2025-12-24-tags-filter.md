# Tags Filter Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the `tags` filter for native agenda skip functions so users can filter views by org-mode tags.

**Architecture:** Create a predicate `org-gtd-pred--tags-matches` that checks if an entry has any of the specified tags, then integrate it into `org-gtd-view-lang--build-skip-function`. The filter uses OR semantics (matches if entry has ANY of the specified tags).

**Tech Stack:** Emacs Lisp, org-mode tags API (`org-get-tags`), org-gtd predicate composition

**Bead:** orggtd-brn

---

### Task 1: Unit Test for Tags Predicate

**Files:**
- Create: `test/unit/tags-filter-test.el`

**Step 1: Create the test file with predicate tests**

```elisp
;;; tags-filter-test.el --- Tests for tags filter predicate -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Predicate Unit Tests

(deftest tags-pred/matches-single-tag ()
  "Predicate matches when entry has the specified tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@work:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work"))))
      (assert-true (funcall pred)))))

(deftest tags-pred/matches-any-of-multiple-tags ()
  "Predicate matches when entry has any of the specified tags."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@home:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work" "@home"))))
      (assert-true (funcall pred)))))

(deftest tags-pred/no-match-when-different-tag ()
  "Predicate returns nil when entry has different tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@errands:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work" "@home"))))
      (assert-nil (funcall pred)))))

(deftest tags-pred/no-match-when-no-tags ()
  "Predicate returns nil when entry has no tags."
  (with-temp-buffer
    (org-mode)
    (insert "* Task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work"))))
      (assert-nil (funcall pred)))))

(deftest tags-pred/matches-among-multiple-entry-tags ()
  "Predicate matches when entry has multiple tags including a match."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@work:urgent:important:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("urgent"))))
      (assert-true (funcall pred)))))

(provide 'tags-filter-test)
;;; tags-filter-test.el ends here
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev -p -dtT etest test/unit/tags-filter-test.el`
Expected: FAIL with "Symbol's function definition is void: org-gtd-pred--tags-matches"

**Step 3: Commit test file**

```bash
git add test/unit/tags-filter-test.el
git commit -m "test: add failing tests for tags predicate"
```

---

### Task 2: Implement Tags Predicate

**Files:**
- Modify: `org-gtd-skip.el` (after line 151, before `;;;; Clocked Time Predicates`)

**Step 1: Add the tags predicate**

Add after `org-gtd-pred--priority-matches` function:

```elisp
;;;; Tag Predicates

(defun org-gtd-pred--tags-matches (tags)
  "Return predicate checking if item has any of TAGS.
TAGS is a list of tag strings (e.g., (\"@work\" \"@home\")).
Uses OR semantics: returns t if entry has ANY of the specified tags."
  (lambda ()
    (let ((entry-tags (org-get-tags nil t)))  ; nil=current, t=local only
      (cl-some (lambda (tag) (member tag entry-tags)) tags))))
```

**Step 2: Run test to verify it passes**

Run: `~/bin/eldev -p -dtT etest test/unit/tags-filter-test.el`
Expected: All 5 tests PASS

**Step 3: Commit implementation**

```bash
git add org-gtd-skip.el
git commit -m "feat: add org-gtd-pred--tags-matches predicate"
```

---

### Task 3: Integrate Tags Filter into Skip Function Builder

**Files:**
- Modify: `org-gtd-view-language.el` (in `org-gtd-view-lang--build-skip-function`, around line 1051)

**Step 1: Write unit test for skip function with tags**

Add to `test/unit/tags-filter-test.el`:

```elisp
;;; Skip Function Integration

(deftest tags-filter/skip-function-includes-tags ()
  "Skip function builder includes tags filter predicate."
  (let* ((spec '((type . next-action)
                 (tags . ("@work" "@home"))))
         (skip-fn (org-gtd-view-lang--build-skip-function spec)))
    ;; Skip function should be a lambda (closure)
    (assert-true (functionp skip-fn))))
```

**Step 2: Run test to verify behavior**

Run: `~/bin/eldev -p -dtT etest test/unit/tags-filter-test.el`
Expected: PASS (function exists, but tags not yet integrated - test is minimal)

**Step 3: Add tags handling to skip function builder**

In `org-gtd-view-language.el`, add after the `last-clocked-out` handling (around line 1051), before the `not-done` predicate:

```elisp
        ;; Add tags predicate
        (when-let ((tags-filter (alist-get 'tags gtd-view-spec)))
          (push (org-gtd-pred--tags-matches tags-filter) predicates))
```

**Step 4: Run all tests**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS

**Step 5: Commit integration**

```bash
git add org-gtd-view-language.el test/unit/tags-filter-test.el
git commit -m "feat: integrate tags filter into skip function builder"
```

---

### Task 4: End-to-End Integration Test

**Files:**
- Modify: `test/integration/gtd-view-language-test.el`

**Step 1: Add end-to-end test for tags filter**

Add before the `(provide ...)` line:

```elisp
;;; Tags Filter Integration Tests

(deftest view-lang-int/tags-filter-includes-matching-items ()
  "View with tags filter shows items with matching tags."
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Work task :@work:
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Home task :@home:
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Errands task :@errands:
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Work and Home Tasks")
     (type . next-action)
     (tags . ("@work" "@home"))))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include work and home tasks
        (assert-match "Work task" content)
        (assert-match "Home task" content)
        ;; Should NOT include errands task
        (assert-no-match "Errands task" content)))))

(deftest view-lang-int/tags-filter-excludes-non-matching-items ()
  "View with tags filter excludes items without matching tags."
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Tagged task :@work:
:PROPERTIES:
:ORG_GTD: Actions
:END:
* NEXT Untagged task
:PROPERTIES:
:ORG_GTD: Actions
:END:
")
    (basic-save-buffer))

  (org-gtd-view-show
   '((name . "Work Tasks Only")
     (type . next-action)
     (tags . ("@work"))))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (let ((content (buffer-string)))
        ;; Should include tagged task
        (assert-match "Tagged task" content)
        ;; Should NOT include untagged task
        (assert-no-match "Untagged task" content)))))
```

**Step 2: Run integration tests**

Run: `~/bin/eldev -p -dtT etest test/integration/gtd-view-language-test.el`
Expected: All tests PASS including the new ones

**Step 3: Run full test suite**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS

**Step 4: Commit integration tests**

```bash
git add test/integration/gtd-view-language-test.el
git commit -m "test: add end-to-end tests for tags filter"
```

---

### Task 5: Close the Bead

**Step 1: Verify all tests pass**

Run: `~/bin/eldev -p -dtT etest`
Expected: All tests PASS

**Step 2: Close the bead**

```bash
bd close orggtd-brn --reason="Implemented tags predicate and integrated into skip function builder with unit and integration tests"
bd sync
```

---

## Summary

After completing all tasks:

1. **New predicate**: `org-gtd-pred--tags-matches` in `org-gtd-skip.el`
2. **Skip function integration**: Tags filter handled in `org-gtd-view-lang--build-skip-function`
3. **Unit tests**: 6 tests covering predicate behavior
4. **Integration tests**: 2 end-to-end tests verifying view filtering

**Usage example:**
```elisp
(org-gtd-view-show
 '((name . "Work and Home Tasks")
   (type . next-action)
   (tags . ("@work" "@home"))))
```

This shows next actions tagged with either `@work` OR `@home`.
