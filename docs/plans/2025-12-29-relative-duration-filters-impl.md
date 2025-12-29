# Relative Duration Filters Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extend the org-gtd view language DSL to support relative duration comparisons like `(< "14d")` in time-based filters.

**Architecture:** Extend the existing duration parsing in `org-gtd-skip.el` to support signed durations and additional units (M, y), then add comparison expression handling in `org-gtd-view-language.el` for when, deadline, scheduled, and done filters.

**Tech Stack:** Emacs Lisp, org-mode, e-unit testing framework

---

## Task 1: Extend Duration Parsing - Add M (months) and y (years) Units

**Files:**
- Modify: `org-gtd-skip.el:396-406` (org-gtd--parse-relative-time)
- Test: `test/unit/duration-parsing-test.el` (new file)

**Step 1: Write the failing tests for new units**

Create new test file `test/unit/duration-parsing-test.el`:

```elisp
;;; duration-parsing-test.el --- Tests for duration parsing -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-skip)

(e-unit-initialize)

;;;; Existing units (sanity checks)

(deftest duration/parse-days ()
  "Parses days correctly (e.g., '2d' = 172800 seconds)."
  (assert-equal 172800 (org-gtd--parse-relative-time "2d")))

(deftest duration/parse-weeks ()
  "Parses weeks correctly (e.g., '1w' = 604800 seconds)."
  (assert-equal 604800 (org-gtd--parse-relative-time "1w")))

;;;; New units: months and years

(deftest duration/parse-months ()
  "Parses months correctly (e.g., '1M' = ~30 days = 2592000 seconds)."
  (assert-equal 2592000 (org-gtd--parse-relative-time "1M")))

(deftest duration/parse-years ()
  "Parses years correctly (e.g., '1y' = ~365 days = 31536000 seconds)."
  (assert-equal 31536000 (org-gtd--parse-relative-time "1y")))

(deftest duration/parse-two-months ()
  "Parses 2M correctly."
  (assert-equal 5184000 (org-gtd--parse-relative-time "2M")))

(deftest duration/parse-two-years ()
  "Parses 2y correctly."
  (assert-equal 63072000 (org-gtd--parse-relative-time "2y")))

(provide 'duration-parsing-test)
;;; duration-parsing-test.el ends here
```

**Step 2: Run tests to verify they fail**

Run: `eldev -p -dtT etest test/unit/duration-parsing-test.el`
Expected: FAIL - months and years tests fail with "Unknown time unit"

**Step 3: Implement M and y units**

In `org-gtd-skip.el`, modify `org-gtd--parse-relative-time`:

```elisp
(defun org-gtd--parse-relative-time (duration-str)
  "Parse DURATION-STR like \"2d\", \"1w\", \"3h\", \"1M\", \"1y\" to seconds.
Supports: m (minutes), h (hours), d (days), w (weeks), M (months), y (years)."
  (let ((num (string-to-number duration-str))
        (unit (substring duration-str -1)))
    (pcase unit
      ("d" (* num 86400))      ; days
      ("w" (* num 604800))     ; weeks
      ("h" (* num 3600))       ; hours
      ("m" (* num 60))         ; minutes
      ("M" (* num 2592000))    ; months (~30 days)
      ("y" (* num 31536000))   ; years (~365 days)
      (_ (error "Unknown time unit in %s" duration-str)))))
```

**Step 4: Run tests to verify they pass**

Run: `eldev -p -dtT etest test/unit/duration-parsing-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-skip.el test/unit/duration-parsing-test.el
git commit -m "feat(skip): add months (M) and years (y) to duration parsing

Extends org-gtd--parse-relative-time to support:
- M for months (~30 days)
- y for years (~365 days)

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 2: Extend Duration Parsing - Add Signed Duration Support

**Files:**
- Modify: `org-gtd-skip.el:396-406` (org-gtd--parse-relative-time)
- Test: `test/unit/duration-parsing-test.el`

**Step 1: Write the failing tests for signed durations**

Add to `test/unit/duration-parsing-test.el`:

```elisp
;;;; Signed durations

(deftest duration/parse-positive-explicit ()
  "Parses explicit positive sign (+14d)."
  (assert-equal 1209600 (org-gtd--parse-relative-time "+14d")))

(deftest duration/parse-negative ()
  "Parses negative duration (-7d) as negative seconds."
  (assert-equal -604800 (org-gtd--parse-relative-time "-7d")))

(deftest duration/parse-negative-weeks ()
  "Parses negative weeks (-2w)."
  (assert-equal -1209600 (org-gtd--parse-relative-time "-2w")))

(deftest duration/parse-positive-months ()
  "Parses positive months (+1M)."
  (assert-equal 2592000 (org-gtd--parse-relative-time "+1M")))

(deftest duration/parse-negative-months ()
  "Parses negative months (-1M)."
  (assert-equal -2592000 (org-gtd--parse-relative-time "-1M")))
```

**Step 2: Run tests to verify they fail**

Run: `eldev -p -dtT etest test/unit/duration-parsing-test.el`
Expected: FAIL - signed duration tests fail (wrong number or error)

**Step 3: Implement signed duration support**

In `org-gtd-skip.el`, modify `org-gtd--parse-relative-time`:

```elisp
(defun org-gtd--parse-relative-time (duration-str)
  "Parse DURATION-STR like \"2d\", \"+14d\", \"-7d\" to seconds.
Supports: m (minutes), h (hours), d (days), w (weeks), M (months), y (years).
Sign: + or no sign = positive, - = negative (returns negative seconds)."
  (let* ((has-sign (string-match "^[+-]" duration-str))
         (sign (if (and has-sign (string-prefix-p "-" duration-str)) -1 1))
         (unsigned-str (if has-sign (substring duration-str 1) duration-str))
         (num (string-to-number unsigned-str))
         (unit (substring unsigned-str -1)))
    (* sign
       (pcase unit
         ("d" (* num 86400))      ; days
         ("w" (* num 604800))     ; weeks
         ("h" (* num 3600))       ; hours
         ("m" (* num 60))         ; minutes
         ("M" (* num 2592000))    ; months (~30 days)
         ("y" (* num 31536000))   ; years (~365 days)
         (_ (error "Unknown time unit in %s" duration-str))))))
```

**Step 4: Run tests to verify they pass**

Run: `eldev -p -dtT etest test/unit/duration-parsing-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-skip.el test/unit/duration-parsing-test.el
git commit -m "feat(skip): add signed duration support (+/-)

Extends org-gtd--parse-relative-time to support:
- Explicit positive: +14d (future)
- Negative: -7d (past, returns negative seconds)

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 3: Add Duration-to-Reference-Time Helper

**Files:**
- Modify: `org-gtd-skip.el` (add new function after org-gtd--parse-relative-time)
- Test: `test/unit/duration-parsing-test.el`

**Step 1: Write the failing tests for reference time calculation**

Add to `test/unit/duration-parsing-test.el`:

```elisp
;;;; Duration to reference time

(deftest duration/reference-time-future ()
  "Converts +14d to a time 14 days from now."
  (let* ((now (current-time))
         (ref (org-gtd--duration-to-reference-time "14d"))
         (diff-days (/ (float-time (time-subtract ref now)) 86400)))
    ;; Should be approximately 14 days in the future
    (assert-true (and (> diff-days 13.9) (< diff-days 14.1)))))

(deftest duration/reference-time-past ()
  "Converts -7d to a time 7 days ago."
  (let* ((now (current-time))
         (ref (org-gtd--duration-to-reference-time "-7d"))
         (diff-days (/ (float-time (time-subtract now ref)) 86400)))
    ;; Should be approximately 7 days in the past
    (assert-true (and (> diff-days 6.9) (< diff-days 7.1)))))

(deftest duration/reference-time-today ()
  "Handles 'today' as a special case."
  (let* ((ref (org-gtd--duration-to-reference-time "today"))
         (today-str (format-time-string "%Y-%m-%d"))
         (ref-str (format-time-string "%Y-%m-%d" ref)))
    (assert-equal today-str ref-str)))
```

**Step 2: Run tests to verify they fail**

Run: `eldev -p -dtT etest test/unit/duration-parsing-test.el`
Expected: FAIL - function not defined

**Step 3: Implement the helper function**

Add to `org-gtd-skip.el` after `org-gtd--parse-relative-time`:

```elisp
(defun org-gtd--duration-to-reference-time (duration-str)
  "Convert DURATION-STR to a reference time for comparison.
DURATION-STR can be:
  - \"today\" for current date at start of day
  - \"14d\" or \"+14d\" for 14 days from now
  - \"-7d\" for 7 days ago
  - Any format supported by `org-gtd--parse-relative-time'
Returns an Emacs time value."
  (if (string-equal duration-str "today")
      (org-time-string-to-time (format-time-string "%Y-%m-%d"))
    (let ((seconds (org-gtd--parse-relative-time duration-str)))
      (time-add (current-time) seconds))))
```

**Step 4: Run tests to verify they pass**

Run: `eldev -p -dtT etest test/unit/duration-parsing-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-skip.el test/unit/duration-parsing-test.el
git commit -m "feat(skip): add duration-to-reference-time helper

New function org-gtd--duration-to-reference-time converts duration
strings like '14d', '-7d', or 'today' to Emacs time values for
comparison in skip predicates.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 4: Add Timestamp Comparison Predicates for Durations

**Files:**
- Modify: `org-gtd-skip.el` (modify org-gtd-pred--property-ts<, ts>, ts=)
- Test: `test/unit/skip-predicates-test.el`

**Step 1: Write failing tests for duration-based comparisons**

Add to `test/unit/skip-predicates-test.el`:

```elisp
;;;; Duration-based timestamp comparisons

(deftest skip-pred/property-ts-less-than-duration-14d ()
  "Property ts< with '14d' matches timestamps within 14 days."
  (with-temp-buffer
    (org-mode)
    ;; Timestamp 7 days from now should match (< "14d")
    (let ((future-ts (format-time-string "<%Y-%m-%d %a>"
                       (time-add (current-time) (days-to-time 7)))))
      (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: %s\n:END:\n" future-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts< "ORG_GTD_TIMESTAMP" "14d")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-ts-less-than-duration-14d-reject ()
  "Property ts< with '14d' rejects timestamps beyond 14 days."
  (with-temp-buffer
    (org-mode)
    ;; Timestamp 20 days from now should NOT match (< "14d")
    (let ((future-ts (format-time-string "<%Y-%m-%d %a>"
                       (time-add (current-time) (days-to-time 20)))))
      (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: %s\n:END:\n" future-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts< "ORG_GTD_TIMESTAMP" "14d")))
      (assert-nil (funcall pred)))))

(deftest skip-pred/property-ts-less-than-negative-duration ()
  "Property ts< with '-7d' matches timestamps within past 7 days."
  (with-temp-buffer
    (org-mode)
    ;; Timestamp 3 days ago should match (< "-7d")
    (let ((past-ts (format-time-string "<%Y-%m-%d %a>"
                     (time-subtract (current-time) (days-to-time 3)))))
      (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: %s\n:END:\n" past-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts< "ORG_GTD_TIMESTAMP" "-7d")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-ts-greater-than-duration ()
  "Property ts> with '7d' matches timestamps beyond 7 days."
  (with-temp-buffer
    (org-mode)
    ;; Timestamp 14 days from now should match (> "7d")
    (let ((future-ts (format-time-string "<%Y-%m-%d %a>"
                       (time-add (current-time) (days-to-time 14)))))
      (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: %s\n:END:\n" future-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts> "ORG_GTD_TIMESTAMP" "7d")))
      (assert-true (funcall pred)))))
```

**Step 2: Run tests to verify they fail**

Run: `eldev -p -dtT etest test/unit/skip-predicates-test.el`
Expected: FAIL - predicates don't handle duration strings

**Step 3: Update org-gtd--parse-reference-date to handle durations**

Modify `org-gtd--parse-reference-date` in `org-gtd-skip.el`:

```elisp
(defun org-gtd--parse-reference-date (ref)
  "Parse reference date REF to internal time representation.
REF can be:
  - \"today\" for current date
  - Duration string like \"14d\", \"+7d\", \"-1w\", \"2M\", \"1y\"
  - A date string like \"2025-01-15\""
  (cond
   ((string-equal ref "today")
    (org-time-string-to-time (format-time-string "%Y-%m-%d")))
   ;; Match duration format: optional sign, digits, unit letter
   ((string-match "^[+-]?[0-9]+[mhdwMy]$" ref)
    (org-gtd--duration-to-reference-time ref))
   ;; Legacy format: +Nd, +Nw, etc. (no sign handling, always future)
   ((string-match "^\\+\\([0-9]+\\)\\([dwmy]\\)$" ref)
    (let* ((n (string-to-number (match-string 1 ref)))
           (unit (match-string 2 ref))
           (days (* n (pcase unit
                        ("d" 1)
                        ("w" 7)
                        ("m" 30)
                        ("y" 365)))))
      (time-add (current-time) (days-to-time days))))
   (t
    (org-time-string-to-time ref))))
```

**Step 4: Run tests to verify they pass**

Run: `eldev -p -dtT etest test/unit/skip-predicates-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-skip.el test/unit/skip-predicates-test.el
git commit -m "feat(skip): timestamp predicates support duration strings

org-gtd--parse-reference-date now accepts duration strings like
'14d', '-7d', '2w', '1M', '1y' in addition to 'today' and date strings.

This enables ts<, ts>, ts= predicates to compare against relative
time references.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 5: Add Comparison Expression Validation in View Language

**Files:**
- Modify: `org-gtd-view-language.el` (add validation helper)
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for validation**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;;; Comparison Expression Validation Tests

(deftest view-lang/validate-comparison-valid ()
  "Valid comparison expression passes validation."
  (assert-nil (org-gtd-view-lang--validate-comparison-expr '(< "14d"))))

(deftest view-lang/validate-comparison-valid-negative ()
  "Valid negative duration passes validation."
  (assert-nil (org-gtd-view-lang--validate-comparison-expr '(> "-7d"))))

(deftest view-lang/validate-comparison-valid-equals ()
  "Valid equals comparison passes validation."
  (assert-nil (org-gtd-view-lang--validate-comparison-expr '(= "today"))))

(deftest view-lang/validate-comparison-invalid-operator ()
  "Invalid operator fails validation."
  (assert-raises 'user-error
    (org-gtd-view-lang--validate-comparison-expr '(? "14d"))))

(deftest view-lang/validate-comparison-invalid-duration ()
  "Invalid duration format fails validation."
  (assert-raises 'user-error
    (org-gtd-view-lang--validate-comparison-expr '(< "invalid"))))

(deftest view-lang/validate-comparison-missing-duration ()
  "Missing duration fails validation."
  (assert-raises 'user-error
    (org-gtd-view-lang--validate-comparison-expr '(<))))
```

**Step 2: Run tests to verify they fail**

Run: `eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: FAIL - function not defined

**Step 3: Implement validation function**

Add to `org-gtd-view-language.el` before `org-gtd-view-lang--build-skip-function`:

```elisp
(defconst org-gtd-view-lang--valid-comparison-ops '(< > =)
  "Valid comparison operators for duration expressions.")

(defconst org-gtd-view-lang--duration-regexp "^[+-]?[0-9]+[mhdwMy]$"
  "Regexp matching valid duration strings.")

(defun org-gtd-view-lang--validate-comparison-expr (expr)
  "Validate comparison expression EXPR like (< \"14d\").
Returns nil if valid, signals user-error if invalid."
  (unless (and (listp expr) (>= (length expr) 2))
    (user-error "Comparison expression must be (OP DURATION), got: %S" expr))
  (let ((op (car expr))
        (duration (cadr expr)))
    (unless (memq op org-gtd-view-lang--valid-comparison-ops)
      (user-error "Invalid comparison operator '%s', must be one of: < > =" op))
    (unless (or (string-equal duration "today")
                (string-match org-gtd-view-lang--duration-regexp duration))
      (user-error "Invalid duration format '%s', expected pattern like '14d', '-7d', '2w'" duration)))
  nil)
```

**Step 4: Run tests to verify they pass**

Run: `eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(view-lang): add comparison expression validation

New function org-gtd-view-lang--validate-comparison-expr validates
comparison expressions like (< \"14d\") ensuring:
- Operator is one of: < > =
- Duration matches pattern [+-]?[0-9]+[mhdwMy] or is \"today\"

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 6: Handle When Filter Comparison Expressions

**Files:**
- Modify: `org-gtd-view-language.el:683-694` (when filter handling in build-skip-function)
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for when filter with comparison**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;;; When Filter Comparison Expression Tests

(deftest view-lang/skip-function-when-comparison-less-than ()
  "Skip function handles when=(< \"14d\") filter."
  (with-temp-buffer
    (org-mode)
    ;; Timestamp 7 days from now should match (< "14d")
    (let ((future-ts (format-time-string "<%Y-%m-%d %a>"
                       (time-add (current-time) (days-to-time 7)))))
      (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: %s\n:END:\n" future-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated) (when . (< "14d"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-function-when-comparison-rejects ()
  "Skip function rejects items outside comparison range."
  (with-temp-buffer
    (org-mode)
    ;; Timestamp 20 days from now should NOT match (< "14d")
    (let ((future-ts (format-time-string "<%Y-%m-%d %a>"
                       (time-add (current-time) (days-to-time 20)))))
      (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: %s\n:END:\n" future-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated) (when . (< "14d"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))

(deftest view-lang/skip-function-when-comparison-greater-than ()
  "Skip function handles when=(> \"-7d\") filter for past items."
  (with-temp-buffer
    (org-mode)
    ;; Timestamp 10 days ago should match (> "-7d") meaning "further in past than 7d ago"
    (let ((past-ts (format-time-string "<%Y-%m-%d %a>"
                     (time-subtract (current-time) (days-to-time 10)))))
      (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: %s\n:END:\n" past-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated) (when . (> "-7d"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))
```

**Step 2: Run tests to verify they fail**

Run: `eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: FAIL - comparison expressions not handled

**Step 3: Update when filter handling**

Modify the when filter section in `org-gtd-view-lang--build-skip-function`:

```elisp
        ;; Add when predicate based on type's semantic property
        (when (and when-filter type-filter)
          (let ((when-prop (org-gtd-type-property type-filter :when)))
            (when when-prop
              (cond
               ;; Comparison expression: (< "14d"), (> "-7d"), (= "today")
               ((and (listp when-filter) (memq (car when-filter) '(< > =)))
                (org-gtd-view-lang--validate-comparison-expr when-filter)
                (let ((op (car when-filter))
                      (duration (cadr when-filter)))
                  (pcase op
                    ('< (push (org-gtd-pred--property-ts< when-prop duration) predicates))
                    ('> (push (org-gtd-pred--property-ts> when-prop duration) predicates))
                    ('= (push (org-gtd-pred--property-ts= when-prop duration) predicates)))))
               ;; Symbol: past, today, future
               ((eq when-filter 'past)
                (push (org-gtd-pred--property-ts< when-prop "today") predicates))
               ((eq when-filter 'today)
                (push (org-gtd-pred--property-ts= when-prop "today") predicates))
               ((eq when-filter 'future)
                (push (org-gtd-pred--property-ts> when-prop "today") predicates))))))
```

**Step 4: Run tests to verify they pass**

Run: `eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(view-lang): when filter supports comparison expressions

The 'when' filter now accepts comparison expressions in addition to
symbols:
- (when . (< \"14d\")) - within next 14 days
- (when . (> \"-7d\")) - more than 7 days ago
- (when . (= \"today\")) - exactly today

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 7: Handle Deadline/Scheduled Filter Comparison Expressions

**Files:**
- Modify: `org-gtd-view-language.el:737-741` (deadline/scheduled handling)
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for deadline/scheduled comparisons**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;;; Deadline/Scheduled Comparison Expression Tests

(deftest view-lang/skip-function-deadline-comparison ()
  "Skip function handles deadline=(< \"3d\") filter."
  (with-temp-buffer
    (org-mode)
    ;; Item with deadline 2 days from now should match (< "3d")
    (let ((deadline-ts (format-time-string "<%Y-%m-%d %a>"
                         (time-add (current-time) (days-to-time 2)))))
      (insert (format "* TODO Test\nDEADLINE: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" deadline-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action) (deadline . (< "3d"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-function-scheduled-comparison ()
  "Skip function handles scheduled=(< \"7d\") filter."
  (with-temp-buffer
    (org-mode)
    ;; Item scheduled 3 days from now should match (< "7d")
    (let ((scheduled-ts (format-time-string "<%Y-%m-%d %a>"
                          (time-add (current-time) (days-to-time 3)))))
      (insert (format "* TODO Test\nSCHEDULED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" scheduled-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action) (scheduled . (< "7d"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))
```

**Step 2: Run tests to verify they fail**

Run: `eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: FAIL - comparison expressions not handled for deadline/scheduled

**Step 3: Update deadline/scheduled filter handling**

Modify the deadline and scheduled filter sections in `org-gtd-view-lang--build-skip-function`:

```elisp
        ;; Add deadline predicate
        (when-let ((deadline-filter (alist-get 'deadline gtd-view-spec)))
          (cond
           ;; Comparison expression: (< "3d"), (> "-1w")
           ((and (listp deadline-filter) (memq (car deadline-filter) '(< > =)))
            (org-gtd-view-lang--validate-comparison-expr deadline-filter)
            (let ((op (car deadline-filter))
                  (duration (cadr deadline-filter)))
              (pcase op
                ('< (push (org-gtd-pred--deadline-ts< duration) predicates))
                ('> (push (org-gtd-pred--deadline-ts> duration) predicates))
                ('= (push (org-gtd-pred--deadline-ts= duration) predicates)))))
           ;; Symbol: past, today, future
           (t (push (org-gtd-pred--deadline-matches deadline-filter) predicates))))
        ;; Add scheduled predicate
        (when-let ((scheduled-filter (alist-get 'scheduled gtd-view-spec)))
          (cond
           ;; Comparison expression: (< "7d"), (> "-1w")
           ((and (listp scheduled-filter) (memq (car scheduled-filter) '(< > =)))
            (org-gtd-view-lang--validate-comparison-expr scheduled-filter)
            (let ((op (car scheduled-filter))
                  (duration (cadr scheduled-filter)))
              (pcase op
                ('< (push (org-gtd-pred--scheduled-ts< duration) predicates))
                ('> (push (org-gtd-pred--scheduled-ts> duration) predicates))
                ('= (push (org-gtd-pred--scheduled-ts= duration) predicates)))))
           ;; Symbol: past, today, future
           (t (push (org-gtd-pred--scheduled-matches scheduled-filter) predicates))))
```

**Step 4: Add the new deadline/scheduled predicates**

Add to `org-gtd-skip.el` after the existing deadline/scheduled predicates:

```elisp
(defun org-gtd-pred--deadline-ts< (reference-date)
  "Return predicate checking if item's deadline is before REFERENCE-DATE.
REFERENCE-DATE can be \"today\", \"14d\", \"-7d\", etc."
  (lambda ()
    (when-let ((deadline-time (org-get-deadline-time (point))))
      (let ((ref-time (org-gtd--duration-to-reference-time reference-date)))
        (time-less-p deadline-time ref-time)))))

(defun org-gtd-pred--deadline-ts> (reference-date)
  "Return predicate checking if item's deadline is after REFERENCE-DATE."
  (lambda ()
    (when-let ((deadline-time (org-get-deadline-time (point))))
      (let ((ref-time (org-gtd--duration-to-reference-time reference-date)))
        (time-less-p ref-time deadline-time)))))

(defun org-gtd-pred--deadline-ts= (reference-date)
  "Return predicate checking if item's deadline equals REFERENCE-DATE (same day)."
  (lambda ()
    (when-let ((deadline-time (org-get-deadline-time (point))))
      (let ((ref-time (org-gtd--duration-to-reference-time reference-date)))
        (let ((dl-decoded (decode-time deadline-time))
              (ref-decoded (decode-time ref-time)))
          (and (= (nth 3 dl-decoded) (nth 3 ref-decoded))
               (= (nth 4 dl-decoded) (nth 4 ref-decoded))
               (= (nth 5 dl-decoded) (nth 5 ref-decoded))))))))

(defun org-gtd-pred--scheduled-ts< (reference-date)
  "Return predicate checking if item's scheduled is before REFERENCE-DATE."
  (lambda ()
    (when-let ((scheduled-time (org-get-scheduled-time (point))))
      (let ((ref-time (org-gtd--duration-to-reference-time reference-date)))
        (time-less-p scheduled-time ref-time)))))

(defun org-gtd-pred--scheduled-ts> (reference-date)
  "Return predicate checking if item's scheduled is after REFERENCE-DATE."
  (lambda ()
    (when-let ((scheduled-time (org-get-scheduled-time (point))))
      (let ((ref-time (org-gtd--duration-to-reference-time reference-date)))
        (time-less-p ref-time scheduled-time)))))

(defun org-gtd-pred--scheduled-ts= (reference-date)
  "Return predicate checking if item's scheduled equals REFERENCE-DATE (same day)."
  (lambda ()
    (when-let ((scheduled-time (org-get-scheduled-time (point))))
      (let ((ref-time (org-gtd--duration-to-reference-time reference-date)))
        (let ((sched-decoded (decode-time scheduled-time))
              (ref-decoded (decode-time ref-time)))
          (and (= (nth 3 sched-decoded) (nth 3 ref-decoded))
               (= (nth 4 sched-decoded) (nth 4 ref-decoded))
               (= (nth 5 sched-decoded) (nth 5 ref-decoded))))))))
```

**Step 4: Run tests to verify they pass**

Run: `eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-skip.el org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(view-lang): deadline/scheduled filters support comparisons

Both 'deadline' and 'scheduled' filters now accept comparison
expressions:
- (deadline . (< \"3d\")) - deadline within 3 days
- (scheduled . (> \"-1w\")) - scheduled more than 1 week ago

New predicates: org-gtd-pred--deadline-ts<, ts>, ts=
               org-gtd-pred--scheduled-ts<, ts>, ts=

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 8: Handle Done Filter Comparison Expressions

**Files:**
- Modify: `org-gtd-view-language.el` (done filter handling)
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for done filter comparisons**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;;; Done Filter Comparison Expression Tests

(deftest view-lang/skip-function-done-comparison ()
  "Skip function handles done=(< \"7d\") filter."
  (with-temp-buffer
    (org-mode)
    ;; Item closed 3 days ago should match (< "7d") for done
    (let ((closed-ts (format-time-string "[%Y-%m-%d %a %H:%M]"
                       (time-subtract (current-time) (days-to-time 3)))))
      (insert (format "* DONE Test\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" closed-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((done . (< "7d"))))
           (skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter '(< "7d")))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-function-done-comparison-rejects-old ()
  "Skip function rejects items closed too long ago."
  (with-temp-buffer
    (org-mode)
    ;; Item closed 10 days ago should NOT match (< "7d")
    (let ((closed-ts (format-time-string "[%Y-%m-%d %a %H:%M]"
                       (time-subtract (current-time) (days-to-time 10)))))
      (insert (format "* DONE Test\nCLOSED: %s\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n" closed-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter '(< "7d")))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))

(deftest view-lang/done-filter-rejects-positive-duration ()
  "Done filter with positive duration signals error."
  (assert-raises 'user-error
    (org-gtd-view-lang--validate-done-comparison '(< "+7d"))))

(deftest view-lang/done-filter-accepts-implicit-negative ()
  "Done filter accepts implicit negative (no sign means past)."
  (assert-nil (org-gtd-view-lang--validate-done-comparison '(< "7d"))))
```

**Step 2: Run tests to verify they fail**

Run: `eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: FAIL - validation and comparison not implemented

**Step 3: Implement done filter validation and comparison support**

Add to `org-gtd-view-language.el`:

```elisp
(defun org-gtd-view-lang--validate-done-comparison (expr)
  "Validate done filter comparison expression EXPR.
For done filters, durations are implicitly past - positive durations error."
  (org-gtd-view-lang--validate-comparison-expr expr)
  (let ((duration (cadr expr)))
    (when (and (not (string-equal duration "today"))
               (string-prefix-p "+" duration))
      (user-error "Done filter cannot use positive duration '%s' (done items are in the past)" duration)))
  nil)
```

Modify `org-gtd-view-lang--build-skip-function-for-done-filter`:

```elisp
(defun org-gtd-view-lang--build-skip-function-for-done-filter (done-value)
  "Build a skip function for done filter with DONE-VALUE.
Matches items that are done and closed within the time range.
DONE-VALUE can be:
- Symbol: t, recent, today, past-day, past-week, past-month, past-year
- Comparison: (< \"7d\") - closed within 7 days"
  (cond
   ;; Comparison expression: (< "7d"), (> "14d")
   ((and (listp done-value) (memq (car done-value) '(< > =)))
    (org-gtd-view-lang--validate-done-comparison done-value)
    (let ((op (car done-value))
          (duration (cadr done-value)))
      ;; For done, duration is implicitly negative (looking backward)
      ;; "7d" means "within the past 7 days"
      (let ((ref-duration (if (or (string-prefix-p "-" duration)
                                  (string-equal duration "today"))
                              duration
                            (concat "-" duration))))
        (lambda ()
          (let ((end (org-entry-end-position)))
            (if (not (org-entry-is-done-p))
                end  ; Skip - not done
              (let ((closed-ts (org-entry-get (point) "CLOSED")))
                (if (not closed-ts)
                    end  ; Skip - no CLOSED
                  (let ((closed-time (org-time-string-to-time closed-ts))
                        (ref-time (org-gtd--duration-to-reference-time ref-duration)))
                    (if (pcase op
                          ('< (time-less-p ref-time closed-time))  ; closed after ref (more recent)
                          ('> (time-less-p closed-time ref-time))  ; closed before ref (older)
                          ('= (let ((cl-dec (decode-time closed-time))
                                    (ref-dec (decode-time ref-time)))
                                (and (= (nth 3 cl-dec) (nth 3 ref-dec))
                                     (= (nth 4 cl-dec) (nth 4 ref-dec))
                                     (= (nth 5 cl-dec) (nth 5 ref-dec))))))
                        nil    ; Include
                      end))))))))))  ; Skip
   ;; Existing symbol handling
   (t
    (let ((days-back (org-gtd-view-lang--done-filter-days done-value)))
      (lambda ()
        (let ((end (org-entry-end-position)))
          (if (not (org-entry-is-done-p))
              end
            (if (not days-back)
                nil
              (let ((closed-ts (org-entry-get (point) "CLOSED")))
                (if (not closed-ts)
                    end
                  (let ((closed-time (org-time-string-to-time closed-ts))
                        (cutoff-time (time-subtract (current-time)
                                                    (days-to-time days-back))))
                    (if (time-less-p cutoff-time closed-time)
                        nil
                      end))))))))))))
```

**Step 4: Run tests to verify they pass**

Run: `eldev -p -dtT etest test/unit/gtd-view-language-test.el`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(view-lang): done filter supports comparison expressions

The 'done' filter now accepts comparison expressions:
- (done . (< \"7d\")) - completed within past 7 days
- (done . (> \"1M\")) - completed more than 1 month ago

Durations are implicitly past for done filters. Positive durations
(+7d) signal an error since done items are always in the past.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 9: Update Documentation

**Files:**
- Modify: `org-gtd-view-language.el` (Commentary section)

**Step 1: Update the module commentary**

Add duration comparison documentation to the Commentary section in `org-gtd-view-language.el`:

```elisp
;; Time Filter Comparison Expressions:
;;   (when . (< "14d"))         - Within next 14 days
;;   (when . (> "-7d"))         - More than 7 days ago
;;   (when . (= "today"))       - Exactly today
;;   (deadline . (< "3d"))      - Deadline within 3 days
;;   (scheduled . (> "1w"))     - Scheduled more than 1 week away
;;   (done . (< "7d"))          - Completed within past 7 days
;;
;; Duration Syntax:
;;   Format: [+-]?[0-9]+[mhdwMy]
;;   Units: m (minutes), h (hours), d (days), w (weeks), M (months), y (years)
;;   Sign: + or no sign = future, - = past
;;   Special: "today" = current date
;;
;; Examples:
;;   "14d" or "+14d" = 14 days from now
;;   "-7d" = 7 days ago
;;   "2w" = 2 weeks from now
;;   "-1M" = 1 month ago
;;
;; Note: For done/last-clocked-out filters, durations are implicitly past.
;; Positive durations (+7d) produce an error for these filters.
```

**Step 2: Commit**

```bash
git add org-gtd-view-language.el
git commit -m "docs(view-lang): document duration comparison syntax

Updates module commentary with:
- Duration comparison expression syntax
- Supported units (m, h, d, w, M, y)
- Sign conventions (+/- and implicit)
- Examples for each filter type

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"
```

---

## Task 10: Run Full Test Suite and Verify

**Step 1: Run all tests**

Run: `eldev -p -dtT etest`
Expected: All tests pass

**Step 2: Run linter**

Run: `eldev lint`
Expected: No errors

**Step 3: Final commit (if any fixes needed)**

If any issues found, fix and commit with appropriate message.

---

## Summary

This plan implements relative duration filters in 10 tasks:

1. Add M (months) and y (years) units to duration parsing
2. Add signed duration support (+/-)
3. Add duration-to-reference-time helper
4. Update timestamp predicates for duration strings
5. Add comparison expression validation
6. Handle when filter comparison expressions
7. Handle deadline/scheduled filter comparisons
8. Handle done filter comparison expressions
9. Update documentation
10. Run full test suite

Total estimated changes:
- ~50-80 lines in `org-gtd-skip.el`
- ~60-80 lines in `org-gtd-view-language.el`
- ~200-250 lines of tests
- Documentation updates
