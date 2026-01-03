# Priority, Effort, and Clocked Filters Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add three new view DSL filters: priority, effort, and clocked time for GTD views.

**Architecture:** Each filter needs (1) a translation function in org-gtd-view-language.el for org-ql queries, (2) a skip predicate in org-gtd-skip.el for native agenda blocks, and (3) integration with the existing filter router.

**Tech Stack:** Emacs Lisp, org-mode, org-duration.el for time parsing, e-unit for testing.

---

## Task 1: Priority Filter - Translation Function

**Files:**
- Modify: `org-gtd-view-language.el:293-330` (in `org-gtd-view-lang--translate-filter`)
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for priority translation**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;; Priority Filter Tests

(deftest view-lang/priority-single-value ()
  "Translates priority=A to property match."
  (let ((view-spec '((name . "High Priority")
                     (type . next-action)
                     (priority . A))))
    (assert-equal
     `(and (property "ORG_GTD" "Actions")
           (todo ,(org-gtd-keywords--next))
           (property "PRIORITY" "A"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/priority-list ()
  "Translates priority=(A B) to OR match."
  (let ((view-spec '((name . "High/Medium Priority")
                     (type . next-action)
                     (priority . (A B)))))
    (assert-equal
     `(and (property "ORG_GTD" "Actions")
           (todo ,(org-gtd-keywords--next))
           (or (property "PRIORITY" "A")
               (property "PRIORITY" "B")))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/priority-comparison-gte ()
  "Translates priority=(>= B) to B or higher (A, B)."
  (let ((view-spec '((name . "B or Higher")
                     (type . next-action)
                     (priority . (>= B)))))
    (assert-equal
     `(and (property "ORG_GTD" "Actions")
           (todo ,(org-gtd-keywords--next))
           (or (property "PRIORITY" "A")
               (property "PRIORITY" "B")))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/priority-nil-missing ()
  "Translates priority=nil to missing priority."
  (let ((view-spec '((name . "No Priority")
                     (type . next-action)
                     (priority . nil))))
    (assert-equal
     `(and (property "ORG_GTD" "Actions")
           (todo ,(org-gtd-keywords--next))
           (property-empty-or-missing "PRIORITY"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))
```

**Step 2: Run tests to verify they fail**

Run: `~/.local/bin/eldev etest -B "priority"`
Expected: FAIL - unknown filter type

**Step 3: Add priority translation function**

Add to `org-gtd-view-language.el` after line 330 (after `org-gtd-view-lang--translate-when-filter`):

```elisp
(defun org-gtd-view-lang--priority-to-number (priority)
  "Convert PRIORITY letter to numeric value for comparison.
A=1, B=2, C=3, etc. Lower number = higher priority.
Respects `org-priority-highest' and `org-priority-lowest'."
  (let ((highest (or org-priority-highest ?A))
        (char (if (symbolp priority)
                  (aref (symbol-name priority) 0)
                (aref priority 0))))
    (1+ (- char highest))))

(defun org-gtd-view-lang--priorities-in-range (op reference)
  "Return list of priority letters matching OP compared to REFERENCE.
OP is one of <, >, <=, >=.  REFERENCE is a priority symbol like B.
Returns list like (\"A\" \"B\") for (>= B)."
  (let* ((highest (or org-priority-highest ?A))
         (lowest (or org-priority-lowest ?C))
         (ref-num (org-gtd-view-lang--priority-to-number reference))
         (result '()))
    (cl-loop for char from highest to lowest
             for num from 1
             when (pcase op
                    ('< (< num ref-num))
                    ('> (> num ref-num))
                    ('<= (<= num ref-num))
                    ('>= (<= num ref-num)))  ; >= B means A,B (lower numbers)
             do (push (char-to-string char) result))
    (nreverse result)))

(defun org-gtd-view-lang--translate-priority-filter (value)
  "Translate priority VALUE to org-ql filter.
VALUE can be:
  - A symbol like A, B, C (single priority)
  - A list of symbols like (A B) (OR match)
  - A comparison like (>= B) (range match)
  - nil (missing priority)"
  (cond
   ;; nil = missing priority
   ((null value)
    (list '(property-empty-or-missing "PRIORITY")))
   ;; Comparison: (>= B), (< C), etc.
   ((and (listp value)
         (memq (car value) '(< > <= >=)))
    (let* ((op (car value))
           (ref (cadr value))
           (priorities (org-gtd-view-lang--priorities-in-range op ref)))
      (if (= (length priorities) 1)
          (list `(property "PRIORITY" ,(car priorities)))
        (list `(or ,@(mapcar (lambda (p) `(property "PRIORITY" ,p)) priorities))))))
   ;; List of priorities: (A B)
   ((and (listp value) (not (memq (car value) '(< > <= >=))))
    (let ((priorities (mapcar (lambda (p) (if (symbolp p) (symbol-name p) p)) value)))
      (if (= (length priorities) 1)
          (list `(property "PRIORITY" ,(car priorities)))
        (list `(or ,@(mapcar (lambda (p) `(property "PRIORITY" ,p)) priorities))))))
   ;; Single priority symbol: A
   ((symbolp value)
    (list `(property "PRIORITY" ,(symbol-name value))))
   ;; Single priority string: "A"
   ((stringp value)
    (list `(property "PRIORITY" ,value)))
   (t (user-error "Invalid priority filter value: %S" value))))
```

**Step 4: Add priority to the filter router**

In `org-gtd-view-lang--translate-filter`, add after the `type` case (around line 323):

```elisp
     ((eq filter-type 'priority)
      (org-gtd-view-lang--translate-priority-filter filter-value))
```

**Step 5: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "priority"`
Expected: PASS

**Step 6: Commit**

```bash
git add org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(view-lang): add priority filter translation"
```

---

## Task 2: Priority Filter - Skip Predicate

**Files:**
- Modify: `org-gtd-skip.el`
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for priority skip predicate**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;; Priority Skip Predicate Tests

(deftest view-lang/skip-priority-matches-single ()
  "Skip predicate includes items with matching priority."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-priority ?A)
    (let* ((view-spec '((type . next-action)
                        (priority . A)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Should include (return nil)
      (assert-nil result))))

(deftest view-lang/skip-priority-skips-non-matching ()
  "Skip predicate skips items with non-matching priority."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-priority ?C)
    (let* ((view-spec '((type . next-action)
                        (priority . A)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Should skip (return number)
      (assert-true (numberp result)))))

(deftest view-lang/skip-priority-nil-matches-missing ()
  "Skip predicate with priority=nil matches items without priority."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (priority . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Should include (return nil)
      (assert-nil result))))
```

**Step 2: Run tests to verify they fail**

Run: `~/.local/bin/eldev etest -B "skip-priority"`
Expected: FAIL

**Step 3: Add priority skip predicate**

Add to `org-gtd-skip.el` after `org-gtd-pred--not-done`:

```elisp
;;;; Priority Predicates

(defun org-gtd-pred--priority-matches (value)
  "Return predicate checking if item priority matches VALUE.
VALUE can be:
  - A symbol/string like A (single priority)
  - A list like (A B) (any of these priorities)
  - A comparison like (>= B)
  - nil (no priority set)"
  (lambda ()
    (let ((item-priority (org-entry-get (point) "PRIORITY")))
      (cond
       ;; nil = match missing priority
       ((null value)
        (or (null item-priority)
            (string-empty-p item-priority)))
       ;; Comparison: (>= B)
       ((and (listp value) (memq (car value) '(< > <= >=)))
        (when item-priority
          (let* ((op (car value))
                 (ref (cadr value))
                 (highest (or org-priority-highest ?A))
                 (item-num (1+ (- (aref item-priority 0) highest)))
                 (ref-num (1+ (- (aref (symbol-name ref) 0) highest))))
            (pcase op
              ('< (< item-num ref-num))
              ('> (> item-num ref-num))
              ('<= (<= item-num ref-num))
              ('>= (<= item-num ref-num))))))  ; >= B means numerically <= B
       ;; List: (A B)
       ((listp value)
        (when item-priority
          (member item-priority
                  (mapcar (lambda (p) (if (symbolp p) (symbol-name p) p)) value))))
       ;; Single value
       (t
        (when item-priority
          (equal item-priority
                 (if (symbolp value) (symbol-name value) value))))))))
```

**Step 4: Integrate priority predicate into skip function builder**

In `org-gtd-view-lang--build-skip-function` (org-gtd-view-language.el), add after the area-filter handling:

```elisp
        ;; Add priority predicate
        (when-let ((priority-filter (alist-get 'priority gtd-view-spec)))
          (push (org-gtd-pred--priority-matches priority-filter) predicates))
```

Also handle the special case of `(priority . nil)`:

```elisp
        ;; Handle priority=nil explicitly (not filtered by when-let)
        (when (and (assq 'priority gtd-view-spec)
                   (null (alist-get 'priority gtd-view-spec)))
          (push (org-gtd-pred--priority-matches nil) predicates))
```

**Step 5: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "skip-priority"`
Expected: PASS

**Step 6: Commit**

```bash
git add org-gtd-skip.el org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(skip): add priority skip predicate"
```

---

## Task 3: Effort Filter - Translation Function

**Files:**
- Modify: `org-gtd-view-language.el`
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for effort translation**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;; Effort Filter Tests

(deftest view-lang/effort-less-than ()
  "Translates effort=(< \"0:30\") to effort comparison."
  (let ((view-spec '((name . "Quick Tasks")
                     (type . next-action)
                     (effort . (< "0:30")))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      ;; Should contain effort-< predicate
      (assert-true (cl-find 'effort-< (flatten-list query))))))

(deftest view-lang/effort-greater-than ()
  "Translates effort=(> \"1:00\") to effort comparison."
  (let ((view-spec '((name . "Deep Work")
                     (type . next-action)
                     (effort . (> "1:00")))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      (assert-true (cl-find 'effort-> (flatten-list query))))))

(deftest view-lang/effort-between ()
  "Translates effort=(between \"0:15\" \"1:00\") to range."
  (let ((view-spec '((name . "Medium Tasks")
                     (type . next-action)
                     (effort . (between "0:15" "1:00")))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      (assert-true (cl-find 'effort-between (flatten-list query))))))

(deftest view-lang/effort-nil-missing ()
  "Translates effort=nil to missing effort."
  (let ((view-spec '((name . "No Estimate")
                     (type . next-action)
                     (effort . nil))))
    (assert-equal
     `(and (property "ORG_GTD" "Actions")
           (todo ,(org-gtd-keywords--next))
           (property-empty-or-missing "Effort"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))
```

**Step 2: Run tests to verify they fail**

Run: `~/.local/bin/eldev etest -B "effort"`
Expected: FAIL

**Step 3: Add effort translation function**

Add to `org-gtd-view-language.el`:

```elisp
(defun org-gtd-view-lang--translate-effort-filter (value)
  "Translate effort VALUE to org-ql filter.
VALUE can be:
  - (< \"0:30\") - less than 30 minutes
  - (> \"1:00\") - more than 1 hour
  - (between \"0:15\" \"1:00\") - range (inclusive)
  - nil - missing effort estimate"
  (cond
   ((null value)
    (list '(property-empty-or-missing "Effort")))
   ((and (listp value) (eq (car value) '<))
    (list `(effort-< ,(cadr value))))
   ((and (listp value) (eq (car value) '>))
    (list `(effort-> ,(cadr value))))
   ((and (listp value) (eq (car value) 'between))
    (list `(effort-between ,(cadr value) ,(caddr value))))
   (t (user-error "Invalid effort filter value: %S" value))))
```

**Step 4: Add effort to the filter router**

In `org-gtd-view-lang--translate-filter`, add:

```elisp
     ((eq filter-type 'effort)
      (org-gtd-view-lang--translate-effort-filter filter-value))
```

**Step 5: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "effort"`
Expected: PASS

**Step 6: Commit**

```bash
git add org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(view-lang): add effort filter translation"
```

---

## Task 4: Effort Filter - Skip Predicate

**Files:**
- Modify: `org-gtd-skip.el`
- Modify: `org-gtd-view-language.el`
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for effort skip predicate**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;; Effort Skip Predicate Tests

(deftest view-lang/skip-effort-less-than-includes ()
  "Skip predicate includes items with effort under threshold."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:Effort: 0:15\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (effort . (< "0:30"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-effort-less-than-skips ()
  "Skip predicate skips items with effort over threshold."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:Effort: 2:00\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (effort . (< "0:30"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))

(deftest view-lang/skip-effort-nil-matches-missing ()
  "Skip predicate with effort=nil matches items without effort."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (effort . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-effort-parses-duration-formats ()
  "Skip predicate handles various duration formats."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:Effort: 1d2h30min\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (effort . (> "1:00"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; 1d2h30min > 1:00, should include
      (assert-nil result))))
```

**Step 2: Run tests to verify they fail**

Run: `~/.local/bin/eldev etest -B "skip-effort"`
Expected: FAIL

**Step 3: Add effort skip predicate**

Add to `org-gtd-skip.el`:

```elisp
;;;; Effort Predicates

(require 'org-duration)

(defun org-gtd-pred--effort-matches (value)
  "Return predicate checking if item effort matches VALUE.
VALUE can be:
  - (< \"0:30\") - less than 30 minutes
  - (> \"1:00\") - more than 1 hour
  - (between \"0:15\" \"1:00\") - range (inclusive)
  - nil - no effort set"
  (lambda ()
    (let ((effort-str (org-entry-get (point) "Effort")))
      (cond
       ;; nil = match missing effort
       ((null value)
        (or (null effort-str)
            (string-empty-p effort-str)))
       ;; Have effort, need to compare
       ((and effort-str (not (string-empty-p effort-str)))
        (condition-case nil
            (let ((effort-mins (org-duration-to-minutes effort-str)))
              (pcase (car value)
                ('<
                 (let ((threshold (org-duration-to-minutes (cadr value))))
                   (< effort-mins threshold)))
                ('>
                 (let ((threshold (org-duration-to-minutes (cadr value))))
                   (> effort-mins threshold)))
                ('between
                 (let ((low (org-duration-to-minutes (cadr value)))
                       (high (org-duration-to-minutes (caddr value))))
                   (and (>= effort-mins low) (<= effort-mins high))))
                (_ nil)))
          (error nil)))  ; Invalid duration format, skip item
       (t nil)))))  ; No effort but filter wants comparison
```

**Step 4: Integrate effort predicate into skip function builder**

In `org-gtd-view-lang--build-skip-function`, add:

```elisp
        ;; Add effort predicate
        (when-let ((effort-filter (alist-get 'effort gtd-view-spec)))
          (push (org-gtd-pred--effort-matches effort-filter) predicates))
        ;; Handle effort=nil explicitly
        (when (and (assq 'effort gtd-view-spec)
                   (null (alist-get 'effort gtd-view-spec)))
          (push (org-gtd-pred--effort-matches nil) predicates))
```

**Step 5: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "skip-effort"`
Expected: PASS

**Step 6: Commit**

```bash
git add org-gtd-skip.el org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(skip): add effort skip predicate"
```

---

## Task 5: Clocked Filter - Translation Function

**Files:**
- Modify: `org-gtd-view-language.el`
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for clocked translation**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;; Clocked Filter Tests

(deftest view-lang/clocked-less-than ()
  "Translates clocked=(< \"0:30\") to clocked comparison."
  (let ((view-spec '((name . "Low Investment")
                     (type . next-action)
                     (clocked . (< "0:30")))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      (assert-true (cl-find 'clocked-< (flatten-list query))))))

(deftest view-lang/clocked-greater-than ()
  "Translates clocked=(> \"2:00\") to clocked comparison."
  (let ((view-spec '((name . "High Investment")
                     (type . next-action)
                     (clocked . (> "2:00")))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      (assert-true (cl-find 'clocked-> (flatten-list query))))))

(deftest view-lang/clocked-nil-zero ()
  "Translates clocked=nil to zero clocked time."
  (let ((view-spec '((name . "Not Started")
                     (type . next-action)
                     (clocked . nil))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      (assert-true (cl-find 'clocked-zero (flatten-list query))))))
```

**Step 2: Run tests to verify they fail**

Run: `~/.local/bin/eldev etest -B "clocked"`
Expected: FAIL

**Step 3: Add clocked translation function**

Add to `org-gtd-view-language.el`:

```elisp
(defun org-gtd-view-lang--translate-clocked-filter (value)
  "Translate clocked VALUE to org-ql filter.
VALUE can be:
  - (< \"0:30\") - less than 30 minutes clocked
  - (> \"2:00\") - more than 2 hours clocked
  - (between \"0:30\" \"2:00\") - range (inclusive)
  - nil - zero time clocked"
  (cond
   ((null value)
    (list '(clocked-zero)))
   ((and (listp value) (eq (car value) '<))
    (list `(clocked-< ,(cadr value))))
   ((and (listp value) (eq (car value) '>))
    (list `(clocked-> ,(cadr value))))
   ((and (listp value) (eq (car value) 'between))
    (list `(clocked-between ,(cadr value) ,(caddr value))))
   (t (user-error "Invalid clocked filter value: %S" value))))
```

**Step 4: Add clocked to the filter router**

In `org-gtd-view-lang--translate-filter`, add:

```elisp
     ((eq filter-type 'clocked)
      (org-gtd-view-lang--translate-clocked-filter filter-value))
```

**Step 5: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "clocked"`
Expected: PASS

**Step 6: Commit**

```bash
git add org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(view-lang): add clocked filter translation"
```

---

## Task 6: Clocked Filter - Skip Predicate

**Files:**
- Modify: `org-gtd-skip.el`
- Modify: `org-gtd-view-language.el`
- Test: `test/unit/gtd-view-language-test.el`

**Step 1: Write failing tests for clocked skip predicate**

Add to `test/unit/gtd-view-language-test.el`:

```elisp
;;; Clocked Skip Predicate Tests

(deftest view-lang/skip-clocked-nil-matches-zero ()
  "Skip predicate with clocked=nil matches items with zero clock time."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (clocked . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; No clock entries = zero time, should match
      (assert-nil result))))

(deftest view-lang/skip-clocked-greater-skips-zero ()
  "Skip predicate with clocked=(> \"0:30\") skips items with zero time."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (clocked . (> "0:30"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; No clock = 0 time, should skip
      (assert-true (numberp result)))))
```

**Step 2: Run tests to verify they fail**

Run: `~/.local/bin/eldev etest -B "skip-clocked"`
Expected: FAIL

**Step 3: Add clocked skip predicate**

Add to `org-gtd-skip.el`:

```elisp
;;;; Clocked Time Predicates

(defun org-gtd-pred--clocked-matches (value)
  "Return predicate checking if item clocked time matches VALUE.
VALUE can be:
  - (< \"0:30\") - less than 30 minutes
  - (> \"2:00\") - more than 2 hours
  - (between \"0:30\" \"2:00\") - range (inclusive)
  - nil - zero time clocked"
  (lambda ()
    (let ((clocked-mins (save-excursion
                          (org-clock-sum-current-item))))
      (cond
       ;; nil = match zero clocked time
       ((null value)
        (or (null clocked-mins) (= clocked-mins 0)))
       ;; Have clocked time, compare
       (t
        (let ((mins (or clocked-mins 0)))
          (pcase (car value)
            ('<
             (let ((threshold (org-duration-to-minutes (cadr value))))
               (< mins threshold)))
            ('>
             (let ((threshold (org-duration-to-minutes (cadr value))))
               (> mins threshold)))
            ('between
             (let ((low (org-duration-to-minutes (cadr value)))
                   (high (org-duration-to-minutes (caddr value))))
               (and (>= mins low) (<= mins high))))
            (_ nil))))))))
```

**Step 4: Integrate clocked predicate into skip function builder**

In `org-gtd-view-lang--build-skip-function`, add:

```elisp
        ;; Add clocked predicate
        (when-let ((clocked-filter (alist-get 'clocked gtd-view-spec)))
          (push (org-gtd-pred--clocked-matches clocked-filter) predicates))
        ;; Handle clocked=nil explicitly
        (when (and (assq 'clocked gtd-view-spec)
                   (null (alist-get 'clocked gtd-view-spec)))
          (push (org-gtd-pred--clocked-matches nil) predicates))
```

**Step 5: Run tests to verify they pass**

Run: `~/.local/bin/eldev etest -B "skip-clocked"`
Expected: PASS

**Step 6: Commit**

```bash
git add org-gtd-skip.el org-gtd-view-language.el test/unit/gtd-view-language-test.el
git commit -m "feat(skip): add clocked skip predicate"
```

---

## Task 7: Integration Tests

**Files:**
- Create: `test/integration/view-filters-test.el`

**Step 1: Write integration tests**

Create `test/integration/view-filters-test.el`:

```elisp
;;; view-filters-test.el --- Integration tests for view filters -*- lexical-binding: t; coding: utf-8 -*-

(require 'e-unit)
(require 'org-gtd)
(require 'ogt-test-helpers)

;;; Priority Integration Tests

(deftest view-filters/priority-end-to-end ()
  "Priority filter works in full view creation flow."
  (ogt-with-test-environment
   (let ((view-spec '((name . "High Priority Actions")
                      (type . next-action)
                      (priority . A))))
     ;; Should not error when creating the view
     (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
       (assert-true commands)
       (assert-equal 1 (length (caddr (car commands))))))))

;;; Effort Integration Tests

(deftest view-filters/effort-end-to-end ()
  "Effort filter works in full view creation flow."
  (ogt-with-test-environment
   (let ((view-spec '((name . "Quick Wins")
                      (type . next-action)
                      (effort . (< "0:30")))))
     (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
       (assert-true commands)))))

;;; Clocked Integration Tests

(deftest view-filters/clocked-end-to-end ()
  "Clocked filter works in full view creation flow."
  (ogt-with-test-environment
   (let ((view-spec '((name . "Invested Tasks")
                      (type . next-action)
                      (clocked . (> "0:30")))))
     (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
       (assert-true commands)))))

;;; Combined Filters

(deftest view-filters/combined-priority-effort ()
  "Priority and effort filters combine correctly."
  (ogt-with-test-environment
   (let ((view-spec '((name . "High Priority Quick Wins")
                      (type . next-action)
                      (priority . (A B))
                      (effort . (< "0:30")))))
     (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
       (assert-true commands)))))

(provide 'view-filters-test)
;;; view-filters-test.el ends here
```

**Step 2: Run integration tests**

Run: `~/.local/bin/eldev etest -B "view-filters/"`
Expected: PASS

**Step 3: Commit**

```bash
git add test/integration/view-filters-test.el
git commit -m "test: add integration tests for priority/effort/clocked filters"
```

---

## Task 8: Documentation Update

**Files:**
- Modify: `doc/org-gtd.org`

**Step 1: Add filter documentation**

In `doc/org-gtd.org`, in the View DSL section, add:

```org
**** Priority Filter

Filter by org-mode priority ([#A], [#B], [#C]):

#+begin_src elisp
;; Single priority
'((type . next-action)
  (priority . A))

;; Multiple priorities (OR)
'((type . next-action)
  (priority . (A B)))

;; Comparison (B or higher = A, B)
'((type . next-action)
  (priority . (>= B)))

;; Items missing priority
'((type . next-action)
  (priority . nil))
#+end_src

**** Effort Filter

Filter by effort estimate (Effort property):

#+begin_src elisp
;; Under 30 minutes
'((type . next-action)
  (effort . (< "0:30")))

;; Over 1 hour
'((type . next-action)
  (effort . (> "1:00")))

;; Between 15 and 60 minutes
'((type . next-action)
  (effort . (between "0:15" "1:00")))

;; Items missing effort estimate
'((type . next-action)
  (effort . nil))
#+end_src

Duration strings support all org-mode formats: "0:30", "1:23:45", "1d3h5min", etc.

**** Clocked Filter

Filter by total time clocked on an item:

#+begin_src elisp
;; Tasks I haven't started yet
'((type . next-action)
  (clocked . nil))

;; Tasks I've invested significant time in
'((type . next-action)
  (clocked . (> "2:00")))

;; Tasks with moderate investment
'((type . next-action)
  (clocked . (between "0:30" "2:00")))
#+end_src
```

**Step 2: Commit**

```bash
git add doc/org-gtd.org
git commit -m "docs: add priority/effort/clocked filter documentation"
```

---

## Task 9: Run Full Test Suite

**Step 1: Run all tests**

Run: `~/.local/bin/eldev etest -B`
Expected: All tests pass

**Step 2: Compile to check for warnings**

Run: `~/.local/bin/eldev compile`
Expected: No errors or warnings

---

## Summary

Total tasks: 9
Files modified:
- `org-gtd-view-language.el` - 3 translation functions + router updates
- `org-gtd-skip.el` - 3 skip predicates + require org-duration
- `test/unit/gtd-view-language-test.el` - ~20 new tests
- `test/integration/view-filters-test.el` - 4 integration tests
- `doc/org-gtd.org` - filter documentation
