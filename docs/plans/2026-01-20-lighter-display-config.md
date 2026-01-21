# Configurable Lighter Display Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Let users control when the org-gtd-mode lighter appears in the mode-line (Issue #254).

**Architecture:** Add a defcustom with three options (always/never/when-non-zero), modify `org-gtd-mode-lighter` to respect it.

**Tech Stack:** Emacs Lisp, defcustom, pcase

---

## Task 1: Add defcustom for lighter display

**Files:**
- Modify: `org-gtd-mode.el` (add after line 62, in Customization section)

**Step 1: Write the failing test**

In `test/unit/mode-test.el`, add:

```elisp
(deftest mode/lighter-display-variable-exists ()
  "The lighter display customization variable exists with correct default."
  (assert-true (boundp 'org-gtd-mode-lighter-display))
  (assert-equal 'always (default-value 'org-gtd-mode-lighter-display)))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest test/unit/mode-test.el -r dot`
Expected: FAIL - variable not bound

**Step 3: Write the defcustom**

Add to `org-gtd-mode.el` after `org-gtd-mode-update-interval`:

```elisp
(defcustom org-gtd-mode-lighter-display 'always
  "When to display the org-gtd-mode lighter in the mode-line.

- `always': Always show (e.g., GTD[0], GTD[5]) - default behavior
- `never': Never show the lighter
- `when-non-zero': Only show when inbox has items (hides GTD[0])"
  :group 'org-gtd
  :type '(choice (const :tag "Always show" always)
                 (const :tag "Never show" never)
                 (const :tag "Only when inbox non-empty" when-non-zero)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest test/unit/mode-test.el -r dot`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-mode.el test/unit/mode-test.el
git commit -m "feat(mode): add org-gtd-mode-lighter-display customization variable"
```

---

## Task 2: Modify existing lighter tests to be explicit

**Files:**
- Modify: `test/unit/mode-test.el`

**Step 1: Read existing tests**

Find tests `mode/lighter-shows-inbox-count` and `mode/lighter-shows-zero-for-empty-inbox`.

**Step 2: Wrap with explicit configuration**

Update tests to explicitly set `org-gtd-mode-lighter-display` to `'always`:

```elisp
(deftest mode/lighter-shows-inbox-count ()
  "Mode lighter shows inbox count when display is set to always."
  (let ((org-gtd-mode-lighter-display 'always))
    (capture-inbox-item "one")
    (capture-inbox-item "two")
    (assert-equal " GTD[2]" (org-gtd-mode-lighter))))

(deftest mode/lighter-shows-zero-for-empty-inbox ()
  "Mode lighter shows zero count when display is set to always and inbox empty."
  (let ((org-gtd-mode-lighter-display 'always))
    (assert-equal " GTD[0]" (org-gtd-mode-lighter))))
```

**Step 3: Run tests to verify they still pass**

Run: `~/bin/eldev etest test/unit/mode-test.el -r dot`
Expected: PASS

**Step 4: Commit**

```bash
git add test/unit/mode-test.el
git commit -m "test(mode): make lighter tests explicit about display configuration"
```

---

## Task 3: Implement 'never' option

**Files:**
- Modify: `org-gtd-mode.el:191-193` (org-gtd-mode-lighter function)
- Modify: `test/unit/mode-test.el`

**Step 1: Write failing test**

```elisp
(deftest mode/lighter-never-returns-nil ()
  "Mode lighter returns nil when display is set to never."
  (let ((org-gtd-mode-lighter-display 'never))
    (capture-inbox-item "item")
    (assert-nil (org-gtd-mode-lighter))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest test/unit/mode-test.el -r dot`
Expected: FAIL - returns " GTD[1]" instead of nil

**Step 3: Update org-gtd-mode-lighter**

```elisp
(defun org-gtd-mode-lighter ()
  "Return the mode-line lighter string based on `org-gtd-mode-lighter-display'."
  (pcase org-gtd-mode-lighter-display
    ('never nil)
    (_ (format " GTD[%d]" (org-gtd-inbox-count)))))
```

**Step 4: Run tests to verify they pass**

Run: `~/bin/eldev etest test/unit/mode-test.el -r dot`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-mode.el test/unit/mode-test.el
git commit -m "feat(mode): implement 'never' option for lighter display"
```

---

## Task 4: Implement 'when-non-zero' option

**Files:**
- Modify: `org-gtd-mode.el:191-193`
- Modify: `test/unit/mode-test.el`

**Step 1: Write failing tests**

```elisp
(deftest mode/lighter-when-non-zero-hides-empty ()
  "Mode lighter returns nil when display is when-non-zero and inbox empty."
  (let ((org-gtd-mode-lighter-display 'when-non-zero))
    (assert-nil (org-gtd-mode-lighter))))

(deftest mode/lighter-when-non-zero-shows-count ()
  "Mode lighter shows count when display is when-non-zero and inbox has items."
  (let ((org-gtd-mode-lighter-display 'when-non-zero))
    (capture-inbox-item "item")
    (capture-inbox-item "another")
    (assert-equal " GTD[2]" (org-gtd-mode-lighter))))
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev etest test/unit/mode-test.el -r dot`
Expected: FAIL - first test returns " GTD[0]" instead of nil

**Step 3: Update org-gtd-mode-lighter**

```elisp
(defun org-gtd-mode-lighter ()
  "Return the mode-line lighter string based on `org-gtd-mode-lighter-display'."
  (pcase org-gtd-mode-lighter-display
    ('never nil)
    ('when-non-zero
     (let ((count (org-gtd-inbox-count)))
       (when (> count 0)
         (format " GTD[%d]" count))))
    (_ ; 'always or any other value
     (format " GTD[%d]" (org-gtd-inbox-count)))))
```

**Step 4: Run tests to verify they pass**

Run: `~/bin/eldev etest test/unit/mode-test.el -r dot`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-mode.el test/unit/mode-test.el
git commit -m "feat(mode): implement 'when-non-zero' option for lighter display

Closes #254"
```

---

## Task 5: Run full test suite

**Step 1: Run all tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 2: If failures, fix and recommit**

---

## Task 6: Update documentation

**Files:**
- Modify: `doc/org-gtd.org` (add to customization section)

**Step 1: Add documentation for new variable**

Find the customization variables section and add:

```org
**** ~org-gtd-mode-lighter-display~

Controls when the GTD lighter appears in the mode-line.

- ~always~ (default): Always show, e.g., ~GTD[0]~, ~GTD[5]~
- ~never~: Never show the lighter
- ~when-non-zero~: Only show when inbox has items

#+begin_src elisp
;; Hide lighter when inbox is empty
(setq org-gtd-mode-lighter-display 'when-non-zero)
#+end_src
```

**Step 2: Regenerate info file**

User will regenerate `org-gtd.info` from Emacs.

**Step 3: Commit**

```bash
git add doc/org-gtd.org org-gtd.info
git commit -m "docs: document org-gtd-mode-lighter-display variable"
```
