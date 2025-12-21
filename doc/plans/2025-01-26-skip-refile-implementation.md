# Skip Refile Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow users to reconfigure GTD items in place without refiling, via C-u prefix or transient toggle.

**Architecture:** Add buffer-local flag `org-gtd-clarify--skip-refile` set at clarify-time or toggle-time. Category `--finalize` functions check flag and either refile or update-in-place. Archive categories (quick-action, knowledge, trash) are unaffected.

**Tech Stack:** Emacs Lisp, transient.el, buttercup tests

---

## Task 1: Add Skip-Refile Variable

**Files:**
- Modify: `org-gtd-clarify.el:71-84` (Variables section)

**Step 1: Add the variable definition**

Add after line 84 (after `org-gtd-clarify--window-config`):

```elisp
(defvar-local org-gtd-clarify--skip-refile nil
  "When non-nil, update item in place instead of refiling.
Set via C-u prefix to clarify commands or transient toggle.")
```

**Step 2: Verify no syntax errors**

Run: `eldev compile org-gtd-clarify.el`
Expected: No errors

**Step 3: Commit**

```bash
git add org-gtd-clarify.el
git commit -m "feat: add org-gtd-clarify--skip-refile variable"
```

---

## Task 2: Modify org-gtd-clarify-item for Prefix Arg

**Files:**
- Modify: `org-gtd-clarify.el:134-152` (org-gtd-clarify-item function)
- Test: `test/clarify-test.el` (new test)

**Step 1: Write failing test**

Create or add to `test/clarify-test.el`:

```elisp
(describe "org-gtd-clarify-item"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "sets skip-refile flag when called with prefix arg"
    (create-single-action "Test item")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Test item")
      (org-back-to-heading t)
      (let ((current-prefix-arg '(4)))
        (org-gtd-clarify-item))
      ;; Find the WIP buffer and check the flag
      (let ((wip-buffers (org-gtd-wip--get-buffers)))
        (expect wip-buffers :not :to-be nil)
        (with-current-buffer (car wip-buffers)
          (expect org-gtd-clarify--skip-refile :to-be t))))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "sets skip-refile flag"`
Expected: FAIL (flag not set)

**Step 3: Modify org-gtd-clarify-item**

Change the function to capture prefix arg and set flag. Replace lines 134-152:

```elisp
;;;###autoload
(defun org-gtd-clarify-item (&optional marker window-config)
  "Clarify the GTD item at point for decision-making.
Opens a dedicated clarification buffer where you can refine the item's
details before organizing it.

With prefix argument (C-u), mark item for in-place update instead of refile.

MARKER must be a marker pointing to an org heading.
WINDOW-CONFIG is the window config to set after clarification finishes."
  (interactive)
  (let* ((skip-refile current-prefix-arg)
         (window-config (or window-config (current-window-configuration)))
         (source-heading-marker (or marker (point-marker)))
         (clarify-id (org-gtd-id-get-create source-heading-marker))
         (processing-buffer (org-gtd-wip--get-buffer clarify-id)))
    (org-gtd-wip--maybe-initialize-buffer-contents source-heading-marker processing-buffer)
    (with-current-buffer processing-buffer
      (org-gtd-clarify-mode 1)
      (setq-local org-gtd-clarify--window-config window-config
                  org-gtd-clarify--source-heading-marker source-heading-marker
                  org-gtd-clarify--clarify-id clarify-id
                  org-gtd-clarify--skip-refile (when skip-refile t)))
    (org-gtd-clarify-setup-windows processing-buffer)))
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "sets skip-refile flag"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/clarify-test.el
git commit -m "feat: org-gtd-clarify-item respects C-u prefix for skip-refile"
```

---

## Task 3: Modify org-gtd-clarify-agenda-item for Prefix Arg

**Files:**
- Modify: `org-gtd-clarify.el:123-131` (org-gtd-clarify-agenda-item function)
- Test: `test/clarify-test.el`

**Step 1: Write failing test**

Add to `test/clarify-test.el`:

```elisp
(describe "org-gtd-clarify-agenda-item"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "sets skip-refile flag when called with prefix arg"
    (create-single-action "Agenda test item")
    (ogt--save-all-buffers)
    (org-gtd-engage)
    (with-current-buffer org-agenda-buffer
      (goto-char (point-min))
      (search-forward "Agenda test item")
      (let ((current-prefix-arg '(4)))
        (org-gtd-clarify-agenda-item))
      ;; Find the WIP buffer and check the flag
      (let ((wip-buffers (org-gtd-wip--get-buffers)))
        (expect wip-buffers :not :to-be nil)
        (with-current-buffer (car wip-buffers)
          (expect org-gtd-clarify--skip-refile :to-be t))))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "org-gtd-clarify-agenda-item.*sets skip-refile"`
Expected: FAIL

**Step 3: Modify org-gtd-clarify-agenda-item**

The function needs to capture prefix arg before calling org-gtd-clarify-item. Replace lines 123-131:

```elisp
(defun org-gtd-clarify-agenda-item ()
  "Process item at point on agenda view.
With prefix argument (C-u), mark item for in-place update instead of refile."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let ((heading-marker (or (org-get-at-bol 'org-marker)
                            (org-agenda-error)))
        (prefix-arg current-prefix-arg))
    ;; Rebind current-prefix-arg so org-gtd-clarify-item sees it
    (let ((current-prefix-arg prefix-arg))
      (org-gtd-clarify-item heading-marker
                            (current-window-configuration)))))
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "org-gtd-clarify-agenda-item.*sets skip-refile"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/clarify-test.el
git commit -m "feat: org-gtd-clarify-agenda-item passes prefix arg through"
```

---

## Task 4: Add Update-In-Place Function

**Files:**
- Modify: `org-gtd-organize.el` (add new function after line 136)
- Test: `test/organizing-test.el`

**Step 1: Write failing test**

Add to `test/organizing-test.el`:

```elisp
(describe "org-gtd-organize--update-in-place"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "replaces original heading with WIP buffer content"
    (create-single-action "Original title")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Original title")
      (org-back-to-heading t)
      (let ((original-marker (point-marker)))
        ;; Clarify the item
        (org-gtd-clarify-item)
        ;; Get the WIP buffer and modify content
        (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
          (with-current-buffer wip-buffer
            (goto-char (point-min))
            (search-forward "Original title")
            (replace-match "Modified title")
            ;; Set up the marker reference
            (setq-local org-gtd-clarify--source-heading-marker original-marker)
            ;; Call update-in-place
            (org-gtd-organize--update-in-place)))
        ;; Verify original location has new content
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (expect (search-forward "Modified title" nil t) :to-be-truthy)
          (goto-char (point-min))
          (expect (search-forward "Original title" nil t) :to-be nil))))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "replaces original heading"`
Expected: FAIL (function doesn't exist)

**Step 3: Add the function**

Add to `org-gtd-organize.el` after line 136 (in Private functions section):

```elisp
(defun org-gtd-organize--update-in-place ()
  "Replace original heading with configured content from WIP buffer.
Uses `org-gtd-clarify--source-heading-marker' to find the original location."
  (let ((new-content (save-excursion
                       (goto-char (point-min))
                       (when (org-before-first-heading-p)
                         (org-next-visible-heading 1))
                       (org-copy-subtree)
                       (current-kill 0))))
    (when (and (boundp 'org-gtd-clarify--source-heading-marker)
               org-gtd-clarify--source-heading-marker
               (markerp org-gtd-clarify--source-heading-marker)
               (marker-buffer org-gtd-clarify--source-heading-marker))
      (with-current-buffer (marker-buffer org-gtd-clarify--source-heading-marker)
        (goto-char org-gtd-clarify--source-heading-marker)
        (org-back-to-heading t)
        (org-cut-subtree)
        (insert new-content)
        (save-buffer)))))
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "replaces original heading"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-organize.el test/organizing-test.el
git commit -m "feat: add org-gtd-organize--update-in-place function"
```

---

## Task 5: Add Transient Toggle

**Files:**
- Modify: `org-gtd-organize.el:85-98` (transient definition)
- Test: `test/organizing-test.el`

**Step 1: Write failing test**

Add to `test/organizing-test.el`:

```elisp
(describe "org-gtd-organize transient"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "has skip-refile toggle when not processing inbox"
    (create-single-action "Test item")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Test item")
      (org-back-to-heading t)
      (org-gtd-clarify-item)
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        ;; Not inbox processing
        (expect org-gtd-clarify--inbox-p :to-be nil)
        ;; Check transient has the suffix
        (let ((suffixes (transient-suffixes 'org-gtd-organize)))
          (expect (cl-find-if (lambda (s)
                                (and (listp s)
                                     (string= "-n" (car s))))
                              (flatten-tree suffixes))
                  :to-be-truthy))))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "has skip-refile toggle"`
Expected: FAIL (toggle doesn't exist)

**Step 3: Modify transient definition**

Replace lines 85-98:

```elisp
(transient-define-prefix org-gtd-organize ()
  "Choose how to categorize the current item."
  [:if (lambda () (not org-gtd-clarify--inbox-p))
   "Options"
   ("-n" "Update in place (no refile)" org-gtd-clarify--skip-refile)]
  ["Actionable"
   [("q" "Quick action" org-gtd-quick-action)
    ("s" "Single action" org-gtd-single-action)]
   [("d" "Delegate" org-gtd-delegate)
    ("c" "Calendar" org-gtd-calendar)
    ("h" "Habit" org-gtd-habit)]]
  [("p" "Project (multi-step)" org-gtd-project-new)
   ("a" "Add this task to an existing project" org-gtd-project-extend)]
  ["Non-actionable"
   [("i" "Incubate" org-gtd-incubate)
    ("k" "Knowledge to be stored" org-gtd-knowledge)]
   [("t" "Trash" org-gtd-trash)]])
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "has skip-refile toggle"`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-organize.el test/organizing-test.el
git commit -m "feat: add skip-refile toggle to org-gtd-organize transient"
```

---

## Task 6: Modify org-gtd-organize--call for Skip-Refile

**Files:**
- Modify: `org-gtd-organize.el:139-175`

**Step 1: Modify the cleanup section**

The section that cuts the original (lines 156-165) should be conditional. Replace that section:

```elisp
(defun org-gtd-organize--call (func)
  "Wrap FUNC, which does the real work, to keep Emacs clean.
This handles the internal bits of `org-gtd'."
  (goto-char (point-min))
  (when (org-before-first-heading-p)
    (org-next-visible-heading 1))
  ;; v4: Users configure org-agenda-files directly, no need for with-org-gtd-context
  (let ((error-caught
         (catch 'org-gtd-error
           (save-excursion (funcall func))
           nil))) ;; Return nil when no error was thrown
    (unless error-caught
      ;; Only run cleanup if no error was thrown
      (let ((loop-p org-gtd-clarify--inbox-p)
            (task-id org-gtd-clarify--clarify-id)
            (window-config org-gtd-clarify--window-config)
            (skip-refile org-gtd-clarify--skip-refile))
        ;; Only cut original if we refiled (not updated in place)
        (unless skip-refile
          (when (and (boundp 'org-gtd-clarify--source-heading-marker)
                     org-gtd-clarify--source-heading-marker
                     (markerp org-gtd-clarify--source-heading-marker))
            (let ((buffer (marker-buffer org-gtd-clarify--source-heading-marker))
                  (position (marker-position org-gtd-clarify--source-heading-marker)))
              (when (and buffer position)
                (with-current-buffer buffer
                  (goto-char position)
                  (with-temp-message ""
                    (org-cut-subtree)))))))
        (when task-id
          (org-gtd-wip--cleanup-temp-file task-id))
        (when window-config
          (set-window-configuration window-config))
        (if loop-p (org-gtd-process-inbox))
        ;; Save GTD buffers after organizing
        (org-gtd-save-buffers)
        ;; Clean up horizons view for one-off clarification
        (unless loop-p
          (org-gtd-clarify--cleanup-horizons-view))))))
```

**Step 2: Verify compilation**

Run: `eldev compile org-gtd-organize.el`
Expected: No errors

**Step 3: Commit**

```bash
git add org-gtd-organize.el
git commit -m "feat: org-gtd-organize--call skips cutting original when skip-refile set"
```

---

## Task 7: Modify Category Finalize Functions

**Files:**
- Modify: `org-gtd-single-action.el:79-83`
- Modify: `org-gtd-calendar.el:88-92`
- Modify: `org-gtd-delegate.el:133-137`
- Modify: `org-gtd-habit.el:92-96`
- Modify: `org-gtd-incubate.el:157-161`
- Test: `test/organizing-test.el`

**Step 1: Write failing test for single-action**

Add to `test/organizing-test.el`:

```elisp
(describe "skip-refile behavior"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "updates single-action in place when skip-refile is set"
    (create-single-action "Update me")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Update me")
      (org-back-to-heading t)
      (let ((original-pos (point)))
        ;; Clarify with skip-refile
        (let ((current-prefix-arg '(4)))
          (org-gtd-clarify-item))
        ;; Modify in WIP buffer
        (with-current-buffer (car (org-gtd-wip--get-buffers))
          (goto-char (point-min))
          (search-forward "Update me")
          (replace-match "Updated item")
          ;; Organize as single action
          (org-gtd-single-action))
        ;; Verify item was updated in place
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (expect (search-forward "Updated item" nil t) :to-be-truthy))))))
```

**Step 2: Run test to verify it fails**

Run: `eldev test -B "updates single-action in place"`
Expected: FAIL

**Step 3: Modify org-gtd-single-action--finalize**

Replace in `org-gtd-single-action.el`:

```elisp
(defun org-gtd-single-action--finalize ()
  "Finalize single action organization and refile."
  (setq-local org-gtd--organize-type 'single-action)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-action org-gtd-action-template)))
```

**Step 4: Run test to verify it passes**

Run: `eldev test -B "updates single-action in place"`
Expected: PASS

**Step 5: Modify remaining finalize functions**

Apply same pattern to each:

**org-gtd-calendar.el:**
```elisp
(defun org-gtd-calendar--finalize ()
  "Finalize calendar item organization and refile."
  (setq-local org-gtd--organize-type 'calendar)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-calendar org-gtd-calendar-template)))
```

**org-gtd-delegate.el:**
```elisp
(defun org-gtd-delegate--finalize ()
  "Finalize delegated item organization and refile."
  (setq-local org-gtd--organize-type 'delegated)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-action org-gtd-action-template)))
```

**org-gtd-habit.el:**
```elisp
(defun org-gtd-habit--finalize ()
  "Finalize habit organization and refile."
  (setq-local org-gtd--organize-type 'habit)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-habit org-gtd-habit-template)))
```

**org-gtd-incubate.el:**
```elisp
(defun org-gtd-incubate--finalize ()
  "Finalize incubated item organization and refile."
  (setq-local org-gtd--organize-type 'incubated)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-incubate org-gtd-incubate-template)))
```

**Step 6: Run all tests**

Run: `eldev test -B`
Expected: All pass

**Step 7: Commit**

```bash
git add org-gtd-single-action.el org-gtd-calendar.el org-gtd-delegate.el org-gtd-habit.el org-gtd-incubate.el
git commit -m "feat: all refile categories support skip-refile flag"
```

---

## Task 8: Handle Projects

**Files:**
- Modify: `org-gtd-projects.el` (two locations that call refile)

**Step 1: Find and modify project refile calls**

The project module has two refile paths:
- Line 390: `org-gtd-refile--do` for new projects
- Line 980: `org-gtd-refile--do-project-task` for extending projects

For new projects, modify the function that contains line 390 to check the flag:

```elisp
;; In the function containing line 390, wrap the refile call:
(if org-gtd-clarify--skip-refile
    (org-gtd-organize--update-in-place)
  (org-gtd-refile--do org-gtd-projects org-gtd-projects-template))
```

For project-extend (line 980), same pattern with `org-gtd-refile--do-project-task`:

```elisp
(if org-gtd-clarify--skip-refile
    (org-gtd-organize--update-in-place)
  (org-gtd-refile--do-project-task))
```

**Step 2: Run tests**

Run: `eldev test -B`
Expected: All pass

**Step 3: Commit**

```bash
git add org-gtd-projects.el
git commit -m "feat: projects support skip-refile flag"
```

---

## Task 9: Remove org-gtd-delegate-item-at-point

**Files:**
- Modify: `org-gtd-delegate.el` (remove function at lines 79-92)
- Modify: `test/delegating-test.el` (remove or update test)

**Step 1: Remove the function**

Delete lines 79-92 from `org-gtd-delegate.el` (the `org-gtd-delegate-item-at-point` function).

**Step 2: Update test**

In `test/delegating-test.el`, the test at lines 66-84 uses `org-gtd-delegate-item-at-point`. Replace with new approach using skip-refile:

```elisp
(it "allows delegation without refiling via skip-refile"
    (let ((topic "Custom delegate test")
          (checkin-date (format-time-string "%Y-%m-%d")))
      (create-single-action topic)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward topic)
        (org-back-to-heading t)
        ;; Clarify with skip-refile
        (let ((current-prefix-arg '(4)))
          (org-gtd-clarify-item))
        ;; Delegate via organize menu
        (with-current-buffer (car (org-gtd-wip--get-buffers))
          (with-simulated-input "Custom SPC Person RET RET"
            (org-gtd-delegate)))
        ;; Verify delegation was set up
        (goto-char (point-min))
        (search-forward topic)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
        (expect (org-entry-get (point) "ORG_GTD_DELEGATED_TO") :to-equal "Custom Person"))))
```

**Step 3: Run tests**

Run: `eldev test -B`
Expected: All pass

**Step 4: Commit**

```bash
git add org-gtd-delegate.el test/delegating-test.el
git commit -m "feat: remove org-gtd-delegate-item-at-point (use skip-refile instead)"
```

---

## Task 10: Update Documentation

**Files:**
- Modify: `doc/org-gtd.org`

**Step 1: Use writing-documentation skill**

Use `superpowers:writing-documentation` skill to update the manual with:
- Document C-u prefix behavior for clarify commands
- Document transient toggle (-n option)
- Explain use case: reconfiguring existing agenda items
- Add migration note for `org-gtd-delegate-item-at-point` removal

**Step 2: Regenerate info files**

Run:
```bash
cd doc && makeinfo --no-split -o org-gtd.info org-gtd.texi
```

**Step 3: Commit**

```bash
git add doc/org-gtd.org doc/org-gtd.texi doc/org-gtd.info
git commit -m "docs: document skip-refile feature and delegate-item-at-point removal"
```

---

## Summary

After completing all tasks:
- C-u prefix on clarify commands sets skip-refile flag
- Transient menu shows toggle (hidden during inbox processing)
- All refile categories check flag and update-in-place when set
- Archive categories (quick-action, knowledge, trash) unchanged
- `org-gtd-delegate-item-at-point` removed
- Documentation updated
