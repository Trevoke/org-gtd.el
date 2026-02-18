# Clarify Buffer Kill Hooks - Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Clean up side windows and prompt about pending duplicates when clarify buffers are killed directly (not just via C-c C-k).

**Architecture:** Register buffer-local `kill-buffer-query-functions` and `kill-buffer-hook` in `org-gtd-clarify-mode`. Query function handles duplicate prompts (can abort kill). Kill hook handles side window cleanup. Clear queue before kill in normal organize flow so hooks become pass-through.

**Tech Stack:** Emacs Lisp, org-gtd-clarify.el, org-gtd-organize-core.el, e-unit tests

**Design document:** `docs/plans/2026-01-20-clarify-buffer-kill-hooks.md`

---

## Exhaustive Test Scenarios

### Category A: Kill Buffer Query Function (Duplicate Handling)

| # | Scenario | Expected Behavior |
|---|----------|-------------------|
| A1 | Kill buffer with empty queue | Query returns t (allow kill), no prompt |
| A2 | Kill buffer with 1 duplicate, user chooses "discard" | Prompt shown, returns t, duplicate lost |
| A3 | Kill buffer with 1 duplicate, user chooses "save" | Saves to inbox, returns t |
| A4 | Kill buffer with 1 duplicate, user chooses "cancel" | Returns nil, kill aborted |
| A5 | Kill buffer with 3 duplicates, user chooses "discard" | Prompt shows all 3, returns t, all lost |
| A6 | Kill buffer with 3 duplicates, user chooses "save" | All 3 saved to inbox |

### Category B: Kill Buffer Hook (Side Window Cleanup)

| # | Scenario | Expected Behavior |
|---|----------|-------------------|
| B1 | Kill last clarify buffer, queue window open | Queue window killed |
| B2 | Kill last clarify buffer, horizons window open | Horizons window killed |
| B3 | Kill last clarify buffer, organize help window open | Organize help window killed |
| B4 | Kill last clarify buffer, dependencies window open | Dependencies window killed |
| B5 | Kill last clarify buffer, all 4 side windows open | All 4 windows killed |
| B6 | Kill last clarify buffer, no side windows open | No error, clean exit |
| B7 | Kill last clarify buffer, WIP temp file exists | Temp file deleted |
| B8 | Kill buffer, WIP temp file already deleted | No error |

### Category C: Multiple Clarify Buffers

| # | Scenario | Expected Behavior |
|---|----------|-------------------|
| C1 | Kill buffer A, buffer B exists, side windows open | Side windows remain open |
| C2 | Kill buffer A with duplicates, buffer B exists | Only buffer A's duplicates prompted |
| C3 | Kill buffer A (empty), then kill buffer B (last) | Side windows cleaned on B kill |
| C4 | Buffer A and B both have duplicates, kill A | Only A's duplicates prompted |

### Category D: Normal Organize Flow (Hooks Transparent)

| # | Scenario | Expected Behavior |
|---|----------|-------------------|
| D1 | Normal organize with empty queue | Hooks run, see empty queue, no prompt |
| D2 | Normal organize with duplicates in queue | Queue captured then cleared, hooks pass through |
| D3 | Quick action organize | Same as D1/D2 |
| D4 | Project organize | Same as D1/D2 |
| D5 | Full duplicate workflow (original + 2 duplicates) | All organized without extra prompts |

### Category E: Intentional Cancel (C-c C-k)

| # | Scenario | Expected Behavior |
|---|----------|-------------------|
| E1 | C-c C-k with empty queue | Normal cancel, no prompt from hooks |
| E2 | C-c C-k with duplicates | Existing prompt, then hooks pass through |
| E3 | C-c C-k after cancel at prompt | Kill aborted entirely |

### Category F: Edge Cases

| # | Scenario | Expected Behavior |
|---|----------|-------------------|
| F1 | Programmatic kill-buffer call | Same as direct kill |
| F2 | Kill buffer not displayed in window | Cleanup still works |
| F3 | Side window manually closed before buffer kill | No error on cleanup |
| F4 | Kill buffer during batch mode | Query skipped (no prompt possible) |

### Category G: Integration with Emacs Exit

| # | Scenario | Expected Behavior |
|---|----------|-------------------|
| G1 | Emacs exit with clarify buffer open | kill-emacs-query-functions handles it (existing) |
| G2 | kill-emacs does NOT trigger kill-buffer-hook | Confirmed behavior, no conflict |

---

## Implementation Tasks

### Task 1: Helper - Other Clarify Buffers Exist

**Files:**
- Modify: `org-gtd-clarify.el` (add function near line 700)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing test**

```elisp
(deftest clarify/other-buffers-exist-returns-nil-when-alone ()
  "Returns nil when current buffer is the only clarify buffer."
  (capture-inbox-item "Test item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (assert-nil (org-gtd-clarify--other-clarify-buffers-exist-p))))

(deftest clarify/other-buffers-exist-returns-t-when-multiple ()
  "Returns t when other clarify buffers exist."
  ;; Create first clarify buffer
  (capture-inbox-item "Item one")
  (org-gtd-process-inbox)
  (let ((first-buf (ogt-get-wip-buffer)))
    ;; Create second clarify buffer manually
    (with-temp-buffer
      (org-gtd-clarify-mode)
      ;; Check from first buffer's perspective
      (with-current-buffer first-buf
        (assert-true (org-gtd-clarify--other-clarify-buffers-exist-p))))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/other-buffers-exist`
Expected: FAIL with "void function org-gtd-clarify--other-clarify-buffers-exist-p"

**Step 3: Write minimal implementation**

Add to `org-gtd-clarify.el` after line ~700 (before Footer section):

```elisp
;;;;; Kill Buffer Safety

(defun org-gtd-clarify--other-clarify-buffers-exist-p ()
  "Return t if other clarify buffers exist besides current one."
  (let ((current (current-buffer)))
    (cl-some (lambda (buf)
               (and (not (eq buf current))
                    (buffer-live-p buf)
                    (with-current-buffer buf
                      (derived-mode-p 'org-gtd-clarify-mode))))
             (buffer-list))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/other-buffers-exist`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "feat(clarify): add helper to detect other clarify buffers"
```

---

### Task 2: Helper - Kill Side Window

**Files:**
- Modify: `org-gtd-clarify.el`
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing test**

```elisp
(deftest clarify/kill-side-window-removes-buffer ()
  "Kills buffer and closes window."
  (let ((test-buf (get-buffer-create "*Test Side Window*")))
    (display-buffer-in-side-window test-buf '((side . right)))
    (assert-true (get-buffer-window test-buf))
    (org-gtd-clarify--kill-side-window "*Test Side Window*")
    (assert-nil (get-buffer "*Test Side Window*"))))

(deftest clarify/kill-side-window-handles-nonexistent ()
  "Does not error when buffer doesn't exist."
  (assert-nil (get-buffer "*Nonexistent Buffer*"))
  ;; Should not error
  (org-gtd-clarify--kill-side-window "*Nonexistent Buffer*"))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/kill-side-window`
Expected: FAIL with "void function org-gtd-clarify--kill-side-window"

**Step 3: Write minimal implementation**

```elisp
(defun org-gtd-clarify--kill-side-window (buffer-name)
  "Kill side window buffer BUFFER-NAME if it exists."
  (when-let ((buffer (get-buffer buffer-name)))
    (when-let ((window (get-buffer-window buffer)))
      (quit-window nil window))
    (kill-buffer buffer)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/kill-side-window`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "feat(clarify): add helper to kill side window buffers"
```

---

### Task 3: Kill Buffer Query Function

**Files:**
- Modify: `org-gtd-clarify.el`
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing tests (A1-A4)**

```elisp
;;; Kill Buffer Query Tests

(deftest clarify/kill-buffer-query-allows-when-queue-empty ()
  "Query function returns t when queue is empty (test A1)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue nil)
    (assert-true (org-gtd-clarify--kill-buffer-query))))

(deftest clarify/kill-buffer-query-prompts-discard ()
  "Query function prompts and returns t on discard (test A2)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (with-simulated-input "d"
      (assert-true (org-gtd-clarify--kill-buffer-query)))
    ;; Queue unchanged (will be cleared by actual kill)
    (assert-equal 1 (length org-gtd-clarify--duplicate-queue))))

(deftest clarify/kill-buffer-query-prompts-save ()
  "Query function saves to inbox and returns t on save (test A3)."
  (ogt-eunit-with-mock-gtd
    (with-temp-buffer
      (org-gtd-clarify-mode)
      (setq org-gtd-clarify--duplicate-queue '((:title "Saved Item" :content "* Saved Item")))
      (with-simulated-input "s"
        (assert-true (org-gtd-clarify--kill-buffer-query)))
      ;; Check inbox has the saved item
      (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
        (revert-buffer t t)
        (assert-match "Saved Item" (buffer-string))))))

(deftest clarify/kill-buffer-query-prompts-cancel ()
  "Query function returns nil on cancel, aborting kill (test A4)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (with-simulated-input "c"
      (assert-nil (org-gtd-clarify--kill-buffer-query)))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/kill-buffer-query`
Expected: FAIL with "void function org-gtd-clarify--kill-buffer-query"

**Step 3: Write minimal implementation**

```elisp
(defun org-gtd-clarify--kill-buffer-query ()
  "Query before killing clarify buffer if duplicates are pending.
Returns t to allow kill, nil to abort.
Added to `kill-buffer-query-functions' buffer-locally."
  (if (org-gtd-clarify--queue-empty-p)
      t  ; No duplicates, allow kill
    ;; Prompt user - reuse existing prompt logic
    (pcase (org-gtd-clarify--prompt-queue-action)
      ('save (org-gtd-clarify--queue-save-to-inbox) t)
      ('discard t)
      ('cancel nil))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/kill-buffer-query`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "feat(clarify): add kill-buffer query function for duplicate prompts"
```

---

### Task 4: Kill Buffer Cleanup Function

**Files:**
- Modify: `org-gtd-clarify.el`
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing tests (B1, B6, C1)**

```elisp
;;; Kill Buffer Cleanup Tests

(deftest clarify/kill-buffer-cleanup-kills-queue-window ()
  "Cleanup kills queue window when last clarify buffer (test B1)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (org-gtd-clarify--queue-display)
    (assert-true (get-buffer "*Org GTD Duplicate Queue*"))
    ;; Simulate being the last buffer
    (org-gtd-clarify--kill-buffer-cleanup)
    (assert-nil (get-buffer "*Org GTD Duplicate Queue*"))))

(deftest clarify/kill-buffer-cleanup-no-error-when-empty ()
  "Cleanup handles case with no side windows (test B6)."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    ;; Should not error
    (org-gtd-clarify--kill-buffer-cleanup)))

(deftest clarify/kill-buffer-cleanup-preserves-windows-when-other-buffers ()
  "Cleanup preserves side windows when other clarify buffers exist (test C1)."
  (let ((other-buf (get-buffer-create "*Other Clarify*")))
    (with-current-buffer other-buf
      (org-gtd-clarify-mode))
    (unwind-protect
        (with-temp-buffer
          (org-gtd-clarify-mode)
          ;; Create queue window
          (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
          (org-gtd-clarify--queue-display)
          (assert-true (get-buffer "*Org GTD Duplicate Queue*"))
          ;; Cleanup - other buffer exists, so windows should remain
          (org-gtd-clarify--kill-buffer-cleanup)
          (assert-true (get-buffer "*Org GTD Duplicate Queue*")))
      ;; Cleanup test buffer
      (kill-buffer other-buf)
      (when-let ((buf (get-buffer "*Org GTD Duplicate Queue*")))
        (kill-buffer buf)))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/kill-buffer-cleanup`
Expected: FAIL with "void function org-gtd-clarify--kill-buffer-cleanup"

**Step 3: Write minimal implementation**

```elisp
(defun org-gtd-clarify--kill-buffer-cleanup ()
  "Clean up side windows when clarify buffer is killed.
Only cleans up global side windows if no other clarify buffers exist.
Added to `kill-buffer-hook' buffer-locally."
  (unless (org-gtd-clarify--other-clarify-buffers-exist-p)
    ;; Clean up all side windows
    (org-gtd-clarify--kill-side-window org-gtd-clarify--queue-buffer-name)
    (org-gtd-clarify--kill-side-window org-gtd-clarify-organize-help-buffer-name)
    (org-gtd-clarify--kill-side-window org-gtd-horizons--buffer-name)
    (org-gtd-clarify--kill-side-window "*Org GTD Dependencies*"))
  ;; Always clean up WIP temp file for this buffer
  (when org-gtd-clarify--clarify-id
    (org-gtd-wip--cleanup-temp-file org-gtd-clarify--clarify-id)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/kill-buffer-cleanup`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "feat(clarify): add kill-buffer cleanup for side windows"
```

---

### Task 5: Register Hooks in Clarify Mode

**Files:**
- Modify: `org-gtd-clarify.el` (the `define-derived-mode` block, ~line 206)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing test**

```elisp
;;; Hook Registration Tests

(deftest clarify/mode-registers-kill-buffer-hooks ()
  "Clarify mode registers buffer-local kill hooks."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (assert-true (memq 'org-gtd-clarify--kill-buffer-query
                       (buffer-local-value 'kill-buffer-query-functions (current-buffer))))
    (assert-true (memq 'org-gtd-clarify--kill-buffer-cleanup
                       (buffer-local-value 'kill-buffer-hook (current-buffer))))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/mode-registers-kill-buffer`
Expected: FAIL (hooks not yet registered)

**Step 3: Modify org-gtd-clarify-mode**

Add to `define-derived-mode org-gtd-clarify-mode` body (after `auto-save-mode 1`):

```elisp
  ;; Kill buffer hooks for cleanup
  (add-hook 'kill-buffer-query-functions
            #'org-gtd-clarify--kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook
            #'org-gtd-clarify--kill-buffer-cleanup nil t)
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/mode-registers-kill-buffer`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "feat(clarify): register kill-buffer hooks in clarify-mode"
```

---

### Task 6: Clear Queue Before Kill in Organize Flow

**Files:**
- Modify: `org-gtd-organize-core.el:122` (after copy-sequence, before cleanup)
- Test: `test/unit/organizing-test.el` or `test/unit/clarify-test.el`

**Step 1: Write the failing test (D2)**

```elisp
;;; Organize Flow Hook Transparency Tests

(deftest clarify/organize-flow-clears-queue-before-kill ()
  "Organize flow clears queue before kill so hooks don't prompt (test D2)."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  ;; Add a duplicate while clarifying
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact))
  ;; Verify we have one duplicate in queue
  (with-wip-buffer
    (assert-equal 1 (length org-gtd-clarify--duplicate-queue)))
  ;; Organize - hooks should NOT prompt because queue is cleared
  ;; If hooks prompt, this test will hang waiting for input
  (with-wip-buffer
    (organize-as-single-action))
  ;; Should now be clarifying the duplicate
  (assert-true (ogt-get-wip-buffer))
  ;; Organize the duplicate
  (with-wip-buffer
    (organize-as-single-action))
  ;; Done
  (assert-nil (ogt-get-wip-buffer)))
```

**Step 2: Run test to verify it fails (or hangs)**

Run: `~/bin/eldev etest -r dot -p clarify/organize-flow-clears`
Expected: Test may hang (hooks prompt) or pass if they don't prompt yet (hooks not registered)

**Step 3: Modify org-gtd-organize--call**

In `org-gtd-organize-core.el`, after line 122 (after `copy-sequence`), add:

```elisp
        ;; Clear queue so kill-buffer hooks don't prompt
        (setq org-gtd-clarify--duplicate-queue nil)
```

The let block should now look like:
```elisp
      (let ((continuation org-gtd-clarify--continuation)
            (task-id org-gtd-clarify--clarify-id)
            (window-config org-gtd-clarify--window-config)
            (skip-refile org-gtd-clarify--skip-refile)
            (duplicate-queue (copy-sequence org-gtd-clarify--duplicate-queue)))
        ;; Clear queue so kill-buffer hooks don't prompt
        (setq org-gtd-clarify--duplicate-queue nil)
        ;; Only cut original if we refiled ...
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/organize-flow-clears`
Expected: PASS (no hang, completes normally)

**Step 5: Commit**

```bash
git add org-gtd-organize-core.el test/unit/clarify-test.el
git commit -m "fix(organize): clear queue before kill for hook transparency"
```

---

### Task 7: Update org-gtd-clarify-stop for Hook Transparency

**Files:**
- Modify: `org-gtd-clarify.el:281` (`org-gtd-clarify-stop`)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing test (E2)**

```elisp
;;; Clarify Stop Hook Transparency Tests

(deftest clarify/stop-clears-queue-before-cleanup ()
  "Clarify-stop clears queue before cleanup so hooks don't double-prompt (test E2)."
  (capture-inbox-item "Test item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact))
  ;; Stop with "discard" - should only prompt once (from clarify-stop, not hooks)
  ;; If prompted twice, test will fail or hang
  (with-wip-buffer
    (with-simulated-input "d"
      (org-gtd-clarify-stop)))
  ;; Verify cleanup happened
  (assert-nil (get-buffer "*Org GTD Duplicate Queue*")))
```

**Step 2: Run test to verify behavior**

Run: `~/bin/eldev etest -r dot -p clarify/stop-clears-queue`
Expected: May prompt twice currently (once from clarify-stop, once from hooks)

**Step 3: Modify org-gtd-clarify-stop**

In `org-gtd-clarify-stop`, after handling duplicates (around line 294), clear the queue:

```elisp
  ;; Handle pending duplicates first
  (when (and (boundp 'org-gtd-clarify--duplicate-queue)
             (not (org-gtd-clarify--queue-empty-p)))
    (pcase (org-gtd-clarify--prompt-queue-action)
      ('save (org-gtd-clarify--queue-save-to-inbox))
      ('cancel (keyboard-quit))
      ('discard nil)))  ; Just continue with cleanup

  ;; Clear queue so kill-buffer hooks don't prompt again
  (setq org-gtd-clarify--duplicate-queue nil)
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/stop-clears-queue`
Expected: PASS (only one prompt)

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "fix(clarify): clear queue in stop for hook transparency"
```

---

### Task 8: Integration Tests

**Files:**
- Test: `test/unit/clarify-test.el`

**Step 1: Write integration tests**

```elisp
;;; Kill Buffer Integration Tests

(deftest clarify/direct-kill-cleans-up-side-windows ()
  "Direct buffer kill cleans up all side windows (test B5 integration)."
  (capture-inbox-item "Test item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    ;; Open side windows
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (org-gtd-clarify--queue-display)
    (let ((org-gtd-clarify-show-organize-help 'right))
      (org-gtd-clarify-toggle-organize-help)))
  ;; Kill buffer directly (empty queue for this test)
  (with-wip-buffer
    (setq org-gtd-clarify--duplicate-queue nil)
    (kill-buffer))
  ;; Verify side windows cleaned
  (assert-nil (get-buffer "*Org GTD Duplicate Queue*"))
  (assert-nil (get-buffer "*Org GTD Organize Help*")))

(deftest clarify/direct-kill-with-duplicates-saves-to-inbox ()
  "Direct kill with save option preserves duplicates (test A3 integration)."
  (ogt-eunit-with-mock-gtd
    (capture-inbox-item "Original")
    (org-gtd-process-inbox)
    (let ((wip-buf (ogt-get-wip-buffer)))
      (with-current-buffer wip-buf
        (org-gtd-clarify-duplicate-exact))
      ;; Kill buffer directly, choose save
      (with-current-buffer wip-buf
        (with-simulated-input "s"
          (kill-buffer))))
    ;; Verify saved to inbox
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (revert-buffer t t)
      (assert-match "Original" (buffer-string)))))

(deftest clarify/direct-kill-cancel-aborts ()
  "Direct kill with cancel option aborts kill (test A4 integration)."
  (capture-inbox-item "Test item")
  (org-gtd-process-inbox)
  (let ((wip-buf (ogt-get-wip-buffer)))
    (with-current-buffer wip-buf
      (org-gtd-clarify-duplicate-exact))
    ;; Try to kill buffer, choose cancel
    (with-current-buffer wip-buf
      (with-simulated-input "c"
        (kill-buffer)))
    ;; Buffer should still exist
    (assert-true (buffer-live-p wip-buf))
    ;; Cleanup
    (with-current-buffer wip-buf
      (setq org-gtd-clarify--duplicate-queue nil)
      (kill-buffer))))
```

**Step 2: Run tests**

Run: `~/bin/eldev etest -r dot -p clarify/direct-kill`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/clarify-test.el
git commit -m "test(clarify): add integration tests for direct buffer kill"
```

---

### Task 9: Run Full Test Suite

**Step 1: Run all tests**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass (1120+ tests)

**Step 2: Commit if any cleanup needed**

```bash
git add -A
git commit -m "chore: test suite cleanup"
```

---

### Task 10: Final Review and Documentation

**Step 1: Run linting**

Run: `~/bin/eldev lint --file="org-gtd-clarify.el"`
Expected: No errors

**Step 2: Update documentation (if needed)**

The user documentation in `doc/org-gtd.org` should already cover the cancel behavior. Verify:
- The duplicate queue section mentions that killing the buffer prompts about pending duplicates
- No additional documentation changes needed for this internal improvement

**Step 3: Final commit**

```bash
git add -A
git commit -m "docs: update clarify kill-buffer behavior notes"
```

---

## Testing Checklist (from design doc)

After implementation, verify all scenarios manually:

- [ ] Direct kill with empty queue cleans up side windows
- [ ] Direct kill with duplicates prompts user
- [ ] Discard option allows kill, loses duplicates
- [ ] Save option saves to inbox, allows kill
- [ ] Cancel option aborts kill
- [ ] Multiple clarify buffers: killing one doesn't close shared side windows
- [ ] Normal organize flow: no prompts, duplicates processed correctly
- [ ] `C-c C-k` still works as before
- [ ] Emacs exit still prompts about pending duplicates
