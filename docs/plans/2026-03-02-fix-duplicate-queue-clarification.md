# Fix Duplicate Queue Clarification Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix 6 bugs in the duplicate item clarification workflow by reusing the WIP buffer when cycling through queue items, instead of creating a new buffer per item.

**Architecture:** Rather than tear down and rebuild the entire clarification UI for each queued duplicate, we reuse the existing WIP buffer: erase, insert new content, regenerate the ID, rename the buffer. This preserves the window layout (side windows for queue display, horizons, etc.) naturally, because we never call `org-gtd-clarify-setup-windows` again. The original `window-config` snapshot is still restored once the queue is fully drained.

**Tech Stack:** Emacs Lisp, org-mode, e-unit test framework, `with-simulated-input`

**Context:** This addresses the issues raised in [PR #281](https://github.com/Trevoke/org-gtd.el/pull/281) but takes a different approach. PR #281 routes duplicates through `inbox.org`; this plan keeps them in WIP buffers but fixes the root causes directly.

**Bugs fixed:**
1. Heading retains original title instead of new title
2. Queue window destroyed by `delete-other-windows` during queue cycling
3. Duplicate items share stale ID from original
4. Buffer name is UUID instead of human-readable slug
5. Cancel discards entire queue instead of just current item
6. WIP prefix `"Org-GTD Clarify"` inconsistent with `"Org GTD ..."`

---

### Task 1: Fix WIP prefix inconsistency (bug 6)

**Files:**
- Modify: `org-gtd-wip.el:38`
- Test: `test/unit/clarify-test.el` (existing tests use the prefix implicitly via `ogt-get-wip-buffer`)

This is the simplest change and a good warmup. It also must happen first because later tasks add tests that assert buffer names with the new prefix.

**Step 1: Update the prefix constant**

In `org-gtd-wip.el`, change line 38:

```elisp
;; Before:
(defconst org-gtd-wip--prefix "Org-GTD Clarify")

;; After:
(defconst org-gtd-wip--prefix "Org GTD Clarify")
```

**Step 2: Run all tests to verify nothing breaks**

Run: `@test`
Expected: All pass. The test helper `ogt-get-wip-buffer` uses `string-search org-gtd-wip--prefix` so it automatically picks up the new value.

**Step 3: Commit**

```bash
git add org-gtd-wip.el
git commit -m "fix: make WIP buffer prefix consistent with other Org GTD buffers"
```

---

### Task 2: Fix heading title in queued content (bug 1)

**Files:**
- Modify: `org-gtd-clarify.el:319-333` (`org-gtd-clarify-duplicate`)
- Test: `test/unit/clarify-test.el`

When `org-gtd-clarify-duplicate` stores content in the queue, it saves the `:title` separately but leaves the old heading in `:content`. The content's heading must match the new title.

**Step 1: Write failing test**

Add a test after the existing `clarify/duplicate-with-rename-uses-new-title` test (around line 334). This test verifies that the `:content` in the queue has the updated heading, not just the `:title` key:

```elisp
(deftest clarify/duplicate-with-rename-updates-content-heading ()
  "Duplicate with rename updates the heading inside :content."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (with-simulated-input "C-a C-k New SPC title RET"
      (org-gtd-clarify-duplicate))
    (let* ((queued (car org-gtd-clarify--duplicate-queue))
           (content (plist-get queued :content)))
      ;; The org heading inside content should be "New title", not "Original item"
      (assert-match "^\\* New title" content)
      (assert-no-match "^\\* Original item" content)))
  (org-gtd-clarify--queue-cleanup))
```

**Step 2: Run test to verify it fails**

Run: `@test`
Expected: FAIL — content still has `* Original item` heading.

**Step 3: Fix `org-gtd-clarify-duplicate` to update content heading**

In `org-gtd-clarify.el`, modify `org-gtd-clarify-duplicate` (lines 328-331):

```elisp
;; Before:
    (let* ((default-title (plist-get content-plist :title))
           (new-title (read-string "Duplicate title: " default-title))
           (content (plist-get content-plist :content)))
      (org-gtd-clarify--queue-add new-title content)

;; After:
    (let* ((default-title (plist-get content-plist :title))
           (new-title (read-string "Duplicate title: " default-title))
           (content (plist-get content-plist :content))
           (updated-content
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (org-mode)
              (org-edit-headline new-title)
              (buffer-string))))
      (org-gtd-clarify--queue-add new-title updated-content)
```

**Step 4: Run tests to verify they pass**

Run: `@test`
Expected: All pass, including the new test.

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "fix: update heading in content when duplicating with new title"
```

---

### Task 3: Add `org-gtd-wip--rekey` helper function (prep for bug 2, 3, 4)

**Files:**
- Modify: `org-gtd-wip.el` (add new function after `org-gtd-wip--cleanup-temp-file`)
- Test: `test/unit/wip-test.el` (or the appropriate wip test file)

The buffer-reuse approach needs a way to update the hash table key when the clarify-id changes. Add a small helper to `org-gtd-wip.el`.

**Step 1: Write failing test**

Find the WIP test file and add:

```elisp
(deftest wip/rekey-updates-hash-and-buffer-name ()
  "Rekeying a WIP buffer updates hash tracking and buffer name."
  (let* ((old-id "old-test-id")
         (new-id "new-test-id")
         (buf (org-gtd-wip--get-buffer old-id)))
    ;; Verify initial state
    (assert-true (gethash old-id org-gtd-wip--temp-files))
    (assert-match "old-test-id" (buffer-name buf))
    ;; Rekey
    (org-gtd-wip--rekey old-id new-id)
    ;; Old key gone, new key present, same file
    (assert-nil (gethash old-id org-gtd-wip--temp-files))
    (assert-true (gethash new-id org-gtd-wip--temp-files))
    (assert-match "new-test-id" (buffer-name buf))
    ;; Cleanup
    (org-gtd-wip--cleanup-temp-file new-id)))
```

**Step 2: Run test to verify it fails**

Run: `@test`
Expected: FAIL — `org-gtd-wip--rekey` is void.

**Step 3: Implement `org-gtd-wip--rekey`**

In `org-gtd-wip.el`, add after `org-gtd-wip--cleanup-temp-file` (after line 125):

```elisp
(defun org-gtd-wip--rekey (old-id new-id)
  "Update the tracking for a WIP buffer from OLD-ID to NEW-ID.
Moves the temp file hash entry and renames the buffer."
  (let ((temp-file (gethash old-id org-gtd-wip--temp-files)))
    (when temp-file
      (remhash old-id org-gtd-wip--temp-files)
      (puthash new-id temp-file org-gtd-wip--temp-files)
      (when-let ((buffer (find-buffer-visiting temp-file)))
        (with-current-buffer buffer
          (rename-buffer (org-gtd-wip--buffer-name new-id) t))))))
```

**Step 4: Run tests to verify they pass**

Run: `@test`
Expected: All pass.

**Step 5: Commit**

```bash
git add org-gtd-wip.el test/unit/wip-test.el
git commit -m "feat: add org-gtd-wip--rekey for updating WIP buffer identity"
```

---

### Task 4: Rewrite queue processing to reuse WIP buffer (bugs 2, 3, 4)

**Files:**
- Modify: `org-gtd-clarify.el:625-657` (`org-gtd-clarify--process-next-queued-item`)
- Modify: `org-gtd-organize-core.el:115-147` (`org-gtd-organize--call`)
- Test: `test/unit/clarify-test.el`

This is the core change. Instead of killing the WIP buffer and creating a new one for each queue item, we reuse the existing buffer.

**Step 1: Write failing test — queued duplicate gets a fresh ID**

```elisp
(deftest clarify/queued-duplicate-gets-fresh-id ()
  "Each queued duplicate gets its own unique org-gtd-style ID, not a stale copy."
  (capture-inbox-item "Buy groceries")
  (org-gtd-process-inbox)
  ;; Capture the original's ID
  (let (original-id)
    (with-wip-buffer
      (goto-char (point-min))
      (setq original-id (org-entry-get nil "ID"))
      (org-gtd-clarify-duplicate-exact))
    ;; Organize the original
    (with-wip-buffer
      (organize-as-single-action))
    ;; Now clarifying the duplicate — check its ID
    (with-wip-buffer
      (goto-char (point-min))
      (let ((dup-id (org-entry-get nil "ID")))
        ;; Must have an ID
        (assert-true dup-id)
        ;; Must differ from original
        (assert-not-equal original-id dup-id)
        ;; Should be human-readable (contains heading slug), not a UUID
        (assert-match "buy-groceries" (downcase dup-id))))
    ;; Cleanup
    (with-wip-buffer
      (organize-as-single-action))))
```

**Step 2: Write failing test — buffer name is human-readable after queue cycling**

```elisp
(deftest clarify/queued-duplicate-has-readable-buffer-name ()
  "WIP buffer has a human-readable name when processing queue items."
  (capture-inbox-item "Plan vacation")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact))
  (with-wip-buffer
    (organize-as-single-action))
  ;; Now processing the duplicate
  (let ((buf (ogt-get-wip-buffer)))
    (assert-true buf)
    ;; Buffer name should contain a slug, not a UUID
    (assert-match "plan-vacation" (downcase (buffer-name buf))))
  ;; Cleanup
  (with-wip-buffer
    (organize-as-single-action)))
```

**Step 3: Run tests to verify they fail**

Run: `@test`
Expected: FAIL — IDs are UUIDs and buffer names contain UUIDs.

**Step 4: Modify `org-gtd-organize--call` to skip buffer cleanup when queue exists**

In `org-gtd-organize-core.el`, the key change is: when there's a `duplicate-queue`, don't kill the WIP buffer. Pass it to the queue processor instead.

Replace lines 137-147:

```elisp
;; Before:
        (when task-id
          (org-gtd-wip--cleanup-temp-file task-id))
        ;; Check if we have queued duplicates to process
        (if duplicate-queue
            ;; Process queued duplicates before calling continuation
            (org-gtd-clarify--process-next-queued-item
             duplicate-queue window-config continuation)
          ;; No queue - proceed with normal flow
          (when window-config
            (set-window-configuration window-config))
          (when continuation (funcall continuation)))

;; After:
        ;; Check if we have queued duplicates to process
        (if duplicate-queue
            ;; Reuse current buffer for next queued item
            (org-gtd-clarify--process-next-queued-item
             duplicate-queue window-config continuation task-id)
          ;; No queue - clean up and proceed with normal flow
          (when task-id
            (org-gtd-wip--cleanup-temp-file task-id))
          (when window-config
            (set-window-configuration window-config))
          (when continuation (funcall continuation)))
```

Note: `task-id` cleanup moves into the else branch. The WIP buffer stays alive when there's a queue. The `task-id` is passed as a new 4th argument.

**Step 5: Rewrite `org-gtd-clarify--process-next-queued-item` to reuse buffer**

Replace the function at `org-gtd-clarify.el:625-657`:

```elisp
(defun org-gtd-clarify--process-next-queued-item (queue window-config continuation old-task-id)
  "Process the next item from the duplicate QUEUE.
WINDOW-CONFIG is restored after all items are processed.
CONTINUATION is called after the queue is empty.
OLD-TASK-ID is the clarify-id of the buffer being reused."
  (let ((item (pop queue)))
    (if item
        (let* ((content (plist-get item :content))
               (processing-buffer (gethash old-task-id org-gtd-wip--temp-files))
               (processing-buffer (if processing-buffer
                                      (find-buffer-visiting processing-buffer)
                                    (org-gtd-wip--get-buffer (org-id-new)))))
          (with-current-buffer processing-buffer
            ;; Clear buffer and insert new content
            (let ((inhibit-read-only t))
              (erase-buffer))
            (insert content)
            (goto-char (point-min))
            ;; Delete stale ID from original, generate fresh one
            (org-entry-delete nil "ID")
            (let ((new-id (org-gtd-id-get-create)))
              (org-gtd-wip--rekey old-task-id new-id)
              ;; Ensure mode is active
              (unless (derived-mode-p 'org-gtd-clarify-mode)
                (org-gtd-clarify-mode))
              ;; Update buffer-local state
              (setq-local org-gtd-clarify--window-config window-config
                          org-gtd-clarify--clarify-id new-id
                          org-gtd-clarify--continuation continuation
                          org-gtd-clarify--source-heading-marker nil
                          org-gtd-clarify--duplicate-queue queue))
            ;; Update queue display or cleanup if empty
            (if (org-gtd-clarify--queue-empty-p)
                (org-gtd-clarify--queue-cleanup)
              (org-gtd-clarify--queue-display))))
      ;; No more items - cleanup and continue
      (when old-task-id
        (org-gtd-wip--cleanup-temp-file old-task-id))
      (org-gtd-clarify--queue-cleanup)
      (message "All duplicates processed")
      (when window-config
        (set-window-configuration window-config))
      (when continuation
        (funcall continuation)))))
```

Key differences from the old version:
- Accepts `old-task-id` to find and reuse the existing buffer
- Erases and repopulates instead of creating new buffer
- Deletes stale ID, generates fresh one with `org-gtd-id-get-create`
- Calls `org-gtd-wip--rekey` to update hash tracking and buffer name
- Does NOT call `org-gtd-clarify-setup-windows` — window layout stays intact
- Cleans up the temp file only when the queue is fully drained

**Step 6: Run tests to verify they pass**

Run: `@test`
Expected: All pass, including new tests and the existing `clarify/organize-processes-queue-before-continuation` and `clarify/duplicate-full-workflow` integration tests.

**Step 7: Commit**

```bash
git add org-gtd-clarify.el org-gtd-organize-core.el test/unit/clarify-test.el
git commit -m "fix: reuse WIP buffer when cycling queue items

Preserves window layout (queue, horizons, helpers) by reusing the
existing buffer instead of creating a new one per item. Each duplicate
gets a fresh human-readable ID via org-gtd-id-get-create."
```

---

### Task 5: Fix cancel to only discard current item (bug 5)

**Files:**
- Modify: `org-gtd-clarify.el:286-317` (`org-gtd-clarify-stop`)
- Test: `test/unit/clarify-test.el`

Currently, canceling with a queue prompts to discard/save ALL pending duplicates. The correct behavior: discard just the current item and move to the next queued item.

**Step 1: Write failing test**

```elisp
(deftest clarify/stop-continues-queue-with-next-item ()
  "Stopping discards only the current item and processes next in queue."
  (capture-inbox-item "Multi task")
  (org-gtd-process-inbox)
  ;; Create two duplicates
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact)
    (org-gtd-clarify-duplicate-exact))
  ;; Organize original
  (with-wip-buffer
    (organize-as-single-action))
  ;; Now clarifying first duplicate — cancel it
  (with-wip-buffer
    (org-gtd-clarify-stop))
  ;; Should now be clarifying the second duplicate, not done
  (assert-true (ogt-get-wip-buffer))
  ;; Organize the second one
  (with-wip-buffer
    (organize-as-single-action))
  ;; Now done
  (assert-nil (ogt-get-wip-buffer)))
```

**Step 2: Run test to verify it fails**

Run: `@test`
Expected: FAIL — `org-gtd-clarify-stop` tears down everything.

**Step 3: Rewrite `org-gtd-clarify-stop` to handle queue continuation**

Replace `org-gtd-clarify-stop` at lines 286-317:

```elisp
(defun org-gtd-clarify-stop ()
  "Stop clarifying the current item and restore previous state.
If there are pending duplicates in the queue, discards only the current
item and moves to the next queued item. Otherwise restores the window
configuration and cleans up."
  (interactive)
  (let ((queue (copy-sequence org-gtd-clarify--duplicate-queue))
        (window-config org-gtd-clarify--window-config)
        (task-id org-gtd-clarify--clarify-id)
        (continuation org-gtd-clarify--continuation)
        (inbox-p org-gtd-clarify--inbox-p))
    ;; Clear queue on current buffer so kill hooks don't prompt
    (setq org-gtd-clarify--duplicate-queue nil)
    (if queue
        ;; Queue has items — discard current, process next
        (org-gtd-clarify--process-next-queued-item
         queue window-config continuation task-id)
      ;; No queue — full cleanup
      (org-gtd-clarify--queue-cleanup)
      (org-gtd-clarify--cleanup-horizons-view)
      (when task-id
        (org-gtd-wip--cleanup-temp-file task-id))
      (when inbox-p
        (setq org-gtd-process--session-active nil
              org-gtd-process--pending-inboxes nil))
      (when window-config
        (set-window-configuration window-config))
      (message "Stopped clarifying"))))
```

Key changes:
- When there's a queue: reuses the buffer for the next item via `org-gtd-clarify--process-next-queued-item` (same path as organize)
- When no queue: does full cleanup as before
- No more 3-way prompt (discard/save/cancel) — cancel always means "skip this one"

**Step 4: Update existing cancel tests**

The existing tests `clarify/stop-with-queue-prompts-discard` and `clarify/stop-save-to-inbox-preserves-duplicates` test the old 3-way prompt behavior. Update them to match the new "skip current, continue queue" behavior:

Replace `clarify/stop-with-queue-prompts-discard` (lines 368-380):

```elisp
(deftest clarify/stop-with-queue-skips-to-next ()
  "Stopping with pending duplicates skips to next item in queue."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-clarify-duplicate-exact)
    (org-gtd-clarify-duplicate-exact))
  ;; Organize the original
  (with-wip-buffer
    (organize-as-single-action))
  ;; Now on first duplicate — stop/skip it
  (with-wip-buffer
    (org-gtd-clarify-stop))
  ;; Should still have a WIP buffer (second duplicate)
  (assert-true (ogt-get-wip-buffer))
  ;; Organize the remaining one
  (with-wip-buffer
    (organize-as-single-action))
  (assert-nil (ogt-get-wip-buffer)))
```

Remove or replace `clarify/stop-save-to-inbox-preserves-duplicates` since the save-to-inbox prompt no longer exists during stop. The queue save-to-inbox behavior still exists via the kill-buffer query function, but the stop command now simply skips.

```elisp
(deftest clarify/stop-without-queue-restores-window-config ()
  "Stopping without a queue restores the original window configuration."
  (capture-inbox-item "Original item")
  (org-gtd-process-inbox)
  ;; Stop without adding any duplicates
  (with-wip-buffer
    (org-gtd-clarify-stop))
  ;; No WIP buffer should remain
  (assert-nil (ogt-get-wip-buffer)))
```

**Step 5: Run tests to verify they pass**

Run: `@test`
Expected: All pass.

**Step 6: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "fix: cancel only discards current item, continues queue

Instead of prompting to discard/save the entire queue, stopping now
skips only the current item and moves to the next queued duplicate.
Uses the same buffer-reuse path as the organize flow."
```

---

### Task 6: Verify full integration and clean up

**Files:**
- Test: `test/unit/clarify-test.el`

**Step 1: Review and run the existing integration test**

The existing `clarify/duplicate-full-workflow` test (line 547) exercises the full create-process-verify cycle. It should already pass from the changes above. Run it to confirm.

Run: `@test`
Expected: All pass.

**Step 2: Add integration test for rename + queue + organize cycle**

This tests the complete flow including the title rename fix (bug 1) through to organization:

```elisp
(deftest clarify/duplicate-rename-full-workflow ()
  "Renamed duplicates have correct titles throughout the workflow."
  (capture-inbox-item "Generic task")
  (org-gtd-process-inbox)
  ;; Create a renamed duplicate
  (with-wip-buffer
    (with-simulated-input "C-a C-k Specific SPC task RET"
      (org-gtd-clarify-duplicate)))
  ;; Organize original
  (with-wip-buffer
    (organize-as-single-action))
  ;; Now processing the renamed duplicate
  (with-wip-buffer
    ;; Heading should be the renamed title
    (goto-char (point-min))
    (assert-equal "Specific task" (nth 4 (org-heading-components))))
  ;; Organize the duplicate
  (with-wip-buffer
    (organize-as-single-action))
  ;; Verify both exist in GTD system
  (with-current-buffer (org-gtd--default-file)
    (revert-buffer t t)
    (let ((content (buffer-string)))
      (assert-match "Generic task" content)
      (assert-match "Specific task" content))))
```

**Step 3: Run all tests one final time**

Run: `@test`
Expected: All pass.

**Step 4: Commit**

```bash
git add test/unit/clarify-test.el
git commit -m "test: add integration test for renamed duplicate workflow"
```

---

## Implementation Notes

**What we're NOT changing:**
- The `window-configuration` snapshot/restore pattern stays as-is (broader refactor for another day)
- `org-gtd-clarify-setup-windows` stays unchanged (only called once at initial clarify entry)
- Queue display still uses `display-buffer-in-side-window` (already correct for side windows)
- `org-gtd-clarify--queue-save-to-inbox` stays for the kill-buffer query path
- `org-gtd-clarify--prompt-queue-action` stays for the kill-buffer/kill-emacs query path

**Edge cases to watch:**
- The `org-gtd-clarify--kill-buffer-cleanup` hook (line ~779) directly accesses `org-gtd-wip--temp-files`. Verify it still works since we rekey instead of creating new entries.
- The `org-gtd-clarify-switch-to-buffer` command lists WIP buffers by prefix. Verify buffer rename doesn't break this (it shouldn't — `rename-buffer` updates `buffer-name`).
- `org-gtd-clarify-duplicate-exact` doesn't need changes — it keeps the same title, so content heading matches.
