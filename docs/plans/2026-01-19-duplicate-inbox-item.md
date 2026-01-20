# Duplicate Inbox Item Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Allow users to duplicate items during clarification, creating a queue that must be fully processed before returning to the previous context.

**Architecture:** Add buffer-local queue to clarify buffers, display in side window, modify organize completion to check queue before calling continuation. All state lives in the clarify buffer; queue window is a read-only display.

**Tech Stack:** Emacs Lisp, e-unit test framework, transient.el (existing)

---

## Task 1: Add Customization Variable for Queue Window Position

**Files:**
- Modify: `org-gtd-clarify.el:38-76` (customization section)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing test**

Add to `test/unit/clarify-test.el`:

```elisp
;;; Duplicate Queue Customization Tests

(deftest clarify/duplicate-queue-position-customizable ()
  "Has a customizable variable for queue window position."
  (assert-true (boundp 'org-gtd-clarify-duplicate-queue-position))
  (assert-equal 'bottom (default-value 'org-gtd-clarify-duplicate-queue-position)))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/duplicate`
Expected: FAIL with "Symbol's value as variable is void"

**Step 3: Write minimal implementation**

Add to `org-gtd-clarify.el` after line 76 (after `org-gtd-clarify-show-organize-help`):

```elisp
(defcustom org-gtd-clarify-duplicate-queue-position 'bottom
  "Position for the duplicate queue window during clarification.
When duplicates are created, they appear in a side window at this position.
Values can be: top, right, left, bottom."
  :group 'org-gtd-clarify
  :package-version '(org-gtd . "4.4.0")
  :type '(choice (const :tag "Top" top)
                 (const :tag "Right" right)
                 (const :tag "Left" left)
                 (const :tag "Bottom" bottom)))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/duplicate`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): add customization for duplicate queue window position

Add `org-gtd-clarify-duplicate-queue-position` defcustom to control
where the queue window appears. Defaults to bottom.

Ref: #275
EOF
)"
```

---

## Task 2: Add Buffer-Local Queue Variable

**Files:**
- Modify: `org-gtd-clarify.el:130-156` (variables section)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing test**

```elisp
(deftest clarify/duplicate-queue-variable-exists ()
  "Has a buffer-local variable for the duplicate queue."
  (assert-true (boundp 'org-gtd-clarify--duplicate-queue))
  ;; Verify it's buffer-local by default
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue '("test"))
    (assert-equal '("test") org-gtd-clarify--duplicate-queue))
  ;; Different buffer should have nil
  (with-temp-buffer
    (assert-nil org-gtd-clarify--duplicate-queue)))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/duplicate-queue-variable`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-clarify.el` after line 151 (after `org-gtd-clarify--continuation`):

```elisp
(defvar-local org-gtd-clarify--duplicate-queue nil
  "List of pending duplicate items to clarify after current item.
Each element is a plist with :title and :content keys.")
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/duplicate-queue-variable`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): add buffer-local queue for duplicates

Add `org-gtd-clarify--duplicate-queue` to hold pending duplicates
during clarification.

Ref: #275
EOF
)"
```

---

## Task 3: Add Queue Helper Functions (empty-p, add, pop)

**Files:**
- Modify: `org-gtd-clarify.el` (add new section before Footer)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing tests**

```elisp
;;; Duplicate Queue Helper Tests

(deftest clarify/queue-empty-p-returns-true-when-empty ()
  "Returns t when queue is empty or nil."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue nil)
    (assert-true (org-gtd-clarify--queue-empty-p))))

(deftest clarify/queue-empty-p-returns-nil-when-has-items ()
  "Returns nil when queue has items."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (assert-nil (org-gtd-clarify--queue-empty-p))))

(deftest clarify/queue-add-appends-to-queue ()
  "Adds item to end of queue."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue nil)
    (org-gtd-clarify--queue-add "First" "* First")
    (org-gtd-clarify--queue-add "Second" "* Second")
    (assert-equal 2 (length org-gtd-clarify--duplicate-queue))
    (assert-equal "First" (plist-get (car org-gtd-clarify--duplicate-queue) :title))))

(deftest clarify/queue-pop-returns-and-removes-first-item ()
  "Pops first item from queue (FIFO)."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue
          '((:title "First" :content "* First")
            (:title "Second" :content "* Second")))
    (let ((item (org-gtd-clarify--queue-pop)))
      (assert-equal "First" (plist-get item :title))
      (assert-equal 1 (length org-gtd-clarify--duplicate-queue))
      (assert-equal "Second" (plist-get (car org-gtd-clarify--duplicate-queue) :title)))))

(deftest clarify/queue-pop-returns-nil-when-empty ()
  "Returns nil when popping from empty queue."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue nil)
    (assert-nil (org-gtd-clarify--queue-pop))))
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev etest -r dot -p clarify/queue`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-clarify.el` before the Footer section:

```elisp
;;;; Duplicate Queue Functions

;;;;; Queue Predicates

(defun org-gtd-clarify--queue-empty-p ()
  "Return t if the duplicate queue is empty."
  (null org-gtd-clarify--duplicate-queue))

;;;;; Queue Operations

(defun org-gtd-clarify--queue-add (title content)
  "Add item with TITLE and CONTENT to end of duplicate queue."
  (setq org-gtd-clarify--duplicate-queue
        (append org-gtd-clarify--duplicate-queue
                (list (list :title title :content content)))))

(defun org-gtd-clarify--queue-pop ()
  "Remove and return first item from duplicate queue.
Returns nil if queue is empty."
  (when org-gtd-clarify--duplicate-queue
    (pop org-gtd-clarify--duplicate-queue)))
```

**Step 4: Run tests to verify they pass**

Run: `~/bin/eldev etest -r dot -p clarify/queue`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): add queue helper functions

Add org-gtd-clarify--queue-empty-p, org-gtd-clarify--queue-add,
and org-gtd-clarify--queue-pop for managing the duplicate queue.

Ref: #275
EOF
)"
```

---

## Task 4: Add Queue Display Functions

**Files:**
- Modify: `org-gtd-clarify.el`
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing tests**

```elisp
;;; Queue Display Tests

(defconst org-gtd-clarify--queue-buffer-name "*Org GTD Duplicate Queue*"
  "Buffer name for test assertions.")

(deftest clarify/queue-display-creates-buffer ()
  "Creates queue buffer with correct content."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue
          '((:title "Task A" :content "* Task A")
            (:title "Task B" :content "* Task B")))
    (org-gtd-clarify--queue-display)
    (let ((queue-buf (get-buffer "*Org GTD Duplicate Queue*")))
      (assert-true queue-buf)
      (with-current-buffer queue-buf
        (assert-match "Pending (2)" (buffer-string))
        (assert-match "1\\. Task A" (buffer-string))
        (assert-match "2\\. Task B" (buffer-string)))
      (kill-buffer queue-buf))))

(deftest clarify/queue-cleanup-kills-buffer ()
  "Cleanup kills the queue buffer."
  (with-temp-buffer
    (setq org-gtd-clarify--duplicate-queue '((:title "Test" :content "* Test")))
    (org-gtd-clarify--queue-display)
    (assert-true (get-buffer "*Org GTD Duplicate Queue*"))
    (org-gtd-clarify--queue-cleanup)
    (assert-nil (get-buffer "*Org GTD Duplicate Queue*"))))
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev etest -r dot -p clarify/queue-display`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-clarify.el` in the Duplicate Queue Functions section:

```elisp
;;;;; Queue Display

(defconst org-gtd-clarify--queue-buffer-name "*Org GTD Duplicate Queue*"
  "Buffer name for the duplicate queue window.")

(defun org-gtd-clarify--queue-display ()
  "Display the duplicate queue in a side window.
Creates or updates the queue buffer with current queue contents."
  (let ((buffer (get-buffer-create org-gtd-clarify--queue-buffer-name))
        (queue org-gtd-clarify--duplicate-queue))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Pending (%d):\n" (length queue)))
        (let ((idx 1))
          (dolist (item queue)
            (insert (format "  %d. %s\n" idx (plist-get item :title)))
            (setq idx (1+ idx))))
        (goto-char (point-min)))
      (setq buffer-read-only t))
    (display-buffer buffer
                    `(display-buffer-in-side-window
                      . ((side . ,org-gtd-clarify-duplicate-queue-position))))))

(defun org-gtd-clarify--queue-cleanup ()
  "Close the queue window and kill the queue buffer."
  (when-let ((buffer (get-buffer org-gtd-clarify--queue-buffer-name)))
    (when-let ((window (get-buffer-window buffer)))
      (quit-window nil window))
    (kill-buffer buffer))
  (setq org-gtd-clarify--duplicate-queue nil))
```

**Step 4: Run tests to verify they pass**

Run: `~/bin/eldev etest -r dot -p clarify/queue-display`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): add queue display functions

Add org-gtd-clarify--queue-display to show pending duplicates
in a side window, and org-gtd-clarify--queue-cleanup to close it.

Ref: #275
EOF
)"
```

---

## Task 5: Add Content Extraction Helper

**Files:**
- Modify: `org-gtd-clarify.el`
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing test**

```elisp
;;; Content Extraction Tests

(deftest clarify/get-wip-content-extracts-heading ()
  "Extracts title and content from WIP buffer."
  (ogt-eunit-with-mock-gtd
    (capture-inbox-item "Test heading")
    (org-gtd-process-inbox)
    (with-wip-buffer
      (let ((result (org-gtd-clarify--get-wip-content)))
        (assert-match "Test heading" (plist-get result :title))
        (assert-match "\\* .*Test heading" (plist-get result :content))))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/get-wip-content`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-clarify.el`:

```elisp
;;;;; Content Extraction

(defun org-gtd-clarify--get-wip-content ()
  "Extract title and full content from current WIP buffer.
Returns plist with :title and :content keys, or nil if buffer is empty."
  (save-excursion
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (when (org-at-heading-p)
      (let ((title (org-get-heading t t t t))
            (content (buffer-substring-no-properties
                      (point-min) (point-max))))
        (when (and title (not (string-empty-p (string-trim title))))
          (list :title title :content content))))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/get-wip-content`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): add content extraction helper

Add org-gtd-clarify--get-wip-content to extract title and content
from WIP buffer for duplicate creation.

Ref: #275
EOF
)"
```

---

## Task 6: Add Duplicate Commands

**Files:**
- Modify: `org-gtd-clarify.el`
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing tests**

```elisp
;;; Duplicate Command Tests

(deftest clarify/duplicate-exact-adds-to-queue ()
  "Exact duplicate adds current content to queue."
  (ogt-eunit-with-mock-gtd
    (capture-inbox-item "Original item")
    (org-gtd-process-inbox)
    (with-wip-buffer
      (org-gtd-clarify-duplicate-exact)
      (assert-equal 1 (length org-gtd-clarify--duplicate-queue))
      (assert-equal "Original item"
                    (plist-get (car org-gtd-clarify--duplicate-queue) :title)))
    ;; Cleanup
    (org-gtd-clarify--queue-cleanup)))

(deftest clarify/duplicate-exact-shows-queue-window ()
  "Exact duplicate displays queue window."
  (ogt-eunit-with-mock-gtd
    (capture-inbox-item "Original item")
    (org-gtd-process-inbox)
    (with-wip-buffer
      (org-gtd-clarify-duplicate-exact)
      (assert-true (get-buffer "*Org GTD Duplicate Queue*")))
    ;; Cleanup
    (org-gtd-clarify--queue-cleanup)))

(deftest clarify/duplicate-with-rename-uses-new-title ()
  "Duplicate with rename uses provided title."
  (ogt-eunit-with-mock-gtd
    (capture-inbox-item "Original item")
    (org-gtd-process-inbox)
    (with-wip-buffer
      (with-simulated-input "New title RET"
        (org-gtd-clarify-duplicate))
      (assert-equal 1 (length org-gtd-clarify--duplicate-queue))
      (assert-equal "New title"
                    (plist-get (car org-gtd-clarify--duplicate-queue) :title)))
    ;; Cleanup
    (org-gtd-clarify--queue-cleanup)))

(deftest clarify/duplicate-fails-on-empty-buffer ()
  "Duplicate fails when buffer has no content."
  (with-temp-buffer
    (org-gtd-clarify-mode)
    (assert-error (org-gtd-clarify-duplicate-exact))))
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev etest -r dot -p clarify/duplicate`
Expected: FAIL

**Step 3: Write minimal implementation**

Add to `org-gtd-clarify.el` Commands section:

```elisp
(defun org-gtd-clarify-duplicate ()
  "Duplicate current item with a new title.
Prompts for a new title, then adds the duplicate to the queue."
  (interactive)
  (unless (derived-mode-p 'org-gtd-clarify-mode)
    (user-error "Not in a clarify buffer"))
  (let ((content-plist (org-gtd-clarify--get-wip-content)))
    (unless content-plist
      (user-error "Nothing to duplicate"))
    (let* ((default-title (plist-get content-plist :title))
           (new-title (read-string "Duplicate title: " default-title))
           (content (plist-get content-plist :content)))
      (org-gtd-clarify--queue-add new-title content)
      (org-gtd-clarify--queue-display)
      (message "Duplicated: %s" new-title))))

(defun org-gtd-clarify-duplicate-exact ()
  "Duplicate current item exactly as-is.
Adds an exact copy to the queue without prompting for changes."
  (interactive)
  (unless (derived-mode-p 'org-gtd-clarify-mode)
    (user-error "Not in a clarify buffer"))
  (let ((content-plist (org-gtd-clarify--get-wip-content)))
    (unless content-plist
      (user-error "Nothing to duplicate"))
    (let ((title (plist-get content-plist :title))
          (content (plist-get content-plist :content)))
      (org-gtd-clarify--queue-add title content)
      (org-gtd-clarify--queue-display)
      (message "Duplicated: %s" title))))
```

**Step 4: Run tests to verify they pass**

Run: `~/bin/eldev etest -r dot -p clarify/duplicate`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): add duplicate commands

Add org-gtd-clarify-duplicate (with rename prompt) and
org-gtd-clarify-duplicate-exact (exact copy) commands.

Ref: #275
EOF
)"
```

---

## Task 7: Add Keybindings

**Files:**
- Modify: `org-gtd-clarify.el:163-171` (keymap section)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing tests**

```elisp
;;; Keybinding Tests

(deftest clarify/keymap-has-duplicate-bindings ()
  "Clarify mode map has d and D for duplicate commands."
  (assert-equal 'org-gtd-clarify-duplicate
                (lookup-key org-gtd-clarify-mode-map (kbd "d")))
  (assert-equal 'org-gtd-clarify-duplicate-exact
                (lookup-key org-gtd-clarify-mode-map (kbd "D"))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/keymap-has-duplicate`
Expected: FAIL

**Step 3: Write minimal implementation**

Modify the keymap definition in `org-gtd-clarify.el`:

```elisp
(defvar org-gtd-clarify-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'org-gtd-clarify-stop)
    (define-key map (kbd "d") #'org-gtd-clarify-duplicate)
    (define-key map (kbd "D") #'org-gtd-clarify-duplicate-exact)
    map)
  "Keymap for `org-gtd-clarify-mode'.")
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/keymap-has-duplicate`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): add keybindings for duplicate commands

Bind d to duplicate-with-rename and D to duplicate-exact
in org-gtd-clarify-mode-map.

Ref: #275
EOF
)"
```

---

## Task 8: Process Queue After Organize

**Files:**
- Modify: `org-gtd-organize-core.el:104-142` (org-gtd-organize--call)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing test**

```elisp
;;; Queue Processing Tests

(deftest clarify/organize-processes-queue-before-continuation ()
  "After organizing, processes queued duplicates before calling continuation."
  (ogt-eunit-with-mock-gtd
    (capture-inbox-item "Original item")
    (org-gtd-process-inbox)
    ;; Add a duplicate
    (with-wip-buffer
      (org-gtd-clarify-duplicate-exact))
    ;; Organize the original as single action
    (with-wip-buffer
      (organize-as-single-action))
    ;; Should now be clarifying the duplicate
    (assert-true (ogt-get-wip-buffer))
    (with-wip-buffer
      (assert-match "Original item" (buffer-string)))
    ;; Organize the duplicate
    (with-wip-buffer
      (organize-as-single-action))
    ;; Queue should be empty, no more WIP buffers from this session
    (assert-nil (ogt-get-wip-buffer))))
```

**Step 2: Run test to verify it fails**

Run: `~/bin/eldev etest -r dot -p clarify/organize-processes-queue`
Expected: FAIL

**Step 3: Write minimal implementation**

First, add a function to start clarifying from queue content in `org-gtd-clarify.el`:

```elisp
(defun org-gtd-clarify--process-next-queued-item (window-config continuation)
  "Process the next item from the duplicate queue.
WINDOW-CONFIG is restored after all items are processed.
CONTINUATION is called after the queue is empty."
  (let ((item (org-gtd-clarify--queue-pop)))
    (if item
        (let* ((content (plist-get item :content))
               (clarify-id (org-id-new))
               (processing-buffer (org-gtd-wip--get-buffer clarify-id)))
          ;; Update queue display or cleanup if empty
          (if (org-gtd-clarify--queue-empty-p)
              (org-gtd-clarify--queue-cleanup)
            (org-gtd-clarify--queue-display))
          ;; Initialize buffer with queued content
          (with-current-buffer processing-buffer
            (insert content)
            (goto-char (point-min))
            (unless (derived-mode-p 'org-gtd-clarify-mode)
              (org-gtd-clarify-mode))
            (setq-local org-gtd-clarify--window-config window-config
                        org-gtd-clarify--clarify-id clarify-id
                        org-gtd-clarify--continuation continuation
                        org-gtd-clarify--source-heading-marker nil))
          (org-gtd-clarify-setup-windows processing-buffer)
          (message "All duplicates processed"))
      ;; No more items - cleanup and continue
      (org-gtd-clarify--queue-cleanup)
      (message "All duplicates processed")
      (when window-config
        (set-window-configuration window-config))
      (when continuation
        (funcall continuation)))))
```

Then modify `org-gtd-organize--call` in `org-gtd-organize-core.el` to check queue:

Replace the continuation handling section (around lines 117-142) with:

```elisp
      ;; Check if there are queued duplicates to process
      (let ((has-queued-items (not (org-gtd-clarify--queue-empty-p))))
        (when task-id
          (org-gtd-wip--cleanup-temp-file task-id))
        (if has-queued-items
            ;; Process next queued item instead of calling continuation
            (org-gtd-clarify--process-next-queued-item window-config continuation)
          ;; No queue - normal flow
          (when window-config
            (set-window-configuration window-config))
          (when continuation (funcall continuation))
          ;; Save GTD buffers after organizing
          (org-gtd-save-buffers)
          ;; Clean up horizons view for one-off clarification
          (unless continuation
            (org-gtd-clarify--cleanup-horizons-view)))))))
```

**Step 4: Run test to verify it passes**

Run: `~/bin/eldev etest -r dot -p clarify/organize-processes-queue`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el org-gtd-organize-core.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): process duplicate queue after organizing

Modify org-gtd-organize--call to check the duplicate queue after
organizing. If items are queued, process them before calling the
continuation.

Ref: #275
EOF
)"
```

---

## Task 9: Handle Cancel with Pending Duplicates

**Files:**
- Modify: `org-gtd-clarify.el` (org-gtd-clarify-stop)
- Test: `test/unit/clarify-test.el`

**Step 1: Write the failing tests**

```elisp
;;; Cancel with Queue Tests

(deftest clarify/stop-with-queue-prompts-user ()
  "Stopping with pending duplicates prompts for action."
  (ogt-eunit-with-mock-gtd
    (capture-inbox-item "Original item")
    (org-gtd-process-inbox)
    (with-wip-buffer
      (org-gtd-clarify-duplicate-exact)
      (org-gtd-clarify-duplicate-exact))
    ;; Simulate choosing discard
    (with-wip-buffer
      (with-simulated-input "d"
        (org-gtd-clarify-stop)))
    ;; Queue should be cleared
    (assert-nil (get-buffer "*Org GTD Duplicate Queue*"))))

(deftest clarify/stop-save-to-inbox-preserves-duplicates ()
  "Choosing save-to-inbox preserves duplicates in inbox."
  (ogt-eunit-with-mock-gtd
    (capture-inbox-item "Original item")
    (org-gtd-process-inbox)
    (with-wip-buffer
      (org-gtd-clarify-duplicate-exact))
    ;; Simulate choosing save
    (with-wip-buffer
      (with-simulated-input "s"
        (org-gtd-clarify-stop)))
    ;; Check inbox has the duplicate
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (assert-match "Original item" (buffer-string)))))
```

**Step 2: Run tests to verify they fail**

Run: `~/bin/eldev etest -r dot -p clarify/stop`
Expected: FAIL

**Step 3: Write minimal implementation**

Add helper function and modify `org-gtd-clarify-stop` in `org-gtd-clarify.el`:

```elisp
(defun org-gtd-clarify--queue-save-to-inbox ()
  "Save all queued duplicates to the inbox."
  (let ((inbox-file (org-gtd-inbox-path)))
    (with-current-buffer (find-file-noselect inbox-file)
      (goto-char (point-max))
      (dolist (item org-gtd-clarify--duplicate-queue)
        (insert "\n" (plist-get item :content)))
      (save-buffer))))

(defun org-gtd-clarify--prompt-queue-action ()
  "Prompt user for action on pending duplicates.
Returns \\='discard, \\='save, or \\='cancel."
  (let* ((count (length org-gtd-clarify--duplicate-queue))
         (titles (mapcar (lambda (item) (plist-get item :title))
                         org-gtd-clarify--duplicate-queue))
         (prompt (format "%d pending duplicate%s:\n  - %s\n[d]iscard all  [s]ave to inbox  [c]ancel: "
                         count
                         (if (= count 1) "" "s")
                         (string-join titles "\n  - "))))
    (pcase (read-char-choice prompt '(?d ?s ?c))
      (?d 'discard)
      (?s 'save)
      (?c 'cancel))))
```

Modify `org-gtd-clarify-stop`:

```elisp
(defun org-gtd-clarify-stop ()
  "Stop clarifying the current item and restore previous state.
If there are pending duplicates, prompts whether to discard or save them."
  (interactive)
  ;; Handle pending duplicates
  (when (and (boundp 'org-gtd-clarify--duplicate-queue)
             (not (org-gtd-clarify--queue-empty-p)))
    (pcase (org-gtd-clarify--prompt-queue-action)
      ('save (org-gtd-clarify--queue-save-to-inbox))
      ('cancel (keyboard-quit))
      ('discard nil)))  ; Just continue with cleanup

  (let ((window-config org-gtd-clarify--window-config)
        (task-id org-gtd-clarify--clarify-id)
        (inbox-p org-gtd-clarify--inbox-p))
    ;; Clean up queue
    (org-gtd-clarify--queue-cleanup)
    ;; Clean up horizons view
    (org-gtd-clarify--cleanup-horizons-view)
    ;; Clean up temp file and kill buffer
    (when task-id
      (org-gtd-wip--cleanup-temp-file task-id))
    ;; Clear inbox processing session state if we were processing inbox
    (when inbox-p
      (setq org-gtd-process--session-active nil
            org-gtd-process--pending-inboxes nil))
    ;; Restore window configuration
    (when window-config
      (set-window-configuration window-config))
    (message "Stopped clarifying")))
```

**Step 4: Run tests to verify they pass**

Run: `~/bin/eldev etest -r dot -p clarify/stop`
Expected: PASS

**Step 5: Commit**

```bash
git add org-gtd-clarify.el test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
feat(clarify): handle cancel with pending duplicates

When stopping clarification with pending duplicates, prompt user
to discard, save to inbox, or cancel the stop operation.

Ref: #275
EOF
)"
```

---

## Task 10: Update Header-Line and Documentation

**Files:**
- Modify: `org-gtd-clarify.el` (mode definition, docstrings)
- No test needed for documentation

**Step 1: Update mode header-line**

Modify the `org-gtd-clarify-mode` definition to mention duplicate keys:

```elisp
(setq-local
 header-line-format
 (substitute-command-keys
  "\\<org-gtd-clarify-mode-map>Clarify item. \\[org-gtd-organize] to file, \\[org-gtd-clarify-duplicate] to duplicate, \\[org-gtd-clarify-stop] to cancel."))
```

**Step 2: Run full test suite**

Run: `~/bin/eldev etest -r dot`
Expected: All tests pass

**Step 3: Commit**

```bash
git add org-gtd-clarify.el
git commit -m "$(cat <<'EOF'
docs(clarify): update header-line with duplicate keybinding

Show the duplicate command in the clarify buffer header-line.

Ref: #275
EOF
)"
```

---

## Task 11: Integration Test - Full Workflow

**Files:**
- Test: `test/unit/clarify-test.el`

**Step 1: Write comprehensive integration test**

```elisp
;;; Integration Tests

(deftest clarify/duplicate-full-workflow ()
  "Test complete duplicate workflow: create, process, verify."
  (ogt-eunit-with-mock-gtd
    ;; Start with inbox item
    (capture-inbox-item "Meeting prep")
    (org-gtd-process-inbox)

    ;; Create two duplicates with different names
    (with-wip-buffer
      (with-simulated-input "Book train RET"
        (org-gtd-clarify-duplicate))
      (with-simulated-input "Prepare notes RET"
        (org-gtd-clarify-duplicate)))

    ;; Verify queue has 2 items
    (with-wip-buffer
      (assert-equal 2 (length org-gtd-clarify--duplicate-queue)))

    ;; Organize original as calendar
    (with-wip-buffer
      (with-simulated-input "2026-02-01 RET"
        (org-gtd-calendar)))

    ;; Now clarifying first duplicate
    (with-wip-buffer
      (assert-match "Book train\\|Meeting prep" (buffer-string)))

    ;; Organize first duplicate as single action
    (with-wip-buffer
      (organize-as-single-action))

    ;; Now clarifying second duplicate
    (with-wip-buffer
      (assert-match "Prepare notes\\|Meeting prep" (buffer-string)))

    ;; Organize second duplicate as single action
    (with-wip-buffer
      (organize-as-single-action))

    ;; Should be done - no more WIP buffers
    (assert-nil (ogt-get-wip-buffer))

    ;; Verify all three items exist in GTD system
    (with-current-buffer (org-gtd--default-file)
      (assert-match "Meeting prep" (buffer-string))
      (assert-match "Book train\\|Prepare notes" (buffer-string)))))
```

**Step 2: Run integration test**

Run: `~/bin/eldev etest -r dot -p clarify/duplicate-full-workflow`
Expected: PASS

**Step 3: Commit**

```bash
git add test/unit/clarify-test.el
git commit -m "$(cat <<'EOF'
test(clarify): add integration test for duplicate workflow

Comprehensive test covering the full duplicate workflow:
create duplicates, organize each, verify all items in GTD system.

Ref: #275
EOF
)"
```

---

## Task 12: Final Verification and Cleanup

**Step 1: Run full test suite**

```bash
~/bin/eldev clean && ~/bin/eldev compile && ~/bin/eldev etest -r dot
```

Expected: All tests pass, no compilation warnings

**Step 2: Run linter**

```bash
~/bin/eldev lint
```

Expected: No critical issues

**Step 3: Update CHANGELOG**

Add to CHANGELOG.org under next release:

```org
** New Features
- Add ability to duplicate items during clarification (#275)
  - ~d~ duplicates with rename prompt
  - ~D~ duplicates exactly
  - Duplicates are processed before returning to previous context
  - Cancel with pending duplicates prompts to discard or save to inbox
```

**Step 4: Final commit**

```bash
git add CHANGELOG.org
git commit -m "$(cat <<'EOF'
docs: update CHANGELOG for duplicate feature

Document the new duplicate item functionality for the next release.

Closes #275
EOF
)"
```

---

## Summary

| Task | Description | Files |
|------|-------------|-------|
| 1 | Add customization variable | org-gtd-clarify.el |
| 2 | Add buffer-local queue variable | org-gtd-clarify.el |
| 3 | Add queue helper functions | org-gtd-clarify.el |
| 4 | Add queue display functions | org-gtd-clarify.el |
| 5 | Add content extraction helper | org-gtd-clarify.el |
| 6 | Add duplicate commands | org-gtd-clarify.el |
| 7 | Add keybindings | org-gtd-clarify.el |
| 8 | Process queue after organize | org-gtd-organize-core.el |
| 9 | Handle cancel with pending duplicates | org-gtd-clarify.el |
| 10 | Update header-line and docs | org-gtd-clarify.el |
| 11 | Integration test | test/unit/clarify-test.el |
| 12 | Final verification | CHANGELOG.org |
