# Checklists + Guided Weekly Review Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement the design in `docs/plans/2026-07-06-checklists-and-guided-review-design.md`: plain-org checklist templates (`checklists.org` + insert command + reset-on-repeat hook) and a guided, pausable, profile-driven review session (`org-gtd-review`), plus `org-gtd-review-schedule` and `org-gtd-init-system`.

**Architecture:** Two new modules — `org-gtd-checklist.el` (file convention, parser, insert, reset hook) and `org-gtd-review.el` (profiles defcustom + session engine in a `*GTD Review*` special-mode buffer with pause/resume via `review-state.eld`) — plus a tiny `org-gtd-init.el`. No changes to the type registry or organize transient. The reset hook is installed by `org-gtd-mode` exactly like the existing `org-after-todo-state-change-hook` functions.

**Tech Stack:** Emacs Lisp (28.1+), org-mode, transient (command center rows only), f.el, e-unit test framework with `ogt-eunit-with-mock-gtd`, `with-simulated-input`.

---

## Ground rules for the executor

- **Run tests ONLY via the `/test` skill** (Skill tool, `skill: test`, `args: <test-file-path>` for one file, no args for full suite). Never run `eldev` directly. Test selection is file-level only.
- **TDD**: every task writes the failing test first, sees it fail, implements, sees it pass, commits.
- **New `.el` files** must copy the header boilerplate (Copyright/GPL/Commentary) and footer (`provide` + `;;; ... ends here`) from `org-gtd-files.el`, adjusting the description. `lexical-binding: t` in the first line. Checkdoc-clean docstrings (imperative first line, ≤ 80 cols).
- **Test files** follow `test/unit/files-test.el` exactly: `(require 'ogt-eunit-prelude "test/helpers/prelude.el")`, `(e-unit-initialize)`, `around-each` wrapping `ogt-eunit-with-mock-gtd`, `deftest group/name ()` naming, `assert-equal` / `assert-nil` / `assert-match` assertions, `(provide 'xxx-test)` footer.
- **Mock FS facts**: `org-gtd-directory` is `/mock:/gtd/`; the mock spec pre-creates `inbox.org`, `org-gtd-tasks.org`, `org-gtd-calendar.org`, `org-gtd-incubate.org` as empty files — it does **not** pre-create `checklists.org`, so seeding is testable. Use `(let ((org-inhibit-logging t)) ...)` around `org-todo` calls.
- Commit after every green test with the message given in the task.

---

### Task 1: Checklist file, seeding, and visit command

**Files:**
- Create: `org-gtd-checklist.el`
- Create: `test/unit/checklist-test.el`

**Step 1: Write the failing tests**

```elisp
;;; checklist-test.el --- Tests for org-gtd checklists -*- lexical-binding: t; coding: utf-8 -*-
;; (header boilerplate as per files-test.el)
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest checklist/file-is-created-with-starter-templates ()
  "First touch creates checklists.org seeded with starter trigger lists."
  (let ((buf (org-gtd-checklist--file-buffer)))
    (with-current-buffer buf
      (assert-match "\\* Weekly Review triggers" (buffer-string))
      (assert-match "\\* Mind sweep prompts" (buffer-string))
      (assert-match "- \\[ \\]" (buffer-string)))))

(deftest checklist/seeding-is-idempotent ()
  "Touching the file twice does not duplicate the starters."
  (org-gtd-checklist--file-buffer)
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (goto-char (point-min))
    (assert-equal 1 (count-matches "^\\* Weekly Review triggers$"))))

(provide 'checklist-test)
;;; checklist-test.el ends here
```

**Step 2: Run to verify failure**

Use the Skill tool: `skill: test`, `args: test/unit/checklist-test.el`
Expected: FAIL — `void-function org-gtd-checklist--file-buffer`.

**Step 3: Write minimal implementation**

Create `org-gtd-checklist.el` (full header boilerplate; Commentary: "Reusable checklist templates stored as plain org headings in checklists.org."):

```elisp
;;;; Requirements

(require 'org)
(require 'f)
(require 'org-gtd-core)
(require 'org-gtd-files)

;;;; Constants

(defconst org-gtd-checklist-file-name "checklists"
  "Base name of the checklist templates file inside `org-gtd-directory'.")

(defconst org-gtd-checklist--starter-contents
  "* Weekly Review triggers
- [ ] Projects started but not completed?
- [ ] Commitments or promises made to others?
- [ ] Communications to make or expecting (calls, emails)?
- [ ] Writing to finish or submit?
- [ ] Meetings that need to be set or requested?
- [ ] Decisions that need to be made?
- [ ] Waiting for someone else's reply or delivery?
- [ ] Financial or administrative loose ends?

* Mind sweep prompts
- [ ] Boss, partners, colleagues?
- [ ] Family and friends?
- [ ] Household — repairs, maintenance, errands?
- [ ] Health — appointments, checkups, exercise?
- [ ] Finances — bills, taxes, banks?
- [ ] Car or transportation?
- [ ] Creative ideas, things to learn?
- [ ] Places to go, people to see?
"
  "Contents used to seed a brand-new checklists file.")

;;;; Functions

;;;;; Private

(defun org-gtd-checklist--file-path ()
  "Return the full path to the checklists file."
  (org-gtd--path org-gtd-checklist-file-name))

(defun org-gtd-checklist--file-buffer ()
  "Return a buffer visiting the checklists file, creating it if needed.
A newly created file is seeded with starter templates."
  (let ((path (org-gtd-checklist--file-path)))
    (org-gtd--ensure-file-exists path org-gtd-checklist--starter-contents)
    (find-file-noselect path)))

;;;;; Commands

;;;###autoload
(defun org-gtd-checklist-visit ()
  "Visit the checklist templates file.
Each top-level heading is a reusable checklist template."
  (interactive)
  (pop-to-buffer (org-gtd-checklist--file-buffer)))

;;;; Footer

(provide 'org-gtd-checklist)
```

**Step 4: Run to verify pass**

`skill: test`, `args: test/unit/checklist-test.el` — Expected: PASS (2 tests).

**Step 5: Commit**

```bash
git add org-gtd-checklist.el test/unit/checklist-test.el
git commit -m "feat: add checklists.org template file with starter trigger lists"
```

---

### Task 2: Template names and items parser

**Files:**
- Modify: `org-gtd-checklist.el`
- Modify: `test/unit/checklist-test.el`

**Step 1: Add failing tests**

```elisp
(deftest checklist/names-lists-top-level-headings ()
  "Template names are the top-level heading titles."
  (assert-equal '("Weekly Review triggers" "Mind sweep prompts")
                (org-gtd-checklist-names)))

(deftest checklist/items-returns-ordered-item-strings ()
  "Items of a named checklist come back as ordered plain strings."
  (let ((items (org-gtd-checklist--items "Mind sweep prompts")))
    (assert-equal "Boss, partners, colleagues?" (car items))
    (assert-equal 8 (length items))))

(deftest checklist/items-ignores-checked-state ()
  "A checked box still yields its item text."
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (goto-char (point-min))
    (search-forward "- [ ] Boss")
    (replace-match "- [X] Boss")
    (basic-save-buffer))
  (assert-equal "Boss, partners, colleagues?"
                (car (org-gtd-checklist--items "Mind sweep prompts"))))

(deftest checklist/items-nil-for-unknown-name ()
  "Unknown checklist name returns nil, no error."
  (assert-nil (org-gtd-checklist--items "No such list")))
```

**Step 2: Run to verify failure** — `args: test/unit/checklist-test.el`, expected `void-function org-gtd-checklist-names`.

**Step 3: Implement** (add to `org-gtd-checklist.el` under Private / a new Public section):

```elisp
(defun org-gtd-checklist-names ()
  "Return the list of checklist template names, in file order."
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (names)
       (while (re-search-forward "^\\* +\\(.+?\\)[ \t]*$" nil t)
         (push (match-string-no-properties 1) names))
       (nreverse names)))))

(defun org-gtd-checklist--items (name)
  "Return the ordered checkbox item strings of checklist NAME.
Return nil when no checklist NAME exists or it has no items."
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward
            (format "^\\* +%s[ \t]*$" (regexp-quote name)) nil t)
       (let ((end (save-excursion (org-end-of-subtree t t) (point)))
             items)
         (while (re-search-forward
                 "^[ \t]*- \\[[ Xx-]\\] +\\(.+?\\)[ \t]*$" end t)
           (push (match-string-no-properties 1) items))
         (nreverse items))))))
```

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: parse checklist template names and items"`

---

### Task 3: `org-gtd-checklist-insert`

**Files:**
- Modify: `org-gtd-checklist.el`
- Modify: `test/unit/checklist-test.el`

**Step 1: Add failing tests**

```elisp
;; add at top of test file, after the prelude require:
;; (require 'with-simulated-input)

(deftest checklist/insert-copies-subtree-at-point ()
  "Insert spawns the named template as a subtree at point."
  (with-temp-buffer
    (org-mode)
    (with-simulated-input "Weekly SPC Review SPC triggers RET"
      (call-interactively #'org-gtd-checklist-insert))
    (assert-match "^\\* Weekly Review triggers" (buffer-string))
    (assert-match "- \\[ \\] Projects started" (buffer-string))))

(deftest checklist/insert-adapts-level-to-context ()
  "Inserting under an existing heading demotes the copy."
  (with-temp-buffer
    (org-mode)
    (insert "* Trip to the beach\n")
    (goto-char (point-max))
    (org-gtd-checklist-insert "Mind sweep prompts")
    (assert-match "^\\*\\* Mind sweep prompts" (buffer-string))))

(deftest checklist/insert-unknown-name-errors-cleanly ()
  "Unknown name signals a user-error naming the file."
  (with-temp-buffer
    (org-mode)
    (let ((err (should-error-p (org-gtd-checklist-insert "Nope"))))
      (assert-non-nil err))))
```

Note for executor: if the helpers provide no `should-error-p`, use
`(condition-case e (progn (org-gtd-checklist-insert "Nope") nil) (user-error e))`
and assert non-nil. Check `test/helpers/assertions.el` first.

**Step 2: Run to verify failure.**

**Step 3: Implement**

```elisp
;;;###autoload
(defun org-gtd-checklist-insert (name)
  "Insert a fresh instance of checklist NAME as a subtree at point.
The copy is an ordinary org subtree — org-gtd does not track it.
To make it a recurring task, organize it (e.g. as a habit) with
`org-gtd-clarify-item' after inserting."
  (interactive
   (list (completing-read "Checklist: " (org-gtd-checklist-names) nil t)))
  (let ((subtree
         (with-current-buffer (org-gtd-checklist--file-buffer)
           (org-with-wide-buffer
            (goto-char (point-min))
            (unless (re-search-forward
                     (format "^\\* +%s[ \t]*$" (regexp-quote name)) nil t)
              (user-error "No checklist named '%s' — edit %s"
                          name (org-gtd-checklist--file-path)))
            (buffer-substring-no-properties
             (line-beginning-position)
             (save-excursion (org-end-of-subtree t t) (point)))))))
    (unless (bolp) (insert "\n"))
    (org-paste-subtree nil subtree)))
```

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: add org-gtd-checklist-insert to spawn template instances"`

---

### Task 4: Checkbox reset on repeater re-arm

**Files:**
- Modify: `org-gtd-checklist.el`
- Modify: `org-gtd-mode.el` (hook install ~line 144 block, removal ~line 114 block)
- Create: `test/unit/checklist-reset-test.el`

**Step 1: Write failing tests**

```elisp
;;; checklist-reset-test.el --- Tests for checkbox reset on repeat -*- lexical-binding: t -*-
;; (boilerplate as usual)

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defmacro checklist-reset--with-heading (text &rest body)
  "Run BODY in an org buffer containing TEXT, point on first heading,
with the reset hook installed."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (goto-char (point-min))
     (add-hook 'org-after-todo-state-change-hook
               #'org-gtd-checklist--maybe-reset-checkboxes)
     (unwind-protect (progn ,@body)
       (remove-hook 'org-after-todo-state-change-hook
                    #'org-gtd-checklist--maybe-reset-checkboxes))))

(deftest checklist-reset/repeating-heading-resets-boxes-on-done ()
  "Completing a repeating heading clears its checkboxes and re-arms."
  (checklist-reset--with-heading
      "* TODO Weekly triggers\nSCHEDULED: <2026-07-10 Fri .+1w>\n- [X] Boss?\n- [ ] Car?\n"
    (let ((org-inhibit-logging t) (org-log-repeat nil))
      (org-todo "DONE"))
    (assert-nil (string-match-p "\\[X\\]" (buffer-string)))
    (assert-match "\\* TODO Weekly triggers" (buffer-string))))

(deftest checklist-reset/plain-done-keeps-boxes ()
  "Completing a non-repeating heading leaves checkboxes alone."
  (checklist-reset--with-heading
      "* TODO Beach packing\n- [X] Sunscreen\n- [ ] Towel\n"
    (let ((org-inhibit-logging t))
      (org-todo "DONE"))
    (assert-match "\\[X\\] Sunscreen" (buffer-string))))

(deftest checklist-reset/org-gtd-mode-installs-hook ()
  "org-gtd-mode adds and removes the reset hook."
  (org-gtd-mode 1)
  (assert-non-nil (memq #'org-gtd-checklist--maybe-reset-checkboxes
                        org-after-todo-state-change-hook))
  (org-gtd-mode -1)
  (assert-nil (memq #'org-gtd-checklist--maybe-reset-checkboxes
                    org-after-todo-state-change-hook)))

(provide 'checklist-reset-test)
;;; checklist-reset-test.el ends here
```

**Step 2: Run to verify failure.**

**Step 3: Implement.** In `org-gtd-checklist.el`:

```elisp
(defun org-gtd-checklist--maybe-reset-checkboxes ()
  "Clear checkboxes in the subtree when a repeating heading is completed.
Meant for `org-after-todo-state-change-hook'.  When the heading at
point carries a repeater and just entered a done state, org re-arms
it; clearing the boxes makes the next run start fresh.  A plain DONE
on a non-repeating heading is left untouched."
  (when (and (org-get-repeat)
             (member org-state org-done-keywords))
    (org-reset-checkbox-state-subtree)))
```

In `org-gtd-mode.el`: add `(require 'org-gtd-checklist)` to the requires, then in the enable block (next to the existing `org-after-todo-state-change-hook` adds around line 144):

```elisp
(add-hook 'org-after-todo-state-change-hook #'org-gtd-checklist--maybe-reset-checkboxes)
```

and the matching `remove-hook` in the disable block (~line 114).

If the repeating test fails because org's auto-repeat runs *before* the hook and `org-state` is already "TODO": change the condition to also fire when `(and (org-get-repeat) (equal org-state "TODO") org-last-state (member org-last-state org-done-keywords))` — debug with the actual hook semantics, don't guess; add a temporary `message` to see `org-state`/`org-last-todo-state` values.

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: reset checkboxes when a repeating heading re-arms"`

---

### Task 5: Wire checklist module into org-gtd.el and command center

**Files:**
- Modify: `org-gtd.el` (require list, after line 58 `org-gtd-files`)
- Modify: `org-gtd-command-center.el` (Reflect group + requires)
- Modify: `test/unit/command-center-test.el`

**Step 1: Add failing test** (mirror the style of existing tests in `command-center-test.el` — read it first; it likely asserts transient suffixes/bindings):

```elisp
(deftest command-center/has-checklists-entry ()
  "The Reflect group binds l to visiting checklists."
  (let ((layout (get 'org-gtd-command-center 'transient--layout)))
    (assert-match "org-gtd-checklist-visit" (format "%S" layout))))
```

**Step 2: Run to verify failure.**

**Step 3: Implement.**
- `org-gtd.el`: add `(require 'org-gtd-checklist)` after `(require 'org-gtd-files)`.
- `org-gtd-command-center.el`: add `(require 'org-gtd-checklist)` to requires; add to the "Reflect" column after the `R` row:

```elisp
    ("l" "Checklists" org-gtd-checklist-visit)
```

**Step 4: Run to verify pass** (`args: test/unit/command-center-test.el`).

**Step 5: Commit** — `git commit -m "feat: expose checklists from the command center"`

---

### Task 6: Review profiles defcustom + accessors

**Files:**
- Create: `org-gtd-review.el`
- Create: `test/unit/review-test.el`

**Step 1: Write failing tests**

```elisp
;;; review-test.el --- Tests for the guided review engine -*- lexical-binding: t -*-
;; (boilerplate)

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (setq org-gtd-review--state nil
          org-gtd-review--window-config nil)
    (funcall proceed context)))

(deftest review/default-profile-is-weekly-three-phase ()
  "The shipped default is a Weekly Review with the three GTD phases."
  (let ((profile (assoc "Weekly Review" org-gtd-review-profiles)))
    (assert-non-nil profile)
    (assert-equal '("Get Clear" "Get Current" "Get Creative")
                  (mapcar #'car (cdr profile)))))

(deftest review/default-mind-sweep-references-starter-checklist ()
  "The Get Clear phase walks the bundled trigger list."
  (let* ((phases (cdr (assoc "Weekly Review" org-gtd-review-profiles)))
         (get-clear (cdr (assoc "Get Clear" phases)))
         (sweep (seq-find (lambda (s) (eq (plist-get s :type) 'checklist))
                          get-clear)))
    (assert-equal "Weekly Review triggers" (plist-get sweep :checklist))))

(provide 'review-test)
;;; review-test.el ends here
```

**Step 2: Run to verify failure.**

**Step 3: Implement.** Create `org-gtd-review.el` (full boilerplate; Commentary: "Guided, profile-driven review sessions — Weekly Review by default."):

```elisp
;;;; Requirements

(require 'org)
(require 'f)
(require 'seq)
(require 'org-gtd-core)
(require 'org-gtd-files)
(require 'org-gtd-checklist)
(require 'org-gtd-create)

;;;; Customization

(defcustom org-gtd-review-profiles
  '(("Weekly Review"
     ("Get Clear"
      (:title "Gather loose materials"
       :type prompt
       :instruction "Collect loose papers, receipts, and notes.  Capture each one into the inbox.")
      (:title "Mind sweep"
       :type checklist
       :checklist "Weekly Review triggers"
       :instruction "Walk each trigger.  Press c to capture whatever it shakes loose.")
      (:title "Inbox to zero"
       :type command
       :command org-gtd-process-inbox
       :instruction "Process every inbox item.  Come back here and press n when the inbox is empty."))
     ("Get Current"
      (:title "Review missed items"
       :type view
       :view org-gtd-reflect-missed-items
       :instruction "Reschedule, complete, or cancel anything that slipped.")
      (:title "Review Waiting-For"
       :type view
       :view org-gtd-reflect-upcoming-delegated
       :instruction "Nudge, close, or re-delegate each delegated item.")
      (:title "Review next actions"
       :type view
       :view org-gtd-show-all-next
       :instruction "Mark done what is done; check these still feel current.")
      (:title "Review stuck projects"
       :type view
       :view org-gtd-reflect-stuck-projects
       :instruction "Give every active project a next action."))
     ("Get Creative"
      (:title "Review Someday/Maybe"
       :type view
       :view org-gtd-reflect-someday-maybe
       :instruction "Reactivate anything whose time has come.")
      (:title "Capture new ideas"
       :type prompt
       :instruction "Any creative, risky, or fun ideas?  Capture them.")))
  "Alist of guided review profiles.
Each entry is (PROFILE-NAME . PHASES); each phase is
\(PHASE-NAME . STEPS); each step is a plist with :title, :type
\(one of `prompt', `command', `view', `checklist'), an optional
:instruction, and the type-specific key :command, :view, or
:checklist (a template name in the checklists file)."
  :group 'org-gtd
  :type 'sexp)

;;;; Variables

(defvar org-gtd-review--state nil
  "State plist of the active review session.
Keys: :profile (name string), :phase (index), :step (index),
:acted (step-local flag), :walk-items, :walk-pos, :done, :skipped.")

(defvar org-gtd-review--window-config nil
  "Window configuration to restore when the session ends.")

(defconst org-gtd-review--buffer-name "*GTD Review*")

;;;; Accessors

(defun org-gtd-review--phases ()
  "Return the phases of the active profile."
  (cdr (assoc (plist-get org-gtd-review--state :profile)
              org-gtd-review-profiles)))

(defun org-gtd-review--current-phase ()
  "Return the (NAME . STEPS) phase the session is in."
  (nth (plist-get org-gtd-review--state :phase) (org-gtd-review--phases)))

(defun org-gtd-review--current-step ()
  "Return the step plist the session is on."
  (nth (plist-get org-gtd-review--state :step)
       (cdr (org-gtd-review--current-phase))))
```

(Footer: `provide` etc.)

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: add org-gtd-review-profiles with default Weekly Review"`

---

### Task 7: Session buffer, prompt steps, phase advance, completion

**Files:**
- Modify: `org-gtd-review.el`
- Modify: `test/unit/review-test.el`

**Step 1: Add failing tests**

```elisp
(defvar review-test--tiny-profile
  '(("Tiny"
     ("Phase A"
      (:title "Step one" :type prompt :instruction "Do one.")
      (:title "Step two" :type prompt))
     ("Phase B"
      (:title "Step three" :type prompt))))
  "Minimal all-prompt profile for engine tests.")

(deftest review/start-opens-session-buffer-on-first-step ()
  "Starting a session renders profile, phase, and step 1."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "Tiny" (buffer-string))
      (assert-match "Phase A" (buffer-string))
      (assert-match "step 1/2" (buffer-string))
      (assert-match "Step one" (buffer-string)))))

(deftest review/n-advances-through-steps-and-phases ()
  "n on prompt steps advances; crossing a phase boundary re-renders."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (assert-match "Step two" (buffer-string))
      (org-gtd-review-next)
      (assert-match "Phase B" (buffer-string))
      (assert-match "Step three" (buffer-string)))))

(deftest review/completing-last-step-ends-session ()
  "Finishing the last step tears the session down."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-next)
      (org-gtd-review-next))
    (assert-nil org-gtd-review--state)
    (assert-nil (get-buffer org-gtd-review--buffer-name))))

(deftest review/skip-counts-separately ()
  "s advances but tallies into :skipped."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-skip))
    (assert-equal 1 (plist-get org-gtd-review--state :skipped))
    (assert-equal 0 (plist-get org-gtd-review--state :done))))
```

**Step 2: Run to verify failure.**

**Step 3: Implement.** Add to `org-gtd-review.el`:

```elisp
;;;; Keymap and mode

(defvar org-gtd-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'org-gtd-review-next)
    (define-key map (kbd "s") #'org-gtd-review-skip)
    (define-key map (kbd "c") #'org-gtd-review-capture)
    (define-key map (kbd "p") #'org-gtd-review-pause)
    (define-key map (kbd "q") #'org-gtd-review-quit)
    map)
  "Keymap for `org-gtd-review-mode'.")

(define-derived-mode org-gtd-review-mode special-mode "GTD-Review"
  "Major mode for the guided review session console.

\\{org-gtd-review-mode-map}"
  :group 'org-gtd)

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-gtd-review-mode 'emacs)
  (add-hook 'org-gtd-review-mode-hook #'evil-emacs-state))

;;;; Rendering

(defun org-gtd-review--header-line (step)
  "Compute the header line advertising keys for STEP."
  (concat "[n] Do/advance  [s] Skip  [p] Pause  [q] Quit"
          (when (eq (plist-get step :type) 'checklist)
            "  [c] Capture")))

(defun org-gtd-review--phase-tracker ()
  "Render the phase tracker line."
  (let ((current (plist-get org-gtd-review--state :phase)))
    (mapconcat
     (lambda (pair)
       (let ((i (car pair)) (name (car (cdr pair))))
         (cond ((< i current) (format "[✓ %s]" name))
               ((= i current) (format "▸ %s ◂" name))
               (t (format "[ %s ]" name)))))
     (seq-map-indexed (lambda (ph i) (cons i ph)) (org-gtd-review--phases))
     "  ")))

(defun org-gtd-review--render ()
  "Render the session buffer from `org-gtd-review--state'."
  (let* ((state org-gtd-review--state)
         (phase (org-gtd-review--current-phase))
         (steps (cdr phase))
         (step (org-gtd-review--current-step)))
    (with-current-buffer (get-buffer-create org-gtd-review--buffer-name)
      (unless (derived-mode-p 'org-gtd-review-mode) (org-gtd-review-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n" (plist-get state :profile)))
        (insert (org-gtd-review--phase-tracker) "\n\n")
        (insert (format "%s — step %d/%d\n\n"
                        (car phase)
                        (1+ (plist-get state :step))
                        (length steps)))
        (insert (format "  %s\n" (plist-get step :title)))
        (when-let ((instr (plist-get step :instruction)))
          (insert (format "\n  %s\n" instr)))
        (when (and (eq (plist-get step :type) 'checklist)
                   (plist-get state :walk-items))
          (let ((items (plist-get state :walk-items))
                (pos (plist-get state :walk-pos)))
            (insert (format "\n    → %s   (%d/%d)\n"
                            (nth pos items) (1+ pos) (length items)))))
        (goto-char (point-min)))
      (setq header-line-format (org-gtd-review--header-line step))
      (pop-to-buffer (current-buffer)))))

;;;; Step advancement

(defun org-gtd-review--complete-step (&optional skipped)
  "Advance past the current step, tallying SKIPPED or done."
  (let ((state org-gtd-review--state)
        (counter (if skipped :skipped :done)))
    (plist-put state counter (1+ (plist-get state counter)))
    (plist-put state :acted nil)
    (plist-put state :walk-items nil)
    (plist-put state :walk-pos 0)
    (let ((steps (cdr (org-gtd-review--current-phase)))
          (next-step (1+ (plist-get state :step))))
      (if (< next-step (length steps))
          (progn (plist-put state :step next-step)
                 (org-gtd-review--render))
        (let ((phases (org-gtd-review--phases))
              (next-phase (1+ (plist-get state :phase))))
          (if (< next-phase (length phases))
              (progn
                (message "%s complete — on to %s."
                         (car (nth (plist-get state :phase) phases))
                         (car (nth next-phase phases)))
                (plist-put state :phase next-phase)
                (plist-put state :step 0)
                (org-gtd-review--render))
            (org-gtd-review--finish)))))))

(defun org-gtd-review--teardown ()
  "Kill the session buffer, clear state, restore windows."
  (setq org-gtd-review--state nil)
  (when (get-buffer org-gtd-review--buffer-name)
    (kill-buffer org-gtd-review--buffer-name))
  (when org-gtd-review--window-config
    (set-window-configuration org-gtd-review--window-config)
    (setq org-gtd-review--window-config nil)))

(defun org-gtd-review--finish ()
  "Complete the session: report, clean up."
  (let ((done (plist-get org-gtd-review--state :done))
        (skipped (plist-get org-gtd-review--state :skipped)))
    (org-gtd-review--teardown)
    (message (concat "Review complete: %d steps done, %d skipped.  "
                     "Tip: M-x org-gtd-review-schedule puts this on your calendar.")
             done skipped)))

;;;; Commands

(defun org-gtd-review-next ()
  "Do the current step, or advance past it."
  (interactive)
  (let* ((step (org-gtd-review--current-step))
         (type (plist-get step :type)))
    (pcase type
      ('prompt (org-gtd-review--complete-step))
      (_ (message "Step type %s not implemented yet" type)))))

(defun org-gtd-review-skip ()
  "Skip the current step for this run only."
  (interactive)
  (org-gtd-review--complete-step t))

(defun org-gtd-review-capture ()
  "Capture something to the inbox mid-review."
  (interactive)
  (call-interactively #'org-gtd-capture))

(defun org-gtd-review-pause () "Placeholder." (interactive))
(defun org-gtd-review-quit () "Placeholder." (interactive) (org-gtd-review--teardown))

;;;; Entry point

;;;###autoload
(defun org-gtd-review (&optional profile-name)
  "Run a guided review session.
With more than one profile in `org-gtd-review-profiles', prompt;
PROFILE-NAME selects one non-interactively."
  (interactive)
  (let* ((names (mapcar #'car org-gtd-review-profiles))
         (name (or profile-name
                   (if (cdr names)
                       (completing-read "Review profile: " names nil t)
                     (car names)))))
    (unless (assoc name org-gtd-review-profiles)
      (user-error "No review profile named '%s'" name))
    (setq org-gtd-review--window-config (current-window-configuration))
    (setq org-gtd-review--state
          (list :profile name :phase 0 :step 0 :acted nil
                :walk-items nil :walk-pos 0 :done 0 :skipped 0))
    (org-gtd-review--render)))
```

`org-gtd-capture` needs `(require 'org-gtd-capture)` — add it to the requires.

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: guided review session engine with prompt steps"`

---

### Task 8: `command` and `view` step types

**Files:**
- Modify: `org-gtd-review.el` (the `pcase` in `org-gtd-review-next`)
- Modify: `test/unit/review-test.el`

**Step 1: Add failing tests**

```elisp
(defvar review-test--command-calls 0)
(defun review-test--command ()
  "Test command that records invocations."
  (interactive)
  (setq review-test--command-calls (1+ review-test--command-calls)))

(deftest review/command-step-runs-command-then-advances ()
  "First n invokes :command; second n advances."
  (setq review-test--command-calls 0)
  (let ((org-gtd-review-profiles
         '(("T" ("P" (:title "Run it" :type command :command review-test--command)
                     (:title "After" :type prompt))))))
    (org-gtd-review "T")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (assert-equal 1 review-test--command-calls)
      (assert-match "Run it" (buffer-string))   ; still on the step
      (org-gtd-review-next)
      (assert-match "After" (buffer-string)))))

(deftest review/view-step-shows-view-then-advances ()
  "First n calls :view (other window); second n advances."
  (let ((org-gtd-review-profiles
         '(("T" ("P" (:title "Look" :type view :view review-test--command)
                     (:title "After" :type prompt))))))
    (setq review-test--command-calls 0)
    (org-gtd-review "T")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (assert-equal 1 review-test--command-calls)
      (org-gtd-review-next)
      (assert-match "After" (buffer-string)))))

(deftest review/unknown-step-type-skips-with-message ()
  "An unknown :type never errors; it advances."
  (let ((org-gtd-review-profiles
         '(("T" ("P" (:title "Weird" :type frobnicate)
                     (:title "After" :type prompt))))))
    (org-gtd-review "T")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (assert-match "After" (buffer-string)))))
```

**Step 2: Run to verify failure.**

**Step 3: Implement** — replace the `pcase` in `org-gtd-review-next`:

```elisp
(pcase type
  ('prompt (org-gtd-review--complete-step))
  ('command
   (if (plist-get org-gtd-review--state :acted)
       (org-gtd-review--complete-step)
     (plist-put org-gtd-review--state :acted t)
     (call-interactively (plist-get step :command))))
  ('view
   (if (plist-get org-gtd-review--state :acted)
       (org-gtd-review--complete-step)
     (plist-put org-gtd-review--state :acted t)
     (save-selected-window
       (call-interactively (plist-get step :view)))))
  ('checklist (org-gtd-review--walk-next step))
  (_
   (message "Step type '%s' is unknown — skipping this step" type)
   (org-gtd-review--complete-step t)))
```

Add a placeholder so this compiles: `(defun org-gtd-review--walk-next (_step) "Placeholder." nil)` — Task 9 replaces it.

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: command and view step types in review sessions"`

---

### Task 9: `checklist` walk step

**Files:**
- Modify: `org-gtd-review.el`
- Modify: `test/unit/review-test.el`

**Step 1: Add failing tests**

```elisp
(defvar review-test--walk-profile
  '(("Walk"
     ("P"
      (:title "Sweep" :type checklist :checklist "Mind sweep prompts")
      (:title "After" :type prompt)))))

(deftest review/checklist-step-walks-items-one-at-a-time ()
  "n loads the walk, then advances item by item, then leaves the step."
  (let ((org-gtd-review-profiles review-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)                       ; load walk, show item 1
      (assert-match "Boss, partners, colleagues\\?" (buffer-string))
      (assert-match "(1/8)" (buffer-string))
      (org-gtd-review-next)                       ; item 2
      (assert-match "(2/8)" (buffer-string))
      (dotimes (_ 7) (org-gtd-review-next))       ; through item 8 and out
      (assert-match "After" (buffer-string)))))

(deftest review/checklist-step-missing-template-auto-advances ()
  "A missing/empty checklist self-satisfies instead of erroring."
  (let ((org-gtd-review-profiles
         '(("W" ("P" (:title "Sweep" :type checklist :checklist "Nope")
                     (:title "After" :type prompt))))))
    (org-gtd-review "W")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (assert-match "After" (buffer-string)))))
```

**Step 2: Run to verify failure.**

**Step 3: Implement** — replace the placeholder:

```elisp
(defun org-gtd-review--walk-next (step)
  "Advance the checklist walk for STEP, loading it on first call."
  (let ((state org-gtd-review--state))
    (if (not (plist-get state :acted))
        (let ((items (org-gtd-checklist--items (plist-get step :checklist))))
          (if (null items)
              (progn
                (message "Nothing in checklist '%s' — moving on.  (Edit %s to add items.)"
                         (plist-get step :checklist)
                         (org-gtd-checklist--file-path))
                (org-gtd-review--complete-step))
            (plist-put state :acted t)
            (plist-put state :walk-items items)
            (plist-put state :walk-pos 0)
            (org-gtd-review--render)))
      (let ((next (1+ (plist-get state :walk-pos))))
        (if (< next (length (plist-get state :walk-items)))
            (progn (plist-put state :walk-pos next)
                   (org-gtd-review--render))
          (org-gtd-review--complete-step))))))
```

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: checklist walk steps in review sessions"`

---

### Task 10: Pause, resume, quit

**Files:**
- Modify: `org-gtd-review.el`
- Modify: `test/unit/review-test.el`

**Step 1: Add failing tests**

```elisp
(deftest review/pause-persists-state-and-tears-down ()
  "p writes review-state.eld and closes the session."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))
    (assert-nil org-gtd-review--state)
    (assert-non-nil (file-exists-p (org-gtd-review--state-file)))))

(deftest review/resume-restores-position ()
  "Starting again after a pause offers resume and restores the step."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))
    (with-simulated-input "y" (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "Step two" (buffer-string)))))

(deftest review/resume-with-changed-profile-starts-over ()
  "Out-of-range saved state falls back to a fresh session."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause)))
  (let ((org-gtd-review-profiles
         '(("Tiny" ("Only" (:title "Sole" :type prompt))))))
    (org-gtd-review)                       ; no resume prompt: state invalid
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "Sole" (buffer-string)))
    (assert-nil (file-exists-p (org-gtd-review--state-file)))))

(deftest review/completion-deletes-state-file ()
  "Finishing a resumed session removes review-state.eld."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-pause))
    (with-simulated-input "y" (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (dotimes (_ 3) (org-gtd-review-next)))
    (assert-nil (file-exists-p (org-gtd-review--state-file)))))
```

**Step 2: Run to verify failure.**

**Step 3: Implement.** Add persistence section:

```elisp
;;;; Pause / resume persistence

(defun org-gtd-review--state-file ()
  "Return the path of the paused-session state file."
  (f-join org-gtd-directory "review-state.eld"))

(defun org-gtd-review--save-state ()
  "Serialize the session state to `org-gtd-review--state-file'."
  (with-temp-file (org-gtd-review--state-file)
    (let ((print-length nil) (print-level nil))
      (prin1 org-gtd-review--state (current-buffer)))))

(defun org-gtd-review--load-state ()
  "Read a saved session state, or nil."
  (let ((file (org-gtd-review--state-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (ignore-errors (read (current-buffer)))))))

(defun org-gtd-review--delete-state-file ()
  "Remove the saved session state, if any."
  (let ((file (org-gtd-review--state-file)))
    (when (file-exists-p file) (delete-file file))))

(defun org-gtd-review--state-valid-p (state)
  "Non-nil when STATE still fits `org-gtd-review-profiles'."
  (when-let* ((profile (assoc (plist-get state :profile)
                              org-gtd-review-profiles))
              (phases (cdr profile))
              (p (plist-get state :phase))
              (s (plist-get state :step)))
    (and (integerp p) (integerp s)
         (< p (length phases))
         (< s (length (cdr (nth p phases)))))))
```

Replace the placeholder pause/quit commands:

```elisp
(defun org-gtd-review-pause ()
  "Pause the session; `org-gtd-review' resumes it later."
  (interactive)
  (org-gtd-review--save-state)
  (org-gtd-review--teardown)
  (message "Review paused — run M-x org-gtd-review to resume."))

(defun org-gtd-review-quit ()
  "Quit the session, offering to keep or abandon progress."
  (interactive)
  (if (y-or-n-p "Keep progress to resume later? ")
      (org-gtd-review-pause)
    (org-gtd-review--delete-state-file)
    (org-gtd-review--teardown)
    (message "Review abandoned.")))
```

Rework the entry point to check for saved state, and delete the file in `--finish`:

```elisp
;;;###autoload
(defun org-gtd-review (&optional profile-name)
  "Run a guided review session, resuming a paused one when offered.
With more than one profile in `org-gtd-review-profiles', prompt;
PROFILE-NAME selects one non-interactively."
  (interactive)
  (let ((saved (org-gtd-review--load-state)))
    (cond
     ((and saved (not (org-gtd-review--state-valid-p saved)))
      (org-gtd-review--delete-state-file)
      (message "Saved review no longer matches your profiles — starting over.")
      (org-gtd-review--start-fresh profile-name))
     ((and saved
           (y-or-n-p (format "Resume paused '%s' review? "
                             (plist-get saved :profile))))
      (setq org-gtd-review--window-config (current-window-configuration))
      (setq org-gtd-review--state saved)
      (org-gtd-review--render))
     (t
      (org-gtd-review--delete-state-file)
      (org-gtd-review--start-fresh profile-name)))))

(defun org-gtd-review--start-fresh (profile-name)
  "Start a new session for PROFILE-NAME (prompting when nil)."
  (let* ((names (mapcar #'car org-gtd-review-profiles))
         (name (or profile-name
                   (if (cdr names)
                       (completing-read "Review profile: " names nil t)
                     (car names)))))
    (unless (assoc name org-gtd-review-profiles)
      (user-error "No review profile named '%s'" name))
    (setq org-gtd-review--window-config (current-window-configuration))
    (setq org-gtd-review--state
          (list :profile name :phase 0 :step 0 :acted nil
                :walk-items nil :walk-pos 0 :done 0 :skipped 0))
    (org-gtd-review--render)))
```

In `org-gtd-review--finish`, add `(org-gtd-review--delete-state-file)` as the first form.

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: pause and resume for guided review sessions"`

---

### Task 11: `org-gtd-review-schedule`

**Files:**
- Modify: `org-gtd-review.el`
- Create: `test/unit/review-schedule-test.el`

**Step 1: Write failing test**

```elisp
;; (standard boilerplate + around-each with mock-gtd)

(deftest review-schedule/creates-habit-with-repeater ()
  "Scheduling creates a properly-typed habit in the tasks file."
  (org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w")
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (let ((text (buffer-string)))
      (assert-match "Weekly Review" text)
      (assert-match ":ORG_GTD: +Habits" text)
      (assert-match "SCHEDULED: <2026-07-10 [A-Za-z]+ \\.\\+1w>" text)
      (assert-match "M-x org-gtd-review" text))))
```

(Executor: check how other tests assert on property drawers — property matching may need `":ORG_GTD:\\s-+Habits"`. Adjust the regexp, not the behavior.)

**Step 2: Run to verify failure.**

**Step 3: Implement**

```elisp
;;;###autoload
(defun org-gtd-review-schedule (&optional profile-name date repeater)
  "Create a recurring habit reminding you to run a review.
PROFILE-NAME, DATE (YYYY-MM-DD) and REPEATER (org repeater like
\".+1w\") are prompted for interactively."
  (interactive)
  (let* ((names (mapcar #'car org-gtd-review-profiles))
         (profile (or profile-name
                      (if (cdr names)
                          (completing-read "Review profile: " names nil t)
                        (car names))))
         (date (or date (org-read-date nil nil nil "First review: ")))
         (repeater (or repeater
                       (read-string "How often? (org repeater, e.g. .+1w): "
                                    nil nil ".+1w"))))
    (org-gtd-create-item 'habit profile
                         `((:when . ,(format "<%s %s>" date repeater))))
    (with-current-buffer (org-gtd--default-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (when (re-search-forward
              (format "^\\*+ +.*%s[ \t]*$" (regexp-quote profile)) nil t)
         (org-end-of-meta-data t)
         (insert "Run M-x org-gtd-review when you sit down for this.\n")
         (basic-save-buffer))))
    (message "'%s' reminder created — it will show up in your engage view."
             profile)))
```

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: org-gtd-review-schedule creates a recurring review habit"`

---

### Task 12: `org-gtd-init-system`

**Files:**
- Create: `org-gtd-init.el`
- Create: `test/unit/init-system-test.el`

**Step 1: Write failing tests**

```elisp
;; (standard boilerplate + around-each with mock-gtd)

(deftest init-system/creates-all-gtd-files ()
  "Init ensures tasks, inbox, and seeded checklists files exist."
  (with-simulated-input "n" (org-gtd-init-system))
  (assert-non-nil (file-exists-p (org-gtd--path org-gtd-default-file-name)))
  (assert-non-nil (file-exists-p (org-gtd-inbox-path)))
  (assert-non-nil (file-exists-p (org-gtd-checklist--file-path))))

(deftest init-system/is-idempotent ()
  "Running init twice neither errors nor duplicates seeds."
  (with-simulated-input "n" (org-gtd-init-system))
  (with-simulated-input "n" (org-gtd-init-system))
  (with-current-buffer (org-gtd-checklist--file-buffer)
    (goto-char (point-min))
    (assert-equal 1 (count-matches "^\\* Weekly Review triggers$"))))

(deftest init-system/offers-review-schedule ()
  "Answering yes routes into org-gtd-review-schedule."
  (with-simulated-input "y RET RET RET"
    (org-gtd-init-system))
  (ogt--save-all-buffers)
  (with-current-buffer (org-gtd--default-file)
    (assert-match ":ORG_GTD: +Habits" (buffer-string))))
```

(Executor: the `"y RET RET RET"` drives y-or-n-p + the three schedule prompts — profile picker is skipped with a single profile, `org-read-date` and `read-string` take defaults on RET. If `org-read-date` misbehaves under simulated input, call `(org-gtd-review-schedule "Weekly Review" "2026-07-10" ".+1w")` from a `cl-letf`-stubbed `y-or-n-p` instead; the point of the test is the wiring, not the prompts.)

**Step 2: Run to verify failure.**

**Step 3: Implement.** Create `org-gtd-init.el` (boilerplate; Commentary: "Idempotent first-time setup concierge."):

```elisp
;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-files)
(require 'org-gtd-capture)
(require 'org-gtd-checklist)
(require 'org-gtd-review)

;;;; Commands

;;;###autoload
(defun org-gtd-init-system ()
  "Set up org-gtd for first use.  Safe to run again at any time.
Ensures the GTD files exist (seeding starter checklists) and offers
to schedule a recurring Weekly Review.  Every step reports and skips
when already satisfied — lazy initialization elsewhere is untouched."
  (interactive)
  (unless (file-directory-p org-gtd-directory)
    (make-directory org-gtd-directory t))
  (org-gtd--default-file)
  (org-gtd-inbox-path)
  (org-gtd-checklist--file-buffer)
  (message "✓ GTD files ready in %s" (abbreviate-file-name org-gtd-directory))
  (if (org-gtd-init--review-reminder-exists-p)
      (message "✓ A review reminder is already scheduled")
    (when (y-or-n-p "Schedule a recurring Weekly Review reminder? ")
      (call-interactively #'org-gtd-review-schedule))))

;;;; Functions

(defun org-gtd-init--review-reminder-exists-p ()
  "Non-nil when a habit named after a review profile already exists."
  (with-current-buffer (org-gtd--default-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (catch 'found
       (while (re-search-forward org-heading-regexp nil t)
         (when (and (equal (org-entry-get (point) "ORG_GTD") "Habits")
                    (assoc (org-get-heading t t t t)
                           org-gtd-review-profiles))
           (throw 'found t)))
       nil))))
```

**Step 4: Run to verify pass.**

**Step 5: Commit** — `git commit -m "feat: org-gtd-init-system first-run concierge"`

---

### Task 13: Wire review + init into org-gtd.el and command center

**Files:**
- Modify: `org-gtd.el` (requires)
- Modify: `org-gtd-command-center.el`
- Modify: `test/unit/command-center-test.el`

**Step 1: Add failing test**

```elisp
(deftest command-center/has-guided-review-entry ()
  "The Reflect group binds w to the guided review."
  (let ((layout (get 'org-gtd-command-center 'transient--layout)))
    (assert-match "org-gtd-review" (format "%S" layout))))
```

**Step 2: Run to verify failure.**

**Step 3: Implement.**
- `org-gtd.el`: add `(require 'org-gtd-review)` after `(require 'org-gtd-someday-review)` and `(require 'org-gtd-init)` just before `(require 'org-gtd-command-center)`.
- `org-gtd-command-center.el`: `(require 'org-gtd-review)`; add as the **first** row of the Reflect column:

```elisp
    ("w" "Weekly Review (guided)" org-gtd-review)
```

**Step 4: Run to verify pass** (also re-run `test/unit/checklist-test.el` and `test/unit/review-test.el` to catch load-order issues).

**Step 5: Commit** — `git commit -m "feat: guided review and init entries in command center"`

---

### Task 14: Documentation

**Files:**
- Modify: `doc/org-gtd.org` (new sections)
- Modify: `CHANGELOG.org` (unreleased entry)

**Step 1: Read** the manual's existing structure (`doc/org-gtd.org`) to match heading levels and tone.

**Step 2: Add a "Checklists" section** covering: the `checklists.org` convention, editing = customizing, `org-gtd-checklist-insert`, the reset-on-repeat behavior, and the composition pattern (insert → clarify → organize as habit for a recurring checklist-as-task).

**Step 3: Add a "Weekly Review (guided)" section** covering: `org-gtd-review`, the profile defcustom with the full default value reproduced, each step type, keys (`n s p q`, `c` on walks), pause/resume, `org-gtd-review-schedule`, `org-gtd-init-system`.

**Step 4: CHANGELOG.org** — add under an Unreleased heading:

```org
- Add =checklists.org= reusable checklist templates with =org-gtd-checklist-insert=
- Reset checkboxes automatically when a repeating heading re-arms
- Add =org-gtd-review=: guided, pausable, customizable review sessions (Weekly Review built in)
- Add =org-gtd-review-schedule= and =org-gtd-init-system=
- Command center: =w= guided review, =l= checklists
```

**Step 5: Commit** — `git commit -m "docs: manual and changelog for checklists and guided review"`

---

### Task 15: Full verification

**Step 1: Full test suite** — Skill tool, `skill: test`, no args. Expected: all green. Fix any regressions (suspect load order and hook leaks first — see memory notes on `around-each`).

**Step 2: Compile clean**

```bash
~/bin/eldev clean && ~/bin/eldev compile --warnings-as-errors
```

Expected: no warnings. Byte-compile warnings about undeclared functions mean a missing `require` or `declare-function`.

**Step 3: Lint**

```bash
~/bin/eldev lint --file="org-gtd-checklist.el" --file="org-gtd-review.el" --file="org-gtd-init.el"
```

Fix checkdoc/package-lint complaints (docstring format, `:type` specs).

**Step 4: yx bookkeeping**

```bash
yx start implement-checklists-and-guided-weekly-review-izj1   # if not already
yx done implement-checklists-and-guided-weekly-review-izj1
```

**Step 5: Commit any fixes** — `git commit -m "chore: lint and compile fixes for checklist/review modules"`

**Step 6:** Use superpowers:finishing-a-development-branch (the branch already has draft PR #294 — push and mark it ready or hand off per user preference).

---

## Deferred (do NOT implement — tracked in the design doc §7/§8)

Stats block, review log, back-step, org-heading `walk` step type, someday-review generalization, checklist manager transient, `CHECKLIST_KIND`, instance↔template links, agenda-files/keywords steps in init-system.
