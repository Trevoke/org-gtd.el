# Unified Type and Hook Model Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Refactor org-gtd's per-type modules onto a unified type+hook model driven from `org-gtd-types.el`, removing duplicated `--configure`/`--finalize`/`--apply` trios, collapsing agenda-specific DWIM variants, and giving users a single customization surface (`org-gtd-define-type` / `org-gtd-customize-type`) with pre/post hooks at six stages.

**Architecture:** Extend the existing type registry (`org-gtd-types.el`) with implementation-wiring fields (`:organize-fn`, `:disposition`, `:supports`, `:project-fn`, `:prompt-to-refile`, `:transient-key`, `:hooks`). Introduce two primitives in `org-gtd-organize-core.el` (`org-gtd-process-heading`, `org-gtd-process-project`) plus `org-gtd--dispatch` for DWIM context detection. Wire six hook stages (before/after × clarify/organize/file) as observation-only seams. Migrate each per-type module one-by-one behind green tests. The existing `configure-as-type`, `refile--do`, view DSL, and skip predicates are untouched.

**Tech Stack:** Emacs Lisp, org-mode, ert (via eldev + project's `/test` skill), transient.el.

**Design doc:** `docs/plans/2026-04-07-unified-type-and-hook-model-design.md` — read before starting.

**Testing note:** Always run tests via the `/test` skill (Skill tool), never directly. Flaky tests are usually setup issues in `around-each` or `ogt-eunit-with-mock-gtd`.

**Commit cadence:** Commit after every passing test cycle. Never batch phases.

---

## Phase 1 — Extend the type registry

Goal: new fields land in `org-gtd-types.el`, declaration/customization wrappers exist, nothing changes behaviorally.

### Task 1.1: Test — `org-gtd-type-get` exposes new wiring fields

**Files:**
- Modify: `test/org-gtd-types-test.el`

**Step 1: Write the failing test**

```elisp
(ert-deftest org-gtd-types-test-wiring-field-defaults ()
  "Wiring fields default to documented values when not declared."
  (let ((org-gtd-types '((fake :org-gtd "Fake" :state nil :properties nil))))
    (should (eq (org-gtd-type-organize-fn 'fake) #'org-gtd-configure-as-type))
    (should (eq (org-gtd-type-disposition 'fake) 'list))
    (should (null (org-gtd-type-supports 'fake)))
    (should (null (org-gtd-type-project-fn 'fake)))
    (should (null (org-gtd-type-prompt-to-refile 'fake)))
    (should (null (org-gtd-type-transient-key 'fake)))
    (should (null (org-gtd-type-hooks 'fake)))))
```

**Step 2: Run test** — expect FAIL (accessors undefined). Use `/test` skill.

**Step 3: Add accessors in `org-gtd-types.el`**

```elisp
(defun org-gtd-type-organize-fn (type-name)
  "Return the :organize-fn for TYPE-NAME, or the default."
  (or (plist-get (cdr (org-gtd-type-get type-name)) :organize-fn)
      #'org-gtd-configure-as-type))

(defun org-gtd-type-disposition (type-name)
  "Return the :disposition for TYPE-NAME (default \\='list)."
  (or (plist-get (cdr (org-gtd-type-get type-name)) :disposition)
      'list))

(defun org-gtd-type-supports (type-name)
  "Return the :supports list for TYPE-NAME."
  (plist-get (cdr (org-gtd-type-get type-name)) :supports))

(defun org-gtd-type-supports-p (type-name flag)
  "Return non-nil if TYPE-NAME declares FLAG in :supports."
  (memq flag (org-gtd-type-supports type-name)))

(defun org-gtd-type-project-fn (type-name)
  (plist-get (cdr (org-gtd-type-get type-name)) :project-fn))

(defun org-gtd-type-prompt-to-refile (type-name)
  (plist-get (cdr (org-gtd-type-get type-name)) :prompt-to-refile))

(defun org-gtd-type-transient-key (type-name)
  (plist-get (cdr (org-gtd-type-get type-name)) :transient-key))

(defun org-gtd-type-hooks (type-name)
  (plist-get (cdr (org-gtd-type-get type-name)) :hooks))
```

**Step 4: Run test** — expect PASS.

**Step 5: Commit**

```bash
git add org-gtd-types.el test/org-gtd-types-test.el
git commit -m "feat(types): add wiring-field accessors with defaults"
```

---

### Task 1.2: Test — merge helper preserves new fields

**Files:**
- Modify: `test/org-gtd-types-test.el`, `org-gtd-types.el`

**Step 1: Write failing tests**

```elisp
(ert-deftest org-gtd-types-test-merge-preserves-wiring ()
  "Merging user overrides must preserve builtin wiring fields."
  (let* ((builtin '(t1 :org-gtd "T1" :state nil :properties nil
                       :organize-fn my/fn :disposition 'done-and-archive
                       :supports (reactivate)))
         (user    '(t1 :properties nil))
         (merged  (org-gtd--merge-type-definitions builtin user)))
    (should (eq (plist-get (cdr merged) :organize-fn) 'my/fn))
    (should (eq (plist-get (cdr merged) :disposition) 'done-and-archive))
    (should (equal (plist-get (cdr merged) :supports) '(reactivate)))))

(ert-deftest org-gtd-types-test-merge-hooks-append ()
  "Local hooks must append across builtin and user definitions."
  (let* ((builtin '(t1 :org-gtd "T1" :state nil :properties nil
                       :hooks (:after-organize (fn-a))))
         (user    '(t1 :hooks (:after-organize (fn-b))))
         (merged  (org-gtd--merge-type-definitions builtin user))
         (hooks   (plist-get (cdr merged) :hooks)))
    (should (equal (plist-get hooks :after-organize) '(fn-a fn-b)))))

(ert-deftest org-gtd-types-test-merge-scalar-replaces ()
  (let* ((builtin '(t1 :org-gtd "T1" :state nil :properties nil
                       :prompt-to-refile nil))
         (user    '(t1 :prompt-to-refile t))
         (merged  (org-gtd--merge-type-definitions builtin user)))
    (should (eq (plist-get (cdr merged) :prompt-to-refile) t))))
```

**Step 2: Run tests** — expect FAIL.

**Step 3: Rewrite `org-gtd--merge-type-definitions`**

```elisp
(defconst org-gtd--scalar-fields
  '(:org-gtd :state :organize-fn :disposition :project-fn
    :prompt-to-refile :transient-key)
  "Type-plist keys where user values replace builtin values.")

(defconst org-gtd--list-fields
  '(:supports)
  "Type-plist keys where user values append to builtin values.")

(defun org-gtd--merge-hooks (builtin-hooks user-hooks)
  "Merge two :hooks plists — per-stage lists append."
  (let ((result (copy-sequence builtin-hooks)))
    (cl-loop for (stage fns) on user-hooks by #'cddr do
             (setq result
                   (plist-put result stage
                              (append (plist-get result stage) fns))))
    result))

(defun org-gtd--merge-type-definitions (builtin user)
  "Merge USER type definition into BUILTIN.
:org-gtd is never overridden.  Scalar fields replace, list fields append,
:properties merge by semantic name, :hooks merge per stage."
  (let* ((name (car builtin))
         (b (cdr builtin))
         (u (cdr user))
         (out (copy-sequence b)))
    ;; scalar fields (except :org-gtd which is immutable)
    (dolist (k org-gtd--scalar-fields)
      (unless (eq k :org-gtd)
        (when (plist-member u k)
          (setq out (plist-put out k (plist-get u k))))))
    ;; list-append fields
    (dolist (k org-gtd--list-fields)
      (when (plist-member u k)
        (setq out (plist-put out k
                             (append (plist-get b k) (plist-get u k))))))
    ;; :properties merged by semantic name (existing helper)
    (when (plist-member u :properties)
      (setq out (plist-put out :properties
                           (org-gtd--merge-properties
                            (plist-get b :properties)
                            (plist-get u :properties)))))
    ;; :hooks merged per-stage
    (when (plist-member u :hooks)
      (setq out (plist-put out :hooks
                           (org-gtd--merge-hooks
                            (plist-get b :hooks)
                            (plist-get u :hooks)))))
    (cons name out)))
```

**Step 4: Run tests** — expect PASS. Also run the full types-test file to make sure the old merge tests still pass.

**Step 5: Commit**

```bash
git commit -am "feat(types): merge helper handles wiring/hooks/list fields"
```

---

### Task 1.3: Test — `org-gtd-define-type` registers a new type

**Files:**
- Modify: `test/org-gtd-types-test.el`, `org-gtd-types.el`

**Step 1: Write failing test**

```elisp
(ert-deftest org-gtd-types-test-define-type-registers ()
  (let ((org-gtd-types (copy-tree org-gtd-types)))
    (org-gtd-define-type 'watching
      :org-gtd "Watching"
      :state :wait
      :properties '((:when :org-property "ORG_GTD_TIMESTAMP"
                           :type repeating-timestamp :required t
                           :prompt "Check back on: "))
      :disposition 'list
      :supports '(reactivate))
    (should (equal (org-gtd-type-org-gtd-value 'watching) "Watching"))
    (should (org-gtd-type-supports-p 'watching 'reactivate))))
```

**Step 2: Run** — FAIL.

**Step 3: Implement**

```elisp
(defun org-gtd-define-type (name &rest plist)
  "Register or replace type NAME with PLIST.
PLIST keys: :org-gtd :state :properties :organize-fn :disposition
:supports :project-fn :prompt-to-refile :transient-key :hooks."
  (let ((existing (assq name org-gtd-types))
        (entry (cons name plist)))
    (if existing
        (setcdr existing plist)
      (push entry org-gtd-types)))
  name)
```

**Step 4: Run** — PASS.

**Step 5: Commit**

```bash
git commit -am "feat(types): org-gtd-define-type entry point"
```

---

### Task 1.4: Test — `org-gtd-customize-type` accepts symbol or list

**Files:**
- Modify: `test/org-gtd-types-test.el`, `org-gtd-types.el`

**Step 1: Write failing tests**

```elisp
(ert-deftest org-gtd-types-test-customize-single ()
  (let ((org-gtd-types (copy-tree org-gtd-types))
        (org-gtd-user-types nil))
    (org-gtd-customize-type 'calendar :prompt-to-refile t)
    (should (eq (org-gtd-type-prompt-to-refile 'calendar) t))))

(ert-deftest org-gtd-types-test-customize-list ()
  (let ((org-gtd-types (copy-tree org-gtd-types))
        (org-gtd-user-types nil))
    (org-gtd-customize-type '(calendar tickler) :prompt-to-refile t)
    (should (eq (org-gtd-type-prompt-to-refile 'calendar) t))
    (should (eq (org-gtd-type-prompt-to-refile 'tickler) t))))

(ert-deftest org-gtd-types-test-customize-hooks-append ()
  (let ((org-gtd-types (copy-tree org-gtd-types))
        (org-gtd-user-types nil))
    (org-gtd-customize-type 'calendar :hooks '(:after-file (fn-a)))
    (org-gtd-customize-type 'calendar :hooks '(:after-file (fn-b)))
    (should (equal (plist-get (org-gtd-type-hooks 'calendar) :after-file)
                   '(fn-a fn-b)))))
```

**Step 2: Run** — FAIL.

**Step 3: Implement**

```elisp
(defun org-gtd-customize-type (name-or-names &rest plist)
  "Merge PLIST into the type definition(s) named by NAME-OR-NAMES.
NAME-OR-NAMES is a type symbol or a list of type symbols.
Scalar fields replace, list fields append, :hooks merge per stage."
  (dolist (name (if (listp name-or-names) name-or-names (list name-or-names)))
    (let* ((existing (or (assq name org-gtd-types)
                         (error "Unknown org-gtd type: %s" name)))
           (merged (org-gtd--merge-type-definitions existing (cons name plist))))
      (setcdr existing (cdr merged)))))
```

**Step 4: Run** — PASS.

**Step 5: Commit**

```bash
git commit -am "feat(types): org-gtd-customize-type with list + merge semantics"
```

---

## Phase 2 — Hook infrastructure

Goal: six global hook variables exist, a runner invokes global + local for a type, errors are caught.

### Task 2.1: Test — hook runner calls global then local and swallows errors

**Files:**
- Create: `test/org-gtd-hooks-test.el`
- Create: `org-gtd-hooks.el`

**Step 1: Write failing test**

```elisp
;;; org-gtd-hooks-test.el -*- lexical-binding: t -*-
(require 'ert)
(require 'org-gtd-hooks)
(require 'org-gtd-types)

(ert-deftest org-gtd-hooks-test-order-global-then-local ()
  (let* ((log nil)
         (org-gtd-before-organize-hook
          (list (lambda (_pom) (push 'g log))))
         (org-gtd-types (copy-tree org-gtd-types)))
    (org-gtd-customize-type 'calendar
      :hooks `(:before-organize ((lambda (_pom) (push 'l log)))))
    (org-gtd-hooks-run :before-organize 'calendar (point-marker))
    (should (equal (reverse log) '(g l)))))

(ert-deftest org-gtd-hooks-test-error-is-caught ()
  (let ((log nil)
        (org-gtd-before-organize-hook
         (list (lambda (_pom) (error "boom"))
               (lambda (_pom) (push 'ran log)))))
    (org-gtd-hooks-run :before-organize 'calendar (point-marker))
    (should (equal log '(ran)))))
```

**Step 2: Run** — FAIL (module missing).

**Step 3: Implement `org-gtd-hooks.el`**

```elisp
;;; org-gtd-hooks.el --- Six-stage hook infrastructure -*- lexical-binding: t -*-
(require 'org-gtd-types)

(defvar org-gtd-before-clarify-hook  nil "Runs before clarify for any type.")
(defvar org-gtd-after-clarify-hook   nil "Runs after clarify for any type.")
(defvar org-gtd-before-organize-hook nil "Runs before :organize-fn for any type.")
(defvar org-gtd-after-organize-hook  nil "Runs after :organize-fn for any type.")
(defvar org-gtd-before-file-hook     nil "Runs before refile/update for any type.")
(defvar org-gtd-after-file-hook      nil "Runs after refile/update for any type.")

(defconst org-gtd-hooks--global-alist
  '((:before-clarify  . org-gtd-before-clarify-hook)
    (:after-clarify   . org-gtd-after-clarify-hook)
    (:before-organize . org-gtd-before-organize-hook)
    (:after-organize  . org-gtd-after-organize-hook)
    (:before-file     . org-gtd-before-file-hook)
    (:after-file      . org-gtd-after-file-hook)))

(defun org-gtd-hooks--call-safely (fn pom)
  (condition-case err
      (funcall fn pom)
    (error (message "org-gtd hook %S errored: %s" fn (error-message-string err)))))

(defun org-gtd-hooks-run (stage type pom)
  "Run global then local hooks for STAGE on TYPE with POM."
  (let ((global-var (cdr (assq stage org-gtd-hooks--global-alist)))
        (local (plist-get (org-gtd-type-hooks type) stage)))
    (dolist (fn (symbol-value global-var))
      (org-gtd-hooks--call-safely fn pom))
    (dolist (fn local)
      (org-gtd-hooks--call-safely fn pom))))

(provide 'org-gtd-hooks)
```

**Step 4: Run** — PASS.

**Step 5: Commit**

```bash
git add org-gtd-hooks.el test/org-gtd-hooks-test.el
git commit -m "feat(hooks): six-stage hook runner with error isolation"
```

---

## Phase 3 — Primitives and dispatch

Goal: `process-heading`, `process-project`, `--dispatch` live in `organize-core.el` and are proven in isolation before per-type migration starts.

### Task 3.1: Test — `process-heading` runs the full sequence in order

**Files:**
- Modify: `org-gtd-organize-core.el`, `test/org-gtd-organize-core-test.el`

**Step 1: Write failing test**

The test sets up a fake type with a recorder `:organize-fn` and recording hooks, then calls `org-gtd-process-heading` on a scratch heading. Assert the exact order: `before-organize → organize-fn → after-organize → before-file → after-file`.

```elisp
(ert-deftest org-gtd-organize-core-test-process-heading-order ()
  (let* ((log nil)
         (record (lambda (tag) (lambda (_pom) (push tag log))))
         (org-gtd-before-organize-hook (list (funcall record 'b-org)))
         (org-gtd-after-organize-hook  (list (funcall record 'a-org)))
         (org-gtd-before-file-hook     (list (funcall record 'b-file)))
         (org-gtd-after-file-hook      (list (funcall record 'a-file)))
         (org-gtd-types
          `((fake :org-gtd "Fake" :state nil :properties nil
                  :organize-fn ,(lambda (&rest _) (push 'org-fn log))
                  :disposition list))))
    (cl-letf (((symbol-function 'org-gtd-refile--do) (lambda (&rest _) nil)))
      (with-temp-buffer
        (org-mode)
        (insert "* Thing\n")
        (goto-char (point-min))
        (org-gtd-process-heading (point-marker) 'fake)))
    (should (equal (reverse log) '(b-org org-fn a-org b-file a-file)))))
```

**Step 2: Run** — FAIL.

**Step 3: Implement in `org-gtd-organize-core.el`**

```elisp
(require 'org-gtd-hooks)
(require 'org-gtd-types)

(defun org-gtd-process-heading (pom type &optional config)
  "Organize heading at POM as TYPE, running the full hook pipeline.
CONFIG is an optional alist forwarded to the type's :organize-fn."
  (org-with-point-at pom
    (when (org-gtd-type-supports-p type 'reactivate)
      (org-gtd-save-state))
    (org-gtd--clear-foreign-properties type)
    (org-gtd-hooks-run :before-organize type pom)
    (funcall (org-gtd-type-organize-fn type) type config)
    (org-gtd-hooks-run :after-organize type pom)
    (org-gtd-hooks-run :before-file type pom)
    (org-gtd--run-disposition type pom)
    (org-gtd-hooks-run :after-file type pom)))
```

Stub `org-gtd--clear-foreign-properties` and `org-gtd--run-disposition` as no-ops for now; later tasks will flesh them out.

**Step 4: Run** — PASS.

**Step 5: Commit**

```bash
git commit -am "feat(organize-core): process-heading primitive with hook pipeline"
```

---

### Task 3.2: Test — disposition runner honors `:disposition`

**Files:**
- Modify: `org-gtd-organize-core.el`, `test/org-gtd-organize-core-test.el`

**Step 1: Failing test**

```elisp
(ert-deftest org-gtd-organize-core-test-disposition-list-refiles ()
  (let ((called nil)
        (org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil
                 :organize-fn ignore :disposition list))))
    (cl-letf (((symbol-function 'org-gtd-refile--do)
               (lambda (&rest _) (setq called 'refile))))
      (with-temp-buffer
        (org-mode)
        (insert "* Thing\n")
        (goto-char (point-min))
        (org-gtd-process-heading (point-marker) 'fake))
      (should (eq called 'refile)))))

(ert-deftest org-gtd-organize-core-test-disposition-done-and-archive ()
  (let ((called nil)
        (org-gtd-types
         '((fake :org-gtd "Fake" :state :done :properties nil
                 :organize-fn ignore :disposition done-and-archive))))
    (cl-letf (((symbol-function 'org-gtd-archive--do)
               (lambda (&rest _) (setq called 'archive))))
      (with-temp-buffer
        (org-mode)
        (insert "* Thing\n")
        (goto-char (point-min))
        (org-gtd-process-heading (point-marker) 'fake))
      (should (eq called 'archive)))))
```

**Step 2: Run** — FAIL.

**Step 3: Implement `org-gtd--run-disposition`**

```elisp
(defun org-gtd--run-disposition (type pom)
  "Dispatch on the type's :disposition."
  (let ((disp (org-gtd-type-disposition type)))
    (cond
     (org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place))
     ((eq disp 'list)
      (org-gtd-refile--do type (org-gtd--refile-target-for type)))
     ((eq disp 'done-and-archive)
      (org-gtd-archive--do type pom 'done))
     ((eq disp 'cancel-and-archive)
      (org-gtd-archive--do type pom 'canceled))
     ((eq disp 'externalize)
      (org-gtd-externalize--do type pom))
     (t (error "Unknown disposition: %s" disp)))))
```

Extract `org-gtd-archive--do` and `org-gtd-externalize--do` as thin wrappers around the existing knowledge/quick-action/trash logic in their own files — stub them now, fill from the per-type modules later.

**Step 4: Run** — PASS.

**Step 5: Commit**

```bash
git commit -am "feat(organize-core): disposition runner"
```

---

### Task 3.3: Test — foreign-property clearing

**Files:**
- Modify: `org-gtd-organize-core.el`, `test/org-gtd-organize-core-test.el`

Write a test: set up a heading that was previously `delegated` (with `DELEGATED_TO` and `ORG_GTD_TIMESTAMP`), convert to `calendar`, assert `DELEGATED_TO` is gone but `ORG_GTD_TIMESTAMP` survives (calendar declares it).

Implement `org-gtd--clear-foreign-properties` by diffing the outgoing type's `:properties` against the incoming type's `:properties` (match by `:org-property`) and calling `org-entry-delete` on each leftover. Read outgoing via `ORG_GTD` property. Use same TDD loop. Commit.

---

### Task 3.4: Test — `process-project` delegates to `:project-fn`

Same TDD pattern: fake type with `:supports '(project-handler)` and `:project-fn` → recorder, call `org-gtd-process-project`, assert the recorder fired and received the marker. Error path: type without project-handler raises `user-error`. Commit.

---

### Task 3.5: Test — `org-gtd--dispatch` reads agenda markers and routes correctly

Three tests: (a) on plain inbox heading → calls `process-heading`; (b) on project heading (`ORG_GTD = Projects`) → calls `process-project`; (c) on project task (non-empty `ORG_GTD_PROJECT_IDS`) → prompts for project marker and calls `process-project`.

Use `cl-letf` to stub both primitives. Implement `--dispatch` lifting the existing logic from `org-gtd-tickler`/`org-gtd-someday`. Commit.

---

## Phase 4 — Refile integration

### Task 4.1: Test — `refile--should-prompt-p` reads `:prompt-to-refile`

**Files:** `org-gtd-refile.el`, `test/org-gtd-refile-test.el`

Test that a type with `:prompt-to-refile t` makes `refile--should-prompt-p` return non-nil, and a type without it falls through to `org-gtd-refile-prompt-default`.

Add the defcustom:

```elisp
(defcustom org-gtd-refile-prompt-default nil
  "Default value for whether to prompt on refile when a type does not set
`:prompt-to-refile' explicitly."
  :group 'org-gtd :type 'boolean)
```

Rewrite `refile--should-prompt-p`:

```elisp
(defun org-gtd-refile--should-prompt-p (type)
  (let ((val (org-gtd-type-prompt-to-refile type)))
    (if (null val)
        org-gtd-refile-prompt-default
      val)))
```

Commit.

### Task 4.2: Migration shim for `org-gtd-refile-prompt-for-types`

At load time in `org-gtd-refile.el`:

```elisp
(defvar org-gtd-refile-prompt-for-types nil
  "Deprecated. Use :prompt-to-refile on the type or
`org-gtd-refile-prompt-default'.")
(make-obsolete-variable 'org-gtd-refile-prompt-for-types
                        'org-gtd-refile-prompt-default "4.0")

(defun org-gtd-refile--migrate-prompt-for-types ()
  (when org-gtd-refile-prompt-for-types
    (dolist (type org-gtd-refile-prompt-for-types)
      (when (assq type org-gtd-types)
        (org-gtd-customize-type type :prompt-to-refile t)))
    (setq org-gtd-refile-prompt-for-types nil)))
(add-hook 'org-gtd-mode-hook #'org-gtd-refile--migrate-prompt-for-types)
```

Test with the variable set, assert post-hook the types have the flag. Commit.

---

## Phase 5 — Per-type migrations

**Rule for every task in this phase:** the acceptance test is "all previously-passing tests for this type still pass, plus one new test that asserts the type declaration carries the expected new wiring fields." Do not delete the old `--configure`/`--finalize`/`--apply` functions in the same commit that adds the declaration — migrate in two steps per type.

### Task 5.1: Calendar migration — add wiring fields

**Files:** `org-gtd-types.el`, `org-gtd-calendar.el`, `test/org-gtd-calendar-test.el`

In the calendar entry of `org-gtd-types`, add `:disposition 'list`, `:transient-key "c"`. Add test asserting these accessors return the right values. Run the full calendar test suite via `/test`. Commit.

### Task 5.2: Calendar migration — switch command to dispatch

Rewrite `org-gtd-calendar` to:

```elisp
(defun org-gtd-calendar ()
  "DWIM: clarify and organize item at point as a calendar item."
  (interactive)
  (org-gtd--dispatch 'calendar))
```

Delete `org-gtd-calendar--configure`, `org-gtd-calendar--finalize`, `org-gtd-calendar--apply`. Keep `org-gtd-calendar-create` for now (Phase 6 will unify creators). Run the calendar test suite. If anything breaks, the primitive needs to cover the gap — patch the primitive, not the calendar module. Commit.

### Task 5.3: Delegate migration

Same pattern. Declaration gets `:disposition 'list`, `:transient-key "d"`. Command rewrites to `(org-gtd--dispatch 'delegated)`. Delete private functions. Run delegate test suite. Commit in two steps (fields, then swap).

### Task 5.4: Single-action migration

Declaration: `:disposition 'list`, `:transient-key "s"`. Command rewrites. Two commits.

### Task 5.5: Habit migration

Declaration: `:disposition 'list`, `:transient-key "h"`. Command rewrites. Two commits.

### Task 5.6: Tickler migration

Declaration: `:disposition 'list`, `:supports '(reactivate project-handler)`, `:project-fn #'org-gtd-project-incubate`, `:transient-key "i"`. Command rewrites. This is the most semantically loaded — verify the existing `ogt-eunit-with-mock-gtd` tests around tickler project handling still pass. The `config-override` alist path from `org-gtd-tickler-create` must keep working — forward it through the `config` arg on `process-heading`. Two commits.

### Task 5.7: Someday migration

Declaration: `:disposition 'list`, `:supports '(reactivate project-handler)`, `:project-fn #'org-gtd-project-someday`, `:transient-key "m"`. Command rewrites. Keep the `org-gtd-someday-lists` prompt inside a custom `:organize-fn` since `configure-as-type` cannot express "prompt for a value not in `:properties`":

```elisp
(defun org-gtd-someday--organize (type config)
  (org-gtd-configure-as-type type config)
  (when org-gtd-someday-lists
    (org-entry-put nil org-gtd-prop-someday-list
                   (completing-read "Someday list: "
                                    org-gtd-someday-lists nil t))))
```

Wire via `:organize-fn #'org-gtd-someday--organize` in the declaration. Two commits (fields, then swap).

### Task 5.8: Knowledge migration

Declaration: `:disposition 'done-and-archive`, `:transient-key "k"`. `org-gtd-archive--do` must handle `done` for this type. Also fix the long-standing bug: currently knowledge ignores `org-gtd-clarify--skip-refile`. The new disposition runner honors skip-refile uniformly, so migration fixes the bug as a side effect — add a regression test. Two commits.

### Task 5.9: Quick-action migration

Declaration: `:disposition 'done-and-archive`, `:transient-key "q"`. Same skip-refile fix applies. Two commits.

### Task 5.10: Trash migration

Declaration: `:disposition 'cancel-and-archive`, `:transient-key "x"`. Trash currently lacks the standard split — the migration actually adds structure rather than collapsing it. Write tests first that assert the declarative behavior, then delete the old bespoke path. Two commits.

### Task 5.11: Reference migration

Declaration: `:disposition 'done-and-archive` (or similar — confirm current behavior before writing), `:transient-key "r"`. Two commits.

---

## Phase 6 — Kill agenda special cases and unify creators

### Task 6.1: Obsolete `org-gtd-delegate-agenda-item` and siblings

Find all `*-agenda-item` commands (grep first). For each, replace the body with a call to the canonical DWIM command and mark obsolete:

```elisp
;;;###autoload
(define-obsolete-function-alias 'org-gtd-delegate-agenda-item
  #'org-gtd-delegate "4.0")
```

Verify agenda tests still pass (the DWIM wrapper handles `org-get-at-bol`). Commit once per deprecated command so bisection is easy.

### Task 6.2: Unified `org-gtd-create-item`

**Files:** new `org-gtd-create.el`, `test/org-gtd-create-test.el`

```elisp
(defun org-gtd-create-item (type topic &optional config)
  "Programmatically create a GTD item of TYPE with heading TOPIC.
CONFIG is forwarded to the type's :organize-fn."
  (let ((buffer (generate-new-buffer "*org-gtd-create*"))
        (org-id-overriding-file-name "org-gtd"))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert (format "* %s" topic))
          (goto-char (point-min))
          (org-gtd-process-heading (point-marker) type config))
      (kill-buffer buffer))))
```

Test per type (calendar, tickler, someday, delegated). Keep the per-type `-create` helpers as obsolete aliases. Commit.

---

## Phase 7 — Transient menu autogen

### Task 7.1: Test — transient entries built from `org-gtd-types`

**Files:** `org-gtd-organize.el`, `test/org-gtd-organize-test.el`

Assert that after defining a new type with `:transient-key "w"`, the transient prefix's suffix list includes an entry whose description matches the type's `:org-gtd` value and whose key is `"w"`.

### Task 7.2: Implement autogeneration

Replace the hand-curated `transient-define-prefix` body with a loop that reads `org-gtd-types`, filters to entries with `:transient-key` set, and generates suffixes that call `(lambda () (interactive) (org-gtd-process-heading (point) 'TYPE))`. The transient is rebuilt whenever a new type is defined (add a hook on `org-gtd-define-type` / `org-gtd-customize-type` that re-runs `transient-define-prefix`, or use `transient-setup-children` dynamically).

Run the full test suite. Commit.

---

## Phase 8 — Documentation and changelog

### Task 8.1: Update `doc/org-gtd.org`

Add a "Defining your own types" section pointing at `org-gtd-define-type` and `org-gtd-customize-type`. Show the Watching example from the design doc. Update the "Hooks" section with the six stages. Document that `org-gtd-refile-prompt-for-types` is obsolete.

### Task 8.2: Update `CHANGELOG.org`

Entry for the unreleased version summarizing: unified DWIM flow, hook stages, `define-type`/`customize-type`, agenda-command deprecations, refile-prompt migration. Link to the design doc.

### Task 8.3: Full test suite + lint

```bash
# via /test skill
~/bin/eldev lint
~/bin/eldev clean && ~/bin/eldev compile --warnings-as-errors
```

Fix anything that surfaces. Commit.

---

## Watchpoints during execution

- **`ogt-eunit-with-mock-gtd` / `around-each`** — if a test fails with "default-directory" weirdness, reset `default-directory` to `ogt-eunit--project-root` at test start.
- **Autoloads** — the new accessors and `define-type` are autoloaded. Re-run `~/bin/eldev compile` after touching the headers.
- **Obsolete aliases** — when you `define-obsolete-function-alias`, run `(makunbound 'old-name)` is wrong — use the macro so byte-compile knows about it. Verify there's no lingering `(defun org-gtd-delegate-agenda-item …)` left.
- **Emacs caching** — per the CLAUDE.md note, if using the emacs MCP server, use `eval-buffer`, not file loads.
- **`org-inhibit-logging`** — any test that drives state changes should bind this to `t` to suppress interactive note prompts.

## Rollback criteria

If any phase's full test run is red for more than one fix attempt, stop and investigate. Do not advance phases on broken tests. Phases 1–4 are independent of per-type migration — if Phase 5 gets stuck on one type, the other types can still ship independently because each migration is a self-contained two-commit pair.

---

Plan complete and saved to `docs/plans/2026-04-07-unified-type-and-hook-model-plan.md`. Two execution options:

1. **Subagent-Driven (this session)** — I dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Parallel Session (separate)** — open a new session with executing-plans, batch execution with checkpoints.

Which approach?
