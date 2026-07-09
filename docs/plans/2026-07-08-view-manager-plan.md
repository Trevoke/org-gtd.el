# View Manager Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Give org-gtd users an interactive layer to create, live-preview, name, save, recall, edit, and delete a custom GTD view without writing elisp, built strictly on top of the released view DSL.

**Architecture:** A new self-contained module `org-gtd-view-manager.el` adds four surfaces on top of the existing DSL: a plain-file `name -> spec` store (`views.eld` in `org-gtd-directory`), a builder transient (`org-gtd-view-manager--build`) whose infix rows are derived from a curated filter-spec table validated against the DSL, a list transient (`org-gtd-view-manager`) that browses/acts on saved views, and an autoloaded `completing-read` recall command (`org-gtd-view-run`). The DSL (`org-gtd-view-show`, `org-gtd-view-lang--known-filter-keys`, `--type-defaults`, `--simple-types`, `--complex-types`) is the render + preview engine and is **never modified**. Pure logic (store read/write, spec compile, badge formatter, migration flatten, anti-drift assertion) is unit-tested; the transient/preview interaction gets thin integration coverage plus manual steps.

**Tech Stack:** Emacs 28.1+, `transient.el` (>= 0.3.7), `org-agenda`, `f.el`, lexical-binding. Tests use the **e-unit** framework (`deftest`, `assert-equal`/`assert-true`/`assert-nil`, `around-each`, `ogt-eunit-with-mock-gtd`, `mock-fs`). Tests are run via the `/test` skill / `.claude/skills/test/run-tests.sh <file>` (single-file granularity only; NOT raw eldev).

---

## Authoritative sources (read before starting)

- **THE spec:** `docs/plans/2026-07-08-view-manager-design.md` — follow its decisions exactly, especially §2 (DSL reconciliation), §7 (storage/migration), and the Key-decisions table. Do not re-litigate; this plan implements it.
- **External handoff (interaction fidelity only):** `docs/design-references/view-manager/README.md` — keymaps, copy, layout. The design doc wins on every conflict (see design §10).
- **DSL you build ON TOP of (do NOT modify):** `org-gtd-view-language.el`
  - `org-gtd-view-lang--known-filter-keys` (:178) — the anti-drift authority.
  - `org-gtd-view-lang--type-defaults` (:188), `--default-prefix` (:209).
  - `org-gtd-view-lang--simple-types` (:215), `--complex-types` (:220) — type candidates.
  - `org-gtd-view-show` (:1081), signature `(view-spec-or-specs &optional keys)`.
- `org-gtd-command-center.el` (:44) — `v "Views…"` goes in the Engage column (Engage uses `e`/`@`/`n`; `v` is free).
- `org-gtd-reflect.el` (:296) — `org-gtd-reflect-missed-custom-views`, migration source (NESTED `(filters . (...))` shape).
- `org-gtd-files.el` — `org-gtd-directory` / `org-gtd--path` for locating the store. Do **not** reuse `org-gtd--ensure-file-exists` (:47) for `views.eld`: it runs `org-gtd-core-prepare-buffer` → `org-mode-restart` (`.org`-only). Write the `.eld` directly (Open Questions §1).
- `org-gtd-clarify.el` — window-config snapshot/restore discipline to mirror (`current-window-configuration` / `set-window-configuration`).

## Hard constraints (respect in EVERY task)

- Do **NOT** modify `org-gtd-view-show` or the view language. Build on top only.
- **No `org-ql`, no `org-super-agenda`.** The DSL exists for per-item `prefix` resolution they cannot express.
- **No default keybindings** — reached via `M-x` or the command-center `v` entry only.
- Fail-soft, teaching voice; never surface a stack trace to the user.
- Snapshot the window configuration on entry to both transients and **restore it on every exit path** (quit / abort / done).

## Module boilerplate (Task 1 creates the file; reuse this header verbatim)

```elisp
;;; org-gtd-view-manager.el --- Interactive manager for saved GTD views -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Interactive management layer for custom org-gtd views, built strictly on
;; top of the released view DSL (see `org-gtd-view-language.el').  Provides a
;; plain-file `name -> spec' store (`views.eld' in `org-gtd-directory'), a
;; builder transient with live org-agenda preview, a list transient to
;; browse/act on saved views, and a `completing-read' recall command.
;;
;;; Code:

;;;; Requirements

(require 'transient)
(require 'f)
(require 'org-gtd-core)
(require 'org-gtd-files)
(require 'org-gtd-view-language)

;; ... module body added task by task ...

;;;; Footer

(provide 'org-gtd-view-manager)

;;; org-gtd-view-manager.el ends here
```

**Test file header** (every new test file uses this shape — copy from `test/unit/files-test.el` for store/migration tests that need mock-fs, or from `test/unit/todo-filter-test.el` for pure tests that do not):

```elisp
;;; view-manager-store-test.el --- Tests for the views.eld store -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the view-manager name -> spec store.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;; ... deftests ...

(provide 'view-manager-store-test)
;;; view-manager-store-test.el ends here
```

Pure-logic tests (compile, badge, anti-drift, migration flatten) can require only `org-gtd-view-manager` directly and skip mock-fs, like `todo-filter-test.el`.

---

## Task 1: Store path + lazy-created `views.eld` store

**Files:**
- Create: `org-gtd-view-manager.el` (module header above + this task's code)
- Modify: `org-gtd.el` (add `(require 'org-gtd-view-manager)` after `(require 'org-gtd-view-language)` on line 86)
- Test: `test/unit/view-manager-store-test.el` (create)

Read the store as a `name -> spec` alist; write it back; lazily create the file with a guidance-comment header. Round-trip must be lossless.

**Step 1: Write the failing tests**

```elisp
(deftest view-manager-store/round-trips-specs ()
  "Writing then reading the store preserves specs verbatim."
  (let ((views '(("Weekend errands" . ((name . "Weekend errands")
                                        (type . next-action)
                                        (area-of-focus . "Home")
                                        (effort . (< "30m"))))
                 ("Waiting on Sam" . ((name . "Waiting on Sam")
                                      (type . delegated)
                                      (who . "Sam"))))))
    (org-gtd-view-manager--store-write views)
    (assert-equal views (org-gtd-view-manager--store-read))))

(deftest view-manager-store/missing-file-reads-empty ()
  "Reading before any write returns nil (empty store), creating the file."
  (assert-nil (org-gtd-view-manager--store-read))
  (assert-true (f-exists-p (org-gtd-view-manager--store-path))))

(deftest view-manager-store/header-comment-present ()
  "The lazily created store carries a guidance-comment header."
  (org-gtd-view-manager--store-read)
  (assert-match "Managed by org-gtd"
                (f-read-text (org-gtd-view-manager--store-path))))
```

**Step 2: Run to verify failure**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-store-test.el`
Expected: FAIL — `org-gtd-view-manager--store-write` / `--store-read` / `--store-path` void.

**Step 3: Minimal implementation** (in `org-gtd-view-manager.el`)

```elisp
;;;; Store

(defconst org-gtd-view-manager--store-file-name "views.eld"
  "Base name of the saved-views store inside `org-gtd-directory'.")

(defconst org-gtd-view-manager--store-header
  ";; Managed by org-gtd's View Manager (M-x org-gtd-view-manager).
;; A name -> spec alist of saved GTD views.  Edit via the manager, not by hand.
"
  "Guidance comment written to the top of a freshly created store.")

(defun org-gtd-view-manager--store-path ()
  "Return the absolute path to the views store."
  (f-join org-gtd-directory org-gtd-view-manager--store-file-name))

(defun org-gtd-view-manager--store-read ()
  "Return the saved-views alist, creating an empty store if absent.
Returns nil when the store is empty.  Fail-soft: a corrupt store
yields nil with a `message', never an error."
  (let ((path (org-gtd-view-manager--store-path)))
    ;; Lazily create the store by writing the header DIRECTLY.  Do NOT use
    ;; `org-gtd--ensure-file-exists' here: it runs `org-gtd-core-prepare-buffer'
    ;; -> `org-mode-restart', which is for .org files, not a .eld store.
    (unless (f-exists-p path)
      (f-mkdir-full-path (f-dirname path))
      (f-write-text org-gtd-view-manager--store-header 'utf-8 path))
    (condition-case err
        (let ((text (f-read-text path)))
          (if (string-blank-p text) nil (car (read-from-string
                                              (concat "(" text ")")))))
      (error (message "org-gtd: could not read views store: %s"
                      (error-message-string err))
             nil))))

(defun org-gtd-view-manager--store-write (views)
  "Persist VIEWS (a name -> spec alist) to the store, header preserved."
  (let ((path (org-gtd-view-manager--store-path)))
    (with-temp-file path
      (insert org-gtd-view-manager--store-header)
      (insert (prin1-to-string views))
      (insert "\n"))))
```

Note the read wraps the file body in parens so the header comment lines are skipped by the reader, then takes the single alist form. If the reader idiom proves fragile with the header, store the alist as one top-level form and read it directly — pick whichever passes the round-trip test; keep the header either way.

**Step 4: Run to verify pass**

Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-store-test.el`
Expected: PASS (3 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el org-gtd.el test/unit/view-manager-store-test.el
git commit -m "feat(view-manager): add lazily-created views.eld store"
```

> **Resolved (Open Questions §1):** the store is created by writing the header **directly** with `f-write-text`, not `org-gtd--ensure-file-exists` — that helper runs `org-gtd-core-prepare-buffer` → `org-mode-restart`, which is `.org`-only and would corrupt a `.eld` store. The code above reflects this.

---

## Task 2: Store CRUD helpers (upsert / delete / get)

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-store-test.el`

Small pure helpers the transients call, so the transient code stays thin.

**Step 1: Write the failing tests**

```elisp
(deftest view-manager-store/upsert-adds-and-replaces ()
  "Upsert adds a new view and replaces an existing one by name."
  (org-gtd-view-manager--store-upsert "A" '((name . "A") (type . next-action)))
  (assert-equal '((name . "A") (type . next-action))
                (org-gtd-view-manager--store-get "A"))
  (org-gtd-view-manager--store-upsert "A" '((name . "A") (type . delegated)))
  (assert-equal '((name . "A") (type . delegated))
                (org-gtd-view-manager--store-get "A"))
  (assert-equal 1 (length (org-gtd-view-manager--store-read))))

(deftest view-manager-store/delete-removes-entry ()
  "Delete removes a named view; getting it then returns nil."
  (org-gtd-view-manager--store-upsert "A" '((name . "A")))
  (org-gtd-view-manager--store-delete "A")
  (assert-nil (org-gtd-view-manager--store-get "A")))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-store-test.el` — Expected: FAIL (void functions).

**Step 3: Minimal implementation**

```elisp
(defun org-gtd-view-manager--store-get (name)
  "Return the stored spec for NAME, or nil."
  (cdr (assoc name (org-gtd-view-manager--store-read))))

(defun org-gtd-view-manager--store-upsert (name spec)
  "Store SPEC under NAME, replacing any existing entry, and persist."
  (let ((views (assoc-delete-all name (org-gtd-view-manager--store-read))))
    (org-gtd-view-manager--store-write
     (append views (list (cons name spec))))))

(defun org-gtd-view-manager--store-delete (name)
  "Remove NAME from the store and persist."
  (org-gtd-view-manager--store-write
   (assoc-delete-all name (org-gtd-view-manager--store-read))))
```

**Step 4: Run to verify pass** — Expected: PASS (5 tests total in file).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-store-test.el
git commit -m "feat(view-manager): add store upsert/get/delete helpers"
```

---

## Task 3: Filter-spec metadata table + load-time anti-drift assertion

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-filter-specs-test.el` (create; pure — no mock-fs)

This is the anti-drift guard from design §2.1/§2.2: a curated table of user-facing filter keys (key -> group, infix letter, reader, formatter). At load, assert every key it names is a member of `org-gtd-view-lang--known-filter-keys`. Type candidates come from `--simple-types` + `--complex-types`.

**Step 1: Write the failing tests**

```elisp
(require 'org-gtd-view-manager)

(deftest view-manager-filter-specs/keys-are-known-dsl-keys ()
  "Every key in the filter-spec table is a known DSL filter key."
  (dolist (entry org-gtd-view-manager--filter-specs)
    (assert-true (memq (car entry) org-gtd-view-lang--known-filter-keys)
                 (format "%s not in known-filter-keys" (car entry)))))

(deftest view-manager-filter-specs/excludes-structural-keys ()
  "The table never surfaces reserved structural keys as infixes."
  (dolist (structural '(view-type block-type group-by native filters
                        additional-blocks agenda-span show-habits not-habit
                        group-contexts prefix-format blocks))
    (assert-nil (assq structural org-gtd-view-manager--filter-specs))))

(deftest view-manager-filter-specs/covers-five-groups ()
  "The five handoff groups are all represented."
  (let ((groups (delete-dups
                 (mapcar (lambda (e) (plist-get (cdr e) :group))
                         org-gtd-view-manager--filter-specs))))
    (dolist (g '(type time structural metadata prefix))
      (assert-true (memq g groups) (format "missing group %s" g)))))

(deftest view-manager-filter-specs/type-candidates-from-dsl-constants ()
  "Type candidates come from the DSL simple+complex type constants."
  (let ((cands (org-gtd-view-manager--type-candidates)))
    (assert-true (memq 'next-action cands))
    (assert-true (memq 'stuck-project cands))
    (assert-true (memq 'quick-action cands))   ; handoff omitted these
    (assert-true (memq 'tickler cands))
    (assert-true (memq 'trash cands))))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-filter-specs-test.el` — Expected: FAIL (void).

**Step 3: Minimal implementation**

```elisp
;;;; Filter-spec metadata (the builder's source of truth, per design 2.1)

;; Each entry: (DSL-KEY :group G :key "L" :reader FN :formatter FN)
;; :reader  reads a value interactively (returns the value to store, or nil to unset).
;; :formatter renders a stored value for the badge/summary (a short string).
(defconst org-gtd-view-manager--filter-specs
  '((type          :group type       :key "t"
                   :reader org-gtd-view-manager--read-type
                   :formatter org-gtd-view-manager--fmt-symbol)
    (when          :group time       :key "w"
                   :reader org-gtd-view-manager--read-time
                   :formatter org-gtd-view-manager--fmt-time)
    (deadline      :group time       :key "D"
                   :reader org-gtd-view-manager--read-time
                   :formatter org-gtd-view-manager--fmt-time)
    (scheduled     :group time       :key "C"
                   :reader org-gtd-view-manager--read-time
                   :formatter org-gtd-view-manager--fmt-time)
    (todo          :group structural :key "o"
                   :reader org-gtd-view-manager--read-string
                   :formatter org-gtd-view-manager--fmt-string)
    (done          :group structural :key "O"
                   :reader org-gtd-view-manager--read-time
                   :formatter org-gtd-view-manager--fmt-time)
    (not-done      :group structural :key "N"
                   :reader org-gtd-view-manager--read-flag
                   :formatter org-gtd-view-manager--fmt-flag)
    (area-of-focus :group metadata   :key "A"
                   :reader org-gtd-view-manager--read-area
                   :formatter org-gtd-view-manager--fmt-string)
    (effort        :group metadata   :key "e"
                   :reader org-gtd-view-manager--read-effort
                   :formatter org-gtd-view-manager--fmt-effort)
    (who           :group metadata   :key "W"
                   :reader org-gtd-view-manager--read-string
                   :formatter org-gtd-view-manager--fmt-string)
    (tags          :group metadata   :key "G"
                   :reader org-gtd-view-manager--read-string
                   :formatter org-gtd-view-manager--fmt-string)
    (priority      :group metadata   :key "P"
                   :reader org-gtd-view-manager--read-string
                   :formatter org-gtd-view-manager--fmt-string)
    (prefix        :group prefix     :key "x"
                   :reader org-gtd-view-manager--read-prefix
                   :formatter org-gtd-view-manager--fmt-prefix)
    (prefix-width  :group prefix     :key "X"
                   :reader org-gtd-view-manager--read-width
                   :formatter org-gtd-view-manager--fmt-number))
  "Curated user-facing filter keys the builder exposes, per design 2.1.
Each key MUST be a member of `org-gtd-view-lang--known-filter-keys'
\(asserted at load).  Structural/reserved keys are deliberately excluded.")

(defun org-gtd-view-manager--type-candidates ()
  "Return all selectable type values from the DSL type constants."
  (append org-gtd-view-lang--simple-types
          org-gtd-view-lang--complex-types))

;; Anti-drift guard: fail loudly at load if the curated table names a key the
;; DSL no longer knows about.  Honors "don't modify the DSL" while guaranteeing
;; the builder cannot silently drift.
(let ((unknown (seq-remove
                (lambda (key) (memq key org-gtd-view-lang--known-filter-keys))
                (mapcar #'car org-gtd-view-manager--filter-specs))))
  (when unknown
    (error "org-gtd-view-manager: filter-spec keys not in the DSL: %S" unknown)))
```

The reader/formatter functions are stubbed in later tasks (Task 4 badge formatters, Task 7 readers). For this task, provide trivial placeholder `defun`s so the file loads (e.g. formatters that `format "%s"`); real bodies land in Tasks 4/7. Keep the load-time assertion at the very end of the section.

**Step 4: Run to verify pass** — Expected: PASS (4 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-filter-specs-test.el
git commit -m "feat(view-manager): add filter-spec table with load-time anti-drift guard"
```

---

## Task 4: Badge / summary formatter (spec -> one-line string)

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-badge-test.el` (create; pure)

Produces the one-line badge used in the manager rows and the builder summary, e.g. `next-action · Home · <30m`, `delegated · who=Sam`, `project · not-done`.

**Step 1: Write the failing tests**

```elisp
(require 'org-gtd-view-manager)

(deftest view-manager-badge/next-action-area-effort ()
  (assert-equal "next-action · Home · <30m"
                (org-gtd-view-manager--badge
                 '((name . "Weekend errands") (type . next-action)
                   (area-of-focus . "Home") (effort . (< "30m"))))))

(deftest view-manager-badge/delegated-who ()
  (assert-equal "delegated · who=Sam"
                (org-gtd-view-manager--badge
                 '((name . "x") (type . delegated) (who . "Sam")))))

(deftest view-manager-badge/project-not-done ()
  (assert-equal "project · not-done"
                (org-gtd-view-manager--badge
                 '((name . "x") (type . project) (not-done . t)))))

(deftest view-manager-badge/name-only-is-not-shown ()
  "The name is the row label, not part of the badge."
  (assert-equal "next-action"
                (org-gtd-view-manager--badge
                 '((name . "Anything") (type . next-action)))))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-badge-test.el` — Expected: FAIL.

**Step 3: Minimal implementation** — replace the placeholder formatters from Task 3 with real bodies and add the badge assembler.

```elisp
;;;; Badge / summary

(defun org-gtd-view-manager--fmt-symbol (v) (format "%s" v))
(defun org-gtd-view-manager--fmt-string (v) (format "%s" v))
(defun org-gtd-view-manager--fmt-number (v) (format "%s" v))
(defun org-gtd-view-manager--fmt-flag (_v) nil)         ; label carries the key
(defun org-gtd-view-manager--fmt-time (v) (format "%s" v))
(defun org-gtd-view-manager--fmt-effort (v)
  ;; (< "30m") -> "<30m" ; (> "1h") -> ">1h"
  (if (and (listp v) (= 2 (length v)))
      (format "%s%s" (car v) (cadr v))
    (format "%s" v)))
(defun org-gtd-view-manager--fmt-prefix (v) (format "%s" v))

(defun org-gtd-view-manager--badge (spec)
  "Return a compact one-line summary of SPEC (name excluded)."
  (let (parts)
    (dolist (entry org-gtd-view-manager--filter-specs)
      (let* ((key (car entry))
             (cell (assq key spec)))
        (when cell
          (let* ((val (cdr cell))
                 (fmt (funcall (plist-get (cdr entry) :formatter) val)))
            (push (cond
                   ;; flag keys (not-done): show the key name itself
                   ((eq key 'not-done) "not-done")
                   ;; who/tags/priority read better as key=value
                   ((memq key '(who tags priority)) (format "%s=%s" key fmt))
                   (t fmt))
                  parts)))))
    (string-join (nreverse parts) " · ")))
```

The badge iterates the filter-spec table in declaration order so output ordering is deterministic (type first). Confirm the test expectations match that order.

**Step 4: Run to verify pass** — Expected: PASS (4 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-badge-test.el
git commit -m "feat(view-manager): add spec badge/summary formatter"
```

---

## Task 5: Spec compile (builder infix state -> flat view alist)

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-compile-test.el` (create; pure)

The builder holds infix state as a `key -> value` alist (unset keys absent). Compile normalizes it into a flat view alist that `org-gtd-view-show` accepts: unset keys stay absent (never `nil`), and effort/prefix shapes are correct. Per design §6, keys left unset are simply absent so the DSL applies its own defaults.

**Step 1: Write the failing tests**

```elisp
(require 'org-gtd-view-manager)

(deftest view-manager-compile/omits-unset-keys ()
  "Unset keys are absent from the compiled spec, not nil."
  (let ((spec (org-gtd-view-manager--compile
               '((name . "x") (type . next-action)))))
    (assert-nil (assq 'who spec))
    (assert-nil (assq 'effort spec))
    (assert-equal 'next-action (cdr (assq 'type spec)))))

(deftest view-manager-compile/effort-shape ()
  "Effort compiles to a comparison list like (< \"30m\")."
  (let ((spec (org-gtd-view-manager--compile
               '((name . "x") (type . next-action) (effort . (< "30m"))))))
    (assert-equal '(< "30m") (cdr (assq 'effort spec)))))

(deftest view-manager-compile/prefix-chain-shape ()
  "Prefix compiles to a fallback chain list, not a string."
  (let ((spec (org-gtd-view-manager--compile
               '((name . "x") (type . next-action)
                 (prefix . (project area-of-focus "—"))
                 (prefix-width . 12)))))
    (assert-equal '(project area-of-focus "—") (cdr (assq 'prefix spec)))
    (assert-equal 12 (cdr (assq 'prefix-width spec)))))

(deftest view-manager-compile/drops-nil-values ()
  "A key explicitly set to nil is dropped (treated as unset)."
  (let ((spec (org-gtd-view-manager--compile
               '((name . "x") (type . next-action) (who . nil)))))
    (assert-nil (assq 'who spec))))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el` — Expected: FAIL.

**Step 3: Minimal implementation**

```elisp
;;;; Compile

(defun org-gtd-view-manager--compile (state)
  "Compile builder STATE (a key -> value alist) into a flat view spec.
Keys whose value is nil are omitted so the DSL applies its own
defaults.  `name' and every curated filter key pass through as-is;
values are already in DSL shape (readers produce them)."
  (let ((allowed (cons 'name (mapcar #'car org-gtd-view-manager--filter-specs)))
        result)
    (dolist (cell state)
      (when (and (memq (car cell) allowed)
                 (not (null (cdr cell))))
        (push cell result)))
    (nreverse result)))
```

Values arrive already in DSL shape because the infix readers (Task 7) produce them — the compile step is purely structural (allow-list + drop-nil + order). If a reader ever needs to transform raw input into DSL shape (e.g. effort string `<30m` into `(< "30m")`), that transform lives in the **reader**, not here, so this stays pure and trivially testable.

**Step 4: Run to verify pass** — Expected: PASS (4 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-compile-test.el
git commit -m "feat(view-manager): compile builder state to flat view spec"
```

---

## Task 6: Migration — import & flatten `org-gtd-reflect-missed-custom-views`

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-migration-test.el` (create; mock-fs)

One-time, fail-soft import (design §7). Source entries nest filters under a `filters` key; flatten into top-level keys so the builder can edit them. Skip-and-`message` bad entries (unknown keys); never abort the whole import on one bad entry.

**Step 1: Write the failing tests**

```elisp
(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(e-unit-initialize)
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd (funcall proceed context)))

(deftest view-manager-migration/flattens-nested-filters ()
  "A nested (filters . (...)) entry imports to a flat, editable spec."
  (let ((org-gtd-reflect-missed-custom-views
         '(((name . "My Custom View")
            (filters . ((type . delegated) (area-of-focus . "Work")))))))
    (org-gtd-view-manager--migrate)
    (assert-equal '((name . "My Custom View")
                    (type . delegated)
                    (area-of-focus . "Work"))
                  (org-gtd-view-manager--store-get "My Custom View"))))

(deftest view-manager-migration/already-flat-entry-imports ()
  "An entry already flat imports unchanged."
  (let ((org-gtd-reflect-missed-custom-views
         '(((name . "Flat") (type . next-action)))))
    (org-gtd-view-manager--migrate)
    (assert-equal '((name . "Flat") (type . next-action))
                  (org-gtd-view-manager--store-get "Flat"))))

(deftest view-manager-migration/bad-entry-skipped-not-fatal ()
  "An entry with an unknown key is skipped; good entries still import."
  (let ((org-gtd-reflect-missed-custom-views
         '(((name . "Bad") (filters . ((bogus-key . 1))))
           ((name . "Good") (type . next-action)))))
    (org-gtd-view-manager--migrate)
    (assert-nil (org-gtd-view-manager--store-get "Bad"))
    (assert-equal '((name . "Good") (type . next-action))
                  (org-gtd-view-manager--store-get "Good"))))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-migration-test.el` — Expected: FAIL.

**Step 3: Minimal implementation**

```elisp
;;;; Migration (one-time, fail-soft)

(defun org-gtd-view-manager--flatten-entry (entry)
  "Flatten a legacy ENTRY: hoist any nested `filters' alist to top level.
Return the flat spec, or signal `error' if it names an unknown key."
  (let* ((allowed (cons 'name (mapcar #'car org-gtd-view-manager--filter-specs)))
         (nested (alist-get 'filters entry))
         (flat (append (assq-delete-all 'filters (copy-alist entry)) nested)))
    (dolist (cell flat)
      (unless (memq (car cell) allowed)
        (error "unknown key %s" (car cell))))
    flat))

(defun org-gtd-view-manager--migrate ()
  "Import `org-gtd-reflect-missed-custom-views' into the store, fail-soft.
Flattens nested `filters'; skips and `message's any bad entry."
  (dolist (entry org-gtd-reflect-missed-custom-views)
    (condition-case err
        (let* ((flat (org-gtd-view-manager--flatten-entry entry))
               (name (alist-get 'name flat)))
          (when name
            (org-gtd-view-manager--store-upsert name flat)))
      (error (message "org-gtd: skipped migrating view %S: %s"
                      (alist-get 'name entry)
                      (error-message-string err))))))
```

Migration must be invoked once, guarded, when the manager first opens — wire that in Task 11 (a `org-gtd-view-manager--migrate-once` flag/sentinel), not here. This task only proves the pure flatten + fail-soft behavior.

**Step 4: Run to verify pass** — Expected: PASS (3 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-migration-test.el
git commit -m "feat(view-manager): migrate + flatten legacy custom views"
```

---

## Task 7: Infix readers

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-readers-test.el` (create; pure where possible + `with-simulated-input` for interactive ones)

Replace the Task-3 placeholder readers with real bodies. Each reader returns a DSL-shaped value (or nil to unset). The effort reader is the one with a real transform + fail-soft teaching message (design §8: *"Effort needs a duration like 30m."*). The type reader offers `org-gtd-view-manager--type-candidates`. The prefix reader edits a **list** (chain), never a format string (design §2.3). Area reader completes over `org-gtd-areas-of-focus`.

**Step 1: Write the failing tests** (focus the automatable, pure transforms; interactive `completing-read` readers get thin `with-simulated-input` coverage)

```elisp
(require 'org-gtd-view-manager)

(deftest view-manager-reader/effort-parses-comparison ()
  "A `<30m' entry becomes the DSL shape (< \"30m\")."
  (assert-equal '(< "30m") (org-gtd-view-manager--effort->dsl "<30m"))
  (assert-equal '(> "1h")  (org-gtd-view-manager--effort->dsl ">1h")))

(deftest view-manager-reader/effort-rejects-garbage ()
  "A malformed effort raises the teaching error, not a stack trace."
  (let ((err (should-error (org-gtd-view-manager--effort->dsl "banana")
                           :type 'user-error)))
    (assert-match "duration like 30m" (cadr err))))
```

Note: e-unit exposes `assert-*`; use `should-error`/`condition-case` per the framework's actual API — mirror whatever an existing test in `test/unit/` uses for expected-error assertions (grep `test/unit/` for `should-error` or `condition-case`; if absent, wrap in `condition-case` and `assert-match` on the message). Adjust the test to the house idiom before writing implementation.

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-readers-test.el` — Expected: FAIL.

**Step 3: Minimal implementation**

```elisp
;;;; Infix readers

(defconst org-gtd-view-manager--effort-regexp
  "\\`\\([<>]\\)\\([0-9]+[smhd]\\|[0-9]+:[0-9]+\\)\\'"
  "Matches a comparison effort like `<30m' or `>1:00'.")

(defun org-gtd-view-manager--effort->dsl (input)
  "Turn INPUT (e.g. \"<30m\") into a DSL effort list (< \"30m\").
Fail-soft: a malformed value raises a teaching `user-error'."
  (if (string-match org-gtd-view-manager--effort-regexp input)
      (list (intern (match-string 1 input)) (match-string 2 input))
    (user-error "Effort needs a duration like 30m (e.g. <30m, >1h)")))

(defun org-gtd-view-manager--read-effort (&rest _)
  (org-gtd-view-manager--effort->dsl
   (read-string "Effort (e.g. <30m, >1h): ")))

(defun org-gtd-view-manager--read-type (&rest _)
  (intern (completing-read
           "Type: " (mapcar #'symbol-name (org-gtd-view-manager--type-candidates))
           nil t)))

(defun org-gtd-view-manager--read-time (&rest _)
  ;; past/today/future or a duration/offset regex like <30m, +7d
  (let ((v (read-string "When (past/today/future or <30m, +7d): ")))
    (if (member v '("past" "today" "future")) (intern v) v)))

(defun org-gtd-view-manager--read-string (&rest _)
  (let ((v (read-string "Value: ")))
    (if (string-blank-p v) nil v)))

(defun org-gtd-view-manager--read-area (&rest _)
  (let ((v (completing-read "Area of focus: "
                            (mapcar #'symbol-name org-gtd-areas-of-focus) nil nil)))
    (if (string-blank-p v) nil v)))

(defun org-gtd-view-manager--read-flag (&rest _) t)

(defun org-gtd-view-manager--read-width (&rest _)
  (read-number "Prefix width: "))

(defun org-gtd-view-manager--read-prefix (&rest _)
  ;; Edits a fallback CHAIN (list), not a format string (design 2.3).
  (read (read-string "Prefix chain (e.g. (project area-of-focus \"—\")): "
                     (prin1-to-string org-gtd-view-lang--default-prefix))))
```

`org-gtd-areas-of-focus` lives in `org-gtd-areas-of-focus`; add `(require 'org-gtd-areas-of-focus)` to the module requirements if the area reader needs it. Readers return nil to unset where blank; the compile step (Task 5) drops nils.

**Step 4: Run to verify pass** — Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-readers-test.el
git commit -m "feat(view-manager): add infix readers with fail-soft effort parsing"
```

---

## Task 8: `org-gtd-view-run` — autoloaded completing-read recall

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-run-test.el` (create; mock-fs + `with-simulated-input`)

Autoloaded keyboard recall over saved names, without opening the manager (bind-it-yourself, no default key). Empty store gives a teaching `user-error`, not a stack trace.

**Step 1: Write the failing tests**

```elisp
(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(e-unit-initialize)
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd (funcall proceed context)))

(deftest view-manager-run/renders-selected-view ()
  "Selecting a saved name calls org-gtd-view-show with its spec."
  (org-gtd-view-manager--store-upsert
   "Errands" '((name . "Errands") (type . next-action)))
  (let (captured)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (spec &rest _) (setq captured spec))))
      (with-simulated-input "Errands RET"
        (org-gtd-view-run)))
    (assert-equal '((name . "Errands") (type . next-action)) captured)))

(deftest view-manager-run/empty-store-teaches ()
  "With no saved views, a teaching user-error fires, not a crash."
  (let ((err (should-error (org-gtd-view-run) :type 'user-error)))
    (assert-match "No saved views" (cadr err))))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-run-test.el` — Expected: FAIL.

**Step 3: Minimal implementation**

```elisp
;;;; Recall

;;;###autoload
(defun org-gtd-view-run ()
  "Prompt for a saved view by name and render it via `org-gtd-view-show'."
  (interactive)
  (let ((views (org-gtd-view-manager--store-read)))
    (unless views
      (user-error "No saved views yet — build one with M-x org-gtd-view-manager"))
    (let* ((name (completing-read "View: " (mapcar #'car views) nil t))
           (spec (cdr (assoc name views))))
      (org-gtd-view-show spec))))
```

**Step 4: Run to verify pass** — Expected: PASS (2 tests). Add `(require 'cl-lib)` to the test file if `cl-letf` needs it (mirror an existing test that uses `cl-letf`).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-run-test.el
git commit -m "feat(view-manager): add org-gtd-view-run completing-read recall"
```

---

## Task 9: Builder transient `org-gtd-view-manager--build` (structure + save/abort + window discipline)

> **Transient-heavy — pure TDD is awkward here.** Unit-test what stays pure (already covered: compile, badge, readers). This task adds a **thin integration test** that the prefix is defined and its infixes/footer keys are wired, plus **manual verification steps**. Do not fake a unit test for the interactive flow.

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-build-test.el` (create; thin, uses `ogt--transient-suffix-plist`)

The builder is a `transient-define-prefix` with the five groups from §2.1 generated from `org-gtd-view-manager--filter-specs`, a summary line echoing the badge, and footer keys `RET` Preview / `s` Save / `C-c C-k` Abort. Snapshot the window config on entry; restore on every exit path (save/abort). Builder state is held in a buffer-local/prefix-scope variable seeded from the starting spec (fresh `((name . "Untitled") (type . next-action))` for create, the stored spec for edit).

**Step 1: Write the thin integration tests**

```elisp
(require 'e-unit)
(require 'org-gtd-view-manager)
(e-unit-initialize)

(deftest view-manager-build/is-a-transient-prefix ()
  (assert-true (fboundp 'org-gtd-view-manager--build)))

(deftest view-manager-build/has-save-suffix ()
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager--build "s")))
    (assert-equal "s" (plist-get plist :key))))

(deftest view-manager-build/has-type-infix ()
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager--build "t")))
    (assert-equal "t" (plist-get plist :key))))
```

Load `ogt--transient-suffix-plist` via `(require 'org-gtd-test-helper-utils "test/helpers/utils.el")` as `command-center-test.el` does.

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el` — Expected: FAIL.

**Step 3: Minimal implementation** (sketch — the executing engineer fills the transient body against transient.el docs)

```elisp
;;;; Builder transient

(defvar org-gtd-view-manager--build-state nil
  "Alist of the view spec being built (key -> value).")
(defvar org-gtd-view-manager--build-window-config nil
  "Window configuration to restore when the builder exits.")
(defvar org-gtd-view-manager--build-dirty nil
  "Non-nil when the builder has unsaved changes.")

(defun org-gtd-view-manager--build-summary ()
  "Return the summary line for the builder header."
  (concat "View: " (or (alist-get 'name org-gtd-view-manager--build-state) "Untitled")
          "  —  " (org-gtd-view-manager--badge
                   (org-gtd-view-manager--compile org-gtd-view-manager--build-state))))

;; Generate one infix command per filter-spec entry.  Each sets the key in
;; --build-state, marks dirty, and triggers a (debounced) preview (Task 10).
(defmacro org-gtd-view-manager--definfix (key group letter reader)
  "Define a transient infix command for filter KEY." ...)

;; ... iterate org-gtd-view-manager--filter-specs to define the infixes, or
;; define them explicitly grouped by :group.  Keep the mapping DRY: the letters
;; and readers come from the table, never hand-duplicated.

(transient-define-prefix org-gtd-view-manager--build (&optional starting-spec)
  "Build or edit a GTD view spec with a live preview."
  [:description org-gtd-view-manager--build-summary
   ["Type"       (org-gtd-view-manager--infix-type)]
   ["Time"       (org-gtd-view-manager--infix-when)
                 (org-gtd-view-manager--infix-deadline)
                 (org-gtd-view-manager--infix-scheduled)]
   ["Structural" (org-gtd-view-manager--infix-todo)
                 (org-gtd-view-manager--infix-done)
                 (org-gtd-view-manager--infix-not-done)]
   ["Metadata"   (org-gtd-view-manager--infix-area-of-focus)
                 (org-gtd-view-manager--infix-effort)
                 (org-gtd-view-manager--infix-who)
                 (org-gtd-view-manager--infix-tags)
                 (org-gtd-view-manager--infix-priority)]
   ["Prefix"     (org-gtd-view-manager--infix-prefix)
                 (org-gtd-view-manager--infix-prefix-width)]]
  [["Actions"
    ("RET" "Preview now" org-gtd-view-manager--preview)
    ("s"   "Save"        org-gtd-view-manager--save)
    ("C-c C-k" "Abort"   org-gtd-view-manager--abort)]]
  (interactive)
  (setq org-gtd-view-manager--build-window-config (current-window-configuration)
        org-gtd-view-manager--build-state (copy-alist
                                           (or starting-spec
                                               '((name . "Untitled")
                                                 (type . next-action))))
        org-gtd-view-manager--build-dirty nil)
  (transient-setup 'org-gtd-view-manager--build))

(defun org-gtd-view-manager--save ()
  "Save the current build state to the store, then restore windows."
  (interactive)
  (let* ((name (read-string "Name this view: "
                            (alist-get 'name org-gtd-view-manager--build-state)))
         (spec (cons (cons 'name name)
                     (assq-delete-all 'name
                       (org-gtd-view-manager--compile
                        org-gtd-view-manager--build-state)))))
    (when (and (org-gtd-view-manager--store-get name)
               (not (y-or-n-p (format "A view named '%s' exists — overwrite? " name))))
      (user-error "Save cancelled"))
    (org-gtd-view-manager--store-upsert name spec)
    (setq org-gtd-view-manager--build-dirty nil)
    (org-gtd-view-manager--build-restore-windows)))

(defun org-gtd-view-manager--abort ()
  "Discard the build (guarding if dirty) and restore windows."
  (interactive)
  (when (or (not org-gtd-view-manager--build-dirty)
            (y-or-n-p "Discard unsaved view? "))
    (org-gtd-view-manager--build-restore-windows)))

(defun org-gtd-view-manager--build-restore-windows ()
  "Restore the window configuration snapshotted on builder entry."
  (when org-gtd-view-manager--build-window-config
    (set-window-configuration org-gtd-view-manager--build-window-config)))
```

Keep the infix definitions generated from the filter-spec table (DRY — the primer's "generate the transient from the registry" move). Consult transient.el for the correct `transient-define-infix` / dynamic-description API; the exact form is the engineer's to finalize against the installed transient version (>= 0.3.7).

**Step 4: Run to verify pass** — Expected: PASS (3 thin tests).

**Manual verification (record in the commit body):**
1. `M-x org-gtd-view-manager--build` — the five groups render; summary line shows `Untitled — next-action`.
2. Set type to `delegated`, set who — summary updates; badge reads `delegated · who=…`.
3. `s` prompts for a name, writes to `views.eld`, restores the prior window layout.
4. `C-c C-k` after a change prompts "Discard unsaved view?"; answering yes restores windows.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-build-test.el
git commit -m "feat(view-manager): add builder transient with save/abort + window discipline"
```

---

## Task 10: Live preview loop (debounced, skip-if-unchanged)

> **Transient-heavy / timer-driven — thin test + manual verification.** Unit-test the pure "should we re-render?" decision (spec-changed check); the timer wiring gets manual steps.

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-preview-test.el` (create; pure for the changed-check)

Auto-refresh calls `org-gtd-view-show` on the current compiled spec, **debounced ~250ms** (idle timer) and **skipped when the compiled spec is unchanged** (cache the last compiled spec). Explicit `RET` "Preview now" bypasses the debounce. `org-gtd-view-show` errors are caught and surfaced as a one-line teaching message in the preview pane (design §8), never a stack trace.

**Step 1: Write the failing test (pure changed-check)**

```elisp
(require 'org-gtd-view-manager)

(deftest view-manager-preview/skips-when-spec-unchanged ()
  "The changed-check returns nil when the compiled spec equals the cache."
  (let ((org-gtd-view-manager--preview-last '((name . "x") (type . next-action))))
    (assert-nil (org-gtd-view-manager--preview-changed-p
                 '((name . "x") (type . next-action))))))

(deftest view-manager-preview/detects-change ()
  "The changed-check returns non-nil when the compiled spec differs."
  (let ((org-gtd-view-manager--preview-last '((name . "x") (type . next-action))))
    (assert-true (org-gtd-view-manager--preview-changed-p
                  '((name . "x") (type . delegated))))))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el` — Expected: FAIL.

**Step 3: Minimal implementation**

```elisp
;;;; Live preview

(defvar org-gtd-view-manager--preview-last nil
  "Last compiled spec rendered in the preview, to skip redundant renders.")
(defvar org-gtd-view-manager--preview-timer nil
  "Idle timer for debounced preview refresh.")
(defconst org-gtd-view-manager--preview-delay 0.25
  "Debounce delay (seconds) before auto-refreshing the preview.")

(defun org-gtd-view-manager--preview-changed-p (spec)
  "Return non-nil if SPEC differs from the last previewed spec."
  (not (equal spec org-gtd-view-manager--preview-last)))

(defun org-gtd-view-manager--preview-now ()
  "Render the current build state immediately, fail-soft."
  (let ((spec (org-gtd-view-manager--compile org-gtd-view-manager--build-state)))
    (when (org-gtd-view-manager--preview-changed-p spec)
      (setq org-gtd-view-manager--preview-last spec)
      (condition-case err
          (org-gtd-view-manager--render-preview spec)
        (error (message "org-gtd view preview: %s"
                        (error-message-string err)))))))

(defun org-gtd-view-manager--preview (&rest _)
  "Explicit `RET' preview — bypass the debounce."
  (interactive)
  (org-gtd-view-manager--preview-now))

(defun org-gtd-view-manager--preview-schedule ()
  "Schedule a debounced preview refresh (called from infixes)."
  (when (timerp org-gtd-view-manager--preview-timer)
    (cancel-timer org-gtd-view-manager--preview-timer))
  (setq org-gtd-view-manager--preview-timer
        (run-with-idle-timer org-gtd-view-manager--preview-delay nil
                             #'org-gtd-view-manager--preview-now)))
```

`org-gtd-view-manager--render-preview` wraps `org-gtd-view-show` (over the real `org-agenda-files`) and applies the sample-data path from Task 11. Wire `--preview-schedule` into each infix's set action (from Task 9). Cancel the timer on builder exit (save/abort) so it does not fire into a restored layout.

**Step 4: Run to verify pass** — Expected: PASS (2 tests).

**Manual verification:** open the builder, change infixes rapidly — the preview pane refreshes at most ~4×/sec and does not re-render on a no-op toggle; `RET` forces an immediate render.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-preview-test.el
git commit -m "feat(view-manager): add debounced skip-if-unchanged live preview"
```

---

## Task 11: Empty-agenda -> sample-data preview (+ migrate-once wiring)

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-sample-test.el` (create; mock-fs)

When `org-agenda-files` is empty, `let`-bind a tiny temp org file (a few representative headings across types/areas) into `org-agenda-files` **for the render only**, with the banner *"sample data · your agenda-files are empty — previewing org-gtd's built-in set."* **Fallback** (if sample data is cut): the self-teaching message *"No matching items. org-agenda-files is empty…"* — not an error. Also add the guarded once-only migration trigger (Task 6 provided the pure migrate; the manager opener calls `--migrate-once`).

**Step 1: Write the failing tests**

```elisp
(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(e-unit-initialize)
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd (funcall proceed context)))

(deftest view-manager-sample/uses-sample-when-agenda-empty ()
  "With empty org-agenda-files, the preview binds the sample file."
  (let ((org-agenda-files nil) captured)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (&rest _) (setq captured org-agenda-files))))
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action))))
    (assert-true (and captured (cl-every #'stringp captured)))))

(deftest view-manager-sample/keeps-real-files-when-present ()
  "With real agenda files, the preview does not swap in the sample."
  (let ((org-agenda-files (list (org-gtd--path org-gtd-default-file-name)))
        captured)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (&rest _) (setq captured org-agenda-files))))
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action))))
    (assert-equal (list (org-gtd--path org-gtd-default-file-name)) captured)))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-sample-test.el` — Expected: FAIL.

**Step 3: Minimal implementation**

```elisp
;;;; Sample-data preview

(defconst org-gtd-view-manager--sample-contents
  "\
* NEXT Buy stamps :errand:
:PROPERTIES:
:ORG_GTD: Actions
:CATEGORY: Home
:END:
* NEXT Draft quarterly plan
:PROPERTIES:
:ORG_GTD: Actions
:CATEGORY: Work
:END:
* WAIT Response from Sam
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Sam
:END:
"
  "A tiny representative dataset for previews when agenda-files are empty.")

(defun org-gtd-view-manager--sample-file ()
  "Return a path to a temp org file holding the sample dataset."
  (let ((path (f-join temporary-file-directory "org-gtd-view-sample.org")))
    (unless (f-exists-p path) (f-write-text org-gtd-view-manager--sample-contents 'utf-8 path))
    path))

(defun org-gtd-view-manager--render-preview (spec)
  "Render SPEC via `org-gtd-view-show', using sample data if needed."
  (if org-agenda-files
      (org-gtd-view-show spec)
    (let ((org-agenda-files (list (org-gtd-view-manager--sample-file))))
      (message "sample data · your agenda-files are empty — previewing org-gtd's built-in set")
      (org-gtd-view-show spec))))

;;;; Migration trigger

(defvar org-gtd-view-manager--migrated nil
  "Non-nil once legacy custom views have been imported this session.")

(defun org-gtd-view-manager--migrate-once ()
  "Run the one-time legacy import, guarded by a session flag."
  (unless org-gtd-view-manager--migrated
    (org-gtd-view-manager--migrate)
    (setq org-gtd-view-manager--migrated t)))
```

The banner is a `message` for v1 (the design allows the fallback teaching message if sample data is deferred; the sample path here is the fuller option). If rendering the banner *inside* the agenda buffer proves cheap, prefer that; otherwise the echo-area `message` satisfies the fail-soft teaching-voice requirement.

**Step 4: Run to verify pass** — Expected: PASS (2 tests).

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-sample-test.el
git commit -m "feat(view-manager): sample-data preview for empty agenda + migrate-once"
```

---

## Task 12: List transient `org-gtd-view-manager` (browse + CRUD dispatch)

> **Transient-heavy — thin test + manual verification.** Highlight-model and rendering are interactive; test that the prefix exists and the CRUD keys are wired.

**Files:**
- Modify: `org-gtd-view-manager.el`
- Test: `test/unit/view-manager-list-test.el` (create; thin)

The flat "Your saved views" list (moment-grouping deferred per design). Keys: `RET` render highlighted via `org-gtd-view-show`, `c` create (fresh spec), `e` edit (stored spec), `d` duplicate (`"Name copy"`) then edit, `D` delete (confirm `y/n`), `↑`/`↓` move highlight, `q` quit + restore windows. Empty/first-run state: a teaching line, never a blank pane. Snapshot + restore window config. Call `--migrate-once` on open.

**Step 1: Write the thin integration tests**

```elisp
(require 'e-unit)
(require 'org-gtd-view-manager)
(require 'org-gtd-test-helper-utils "test/helpers/utils.el")
(e-unit-initialize)

(deftest view-manager-list/is-a-transient-prefix ()
  (assert-true (fboundp 'org-gtd-view-manager)))

(deftest view-manager-list/has-create-key ()
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager "c")))
    (assert-equal "c" (plist-get plist :key))))

(deftest view-manager-list/has-delete-key ()
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager "D")))
    (assert-equal "D" (plist-get plist :key))))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el` — Expected: FAIL.

**Step 3: Minimal implementation** (sketch — engineer finalizes the highlight/redisplay model against transient.el)

```elisp
;;;; List transient

(defvar org-gtd-view-manager--highlight 0
  "Index of the highlighted saved view in the list.")
(defvar org-gtd-view-manager--list-window-config nil)

(defun org-gtd-view-manager--rows ()
  "Return a description string for the saved views (teaching line if empty)."
  (let ((views (org-gtd-view-manager--store-read)))
    (if (null views)
        "No saved views yet. Press c to build one, or RET a built-in like Engage."
      (string-join
       (seq-map-indexed
        (lambda (v i)
          (format "%s %-24s %s"
                  (if (= i org-gtd-view-manager--highlight) "▸" " ")
                  (car v)
                  (org-gtd-view-manager--badge (cdr v))))
        views)
       "\n"))))

;;;###autoload (autoload 'org-gtd-view-manager "org-gtd-view-manager" nil t)
(transient-define-prefix org-gtd-view-manager ()
  "Browse and manage saved GTD views."
  [:description org-gtd-view-manager--rows
   [("RET" "Render"    org-gtd-view-manager--list-render)
    ("c"   "Create"    org-gtd-view-manager--list-create)
    ("e"   "Edit"      org-gtd-view-manager--list-edit)
    ("d"   "Duplicate" org-gtd-view-manager--list-duplicate)
    ("D"   "Delete"    org-gtd-view-manager--list-delete)
    ("<up>"   "Up"     org-gtd-view-manager--list-up   :transient t)
    ("<down>" "Down"   org-gtd-view-manager--list-down :transient t)
    ("q"   "Quit"      org-gtd-view-manager--list-quit)]]
  (interactive)
  (setq org-gtd-view-manager--list-window-config (current-window-configuration)
        org-gtd-view-manager--highlight 0)
  (org-gtd-view-manager--migrate-once)
  (transient-setup 'org-gtd-view-manager))

;; --list-render/-create/-edit/-duplicate/-delete/-up/-down/-quit:
;;   operate on the highlighted view; create/edit call org-gtd-view-manager--build;
;;   duplicate stores "<name> copy" then edits; delete confirms with y-or-n-p;
;;   quit restores --list-window-config.
```

The `<up>`/`<down>` movers adjust `--highlight` (clamped) and stay in the transient (`:transient t`) so redisplay reflects the new highlight. `--list-render` calls `org-gtd-view-manager--render-preview` (or `org-gtd-view-show`) on the highlighted stored spec. **When the store is empty, `RET` calls the existing `org-gtd-engage` command** (Open Questions §2, resolved) — built-ins stay commands, not stored specs, so nothing is seeded into the store.

**Step 4: Run to verify pass** — Expected: PASS (3 thin tests).

**Manual verification:**
1. `M-x org-gtd-view-manager` with an empty store — the teaching line shows, no blank pane.
2. `c` opens the builder; save; back in the manager the new view appears with its badge.
3. `↑`/`↓` move the `▸` highlight; `RET` renders the highlighted view; `D` confirms then removes it; `q` restores the prior window layout.

**Step 5: Commit**

```bash
git add org-gtd-view-manager.el test/unit/view-manager-list-test.el
git commit -m "feat(view-manager): add list transient with CRUD dispatch"
```

---

## Task 13: Wire discovery — command-center `v` entry + deprecate legacy defcustom

**Files:**
- Modify: `org-gtd-command-center.el` (Engage column, :46-49; add `(require 'org-gtd-view-manager)` near :38)
- Modify: `org-gtd-reflect.el` (:296 — mark `org-gtd-reflect-missed-custom-views` obsolete post-migration)
- Test: `test/unit/command-center-test.el` (add one deftest)

Add `("v" "Views…" org-gtd-view-manager)` to the Engage column (`v` is free — Engage uses `e`/`@`/`n`). Deprecate the legacy defcustom now that migration imports it.

**Step 1: Write the failing test** (append to `command-center-test.el`)

```elisp
(deftest command-center-has-views-entry ()
  "The Engage group binds v to the view manager."
  (assert-true (fboundp 'org-gtd-view-manager))
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-command-center "v")))
    (assert-equal "v" (plist-get plist :key))
    (assert-equal 'org-gtd-view-manager (plist-get plist :command))))
```

**Step 2: Run to verify failure** — Run: `.claude/skills/test/run-tests.sh test/unit/command-center-test.el` — Expected: FAIL (no `v` suffix).

**Step 3: Minimal implementation**

In `org-gtd-command-center.el`, add the require and the Engage entry:

```elisp
(require 'org-gtd-view-manager)   ; near the other requires
```
```elisp
  [["Engage"
    ("e" "Daily view" org-gtd-engage)
    ("@" "By context" org-gtd-engage-grouped-by-context)
    ("n" "All next actions" org-gtd-show-all-next)
    ("v" "Views…" org-gtd-view-manager)]
```

In `org-gtd-reflect.el`, after the defcustom (:307), mark it obsolete (migration now owns import):

```elisp
(make-obsolete-variable 'org-gtd-reflect-missed-custom-views
                        "migrate to the View Manager (M-x org-gtd-view-manager)" "5.0")
```

Keep `org-gtd-reflect-missed-with-custom` reading the variable for backward compatibility; the obsolescence is advisory, not a removal.

**Step 4: Run to verify pass** — Run: `.claude/skills/test/run-tests.sh test/unit/command-center-test.el` — Expected: PASS.

**Step 5: Commit**

```bash
git add org-gtd-command-center.el org-gtd-reflect.el test/unit/command-center-test.el
git commit -m "feat(view-manager): wire command-center v entry; deprecate legacy custom-views"
```

---

## Task 14: Full-suite regression + docs/changelog stubs

**Files:**
- Modify: `CHANGELOG.org` (add an entry)
- Modify: `doc/org-gtd.org` (add a short View Manager section — source; do NOT hand-edit the generated `.info`)

**Step 1: Run the relevant new test files to confirm all green**

```bash
.claude/skills/test/run-tests.sh test/unit/view-manager-store-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-filter-specs-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-badge-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-compile-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-migration-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-readers-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-run-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-build-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-preview-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-sample-test.el
.claude/skills/test/run-tests.sh test/unit/view-manager-list-test.el
.claude/skills/test/run-tests.sh test/unit/command-center-test.el
```

(The runner supports single-file granularity only — run each file.) Also run the DSL and files suites to confirm no regression: `test/unit/gtd-view-language-test.el`, `test/unit/files-test.el`.

**Step 2: Add the CHANGELOG.org entry** (hand-written; a new Unreleased/x.x.x bullet)

```
- New =View Manager= (=M-x org-gtd-view-manager=, or =v= in the command center):
  create, live-preview, name, save, recall, edit, and delete custom GTD views
  without writing elisp.  Views persist to =views.eld= in your =org-gtd-directory=.
  Existing =org-gtd-reflect-missed-custom-views= entries are imported automatically.
```

**Step 3: Add a short section to `doc/org-gtd.org`** describing the manager, the builder, `org-gtd-view-run`, and the `views.eld` store. Source only.

**Step 4: Commit the hand-written docs/changelog** (source), separately from any generated artifacts

```bash
git add CHANGELOG.org doc/org-gtd.org
git commit -m "docs(view-manager): document the view manager and views.eld store"
```

**Step 5: Regenerate `org-gtd.info` + autoloads in a SEPARATE commit** (project convention — regenerated text is committed on its own; see memory "Split generated text into its own commit"):

```bash
~/bin/eldev build   # or the project's info/autoload build step
git add org-gtd.info org-gtd-autoloads.el
git commit -m "chore: regenerate info manual and autoloads for view manager"
```

---

## Open questions (design was silent — resolve before/at these tasks; do NOT invent an answer)

1. **RESOLVED — do not use `org-gtd--ensure-file-exists` for `views.eld` (Task 1).** Verified: it calls `org-gtd-core-prepare-buffer` → `org-mode-restart` (`org-gtd-core.el:557`), forcing the buffer into org-mode — wrong for a `.eld` file. **Task 1 writes the store directly** (a `;;`-comment guidance header + the printed alist, via `f-write-text` / a plain buffer save), created lazily on first access. Design §1/§7 updated to match.

2. **RESOLVED — empty-state `RET` invokes `org-gtd-engage` (Tasks 11/12).** Built-ins stay commands, not stored specs (§12 defers folding built-ins into the store), so there is no pseudo-view to seed, delete, or edit. `--list-render` operates on stored specs only; the empty state special-cases `RET` to call the existing `org-gtd-engage` command. Teaching line: *"No saved views yet. Press `c` to build one, or `RET` to open Engage."* Design §3 updated to match.

3. **Infix letter for `prefix-width` (Task 3).** Design §2.1 lists `x → prefix` but only "(width) → prefix-width" with no letter. The plan chose `X` (shift of `x`, staying in the Prefix group). Confirm `X` does not collide with any planned builder footer key and reads sensibly.

4. **Effort/time input grammar (Task 7).** The design gives examples (`<30m`, `+7d`, `past/today/future`) but no exhaustive grammar. The plan's `effort->dsl` regex accepts `[<>]` + `NNu`/`NN:NN`; the time reader passes duration/offset strings through verbatim and only interns the three literals. Confirm this matches what `org-gtd-view-show` actually parses for `when`/`deadline`/`scheduled`/`effort` (check the DSL's own duration-regex input) so the builder never emits a shape the DSL rejects.

5. **e-unit expected-error idiom (Task 7/8).** The plan uses `should-error :type 'user-error` provisionally. Confirm e-unit's actual API for asserting a raised `user-error` (grep `test/unit/` for the house pattern) and adjust the reader/run tests to it before implementing.

6. **`TAB` engage/reflect toggle + moment-grouping are explicitly deferred** (design Key-decisions + §12) — not built here. Noted so no one adds them speculatively.
