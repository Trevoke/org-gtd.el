# Walk Resume Identity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Split the walk engine's `:scope` (which today serves both the concurrency lock and the resume-checkpoint filename) so that resume checkpoints are keyed by a new `:resume-key` naming the *selection*, and make `org-gtd-someday-review` resumable per someday list.

**Architecture:** `:scope` keeps ONE job — the coarse concurrency lock (file set). A new optional spec field `:resume-key` names the fine-grained selection; `org-gtd-walk-start` derives the checkpoint filename from `(or (plist-get spec :resume-key) (plist-get spec :name))`. someday-review's standalone entry point sets `:resumable t` + `:resume-key` (from its list filter) on the *freshly built* spec, while the *registered template* stays `:resumable nil` so a review-step-hosted walk delegates persistence to the review session (no double-persist). No new engine lifecycle code — the checkpoint save/load/quit/finish machinery already exists and works.

**Tech Stack:** Emacs Lisp, e-unit test framework (`deftest`/`assert-*`/`around-each`), Eldev. Design: `docs/plans/2026-07-22-walk-resume-identity-design.md`.

**Testing note:** Per project convention, run tests with the `/test` skill (Skill tool) or, for an executing subagent, its script directly: `.claude/skills/test/run-tests.sh test/unit/<file>-test.el`. Never call `eldev etest` directly. A single-file run prints `PASS: N tests in Xs` or the failing test detail.

---

## Preflight (read before Task 1)

Current relevant code:

- `org-gtd-walk.el:110-116` — `org-gtd-walk--checkpoint-path (name scope)`; body is
  `(format "walk-%s-%s.eld" name (md5 (org-gtd-walk--scope-key scope)))`.
- `org-gtd-walk.el:204-206` — the only production caller, inside `org-gtd-walk-start`:
  `(path (and (plist-get spec :resumable) (org-gtd-walk--checkpoint-path name scope)))`.
- `org-gtd-walk.el:78-106` — `--scope-key` / `--lock-scope` / `--scope-locked-p` (the lock;
  **do not touch** — it stays keyed on `:scope`).
- `org-gtd-walk.el:38-43` and `:40` — the spec plist key enumeration in docstrings.
- `org-gtd-someday-review.el:189-198` — `--spec` template (no `:resumable` → nil).
- `org-gtd-someday-review.el:232-252` — `org-gtd-reflect-someday-review` entry point; already
  rebuilds the spec and `plist-put`s `:find` and `:scope`.
- `org-gtd-someday-review.el:132-140` — `--item-matches-filter-p`: `list-filter` is `nil`
  (all), the symbol `unassigned`, or a list-name string. `:resume-key` derives from the same.

Key fact about the fallback: because `--checkpoint-path` md5's `(format "%s" X)`, passing a
string `"stub-scope"` and passing the symbol `'stub` produce **different** files. Existing
driver checkpoint tests hardcode `(org-gtd-walk--checkpoint-path 'stub "stub-scope")`; Task 1
Step 1 keeps them green by giving the test helper a default `:resume-key "stub-scope"`.

Already-covered behavior — **do not re-add**: corrupt-checkpoint→fresh-start is tested at
`test/unit/walk-driver-test.el:247` and `test/unit/walk-scope-test.el:103`; engine resume
from a valid checkpoint at `test/unit/walk-driver-test.el:270`.

---

## Task 1: Engine — checkpoint identity from `:resume-key`, scope stays lock-only

**Files:**
- Modify: `org-gtd-walk.el:110-116` (`--checkpoint-path`), `org-gtd-walk.el:204-206`
  (call site), and the spec docstrings at `org-gtd-walk.el:38-43`.
- Modify (test helper + new tests): `test/unit/walk-driver-test.el:40-55` and its
  `;;; checkpointing` section.
- Modify (rename one test): `test/unit/walk-scope-test.el:72-80`.

**Step 1: Keep existing driver checkpoint tests green by defaulting the helper's resume-key**

In `test/unit/walk-driver-test.el`, add a `:resume-key` default to the stub spec so the five
existing `:resumable t` tests (which expect the checkpoint at
`(org-gtd-walk--checkpoint-path 'stub "stub-scope")`) keep matching after the engine keys on
resume-key. Edit `walk-driver-test--stub-spec` (line 42-52), adding one line to the plist:

```elisp
  (let ((spec (list :name 'stub
                    :find (lambda () (list "a" "b" "c"))
                    :render (lambda (handle _surface)
                              (push handle walk-driver-test--render-log))
                    :actions nil
                    :on-finish (lambda ()
                                 (setq walk-driver-test--finish-count
                                       (1+ walk-driver-test--finish-count)))
                    :resumable nil
                    :resolve nil
                    :resume-key "stub-scope"
                    :scope "stub-scope")))
```

**Step 2: Write the failing tests (new resume-key semantics)**

Append to the `;;; checkpointing` section of `test/unit/walk-driver-test.el`:

```elisp
(deftest walk-checkpoint-keys-on-resume-key-not-scope ()
  "Two specs sharing a :scope but differing in :resume-key checkpoint to different files."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start
     (walk-driver-test--stub-spec :resumable t :resume-key "list-a") surface)
    (with-current-buffer surface (org-gtd-walk-quit))
    (let ((surface-b (generate-new-buffer " *walk-test-b*")))
      (unwind-protect
          (progn
            (org-gtd-walk-start
             (walk-driver-test--stub-spec :resumable t :resume-key "list-b") surface-b)
            (assert-true (file-exists-p (org-gtd-walk--checkpoint-path 'stub "list-a")))
            (assert-true (file-exists-p (org-gtd-walk--checkpoint-path 'stub "list-b")))
            (assert-not-equal (org-gtd-walk--checkpoint-path 'stub "list-a")
                              (org-gtd-walk--checkpoint-path 'stub "list-b")))
        (when (buffer-live-p surface-b) (kill-buffer surface-b))))))

(deftest walk-resume-key-absent-falls-back-to-name ()
  "A resumable spec with no :resume-key keys its checkpoint on the walk :name."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start
     (walk-driver-test--stub-spec :resumable t :resume-key nil) surface)
    (assert-true (file-exists-p (org-gtd-walk--checkpoint-path 'stub 'stub)))))

(deftest walk-scope-lock-ignores-resume-key ()
  "The lock keys on :scope only: a second walk with a different :resume-key but the
same :scope is still refused."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start
     (walk-driver-test--stub-spec :resumable t :resume-key "list-a") surface)
    (assert-true (org-gtd-walk--scope-locked-p "stub-scope"))
    (let ((refused nil)
          (surface-b (generate-new-buffer " *walk-test-b*")))
      (unwind-protect
          (condition-case _err
              (org-gtd-walk-start
               (walk-driver-test--stub-spec :resumable t :resume-key "list-b") surface-b)
            (error (setq refused t)))
        (when (buffer-live-p surface-b) (kill-buffer surface-b)))
      (assert-true refused))))
```

**Step 3: Run the tests — verify they FAIL**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: `walk-resume-key-absent-falls-back-to-name` FAILS — today the checkpoint is written
at the scope path `(--checkpoint-path 'stub "stub-scope")`, so the file at
`(--checkpoint-path 'stub 'stub)` does not exist. (The other two new tests may pass
incidentally since `:resume-key` is currently ignored and the scope happens to differ; the
fallback test is the one that pins the new behavior.)

**Step 4: Implement — key the checkpoint on resume-key with a name fallback**

In `org-gtd-walk.el`, replace `--checkpoint-path` (lines 110-116):

```elisp
(defun org-gtd-walk--checkpoint-path (name resume-key)
  "Return the checkpoint file path for walk NAME under RESUME-KEY.
RESUME-KEY identifies the *selection* being walked (e.g. a someday
list), so two selections of the same walk get independent checkpoints
even when they share a `:scope'.  `org-gtd-walk-start' passes NAME
itself as RESUME-KEY when the spec sets no `:resume-key' (one
checkpoint per walk).  See design
docs/plans/2026-07-22-walk-resume-identity-design.md §3.2."
  (expand-file-name
   (format "walk-%s-%s.eld" name (md5 (format "%s" resume-key)))
   org-gtd-directory))
```

In `org-gtd-walk-start`, change the `path` binding (line 205-206) to derive the resume-key:

```elisp
           (path (and (plist-get spec :resumable)
                      (org-gtd-walk--checkpoint-path
                       name (or (plist-get spec :resume-key) name))))
```

Update the spec-key enumeration docstring at `org-gtd-walk.el:40` (and the `org-gtd-walks`
defvar docstring at lines 38-43 if it lists keys) to include `:resume-key`, e.g.:
`SPEC is a plist (:name :find :render :actions :on-finish :resumable :resolve :scope
:resume-key ...)`, with a one-line note: "`:resume-key` (optional) — selection identity for
the resume checkpoint; defaults to `:name`."

**Step 5: Run the tests — verify they PASS**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-driver-test.el`
Expected: all pass, including the three new tests and the five pre-existing checkpoint tests
(kept green by Step 1's helper default).

**Step 6: Rename the now-misleading scope test**

In `test/unit/walk-scope-test.el`, the test at line 72 asserts checkpoint-path behavior but
its name/docstring say "scope". Rename it and update the docstring (the assertions are
unchanged — they already exercise the second argument generically):

```elisp
(deftest checkpoint-path-keys-on-name-and-resume-key ()
  "Different name or resume-key yields a different checkpoint path; same inputs match."
  (let ((org-gtd-directory "/tmp/gtd/"))
    (assert-not-equal (org-gtd-walk--checkpoint-path 'a "s")
                      (org-gtd-walk--checkpoint-path 'b "s"))
    (assert-not-equal (org-gtd-walk--checkpoint-path 'a "s")
                      (org-gtd-walk--checkpoint-path 'a "t"))
    (assert-equal (org-gtd-walk--checkpoint-path 'a "s")
                  (org-gtd-walk--checkpoint-path 'a "s"))))
```

**Step 7: Run the scope-test file — verify PASS**

Run: `.claude/skills/test/run-tests.sh test/unit/walk-scope-test.el`
Expected: all pass.

**Step 8: Commit**

```bash
git add org-gtd-walk.el test/unit/walk-driver-test.el test/unit/walk-scope-test.el
git commit -m "feat(walk): key resume checkpoints on :resume-key, not :scope

:scope now serves only the concurrency lock; a new optional :resume-key
names the selection and drives the checkpoint filename (falling back to
:name). Lets one walk keep independent resume points per selection.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 2: Make `org-gtd-someday-review` resumable per list

**Files:**
- Modify: `org-gtd-someday-review.el` — add `--resume-key`; set `:resumable t` + `:resume-key`
  in `org-gtd-reflect-someday-review` (lines 232-252). Leave `--spec` (189-198) untouched
  (template stays `:resumable nil`).
- Test: `test/unit/someday-review-test.el`.

**Step 1: Write the failing tests**

Append to `test/unit/someday-review-test.el` (these run under the file's existing
`around-each` → `ogt-eunit-with-mock-gtd`, and use `org-gtd-someday-create` + the walk API
the way the existing `someday-review/defer-*` tests do):

```elisp
;;; Resume Tests

(deftest someday-review/resume-key-encodes-selection ()
  "The resume-key maps each list-filter form to a stable, distinct string."
  (assert-equal "all" (org-gtd-someday-review--resume-key nil))
  (assert-equal "unassigned" (org-gtd-someday-review--resume-key 'unassigned))
  (assert-equal "Work" (org-gtd-someday-review--resume-key "Work")))

(deftest someday-review/registered-template-is-not-resumable ()
  "The registry template stays :resumable nil so a hosted walk delegates persistence."
  (assert-nil (plist-get (org-gtd-walk-get 'someday-review) :resumable)))

(deftest someday-review/quitting-mid-review-then-restart-resumes-at-cursor ()
  "Quit keeps a checkpoint keyed by the selection; restarting the same list resumes."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "One")
    (org-gtd-someday-create "Two")
    (org-gtd-someday-create "Three"))
  (org-gtd-reflect-someday-review)                       ; fresh: cursor 0
  (let ((surface (org-gtd-wip--get-buffer "someday-review")))
    (with-current-buffer surface (org-gtd-walk-advance)) ; cursor 0 -> 1
    (with-current-buffer surface (org-gtd-someday-review-quit)))
  (assert-true (file-exists-p
                (org-gtd-walk--checkpoint-path 'someday-review "all")))
  (org-gtd-reflect-someday-review)                       ; restart: resumes
  (let ((surface (org-gtd-wip--get-buffer "someday-review")))
    (with-current-buffer surface
      (assert-same 1 (plist-get (plist-get org-gtd-walk--active :model) :cursor)))))

(deftest someday-review/lists-resume-independently ()
  "A checkpoint for one list does not affect another list's fresh start."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    (with-suppressed-warnings ((obsolete org-gtd-someday-create))
      (with-simulated-input "Work RET" (org-gtd-someday-create "W1"))
      (with-simulated-input "Work RET" (org-gtd-someday-create "W2"))
      (with-simulated-input "Personal RET" (org-gtd-someday-create "P1")))
    (org-gtd-reflect-someday-review "Work")
    (let ((surface (org-gtd-wip--get-buffer "someday-review")))
      (with-current-buffer surface (org-gtd-walk-advance))
      (with-current-buffer surface (org-gtd-someday-review-quit)))
    (assert-true (file-exists-p
                  (org-gtd-walk--checkpoint-path 'someday-review "Work")))
    (org-gtd-reflect-someday-review "Personal")
    (let ((surface (org-gtd-wip--get-buffer "someday-review")))
      (with-current-buffer surface
        (assert-same 0 (plist-get (plist-get org-gtd-walk--active :model) :cursor))))
    (assert-true (file-exists-p
                  (org-gtd-walk--checkpoint-path 'someday-review "Work")))))

(deftest someday-review/finishing-a-pass-deletes-the-checkpoint ()
  "Completing the walk removes the checkpoint so the next run re-scans fresh."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Only"))
  (org-gtd-reflect-someday-review)
  (let ((surface (org-gtd-wip--get-buffer "someday-review")))
    (with-current-buffer surface (org-gtd-walk-advance))) ; off end -> finish
  (assert-nil (file-exists-p
               (org-gtd-walk--checkpoint-path 'someday-review "all"))))
```

**Step 2: Run the tests — verify they FAIL**

Run: `.claude/skills/test/run-tests.sh test/unit/someday-review-test.el`
Expected FAILs:
- `resume-key-encodes-selection` — `org-gtd-someday-review--resume-key` is void.
- `quitting-mid-review-then-restart-resumes-at-cursor` — the entry point does not set
  `:resumable`, so no checkpoint is written; `file-exists-p` is nil (and the restart shows
  cursor 0, not 1).
- `lists-resume-independently` — likewise no checkpoint file.
- `finishing-a-pass-deletes-the-checkpoint` may pass incidentally (no file is ever written);
  it becomes a real guard once resume is on.
- `registered-template-is-not-resumable` — passes today (template already nil); it is a
  regression guard for Step 3, keep it.

**Step 3: Implement**

In `org-gtd-someday-review.el`, add the helper just above the entry point (after
`--on-finish`, near line 188):

```elisp
(defun org-gtd-someday-review--resume-key (list-filter)
  "Return a stable checkpoint resume-key string for LIST-FILTER.
LIST-FILTER is nil (all lists), the symbol `unassigned', or a list-name
string.  Encoding the selection keeps each list's resume checkpoint
independent (design docs/plans/2026-07-22-walk-resume-identity-design.md
§3.2)."
  (cond ((null list-filter) "all")
        ((eq list-filter 'unassigned) "unassigned")
        (t (format "%s" list-filter))))
```

Then, in `org-gtd-reflect-someday-review`, extend the spec assembly (lines 249-252) to opt
into resume — note this is on the *freshly built* spec only; `--spec` (the registered
template) is left `:resumable nil`:

```elisp
      (let ((spec (org-gtd-someday-review--spec)))
        (setq spec (plist-put spec :find (lambda () items)))
        (setq spec (plist-put spec :scope (org-agenda-files)))
        (setq spec (plist-put spec :resumable t))
        (setq spec (plist-put spec :resume-key
                              (org-gtd-someday-review--resume-key list-filter)))
        (org-gtd-walk-start spec (org-gtd-someday-review--surface)))
```

**Step 4: Run the tests — verify they PASS**

Run: `.claude/skills/test/run-tests.sh test/unit/someday-review-test.el`
Expected: all pass. If `quitting-…-resumes-at-cursor` still shows cursor 0 after restart,
confirm the live `--find-items` still returns the items (they must not have been mutated) so
the entry point's empty-guard does not short-circuit before `org-gtd-walk-start` loads the
checkpoint — advancing directly (not via `defer`) leaves the items intact, so this should
hold.

**Step 5: Commit**

```bash
git add org-gtd-someday-review.el test/unit/someday-review-test.el
git commit -m "feat(someday-review): resume each list independently

The standalone entry point now starts the walk :resumable t with a
:resume-key derived from the list filter, so quitting mid-review and
returning resumes that list at its saved position. The registered
template stays :resumable nil so a review-hosted walk still delegates
persistence to the review session.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Task 3: Changelog

**Files:**
- Modify: `CHANGELOG.org`.

**Step 1: Add the entry**

Under the current unreleased/development heading in `CHANGELOG.org`, add:

```org
- Someday/maybe review is now resumable: quitting mid-review and
  returning resumes that list where you left off. Each list keeps an
  independent resume point. (Walk checkpoints are now keyed by the
  selection being reviewed rather than by the file scope.)
```

Match the surrounding bullet/heading style already in the file (check the top of
`CHANGELOG.org` before inserting).

**Step 2: Commit**

```bash
git add CHANGELOG.org
git commit -m "docs(changelog): note resumable someday-review

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>"
```

---

## Final verification (after all tasks)

**Step 1: Run the full suite**

Run: `.claude/skills/test/run-tests.sh` (or the `/test` skill with no argument).
Expected: full suite green (it was `PASS: 1632` before this work; this adds ~8 tests, so
expect ~1640). Investigate any regression in `walk-driver-test`, `walk-scope-test`,
`someday-review-test`, `review-walk-test`, or `inbox-walk-test`.

**Step 2: Byte-compile clean**

Run: `~/bin/eldev compile --warnings-as-errors`
Expected: no warnings/errors from `org-gtd-walk.el` or `org-gtd-someday-review.el`.

**Step 3: Confirm nothing else calls the changed signature**

Run: `grep -rn "org-gtd-walk--checkpoint-path" --include="*.el" .`
Expected callers: the definition and `org-gtd-walk-start` in `org-gtd-walk.el`, plus the
three test files. `org-gtd-review.el:574` sets `:checkpoint-path nil` directly (hosted path)
and does **not** call the function — leave it as-is; it is exactly the no-double-persist rule.

---

## Out of scope (do NOT build)

- Making `inbox` or `missed-calendar-review` resumable (both deliberately stay
  `:resumable nil` — see design §2). The name-fallback keying means inbox is resume-ready
  for free if that is ever wanted.
- Orphan-checkpoint cleanup for quit-and-never-resumed selections (design §7).
- A "how many resumed" counter (design §7).
- Any change to `--scope-key`/`--lock-scope` (the lock is intentionally left coarse).
