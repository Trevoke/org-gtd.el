;;; review-test.el --- Tests for the guided review engine -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the guided, profile-driven review session engine.
;;
;; Test Coverage:
;; - Default Weekly Review profile shape (2 tests)
;; - Session lifecycle: start, advance, complete, skip (4 tests)
;; - Entry guards, profile validation, teardown safety (12 tests)
;; - Checklist walks and mid-walk capture (4 tests)
;; - Full default-profile end-to-end run (1 test)
;; - Pause / resume / quit persistence and checkpointing (14 tests)
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-review)
(require 'cl-lib)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (setq org-gtd-review--state nil
          org-gtd-review--window-config nil)
    (when (get-buffer org-gtd-review--buffer-name)
      (kill-buffer org-gtd-review--buffer-name))
    ;; Inlined path (rather than org-gtd-review--state-file) so cleanup
    ;; works even before the persistence code exists.
    (let ((state-file (f-join org-gtd-directory "review-state.eld")))
      (when (file-exists-p state-file)
        (delete-file state-file)))
    (funcall proceed context)))

;;; Default Profile Tests

(deftest review/default-profile-is-weekly-three-phase ()
  "The shipped default is a Weekly Review with the three GTD phases."
  (let ((profile (assoc "Weekly Review" org-gtd-review-profiles)))
    (assert-true profile)
    (assert-equal '("Get Clear" "Get Current" "Get Creative")
                  (mapcar #'car (cdr profile)))))

(deftest review/default-mind-sweep-references-starter-checklist ()
  "The Get Clear phase walks the bundled trigger list."
  (let* ((phases (cdr (assoc "Weekly Review" org-gtd-review-profiles)))
         (get-clear (cdr (assoc "Get Clear" phases)))
         (sweep (seq-find (lambda (s) (eq (plist-get s :type) 'checklist))
                          get-clear)))
    (assert-equal "Weekly Review triggers" (plist-get sweep :checklist))))

;;; Session Engine Tests

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

;;; Entry Guard and Validation Tests

(deftest review/second-invocation-signals-user-error ()
  "Starting a review while one is active errors, keeping the session."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (let ((err (condition-case e
                   (progn (org-gtd-review "Tiny") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match "already active" (cadr err)))
    (assert-equal "Tiny" (plist-get org-gtd-review--state :profile))
    (assert-equal 0 (plist-get org-gtd-review--state :step))))

(deftest review/no-profiles-configured-errors ()
  "An empty profiles alist signals a clean user-error."
  (let ((org-gtd-review-profiles nil))
    (let ((err (condition-case e
                   (progn (org-gtd-review) nil)
                 (user-error e))))
      (assert-true err)
      (assert-match "No review profiles configured" (cadr err))))
  (assert-nil org-gtd-review--state))

(deftest review/malformed-phase-errors-cleanly ()
  "A non-list phase user-errors; no session or buffer is left behind."
  (let ((org-gtd-review-profiles '(("Bad" "not-a-phase-list"))))
    (let ((err (condition-case e
                   (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err))
    (assert-nil org-gtd-review--state)
    (assert-nil (get-buffer org-gtd-review--buffer-name))))

(deftest review/dotted-phase-errors-cleanly ()
  "A dotted phase list user-errors instead of crashing."
  (let ((org-gtd-review-profiles '(("Broken" ("Phase" . :notalist)))))
    (let ((err (condition-case e
                   (progn (org-gtd-review "Broken") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match "should be a list starting with a name string"
                    (cadr err)))
    (assert-nil org-gtd-review--state)))

(deftest review/zero-step-phase-errors-cleanly ()
  "A named phase with no steps user-errors instead of rendering junk."
  (let ((org-gtd-review-profiles '(("EmptySteps" ("Nothing here")))))
    (let ((err (condition-case e
                   (progn (org-gtd-review "EmptySteps") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match "has no steps" (cadr err)))
    (assert-nil org-gtd-review--state)))

(deftest review/step-missing-type-errors-cleanly ()
  "A step without :type user-errors in teaching voice."
  (let ((org-gtd-review-profiles
         '(("Bad" ("P" (:title "No type here"))))))
    (let ((err (condition-case e
                   (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match ":type" (cadr err)))
    (assert-nil org-gtd-review--state)))

(deftest review/render-error-still-tears-down ()
  "An unexpected error during session start restores a clean slate."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (cl-letf (((symbol-function 'org-gtd-review--render)
               (lambda () (error "Boom"))))
      (condition-case nil (org-gtd-review "Tiny") (error nil)))
    (assert-nil org-gtd-review--state)
    (assert-nil org-gtd-review--window-config)))

;;; Teardown Safety Tests

(deftest review/capture-advertised-on-every-step ()
  "The header line offers c on prompt steps, not only checklist walks."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "\\[c\\] Capture" (format "%s" header-line-format)))))

(deftest review/killing-buffer-ends-session ()
  "Killing *GTD Review* directly clears the session state."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (kill-buffer org-gtd-review--buffer-name)
    (assert-nil org-gtd-review--state)))

;;; Command and View Step Tests

(defvar review-test--command-calls 0)
(defun review-test--command ()
  "Test command that records invocations."
  (interactive)
  (setq review-test--command-calls (1+ review-test--command-calls)))

(defun review-test--failing-command ()
  "Test command that records the invocation, then signals."
  (interactive)
  (setq review-test--command-calls (1+ review-test--command-calls))
  (error "Boom"))

(defun review-test--popping-view ()
  "Test view that records the invocation and pops another buffer."
  (interactive)
  (setq review-test--command-calls (1+ review-test--command-calls))
  (pop-to-buffer (get-buffer-create "*review-test-view*")))

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

(deftest review/command-step-error-leaves-step-retryable ()
  "A :command that signals leaves the step un-acted so n retries it."
  (setq review-test--command-calls 0)
  (let ((org-gtd-review-profiles
         '(("T" ("P" (:title "Flaky" :type command :command review-test--failing-command)
                     (:title "After" :type prompt))))))
    (org-gtd-review "T")
    (with-current-buffer org-gtd-review--buffer-name
      ;; First n: the command signals; the error propagates.
      (assert-true (condition-case e
                       (progn (org-gtd-review-next) nil)
                     (error e)))
      ;; The step must not count as acted or done.
      (assert-nil (plist-get org-gtd-review--state :acted))
      (assert-equal 0 (plist-get org-gtd-review--state :done))
      ;; Second n retries the command instead of silently advancing.
      (assert-true (condition-case e
                       (progn (org-gtd-review-next) nil)
                     (error e)))
      (assert-equal 2 review-test--command-calls)
      (assert-match "Flaky" (buffer-string)))))

(deftest review/view-step-shows-view-then-advances ()
  "First n calls :view without stealing the window; second n advances."
  (let ((org-gtd-review-profiles
         '(("T" ("P" (:title "Look" :type view :view review-test--popping-view)
                     (:title "After" :type prompt))))))
    (setq review-test--command-calls 0)
    (org-gtd-review "T")
    (unwind-protect
        (with-current-buffer org-gtd-review--buffer-name
          (org-gtd-review-next)
          (assert-equal 1 review-test--command-calls)
          ;; The review console must stay the selected window.
          (assert-equal org-gtd-review--buffer-name
                        (buffer-name (window-buffer (selected-window))))
          (org-gtd-review-next)
          (assert-match "After" (buffer-string)))
      (when (get-buffer "*review-test-view*")
        (kill-buffer "*review-test-view*")))))

(deftest review/command-step-missing-command-errors-cleanly ()
  "A command step without :command is rejected at session start."
  (let ((org-gtd-review-profiles
         '(("Bad" ("P" (:title "Run what?" :type command))))))
    (let ((err (condition-case e
                   (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match ":command" (cadr err)))
    (assert-nil org-gtd-review--state)))

(deftest review/view-step-missing-view-errors-cleanly ()
  "A view step without :view is rejected at session start."
  (let ((org-gtd-review-profiles
         '(("Bad" ("P" (:title "Look at what?" :type view))))))
    (let ((err (condition-case e
                   (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match ":view" (cadr err)))
    (assert-nil org-gtd-review--state)))

(deftest review/checklist-step-missing-checklist-errors-cleanly ()
  "A checklist step without :checklist is rejected at session start."
  (let ((org-gtd-review-profiles
         '(("Bad" ("P" (:title "Sweep what?" :type checklist))))))
    (let ((err (condition-case e
                   (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match ":checklist" (cadr err)))
    (assert-nil org-gtd-review--state)))

(deftest review/unknown-step-type-skips-with-message ()
  "An unknown :type never errors; it advances."
  (let ((org-gtd-review-profiles
         '(("T" ("P" (:title "Weird" :type frobnicate)
                     (:title "After" :type prompt))))))
    (org-gtd-review "T")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (assert-match "After" (buffer-string)))))

;;; Checklist Walk Step Tests

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

(deftest review/skip-mid-walk-exits-whole-step ()
  "s in the middle of a walk leaves the step, tallied as skipped."
  (let ((org-gtd-review-profiles review-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)                       ; load walk, item 1
      (org-gtd-review-next)                       ; item 2
      (org-gtd-review-skip)
      (assert-match "After" (buffer-string)))
    (assert-equal 1 (plist-get org-gtd-review--state :skipped))
    (assert-equal 0 (plist-get org-gtd-review--state :done))
    (assert-nil (plist-get org-gtd-review--state :walk-items))
    (assert-nil (plist-get org-gtd-review--state :acted))))

(deftest review/capture-mid-walk-keeps-position ()
  "c mid-walk fires capture without moving the walk position."
  (let ((org-gtd-review-profiles review-test--walk-profile)
        (captures 0))
    (cl-letf (((symbol-function 'org-gtd-capture)
               (lambda (&rest _)
                 (interactive)
                 (setq captures (1+ captures)))))
      (org-gtd-review "Walk")
      (with-current-buffer org-gtd-review--buffer-name
        (org-gtd-review-next)              ; load walk, item 1
        (org-gtd-review-next)              ; item 2
        (org-gtd-review-capture)
        (assert-equal 1 captures)
        ;; The walk did not move: still on item 2, same step.
        (assert-equal 1 (plist-get org-gtd-review--state :walk-pos))
        (assert-match "(2/8)" (buffer-string))))))

(deftest review/checklist-step-missing-template-auto-advances ()
  "A missing/empty checklist self-satisfies instead of erroring."
  (let ((org-gtd-review-profiles
         '(("W" ("P" (:title "Sweep" :type checklist :checklist "Nope")
                     (:title "After" :type prompt))))))
    (org-gtd-review "W")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (assert-match "After" (buffer-string)))))

;;; Default Profile Integration Test

(defun review-test--step-presses (step)
  "Return how many presses of n STEP consumes in a session.
A prompt advances on one press; command and view steps take one
press to act and one to confirm; a checklist walk takes one press
to load plus one per item after the first plus one to leave — or a
single press when the checklist is empty or missing."
  (pcase (plist-get step :type)
    ('prompt 1)
    ((or 'command 'view) 2)
    ('checklist
     (let ((items (org-gtd-checklist--items (plist-get step :checklist))))
       (if items (1+ (length items)) 1)))))

(deftest review/default-weekly-profile-runs-end-to-end ()
  "The shipped Weekly Review profile completes under n alone.
The profile's command and view symbols are stubbed to interactive
no-ops so the run is deterministic; the press count is derived from
the profile structure, not hardcoded."
  (let* ((phases (cdr (assoc "Weekly Review" org-gtd-review-profiles)))
         (steps (apply #'append (mapcar #'cdr phases)))
         (actions (delq nil (mapcar (lambda (s)
                                      (or (plist-get s :command)
                                          (plist-get s :view)))
                                    steps)))
         (originals (mapcar #'symbol-function actions))
         (presses (apply #'+ (mapcar #'review-test--step-presses steps))))
    (unwind-protect
        (progn
          (dolist (action actions)
            (fset action (lambda () (interactive))))
          (org-gtd-review "Weekly Review")
          (with-current-buffer org-gtd-review--buffer-name
            (dotimes (_ presses) (org-gtd-review-next)))
          (assert-nil org-gtd-review--state)
          (assert-nil (get-buffer org-gtd-review--buffer-name))
          (assert-nil (file-exists-p (org-gtd-review--state-file))))
      (cl-mapc #'fset actions originals))))

;;; Pause / Resume / Quit Tests

(deftest review/pause-persists-state-and-tears-down ()
  "p writes review-state.eld and closes the session."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))
    (assert-nil org-gtd-review--state)
    (assert-true (file-exists-p (org-gtd-review--state-file)))))

(deftest review/resume-restores-position ()
  "Starting again after a pause offers resume and restores the step."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))
    ;; y-or-n-p is stubbed, not fed keys: batch-mode y-or-n-p does not
    ;; reliably consume simulated input (suite-wide convention).
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (org-gtd-review))
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
    ;; The stale save is replaced by the fresh session's checkpoint.
    (assert-equal 0 (plist-get (org-gtd-review--load-state) :step))
    (assert-true (org-gtd-review--state-valid-p
                  (org-gtd-review--load-state)))))

(deftest review/completion-deletes-state-file ()
  "Finishing a resumed session removes review-state.eld."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-pause))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (dotimes (_ 3) (org-gtd-review-next)))
    (assert-nil (file-exists-p (org-gtd-review--state-file)))))

(deftest review/quit-keeping-progress-pauses ()
  "q answered y keeps a state file, like a pause."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
        (org-gtd-review-quit)))
    (assert-nil org-gtd-review--state)
    (assert-true (file-exists-p (org-gtd-review--state-file)))))

(deftest review/quit-abandoning-deletes-state-file ()
  "q answered n abandons: no state file survives, session gone."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-pause))            ; leaves a state file behind
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (org-gtd-review))                  ; resume the paused session
    (with-current-buffer org-gtd-review--buffer-name
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
        (org-gtd-review-quit)))
    (assert-nil org-gtd-review--state)
    (assert-nil (file-exists-p (org-gtd-review--state-file)))))

(deftest review/pause-without-session-errors-and-preserves-save ()
  "A stray M-x pause with no session must not clobber a saved pause."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))            ; save at step two
    (let ((err (condition-case e
                   (progn (org-gtd-review-pause) nil)
                 (user-error e))))
      (assert-true err))
    (assert-equal 1 (plist-get (org-gtd-review--load-state) :step))))

(deftest review/next-without-session-teaches ()
  "A stray M-x next with no session user-errors instead of crashing."
  (let ((err (condition-case e
                 (progn (org-gtd-review-next) nil)
               (user-error e))))
    (assert-true err)
    (assert-match "No review session is active" (cadr err))))

(deftest review/skip-without-session-teaches ()
  "A stray M-x skip with no session user-errors instead of crashing."
  (let ((err (condition-case e
                 (progn (org-gtd-review-skip) nil)
               (user-error e))))
    (assert-true err)
    (assert-match "No review session is active" (cadr err))))

(deftest review/quit-without-session-errors-and-preserves-save ()
  "A stray M-x quit with no session must not delete a saved pause."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil)))
      (let ((err (condition-case e
                     (progn (org-gtd-review-quit) nil)
                   (user-error e))))
        (assert-true err)))
    (assert-equal 1 (plist-get (org-gtd-review--load-state) :step))))

(deftest review/declining-resume-then-abort-keeps-save ()
  "n to resume, then C-g at the profile picker, keeps the pause resumable."
  (let ((org-gtd-review-profiles
         (append review-test--tiny-profile
                 '(("Other" ("P" (:title "Elsewhere" :type prompt)))))))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (condition-case nil (org-gtd-review) (quit nil)))
    (assert-equal 1 (plist-get (org-gtd-review--load-state) :step))
    ;; The declined-but-aborted pause is still resumable.
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "Step two" (buffer-string)))))

(deftest review/kill-buffer-leaves-checkpoint-resumable ()
  "C-x k mid-session leaves a checkpoint; re-entry resumes in place."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-next))             ; now on Phase B / Step three
    (kill-buffer org-gtd-review--buffer-name)
    (assert-nil org-gtd-review--state)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "Step three" (buffer-string)))))

(deftest review/kill-mid-walk-resumes-at-item ()
  "Killing the buffer mid-walk resumes at the checkpointed item."
  (let ((org-gtd-review-profiles review-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)              ; load walk, item 1
      (org-gtd-review-next))             ; item 2
    (kill-buffer org-gtd-review--buffer-name)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "(2/8)" (buffer-string)))))

(deftest review/mangled-state-file-starts-fresh ()
  "A readable but incoherent save starts fresh, without an error loop."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (with-temp-file (org-gtd-review--state-file)
      (prin1 '(:profile "Tiny" :phase 0 :step 0 :acted t
               :walk-items ("a" "b") :walk-pos 9 :done 0 :skipped 0)
             (current-buffer)))
    (org-gtd-review)                     ; no resume prompt: state invalid
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "Step one" (buffer-string)))
    ;; The mangled save was replaced by the fresh session's checkpoint.
    (assert-true (org-gtd-review--state-valid-p
                  (org-gtd-review--load-state)))
    (assert-equal 0 (plist-get (org-gtd-review--load-state) :walk-pos))))

(deftest review/state-valid-p-rejects-corrupt-fields ()
  "Negative indices, non-integer tallies, and bad walk shapes are invalid."
  (let ((org-gtd-review-profiles review-test--tiny-profile)
        (base '(:profile "Tiny" :phase 0 :step 0 :acted nil
                :walk-items nil :walk-pos 0 :done 0 :skipped 0)))
    (cl-flet ((mangle (key val)
                (plist-put (copy-sequence base) key val)))
      (assert-true (org-gtd-review--state-valid-p base))
      (assert-nil (org-gtd-review--state-valid-p (mangle :phase -1)))
      (assert-nil (org-gtd-review--state-valid-p (mangle :step -1)))
      (assert-nil (org-gtd-review--state-valid-p (mangle :done "three")))
      (assert-nil (org-gtd-review--state-valid-p (mangle :skipped nil)))
      (assert-nil (org-gtd-review--state-valid-p
                   (mangle :walk-items "not-a-list")))
      (assert-nil (org-gtd-review--state-valid-p
                   (plist-put (mangle :walk-items '("a")) :walk-pos -1)))
      ;; :walk-pos must be an integer even when :walk-items is empty.
      (assert-nil (org-gtd-review--state-valid-p (mangle :walk-pos "x"))))))

(deftest review/explicit-profile-arg-skips-resume-offer ()
  "An explicit different PROFILE-NAME starts fresh with no resume offer."
  (let ((org-gtd-review-profiles
         (append review-test--tiny-profile
                 '(("Other" ("P" (:title "Elsewhere" :type prompt)))))))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (_prompt) (error "Resume offer should be skipped"))))
      (org-gtd-review "Other"))
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "Elsewhere" (buffer-string)))
    ;; No eager delete: the new session's checkpoint took the slot over.
    (assert-equal "Other" (plist-get (org-gtd-review--load-state) :profile))))

(deftest review/resume-with-edited-bad-profile-teaches-and-deletes ()
  "A profile edited invalid while paused surfaces the teaching error."
  (let ((org-gtd-review-profiles review-test--tiny-profile))
    (org-gtd-review "Tiny")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause)))
  ;; Same shape (state stays index-valid) but a step lost its :type.
  (let ((org-gtd-review-profiles
         '(("Tiny"
            ("Phase A"
             (:title "Step one" :type prompt)
             (:title "No type here"))
            ("Phase B"
             (:title "Step three" :type prompt))))))
    (let ((err (condition-case e
                   (progn
                     (cl-letf (((symbol-function 'y-or-n-p)
                                (lambda (_prompt) t)))
                       (org-gtd-review))
                     nil)
                 (user-error e))))
      (assert-true err)
      (assert-match ":type" (cadr err)))
    (assert-nil (file-exists-p (org-gtd-review--state-file)))
    (assert-nil org-gtd-review--state)))

(provide 'review-test)

;;; review-test.el ends here
