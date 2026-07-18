;;; review-walk-test.el --- Tests for the review console's hosted walk engine fold -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for Phase 2 of the walk engine: folding the review
;; console's bespoke checklist-string walk onto `org-gtd-walk.el',
;; and the generic `walk' review step type.
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-review)
(require 'org-gtd-walk)
(require 'cl-lib)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (setq org-gtd-review--state nil
          org-gtd-review--window-config nil)
    (when (get-buffer org-gtd-review--buffer-name)
      (kill-buffer org-gtd-review--buffer-name))
    (let ((state-file (f-join org-gtd-directory "review-state.eld")))
      (when (file-exists-p state-file)
        (delete-file state-file)))
    (funcall proceed context)))

;;; Checklist Walk Spec Tests (Task A1)

(deftest review-walk/checklist-spec-finds-template-items ()
  "The checklist walk spec's :find yields the template's item strings."
  (let* ((step '(:title "Sweep" :type checklist :checklist "Mind sweep prompts"))
         (spec (org-gtd-review--checklist-walk-spec step)))
    (assert-true (org-gtd-walk-spec-valid-p spec))
    (assert-equal (org-gtd-checklist-template--items "Mind sweep prompts")
                  (funcall (plist-get spec :find)))
    (assert-nil (plist-get spec :resumable))))

;;; Hosted Render + Model Sync Tests (Task A2)

(defvar review-walk-test--tiny-checklist-profile
  '(("T"
     ("P"
      (:title "Sweep" :type checklist :checklist "Mind sweep prompts")
      (:title "After" :type prompt)))))

(deftest review-walk/hosted-render-mirrors-model-and-renders ()
  "Starting a hosted walk renders the current item and mirrors the model.
Drives `org-gtd-review--start-hosted-walk' directly: at this point in
the fold (Task A2), `org-gtd-review-next' still routes checklist
steps through the bespoke `--walk-next' mechanism (rewired in A3), so
the hosted path is exercised through its own entry point."
  (let ((org-gtd-review-profiles review-walk-test--tiny-checklist-profile))
    (org-gtd-review "T")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review--start-hosted-walk
       (org-gtd-review--checklist-walk-spec (org-gtd-review--current-step)))
      (assert-true (plist-get org-gtd-review--state :walk-model))
      (assert-match "(1/" (buffer-string)))))

;;; Checklist Advancement Through the Engine Tests (Task A3)

(defvar review-walk-test--walk-profile
  '(("Walk"
     ("P"
      (:title "Sweep" :type checklist :checklist "Mind sweep prompts")
      (:title "After" :type prompt)))))

(deftest review-walk/checklist-hosted-walk-advances-and-exits ()
  "n loads the hosted walk, advances item by item, then leaves the step.
Asserts :walk-model (not the bespoke :walk-items/:walk-pos) is what
carries state, so this actually discriminates the engine path from
the old one — the rendered text looks identical either way."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)                       ; load, item 1
      (assert-match "(1/8)" (buffer-string))
      (assert-true (plist-get org-gtd-review--state :walk-model))
      (assert-nil (plist-get org-gtd-review--state :walk-items))
      (assert-true org-gtd-walk--active)
      (dotimes (_ 8) (org-gtd-review-next))       ; through 8 and out
      (assert-match "After" (buffer-string)))))

;;; Resume + Teardown Tests (Task A4)

(deftest review-walk/kill-mid-hosted-walk-resumes-at-item ()
  "Killing the console mid-walk resumes at the checkpointed item."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)   ; item 1
      (org-gtd-review-next))  ; item 2
    (kill-buffer org-gtd-review--buffer-name)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
      (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "(2/8)" (buffer-string)))))

(deftest review-walk/kill-mid-hosted-walk-releases-scope-lock ()
  "Killing the console mid-walk unlocks the hosted walk's scope.
Teardown path: kill-buffer."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next))
    (kill-buffer org-gtd-review--buffer-name)
    (assert-nil org-gtd-walk--locked-scopes)))

(deftest review-walk/pause-mid-hosted-walk-releases-scope-lock ()
  "Pausing the console mid-walk unlocks the hosted walk's scope.
Teardown path: reset-session (via pause, distinct from a direct
kill-buffer and from quit)."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-pause))
    (assert-nil org-gtd-walk--locked-scopes)))

(deftest review-walk/quit-mid-hosted-walk-releases-scope-lock ()
  "Abandoning the session (q, then n) mid-walk unlocks the scope.
Teardown path: quit."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) nil)))
        (org-gtd-review-quit)))
    (assert-nil org-gtd-walk--locked-scopes)))

(deftest review-walk/skip-mid-hosted-walk-releases-scope-lock ()
  "Skipping (s) mid-walk unlocks the scope.
Teardown path: complete-step.  The normal off-the-end finish already
unlocks through `org-gtd-walk-finish'; skip mid-walk bypasses that
and must unlock itself (Decision 4)."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)
      (org-gtd-review-skip))
    (assert-nil org-gtd-walk--locked-scopes)))

;;; Corrupt-Checkpoint and Advance-Error Robustness Tests

(deftest review-walk/old-acted-checkpoint-without-model-starts-fresh ()
  "An acted pre-engine checkpoint (no :walk-model) on a walk step is
rejected as invalid, so the session starts fresh; driving the step
with n then loads item 1 instead of advancing off a nil model
(Finding 1)."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    ;; Old-shape save: :acted t on the checklist step, carrying the
    ;; pre-engine :walk-items/:walk-pos and no :walk-model at all.
    (with-temp-file (org-gtd-review--state-file)
      (prin1 '(:profile "Walk" :phase 0 :step 0 :acted t
               :walk-items ("a" "b") :walk-pos 3 :done 0 :skipped 0)
             (current-buffer)))
    ;; --state-valid-p must reject it (acted walk step, no valid model).
    (assert-nil (org-gtd-review--state-valid-p (org-gtd-review--load-state)))
    ;; No resume offer (invalid) -> fresh session.
    (org-gtd-review)
    (with-current-buffer org-gtd-review--buffer-name
      ;; A fresh checklist step is not yet acted; n loads item 1 and
      ;; must not crash advancing off a nil model.
      (assert-nil (plist-get org-gtd-review--state :acted))
      (org-gtd-review-next)
      (assert-match "(1/8)" (buffer-string)))))

(deftest review-walk/advance-error-releases-scope-lock ()
  "An error thrown during a mid-walk advance releases the scope lock
and clears the live walk, instead of leaking the synthetic scope
\(Finding 2)."
  (let ((org-gtd-review-profiles review-walk-test--walk-profile))
    (org-gtd-review "Walk")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)                 ; load, item 1 (scope now locked)
      (assert-true org-gtd-walk--locked-scopes)
      ;; Force the next advance's render to throw.
      (let ((err (condition-case e
                     (progn
                       (cl-letf (((symbol-function 'org-gtd-review--hosted-render)
                                  (lambda (&rest _) (error "Boom"))))
                         (org-gtd-review-next))
                       nil)
                   (error e))))
        (assert-true err))
      ;; The lock is released and the live walk cleared despite the throw.
      (assert-nil org-gtd-walk--locked-scopes)
      (assert-nil org-gtd-walk--active))))

;;; Generic Walk Step Tests (Task B2)

(defvar review-walk-test--handles '("alpha" "beta" "gamma"))

(defun review-walk-test--register ()
  (org-gtd-walk-register
   'review-test-walk
   (list :name 'review-test-walk
         :find (lambda () review-walk-test--handles)
         :render #'org-gtd-review--hosted-render
         :actions nil
         :on-finish #'org-gtd-review--complete-step
         :resumable nil
         :resolve nil
         :scope (list "review-test-walk"))))

(deftest review-walk/walk-step-drives-registered-walk-and-advances ()
  "A :type walk step walks the registered handles, then advances the session."
  (review-walk-test--register)
  (let ((org-gtd-review-profiles
         '(("W" ("P" (:title "Walk it" :type walk :walk review-test-walk)
                     (:title "After" :type prompt))))))
    (org-gtd-review "W")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)                 ; start, alpha
      (assert-match "alpha" (buffer-string))
      (assert-match "(1/3)" (buffer-string))
      (org-gtd-review-next)                 ; beta
      (assert-match "(2/3)" (buffer-string))
      (org-gtd-review-next)                 ; gamma
      (org-gtd-review-next)                 ; off end -> complete-step
      (assert-match "After" (buffer-string)))))

(deftest review-walk/walk-step-resumes-across-restart ()
  "Killing the console mid-walk-step resumes at the checkpointed item,
and releases then re-acquires the registered walk's scope lock (Task
B3 — the A4 rehydrate/teardown machinery generalizes to `walk' steps
via `org-gtd-review--spec-for-step'/`--hosted-spec-for-step')."
  (review-walk-test--register)
  (let ((org-gtd-review-profiles
         '(("W" ("P" (:title "Walk it" :type walk :walk review-test-walk)
                     (:title "After" :type prompt))))))
    (org-gtd-review "W")
    (with-current-buffer org-gtd-review--buffer-name
      (org-gtd-review-next)   ; alpha
      (org-gtd-review-next))  ; beta
    (kill-buffer org-gtd-review--buffer-name)
    (assert-nil org-gtd-walk--locked-scopes)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_p) t)))
      (org-gtd-review))
    (with-current-buffer org-gtd-review--buffer-name
      (assert-match "beta" (buffer-string))
      (assert-match "(2/3)" (buffer-string)))
    (assert-true org-gtd-walk--locked-scopes)))

;;; Walk Step Validation Tests (Task B1)

(deftest review-walk/walk-step-missing-walk-errors-cleanly ()
  "A walk step without :walk is rejected at session start."
  (let ((org-gtd-review-profiles
         '(("Bad" ("P" (:title "Walk what?" :type walk))))))
    (let ((err (condition-case e (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match ":walk" (cadr err)))
    (assert-nil org-gtd-review--state)))

(deftest review-walk/walk-step-unknown-walk-errors-cleanly ()
  "A walk step naming an unregistered walk is rejected at session start."
  (let ((org-gtd-review-profiles
         '(("Bad" ("P" (:title "Ghost" :type walk :walk no-such-walk))))))
    (let ((err (condition-case e (progn (org-gtd-review "Bad") nil)
                 (user-error e))))
      (assert-true err)
      (assert-match "no-such-walk" (cadr err)))
    (assert-nil org-gtd-review--state)))

(provide 'review-walk-test)

;;; review-walk-test.el ends here
