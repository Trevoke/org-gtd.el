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

(provide 'review-walk-test)

;;; review-walk-test.el ends here
