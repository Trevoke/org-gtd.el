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

(provide 'review-walk-test)

;;; review-walk-test.el ends here
