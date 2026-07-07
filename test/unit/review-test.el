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
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-review)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (setq org-gtd-review--state nil
          org-gtd-review--window-config nil)
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

(provide 'review-test)

;;; review-test.el ends here
