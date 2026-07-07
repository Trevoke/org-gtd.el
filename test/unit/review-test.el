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

(provide 'review-test)

;;; review-test.el ends here
