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
;; - Entry guards, profile validation, teardown safety (9 tests)
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

(provide 'review-test)

;;; review-test.el ends here
