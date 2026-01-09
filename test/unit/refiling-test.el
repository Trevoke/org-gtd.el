;;; refiling-test.el --- Tests for GTD item refiling -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for refiling items to GTD targets including user's custom targets.
;;
;; Test Coverage:
;; - User refile targets merged with GTD targets (2 tests)
;; - Project refiling with org-gtd-refile-to-any-target (1 test)
;; - Finding refile targets by ORG_GTD_REFILE property (3 tests)
;; - Multiple file refile targets (1 test)
;;
;; Migrated from test/refiling-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; User Refile Targets Tests

(deftest refiling/includes-user-refile-targets ()
  "Includes user's org-refile-targets in the target list."
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (basic-save-buffer))
  ;; Create a user refile target file OUTSIDE the GTD directory
  ;; Using /mock:/tmp/ path for mock-fs
  (let* ((user-file (make-temp-file "user-targets" nil ".org"))
         (user-buffer (find-file-noselect user-file)))
    (with-current-buffer user-buffer
      (erase-buffer)
      (insert "* User Custom Target\n")
      (basic-save-buffer))
    ;; Set user's org-refile-targets to include their file
    (let* ((org-refile-targets `((,user-file :maxlevel . 1)))
           (target-names (mapcar 'car (org-gtd-refile--get-targets org-gtd-projects))))
      ;; Should include both user's targets and org-gtd's
      (assert-true (member "User Custom Target" target-names))
      (assert-true (member "Projects" target-names)))
    (kill-buffer user-buffer)
    (delete-file user-file)))

(deftest refiling/places-user-targets-before-gtd-targets ()
  "Places user's targets before org-gtd targets."
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (basic-save-buffer))
  ;; Create a user refile target file OUTSIDE the GTD directory
  (let* ((user-file (make-temp-file "user-targets" nil ".org"))
         (user-buffer (find-file-noselect user-file)))
    (with-current-buffer user-buffer
      (erase-buffer)
      (insert "* User Target First\n")
      (basic-save-buffer))
    ;; Set user's org-refile-targets
    (let* ((org-refile-targets `((,user-file :maxlevel . 1)))
           (target-names (mapcar 'car (org-gtd-refile--get-targets org-gtd-projects))))
      ;; User's target should appear before org-gtd's
      (assert-true (< (seq-position target-names "User Target First")
                      (seq-position target-names "Projects"))))
    (kill-buffer user-buffer)
    (delete-file user-file)))

;;; Project Refiling Tests

(deftest refiling/skips-choice-when-refile-to-any-enabled ()
  "Skips refiling choice if option is enabled."
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (basic-save-buffer))

  (let ((org-gtd-refile-to-any-target t)
        (temp-buffer (get-buffer-create (generate-new-buffer-name "wip"))))

    (with-current-buffer temp-buffer
      (org-mode)
      (insert "* foobar")
      (org-gtd-refile--do org-gtd-projects org-gtd-projects-template))

    (with-current-buffer (org-gtd--default-file)
      (assert-match "foobar" (ogt--current-buffer-raw-text)))

    (kill-buffer temp-buffer)))

;;; Finding Refile Targets Tests

(deftest refiling/finds-targets-with-refile-property ()
  "Finds targets marked with ORG_GTD_REFILE property."
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "\n* Work Projects
:PROPERTIES:
:ORG_GTD_REFILE: Projects
:END:")
    (save-buffer))

  (let ((org-gtd-refile-to-any-target nil))
    (let ((targets (mapcar 'car (org-gtd-refile--get-targets org-gtd-projects))))
      (assert-true (member "Work Projects" targets)))))

(deftest refiling/finds-project-target ()
  "Finds the Project target."
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (basic-save-buffer))

  (let ((org-gtd-refile-to-any-target nil))
    (let ((targets (caar (org-gtd-refile--get-targets org-gtd-projects))))
      (assert-equal "Projects" targets))))

(deftest refiling/finds-tickler-headings ()
  "Finds the Tickler headings in the tickler file."
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* To Read
:PROPERTIES:
:ORG_GTD_REFILE: Tickler
:END:
* To Eat
:PROPERTIES:
:ORG_GTD_REFILE: Tickler
:END:")
    (save-buffer))

  (let ((org-gtd-refile-to-any-target nil))
    (let ((ogt-target-names (mapcar 'car (org-gtd-refile--get-targets org-gtd-tickler))))
      ;; Both targets should be found
      (assert-true (member "To Eat" ogt-target-names))
      (assert-true (member "To Read" ogt-target-names)))))

;;; Multiple File Refile Targets Tests

(deftest refiling/offers-targets-from-multiple-files ()
  "Offers refiling targets from multiple files."
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (basic-save-buffer))

  (let ((org-gtd-refile-to-any-target nil)
        (new-buffer (create-additional-project-target "more-projects"))
        (temp-buffer (get-buffer-create (generate-new-buffer-name "wip"))))

    (with-current-buffer temp-buffer
      (org-mode)
      (insert "* choose-refile-target")
      (point-min)
      (with-simulated-input
          "AdditionalHeading RET"
        (org-gtd-refile--do org-gtd-projects org-gtd-projects-template)))

    (assert-match "choose-refile-target"
                  (with-current-buffer new-buffer (ogt--current-buffer-raw-text)))

    ;; Cleanup is handled by mock-fs teardown
    (kill-buffer temp-buffer)))

;;; Should-Prompt Helper Tests

(deftest refile/should-prompt-returns-nil-when-deprecated-var-set ()
  "Returns nil when org-gtd-refile-to-any-target is t (deprecated path)."
  (let ((org-gtd-refile-to-any-target t)
        (org-gtd-refile--deprecated-warning-shown t)) ; suppress warning in test
    (assert-nil (org-gtd-refile--should-prompt-p 'single-action))))

(deftest refile/should-prompt-checks-list-when-deprecated-var-nil ()
  "Checks org-gtd-refile-prompt-for-types when deprecated var is nil."
  (let ((org-gtd-refile-to-any-target nil)
        (org-gtd-refile-prompt-for-types '(single-action calendar)))
    (assert-true (org-gtd-refile--should-prompt-p 'single-action))
    (assert-true (org-gtd-refile--should-prompt-p 'calendar))
    (assert-nil (org-gtd-refile--should-prompt-p 'trash))))

(deftest refile/deprecated-var-shows-warning-once ()
  "Shows deprecation warning only once per session."
  (let ((org-gtd-refile-to-any-target t)
        (org-gtd-refile--deprecated-warning-shown nil)
        (warnings nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (push args warnings))))
      (org-gtd-refile--should-prompt-p 'single-action)
      (org-gtd-refile--should-prompt-p 'calendar)
      (assert-equal 1 (length warnings)))))

;;; Edge Case Tests - User Configuration Issues

;;; Inbox Exclusion Tests

(deftest refile/excludes-inbox-file-even-with-refile-property ()
  "Inbox file should never appear as a refile target, even with ORG_GTD_REFILE property.

The inbox is for capture only. Even if inbox.org accidentally has headings
with ORG_GTD_REFILE properties, they should still be filtered out.
This prevents items from being refiled back to inbox during processing."
  ;; First create a valid GTD target (this goes through the full workflow)
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (basic-save-buffer))

  ;; Now add headings to inbox WITH ORG_GTD_REFILE property (edge case)
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    (goto-char (point-max))
    (insert "* Inbox Projects Heading
:PROPERTIES:
:ORG_GTD_REFILE: Projects
:END:
")
    (basic-save-buffer))

  ;; Get refile targets for projects
  (let* ((org-gtd-refile-to-any-target nil)
         (targets (org-gtd-refile--get-targets org-gtd-projects))
         (target-names (mapcar 'car targets)))
    ;; Should have GTD targets from org-gtd-tasks.org
    (assert-true (member "Projects" target-names))
    ;; Should NOT have inbox headings even if they have the property
    (assert-nil (member "Inbox Projects Heading" target-names))))

(deftest refile/nil-current-buffer-target-excludes-wip-buffer ()
  "User's (nil :maxlevel) target should not match WIP buffer headings.

When a user has org-refile-targets configured with (nil :maxlevel . N),
this means 'current buffer'. During organize, the current buffer is the
WIP/clarify buffer which contains the item being organized.

The refile verify function should exclude WIP buffer headings to prevent
the 'Cannot refile to position inside the tree or region' error that
would occur if the item tried to refile to itself."
  ;; Set up user's org-refile-targets with (nil :maxlevel . 3)
  ;; This configuration should work - WIP buffers should be filtered out
  (let ((org-refile-targets '((nil :maxlevel . 3)))
        (org-gtd-refile-to-any-target t))
    ;; Capture and process an item - this creates a WIP buffer
    (capture-inbox-item "test item with nil refile target")
    (org-gtd-process-inbox)
    ;; Now we're in the WIP buffer with the item
    ;; Organizing should succeed - WIP buffer headings should be filtered
    (organize-as-single-action)
    ;; Verify item was refiled to the default GTD file
    (with-current-buffer (org-gtd--default-file)
      (assert-match "test item with nil refile target" (ogt--current-buffer-raw-text)))
    ;; Verify item shows in engage agenda
    (org-gtd-engage)
    (assert-match "test item with nil refile target" (agenda-raw-text))))

(provide 'refiling-test)

;;; refiling-test.el ends here
