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

(provide 'refiling-test)

;;; refiling-test.el ends here
