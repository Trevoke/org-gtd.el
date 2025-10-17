;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-graph-view-test.el --- Unit tests for graph view interactions -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-view interactive functions.
;;
;; Test Coverage:
;; - org-gtd-graph-view-add-dependency (add dependency from any task)
;; - org-gtd-graph-view-add-blocker (add blocker as root task)
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-data)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)
(require 'org-gtd-files)
(require 'org-gtd-core)
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))
(require 'with-simulated-input)

;;;; Test Setup

(defun org-gtd-graph-view-test--setup ()
  "Set up minimal test environment for graph view tests."
  (setq org-gtd-directory (make-temp-file "org-gtd-graph-test" t)
        org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL")))
  ;; Create the tasks file
  (let ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org")))
    (with-temp-file tasks-file
      (insert ""))))

(defun org-gtd-graph-view-test--teardown ()
  "Clean up after graph view tests."
  (when (and org-gtd-directory (file-exists-p org-gtd-directory))
    (delete-directory org-gtd-directory t)))

(defun org-gtd-graph-view-test--create-project (title)
  "Create a test project with TITLE and return its marker."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* %s\n" title))
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (insert (format ":ID: project-%s\n" (make-temp-name "id")))
    (insert ":END:\n")
    (insert "** TODO Task 1\n")
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: task-%s\n" (make-temp-name "id")))
    (insert ":ORG_GTD: Actions\n")
    (insert (format ":ORG_GTD_PROJECT_IDS: project-%s\n" (make-temp-name "id")))
    (insert ":END:\n")
    (goto-char (point-min))
    (search-forward title)
    (org-back-to-heading t)
    (basic-save-buffer)
    (point-marker)))

;;;; org-gtd-graph-view-add-dependency Tests

(describe "org-gtd-graph-view-add-dependency"

  (before-each (org-gtd-graph-view-test--setup))
  (after-each (org-gtd-graph-view-test--teardown))

  (xit "adds dependency from external task to project task"
    ;; TODO: Implement when simulated input is refined for selecting tasks
    )

  (xit "adds project ID to external blocker's ORG_GTD_PROJECT_IDS"
    ;; TODO: Implement when simulated input is refined for selecting tasks
    )

  (xit "adds TRIGGER property to external blocker"
    ;; TODO: Implement when simulated input is refined for selecting tasks
    ))

;;;; org-gtd-graph-view-add-blocker Tests

(describe "org-gtd-graph-view-add-blocker"

  (before-each (org-gtd-graph-view-test--setup))
  (after-each (org-gtd-graph-view-test--teardown))

  (xit "adds blocker to project's ORG_GTD_FIRST_TASKS"
    ;; TODO: Implement when simulated input is refined for selecting tasks
    )

  (xit "creates dependency relationship"
    ;; TODO: Implement when simulated input is refined for selecting tasks
    )

  (xit "handles external blockers correctly"
    ;; TODO: Implement when simulated input is refined for selecting tasks
    ))

(provide 'org-gtd-graph-view-test)

;;; org-gtd-graph-view-test.el ends here
