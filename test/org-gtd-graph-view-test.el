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
;; - org-gtd-graph-view-create-task (create level 1 single action)
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

;;;; org-gtd-graph-view-create-task Tests

(describe "org-gtd-graph-view-create-task"

  (before-each (org-gtd-graph-view-test--setup))
  (after-each (org-gtd-graph-view-test--teardown))

  (it "creates a level 1 single action task"
    (let* ((project-marker (org-gtd-graph-view-test--create-project "Test Project"))
           (buffer (get-buffer-create "*Org GTD Graph: project-test*")))
      ;; Set up minimal graph view buffer
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        ;; Mock the refresh function to avoid window system issues
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          ;; Create task with simulated input
          (with-simulated-input "New SPC Task RET"
            (org-gtd-graph-view-create-task))))

      ;; Verify task was created in org-gtd-tasks.org as level 1
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        ;; Search for the heading text (without TODO keyword which org adds)
        (let ((found (search-forward "New Task" nil t)))
          (expect found :to-be-truthy)
          (when found
            ;; Verify it's a level 1 heading (starts with single *)
            (org-back-to-heading t)
            (expect (org-current-level) :to-equal 1))))))

  (it "sets correct properties on the new task"
    (let* ((project-marker (org-gtd-graph-view-test--create-project "Test Project"))
           (buffer (get-buffer-create "*Org GTD Graph: project-test*")))
      ;; Set up minimal graph view buffer
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        ;; Mock the refresh function
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "Property SPC Test RET"
            (org-gtd-graph-view-create-task))))

      ;; Verify properties are set correctly
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (let ((found (search-forward "Property Test" nil t)))
          (expect found :to-be-truthy)
          (when found
            (org-back-to-heading t)
            ;; Check ORG_GTD property
            (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
            ;; Check TODO state
            (expect (org-get-todo-state) :to-equal "NEXT")
            ;; Check ID exists
            (expect (org-entry-get (point) "ID") :not :to-be nil))))))

  (it "refreshes the graph after creation"
    (let* ((project-marker (org-gtd-graph-view-test--create-project "Test Project"))
           (buffer (get-buffer-create "*Org GTD Graph: project-test*"))
           (refresh-called nil))
      ;; Set up minimal graph view buffer
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        ;; Mock refresh to track if it was called
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh)
                   (lambda () (setq refresh-called t))))
          (with-simulated-input "Refresh SPC Test RET"
            (org-gtd-graph-view-create-task))))

      ;; Verify refresh was called
      (expect refresh-called :to-be-truthy))))

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
