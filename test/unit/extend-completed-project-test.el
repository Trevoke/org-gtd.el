;;; extend-completed-project-test.el --- Tests for extending completed projects -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for extending completed (but not archived) projects with new tasks.
;;
;; Migrated from test/extend-completed-project-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;;; Unit tests: org-gtd-projects--set-project-name-on-task

(deftest extend-project/sets-both-project-properties ()
  "Sets both ORG_GTD_PROJECT and ORG_GTD_PROJECT_IDS when extending project."
  ;; Create a simple project
  (capture-inbox-item "Test Project")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task 1")
  (organize-as-project)

  ;; Get the project ID
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test Project")
    (org-back-to-heading t)
    (let ((project-id (org-entry-get (point) "ID")))
      (assert-true project-id)

      ;; Create a new task heading under the project manually
      (org-end-of-subtree)
      (insert "\n*** New Extended Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")

      ;; Go to the new task and call the function
      (goto-char (point-min))
      (search-forward "New Extended Task")
      (org-back-to-heading t)

      ;; Call the function under test
      (org-gtd-projects--set-project-name-on-task)

      ;; Verify both properties are set
      (let ((project-name (org-entry-get (point) "ORG_GTD_PROJECT"))
            (project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
        ;; Should set ORG_GTD_PROJECT
        (assert-true project-name)
        (assert-match "Test Project" project-name)

        ;; Should also set ORG_GTD_PROJECT_IDS
        (assert-equal 1 (length project-ids))
        (assert-equal project-id (car project-ids))))))

(deftest extend-project/works-with-task-in-different-file ()
  "Works when task is in different file than project (multi-file DAG)."
  ;; Create a project in main file
  (capture-inbox-item "Multi-file Project")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task in main file")
  (organize-as-project)

  ;; Get the project ID
  (let (project-id second-file)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Multi-file Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))
      (assert-true project-id))

    ;; Create a task in a DIFFERENT file
    (setq second-file (org-gtd--path "other-file"))
    (with-temp-file second-file
      (insert "* Task in other file\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-other-file\n")
      (insert ":ORG_GTD: Actions\n")
      (insert (format ":ORG_GTD_PROJECT_IDS: %s\n" project-id))
      (insert ":END:\n"))

    ;; Now call org-gtd-projects--set-project-name-on-task from the other file
    (with-current-buffer (find-file-noselect second-file)
      (org-mode)
      (goto-char (point-min))
      (search-forward "Task in other file")
      (org-back-to-heading t)

      ;; This should work even though task is in different file
      (org-gtd-projects--set-project-name-on-task)

      ;; Verify ORG_GTD_PROJECT was set
      (let ((project-name (org-entry-get (point) "ORG_GTD_PROJECT")))
        (assert-true project-name)
        (assert-match "Multi-file Project" project-name)))))

;;;; Acceptance tests: extending a completed but not archived project

(deftest extend-project/allows-adding-task-to-completed-project ()
  "Allows adding a new task to a completed project."
  ;; Step 1: Create a simple project with 2 tasks
  (capture-inbox-item "Completed Project")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Task 1\n** Task 2")
  (organize-as-project)

  ;; Step 2: Mark all tasks as DONE (complete the project)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (org-todo "DONE")

    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)
    (org-todo "DONE"))

  ;; Step 3: Verify project is complete (all tasks DONE)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Completed Project")
    (org-back-to-heading t)
    (assert-true (org-gtd--all-project-tasks-done-p)))

  ;; Step 4: Extend the completed project with a new task
  (add-task-to-existing-project
   "New Task"
   "Projects/Completed SPC Project TAB RET")

  ;; Step 5: Verify the new task was added to the project
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "New Task")
    (org-back-to-heading t)

    ;; Verify task has project ID
    (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (assert-equal 1 (length project-ids))
      (assert-true (car project-ids)))

    ;; Verify task is properly configured as a project task
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
    (assert-true (org-entry-get (point) "ORG_GTD_PROJECT")))

  ;; Step 6: Verify project is no longer complete (has incomplete task)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Completed Project")
    (org-back-to-heading t)
    (assert-nil (org-gtd--all-project-tasks-done-p))))

(deftest extend-project/does-not-archive-when-extension-incomplete ()
  "Does not archive extended project when only new task is incomplete."
  ;; Step 1: Create and complete a project
  (capture-inbox-item "Project To Extend")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Original Task")
  (organize-as-project)

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Original Task")
    (org-back-to-heading t)
    (org-todo "DONE"))

  ;; Step 2: Extend with new task
  (add-task-to-existing-project
   "Extension Task"
   "Projects/Project SPC To SPC Extend TAB RET")

  ;; Step 3: Run archive
  (org-gtd-archive-completed-items)

  ;; Step 4: Verify project and tasks are NOT archived
  (with-current-buffer (org-gtd--default-file)
    (let ((content (buffer-string)))
      (assert-match "Project To Extend" content)
      (assert-match "Original Task" content)
      (assert-match "Extension Task" content)))

  ;; Verify nothing was archived
  (let ((archived-content (ogt--archive-string)))
    (refute-match "Project To Extend" archived-content)
    (refute-match "Extension Task" archived-content)))

(deftest extend-project/archives-after-completing-extension-task ()
  "Archives project after completing the extension task."
  ;; Step 1: Create and complete a project
  (capture-inbox-item "Full Cycle Project")
  (org-gtd-process-inbox)
  (goto-char (point-max))
  (newline)
  (insert "** Initial Task")
  (organize-as-project)

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Initial Task")
    (org-back-to-heading t)
    (org-todo "DONE"))

  ;; Step 2: Extend with new task
  (add-task-to-existing-project
   "Final Task"
   "Projects/Full SPC Cycle SPC Project TAB RET")

  ;; Step 3: Complete the extension task
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Final Task")
    (org-back-to-heading t)
    (org-todo "DONE"))

  ;; Step 4: Run archive
  (org-gtd-archive-completed-items)

  ;; Step 5: Verify project is archived
  (with-current-buffer (org-gtd--default-file)
    (let ((content (buffer-string)))
      (refute-match "Full Cycle Project" content)))

  ;; Verify project was archived
  (let ((archived-content (ogt--archive-string)))
    (assert-match "Full Cycle Project" archived-content)
    (assert-match "Initial Task" archived-content)
    (assert-match "Final Task" archived-content)))

(provide 'extend-completed-project-test)

;;; extend-completed-project-test.el ends here
