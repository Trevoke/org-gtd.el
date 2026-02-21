;;; active-project-predicate-test.el --- Tests for task-has-active-project predicate -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;;; Commentary:
;;
;; Tests for org-gtd-pred--task-has-active-project predicate.
;; This predicate checks whether a task belongs to at least one active project.

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

(deftest active-project-pred/no-project-ids-returns-t ()
  "Task with no ORG_GTD_PROJECT_IDS returns t (standalone action, include)."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (make-task "Standalone task" :id "standalone-1")
    (goto-char (point-min))
    (search-forward "Standalone task")
    (org-back-to-heading t)
    (let ((pred (org-gtd-pred--task-has-active-project)))
      (assert-true (funcall pred)))))

(deftest active-project-pred/one-active-project-returns-t ()
  "Task with one active project returns t (include)."
  (create-project "Active Project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Active Project")
    (org-back-to-heading t)
    (let ((project-id (org-id-get)))
      ;; Navigate to a task in this project
      (org-next-visible-heading 1)
      ;; Verify the task has the project ID
      (assert-true (member project-id
                           (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
      (let ((pred (org-gtd-pred--task-has-active-project)))
        (assert-true (funcall pred))))))

(deftest active-project-pred/one-cncl-project-returns-nil ()
  "Task with one cancelled project returns nil (skip)."
  (create-project "Cancelled Project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Cancelled Project")
    (org-back-to-heading t)
    (let ((project-id (org-id-get)))
      ;; Cancel the project
      (org-todo "CNCL")
      ;; Navigate to a task in this project
      (org-next-visible-heading 1)
      (let ((pred (org-gtd-pred--task-has-active-project)))
        (assert-nil (funcall pred))))))

(deftest active-project-pred/one-done-project-returns-nil ()
  "Task with one done project returns nil (skip)."
  (create-project "Done Project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Done Project")
    (org-back-to-heading t)
    (let ((project-id (org-id-get)))
      ;; Mark the project as done
      (org-todo "DONE")
      ;; Navigate to a task in this project
      (org-next-visible-heading 1)
      (let ((pred (org-gtd-pred--task-has-active-project)))
        (assert-nil (funcall pred))))))

(deftest active-project-pred/multi-project-one-active-returns-t ()
  "Task with one CNCL project and one active project returns t (include)."
  (create-project "Project A")
  (create-project "Project B")
  (with-current-buffer (org-gtd--default-file)
    ;; Get Project A's ID
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-a-id (org-id-get)))
      ;; Get Project B's ID
      (goto-char (point-min))
      (search-forward "Project B")
      (org-back-to-heading t)
      (let ((project-b-id (org-id-get)))
        ;; Cancel Project A
        (goto-char (point-min))
        (search-forward "Project A")
        (org-back-to-heading t)
        (org-todo "CNCL")
        ;; Navigate to Task 1 of Project A and add both project IDs
        (org-next-visible-heading 1)
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS"
                       (format "%s %s" project-a-id project-b-id))
        (let ((pred (org-gtd-pred--task-has-active-project)))
          (assert-true (funcall pred)))))))

(deftest active-project-pred/multi-project-all-cncl-returns-nil ()
  "Task with both projects cancelled returns nil (skip)."
  (create-project "Project X")
  (create-project "Project Y")
  (with-current-buffer (org-gtd--default-file)
    ;; Get Project X's ID
    (goto-char (point-min))
    (search-forward "Project X")
    (org-back-to-heading t)
    (let ((project-x-id (org-id-get)))
      ;; Get Project Y's ID
      (goto-char (point-min))
      (search-forward "Project Y")
      (org-back-to-heading t)
      (let ((project-y-id (org-id-get)))
        ;; Cancel both projects
        (goto-char (point-min))
        (search-forward "Project X")
        (org-back-to-heading t)
        (org-todo "CNCL")
        (goto-char (point-min))
        (search-forward "Project Y")
        (org-back-to-heading t)
        (org-todo "CNCL")
        ;; Navigate to Task 1 of Project X and set both project IDs
        (goto-char (point-min))
        (search-forward "Project X")
        (org-next-visible-heading 1)
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS"
                       (format "%s %s" project-x-id project-y-id))
        (let ((pred (org-gtd-pred--task-has-active-project)))
          (assert-nil (funcall pred)))))))

(deftest active-project-pred/unresolvable-id-returns-t ()
  "Task with unresolvable project ID returns t (fail open, include)."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (make-task "Orphan task"
               :id "orphan-task-1"
               :project-ids '("nonexistent-project-id-12345"))
    (goto-char (point-min))
    (search-forward "Orphan task")
    (org-back-to-heading t)
    (let ((pred (org-gtd-pred--task-has-active-project)))
      (assert-true (funcall pred)))))

(provide 'active-project-predicate-test)

;;; active-project-predicate-test.el ends here
