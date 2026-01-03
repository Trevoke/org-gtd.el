;;; context-test.el --- Tests for org-gtd-context -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-context struct and context resolution.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-context)

(e-unit-initialize)

;;;; Test Setup

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defun ogt-create-project-with-task (project-name task-name)
  "Create project with one task. Return (project-marker . task-id)."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* %s\n" project-name))
    (forward-line -1)
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create))
          (project-marker (point-marker)))
      (org-entry-put (point) "ORG_GTD" "Projects")
      (org-end-of-subtree t t)
      (insert (format "** NEXT %s\n" task-name))
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (org-entry-add-to-multivalued-property project-marker "ORG_GTD_FIRST_TASKS" task-id)
        (basic-save-buffer)
        (cons project-marker task-id)))))

;;;; Struct Tests

(deftest context/struct-accessors-work ()
  "Context struct has all expected accessors."
  (let ((ctx (org-gtd-context-create
              :mode 'graph-view
              :project-marker (point-marker)
              :project-id "proj-123"
              :task-id "task-456"
              :task-marker nil)))
    (assert-equal 'graph-view (org-gtd-context-mode ctx))
    (assert-equal "proj-123" (org-gtd-context-project-id ctx))
    (assert-equal "task-456" (org-gtd-context-task-id ctx))))

(deftest context/from-graph-view-requires-marker ()
  "Context from graph view errors without project marker."
  (with-temp-buffer
    (setq major-mode 'org-gtd-graph-view-mode)
    (setq-local org-gtd-graph-view--project-marker nil)
    (assert-raises 'user-error
      (org-gtd-context-at-point))))

;;;; Context Resolution Tests

(deftest context/from-graph-view-returns-context ()
  "Context from graph view populates all fields."
  (let* ((result (ogt-create-project-with-task "Test Project" "Test Task"))
         (project-marker (car result))
         (task-id (cdr result)))
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)
      (setq-local org-gtd-graph-ui--selected-node-id task-id)
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'graph-view (org-gtd-context-mode ctx))
        (assert-true (markerp (org-gtd-context-project-marker ctx)))
        (assert-equal task-id (org-gtd-context-task-id ctx))))))

(deftest context/from-agenda-returns-context ()
  "Context from agenda resolves project from task."
  (let* ((result (ogt-create-project-with-task "Agenda Project" "Agenda Task"))
         (project-marker (car result))
         (task-id (cdr result))
         (task-marker (org-id-find task-id t)))
    (with-temp-buffer
      (org-agenda-mode)
      (insert "  agenda:  Agenda Task")
      (put-text-property (point-min) (point-max) 'org-marker task-marker)
      (goto-char (point-min))
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'agenda (org-gtd-context-mode ctx))
        (assert-equal task-id (org-gtd-context-task-id ctx))))))

(deftest context/from-org-buffer-on-task ()
  "Context from org buffer on task heading."
  (let* ((result (ogt-create-project-with-task "Org Project" "Org Task"))
         (task-id (cdr result))
         (task-marker (org-id-find task-id t)))
    (org-with-point-at task-marker
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'org (org-gtd-context-mode ctx))
        (assert-equal task-id (org-gtd-context-task-id ctx))))))

(deftest context/from-org-buffer-on-project-heading ()
  "Context from org buffer on project heading has nil task-id."
  (let* ((result (ogt-create-project-with-task "Direct Project" "Direct Task"))
         (project-marker (car result)))
    (org-with-point-at project-marker
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'org (org-gtd-context-mode ctx))
        (assert-nil (org-gtd-context-task-id ctx))))))

(deftest context/errors-in-invalid-buffer ()
  "Context resolution errors in non-supported buffer."
  (with-temp-buffer
    (fundamental-mode)
    (assert-raises 'user-error
      (org-gtd-context-at-point))))

(provide 'context-test)
;;; context-test.el ends here
