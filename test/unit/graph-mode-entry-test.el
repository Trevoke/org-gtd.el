;;; graph-mode-entry-test.el --- Tests for graph entry point expansion -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-show-project-graph working on task headings and agenda items.
;; This tests the expansion of --find-project-at-point to support task headings
;; in addition to project headings.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

(require 'org-gtd-graph-mode)
(require 'with-simulated-input)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Tests

(deftest graph-mode-entry/task-in-single-project-finds-project ()
  "When point is on a task belonging to one project, find that project."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (let* ((project-info (make-project "Test Project"
                                       :id "proj-1"
                                       :tasks '("Task One")))
           (project-marker (plist-get project-info :marker))
           (task-markers (plist-get project-info :task-markers))
           (task-marker (car task-markers)))
      (basic-save-buffer)
      ;; Go to task heading
      (goto-char (marker-position task-marker))
      (org-back-to-heading t)
      ;; Find project should return project marker
      (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
        (assert-true found-marker)
        (assert-equal (marker-position project-marker)
                      (marker-position found-marker))))))

(deftest graph-mode-entry/project-heading-returns-itself ()
  "When point is on a project heading, return that project (regression test)."
  (with-current-buffer (org-gtd--default-file)
    (let* ((project-info (make-project "Direct Project" :id "direct-proj"))
           (project-marker (plist-get project-info :marker)))
      (basic-save-buffer)
      ;; Go to project heading
      (goto-char (marker-position project-marker))
      ;; Find project should return the same marker
      (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
        (assert-true found-marker)
        (assert-equal (marker-position found-marker)
                      (marker-position project-marker))))))

(deftest graph-mode-entry/task-in-multiple-projects-prompts-selection ()
  "When task is in multiple projects, prompt user to select."
  (with-current-buffer (org-gtd--default-file)
    ;; Create two projects
    (let* ((proj1-info (make-project "Project Alpha" :id "proj-alpha"))
           (proj2-info (make-project "Project Beta" :id "proj-beta"))
           (proj1-marker (plist-get proj1-info :marker))
           (proj2-marker (plist-get proj2-info :marker)))

      ;; Create a task that belongs to both projects
      (goto-char (point-max))
      (let ((task-marker (make-task "Shared Task"
                                    :id "shared-task"
                                    :project-ids '("proj-alpha" "proj-beta")
                                    :level 1)))
        (basic-save-buffer)
        ;; Go to task
        (goto-char (marker-position task-marker))

        ;; Simulate selecting "Project Beta"
        (with-simulated-input "Project SPC Beta RET"
          (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
            (assert-true found-marker)
            (assert-equal (marker-position found-marker)
                          (marker-position proj2-marker))))))))

(deftest graph-mode-entry/task-not-in-project-shows-message ()
  "When task has no ORG_GTD_PROJECT_IDS, show message and return nil."
  (with-current-buffer (org-gtd--default-file)
    ;; Create a standalone single action (no project)
    (goto-char (point-max))
    (let ((task-marker (make-task "Standalone Action"
                                  :id "standalone-task"
                                  :level 1)))
      ;; Task has ORG_GTD but no PROJECT_IDS (make-task doesn't add PROJECT_IDS by default)
      (basic-save-buffer)
      (goto-char (marker-position task-marker))

      ;; Find project should return nil
      (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
        (assert-nil found-marker)))))

(deftest graph-mode-entry/non-gtd-heading-shows-message ()
  "When heading has no ORG_GTD property, show message and return nil."
  (with-current-buffer (org-gtd--default-file)
    ;; Create a plain org heading (no GTD properties)
    (goto-char (point-max))
    (insert "* Regular Heading\n")
    (forward-line -1)
    (org-back-to-heading t)
    (basic-save-buffer)

    ;; Find project should return nil
    (let ((found-marker (org-gtd-graph-mode--find-project-at-point)))
      (assert-nil found-marker))))

(deftest graph-mode-entry/from-agenda-finds-project ()
  "When called from org-agenda on a task, find its project."
  (with-current-buffer (org-gtd--default-file)
    (let* ((project-info (make-project "Agenda Project"
                                       :id "agenda-proj"
                                       :tasks '((:description "Agenda Task"
                                                 :id "agenda-task"
                                                 :status next))))
           (project-marker (plist-get project-info :marker))
           (task-ids (plist-get project-info :task-ids))
           (task-id (car task-ids)))
      (basic-save-buffer)

      ;; Build TODO agenda (shows NEXT tasks)
      (let ((org-agenda-files (list (buffer-file-name))))
        (org-todo-list "NEXT")

        (with-current-buffer org-agenda-buffer-name
          ;; Find our task in agenda
          (goto-char (point-min))
          (search-forward "Agenda Task")
          (beginning-of-line)

          ;; Should find project from agenda context
          (let ((found-marker (org-gtd-graph-mode--find-project-at-point-or-agenda)))
            (assert-true found-marker)
            (assert-equal (marker-position project-marker)
                          (marker-position found-marker))))

        ;; Clean up agenda buffer
        (when (get-buffer org-agenda-buffer-name)
          (kill-buffer org-agenda-buffer-name))))))

(deftest graph-mode-entry/agenda-item-not-in-project-shows-message ()
  "When agenda item is not in a project, show message."
  (with-current-buffer (org-gtd--default-file)
    ;; Create standalone action (next, so it appears in TODO agenda)
    (goto-char (point-max))
    (let ((task-marker (make-task "Standalone Next"
                                  :id "standalone-next"
                                  :status 'next
                                  :level 1)))
      (goto-char (marker-position task-marker))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer)

      ;; Build TODO agenda (shows NEXT tasks)
      (let ((org-agenda-files (list (buffer-file-name))))
        (org-todo-list "NEXT")

        (with-current-buffer org-agenda-buffer-name
          ;; Find our task in agenda
          (goto-char (point-min))
          (search-forward "Standalone Next")
          (beginning-of-line)

          ;; Should return nil
          (let ((found-marker (org-gtd-graph-mode--find-project-at-point-or-agenda)))
            (assert-nil found-marker)))

        ;; Clean up
        (when (get-buffer org-agenda-buffer-name)
          (kill-buffer org-agenda-buffer-name))))))

(provide 'graph-mode-entry-test)

;;; graph-mode-entry-test.el ends here
