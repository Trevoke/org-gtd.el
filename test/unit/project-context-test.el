;;; project-context-test.el --- Tests for project context helpers -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-project--get-marker-at-point helper function.
;; This function resolves which project a task belongs to.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))
(require 'org-gtd-projects)
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

(deftest project-context/get-marker-at-point-single-project ()
  "Returns project marker when task belongs to single project."
  (let (project-id task-marker)
    ;; Create project with task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Test Project\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")
      (org-end-of-subtree t t)
      (insert "** TODO Task 1\n")
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
        (setq task-marker (point-marker)))
      (basic-save-buffer))

    ;; At task point, should get project marker
    (org-with-point-at task-marker
      (let ((result (org-gtd-project--get-marker-at-point)))
        (assert-true (markerp result))
        (org-with-point-at result
          (assert-equal "Test Project" (org-get-heading t t t t)))))))

(deftest project-context/get-marker-at-point-multi-project-prompts ()
  "Prompts user when task belongs to multiple projects."
  (let (project-a-id project-b-id task-marker)
    ;; Create two projects
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Project Alpha\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")

      (goto-char (point-max))
      (insert "* Project Beta\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-b-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")

      ;; Create task belonging to both
      (goto-char (point-max))
      (insert "* TODO Shared Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" (concat project-a-id " " project-b-id))
        (setq task-marker (point-marker)))
      (basic-save-buffer))

    ;; At task point, should prompt and return chosen project
    (org-with-point-at task-marker
      (with-simulated-input "Project SPC Beta RET"
        (let ((result (org-gtd-project--get-marker-at-point)))
          (assert-true (markerp result))
          (org-with-point-at result
            (assert-equal "Project Beta" (org-get-heading t t t t))))))))

(deftest project-context/get-marker-at-point-errors-on-non-project-task ()
  "Errors when task has no project IDs."
  (let (task-marker)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* TODO Single Action\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      ;; No ORG_GTD_PROJECT_IDS
      (setq task-marker (point-marker))
      (basic-save-buffer))

    (org-with-point-at task-marker
      (assert-raises 'user-error
        (org-gtd-project--get-marker-at-point)))))

(deftest project-context/get-marker-from-context-graph-view ()
  "Returns buffer-local marker when in graph-view-mode."
  (let (project-marker)
    ;; Create a project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Graph Project\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Projects")
      (setq project-marker (point-marker))
      (basic-save-buffer))

    ;; Simulate being in graph view
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)
      (let ((result (org-gtd-project--get-marker-from-context)))
        (assert-true (eq project-marker result))))))

(deftest project-context/get-marker-from-context-agenda ()
  "Returns project marker when called from agenda on project task."
  (let (project-id task-marker)
    ;; Create project with task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Agenda Test Project\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq project-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Projects")
      (org-end-of-subtree t t)
      (insert "** TODO Agenda Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (org-id-get-create)
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (setq task-marker (point-marker))
      (basic-save-buffer))

    ;; Create mock agenda buffer with task marker
    (with-temp-buffer
      (org-agenda-mode)
      (insert "  agenda:  TODO Agenda Task")
      (put-text-property (point-min) (point-max) 'org-marker task-marker)
      (goto-char (point-min))

      ;; Call from agenda context should return project marker
      (let ((result (org-gtd-project--get-marker-from-context)))
        (assert-true (markerp result))
        (org-with-point-at result
          (assert-equal "Agenda Test Project" (org-get-heading t t t t)))))))

(deftest project-context/get-marker-from-context-errors-outside-valid-context ()
  "Errors when not in graph view or agenda."
  ;; In a plain buffer (not graph view, not agenda)
  (with-temp-buffer
    (assert-raises 'user-error
      (org-gtd-project--get-marker-from-context))))

(provide 'project-context-test)
;;; project-context-test.el ends here
