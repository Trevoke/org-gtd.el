;;; project-task-commands-test.el --- Tests for project task commands -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for project task commands (add-successor, add-blocker, add-root, etc).
;; These commands work from both graph view and agenda contexts.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))
(require 'org-gtd-projects)
(require 'org-gtd-graph-data)
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

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

(deftest project-task-commands/add-successor-from-graph-view ()
  "Add successor works from graph view context.
Uses internal function since transient UI can't be tested with simulated input."
  (let* ((result (ogt-create-project-with-task "Successor Project" "First Task"))
         (project-marker (car result))
         (first-task-id (cdr result)))

    ;; Simulate graph view context and verify context detection works
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)

      ;; Verify context helper returns correct marker
      (let ((detected-marker (org-gtd-project--get-marker-from-context)))
        (assert-true (eq project-marker detected-marker))))

    ;; Use internal function directly (like existing graph-transient tests)
    (require 'org-gtd-graph-transient)
    (let ((new-task-id (org-gtd-graph--add-successor-internal
                        "New Successor"
                        (list first-task-id)
                        project-marker)))

      ;; Verify successor was created
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (assert-true (search-forward "New Successor" nil t)))

      ;; Verify dependency: first-task blocks new-task
      (assert-true (member first-task-id (org-gtd-get-task-dependencies new-task-id))))))

(deftest project-task-commands/add-blocker-from-graph-view ()
  "Add blocker works from graph view context.
Uses internal function since transient UI can't be tested with simulated input."
  (let* ((result (ogt-create-project-with-task "Blocker Project" "First Task"))
         (project-marker (car result))
         (first-task-id (cdr result)))

    ;; Simulate graph view context and verify context detection works
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)

      ;; Verify context helper returns correct marker
      (let ((detected-marker (org-gtd-project--get-marker-from-context)))
        (assert-true (eq project-marker detected-marker))))

    ;; Use internal function directly
    (require 'org-gtd-graph-transient)
    (let ((new-task-id (org-gtd-graph--add-blocker-internal
                        "New Blocker"
                        (list first-task-id)
                        project-marker)))

      ;; Verify blocker was created
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (assert-true (search-forward "New Blocker" nil t)))

      ;; Verify dependency: new-task blocks first-task
      (assert-true (member first-task-id (org-gtd-get-task-blockers new-task-id))))))

(deftest project-task-commands/add-root-from-graph-view ()
  "Add root task works from graph view context.
Uses internal function since transient UI can't be tested with simulated input."
  (let* ((result (ogt-create-project-with-task "Root Project" "First Task"))
         (project-marker (car result)))

    ;; Simulate graph view context and verify context detection works
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)

      ;; Verify context helper returns correct marker
      (let ((detected-marker (org-gtd-project--get-marker-from-context)))
        (assert-true (eq project-marker detected-marker))))

    ;; Use internal function directly
    (require 'org-gtd-graph-transient)
    (let ((new-task-id (org-gtd-graph--add-root-internal
                        "New Root Task"
                        project-marker)))

      ;; Verify root task was created
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (assert-true (search-forward "New Root Task" nil t)))

      ;; Verify task is in FIRST_TASKS (root task has no blockers)
      (org-with-point-at project-marker
        (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
          (assert-true (member new-task-id first-tasks)))))))

(deftest project-task-commands/incubate-from-graph-view ()
  "Incubate project works from graph view context."
  (let* ((result (ogt-create-project-with-task "Incubate Project" "Task"))
         (project-marker (car result)))

    ;; Simulate graph view context and call function inside that context
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)

      ;; Verify context helper returns correct marker
      (let ((detected-marker (org-gtd-project--get-marker-from-context)))
        (assert-true (eq project-marker detected-marker)))

      ;; Call incubate with simulated date input
      (with-simulated-input "2025-12-25 RET"
        (org-gtd-project-incubate-from-context)))

    ;; Verify project is incubated (ORG_GTD = Tickler)
    (org-with-point-at project-marker
      (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD")))))

(deftest project-task-commands/incubate-from-agenda-context ()
  "Incubate project works from agenda context on a project task."
  (let* ((result (ogt-create-project-with-task "Agenda Incubate Project" "AgendaTask"))
         (project-marker (car result))
         (task-id (cdr result))
         (task-marker (org-id-find task-id t)))

    ;; Simulate agenda context pointing at the TASK (not project heading)
    (with-temp-buffer
      (org-agenda-mode)
      (insert "  agenda:  TODO AgendaTask")
      (put-text-property (point-min) (point-max) 'org-marker task-marker)
      (goto-char (point-min))

      ;; Call incubate with simulated date input
      (with-simulated-input "2025-12-25 RET"
        (org-gtd-project-incubate-from-context)))

    ;; Verify project heading has ORG_GTD = Tickler
    (org-with-point-at project-marker
      (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD")))

    ;; Verify project task also has ORG_GTD = Tickler
    (org-with-point-at task-marker
      (assert-equal "Tickler" (org-entry-get (point) "ORG_GTD")))))

(deftest project-task-commands/cancel-from-graph-view ()
  "Cancel project works from graph view context."
  (let* ((result (ogt-create-project-with-task "Cancel Project" "CancelTask"))
         (project-marker (car result))
         (task-id (cdr result)))

    ;; Simulate graph view context and call function inside that context
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)

      ;; Verify context helper returns correct marker
      (let ((detected-marker (org-gtd-project--get-marker-from-context)))
        (assert-true (eq project-marker detected-marker)))

      ;; Stub yes-or-no-p to return t (since simulated-input doesn't work well with it)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (org-gtd-project-cancel-from-context)))

    ;; Verify project task is canceled using the task ID directly
    (when-let ((task-marker (org-id-find task-id t)))
      (org-with-point-at task-marker
        (assert-equal "CNCL" (org-get-todo-state))))))

(provide 'project-task-commands-test)
;;; project-task-commands-test.el ends here
