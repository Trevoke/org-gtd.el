;;; project-operations-test.el --- Tests for simple project operations -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for simple project operations (add-successor, add-blocker, add-root).
;; These test the prompt-based UX for org/agenda contexts.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))
(require 'org-gtd-project-operations)
(require 'org-gtd-context)
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;;; Helper Functions

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

;;;; Add Successor Tests

(deftest project-ops/add-successor-simple-creates-task ()
  "Simple add-successor creates task with dependency."
  (let* ((result (ogt-create-project-with-task "Simple Project" "Predecessor"))
         (project-marker (car result))
         (predecessor-id (cdr result)))

    ;; Get context at predecessor task
    (org-with-point-at (org-id-find predecessor-id t)
      (with-simulated-input "New C-q SPC Successor RET"
        (org-gtd-add-successor--simple-with-context (org-gtd-context-at-point))))

    ;; Verify task created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "New Successor" nil t)))

    ;; Verify dependency: predecessor blocks new successor
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "New Successor")
      (org-back-to-heading t)
      (let ((new-task-id (org-id-get)))
        (assert-true (member predecessor-id (org-gtd-get-task-dependencies new-task-id)))))))

(deftest project-ops/add-successor-simple-errors-without-task ()
  "Simple add-successor errors when no task at point."
  (let* ((result (ogt-create-project-with-task "Error Project" "Task"))
         (project-marker (car result)))

    ;; Create context with no task-id (simulating being on project heading)
    (let ((ctx (org-gtd-context-create
                :mode 'org
                :project-marker project-marker
                :project-id (org-with-point-at project-marker (org-id-get))
                :task-id nil
                :task-marker nil)))
      (assert-raises 'user-error
        (org-gtd-add-successor--simple-with-context ctx)))))

(deftest project-ops/add-successor-simple-errors-with-empty-title ()
  "Simple add-successor errors when title is empty."
  (let* ((result (ogt-create-project-with-task "Empty Title Project" "Task"))
         (predecessor-id (cdr result)))

    (org-with-point-at (org-id-find predecessor-id t)
      (assert-raises 'user-error
        (with-simulated-input "RET"
          (org-gtd-add-successor--simple-with-context (org-gtd-context-at-point)))))))

;;;; Add Blocker Tests

(deftest project-ops/add-blocker-simple-creates-task ()
  "Simple add-blocker creates task that blocks task at point."
  (let* ((result (ogt-create-project-with-task "Blocker Project" "Blocked Task"))
         (project-marker (car result))
         (blocked-id (cdr result)))

    ;; Get context at blocked task
    (org-with-point-at (org-id-find blocked-id t)
      (with-simulated-input "New C-q SPC Blocker RET"
        (org-gtd-add-blocker--simple-with-context (org-gtd-context-at-point))))

    ;; Verify task created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "New Blocker" nil t)))

    ;; Verify dependency: new blocker blocks the original task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "New Blocker")
      (org-back-to-heading t)
      (let ((new-task-id (org-id-get)))
        (assert-true (member blocked-id (org-gtd-get-task-blockers new-task-id)))))))

(deftest project-ops/add-blocker-simple-adds-to-first-tasks ()
  "Simple add-blocker adds new task to FIRST_TASKS since it has no blockers."
  (let* ((result (ogt-create-project-with-task "First Tasks Project" "Original Task"))
         (project-marker (car result))
         (blocked-id (cdr result)))

    ;; Get context at blocked task
    (org-with-point-at (org-id-find blocked-id t)
      (with-simulated-input "Root C-q SPC Blocker RET"
        (org-gtd-add-blocker--simple-with-context (org-gtd-context-at-point))))

    ;; Verify new blocker is in FIRST_TASKS
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Root Blocker")
      (org-back-to-heading t)
      (let ((new-task-id (org-id-get)))
        (org-with-point-at project-marker
          (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
            (assert-true (member new-task-id first-tasks))))))))

;;;; Add Root Tests

(deftest project-ops/add-root-simple-creates-task ()
  "Simple add-root creates task with no dependencies."
  (let* ((result (ogt-create-project-with-task "Root Project" "Existing Task"))
         (project-marker (car result))
         (existing-task-id (cdr result)))

    ;; Get context at existing task (we need to be somewhere in the project)
    (org-with-point-at (org-id-find existing-task-id t)
      (with-simulated-input "New C-q SPC Root C-q SPC Task RET"
        (org-gtd-add-root--simple-with-context (org-gtd-context-at-point))))

    ;; Verify task created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "New Root Task" nil t)))

    ;; Verify task is in FIRST_TASKS
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "New Root Task")
      (org-back-to-heading t)
      (let ((new-task-id (org-id-get)))
        (org-with-point-at project-marker
          (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
            (assert-true (member new-task-id first-tasks))))))))

(deftest project-ops/add-root-simple-no-dependencies ()
  "Simple add-root creates task with no blockers or successors."
  (let* ((result (ogt-create-project-with-task "Independent Root Project" "Existing"))
         (existing-task-id (cdr result)))

    (org-with-point-at (org-id-find existing-task-id t)
      (with-simulated-input "Independent C-q SPC Root RET"
        (org-gtd-add-root--simple-with-context (org-gtd-context-at-point))))

    ;; Find new task and verify it has no dependencies
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Independent Root")
      (org-back-to-heading t)
      (let ((new-task-id (org-id-get)))
        (assert-equal nil (org-gtd-get-task-dependencies new-task-id))
        (assert-equal nil (org-gtd-get-task-blockers new-task-id))))))

;;;; Context Integration Tests

(deftest project-ops/works-from-org-buffer-context ()
  "Simple operations work from org buffer context."
  (let* ((result (ogt-create-project-with-task "Org Buffer Project" "Org Task"))
         (task-id (cdr result)))

    ;; In org buffer (not agenda), at the task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (org-id-find task-id t))
      (let ((ctx (org-gtd-context-at-point)))
        (assert-equal 'org (org-gtd-context-mode ctx))
        (assert-equal task-id (org-gtd-context-task-id ctx))))))

(provide 'project-operations-test)
;;; project-operations-test.el ends here
