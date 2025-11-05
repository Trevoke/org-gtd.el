;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-graph-transient-test.el --- Unit tests for graph transient menu -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-transient menu system.
;;
;; Test Coverage:
;; - org-gtd-graph-transient-main (main menu)
;; - Add commands (child, sibling, root)
;; - Edit commands (properties, todo, schedule, deadline)
;; - Placeholder commands for future phases
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-graph-transient)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-ui)
(require 'org-gtd-files)
(require 'org-gtd-core)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))
(require 'with-simulated-input)

(defun org-gtd-graph-transient-test--create-project (title)
  "Create a test project with TITLE and return its marker."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* %s\n" title))
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (let ((id (org-id-get-create)))
      (insert (format ":ID: %s\n" id))
      (insert ":END:\n")
      (insert "** TODO Task 1\n")
      (insert ":PROPERTIES:\n")
      (let ((task-id (org-id-get-create)))
        (insert (format ":ID: %s\n" task-id))
        (insert ":ORG_GTD: Actions\n")
        (insert ":END:\n")
        ;; Use proper function to set properties
        (org-gtd-add-to-multivalued-property task-id org-gtd-prop-project-ids id)
        ;; Go back to project and add task to FIRST_TASKS
        (goto-char (point-min))
        (search-forward title)
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        (basic-save-buffer)
        (point-marker)))))

;;;; org-gtd-graph-transient-main Tests

(describe "org-gtd-graph-transient-main"

  (it "is a valid transient prefix"
    (expect (functionp 'org-gtd-graph-transient-main) :to-be-truthy)
    (expect (get 'org-gtd-graph-transient-main 'transient--prefix) :not :to-be nil))

  (it "can be invoked interactively"
    (expect (commandp 'org-gtd-graph-transient-main) :to-be-truthy)))

;;;; Placeholder Command Tests

(describe "Placeholder commands for future phases"

  (it "navigation commands are defined and callable"
    (expect (commandp 'org-gtd-graph-nav-next-sibling) :to-be-truthy)
    (expect (commandp 'org-gtd-graph-nav-goto) :to-be-truthy))

  (it "view commands are defined and callable"
    (expect (commandp 'org-gtd-graph-transient-zoom) :to-be-truthy))

  (it "undo/redo commands are defined and callable"
    (expect (commandp 'org-gtd-graph-undo) :to-be-truthy)
    (expect (commandp 'org-gtd-graph-redo) :to-be-truthy))

  (it "quit command is defined and callable"
    (expect (commandp 'org-gtd-graph-quit-and-kill) :to-be-truthy)))

;;;; org-gtd-graph-transient-add-child Tests

(describe "org-gtd-graph-transient-add-child"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "creates a child task under selected node"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Test Project"))
           (buffer (get-buffer-create "*Org GTD Graph: test*"))
           parent-task-id)

      ;; Get the project's first task ID
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (setq parent-task-id (org-entry-get (point) "ID")))

      ;; Set up graph view buffer with selected node
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id parent-task-id)
        ;; Mock refresh function
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "Child C-q SPC Task RET"
            (org-gtd-graph-transient-add-child))))

      ;; Verify child task was created
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        ;; Move to end of parent subtree to find child
        (org-end-of-subtree t t)
        (org-back-to-heading t)
        (let ((heading (org-get-heading t t t t)))
          (expect heading :to-equal "Child Task")
          ;; Verify it's a child (level 3)
          (expect (org-current-level) :to-equal 3)))))

  (it "creates dependency relationship with parent"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Dep Project"))
           (buffer (get-buffer-create "*Org GTD Graph: dep-test*"))
           parent-task-id child-task-id)

      ;; Get the parent task ID
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (setq parent-task-id (org-entry-get (point) "ID")))

      ;; Set up graph view and create child
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id parent-task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "Dependent C-q SPC Child RET"
            (org-gtd-graph-transient-add-child))))

      ;; Get child task ID
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Dependent Child")
        (org-back-to-heading t)
        (setq child-task-id (org-entry-get (point) "ID")))

      ;; Verify dependency: parent blocks child
      (expect (org-gtd-get-task-dependencies child-task-id) :to-equal (list parent-task-id))))

  (it "refreshes the graph after creation"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Refresh Project"))
           (buffer (get-buffer-create "*Org GTD Graph: refresh-test*"))
           (refresh-called nil)
           parent-task-id)

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (setq parent-task-id (org-entry-get (point) "ID")))

      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id parent-task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh)
                   (lambda () (setq refresh-called t))))
          (with-simulated-input "Refresh C-q SPC Child RET"
            (org-gtd-graph-transient-add-child))))

      (expect refresh-called :to-be-truthy))))

;;;; org-gtd-graph-transient-add-root Tests

(describe "org-gtd-graph-transient-add-root"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "creates a new root task in the project"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Root Project"))
           (buffer (get-buffer-create "*Org GTD Graph: root-test*"))
           project-id)

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Root Project")
        (org-back-to-heading t)
        (setq project-id (org-entry-get (point) "ID")))

      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "New SPC Root RET"
            (org-gtd-graph-transient-add-root))))

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "New Root")
        (org-back-to-heading t)
        (let ((task-id (org-entry-get (point) "ID")))
          (expect task-id :not :to-be nil)
          (goto-char (point-min))
          (search-forward "Root Project")
          (org-back-to-heading t)
          (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
            (expect (member task-id first-tasks) :to-be-truthy)))))))

;;;; org-gtd-graph-insert-before Tests

(describe "org-gtd-graph-insert-before"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "inserts before root task (no blockers) and makes new task the root"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Insert Before Root"))
           (buffer (get-buffer-create "*Org GTD Graph: insert-before-root*"))
           project-id selected-task-id new-task-id)

      ;; Get project ID and task ID
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Insert Before Root")
        (org-back-to-heading t)
        (setq project-id (org-entry-get (point) "ID"))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (setq selected-task-id (org-entry-get (point) "ID")))

      ;; Verify selected task is root
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Insert Before Root")
        (org-back-to-heading t)
        (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
          (expect (member selected-task-id first-tasks) :to-be-truthy)))

      ;; Set up graph view and insert before
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id selected-task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewPredecessor RET"
            (org-gtd-graph-insert-before))))

      ;; Verify new task created
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "NewPredecessor")
        (org-back-to-heading t)
        (setq new-task-id (org-entry-get (point) "ID"))
        (expect new-task-id :not :to-be nil))

      ;; Verify new task is now root (in FIRST_TASKS)
      ;; Use the same marker context instead of switching buffers
      (org-with-point-at project-marker
        (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
          (expect (member new-task-id first-tasks) :to-be-truthy)
          (expect (member selected-task-id first-tasks) :not :to-be-truthy)))

      ;; Verify dependency: new → selected
      (expect (org-gtd-get-task-dependencies selected-task-id) :to-equal (list new-task-id))))

  (it "inserts before task with single blocker and rewires dependencies"
    (pending "Test implementation needed"))

  (it "inserts before task with multiple blockers and prompts user"
    (pending "Test implementation needed")))

(provide 'org-gtd-graph-transient-test)

;;; org-gtd-graph-transient-test.el ends here
