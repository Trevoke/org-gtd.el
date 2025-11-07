;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-graph-remove-task-test.el --- Tests for removing tasks from project graphs -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; TDD tests for remove/trash operations on project graph tasks.
;;
;; Test Coverage:
;; - Remove from simple chain (A → B → C)
;; - Remove root task
;; - Remove from multiple projects
;; - Keep as independent
;; - Trash from multiple projects
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-graph-transient)
(require 'org-gtd-graph-data)
(require 'org-gtd-graph-view)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)
(require 'org-gtd-files)
(require 'org-gtd-core)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))

;;;; Test Setup
;; Uses standard infrastructure directly - no custom wrappers

(defun org-gtd-graph-remove-test--create-project-with-diamond (project-title)
  "Create a test project with diamond structure: A → C ← B, C → D.
Task C has two parents (A and B) and one successor (D).
Returns alist with keys: project-marker, project-id, task-a-id, task-b-id, task-c-id, task-d-id."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* %s\n:PROPERTIES:\n:ORG_GTD: Projects\n" project-title))
    (let ((project-id (org-id-get-create)))
      (insert (format ":ID: %s\n:END:\n" project-id))
      (insert "** TODO Task A\n:PROPERTIES:\n")
      (let ((task-a-id (org-id-get-create)))
        (insert (format ":ID: %s\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: %s\n:END:\n" task-a-id project-id))
        (insert "** TODO Task B\n:PROPERTIES:\n")
        (let ((task-b-id (org-id-get-create)))
          (insert (format ":ID: %s\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: %s\n:END:\n" task-b-id project-id))
          (insert "** TODO Task C\n:PROPERTIES:\n")
          (let ((task-c-id (org-id-get-create)))
            (insert (format ":ID: %s\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: %s\n:END:\n" task-c-id project-id))
            (insert "** TODO Task D\n:PROPERTIES:\n")
            (let ((task-d-id (org-id-get-create)))
              (insert (format ":ID: %s\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: %s\n:END:\n" task-d-id project-id))
              (org-gtd-dependencies-create task-a-id task-c-id)
              (org-gtd-dependencies-create task-b-id task-c-id)
              (org-gtd-dependencies-create task-c-id task-d-id)
              (org-gtd-add-to-multivalued-property task-a-id org-gtd-prop-project-ids project-id)
              (org-gtd-add-to-multivalued-property task-b-id org-gtd-prop-project-ids project-id)
              (org-gtd-add-to-multivalued-property task-c-id org-gtd-prop-project-ids project-id)
              (org-gtd-add-to-multivalued-property task-d-id org-gtd-prop-project-ids project-id)
              (goto-char (point-min))
              (search-forward project-title)
              (org-back-to-heading t)
              (org-entry-put (point) "ORG_GTD_FIRST_TASKS" (format "%s %s" task-a-id task-b-id))
              (let ((project-marker (point-marker)))
                (basic-save-buffer)
                `((project-marker . ,project-marker)
                  (project-id . ,project-id)
                  (task-a-id . ,task-a-id)
                  (task-b-id . ,task-b-id)
                  (task-c-id . ,task-c-id)
                  (task-d-id . ,task-d-id))))))))))

(defun org-gtd-graph-remove-test--create-project-with-chain (project-title)
  "Create a test project with chain A → B → C.
Returns alist with keys: project-marker, project-id, task-a-id, task-b-id, task-c-id."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))

    ;; Create project heading
    (insert (format "* %s\n" project-title))
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (let ((project-id (org-id-get-create)))
      (insert (format ":ID: %s\n" project-id))
      (insert ":END:\n")

      ;; Create Task A (root)
      (insert "** TODO Task A\n")
      (insert ":PROPERTIES:\n")
      (let ((task-a-id (org-id-get-create)))
        (insert (format ":ID: %s\n" task-a-id))
        (insert ":ORG_GTD: Actions\n")
        (insert (format ":ORG_GTD_PROJECT_IDS: %s\n" project-id))
        (insert ":END:\n")

        ;; Create Task B (middle)
        (insert "** TODO Task B\n")
        (insert ":PROPERTIES:\n")
        (let ((task-b-id (org-id-get-create)))
          (insert (format ":ID: %s\n" task-b-id))
          (insert ":ORG_GTD: Actions\n")
          (insert (format ":ORG_GTD_PROJECT_IDS: %s\n" project-id))
          (insert ":END:\n")

          ;; Create Task C (leaf)
          (insert "** TODO Task C\n")
          (insert ":PROPERTIES:\n")
          (let ((task-c-id (org-id-get-create)))
            (insert (format ":ID: %s\n" task-c-id))
            (insert ":ORG_GTD: Actions\n")
            (insert (format ":ORG_GTD_PROJECT_IDS: %s\n" project-id))
            (insert ":END:\n")

            ;; Create dependencies using the proper function
            (org-gtd-dependencies-create task-a-id task-b-id)
            (org-gtd-dependencies-create task-b-id task-c-id)

            ;; Set project membership using proper function (not just text insertion)
            (org-gtd-add-to-multivalued-property task-a-id org-gtd-prop-project-ids project-id)
            (org-gtd-add-to-multivalued-property task-b-id org-gtd-prop-project-ids project-id)
            (org-gtd-add-to-multivalued-property task-c-id org-gtd-prop-project-ids project-id)

            ;; Set FIRST_TASKS
            (goto-char (point-min))
            (search-forward project-title)
            (org-back-to-heading t)
            (org-entry-put (point) "ORG_GTD_FIRST_TASKS" task-a-id)

            (let ((project-marker (point-marker)))
              (basic-save-buffer)
              `((project-marker . ,project-marker)
                (project-id . ,project-id)
                (task-a-id . ,task-a-id)
                (task-b-id . ,task-b-id)
                (task-c-id . ,task-c-id)))))))))

;;;; Test 1: Remove from Simple Chain

(describe "org-gtd-graph--remove-from-project"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "removes middle task and rewires chain A → B → C to A → C"
    (let* ((project-data (org-gtd-graph-remove-test--create-project-with-chain "Chain Project"))
           (project-id (cdr (assoc 'project-id project-data)))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data)))
           (task-c-id (cdr (assoc 'task-c-id project-data))))

      ;; Verify initial state: A → B → C
      (let ((b-deps (org-gtd-get-task-dependencies task-b-id))
            (c-deps (org-gtd-get-task-dependencies task-c-id))
            (a-blocks (org-gtd-get-task-blockers task-a-id))
            (b-blocks (org-gtd-get-task-blockers task-b-id)))
        (expect b-deps :to-have-same-items-as (list task-a-id))
        (expect c-deps :to-have-same-items-as (list task-b-id))
        (expect a-blocks :to-have-same-items-as (list task-b-id))
        (expect b-blocks :to-have-same-items-as (list task-c-id)))

      ;; Remove Task B
      (org-gtd-graph--remove-from-project task-b-id project-id)

      ;; Verify rewiring: A → C
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-c-id))
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list task-a-id))

      ;; Verify B removed from project
      (expect (org-gtd-get-task-projects task-b-id) :not :to-contain project-id)

      ;; Verify B no longer blocks C or depends on A
      (expect (org-gtd-get-task-dependencies task-b-id) :not :to-contain task-a-id)
      (expect (org-gtd-get-task-blockers task-b-id) :not :to-contain task-c-id))))

;;;; Test 2: Remove Root Task

(describe "org-gtd-graph--remove-from-project for root task"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "removes root task and promotes successor to root"
    (let* ((project-data (org-gtd-graph-remove-test--create-project-with-chain "Root Remove Project"))
           (project-id (cdr (assoc 'project-id project-data)))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data)))
           (task-c-id (cdr (assoc 'task-c-id project-data))))

      ;; Verify initial state: FIRST_TASKS = [A], A → B → C
      (let ((first-tasks (org-with-point-at project-marker
                          (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
        (expect first-tasks :to-have-same-items-as (list task-a-id)))

      ;; Remove Task A (root)
      (org-gtd-graph--remove-from-project task-a-id project-id)

      ;; Verify B is now root
      (let ((first-tasks (org-with-point-at project-marker
                          (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
        (expect first-tasks :to-have-same-items-as (list task-b-id)))

      ;; Verify A no longer in FIRST_TASKS
      (let ((first-tasks (org-with-point-at project-marker
                          (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
        (expect first-tasks :not :to-contain task-a-id))

      ;; Verify B no longer depends on A
      (expect (org-gtd-get-task-dependencies task-b-id) :not :to-contain task-a-id)

      ;; Verify A removed from project
      (expect (org-gtd-get-task-projects task-a-id) :not :to-contain project-id))))

;;;; Test 3: Remove Task with Multiple Parents

(describe "org-gtd-graph--remove-from-project for task with multiple parents"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "removes task with two parents and rewires both to successor"
    (let* ((project-data (org-gtd-graph-remove-test--create-project-with-diamond "Diamond Project"))
           (project-id (cdr (assoc 'project-id project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data)))
           (task-c-id (cdr (assoc 'task-c-id project-data)))
           (task-d-id (cdr (assoc 'task-d-id project-data))))

      ;; Verify initial state: A → C, B → C, C → D
      (expect (org-gtd-get-task-blockers task-a-id) :to-have-same-items-as (list task-c-id))
      (expect (org-gtd-get-task-blockers task-b-id) :to-have-same-items-as (list task-c-id))
      (expect (org-gtd-get-task-dependencies task-c-id) :to-have-same-items-as (list task-a-id task-b-id))
      (expect (org-gtd-get-task-blockers task-c-id) :to-have-same-items-as (list task-d-id))
      (expect (org-gtd-get-task-dependencies task-d-id) :to-have-same-items-as (list task-c-id))

      ;; Remove Task C (which has two parents)
      (org-gtd-graph--remove-from-project task-c-id project-id)

      ;; Verify both parents now block D: A → D, B → D
      (expect (org-gtd-get-task-blockers task-a-id) :to-have-same-items-as (list task-d-id))
      (expect (org-gtd-get-task-blockers task-b-id) :to-have-same-items-as (list task-d-id))

      ;; Verify D now depends on both A and B
      (expect (org-gtd-get-task-dependencies task-d-id) :to-have-same-items-as (list task-a-id task-b-id))

      ;; Verify C removed from project
      (expect (org-gtd-get-task-projects task-c-id) :not :to-contain project-id)

      ;; Verify C's dependencies cleaned up
      (expect (org-gtd-get-task-dependencies task-c-id) :not :to-contain task-a-id)
      (expect (org-gtd-get-task-dependencies task-c-id) :not :to-contain task-b-id)
      (expect (org-gtd-get-task-blockers task-c-id) :not :to-contain task-d-id)

      ;; Verify A and B no longer reference C
      (expect (org-gtd-get-task-blockers task-a-id) :not :to-contain task-c-id)
      (expect (org-gtd-get-task-blockers task-b-id) :not :to-contain task-c-id))))

;;;; Test 4: Keep as Independent

(describe "org-gtd-graph--keep-as-independent"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "removes task from all projects and makes it independent"
    (let* ((project-data (org-gtd-graph-remove-test--create-project-with-chain "Independence Project"))
           (project-id (cdr (assoc 'project-id project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data)))
           (task-c-id (cdr (assoc 'task-c-id project-data))))

      ;; Keep B as independent
      (org-gtd-graph--keep-as-independent task-b-id)

      ;; Verify B not in any project
      (let ((b-projects (org-gtd-get-task-projects task-b-id)))
        (expect (or (null b-projects) (equal b-projects '())) :to-be-truthy))

      ;; Verify project rewired: A → C (plus A still blocks B)
      (expect (org-gtd-get-task-blockers task-a-id) :to-contain task-c-id)
      (expect (org-gtd-get-task-dependencies task-c-id) :to-contain task-a-id)

      ;; Verify B's dependencies PRESERVED (orphaned but kept for future)
      (expect (org-gtd-get-task-dependencies task-b-id) :to-contain task-a-id)
      (expect (org-gtd-get-task-blockers task-b-id) :to-contain task-c-id))))

;;;; Test 5: Trash Task

(describe "org-gtd-graph--trash-task"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "removes task from all projects, cleans all dependencies, and archives it"
    (let* ((project-data (org-gtd-graph-remove-test--create-project-with-chain "Trash Project"))
           (project-id (cdr (assoc 'project-id project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data)))
           (task-c-id (cdr (assoc 'task-c-id project-data))))

      ;; Verify initial state
      (expect (org-gtd-get-task-dependencies task-b-id) :to-contain task-a-id)
      (expect (org-gtd-get-task-blockers task-b-id) :to-contain task-c-id)

      ;; Trash Task B
      (org-gtd-graph--trash-task task-b-id)

      ;; Verify project rewired: A → C
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-c-id))
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list task-a-id))

      ;; Verify A no longer blocks B, C no longer depends on B
      (expect (org-gtd-get-task-blockers task-a-id) :not :to-contain task-b-id)
      (expect (org-gtd-get-task-dependencies task-c-id) :not :to-contain task-b-id)

      ;; Verify B's dependencies completely cleaned up
      (let ((b-deps (org-gtd-get-task-dependencies task-b-id))
            (b-blocks (org-gtd-get-task-blockers task-b-id)))
        (expect (or (null b-deps) (equal b-deps '())) :to-be-truthy)
        (expect (or (null b-blocks) (equal b-blocks '())) :to-be-truthy))

      ;; Verify B not in any project
      (let ((b-projects (org-gtd-get-task-projects task-b-id)))
        (expect (or (null b-projects) (equal b-projects '())) :to-be-truthy))

      ;; Verify B is archived/canceled
      (let ((marker (org-id-find task-b-id t)))
        (if marker
            (org-with-point-at marker
              (let ((todo-state (org-get-todo-state)))
                (expect todo-state :to-equal (org-gtd-keywords--canceled))))
          ;; Task not found means it was archived to a different file
          (expect marker :to-be nil)))))

  (it "does not orphan successors when trashing middle task in chain"
    (let* ((project-data (org-gtd-graph-remove-test--create-project-with-chain "Orphan Test Project"))
           (project-id (cdr (assoc 'project-id project-data)))
           (project-marker (cdr (assoc 'project-marker project-data)))
           (task-a-id (cdr (assoc 'task-a-id project-data)))
           (task-b-id (cdr (assoc 'task-b-id project-data)))
           (task-c-id (cdr (assoc 'task-c-id project-data))))

      ;; Verify initial state: A → B → C, FIRST_TASKS = [A]
      (let ((first-tasks (org-with-point-at project-marker
                          (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
        (expect first-tasks :to-have-same-items-as (list task-a-id)))

      ;; Trash Task B (middle task)
      (org-gtd-graph--trash-task task-b-id)

      ;; Verify rewiring happened: A → C
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-c-id))
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list task-a-id))

      ;; CRITICAL: C should NOT be in FIRST_TASKS because it has predecessor A
      (let ((first-tasks (org-with-point-at project-marker
                          (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
        (expect first-tasks :to-have-same-items-as (list task-a-id))
        (expect first-tasks :not :to-contain task-c-id))

      ;; Verify A is still the only root task
      (let ((first-tasks (org-with-point-at project-marker
                          (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS"))))
        (expect (length first-tasks) :to-equal 1)
        (expect (car first-tasks) :to-equal task-a-id)))))

(provide 'org-gtd-graph-remove-task-test)

;;; org-gtd-graph-remove-task-test.el ends here
