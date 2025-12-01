;;; projects-fix-todo-keywords-test.el --- Tests for org-gtd-projects-fix-todo-keywords -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-projects-fix-todo-keywords with breadth-first dependency traversal.
;; These tests verify that TODO keywords are correctly set based on task dependencies.
;;
;; Migrated from test/projects-fix-todo-keywords-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem (provides org-todo-keywords setup)
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Simple Sequential Chain Tests

(deftest fix-keywords/first-task-next-when-no-deps-satisfied ()
  "Marks first task NEXT when no dependencies satisfied."
  ;; Use a file in the mock filesystem
  (let ((test-file (concat org-gtd-directory "test-project.org")))
    (with-temp-buffer
      (make-project "Project Test"
                    :id "project-id"
                    :first-tasks '("task-a"))
      (make-task "Task A"
                 :id "task-a"
                 :level 2
                 :status 'todo
                 :project-ids '("project-id")
                 :blocks '("task-b"))
      (make-task "Task B"
                 :id "task-b"
                 :level 2
                 :status 'todo
                 :project-ids '("project-id")
                 :depends-on '("task-a"))
      (write-file test-file))

    (with-current-buffer (find-file-noselect test-file)
      (org-id-update-id-locations (list test-file))
      (goto-char (point-min))
      (org-gtd-projects-fix-todo-keywords (point-marker))

      ;; Task A should be NEXT
      (goto-char (point-min))
      (search-forward "Task A")
      (assert-equal "NEXT" (org-entry-get (point) "TODO"))

      ;; Task B should be TODO
      (goto-char (point-min))
      (search-forward "Task B")
      (assert-equal "TODO" (org-entry-get (point) "TODO")))))

(deftest fix-keywords/next-task-marked-after-first-completes ()
  "Marks next task NEXT after completing first task."
  (with-temp-buffer
    (org-mode)
    (make-project "Project Test"
                  :id "project-id"
                  :first-tasks '("task-a"))
    (make-task "Task A"
               :id "task-a"
               :level 2
               :status 'done  ;; Task A is done
               :project-ids '("project-id")
               :blocks '("task-b"))
    (make-task "Task B"
               :id "task-b"
               :level 2
               :status 'todo
               :project-ids '("project-id")
               :depends-on '("task-a"))

    (goto-char (point-min))
    (org-gtd-projects-fix-todo-keywords (point-marker))

    ;; Task B should now be NEXT (because A is done)
    (goto-char (point-min))
    (search-forward "Task B")
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))))

;;; Parallel Tasks Tests

(deftest fix-keywords/parallel-tasks-both-next-after-parent-completes ()
  "Marks both parallel tasks NEXT after parent completes."
  (with-temp-buffer
    (org-mode)
    (make-project "Project Test"
                  :id "project-id"
                  :first-tasks '("task-a"))
    (make-task "Task A"
               :id "task-a"
               :level 2
               :status 'done  ;; Task A is done
               :project-ids '("project-id")
               :blocks '("task-b" "task-c"))
    (make-task "Task B"
               :id "task-b"
               :level 2
               :status 'todo
               :project-ids '("project-id")
               :depends-on '("task-a"))
    (make-task "Task C"
               :id "task-c"
               :level 2
               :status 'todo
               :project-ids '("project-id")
               :depends-on '("task-a"))

    (goto-char (point-min))
    (org-gtd-projects-fix-todo-keywords (point-marker))

    ;; Both parallel tasks should be NEXT (both at the frontier)
    (goto-char (point-min))
    (search-forward "Task B")
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))

    ;; Task C should also be NEXT (both are ready)
    (goto-char (point-min))
    (search-forward "Task C")
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))))

;;; Three-Task Chain Tests

(deftest fix-keywords/three-chain-b-next-after-a-completes ()
  "Marks B NEXT after A completes, C stays TODO."
  (with-temp-buffer
    (org-mode)
    (make-project "Project Test"
                  :id "project-id"
                  :first-tasks '("task-a"))
    (make-task "Task A"
               :id "task-a"
               :level 2
               :status 'done  ;; Task A is done
               :project-ids '("project-id")
               :blocks '("task-b"))
    (make-task "Task B"
               :id "task-b"
               :level 2
               :status 'todo
               :project-ids '("project-id")
               :depends-on '("task-a")
               :blocks '("task-c"))
    (make-task "Task C"
               :id "task-c"
               :level 2
               :status 'todo
               :project-ids '("project-id")
               :depends-on '("task-b"))

    (goto-char (point-min))
    (org-gtd-projects-fix-todo-keywords (point-marker))

    ;; Task B should be NEXT
    (goto-char (point-min))
    (search-forward "Task B")
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))

    ;; Task C should still be TODO (B not done yet)
    (goto-char (point-min))
    (search-forward "Task C")
    (assert-equal "TODO" (org-entry-get (point) "TODO"))))

(deftest fix-keywords/three-chain-c-next-after-both-complete ()
  "Marks C NEXT after both A and B complete."
  (with-temp-buffer
    (org-mode)
    (make-project "Project Test"
                  :id "project-id"
                  :first-tasks '("task-a"))
    (make-task "Task A"
               :id "task-a"
               :level 2
               :status 'done  ;; Both tasks done
               :project-ids '("project-id")
               :blocks '("task-b"))
    (make-task "Task B"
               :id "task-b"
               :level 2
               :status 'done
               :project-ids '("project-id")
               :depends-on '("task-a")
               :blocks '("task-c"))
    (make-task "Task C"
               :id "task-c"
               :level 2
               :status 'todo
               :project-ids '("project-id")
               :depends-on '("task-b"))

    (goto-char (point-min))
    (org-gtd-projects-fix-todo-keywords (point-marker))

    ;; Task C should now be NEXT
    (goto-char (point-min))
    (search-forward "Task C")
    (assert-equal "NEXT" (org-entry-get (point) "TODO"))))

;;; WAIT State Handling Tests

(deftest fix-keywords/wait-keeps-dependent-todo ()
  "Keeps dependent task TODO when parent changes from NEXT to WAIT."
  ;; This tests the bug from v3:
  ;; When user changes a NEXT task to WAIT, dependent tasks should NOT
  ;; automatically become NEXT. They should stay TODO because the
  ;; blocking task is not done, just waiting.
  (with-temp-buffer
    (org-mode)
    (make-project "Project Test"
                  :id "project-id"
                  :first-tasks '("task-a"))
    (make-task "Task A"
               :id "task-a"
               :level 2
               :status 'next  ;; Task A starts as NEXT
               :project-ids '("project-id")
               :blocks '("task-b"))
    (make-task "Task B"
               :id "task-b"
               :level 2
               :status 'todo   ;; Task B is TODO (blocked by A)
               :project-ids '("project-id")
               :depends-on '("task-a"))

    ;; User manually changes Task A from NEXT to WAIT
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (org-entry-put (point) "TODO" "WAIT")

    ;; Run fix function (simulates what happens on state change or manual fix)
    (goto-char (point-min))
    (org-gtd-projects-fix-todo-keywords (point-marker))

    ;; Verify: Task A should be WAIT (user's explicit choice)
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (assert-equal "WAIT" (org-entry-get (point) "TODO"))

    ;; Verify: Task B should still be TODO (not NEXT!)
    ;; WAIT is not DONE, so B shouldn't become ready
    (goto-char (point-min))
    (search-forward "Task B")
    (org-back-to-heading t)
    (assert-equal "TODO" (org-entry-get (point) "TODO"))))

(deftest fix-keywords/wait-chain-keeps-descendants-todo ()
  "Keeps child task TODO when parent is WAIT in three-task chain."
  ;; Extended case: A→B→C, when A is WAIT, both B and C should be TODO
  (with-temp-buffer
    (org-mode)
    (make-project "Project Test"
                  :id "project-id"
                  :first-tasks '("task-a"))
    (make-task "Task A"
               :id "task-a"
               :level 2
               :status 'wait   ;; A is WAIT (user set or already waiting)
               :project-ids '("project-id")
               :blocks '("task-b"))
    (make-task "Task B"
               :id "task-b"
               :level 2
               :status 'todo
               :project-ids '("project-id")
               :depends-on '("task-a")
               :blocks '("task-c"))
    (make-task "Task C"
               :id "task-c"
               :level 2
               :status 'todo
               :project-ids '("project-id")
               :depends-on '("task-b"))

    (goto-char (point-min))
    (org-gtd-projects-fix-todo-keywords (point-marker))

    ;; Task A: stays WAIT
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (assert-equal "WAIT" (org-entry-get (point) "TODO"))

    ;; Task B: should be TODO (blocked by WAIT A)
    (goto-char (point-min))
    (search-forward "Task B")
    (org-back-to-heading t)
    (assert-equal "TODO" (org-entry-get (point) "TODO"))

    ;; Task C: should be TODO (blocked by TODO B)
    (goto-char (point-min))
    (search-forward "Task C")
    (org-back-to-heading t)
    (assert-equal "TODO" (org-entry-get (point) "TODO"))))

(provide 'projects-fix-todo-keywords-test)

;;; projects-fix-todo-keywords-test.el ends here
