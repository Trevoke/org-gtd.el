;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)


;;; projects-fix-todo-keywords-test.el --- Tests for org-gtd-projects-fix-todo-keywords -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-projects-fix-todo-keywords with breadth-first dependency traversal

;;; Code:

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))
(require 'org-gtd)
(require 'buttercup)

(describe "org-gtd-projects-fix-todo-keywords with dependency-aware traversal"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Simple sequential chain"
    (it "marks first task NEXT when no dependencies satisfied"
        (let ((test-file (make-temp-file "org-gtd-test" nil ".org")))
          (unwind-protect
              (progn
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
                  (expect (org-entry-get (point) "TODO") :to-equal "NEXT")

                  ;; Task B should be TODO
                  (goto-char (point-min))
                  (search-forward "Task B")
                  (expect (org-entry-get (point) "TODO") :to-equal "TODO")))
            (when (file-exists-p test-file)
              (delete-file test-file)))))

    (it "marks next task NEXT after completing first task"
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
          (expect (org-entry-get (point) "TODO") :to-equal "NEXT"))))

  (describe "Parallel tasks (two tasks depend on same parent)"
    (it "marks both parallel tasks NEXT after parent completes"
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
          (expect (org-entry-get (point) "TODO") :to-equal "NEXT")

          ;; Task C should also be NEXT (both are ready)
          (goto-char (point-min))
          (search-forward "Task C")
          (expect (org-entry-get (point) "TODO") :to-equal "NEXT"))))

  (describe "Three-task chain A→B→C"
    (it "marks B NEXT after A completes, C stays TODO"
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
          (expect (org-entry-get (point) "TODO") :to-equal "NEXT")

          ;; Task C should still be TODO (B not done yet)
          (goto-char (point-min))
          (search-forward "Task C")
          (expect (org-entry-get (point) "TODO") :to-equal "TODO")))

    (it "marks C NEXT after both A and B complete"
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
          (expect (org-entry-get (point) "TODO") :to-equal "NEXT")))))

  (describe "WAIT state handling"
    (it "keeps dependent task TODO when parent changes from NEXT to WAIT"
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
          (expect (org-entry-get (point) "TODO") :to-equal "WAIT")

          ;; Verify: Task B should still be TODO (not NEXT!)
          ;; WAIT is not DONE, so B shouldn't become ready
          (goto-char (point-min))
          (search-forward "Task B")
          (org-back-to-heading t)
          (expect (org-entry-get (point) "TODO") :to-equal "TODO")))

    (it "keeps child task TODO when parent is WAIT in three-task chain"
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
          (expect (org-entry-get (point) "TODO") :to-equal "WAIT")

          ;; Task B: should be TODO (blocked by WAIT A)
          (goto-char (point-min))
          (search-forward "Task B")
          (org-back-to-heading t)
          (expect (org-entry-get (point) "TODO") :to-equal "TODO")

          ;; Task C: should be TODO (blocked by TODO B)
          (goto-char (point-min))
          (search-forward "Task C")
          (org-back-to-heading t)
          (expect (org-entry-get (point) "TODO") :to-equal "TODO"))))

;;; projects-fix-todo-keywords-test.el ends here
