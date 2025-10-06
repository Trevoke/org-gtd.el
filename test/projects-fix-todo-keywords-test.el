;;; projects-fix-todo-keywords-test.el --- Tests for org-gtd-projects-fix-todo-keywords -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-projects-fix-todo-keywords with breadth-first dependency traversal

;;; Code:

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
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
                  (insert "* Project Test\n")
                  (insert ":PROPERTIES:\n")
                  (insert ":ID: project-id\n")
                  (insert ":ORG_GTD: Projects\n")
                  (insert ":ORG_GTD_FIRST_TASKS: task-a\n")
                  (insert ":END:\n")
                  (insert "** TODO Task A\n")
                  (insert ":PROPERTIES:\n")
                  (insert ":ID: task-a\n")
                  (insert ":ORG_GTD: Actions\n")
                  (insert ":ORG_GTD_BLOCKS: task-b\n")
                  (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
                  (insert ":END:\n")
                  (insert "** TODO Task B\n")
                  (insert ":PROPERTIES:\n")
                  (insert ":ID: task-b\n")
                  (insert ":ORG_GTD: Actions\n")
                  (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
                  (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
                  (insert ":END:\n")
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
          (insert "* Project Test\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: project-id\n")
          (insert ":ORG_GTD: Projects\n")
          (insert ":ORG_GTD_FIRST_TASKS: task-a\n")
          (insert ":END:\n")
          (insert "** DONE Task A\n")  ;; Task A is done
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-a\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_BLOCKS: task-b\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")
          (insert "** TODO Task B\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-b\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")

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
          (insert "* Project Test\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: project-id\n")
          (insert ":ORG_GTD: Projects\n")
          (insert ":ORG_GTD_FIRST_TASKS: task-a\n")
          (insert ":END:\n")
          (insert "** DONE Task A\n")  ;; Task A is done
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-a\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_BLOCKS: task-b task-c\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")
          (insert "** TODO Task B\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-b\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")
          (insert "** TODO Task C\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-c\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")

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
          (insert "* Project Test\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: project-id\n")
          (insert ":ORG_GTD: Projects\n")
          (insert ":ORG_GTD_FIRST_TASKS: task-a\n")
          (insert ":END:\n")
          (insert "** DONE Task A\n")  ;; Task A is done
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-a\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_BLOCKS: task-b\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")
          (insert "** TODO Task B\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-b\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
          (insert ":ORG_GTD_BLOCKS: task-c\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")
          (insert "** TODO Task C\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-c\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_DEPENDS_ON: task-b\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")

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
          (insert "* Project Test\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: project-id\n")
          (insert ":ORG_GTD: Projects\n")
          (insert ":ORG_GTD_FIRST_TASKS: task-a\n")
          (insert ":END:\n")
          (insert "** DONE Task A\n")  ;; Both tasks done
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-a\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_BLOCKS: task-b\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")
          (insert "** DONE Task B\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-b\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
          (insert ":ORG_GTD_BLOCKS: task-c\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")
          (insert "** TODO Task C\n")
          (insert ":PROPERTIES:\n")
          (insert ":ID: task-c\n")
          (insert ":ORG_GTD: Actions\n")
          (insert ":ORG_GTD_DEPENDS_ON: task-b\n")
          (insert ":ORG_GTD_PROJECT_IDS: project-id\n")
          (insert ":END:\n")

          (goto-char (point-min))
          (org-gtd-projects-fix-todo-keywords (point-marker))

          ;; Task C should now be NEXT
          (goto-char (point-min))
          (search-forward "Task C")
          (expect (org-entry-get (point) "TODO") :to-equal "NEXT")))))

;;; projects-fix-todo-keywords-test.el ends here
