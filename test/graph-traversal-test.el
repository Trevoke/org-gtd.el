;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "Graph traversal for project tasks"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "org-gtd-projects--collect-tasks-by-graph function"

  (it "exists and can be called"
      ;; Test that the function exists
      (expect (fboundp 'org-gtd-projects--collect-tasks-by-graph) :to-be-truthy))

  (it "collects tasks by following ID dependency chains"
      ;; Create a buffer with tasks connected via ID properties
      (with-temp-buffer
        (org-mode)
        ;; Task A - no dependencies
        (insert "* Task A\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task-a-id\n")
        (insert ":BLOCKS: task-b-id\n")
        (insert ":END:\n")

        ;; Task B - depends on A
        (insert "** Task B\n")  ;; Different level
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task-b-id\n")
        (insert ":DEPENDS_ON: task-a-id\n")
        (insert ":END:\n")

        ;; Task C - not connected to graph
        (insert "* Task C\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task-c-id\n")
        (insert ":END:\n")

        ;; Test the graph traversal starting from task A
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph)))
          ;; Should find Task A and Task B, but not Task C (not connected)
          (expect (length connected-tasks) :to-equal 2))))

  (it "handles circular dependencies gracefully"
      ;; Test that circular dependencies don't cause infinite loops
      (with-temp-buffer
        (org-mode)
        ;; Task A blocks B
        (insert "* Task A\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task-a-id\n")
        (insert ":BLOCKS: task-b-id\n")
        (insert ":DEPENDS_ON: task-c-id\n")  ;; Circular: A depends on C
        (insert ":END:\n")

        ;; Task B blocks C
        (insert "* Task B\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task-b-id\n")
        (insert ":DEPENDS_ON: task-a-id\n")
        (insert ":BLOCKS: task-c-id\n")
        (insert ":END:\n")

        ;; Task C completes the cycle
        (insert "* Task C\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD: Actions\n")
        (insert ":ID: task-c-id\n")
        (insert ":DEPENDS_ON: task-b-id\n")
        (insert ":BLOCKS: task-a-id\n")  ;; Circular: C blocks A
        (insert ":END:\n")

        ;; Should handle circular dependencies without infinite loop
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph)))
          ;; Should find all 3 tasks in the circular graph
          (expect (length connected-tasks) :to-equal 3))))

  (it "returns empty list when no tasks have ORG_GTD=Actions"
      ;; Test behavior when buffer has no Action tasks
      (with-temp-buffer
        (org-mode)
        (insert "* Regular Heading\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: regular-id\n")
        (insert ":END:\n")

        (goto-char (point-min))
        (let ((connected-tasks (org-gtd-projects--collect-tasks-by-graph)))
          (expect connected-tasks :to-equal nil))))))