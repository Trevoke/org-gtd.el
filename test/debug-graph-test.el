;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "Debug graph traversal"

 (before-each (setq inhibit-message nil)
              (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "debugs what is happening in graph traversal"
     (with-temp-buffer
       (org-mode)
       ;; Create tasks using builders
       (make-task "Task A"
                 :id "task-a-id"
                 :level 1
                 :blocks '("task-b-id"))
       (make-task "Task B"
                 :id "task-b-id"
                 :level 2
                 :depends-on '("task-a-id"))

       ;; Debug: Print buffer contents
       (message "Buffer contents: %s" (buffer-string))

       ;; Force org-mode to re-parse the buffer
       (org-mode-restart)

       ;; Debug: Check if we can find tasks with ORG_GTD=Actions
       (let ((action-tasks 0)
             (all-tasks 0))
         (org-map-entries
          (lambda ()
            (setq all-tasks (1+ all-tasks))
            (let ((org-gtd-prop (org-entry-get (point) "ORG_GTD")))
              (message "Task at point: %s, ORG_GTD: %s" (org-get-heading t t) org-gtd-prop)
              (when (string= org-gtd-prop "Actions")
                (setq action-tasks (1+ action-tasks)))))
          nil
          nil)
         (message "Found %d total tasks, %d with ORG_GTD=Actions" all-tasks action-tasks)
         (expect action-tasks :to-equal 2))

       ;; Debug: Check if we can read properties correctly
       (goto-char (point-min))
       (search-forward "Task A")
       (org-back-to-heading t)
       (let ((id (org-entry-get (point) "ID"))
             (blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
         (expect id :to-equal "task-a-id")
         (expect blocks :to-equal '("task-b-id")))

       ;; Debug: Check Task B
       (goto-char (point-min))
       (search-forward "Task B")
       (org-back-to-heading t)
       (let ((id (org-entry-get (point) "ID"))
             (depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
         (expect id :to-equal "task-b-id")
         (expect depends :to-equal '("task-a-id"))))))
