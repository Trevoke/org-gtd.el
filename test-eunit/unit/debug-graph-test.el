;;; debug-graph-test.el --- Unit tests for graph traversal debugging -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for debugging graph traversal operations.
;; Verifies task property reading and ORG_GTD property handling.
;;
;; Migrated from test/debug-graph-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Graph Traversal Debug Tests

(deftest debug-graph/verifies-task-properties-and-org-gtd-handling ()
  "Debugs what is happening in graph traversal."
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

    ;; Force org-mode to re-parse the buffer
    (org-mode-restart)

    ;; Check if we can find tasks with ORG_GTD=Actions
    (let ((action-tasks 0)
          (all-tasks 0))
      (org-map-entries
       (lambda ()
         (setq all-tasks (1+ all-tasks))
         (let ((org-gtd-prop (org-entry-get (point) "ORG_GTD")))
           (when (string= org-gtd-prop "Actions")
             (setq action-tasks (1+ action-tasks)))))
       nil
       nil)
      (assert-equal 2 action-tasks))

    ;; Check if we can read properties correctly
    (goto-char (point-min))
    (search-forward "Task A")
    (org-back-to-heading t)
    (let ((id (org-entry-get (point) "ID"))
          (blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (assert-equal "task-a-id" id)
      (assert-equal '("task-b-id") blocks))

    ;; Check Task B
    (goto-char (point-min))
    (search-forward "Task B")
    (org-back-to-heading t)
    (let ((id (org-entry-get (point) "ID"))
          (depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
      (assert-equal "task-b-id" id)
      (assert-equal '("task-a-id") depends))))

(provide 'debug-graph-test)

;;; debug-graph-test.el ends here
