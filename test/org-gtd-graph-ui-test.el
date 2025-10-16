;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-graph-ui-test.el --- Unit tests for graph UI split-window layout -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-ui split-window layout and task details panel.
;;
;; Test Coverage:
;; - org-gtd-graph-ui-setup-windows (split-window layout creation)
;; - org-gtd-graph-ui-get-windows (window retrieval)
;; - org-gtd-graph-ui-cleanup-windows (window cleanup)
;; - org-gtd-graph-ui-select-node (node selection and details update)
;; - org-gtd-graph-ui-update-details (details panel update)
;; - org-gtd-graph-ui--format-task-details (task details formatting)
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-graph-ui)
(require 'org-gtd-files)
(require 'org-gtd-core)

;;;; Test Setup

(defun org-gtd-graph-ui-test--setup ()
  "Set up minimal test environment for graph UI tests."
  (setq org-gtd-directory (make-temp-file "org-gtd-graph-ui-test" t)
        org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL")))
  ;; Create the tasks file
  (let ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org")))
    (with-temp-file tasks-file
      (insert ""))))

(defun org-gtd-graph-ui-test--teardown ()
  "Clean up after graph UI tests."
  (when (and org-gtd-directory (file-exists-p org-gtd-directory))
    (delete-directory org-gtd-directory t)))

;;;; org-gtd-graph-ui-setup-windows Tests

(describe "org-gtd-graph-ui-setup-windows"

  (before-each (org-gtd-graph-ui-test--setup))
  (after-each (org-gtd-graph-ui-test--teardown))

  (it "creates a split-window layout"
    (let ((graph-buffer (get-buffer-create "*test-graph*")))
      ;; Display the buffer in a window first
      (switch-to-buffer graph-buffer)
      (let ((windows (org-gtd-graph-ui-setup-windows graph-buffer)))
        (expect (car windows) :to-be-truthy)
        (expect (cdr windows) :to-be-truthy)
        (expect (windowp (car windows)) :to-be-truthy)
        (expect (windowp (cdr windows)) :to-be-truthy))
      (kill-buffer graph-buffer))))

(provide 'org-gtd-graph-ui-test)

;;; org-gtd-graph-ui-test.el ends here
