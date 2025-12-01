;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-graph-ui-test.el --- Unit tests for graph UI split-window layout -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-ui split-window layout.
;;
;; Test Coverage:
;; - org-gtd-graph-ui-setup-windows (split-window layout creation)
;; - org-gtd-graph-ui-get-windows (window retrieval)
;; - org-gtd-graph-ui-cleanup-windows (window cleanup)
;;
;; MIGRATED to test-eunit/unit/graph-ui-test.el:
;; - Cross-project dependency display tests (3 tests)
;;   - org-gtd-graph-ui--format-task-details (shows cross-project blockers)
;;   - org-gtd-graph-ui--format-task-details (separates cross-project blockers)
;;   - org-gtd-graph-ui--format-task-details (separates cross-project dependents)
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-graph-ui)
(require 'org-gtd-graph-mode)
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
      (kill-buffer graph-buffer)))

  (it "creates an atomic window group when parent is not root"
    (let ((graph-buffer (get-buffer-create "*test-graph*"))
          (other-buffer (get-buffer-create "*other*")))
      ;; Create a non-root parent by splitting the frame first
      (switch-to-buffer other-buffer)
      (delete-other-windows)
      (let ((other-window (split-window-below)))
        (select-window other-window)
        (switch-to-buffer graph-buffer)
        (let* ((windows (org-gtd-graph-ui-setup-windows graph-buffer))
               (graph-window (car windows))
               (details-window (cdr windows))
               (parent-window (window-parent graph-window)))
          ;; Parent window should have window-atom parameter set
          ;; since it's not the root window
          (expect parent-window :to-be-truthy)
          (expect (eq parent-window (frame-root-window)) :not :to-be-truthy)
          (expect (window-parameter parent-window 'window-atom) :to-be-truthy)))
      (kill-buffer graph-buffer)
      (kill-buffer other-buffer)))

  (it "quit command closes both graph and details windows"
    (let ((graph-buffer (get-buffer-create "*test-graph*")))
      ;; Display the buffer in a window first
      (switch-to-buffer graph-buffer)
      (delete-other-windows)
      (with-current-buffer graph-buffer
        (setq org-gtd-graph-ui--details-buffer nil))
      (let* ((windows (org-gtd-graph-ui-setup-windows graph-buffer))
             (graph-window (car windows))
             (details-window (cdr windows))
             (details-buffer (with-current-buffer graph-buffer
                              org-gtd-graph-ui--details-buffer)))
        ;; Both windows should exist
        (expect (window-live-p graph-window) :to-be-truthy)
        (expect (window-live-p details-window) :to-be-truthy)
        ;; Call org-gtd-graph-quit from the graph window
        (with-selected-window graph-window
          (with-current-buffer graph-buffer
            (org-gtd-graph-quit)))
        ;; Details window should be closed
        (expect (window-live-p details-window) :not :to-be-truthy)
        ;; Details buffer should be killed
        (expect (buffer-live-p details-buffer) :not :to-be-truthy)))))

;; Cross-project dependency display tests migrated to test-eunit/unit/graph-ui-test.el

(provide 'org-gtd-graph-ui-test)

;;; org-gtd-graph-ui-test.el ends here
