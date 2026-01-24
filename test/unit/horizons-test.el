;;; horizons-test.el --- Tests for org-gtd higher horizons view -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd higher horizons view functionality.
;;
;; Test Coverage:
;; - Horizons file creation when missing (1 test)
;; - Shows existing horizons file (1 test)
;; - Horizons shown when returning to WIP buffer (1 test)
;; - Horizons hidden when disabled (1 test)
;; - Read-only indirect buffer for horizons view (6 tests)
;; - Stop clarifying cleanup (2 tests)
;;
;; Migrated from test/horizons-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Horizons View When Enabled

(deftest horizons/creates-templated-file-when-missing ()
  "Creates a templated file when there isn't one."
  (let ((org-gtd-clarify-show-horizons 'right))
    (capture-inbox-item "Add a configuration option")
    (org-gtd-process-inbox)
    (assert-true (get-buffer-window "*Org GTD Horizons View*"))
    (assert-true (get-buffer-window (car (org-gtd-wip--get-buffers))))))

(deftest horizons/shows-existing-file ()
  "Shows the existing file if there is one."
  (let ((org-gtd-clarify-show-horizons 'right))
    (ogt--create-org-file-in-org-gtd-dir
     "horizons"
     "We are the champions")
    (capture-inbox-item "Add a configuration option")
    (org-gtd-process-inbox)
    (assert-true (get-buffer-window "*Org GTD Horizons View*"))
    (assert-true (get-buffer-window (car (org-gtd-wip--get-buffers))))
    (assert-match "We are the champions"
                  (file-raw-text (expand-file-name "horizons.org" org-gtd-directory)))))

(deftest horizons/shown-when-returning-to-wip-buffer ()
  "Horizons shown when we return to a WIP buffer."
  (let ((org-gtd-clarify-show-horizons 'right))
    (capture-inbox-item "Add a configuration option")
    (org-gtd-process-inbox)
    (set-buffer "*scratch*")
    (delete-other-windows)
    (with-simulated-input
        "TAB RET"
      (org-gtd-clarify-switch-to-buffer))
    (assert-true (get-buffer-window "*Org GTD Horizons View*"))))

;;; Horizons View When Disabled

(deftest horizons/not-shown-when-disabled ()
  "Does not show the window when horizons is disabled."
  (let ((org-gtd-clarify-show-horizons nil))
    (capture-inbox-item "Add a configuration option")
    (org-gtd-process-inbox)
    (assert-nil (get-buffer-window "horizons.org"))))

;;; Read-only Horizons View Tests

(deftest horizons/creates-read-only-indirect-buffer ()
  "Creates a read-only indirect buffer for horizons view."
  (let ((org-gtd-clarify-show-horizons 'right))
    (ogt--create-org-file-in-org-gtd-dir
     "horizons"
     "* Purpose and principles")
    (capture-inbox-item "Test item")
    (org-gtd-process-inbox)
    (let ((view-buffer (get-buffer "*Org GTD Horizons View*")))
      (assert-true view-buffer)
      (with-current-buffer view-buffer
        (assert-true buffer-read-only)
        (assert-true (buffer-base-buffer))))))

(deftest horizons/view-reflects-file-changes ()
  "View buffer reflects changes to horizons file."
  (let ((org-gtd-clarify-show-horizons 'right))
    (ogt--create-org-file-in-org-gtd-dir
     "horizons"
     "* Purpose")
    (capture-inbox-item "Test item")
    (org-gtd-process-inbox)
    (let ((view-buffer (get-buffer "*Org GTD Horizons View*"))
          (horizons-buffer (get-buffer "horizons.org")))
      ;; Modify the horizons file buffer
      (with-current-buffer horizons-buffer
        (goto-char (point-max))
        (insert "\n* Vision"))
      ;; Check that view buffer sees the change
      (with-current-buffer view-buffer
        (assert-match "Vision" (buffer-string))))))

(deftest horizons/cleanup-kills-view-buffer ()
  "Cleanup function kills the view buffer."
  (let ((org-gtd-clarify-show-horizons 'right))
    (ogt--create-org-file-in-org-gtd-dir
     "horizons"
     "* Purpose")
    ;; Create the view buffer
    (org-gtd-clarify--get-or-create-horizons-view)
    (assert-true (get-buffer "*Org GTD Horizons View*"))
    ;; Clean it up
    (org-gtd-clarify--cleanup-horizons-view)
    (assert-nil (get-buffer "*Org GTD Horizons View*"))))

(deftest horizons/cleaned-up-after-one-off-clarify ()
  "Cleans up view buffer after one-off clarification completes."
  (let ((org-gtd-clarify-show-horizons 'right))
    (ogt--create-org-file-in-org-gtd-dir
     "horizons"
     "* Purpose")
    (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                         "tasks"
                         "* TODO Test task")))
      (with-current-buffer task-buffer
        (goto-char (point-min))
        (org-next-visible-heading 1)
        ;; Start one-off clarification
        (org-gtd-clarify-item))
      ;; View buffer should exist during clarification
      (assert-true (get-buffer "*Org GTD Horizons View*"))
      ;; Switch to WIP buffer and organize as trash
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        (org-gtd-trash))
      ;; View buffer should be cleaned up after organize
      (assert-nil (get-buffer "*Org GTD Horizons View*")))))

(deftest horizons/cleaned-up-after-inbox-processing ()
  "Cleans up view buffer after inbox processing completes."
  (let ((org-gtd-clarify-show-horizons 'right))
    (ogt--create-org-file-in-org-gtd-dir
     "horizons"
     "* Purpose")
    (capture-inbox-item "Test inbox item")
    (org-gtd-process-inbox)
    ;; View buffer should exist during inbox processing
    (assert-true (get-buffer "*Org GTD Horizons View*"))
    ;; Organize as trash
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-trash))
    ;; View buffer should be cleaned up after inbox completes
    (assert-nil (get-buffer "*Org GTD Horizons View*"))))

;;; Stop Clarifying Cleanup

(deftest horizons/stop-clarifying-cleans-up ()
  "Stop clarifying command cleans up and restores state."
  (let ((org-gtd-clarify-show-horizons 'right)
        wip-buffer)
    (ogt--create-org-file-in-org-gtd-dir
     "horizons"
     "* Purpose")
    (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                         "tasks"
                         "* TODO Test task")))
      (with-current-buffer task-buffer
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd-clarify-item))
      (setq wip-buffer (car (org-gtd-wip--get-buffers)))
      ;; View buffer and WIP buffer should exist
      (assert-true (get-buffer "*Org GTD Horizons View*"))
      (assert-true wip-buffer)
      ;; Stop clarifying
      (with-current-buffer wip-buffer
        (org-gtd-clarify-stop))
      ;; WIP buffer should be killed
      (assert-nil (buffer-live-p wip-buffer))
      ;; View buffer should be cleaned up
      (assert-nil (get-buffer "*Org GTD Horizons View*")))))

(deftest horizons/keybinding-bound-to-stop ()
  "C-c C-k keybinding is bound to stop clarifying."
  (assert-equal #'org-gtd-clarify-stop
                (lookup-key org-gtd-clarify-mode-map (kbd "C-c C-k"))))

(deftest horizons/header-shows-keybindings ()
  "Header line shows both organize and stop keybindings."
  (let ((org-gtd-clarify-show-horizons 'right))
    (ogt--create-org-file-in-org-gtd-dir
     "horizons"
     "* Purpose")
    (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                         "tasks"
                         "* TODO Test task")))
      (with-current-buffer task-buffer
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd-clarify-item))
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        (assert-match "C-c c" header-line-format)
        (assert-match "C-c C-k" header-line-format)
        (assert-match "cancel" header-line-format)))))

(provide 'horizons-test)

;;; horizons-test.el ends here
