;;; review-menu-counts-test.el --- Review menu labels -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Exercise actual transient setup, rendering, keyboard dispatch and reopening.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-command-center)
(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (let ((org-gtd-review-show-counts t)
          (org-id-extra-files nil)
          (org-id-search-archives nil)
          (transient-show-popup t)
          (org-gtd-command-center--counts nil))
      (unwind-protect
          (funcall proceed context)
        (org-gtd-review-counts--cancel org-gtd-command-center--counts)
        (remove-hook 'transient-exit-hook #'org-gtd-command-center--stop-counts)))))

(defun ogt-menu-count-test--text ()
  "Return the rendered transient text."
  (with-current-buffer transient--buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun ogt-menu-count-test--finish ()
  "Run the timer steps of the open menu's count job until it is done."
  (let ((job org-gtd-command-center--counts))
    (cl-letf (((symbol-function 'input-pending-p) (lambda () nil)))
      (while (not (org-gtd-review-counts--done-p job))
        (when-let* ((timer (org-gtd-review-counts--job-timer job)))
          (cancel-timer timer))
        (org-gtd-review-counts--tick job)))))

(deftest review-menu-counts/zero-is-hidden-and-selectable ()
  "An empty review keeps its shortcut and opens an empty agenda."
  (org-gtd-command-center--stuck)
  (ogt-menu-count-test--finish)
  (assert-match "Single actions" (ogt-menu-count-test--text))
  (assert-nil (string-match-p "Single actions (" (ogt-menu-count-test--text)))
  (assert-true (eq (key-binding (kbd "s")) 'org-gtd-reflect-stuck-next-action-items))
  (execute-kbd-macro (kbd "s"))
  (assert-true (derived-mode-p 'org-agenda-mode))
  (assert-match "Stuck Single Actions" (buffer-string))
  (assert-nil org-gtd-command-center--counts))

(deftest review-menu-counts/pending-then-refreshed-in-place ()
  "The menu shows pending counts first, then updates without reopening."
  (with-current-buffer (find-file-noselect "/mock:/gtd/org-gtd-tasks.org")
    (insert "* TODO Candidate\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n"))
  (let ((transient-enable-menu-navigation t))
    (org-gtd-command-center--stuck)
    (let ((prefix transient--prefix))
      (assert-match "Single actions (…)" (ogt-menu-count-test--text))
      (assert-match "Projects (…)" (ogt-menu-count-test--text))
      (with-selected-window transient--window
        (transient--goto-button
         (cl-find 'org-gtd-reflect-stuck-habit-items transient--suffixes
                  :key (lambda (suffix) (oref suffix command))))
        (assert-true (get-text-property (point) 'button-data)))
      (ogt-menu-count-test--finish)
      (assert-true (eq prefix transient--prefix))
      (assert-match "Single actions (1)" (ogt-menu-count-test--text))
      (assert-nil (string-match-p "(…)" (ogt-menu-count-test--text)))
      (with-current-buffer transient--buffer
        (assert-match "Habits"
                      (buffer-substring (line-beginning-position)
                                        (line-end-position)))))))

(deftest review-menu-counts/redisplay-and-reopen ()
  "Redisplay uses the job's counts, while actual reopening sees unsaved changes."
  (let ((source (find-file-noselect "/mock:/gtd/org-gtd-tasks.org")))
    (with-current-buffer source
      (insert "* TODO Candidate\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n"))
    (org-gtd-command-center--stuck)
    (ogt-menu-count-test--finish)
    (assert-match "Single actions (1)" (ogt-menu-count-test--text))
    (with-current-buffer source
      (goto-char (point-min))
      (org-entry-put nil "ORG_GTD" "Someday"))
    (cl-letf (((symbol-function 'org-gtd-review-counts--step-1)
               (lambda (&rest _) (error "Rescanned on redisplay"))))
      (transient--redisplay)
      (assert-match "Single actions (1)" (ogt-menu-count-test--text)))
    (transient--emergency-exit)
    (assert-nil org-gtd-command-center--counts)
    (org-gtd-command-center--stuck)
    (ogt-menu-count-test--finish)
    (assert-match "Single actions" (ogt-menu-count-test--text))
    (assert-nil (string-match-p "Single actions (" (ogt-menu-count-test--text)))))

(deftest review-menu-counts/all-empty-menus-omit-counts ()
  "Every empty review label omits its count, including the main S/M totals."
  (dolist (prefix '(org-gtd-command-center
                    org-gtd-command-center--stuck
                    org-gtd-command-center--missed))
    (funcall prefix)
    (ogt-menu-count-test--finish)
    (let ((text (ogt-menu-count-test--text)))
      (assert-nil (string-match-p "(0)" text))
      (assert-nil (string-match-p "(?)" text))
      (assert-nil (string-match-p "(…)" text))
      (when (eq prefix 'org-gtd-command-center)
        (assert-match "Stuck items\\.\\.\\." text)
        (assert-match "Missed items\\.\\.\\." text)))
    (transient--emergency-exit)))

(deftest review-menu-counts/aggregate-counts-are-view-rows ()
  "Main menu totals add constituent rows, including repeated project headings."
  (with-current-buffer (find-file-noselect "/mock:/gtd/org-gtd-tasks.org")
    (insert "* TODO Missing calendar date\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n* TODO Late project\nDEADLINE: <2000-01-01 Sat> SCHEDULED: <2000-01-01 Sat>\n:PROPERTIES:\n:ORG_GTD: Projects\n:END:\n"))
  (org-gtd-command-center)
  (ogt-menu-count-test--finish)
  (let ((text (ogt-menu-count-test--text))
        (job org-gtd-command-center--counts))
    (assert-match "Stuck items\\.\\.\\. (1)" text)
    (assert-match "Missed items\\.\\.\\. (2)" text)
    (assert-match "Completed projects (1)" text)
    ;; The sub-menu reuses the parent's counts, so it needs no scan.
    (execute-kbd-macro (kbd "M"))
    (assert-true (eq job org-gtd-command-center--counts))
    (assert-nil (org-gtd-review-counts--job-timer job)))
  (assert-match "Projects only (2)" (ogt-menu-count-test--text))
  (assert-match "All missed (2)" (ogt-menu-count-test--text)))

;;; review-menu-counts-test.el ends here
