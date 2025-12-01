;;; habits-test.el --- Tests for org-gtd habit items -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd habit item functionality.
;;
;; Test Coverage:
;; - Habit is formatted correctly for org-mode (1 test)
;;
;; Note: "appears in daily agenda after creation with recurring schedule"
;; migrated to test-eunit/acceptance/basic-workflows-test.el
;;
;; Migrated from test/habits-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Habit Formatting Tests

(deftest habits/formatted-like-org-mode-wants ()
  "Habit is formatted like org-mode wants."
  (let* ((repeater "++1m"))
    (create-habit "Yowza" repeater)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Yowza")
      (assert-equal "habit" (task-property (current-task) "STYLE"))
      (assert-match (format "%s" repeater) (task-property (current-task) "SCHEDULED")))))

(provide 'habits-test)

;;; habits-test.el ends here
