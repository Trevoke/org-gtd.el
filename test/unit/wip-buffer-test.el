;;; wip-buffer-test.el --- Tests for org-gtd WIP buffer state -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd WIP (Work In Progress) buffer functionality.
;;
;; Test Coverage:
;; - WIP buffer holds the subtree for clarification (1 test)
;; - WIP buffer has org-gtd-clarify-mode enabled (1 test)
;;
;; Migrated from test/wip-buffer-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Helper Functions

(defun wip-buffer-test--active-minor-modes ()
  "Get a list of which minor modes are enabled in the current buffer.
Taken from https://emacs.stackexchange.com/a/62414/61"
  (let ($list)
    (mapc (lambda ($mode)
            (condition-case nil
                (if (and (symbolp $mode) (symbol-value $mode))
                    (setq $list (cons $mode $list)))
              (error nil)))
          minor-mode-list)
    (sort $list 'string<)))

;;; WIP Buffer State Tests

(deftest wip/holds-subtree-for-task-to-clarify ()
  "WIP buffer holds the subtree for the task we want to clarify."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))
    (assert-match "This is the heading to clarify"
                  (ogt--buffer-string (car (org-gtd-wip--get-buffers))))))

(deftest wip/has-org-gtd-clarify-mode ()
  "WIP buffer has the org-gtd-clarify-mode as major mode."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))
    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        (assert-equal 'org-gtd-clarify-mode major-mode)))))

(provide 'wip-buffer-test)

;;; wip-buffer-test.el ends here
