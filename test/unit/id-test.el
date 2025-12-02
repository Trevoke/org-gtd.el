;;; id-test.el --- Unit tests for ID generation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd ID generation functions.
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from test/id-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; ID Generation

(deftest id-generates-makeshift-for-empty-heading ()
  "Generates a makeshift ID if the heading is empty."
  ;; Pure unit test - no filesystem needed
  (with-temp-buffer
    (org-mode)
    (insert "* \nfoo")
    (goto-char (point-min))
    (org-gtd-id-get-create)

    (assert-match "org-gtd-makeshift-id" (org-entry-get nil "ID"))))

;;; id-test.el ends here
