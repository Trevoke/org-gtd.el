;;; project-cookies-test.el --- Unit tests for project progress cookie functions -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for project progress cookie formatting, insertion, and removal.
;; These tests verify the cookie manipulation functions work correctly.
;;
;; Migrated from test/project-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; org-gtd-project--format-cookies

(deftest cookies/formats-completed-total-percent ()
  "Formats cookies as [completed/total][percent%]."
  (assert-equal "[3/7][42%]" (org-gtd-project--format-cookies 3 7)))

(deftest cookies/handles-zero-total ()
  "Handles zero total gracefully."
  (assert-equal "[0/0][0%]" (org-gtd-project--format-cookies 0 0)))

(deftest cookies/handles-100-percent ()
  "Handles 100% completion."
  (assert-equal "[5/5][100%]" (org-gtd-project--format-cookies 5 5)))

;;; org-gtd-project--set-cookies (position 'end)

(deftest cookies/set-inserts-at-end-of-heading ()
  "Inserts cookies at end of heading with position 'end."
  (let ((org-gtd-project-progress-cookie-position 'end))
    (with-temp-buffer
      (org-mode)
      (insert "* PROJ Buy a house\n")
      (goto-char (point-min))
      (org-gtd-project--set-cookies 3 7)
      (assert-match "Buy a house \\[3/7\\]\\[42%\\]"
                    (buffer-substring-no-properties (point-min) (line-end-position))))))

(deftest cookies/set-inserts-before-tags ()
  "Inserts cookies before tags with position 'end."
  (let ((org-gtd-project-progress-cookie-position 'end))
    (with-temp-buffer
      (org-mode)
      (insert "* PROJ Buy a house  :home:\n")
      (goto-char (point-min))
      (org-gtd-project--set-cookies 3 7)
      (let ((line (buffer-substring-no-properties (point-min) (line-end-position))))
        (assert-match "\\[3/7\\]\\[42%\\].*:home:" line)))))

;;; org-gtd-project--set-cookies (position 'start)

(deftest cookies/set-inserts-at-start-of-heading ()
  "Inserts cookies at start of heading with position 'start."
  (let ((org-gtd-project-progress-cookie-position 'start))
    (with-temp-buffer
      (org-mode)
      (insert "* PROJ Buy a house\n")
      (goto-char (point-min))
      (org-gtd-project--set-cookies 3 7)
      (assert-match "PROJ \\[3/7\\]\\[42%\\] Buy a house"
                    (buffer-substring-no-properties (point-min) (line-end-position))))))

;;; org-gtd-project--set-cookies (position nil)

(deftest cookies/set-does-nothing-when-disabled ()
  "Does nothing when cookie position is nil (disabled)."
  (let ((org-gtd-project-progress-cookie-position nil))
    (with-temp-buffer
      (org-mode)
      (insert "* PROJ Buy a house\n")
      (goto-char (point-min))
      (org-gtd-project--set-cookies 3 7)
      (assert-equal "* PROJ Buy a house"
                    (buffer-substring-no-properties (point-min) (line-end-position))))))

;;; org-gtd-project--remove-cookies

(deftest cookies/remove-from-end-of-heading ()
  "Removes cookies from end of heading."
  (with-temp-buffer
    (org-mode)
    (insert "* PROJ Buy a house [3/7][42%]\n")
    (goto-char (point-min))
    (org-gtd-project--remove-cookies)
    (assert-equal "PROJ Buy a house" (org-get-heading t t t t))))

(deftest cookies/remove-from-start-of-heading ()
  "Removes cookies from start of heading."
  (with-temp-buffer
    (org-mode)
    (insert "* PROJ [3/7][42%] Buy a house\n")
    (goto-char (point-min))
    (org-gtd-project--remove-cookies)
    (assert-equal "PROJ Buy a house" (org-get-heading t t t t))))

(deftest cookies/remove-handles-no-cookies ()
  "Handles heading with no cookies."
  (with-temp-buffer
    (org-mode)
    (insert "* PROJ Buy a house\n")
    (goto-char (point-min))
    (org-gtd-project--remove-cookies)
    (assert-equal "PROJ Buy a house" (org-get-heading t t t t))))

(deftest cookies/remove-preserves-tags ()
  "Preserves tags when removing cookies."
  (with-temp-buffer
    (org-mode)
    (insert "* PROJ Buy a house [3/7][42%]  :home:finance:\n")
    (goto-char (point-min))
    (org-gtd-project--remove-cookies)
    (assert-match ":home:finance:"
                  (buffer-substring-no-properties (point-min) (point-max)))))

(provide 'project-cookies-test)

;;; project-cookies-test.el ends here
