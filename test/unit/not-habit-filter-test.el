;;; not-habit-filter-test.el --- Tests for the not-habit filter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the `not-habit' filter in the View DSL.  A spec with
;; `(not-habit . t)' excludes items whose ORG_GTD marks them as habits
;; while keeping everything else.
;;

;;; Code:

(require 'e-unit)
(require 'org)
(require 'org-gtd-view-language)

;; Initialize e-unit short syntax
(e-unit-initialize)

(deftest not-habit-filter/excludes-habit-item ()
  "A not-habit filter skips an item marked as a habit."
  (with-temp-buffer
    (org-mode)
    (insert (format "* TODO Water plants\n:PROPERTIES:\n:ORG_GTD: %s\n:END:\n"
                    (org-gtd-type-org-gtd-value 'habit)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((skip-fn (org-gtd-view-lang--build-skip-function '((not-habit . t))))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))  ; number = skip

(deftest not-habit-filter/includes-non-habit-item ()
  "A not-habit filter keeps an item that is not a habit."
  (with-temp-buffer
    (org-mode)
    (insert (format "* TODO Buy milk\n:PROPERTIES:\n:ORG_GTD: %s\n:END:\n"
                    (org-gtd-type-org-gtd-value 'next-action)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((skip-fn (org-gtd-view-lang--build-skip-function '((not-habit . t))))
           (result (funcall skip-fn)))
      (assert-nil result))))  ; nil = include

(provide 'not-habit-filter-test)
;;; not-habit-filter-test.el ends here
