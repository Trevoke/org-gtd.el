;;; id-overlay-test.el --- Unit tests for org-gtd-id-overlay text functions -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-id-overlay text extraction and truncation functions.
;; These are pure unit tests that don't require org-mode setup.
;;
;; Migrated from test/id-overlay-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-id-overlay)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; org-gtd-id-overlay--extract-heading-text Tests

(deftest id-overlay/extract-plain-heading ()
  "Extracts plain heading text."
  (assert-equal "Meeting with client"
                (org-gtd-id-overlay--extract-heading-text "Meeting with client")))

(deftest id-overlay/extract-removes-todo-keywords ()
  "Removes TODO keywords."
  (assert-equal "Review quarterly budget"
                (org-gtd-id-overlay--extract-heading-text "TODO Review quarterly budget"))
  (assert-equal "Complete project"
                (org-gtd-id-overlay--extract-heading-text "DONE Complete project"))
  (assert-equal "Call customer"
                (org-gtd-id-overlay--extract-heading-text "NEXT Call customer")))

(deftest id-overlay/extract-removes-statistics-cookies ()
  "Removes statistics cookies."
  (assert-equal "Project tasks"
                (org-gtd-id-overlay--extract-heading-text "Project tasks [1/3]"))
  (assert-equal "Progress"
                (org-gtd-id-overlay--extract-heading-text "Progress [33%]"))
  (assert-equal "Multi"
                (org-gtd-id-overlay--extract-heading-text "Multi [2/5] [40%]")))

(deftest id-overlay/extract-handles-complex-combinations ()
  "Handles complex combinations."
  (assert-equal "Complete project"
                (org-gtd-id-overlay--extract-heading-text "TODO Complete project [2/4] [50%]")))

(deftest id-overlay/extract-handles-empty-gracefully ()
  "Handles empty results gracefully."
  (assert-equal ""
                (org-gtd-id-overlay--extract-heading-text "TODO"))
  (assert-equal ""
                (org-gtd-id-overlay--extract-heading-text "[1/3]")))

(deftest id-overlay/extract-handles-whitespace ()
  "Handles whitespace properly."
  (assert-equal "Multiple spaces"
                (org-gtd-id-overlay--extract-heading-text "TODO   Multiple    spaces   ")))

;;; org-gtd-id-overlay--truncate-text Tests

(deftest id-overlay/truncate-long-text-with-ellipsis ()
  "Truncates text longer than limit with ellipsis."
  (assert-equal "This is a very long ..."
                (org-gtd-id-overlay--truncate-text
                 "This is a very long heading that needs truncation" 20)))

(deftest id-overlay/truncate-short-text-unchanged ()
  "Returns short text unchanged."
  (assert-equal "Short text"
                (org-gtd-id-overlay--truncate-text "Short text" 20)))

(deftest id-overlay/truncate-exact-limit ()
  "Handles text exactly at limit."
  (assert-equal "Exactly twenty chars"
                (org-gtd-id-overlay--truncate-text "Exactly twenty chars" 20)))

(deftest id-overlay/truncate-empty-text ()
  "Handles empty text."
  (assert-equal ""
                (org-gtd-id-overlay--truncate-text "" 20)))

(deftest id-overlay/truncate-custom-length ()
  "Handles custom truncation length."
  (assert-equal "Long tex..."
                (org-gtd-id-overlay--truncate-text "Long text here" 8)))

(provide 'id-overlay-test)

;;; id-overlay-test.el ends here
