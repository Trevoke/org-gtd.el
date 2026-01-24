;;; engage-prefix-test.el --- Unit tests for engage agenda prefix formatting -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for engage agenda prefix width and link prettification.
;;
;; Test Coverage:
;; - Link prettification in prefix (1 test)
;; - Prefix width formatting (5 tests)
;;
;; Migrated from test/engage-view-prefix-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Link Prettification Tests

(deftest engage-prefix/replaces-link-with-description ()
  "Link in project name is prettified to show description with ellipsis."
  (create-project "[[https://workera.ai][Classify Workera]] exam problems")
  (org-gtd-engage)
  (assert-match "Classify Wo…" (ogt--buffer-string org-agenda-buffer)))

;;; Prefix Width Tests

(deftest engage-prefix/adds-ellipses-if-name-too-long ()
  "Adds ellipses when project name exceeds prefix width."
  (create-project "My long project name which needs shortening")
  (let ((org-gtd-prefix-width 17))
    (org-gtd-engage))
  (assert-match "My long project …" (ogt--buffer-string org-agenda-buffer)))

(deftest engage-prefix/shortens-prefix-words-if-necessary ()
  "Shortens prefix to fit within specified width."
  (create-project "P234567890")
  (let ((org-gtd-prefix-width 5))
    (org-gtd-engage))
  (assert-match "P234…" (ogt--buffer-string org-agenda-buffer)))

(deftest engage-prefix/only-shows-ellipses-if-width-is-1 ()
  "Shows only ellipsis when width is 1."
  (create-project "P234567890")
  (let ((org-gtd-prefix-width 1))
    (org-gtd-engage))
  (assert-match "^  …" (ogt--buffer-string org-agenda-buffer)))

(deftest engage-prefix/shows-full-name-without-ellipses-when-fits ()
  "Shows full project name without ellipses when it fits."
  (create-project "P234567890")
  (let ((org-gtd-prefix-width 10))
    (org-gtd-engage))
  (assert-match "P234567890" (ogt--buffer-string org-agenda-buffer)))

(deftest engage-prefix/adds-spaces-for-alignment ()
  "Adds padding spaces for alignment when name is shorter than width."
  (create-project "P234567890")
  (let ((org-gtd-prefix-width 50))
    (org-gtd-engage))
  (assert-match "P234567890                                        " (ogt--buffer-string org-agenda-buffer)))

(provide 'engage-prefix-test)

;;; engage-prefix-test.el ends here
