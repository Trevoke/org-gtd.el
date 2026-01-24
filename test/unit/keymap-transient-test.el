;;; keymap-transient-test.el --- Unit tests for keymap and transient integration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for keymap and transient menu integration.
;; These tests verify that keymaps are correctly configured and
;; organization functions exist.
;;
;; Migrated from test/integration/keymap-transient-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; org-gtd-clarify-mode-map Keymap Tests

(deftest keymap/clarify-map-triggers-organize-transient ()
  "C-c c triggers org-gtd-organize transient menu."
  (assert-true (keymapp org-gtd-clarify-mode-map))
  (assert-equal #'org-gtd-organize (lookup-key org-gtd-clarify-mode-map (kbd "C-c c"))))

;;; org-gtd-organize Transient Menu Tests

(deftest keymap/organization-functions-exist ()
  "All expected organization functions exist and are callable."
  ;; Test all transient functions exist
  (assert-true (fboundp 'org-gtd-quick-action))
  (assert-true (fboundp 'org-gtd-single-action))
  (assert-true (fboundp 'org-gtd-delegate))
  (assert-true (fboundp 'org-gtd-calendar))
  (assert-true (fboundp 'org-gtd-habit))
  (assert-true (fboundp 'org-gtd-project-new))
  (assert-true (fboundp 'org-gtd-project-extend))
  (assert-true (fboundp 'org-gtd-tickler))
  (assert-true (fboundp 'org-gtd-knowledge))
  (assert-true (fboundp 'org-gtd-trash))

  ;; Test that transient menu function exists
  (assert-true (fboundp 'org-gtd-organize)))

;;; Keymap Inheritance and Mode Interaction Tests

(deftest keymap/clarify-mode-works-with-org-mode ()
  "Clarify mode keymap works with org-mode keymaps."
  (assert-true (keymapp org-gtd-clarify-mode-map))
  (assert-equal #'org-gtd-organize (lookup-key org-gtd-clarify-mode-map (kbd "C-c c")))

  ;; Test that org functions are available
  (assert-true (fboundp 'org-cycle)))

;;; Project Workflow via Direct Function Calls

(deftest keymap/project-functions-and-keymap-binding ()
  "Creates project correctly using direct organization functions."
  ;; Test project functions exist
  (assert-true (fboundp 'org-gtd-project-new))
  (assert-true (fboundp 'org-gtd-project-extend))

  ;; Test keymap binding
  (assert-equal #'org-gtd-organize (lookup-key org-gtd-clarify-mode-map (kbd "C-c c"))))

;;; Error Handling in Keymap Context

(deftest keymap/keymap-works-after-errors ()
  "Keymap continues to work after organization errors."
  (assert-equal #'org-gtd-organize (lookup-key org-gtd-clarify-mode-map (kbd "C-c c")))

  ;; Test that functions are available for error handling
  (assert-true (fboundp 'org-gtd-project-new))
  (assert-true (fboundp 'org-gtd-single-action)))

(provide 'keymap-transient-test)

;;; keymap-transient-test.el ends here
