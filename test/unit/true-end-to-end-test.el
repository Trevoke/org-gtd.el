;;; true-end-to-end-test.el --- Unit tests for keyboard integration architecture -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests verifying the keyboard-driven GTD workflow architecture.
;; These tests ensure that all organization functions exist and are properly
;; bound for keyboard-driven usage.
;;
;; Migrated from test/integration/true-end-to-end-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Complete capture-to-organize workflow

(deftest e2e/keyboard-binding-availability-and-basic-workflow ()
  "Demonstrates keyboard binding availability and basic workflow."
  ;; Test that the capture function exists and is bound
  (assert-true (fboundp 'org-gtd-capture))
  (assert-true (fboundp 'org-gtd-organize))

  ;; Test keymap binding
  (assert-equal #'org-gtd-organize (lookup-key org-gtd-clarify-mode-map (kbd "C-c c")))

  ;; Test that organization functions exist
  (assert-true (fboundp 'org-gtd-single-action))
  (assert-true (fboundp 'org-gtd-project-new)))

;;; Keyboard-driven organization functions

(deftest e2e/all-organization-options-available-via-keyboard ()
  "Verifies all organization options are available via keyboard."
  ;; Test single action workflow availability
  (assert-true (fboundp 'org-gtd-single-action))

  ;; Test project workflow availability
  (assert-true (fboundp 'org-gtd-project-new))
  (assert-true (fboundp 'org-gtd-project-extend))

  ;; Test other organization functions
  (assert-true (fboundp 'org-gtd-delegate))
  (assert-true (fboundp 'org-gtd-calendar))
  (assert-true (fboundp 'org-gtd-tickler))
  (assert-true (fboundp 'org-gtd-knowledge))
  (assert-true (fboundp 'org-gtd-habit))
  (assert-true (fboundp 'org-gtd-quick-action))
  (assert-true (fboundp 'org-gtd-trash)))

;;; End-to-end keymap integration

(deftest e2e/clarify-map-properly-configured ()
  "Ensures org-gtd-clarify-mode-map is properly configured."
  ;; Test keymap exists
  (assert-true (keymapp org-gtd-clarify-mode-map))

  ;; Test key binding
  (assert-equal #'org-gtd-organize (lookup-key org-gtd-clarify-mode-map (kbd "C-c c")))

  ;; Test that transient menu function is callable
  (assert-true (fboundp 'org-gtd-organize))
  (assert-true (commandp 'org-gtd-organize)))

;;; Full workflow function availability

(deftest e2e/all-workflow-functions-exist ()
  "Confirms all workflow functions exist for keyboard-driven usage."
  ;; Core workflow functions
  (assert-true (fboundp 'org-gtd-capture))
  (assert-true (fboundp 'org-gtd-engage))
  (assert-true (fboundp 'org-gtd-reflect-stuck-projects))

  ;; Archive and cleanup functions
  (assert-true (fboundp 'org-gtd-archive-completed-items))

  ;; Mode functions
  (assert-true (fboundp 'org-gtd-mode))
  (assert-true (fboundp 'org-gtd-clarify-mode)))

;;; Integration with org-mode keyboard functionality

(deftest e2e/org-mode-and-org-gtd-keyboard-coexist ()
  "Ensures org-mode and org-gtd keyboard functions coexist."
  ;; Test org-mode functions are available
  (assert-true (fboundp 'org-cycle))
  (assert-true (fboundp 'org-next-visible-heading))
  (assert-true (fboundp 'org-capture-finalize))

  ;; Test org-gtd functions don't conflict
  (assert-true (fboundp 'org-gtd-organize))
  (assert-equal #'org-gtd-organize (lookup-key org-gtd-clarify-mode-map (kbd "C-c c")))

  ;; Test that TAB key still works in org-mode context
  (with-temp-buffer
    (org-mode)
    (assert-equal #'org-cycle (key-binding (kbd "TAB")))))

;;; Keyboard workflow validation

(deftest e2e/all-keyboard-operations-well-defined ()
  "Validates that all keyboard-driven operations are well-defined."
  ;; All organize functions should be commands (callable interactively)
  (assert-true (commandp 'org-gtd-single-action))
  (assert-true (commandp 'org-gtd-project-new))
  (assert-true (commandp 'org-gtd-delegate))
  (assert-true (commandp 'org-gtd-calendar))
  (assert-true (commandp 'org-gtd-tickler))
  (assert-true (commandp 'org-gtd-knowledge))
  (assert-true (commandp 'org-gtd-habit))
  (assert-true (commandp 'org-gtd-quick-action))
  (assert-true (commandp 'org-gtd-trash))

  ;; Main workflow functions should be commands
  (assert-true (commandp 'org-gtd-capture))
  (assert-true (commandp 'org-gtd-organize))
  (assert-true (commandp 'org-gtd-engage)))

(provide 'true-end-to-end-test)

;;; true-end-to-end-test.el ends here
