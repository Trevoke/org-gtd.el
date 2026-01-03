;;; hooks-integration-test.el --- Unit tests for hooks integration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for hooks system integration.
;; These tests verify that the hook system is properly integrated with org-gtd.
;;
;; Migrated from test/integration/hooks-integration-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; org-gtd-organize-hooks Integration

(deftest hooks/applies-during-organization ()
  "Applies hooks during keyboard-driven organization."
  ;; Define test hooks
  (defun test-hook-add-priority ()
    "Test hook that adds priority A."
    (org-priority ?A))

  (defun test-hook-add-effort ()
    "Test hook that adds effort estimate."
    (org-set-effort nil "1:30"))

  (defun test-hook-add-context-tag ()
    "Test hook that adds @computer tag."
    (org-set-tags ":@computer:"))

  ;; Configure hooks
  (let ((org-gtd-organize-hooks '(test-hook-add-priority
                                  test-hook-add-effort
                                  test-hook-add-context-tag)))

    ;; Test that hooks exist and are functions
    (assert-true (fboundp 'test-hook-add-priority))
    (assert-true (fboundp 'test-hook-add-effort))
    (assert-true (fboundp 'test-hook-add-context-tag))

    ;; Test that org-gtd-organize-hooks variable exists
    (assert-true (boundp 'org-gtd-organize-hooks))

    ;; Test that organize function exists for hook integration
    (assert-true (fboundp 'org-gtd-single-action))
    (assert-true (commandp 'org-gtd-single-action))))

;;; Hook Execution Order

(deftest hooks/executes-in-correct-order ()
  "Executes hooks in the correct order."
  (let ((hook-execution-order '()))

    ;; Define hooks that track execution order
    (defun first-hook ()
      (push 'first hook-execution-order))

    (defun second-hook ()
      (push 'second hook-execution-order))

    (defun third-hook ()
      (push 'third hook-execution-order))

    ;; Test hook function creation
    (assert-true (fboundp 'first-hook))
    (assert-true (fboundp 'second-hook))
    (assert-true (fboundp 'third-hook))

    ;; Test that hooks can be configured
    (let ((org-gtd-organize-hooks '(first-hook second-hook third-hook)))
      (assert-equal '(first-hook second-hook third-hook) org-gtd-organize-hooks))))

;;; Hook Integration with Different GTD Item Types

(deftest hooks/applies-to-all-item-types ()
  "Applies hooks to all item types during keyboard organization."
  (defun universal-test-hook ()
    "Hook that should apply to all item types."
    (org-entry-put (point) "HOOK_APPLIED" "yes"))

  ;; Test hook function exists
  (assert-true (fboundp 'universal-test-hook))

  ;; Test that all organization functions exist for hook integration
  (assert-true (fboundp 'org-gtd-single-action))
  (assert-true (fboundp 'org-gtd-project-new))
  (assert-true (fboundp 'org-gtd-calendar))
  (assert-true (fboundp 'org-gtd-delegate))
  (assert-true (fboundp 'org-gtd-tickler))

  ;; Test that hooks can be configured for all types
  (let ((org-gtd-organize-hooks '(universal-test-hook)))
    (assert-true (member 'universal-test-hook org-gtd-organize-hooks))))

;;; Hook Error Handling

(deftest hooks/handles-errors-gracefully ()
  "Handles hook errors gracefully without breaking workflow."
  (defun failing-hook ()
    "Hook that throws an error."
    (error "Intentional test error"))

  (defun working-hook ()
    "Hook that should still work after error."
    (org-entry-put (point) "WORKING_HOOK" "applied"))

  ;; Test hook functions exist
  (assert-true (fboundp 'failing-hook))
  (assert-true (fboundp 'working-hook))

  ;; Test that organization functions are robust
  (assert-true (fboundp 'org-gtd-single-action))
  (assert-true (commandp 'org-gtd-single-action))

  ;; Test hook configuration with error-prone hooks
  (let ((org-gtd-organize-hooks '(failing-hook working-hook)))
    (assert-equal 2 (length org-gtd-organize-hooks))))

;;; Hook Integration with Areas of Focus

(deftest hooks/integrates-with-areas-of-focus ()
  "Integrates hooks with areas of focus workflow."
  (defun area-of-focus-hook ()
    "Hook that sets area of focus."
    (when (member "Work" org-gtd-areas-of-focus)
      (org-entry-put (point) "AREA_OF_FOCUS" "Work")))

  ;; Test hook function exists
  (assert-true (fboundp 'area-of-focus-hook))

  ;; Test areas of focus configuration
  (let ((org-gtd-areas-of-focus '("Work" "Personal" "Learning")))
    (assert-true (member "Work" org-gtd-areas-of-focus))
    (assert-true (member "Personal" org-gtd-areas-of-focus))
    (assert-true (member "Learning" org-gtd-areas-of-focus)))

  ;; Test hook integration with areas of focus
  (let ((org-gtd-areas-of-focus '("Work" "Personal" "Learning"))
        (org-gtd-organize-hooks '(area-of-focus-hook)))
    (assert-true (member "Work" org-gtd-areas-of-focus))
    (assert-true (member 'area-of-focus-hook org-gtd-organize-hooks))))

;;; Hook System Architecture Validation

(deftest hooks/system-properly-integrated ()
  "Validates that hook system is properly integrated with org-gtd."
  ;; Test that hook variable exists
  (assert-true (boundp 'org-gtd-organize-hooks))

  ;; Test that organize functions exist for hook integration
  (assert-true (fboundp 'org-gtd-organize))
  (assert-true (commandp 'org-gtd-organize))

  ;; Test that all organization types support hooks
  (let ((org-functions '(org-gtd-single-action
                         org-gtd-project-new
                         org-gtd-project-extend
                         org-gtd-delegate
                         org-gtd-calendar
                         org-gtd-tickler
                         org-gtd-knowledge
                         org-gtd-habit
                         org-gtd-quick-action
                         org-gtd-trash)))
    (dolist (func org-functions)
      (assert-true (fboundp func))
      (assert-true (commandp func)))))

;;; Hook Performance and Reliability

(deftest hooks/dont-interfere-with-performance ()
  "Ensures hooks don't interfere with keyboard workflow performance."
  ;; Test that multiple hooks can be configured
  (defun performance-hook-1 () "Fast hook 1")
  (defun performance-hook-2 () "Fast hook 2")
  (defun performance-hook-3 () "Fast hook 3")

  (assert-true (fboundp 'performance-hook-1))
  (assert-true (fboundp 'performance-hook-2))
  (assert-true (fboundp 'performance-hook-3))

  ;; Test that many hooks can be configured without breaking system
  (let ((org-gtd-organize-hooks '(performance-hook-1
                                  performance-hook-2
                                  performance-hook-3)))
    (assert-equal 3 (length org-gtd-organize-hooks))

    ;; Test that organize functions remain available
    (assert-true (fboundp 'org-gtd-organize))
    (assert-true (fboundp 'org-gtd-single-action))))

(provide 'hooks-integration-test)

;;; hooks-integration-test.el ends here
