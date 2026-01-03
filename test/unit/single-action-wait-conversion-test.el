;;; single-action-wait-conversion-test.el --- Tests for single action WAIT conversion -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the hook that prompts to convert single actions to
;; delegated items when changed to WAIT state.
;;
;; When a single action (ORG_GTD=Actions) is changed to WAIT, the user
;; should be prompted to convert it to a proper delegated item with
;; who and check-in date information.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'with-simulated-input)
(require 'org-gtd-single-action)
(require 'cl-lib)

;; org-state is dynamically bound by org-mode during state change hooks
;; We need to declare it so our let bindings work properly
(defvar org-state nil)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Hook Existence Tests

(deftest single-action-wait/hook-function-exists ()
  "The hook function exists."
  (assert-true (fboundp 'org-gtd-single-action--maybe-convert-to-delegated)))

;;; Integration with org-gtd-mode Tests

(deftest single-action-wait/hook-enabled-by-org-gtd-mode ()
  "Hook is added to org-after-todo-state-change-hook when org-gtd-mode is enabled."
  (org-gtd-mode 1)
  (unwind-protect
      (assert-true (memq 'org-gtd-single-action--maybe-convert-to-delegated
                         org-after-todo-state-change-hook))
    (org-gtd-mode -1)))

(deftest single-action-wait/hook-disabled-by-org-gtd-mode ()
  "Hook is removed from org-after-todo-state-change-hook when org-gtd-mode is disabled."
  (org-gtd-mode 1)
  (org-gtd-mode -1)
  (assert-nil (memq 'org-gtd-single-action--maybe-convert-to-delegated
                    org-after-todo-state-change-hook)))

;;; Trigger Condition Tests

(deftest single-action-wait/triggers-on-actions-to-wait ()
  "Hook triggers when single action (ORG_GTD=Actions) changes to WAIT."
  (with-temp-buffer
    (org-mode)
    (insert (format "* %s Test task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n"
                    (org-gtd-keywords--wait)))
    (goto-char (point-min))
    (org-back-to-heading t)
    ;; Simulate state change context
    ;; org-state is dynamically bound by org-mode in the hook context
    (let ((org-state (org-gtd-keywords--wait))
          (prompted nil)
          (orig-fn (symbol-function 'y-or-n-p)))
      (unwind-protect
          (progn
            (fset 'y-or-n-p (lambda (_prompt) (setq prompted t) nil))
            (org-gtd-single-action--maybe-convert-to-delegated)
            (assert-true prompted))
        (fset 'y-or-n-p orig-fn)))))

(deftest single-action-wait/does-not-trigger-on-other-types ()
  "Hook does not trigger for other ORG_GTD types like Calendar."
  (with-temp-buffer
    (org-mode)
    (insert (format "* %s Test task\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n"
                    (org-gtd-keywords--wait)))
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((org-state (org-gtd-keywords--wait))
          (prompted nil)
          (orig-fn (symbol-function 'y-or-n-p)))
      (unwind-protect
          (progn
            (fset 'y-or-n-p (lambda (_prompt) (setq prompted t) nil))
            (org-gtd-single-action--maybe-convert-to-delegated)
            (assert-nil prompted))
        (fset 'y-or-n-p orig-fn)))))

(deftest single-action-wait/does-not-trigger-on-non-wait-state ()
  "Hook does not trigger when changing to non-WAIT state."
  (with-temp-buffer
    (org-mode)
    (insert (format "* %s Test task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n"
                    (org-gtd-keywords--next)))
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((org-state (org-gtd-keywords--next))  ;; Not WAIT
          (prompted nil)
          (orig-fn (symbol-function 'y-or-n-p)))
      (unwind-protect
          (progn
            (fset 'y-or-n-p (lambda (_prompt) (setq prompted t) nil))
            (org-gtd-single-action--maybe-convert-to-delegated)
            (assert-nil prompted))
        (fset 'y-or-n-p orig-fn)))))

;;; Conversion Tests

(deftest single-action-wait/converts-when-user-confirms ()
  "Converts to delegated item when user confirms."
  (with-temp-buffer
    (org-mode)
    (insert (format "* %s Test task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n"
                    (org-gtd-keywords--wait)))
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((org-state (org-gtd-keywords--wait))
          (orig-yn (symbol-function 'y-or-n-p)))
      (unwind-protect
          (progn
            (fset 'y-or-n-p (lambda (_prompt) t))
            (with-simulated-input "John SPC Doe RET 2025-06-15 RET"
              (org-gtd-single-action--maybe-convert-to-delegated)))
        (fset 'y-or-n-p orig-yn)))
    ;; Verify conversion
    (assert-equal "Delegated" (org-entry-get (point) "ORG_GTD"))
    (assert-equal "John Doe" (org-entry-get (point) "DELEGATED_TO"))
    (assert-match "2025-06-15" (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))

(deftest single-action-wait/preserves-when-user-declines ()
  "Preserves single action state when user declines conversion."
  (with-temp-buffer
    (org-mode)
    (insert (format "* %s Test task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n"
                    (org-gtd-keywords--wait)))
    (goto-char (point-min))
    (org-back-to-heading t)
    (let ((org-state (org-gtd-keywords--wait))
          (orig-fn (symbol-function 'y-or-n-p)))
      (unwind-protect
          (progn
            (fset 'y-or-n-p (lambda (_prompt) nil))
            (org-gtd-single-action--maybe-convert-to-delegated))
        (fset 'y-or-n-p orig-fn)))
    ;; Verify nothing changed
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
    (assert-nil (org-entry-get (point) "DELEGATED_TO"))))

(provide 'single-action-wait-conversion-test)

;;; single-action-wait-conversion-test.el ends here
