;;; backward-compatibility-test.el --- Unit tests for backward compatibility -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests verifying backward compatibility aliases work correctly.
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from test/backward-compatibility-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Backward Compatibility Aliases for incubate→tickler Rename

(deftest incubate-function-is-obsolete-alias-for-tickler ()
  "org-gtd-incubate is an obsolete alias for org-gtd-tickler."
  ;; Verify the function exists
  (assert-true (fboundp 'org-gtd-incubate))
  ;; Verify it's marked as obsolete with the correct replacement
  (let ((obsolete-info (get 'org-gtd-incubate 'byte-obsolete-info)))
    (assert-equal 'org-gtd-tickler (car obsolete-info))
    (assert-equal "4.0" (caddr obsolete-info))))

(deftest incubate-create-is-obsolete-alias-for-tickler-create ()
  "org-gtd-incubate-create is an obsolete alias for org-gtd-tickler-create."
  ;; Verify the function exists
  (assert-true (fboundp 'org-gtd-incubate-create))
  ;; Verify it's marked as obsolete with the correct replacement
  (let ((obsolete-info (get 'org-gtd-incubate-create 'byte-obsolete-info)))
    (assert-equal 'org-gtd-tickler-create (car obsolete-info))
    (assert-equal "4.0" (caddr obsolete-info))))

(deftest incubate-constant-is-obsolete-alias-for-tickler ()
  "org-gtd-incubate constant is an obsolete alias for org-gtd-tickler."
  ;; Verify the constant is accessible via the old name
  (assert-true (boundp 'org-gtd-incubate))
  ;; Verify it has the same value as the new constant
  (assert-equal org-gtd-tickler org-gtd-incubate)
  (assert-equal "Tickler" org-gtd-incubate)
  ;; Verify it's marked as obsolete
  (let ((obsolete-info (get 'org-gtd-incubate 'byte-obsolete-variable)))
    (assert-equal 'org-gtd-tickler (car obsolete-info))
    (assert-equal "4.0" (caddr obsolete-info))))

;;; backward-compatibility-test.el ends here
