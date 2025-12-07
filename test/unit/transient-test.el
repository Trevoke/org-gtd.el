;;; transient-test.el --- Unit tests for transient menus -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd transient menu systems.
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from:
;; - test/org-gtd-agenda-transient-test.el
;; - test/org-gtd-graph-transient-test.el
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-agenda-transient)
(require 'org-gtd-graph-transient)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; org-gtd-agenda-transient Tests

(deftest agenda-transient/is-valid-prefix ()
  "org-gtd-agenda-transient is a valid transient prefix."
  (assert-true (functionp 'org-gtd-agenda-transient))
  (assert-true (get 'org-gtd-agenda-transient 'transient--prefix)))

(deftest agenda-transient/is-interactive-command ()
  "org-gtd-agenda-transient can be invoked interactively."
  (assert-true (commandp 'org-gtd-agenda-transient)))

;;; org-gtd-agenda-transient--has-timestamp-p Tests

(deftest agenda-transient/has-timestamp-calendar ()
  "Returns truthy for Calendar items."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Calendar task\n")
    (goto-char (point-min))
    (org-entry-put (point) "ORG_GTD" "Calendar")
    (assert-true (org-gtd-agenda-transient--has-timestamp-p))))

(deftest agenda-transient/has-timestamp-delegated ()
  "Returns truthy for Delegated items."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Delegated task\n")
    (goto-char (point-min))
    (org-entry-put (point) "ORG_GTD" "Delegated")
    (assert-true (org-gtd-agenda-transient--has-timestamp-p))))

(deftest agenda-transient/has-timestamp-tickler ()
  "Returns truthy for Tickler items."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Tickler task\n")
    (goto-char (point-min))
    (org-entry-put (point) "ORG_GTD" "Tickler")
    (assert-true (org-gtd-agenda-transient--has-timestamp-p))))

(deftest agenda-transient/has-timestamp-habit-nil ()
  "Returns nil for Habit items (excluded from Time section)."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Habit task\n")
    (goto-char (point-min))
    (org-entry-put (point) "ORG_GTD" "Habit")
    (assert-nil (org-gtd-agenda-transient--has-timestamp-p))))

(deftest agenda-transient/has-timestamp-actions-nil ()
  "Returns nil for Actions items."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Action task\n")
    (goto-char (point-min))
    (org-entry-put (point) "ORG_GTD" "Actions")
    (assert-nil (org-gtd-agenda-transient--has-timestamp-p))))

(deftest agenda-transient/has-timestamp-no-property-nil ()
  "Returns nil for items without ORG_GTD property."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Regular task\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-agenda-transient--has-timestamp-p))))

;;; Clarify Action Tests

(deftest agenda-transient/clarify-refile-callable ()
  "Has a callable clarify-refile function."
  (assert-true (commandp 'org-gtd-agenda-transient--clarify-refile)))

(deftest agenda-transient/clarify-in-place-callable ()
  "Has a callable clarify-in-place function."
  (assert-true (commandp 'org-gtd-agenda-transient--clarify-in-place)))

;;; Clocking and Metadata Action Tests

(deftest agenda-transient/clocking-functions-callable ()
  "Has callable clocking functions."
  (assert-true (commandp 'org-gtd-agenda-transient--clock-in))
  (assert-true (commandp 'org-gtd-agenda-transient--clock-out)))

(deftest agenda-transient/metadata-functions-callable ()
  "Has callable metadata functions."
  (assert-true (commandp 'org-gtd-agenda-transient--effort))
  (assert-true (commandp 'org-gtd-agenda-transient--priority))
  (assert-true (commandp 'org-gtd-agenda-transient--tags))
  (assert-true (commandp 'org-gtd-agenda-transient--note))
  (assert-true (commandp 'org-gtd-agenda-transient--area-of-focus)))

;;; org-gtd-graph-transient Tests

(deftest graph-transient/is-valid-prefix ()
  "org-gtd-graph-transient-main is a valid transient prefix."
  (assert-true (functionp 'org-gtd-graph-transient-main))
  (assert-true (get 'org-gtd-graph-transient-main 'transient--prefix)))

(deftest graph-transient/is-interactive-command ()
  "org-gtd-graph-transient-main can be invoked interactively."
  (assert-true (commandp 'org-gtd-graph-transient-main)))

;;; Placeholder Command Tests

(deftest graph-transient/navigation-commands-defined ()
  "Navigation commands are defined and callable."
  (assert-true (commandp 'org-gtd-graph-nav-next-sibling))
  (assert-true (commandp 'org-gtd-graph-nav-goto)))

(deftest graph-transient/view-commands-defined ()
  "View commands are defined and callable."
  (assert-true (commandp 'org-gtd-graph-transient-zoom)))

(deftest graph-transient/quit-command-defined ()
  "Quit command is defined and callable."
  (assert-true (commandp 'org-gtd-graph-quit-and-kill)))

(provide 'transient-test)

;;; transient-test.el ends here
