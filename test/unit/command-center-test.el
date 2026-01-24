;;; command-center-test.el --- Unit tests for command center transient -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests verifying the command center transient and its commands exist.
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from test/command-center-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Command Center Transient

(deftest command-center-is-defined-as-transient-prefix ()
  "org-gtd-command-center is defined as a transient prefix."
  (assert-true (fboundp 'org-gtd-command-center)))

(deftest command-center-contains-engage-commands ()
  "Command center contains engage commands."
  (assert-true (fboundp 'org-gtd-engage))
  (assert-true (fboundp 'org-gtd-engage-tagged))
  (assert-true (fboundp 'org-gtd-show-all-next)))

(deftest command-center-contains-capture-and-process-commands ()
  "Command center contains capture and process commands."
  (assert-true (fboundp 'org-gtd-capture))
  (assert-true (fboundp 'org-gtd-process-inbox))
  (assert-true (fboundp 'org-gtd-clarify-item)))

(deftest command-center-contains-reflect-commands ()
  "Command center contains reflect commands."
  (assert-true (fboundp 'org-gtd-reflect-area-of-focus))
  (assert-true (fboundp 'org-gtd-reflect-someday-maybe))
  (assert-true (fboundp 'org-gtd-reflect-upcoming-delegated))
  (assert-true (fboundp 'org-gtd-reflect-completed-items))
  (assert-true (fboundp 'org-gtd-reflect-completed-projects)))

(deftest command-center-has-stuck-items-submenu ()
  "Command center has a stuck items sub-menu."
  (assert-true (fboundp 'org-gtd-command-center--stuck)))

(deftest command-center-has-missed-items-submenu ()
  "Command center has a missed items sub-menu."
  (assert-true (fboundp 'org-gtd-command-center--missed)))

;;; command-center-test.el ends here
