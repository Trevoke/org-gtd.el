;;; view-manager-build-test.el --- Tests for the builder transient -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Thin integration tests for the view-manager builder transient.  The
;; interactive read/preview loop is verified manually (see the commit body);
;; here we only assert the transient exists and its keys are wired.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-view-manager)
(require 'org-gtd-test-helper-utils "test/helpers/utils.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

(deftest view-manager-build/is-a-transient-prefix ()
  "The builder is defined as a transient prefix command."
  (assert-true (fboundp 'org-gtd-view-manager--build)))

(deftest view-manager-build/has-save-suffix ()
  "The builder binds `s' to the save action."
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager--build "s")))
    (assert-equal "s" (plist-get plist :key))))

(deftest view-manager-build/has-type-infix ()
  "The builder binds `t' to the generated type infix."
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager--build "t")))
    (assert-equal "t" (plist-get plist :key))))

;;; view-manager-build-test.el ends here
