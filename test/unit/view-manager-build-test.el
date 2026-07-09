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

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

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

(deftest view-manager-build/save-rejects-blank-name ()
  "A blank name errors, writes nothing, and leaves the builder dirty.
Guards against silently persisting a nameless `(name . \"\")' entry
that would surface as a blank candidate in `org-gtd-view-run'."
  (setq org-gtd-view-manager--build-state
        (list (cons 'name "Untitled") (cons 'type 'next-action)))
  (setq org-gtd-view-manager--build-dirty t)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
            ;; Keep the test hermetic: don't register a resume timer.
            ((symbol-function 'org-gtd-view-manager--build-resume) #'ignore))
    (assert-raises 'user-error (org-gtd-view-manager--save)))
  (assert-equal nil (org-gtd-view-manager--store-read))
  (assert-true org-gtd-view-manager--build-dirty))

(deftest view-manager-build/flag-infix-toggles-off ()
  "Re-selecting a flag infix unsets it.
Regression: a flag such as `not-done' could be set but never cleared,
so a builder session could not remove it once added."
  (setq org-gtd-view-manager--build-state (list (cons 'name "x")))
  (org-gtd-view-manager--set-value 'not-done)
  (assert-equal t (cdr (assq 'not-done org-gtd-view-manager--build-state)))
  (org-gtd-view-manager--set-value 'not-done)
  (assert-nil (assq 'not-done org-gtd-view-manager--build-state))
  (when (timerp org-gtd-view-manager--preview-timer)
    (cancel-timer org-gtd-view-manager--preview-timer)))

;;; view-manager-build-test.el ends here
