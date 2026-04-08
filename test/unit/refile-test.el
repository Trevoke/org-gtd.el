;;; refile-test.el --- Unit tests for org-gtd-refile -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-refile helpers, focused on
;; `org-gtd-refile--should-prompt-p' and the registry-driven
;; :prompt-to-refile precedence.
;;

;;; Code:

(require 'e-unit)
(require 'cl-lib)
(require 'org-gtd)
(require 'org-gtd-types)
(require 'org-gtd-refile)

(e-unit-initialize)

(defmacro refile-test--with-type (plist &rest body)
  "Run BODY with a temporary `test-type' registered with PLIST.
Also binds `org-gtd-refile-to-any-target' to nil by default."
  (declare (indent 1))
  `(let ((org-gtd-refile-to-any-target nil)
         (org-gtd-refile-prompt-for-types nil)
         (org-gtd-refile-prompt-default nil)
         (org-gtd-types (cons (cons 'test-type ,plist)
                              org-gtd-types))
         (org-gtd-user-types nil))
     ,@body))

(deftest refile-should-prompt-p-registry-explicit-t ()
  "Type with :prompt-to-refile t returns t regardless of default."
  (refile-test--with-type '(:org-gtd "TestType" :prompt-to-refile t)
    (let ((org-gtd-refile-prompt-default nil))
      (assert-true (org-gtd-refile--should-prompt-p 'test-type)))))

(deftest refile-should-prompt-p-registry-explicit-nil ()
  "Explicit :prompt-to-refile nil overrides a non-nil default."
  (refile-test--with-type '(:org-gtd "TestType" :prompt-to-refile nil)
    (let ((org-gtd-refile-prompt-default t))
      (assert-nil (org-gtd-refile--should-prompt-p 'test-type)))))

(deftest refile-should-prompt-p-falls-back-to-default ()
  "Type without :prompt-to-refile uses `org-gtd-refile-prompt-default'."
  (refile-test--with-type '(:org-gtd "TestType")
    (let ((org-gtd-refile-prompt-default t))
      (assert-true (org-gtd-refile--should-prompt-p 'test-type)))
    (let ((org-gtd-refile-prompt-default nil))
      (assert-nil (org-gtd-refile--should-prompt-p 'test-type)))))

(deftest refile-should-prompt-p-legacy-var-no-longer-consulted ()
  "Mutating `org-gtd-refile-prompt-for-types' after load has no effect.
The legacy variable is migrated at load time and then ignored."
  (refile-test--with-type '(:org-gtd "TestType")
    (let ((org-gtd-refile-prompt-for-types '(test-type))
          (org-gtd-refile-prompt-default nil))
      (assert-nil (org-gtd-refile--should-prompt-p 'test-type)))))

(deftest refile-load-time-migration-populates-registry ()
  "Load-time migration set :prompt-to-refile t on legacy default types.
`calendar' is in the default `org-gtd-refile-prompt-for-types' and is a
registered type, so the migration should have marked it."
  (assert-true (org-gtd-type-prompt-to-refile-set-p 'calendar))
  (assert-true (org-gtd-type-prompt-to-refile 'calendar))
  (assert-true (org-gtd-type-prompt-to-refile-set-p 'delegated))
  (assert-true (org-gtd-type-prompt-to-refile 'delegated)))

(deftest refile-should-prompt-p-any-target-deprecated-returns-nil ()
  "`org-gtd-refile-to-any-target' non-nil short-circuits to nil."
  (refile-test--with-type '(:org-gtd "TestType" :prompt-to-refile t)
    (let ((org-gtd-refile-to-any-target t)
          (org-gtd-refile--deprecated-warning-shown t))
      (assert-nil (org-gtd-refile--should-prompt-p 'test-type)))))

(deftest refile-type-prompt-to-refile-set-p ()
  "`org-gtd-type-prompt-to-refile-set-p' distinguishes absent vs explicit nil."
  (refile-test--with-type '(:org-gtd "TestType" :prompt-to-refile nil)
    (assert-true (org-gtd-type-prompt-to-refile-set-p 'test-type)))
  (refile-test--with-type '(:org-gtd "TestType")
    (assert-nil (org-gtd-type-prompt-to-refile-set-p 'test-type))))

(provide 'refile-test)

;;; refile-test.el ends here
