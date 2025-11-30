;;; types-test.el --- Unit tests for GTD type system -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the org-gtd type system (org-gtd-types).
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from test/types-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd)
(require 'org-gtd-types)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; org-gtd-types Constant

(deftest types-defines-all-expected-gtd-types ()
  "org-gtd-types defines all expected GTD types."
  (assert-true org-gtd-types)
  (assert-true (assq 'next-action org-gtd-types))
  (assert-true (assq 'delegated org-gtd-types))
  (assert-true (assq 'calendar org-gtd-types))
  (assert-true (assq 'tickler org-gtd-types))
  (assert-true (assq 'someday org-gtd-types))
  (assert-true (assq 'project org-gtd-types))
  (assert-true (assq 'habit org-gtd-types))
  (assert-true (assq 'reference org-gtd-types))
  (assert-true (assq 'trash org-gtd-types))
  (assert-true (assq 'quick-action org-gtd-types)))

(deftest types-maps-each-type-to-correct-org-gtd-value ()
  "org-gtd-types maps each type to correct ORG_GTD value."
  (assert-equal "Actions"
                (plist-get (cdr (assq 'next-action org-gtd-types)) :org-gtd))
  (assert-equal "Delegated"
                (plist-get (cdr (assq 'delegated org-gtd-types)) :org-gtd))
  (assert-equal "Calendar"
                (plist-get (cdr (assq 'calendar org-gtd-types)) :org-gtd))
  (assert-equal "Tickler"
                (plist-get (cdr (assq 'tickler org-gtd-types)) :org-gtd))
  (assert-equal "Someday"
                (plist-get (cdr (assq 'someday org-gtd-types)) :org-gtd))
  (assert-equal "Projects"
                (plist-get (cdr (assq 'project org-gtd-types)) :org-gtd))
  (assert-equal "Habit"
                (plist-get (cdr (assq 'habit org-gtd-types)) :org-gtd))
  (assert-equal "Reference"
                (plist-get (cdr (assq 'reference org-gtd-types)) :org-gtd))
  (assert-equal "Trash"
                (plist-get (cdr (assq 'trash org-gtd-types)) :org-gtd))
  (assert-equal "Quick"
                (plist-get (cdr (assq 'quick-action org-gtd-types)) :org-gtd)))

(deftest types-maps-to-correct-todo-state-semantics ()
  "org-gtd-types maps types to correct TODO state semantics."
  (assert-equal :next
                (plist-get (cdr (assq 'next-action org-gtd-types)) :state))
  (assert-equal :wait
                (plist-get (cdr (assq 'delegated org-gtd-types)) :state))
  (assert-equal :done
                (plist-get (cdr (assq 'reference org-gtd-types)) :state))
  (assert-equal :canceled
                (plist-get (cdr (assq 'trash org-gtd-types)) :state))
  (assert-equal :done
                (plist-get (cdr (assq 'quick-action org-gtd-types)) :state)))

;;; org-gtd-type-get

(deftest type-get-returns-definition-for-valid-type ()
  "org-gtd-type-get returns type definition for valid type name."
  (let ((result (org-gtd-type-get 'delegated)))
    (assert-true result)
    (assert-equal 'delegated (car result))))

(deftest type-get-returns-nil-for-unknown-type ()
  "org-gtd-type-get returns nil for unknown type."
  (assert-nil (org-gtd-type-get 'nonexistent-type)))

;;; org-gtd-type-org-gtd-value

(deftest type-org-gtd-value-returns-property-value ()
  "org-gtd-type-org-gtd-value returns ORG_GTD property value for type."
  (assert-equal "Delegated" (org-gtd-type-org-gtd-value 'delegated))
  (assert-equal "Actions" (org-gtd-type-org-gtd-value 'next-action))
  (assert-equal "Calendar" (org-gtd-type-org-gtd-value 'calendar)))

(deftest type-org-gtd-value-returns-nil-for-unknown ()
  "org-gtd-type-org-gtd-value returns nil for unknown type."
  (assert-nil (org-gtd-type-org-gtd-value 'nonexistent)))

;;; org-gtd-type-state

(deftest type-state-returns-semantic-for-type ()
  "org-gtd-type-state returns state semantic for type."
  (assert-equal :wait (org-gtd-type-state 'delegated))
  (assert-equal :next (org-gtd-type-state 'next-action))
  (assert-nil (org-gtd-type-state 'calendar)))

;;; org-gtd-type-from-org-gtd-value

(deftest type-from-org-gtd-value-returns-type-name ()
  "org-gtd-type-from-org-gtd-value returns type name for ORG_GTD value."
  (assert-equal 'delegated (org-gtd-type-from-org-gtd-value "Delegated"))
  (assert-equal 'next-action (org-gtd-type-from-org-gtd-value "Actions"))
  (assert-equal 'calendar (org-gtd-type-from-org-gtd-value "Calendar"))
  (assert-equal 'tickler (org-gtd-type-from-org-gtd-value "Tickler"))
  (assert-equal 'someday (org-gtd-type-from-org-gtd-value "Someday"))
  (assert-equal 'project (org-gtd-type-from-org-gtd-value "Projects")))

(deftest type-from-org-gtd-value-returns-nil-for-unknown ()
  "org-gtd-type-from-org-gtd-value returns nil for unknown ORG_GTD value."
  (assert-nil (org-gtd-type-from-org-gtd-value "Unknown")))

;;; types-test.el ends here
