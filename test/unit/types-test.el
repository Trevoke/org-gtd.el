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

(deftest types-calendar-declares-wiring-fields ()
  "calendar type declares disposition, transient-key, and prompt-to-refile."
  (assert-same 'list (org-gtd-type-disposition 'calendar))
  (assert-equal "c" (org-gtd-type-transient-key 'calendar))
  (assert-same t (org-gtd-type-prompt-to-refile 'calendar)))

(deftest types-delegated-declares-wiring-fields ()
  "delegated type declares disposition, transient-key, prompt-to-refile, organize-fn."
  (assert-same 'list (org-gtd-type-disposition 'delegated))
  (assert-equal "d" (org-gtd-type-transient-key 'delegated))
  (assert-same t (org-gtd-type-prompt-to-refile 'delegated))
  (assert-equal 'org-gtd-delegate--organize (org-gtd-type-organize-fn 'delegated)))

(deftest types-next-action-declares-wiring-fields ()
  "next-action type declares disposition, transient-key, prompt-to-refile."
  (assert-same 'list (org-gtd-type-disposition 'next-action))
  (assert-equal "s" (org-gtd-type-transient-key 'next-action))
  (assert-same t (org-gtd-type-prompt-to-refile 'next-action)))

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

;;; org-gtd-type-refile-target

(deftest type-refile-target-falls-back-to-org-gtd-value ()
  "refile-target falls back to :org-gtd when :refile-target not set."
  (assert-equal "Calendar" (org-gtd-type-refile-target 'calendar)))

(deftest type-refile-target-uses-override-for-delegated ()
  "delegated type declares :refile-target \"Actions\"."
  (assert-equal "Actions" (org-gtd-type-refile-target 'delegated)))

(deftest type-refile-target-uses-override-for-habit ()
  "habit type declares :refile-target \"Habits\"."
  (assert-equal "Habits" (org-gtd-type-refile-target 'habit)))

(deftest type-refile-target-returns-nil-for-unknown ()
  "refile-target returns nil for unregistered types."
  (assert-nil (org-gtd-type-refile-target 'nonexistent)))

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

;;; org-gtd-type-property

(deftest type-property-resolves-who-for-delegated ()
  "org-gtd-type-property resolves :who semantic to DELEGATED_TO for delegated type."
  (assert-equal "DELEGATED_TO" (org-gtd-type-property 'delegated :who)))

(deftest type-property-resolves-when-for-delegated ()
  "org-gtd-type-property resolves :when semantic to ORG_GTD_TIMESTAMP for delegated type."
  (assert-equal "ORG_GTD_TIMESTAMP" (org-gtd-type-property 'delegated :when)))

(deftest type-property-resolves-when-for-calendar ()
  "org-gtd-type-property resolves :when semantic to ORG_GTD_TIMESTAMP for calendar type."
  (assert-equal "ORG_GTD_TIMESTAMP" (org-gtd-type-property 'calendar :when)))

(deftest type-property-resolves-when-for-habit ()
  "org-gtd-type-property resolves :when semantic to SCHEDULED for habit type."
  (assert-equal "SCHEDULED" (org-gtd-type-property 'habit :when)))

(deftest type-property-returns-nil-for-missing-semantic ()
  "org-gtd-type-property returns nil for type without the semantic property."
  (assert-nil (org-gtd-type-property 'next-action :when))
  (assert-nil (org-gtd-type-property 'next-action :who)))

;;; org-gtd-type-properties

(deftest type-properties-returns-list-for-type-with-properties ()
  "org-gtd-type-properties returns list of semantic properties for delegated type."
  (let ((props (org-gtd-type-properties 'delegated)))
    (assert-true props)
    (assert-equal 2 (length props))
    ;; Check :who property exists
    (let ((who-prop (seq-find (lambda (p) (eq (car p) :who)) props)))
      (assert-true who-prop)
      (assert-equal "DELEGATED_TO" (plist-get (cdr who-prop) :org-property))
      (assert-equal 'text (plist-get (cdr who-prop) :type))
      (assert-true (plist-get (cdr who-prop) :required)))))

(deftest type-properties-returns-nil-for-type-without-properties ()
  "org-gtd-type-properties returns nil for type without properties."
  (assert-nil (org-gtd-type-properties 'next-action)))

(deftest type-properties-someday-has-no-properties ()
  "someday type has no properties (no timestamp required)."
  (assert-nil (org-gtd-type-properties 'someday)))

;;;; org-gtd-user-types

(deftest user-types-variable-exists ()
  "org-gtd-user-types variable exists."
  (assert-true (boundp 'org-gtd-user-types)))

(deftest user-types-defaults-to-empty-list ()
  "org-gtd-user-types defaults to empty list."
  (let ((org-gtd-user-types '()))
    (assert-equal '() org-gtd-user-types)))

(deftest user-types-returns-builtin-unchanged-when-no-override ()
  "Returns builtin type unchanged when no user override."
  (let ((org-gtd-user-types '()))
    (let ((result (org-gtd-type-get 'delegated)))
      (assert-equal 'delegated (car result))
      (assert-equal "Delegated" (plist-get (cdr result) :org-gtd)))))

(deftest user-types-merges-user-property-override ()
  "Merges user property override with builtin."
  (let ((org-gtd-user-types
         '((delegated
            :properties
            ((:who :org-property "DELEGATED_TO" :type text :required t
                   :prompt "Custom prompt"))))))
    (let* ((result (org-gtd-type-get 'delegated))
           (props (plist-get (cdr result) :properties))
           (who-prop (seq-find (lambda (p) (eq (car p) :who)) props)))
      ;; User prompt should override
      (assert-equal "Custom prompt" (plist-get (cdr who-prop) :prompt))
      ;; :when property from builtin should still exist
      (assert-true (seq-find (lambda (p) (eq (car p) :when)) props)))))

(deftest user-types-can-add-new-properties ()
  "User can add new properties to existing type."
  (let ((org-gtd-user-types
         '((next-action
            :properties
            ((:custom :org-property "MY_CUSTOM" :type text :required nil
                      :prompt "Custom value"))))))
    (let* ((result (org-gtd-type-get 'next-action))
           (props (plist-get (cdr result) :properties)))
      (assert-true (seq-find (lambda (p) (eq (car p) :custom)) props)))))

(deftest user-types-never-overrides-org-gtd ()
  "Never overrides :org-gtd from user config."
  (let ((org-gtd-user-types
         '((delegated
            :org-gtd "HackedValue"))))
    (let ((result (org-gtd-type-get 'delegated)))
      ;; Should still be the builtin value
      (assert-equal "Delegated" (plist-get (cdr result) :org-gtd)))))

(deftest user-types-allows-overriding-state ()
  "Allows overriding :state."
  (let ((org-gtd-user-types
         '((delegated
            :state :next))))
    (let ((result (org-gtd-type-get 'delegated)))
      (assert-equal :next (plist-get (cdr result) :state)))))

(deftest user-types-ignores-unknown-type-names ()
  "Ignores unknown type names in user-types."
  (let ((org-gtd-user-types
         '((nonexistent-type
            :properties ((:foo :org-property "FOO" :type text))))))
    ;; Should not error, just return nil for unknown type
    (assert-nil (org-gtd-type-get 'nonexistent-type))
    ;; Builtin types should still work
    (assert-true (org-gtd-type-get 'delegated))))

(deftest type-wiring-accessors-return-nil-for-unknown-type ()
  "All wiring-field accessors return nil for unregistered types."
  (let ((org-gtd-types '((known :org-gtd "K" :state nil :properties nil))))
    (assert-nil (org-gtd-type-organize-fn 'bogus))
    (assert-nil (org-gtd-type-disposition 'bogus))
    (assert-nil (org-gtd-type-organize-project-fn 'bogus))
    (assert-nil (org-gtd-type-prompt-to-refile 'bogus))
    (assert-nil (org-gtd-type-transient-key 'bogus))
    (assert-nil (org-gtd-type-hooks 'bogus))))

(deftest type-wiring-fields-return-defaults-when-absent ()
  "Wiring-field accessors return documented defaults when not declared."
  (let ((org-gtd-types '((fake :org-gtd "Fake" :state nil :properties nil))))
    (assert-same #'org-gtd-configure-as-type (org-gtd-type-organize-fn 'fake))
    (assert-same 'list (org-gtd-type-disposition 'fake))
    (assert-nil (org-gtd-type-organize-project-fn 'fake))
    (assert-nil (org-gtd-type-prompt-to-refile 'fake))
    (assert-nil (org-gtd-type-transient-key 'fake))
    (assert-nil (org-gtd-type-hooks 'fake))))

(deftest merge-preserves-builtin-wiring-when-user-omits ()
  "Builtin wiring fields survive a user override that does not mention them."
  (let* ((builtin '(t1 :org-gtd "T1" :state nil :properties nil
                       :organize-fn my/fn
                       :disposition done-and-archive
                       :organize-project-fn my/proj
                       :prompt-to-refile t
                       :transient-key "c"))
         (user    '(t1 :properties nil))
         (merged  (org-gtd--merge-type-definitions builtin user))
         (plist   (cdr merged)))
    (assert-same 'my/fn (plist-get plist :organize-fn))
    (assert-same 'done-and-archive (plist-get plist :disposition))
    (assert-same 'my/proj (plist-get plist :organize-project-fn))
    (assert-same t (plist-get plist :prompt-to-refile))
    (assert-equal "c" (plist-get plist :transient-key))))

(deftest merge-scalar-user-value-replaces-builtin ()
  "A user scalar value replaces the builtin value."
  (let* ((builtin '(t1 :org-gtd "T1" :state nil :properties nil
                       :prompt-to-refile nil
                       :disposition list))
         (user    '(t1 :prompt-to-refile t :disposition done-and-archive))
         (merged  (org-gtd--merge-type-definitions builtin user))
         (plist   (cdr merged)))
    (assert-same t (plist-get plist :prompt-to-refile))
    (assert-same 'done-and-archive (plist-get plist :disposition))))

(deftest merge-hooks-per-stage-append ()
  "Local hooks append per stage; stages not touched by user are preserved."
  (let* ((builtin '(t1 :org-gtd "T1" :state nil :properties nil
                       :hooks (:after-organize (fn-a)
                               :before-file (fn-x))))
         (user    '(t1 :hooks (:after-organize (fn-b))))
         (merged  (org-gtd--merge-type-definitions builtin user))
         (hooks   (plist-get (cdr merged) :hooks)))
    (assert-equal '(fn-a fn-b) (plist-get hooks :after-organize))
    (assert-equal '(fn-x) (plist-get hooks :before-file))))

(deftest merge-never-overrides-org-gtd-value ()
  "User cannot override the :org-gtd property value."
  (let* ((builtin '(t1 :org-gtd "T1" :state nil :properties nil))
         (user    '(t1 :org-gtd "Hacked"))
         (merged  (org-gtd--merge-type-definitions builtin user)))
    (assert-equal "T1" (plist-get (cdr merged) :org-gtd))))

;;; org-gtd-customize-type

(deftest customize-type-single-merges-scalar ()
  "Single-type customize-type replaces a scalar field."
  (let ((org-gtd-types (copy-tree org-gtd-types)))
    (org-gtd-customize-type 'calendar :prompt-to-refile t)
    (assert-same t (org-gtd-type-prompt-to-refile 'calendar))))

(deftest customize-type-list-applies-to-each ()
  "List-form customize-type merges into every listed type."
  (let ((org-gtd-types (copy-tree org-gtd-types)))
    (org-gtd-customize-type '(calendar tickler) :prompt-to-refile t)
    (assert-same t (org-gtd-type-prompt-to-refile 'calendar))
    (assert-same t (org-gtd-type-prompt-to-refile 'tickler))))

(deftest customize-type-hooks-append-across-calls ()
  "Successive customize-type calls append to the same hook stage."
  (let ((org-gtd-types (copy-tree org-gtd-types)))
    (org-gtd-customize-type 'calendar :hooks '(:after-file (fn-a)))
    (org-gtd-customize-type 'calendar :hooks '(:after-file (fn-b)))
    (assert-equal '(fn-a fn-b)
                  (plist-get (org-gtd-type-hooks 'calendar) :after-file))))

(deftest customize-type-unknown-type-errors ()
  "Customizing an unregistered type signals an error."
  (let ((org-gtd-types (copy-tree org-gtd-types)))
    (assert-raises 'error
                   (org-gtd-customize-type 'nonsense :prompt-to-refile t))))

(provide 'types-test)

;;; types-test.el ends here
