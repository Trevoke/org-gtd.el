;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'buttercup)

;; This will fail until org-gtd-types.el is created
(require 'org-gtd-types)

(describe "org-gtd-types constant"

  (it "defines all expected GTD types"
    (expect org-gtd-types :to-be-truthy)
    (expect (assq 'next-action org-gtd-types) :to-be-truthy)
    (expect (assq 'delegated org-gtd-types) :to-be-truthy)
    (expect (assq 'calendar org-gtd-types) :to-be-truthy)
    (expect (assq 'incubated org-gtd-types) :to-be-truthy)
    (expect (assq 'project org-gtd-types) :to-be-truthy)
    (expect (assq 'habit org-gtd-types) :to-be-truthy)
    (expect (assq 'reference org-gtd-types) :to-be-truthy)
    (expect (assq 'trash org-gtd-types) :to-be-truthy)
    (expect (assq 'quick-action org-gtd-types) :to-be-truthy))

  (it "maps each type to correct ORG_GTD value"
    (expect (plist-get (cdr (assq 'next-action org-gtd-types)) :org-gtd)
            :to-equal "Actions")
    (expect (plist-get (cdr (assq 'delegated org-gtd-types)) :org-gtd)
            :to-equal "Delegated")
    (expect (plist-get (cdr (assq 'calendar org-gtd-types)) :org-gtd)
            :to-equal "Calendar")
    (expect (plist-get (cdr (assq 'incubated org-gtd-types)) :org-gtd)
            :to-equal "Incubated")
    (expect (plist-get (cdr (assq 'project org-gtd-types)) :org-gtd)
            :to-equal "Projects")
    (expect (plist-get (cdr (assq 'habit org-gtd-types)) :org-gtd)
            :to-equal "Habit")
    (expect (plist-get (cdr (assq 'reference org-gtd-types)) :org-gtd)
            :to-equal "Reference")
    (expect (plist-get (cdr (assq 'trash org-gtd-types)) :org-gtd)
            :to-equal "Trash")
    (expect (plist-get (cdr (assq 'quick-action org-gtd-types)) :org-gtd)
            :to-equal "Quick"))

  (it "maps types to correct TODO state semantics"
    (expect (plist-get (cdr (assq 'next-action org-gtd-types)) :state)
            :to-equal :next)
    (expect (plist-get (cdr (assq 'delegated org-gtd-types)) :state)
            :to-equal :wait)
    (expect (plist-get (cdr (assq 'reference org-gtd-types)) :state)
            :to-equal :done)
    (expect (plist-get (cdr (assq 'trash org-gtd-types)) :state)
            :to-equal :canceled)
    (expect (plist-get (cdr (assq 'quick-action org-gtd-types)) :state)
            :to-equal :done)))

(describe "org-gtd-type-get"

  (it "returns type definition for valid type name"
    (let ((result (org-gtd-type-get 'delegated)))
      (expect result :to-be-truthy)
      (expect (car result) :to-equal 'delegated)))

  (it "returns nil for unknown type"
    (expect (org-gtd-type-get 'nonexistent-type) :to-be nil)))

(describe "org-gtd-type-org-gtd-value"

  (it "returns ORG_GTD property value for type"
    (expect (org-gtd-type-org-gtd-value 'delegated) :to-equal "Delegated")
    (expect (org-gtd-type-org-gtd-value 'next-action) :to-equal "Actions")
    (expect (org-gtd-type-org-gtd-value 'calendar) :to-equal "Calendar"))

  (it "returns nil for unknown type"
    (expect (org-gtd-type-org-gtd-value 'nonexistent) :to-be nil)))

(describe "org-gtd-type-state"

  (it "returns state semantic for type"
    (expect (org-gtd-type-state 'delegated) :to-equal :wait)
    (expect (org-gtd-type-state 'next-action) :to-equal :next)
    (expect (org-gtd-type-state 'calendar) :to-be nil)))

(describe "org-gtd-type-property"

  (it "resolves :who semantic to DELEGATED_TO for delegated type"
    (expect (org-gtd-type-property 'delegated :who)
            :to-equal "DELEGATED_TO"))

  (it "resolves :when semantic to ORG_GTD_TIMESTAMP for delegated type"
    (expect (org-gtd-type-property 'delegated :when)
            :to-equal "ORG_GTD_TIMESTAMP"))

  (it "resolves :when semantic to ORG_GTD_TIMESTAMP for calendar type"
    (expect (org-gtd-type-property 'calendar :when)
            :to-equal "ORG_GTD_TIMESTAMP"))

  (it "resolves :when semantic to SCHEDULED for habit type"
    (expect (org-gtd-type-property 'habit :when)
            :to-equal "SCHEDULED"))

  (it "returns nil for type without the semantic property"
    (expect (org-gtd-type-property 'next-action :when) :to-be nil)
    (expect (org-gtd-type-property 'next-action :who) :to-be nil)))

(describe "org-gtd-type-from-org-gtd-value"

  (it "returns type name for ORG_GTD value"
    (expect (org-gtd-type-from-org-gtd-value "Delegated") :to-equal 'delegated)
    (expect (org-gtd-type-from-org-gtd-value "Actions") :to-equal 'next-action)
    (expect (org-gtd-type-from-org-gtd-value "Calendar") :to-equal 'calendar)
    (expect (org-gtd-type-from-org-gtd-value "Projects") :to-equal 'project))

  (it "returns nil for unknown ORG_GTD value"
    (expect (org-gtd-type-from-org-gtd-value "Unknown") :to-be nil)))

(describe "org-gtd-type-properties"

  (it "returns list of semantic properties for type with properties"
    (let ((props (org-gtd-type-properties 'delegated)))
      (expect props :to-be-truthy)
      (expect (length props) :to-equal 2)
      ;; Check :who property
      (let ((who-prop (seq-find (lambda (p) (eq (car p) :who)) props)))
        (expect who-prop :to-be-truthy)
        (expect (plist-get (cdr who-prop) :org-property) :to-equal "DELEGATED_TO")
        (expect (plist-get (cdr who-prop) :type) :to-equal 'text)
        (expect (plist-get (cdr who-prop) :required) :to-be-truthy))))

  (it "returns nil for type without properties"
    (expect (org-gtd-type-properties 'next-action) :to-be nil)))

(describe "org-gtd-user-types"

  (describe "variable existence"
    (it "org-gtd-user-types variable exists"
      (expect (boundp 'org-gtd-user-types) :to-be-truthy))

    (it "defaults to empty list"
      (let ((org-gtd-user-types '()))
        (expect org-gtd-user-types :to-equal '()))))

  (describe "merging with built-in types"

    (it "returns builtin type unchanged when no user override"
      (let ((org-gtd-user-types '()))
        (let ((result (org-gtd-type-get 'delegated)))
          (expect (car result) :to-equal 'delegated)
          (expect (plist-get (cdr result) :org-gtd) :to-equal "Delegated"))))

    (it "merges user property override with builtin"
      (let ((org-gtd-user-types
             '((delegated
                :properties
                ((:who :org-property "DELEGATED_TO" :type text :required t
                       :prompt "Custom prompt"))))))
        (let* ((result (org-gtd-type-get 'delegated))
               (props (plist-get (cdr result) :properties))
               (who-prop (seq-find (lambda (p) (eq (car p) :who)) props)))
          ;; User prompt should override
          (expect (plist-get (cdr who-prop) :prompt) :to-equal "Custom prompt")
          ;; :when property from builtin should still exist
          (expect (seq-find (lambda (p) (eq (car p) :when)) props) :to-be-truthy))))

    (it "user can add new properties to existing type"
      (let ((org-gtd-user-types
             '((next-action
                :properties
                ((:custom :org-property "MY_CUSTOM" :type text :required nil
                          :prompt "Custom value"))))))
        (let* ((result (org-gtd-type-get 'next-action))
               (props (plist-get (cdr result) :properties)))
          (expect (seq-find (lambda (p) (eq (car p) :custom)) props) :to-be-truthy))))

    (it "never overrides :org-gtd from user config"
      (let ((org-gtd-user-types
             '((delegated
                :org-gtd "HackedValue"))))
        (let ((result (org-gtd-type-get 'delegated)))
          ;; Should still be the builtin value
          (expect (plist-get (cdr result) :org-gtd) :to-equal "Delegated"))))

    (it "allows overriding :state"
      (let ((org-gtd-user-types
             '((delegated
                :state :next))))
        (let ((result (org-gtd-type-get 'delegated)))
          (expect (plist-get (cdr result) :state) :to-equal :next))))

    (it "ignores unknown type names in user-types"
      (let ((org-gtd-user-types
             '((nonexistent-type
                :properties ((:foo :org-property "FOO" :type text))))))
        ;; Should not error, just return nil for unknown type
        (expect (org-gtd-type-get 'nonexistent-type) :to-be nil)
        ;; Builtin types should still work
        (expect (org-gtd-type-get 'delegated) :to-be-truthy)))))

;;; types-test.el ends here
