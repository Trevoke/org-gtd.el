;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'org-gtd-types)

;; Tests migrated to test-eunit/unit/types-test.el:
;; - types-defines-all-expected-gtd-types
;; - types-maps-each-type-to-correct-org-gtd-value
;; - types-maps-to-correct-todo-state-semantics
;; - type-get-returns-definition-for-valid-type
;; - type-get-returns-nil-for-unknown-type
;; - type-org-gtd-value-returns-property-value
;; - type-org-gtd-value-returns-nil-for-unknown
;; - type-state-returns-semantic-for-type
;; - type-from-org-gtd-value-returns-type-name
;; - type-from-org-gtd-value-returns-nil-for-unknown
;; - type-property-resolves-who-for-delegated
;; - type-property-resolves-when-for-delegated
;; - type-property-resolves-when-for-calendar
;; - type-property-resolves-when-for-habit
;; - type-property-returns-nil-for-missing-semantic
;; - type-properties-returns-list-for-type-with-properties
;; - type-properties-returns-nil-for-type-without-properties
;; - type-properties-someday-has-no-properties

;; Remaining tests (not yet migrated - use let-binding for org-gtd-user-types):

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
