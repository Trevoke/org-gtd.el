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

;; org-gtd-user-types tests (8 tests) also migrated to test-eunit/unit/types-test.el

;;; types-test.el ends here
