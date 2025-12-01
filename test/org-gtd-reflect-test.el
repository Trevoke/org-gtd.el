;;; org-gtd-reflect-test.el --- Tests for GTD reflect -*- lexical-binding: t; -*-

;; All tests migrated to test-eunit/unit/reflect-test.el:
;;
;; GTD View Specifications (5 tests):
;; - reflect/missed-engagements-view-specs-defined
;; - reflect/translate-delegated-spec-to-org-ql
;; - reflect/translate-calendar-spec-to-org-ql
;; - reflect/translate-project-deadline-spec-to-org-ql
;; - reflect/translate-project-scheduled-spec-to-org-ql
;;
;; Agenda Block Generation (2 tests):
;; - reflect/creates-org-ql-agenda-blocks
;; - reflect/creates-custom-commands-structure
;;
;; Function Availability (2 tests):
;; - reflect/main-function-exists
;; - reflect/specialized-functions-exist
;;
;; Backward Compatibility (8 tests):
;; - reflect/oops-alias-exists
;; - reflect/oops-delegated-alias-exists
;; - reflect/oops-calendar-alias-exists
;; - reflect/oops-projects-alias-exists
;; - reflect/oops-with-custom-alias-exists
;; - reflect/oops-custom-views-variable-exists
;; - reflect/oops-view-specs-variable-exists
;; - reflect/review-obsolete-aliases-exist
;;
;; Upcoming Delegated (3 tests):
;; - reflect/upcoming-delegated-view-spec-defined
;; - reflect/translate-upcoming-delegated-spec-to-org-ql
;; - reflect/upcoming-delegated-function-exists
;;
;; This file is kept for documentation. The tests now run via e-unit.

;;; org-gtd-reflect-test.el ends here
