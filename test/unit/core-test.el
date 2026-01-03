;;; core-test.el --- E-unit tests for core module -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2025 Aldric Giacomoni

;;; Commentary:
;;
;; E-unit tests for org-gtd-core module.
;; Migrated from buttercup test/core-test.el
;;
;; These are pure unit tests that don't require filesystem setup.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-core)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;;; org-gtd-project-progress-cookie-position customization

(deftest core/progress-cookie-position-defaults-to-end ()
  "org-gtd-project-progress-cookie-position defaults to 'end."
  (assert-equal 'end org-gtd-project-progress-cookie-position))

(deftest core/progress-cookie-position-accepts-valid-values ()
  "org-gtd-project-progress-cookie-position accepts nil, 'start, or 'end."
  (let ((org-gtd-project-progress-cookie-position nil))
    (assert-nil org-gtd-project-progress-cookie-position))
  (let ((org-gtd-project-progress-cookie-position 'start))
    (assert-equal 'start org-gtd-project-progress-cookie-position))
  (let ((org-gtd-project-progress-cookie-position 'end))
    (assert-equal 'end org-gtd-project-progress-cookie-position)))

;;;; org-gtd--extract-keyword-name function

(deftest core/extract-keyword-returns-plain-unchanged ()
  "org-gtd--extract-keyword-name returns plain keyword unchanged."
  (assert-equal "NEXT" (org-gtd--extract-keyword-name "NEXT")))

(deftest core/extract-keyword-strips-single-char-shortcut ()
  "org-gtd--extract-keyword-name strips single-char shortcut."
  (assert-equal "NEXT" (org-gtd--extract-keyword-name "NEXT(n)")))

(deftest core/extract-keyword-strips-shortcut-with-logging ()
  "org-gtd--extract-keyword-name strips shortcut with logging config."
  (assert-equal "NEXT" (org-gtd--extract-keyword-name "NEXT(n/@)"))
  (assert-equal "DONE" (org-gtd--extract-keyword-name "DONE(d/!)")))

(deftest core/extract-keyword-handles-complex-logging ()
  "org-gtd--extract-keyword-name handles complex logging syntax."
  (assert-equal "WAIT" (org-gtd--extract-keyword-name "WAIT(w@/!)")))

;;;; org-gtd-keyword-mapping validation

(deftest core/keyword-mapping-validates-dsl-syntax ()
  "Validates keywords with DSL syntax in org-todo-keywords."
  (let ((org-todo-keywords '((sequence "TODO(t)" "NEXT(n/@)" "WAIT(w@/!)" "|" "DONE(d/!)" "CNCL(c@)")))
        (org-gtd-keyword-mapping nil))
    ;; Should not error - the mapping uses plain keywords while org-todo-keywords uses DSL
    (org-gtd--validate-and-set-keyword-mapping
     'org-gtd-keyword-mapping
     '((todo . "TODO") (next . "NEXT") (wait . "WAIT") (done . "DONE") (canceled . "CNCL")))
    (assert-true t)))  ;; If we got here, no error was thrown

(deftest core/keyword-mapping-rejects-missing-keywords ()
  "Rejects keywords not in org-todo-keywords."
  (let ((org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))
        (org-gtd-keyword-mapping nil))
    ;; Should error - WAIT and CNCL don't exist
    (assert-raises 'user-error
      (org-gtd--validate-and-set-keyword-mapping
       'org-gtd-keyword-mapping
       '((todo . "TODO") (next . "NEXT") (wait . "WAIT") (done . "DONE") (canceled . "CNCL"))))))

;;;; ORG_GTD category constants

(deftest core/org-gtd-delegated-constant ()
  "org-gtd-delegated constant is defined correctly."
  (assert-equal "Delegated" org-gtd-delegated))

(deftest core/org-gtd-quick-constant ()
  "org-gtd-quick constant is defined correctly."
  (assert-equal "Quick" org-gtd-quick))

(deftest core/org-gtd-all-constants-defined ()
  "All existing ORG_GTD constants are defined."
  (assert-equal "Actions" org-gtd-action)
  (assert-equal "Projects" org-gtd-projects)
  (assert-equal "Calendar" org-gtd-calendar)
  (assert-equal "Tickler" org-gtd-tickler)
  (assert-equal "Reference" org-gtd-knowledge)
  (assert-equal "Trash" org-gtd-trash))

(deftest core/someday-list-property-constant-exists ()
  "org-gtd-prop-someday-list constant exists."
  (assert-equal "ORG_GTD_SOMEDAY_LIST" org-gtd-prop-someday-list))

(provide 'core-test)

;;; core-test.el ends here
