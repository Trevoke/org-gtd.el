;;; skip-predicates-test.el --- E-unit tests for skip predicates -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2025 Aldric Giacomoni

;;; Commentary:
;;
;; E-unit tests for org-gtd-skip predicates module.
;; These predicates are used by the view language to build skip functions.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-skip)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;;; org-gtd--compare-values tests

(deftest skip-pred/compare-values-less-than-true ()
  "Compare-values returns t when actual < reference."
  (assert-true (org-gtd--compare-values '< 5 10)))

(deftest skip-pred/compare-values-less-than-false ()
  "Compare-values returns nil when actual >= reference."
  (assert-nil (org-gtd--compare-values '< 10 5))
  (assert-nil (org-gtd--compare-values '< 10 10)))

(deftest skip-pred/compare-values-greater-than-true ()
  "Compare-values returns t when actual > reference."
  (assert-true (org-gtd--compare-values '> 10 5)))

(deftest skip-pred/compare-values-greater-than-false ()
  "Compare-values returns nil when actual <= reference."
  (assert-nil (org-gtd--compare-values '> 5 10))
  (assert-nil (org-gtd--compare-values '> 10 10)))

(deftest skip-pred/compare-values-less-than-or-equal-true ()
  "Compare-values returns t when actual <= reference."
  (assert-true (org-gtd--compare-values '<= 5 10))
  (assert-true (org-gtd--compare-values '<= 10 10)))

(deftest skip-pred/compare-values-less-than-or-equal-false ()
  "Compare-values returns nil when actual > reference."
  (assert-nil (org-gtd--compare-values '<= 10 5)))

(deftest skip-pred/compare-values-greater-than-or-equal-true ()
  "Compare-values returns t when actual >= reference."
  (assert-true (org-gtd--compare-values '>= 10 5))
  (assert-true (org-gtd--compare-values '>= 10 10)))

(deftest skip-pred/compare-values-greater-than-or-equal-false ()
  "Compare-values returns nil when actual < reference."
  (assert-nil (org-gtd--compare-values '>= 5 10)))

(deftest skip-pred/compare-values-invalid-operator ()
  "Compare-values returns nil for invalid operator."
  (assert-nil (org-gtd--compare-values 'invalid 5 10)))

;;;; org-gtd--parse-timestamp tests

(deftest skip-pred/parse-timestamp-valid-active ()
  "Parses valid active org timestamp."
  (let ((result (org-gtd--parse-timestamp "<2025-01-15 Wed>")))
    (assert-true result)
    (assert-true (listp result))))

(deftest skip-pred/parse-timestamp-valid-inactive ()
  "Parses valid inactive org timestamp."
  (let ((result (org-gtd--parse-timestamp "[2025-01-15 Wed]")))
    (assert-true result)
    (assert-true (listp result))))

(deftest skip-pred/parse-timestamp-nil-input ()
  "Returns nil for nil input."
  (assert-nil (org-gtd--parse-timestamp nil)))

(deftest skip-pred/parse-timestamp-empty-string ()
  "Returns nil for empty string."
  (assert-nil (org-gtd--parse-timestamp "")))

(deftest skip-pred/parse-timestamp-invalid-string ()
  "Returns nil for invalid timestamp string."
  (assert-nil (org-gtd--parse-timestamp "not a timestamp")))

(deftest skip-pred/parse-timestamp-whitespace-only ()
  "Returns nil for whitespace-only string."
  (assert-nil (org-gtd--parse-timestamp "   ")))

;;;; Property Predicate tests

(deftest skip-pred/property-equals-returns-true-when-match ()
  "Property equals returns t when property matches value."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-equals "ORG_GTD" "Actions")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-equals-returns-nil-when-no-match ()
  "Property equals returns nil when property doesn't match."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Projects\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-equals "ORG_GTD" "Actions")))
      (assert-nil (funcall pred)))))

(deftest skip-pred/property-equals-returns-nil-when-missing ()
  "Property equals returns nil when property is missing."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-equals "ORG_GTD" "Actions")))
      (assert-nil (funcall pred)))))

(deftest skip-pred/property-empty-or-missing-true-when-missing ()
  "Property empty-or-missing returns t when property is missing."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-empty-or-missing "DELEGATED_TO")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-empty-or-missing-true-when-empty ()
  "Property empty-or-missing returns t when property is empty."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:DELEGATED_TO:\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-empty-or-missing "DELEGATED_TO")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-empty-or-missing-nil-when-has-value ()
  "Property empty-or-missing returns nil when property has value."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:DELEGATED_TO: Alice\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-empty-or-missing "DELEGATED_TO")))
      (assert-nil (funcall pred)))))

(deftest skip-pred/property-invalid-timestamp-true-when-missing ()
  "Property invalid-timestamp returns t when property is missing."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-invalid-timestamp "ORG_GTD_TIMESTAMP")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-invalid-timestamp-true-when-not-timestamp ()
  "Property invalid-timestamp returns t when property is not a timestamp."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: not a timestamp\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-invalid-timestamp "ORG_GTD_TIMESTAMP")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-invalid-timestamp-nil-when-valid ()
  "Property invalid-timestamp returns nil when property is valid timestamp."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: <2025-01-15 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-invalid-timestamp "ORG_GTD_TIMESTAMP")))
      (assert-nil (funcall pred)))))

;;;; Project Predicate tests

(deftest skip-pred/project-has-active-tasks-nil-for-non-project ()
  "Project has-active-tasks returns nil for non-project headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--project-has-active-tasks)))
      (assert-nil (funcall pred)))))

(deftest skip-pred/project-is-stuck-nil-for-non-project ()
  "Project is-stuck returns nil for non-project headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--project-is-stuck)))
      (assert-nil (funcall pred)))))

;;;; Timestamp Comparison Predicate tests

(deftest skip-pred/property-ts-less-than-true-for-past ()
  "Property ts< returns t when timestamp is in the past."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: <2020-01-01 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts< "ORG_GTD_TIMESTAMP" "today")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-ts-less-than-nil-for-future ()
  "Property ts< returns nil when timestamp is in the future."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: <2099-01-01 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts< "ORG_GTD_TIMESTAMP" "today")))
      (assert-nil (funcall pred)))))

(deftest skip-pred/property-ts-greater-than-true-for-future ()
  "Property ts> returns t when timestamp is in the future."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: <2099-01-01 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts> "ORG_GTD_TIMESTAMP" "today")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-ts-greater-than-nil-for-past ()
  "Property ts> returns nil when timestamp is in the past."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: <2020-01-01 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts> "ORG_GTD_TIMESTAMP" "today")))
      (assert-nil (funcall pred)))))

(deftest skip-pred/property-ts-equal-true-for-today ()
  "Property ts= returns t when timestamp is today."
  (with-temp-buffer
    (org-mode)
    (let ((today-ts (format-time-string "<%Y-%m-%d %a>")))
      (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: %s\n:END:\n" today-ts)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts= "ORG_GTD_TIMESTAMP" "today")))
      (assert-true (funcall pred)))))

(deftest skip-pred/property-ts-equal-nil-for-different-day ()
  "Property ts= returns nil when timestamp is not today."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: <2020-01-01 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts= "ORG_GTD_TIMESTAMP" "today")))
      (assert-nil (funcall pred)))))

(deftest skip-pred/property-ts-less-than-nil-when-missing ()
  "Property ts< returns nil when property is missing."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((pred (org-gtd-pred--property-ts< "ORG_GTD_TIMESTAMP" "today")))
      (assert-nil (funcall pred)))))

;;;; org-gtd--parse-reference-date tests

(deftest skip-pred/parse-reference-date-today ()
  "Parses 'today' to current date."
  (let ((result (org-gtd--parse-reference-date "today")))
    (assert-true result)
    (assert-true (listp result))))

(deftest skip-pred/parse-reference-date-days ()
  "Parses '+7d' relative date."
  (let ((result (org-gtd--parse-reference-date "+7d")))
    (assert-true result)
    (assert-true (listp result))))

(deftest skip-pred/parse-reference-date-weeks ()
  "Parses '+1w' relative date."
  (let ((result (org-gtd--parse-reference-date "+1w")))
    (assert-true result)
    (assert-true (listp result))))

(deftest skip-pred/parse-reference-date-absolute ()
  "Parses absolute date string."
  (let ((result (org-gtd--parse-reference-date "2025-01-15")))
    (assert-true result)
    (assert-true (listp result))))

;;;; Skip Function Composition tests

(deftest skip-pred/compose-returns-nil-when-all-pass ()
  "Compose returns nil (include) when all predicates pass."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((pred1 (org-gtd-pred--property-equals "ORG_GTD" "Actions"))
           (skip-fn (org-gtd-skip--compose (list pred1))))
      (assert-nil (funcall skip-fn)))))

(deftest skip-pred/compose-returns-end-when-any-fails ()
  "Compose returns subtree end (skip) when any predicate fails."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Projects\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((pred1 (org-gtd-pred--property-equals "ORG_GTD" "Actions"))
           (skip-fn (org-gtd-skip--compose (list pred1))))
      ;; Should return a position (end of subtree) to skip
      (assert-true (numberp (funcall skip-fn))))))

(deftest skip-pred/compose-handles-multiple-predicates-all-pass ()
  "Compose includes when multiple predicates all pass."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:DELEGATED_TO: Alice\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((pred1 (org-gtd-pred--property-equals "ORG_GTD" "Actions"))
           (pred2 (org-gtd-pred--property-equals "DELEGATED_TO" "Alice"))
           (skip-fn (org-gtd-skip--compose (list pred1 pred2))))
      (assert-nil (funcall skip-fn)))))

(deftest skip-pred/compose-handles-multiple-predicates-one-fails ()
  "Compose skips when one of multiple predicates fails."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:DELEGATED_TO: Bob\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((pred1 (org-gtd-pred--property-equals "ORG_GTD" "Actions"))
           (pred2 (org-gtd-pred--property-equals "DELEGATED_TO" "Alice"))
           (skip-fn (org-gtd-skip--compose (list pred1 pred2))))
      (assert-true (numberp (funcall skip-fn))))))

(deftest skip-pred/compose-handles-empty-predicate-list ()
  "Compose includes (returns nil) when given empty predicate list."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let ((skip-fn (org-gtd-skip--compose '())))
      (assert-nil (funcall skip-fn)))))

(provide 'skip-predicates-test)

;;; skip-predicates-test.el ends here
