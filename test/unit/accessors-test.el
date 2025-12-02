;;; accessors-test.el --- E-unit tests for accessor layer -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Aldric Giacomoni

;;; Commentary:
;;
;; E-unit tests for org-gtd accessor layer that wraps org-mode operations.
;; Migrated from buttercup test/accessors-test.el
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-accessors)
(require 'org-gtd-value-objects)
(require 'org-gtd-core)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;;; Test Setup

(defun accessors-eunit--setup ()
  "Set up minimal test environment for accessor tests."
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL"))))

;;;; Property Readers - org-gtd-get-task-dependencies

(deftest accessors/get-dependencies-returns-nil-when-none ()
  "Returns nil when task has no dependencies."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-get-task-dependencies "task-1"))))

(deftest accessors/get-dependencies-returns-single ()
  "Returns single dependency."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":ORG_GTD_DEPENDS_ON: blocker-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-equal '("blocker-1") (org-gtd-get-task-dependencies "task-1"))))

(deftest accessors/get-dependencies-returns-multiple ()
  "Returns multiple dependencies."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":ORG_GTD_DEPENDS_ON: blocker-1 blocker-2 blocker-3\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-equal '("blocker-1" "blocker-2" "blocker-3")
                  (org-gtd-get-task-dependencies "task-1"))))

(deftest accessors/get-dependencies-returns-nil-for-nonexistent ()
  "Returns nil for non-existent task."
  (accessors-eunit--setup)
  (assert-nil (org-gtd-get-task-dependencies "non-existent")))

;;;; Property Readers - org-gtd-get-task-blockers

(deftest accessors/get-blockers-returns-nil-when-none ()
  "Returns nil when task blocks nothing."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-get-task-blockers "task-1"))))

(deftest accessors/get-blockers-returns-list ()
  "Returns list of tasks blocked by this task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":ORG_GTD_BLOCKS: task-2 task-3\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-equal '("task-2" "task-3") (org-gtd-get-task-blockers "task-1"))))

;;;; Property Readers - org-gtd-get-task-state

(deftest accessors/get-state-returns-todo ()
  "Returns TODO state."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-equal "TODO" (org-gtd-get-task-state "task-1"))))

(deftest accessors/get-state-returns-next ()
  "Returns NEXT state."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-equal "NEXT" (org-gtd-get-task-state "task-1"))))

(deftest accessors/get-state-returns-done ()
  "Returns DONE state."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-equal "DONE" (org-gtd-get-task-state "task-1"))))

(deftest accessors/get-state-returns-nil-for-nonexistent ()
  "Returns nil for non-existent task."
  (accessors-eunit--setup)
  (assert-nil (org-gtd-get-task-state "non-existent")))

;;;; Property Readers - org-gtd-get-task-category

(deftest accessors/get-category-returns-actions ()
  "Returns Actions category."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":ORG_GTD: Actions\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-equal "Actions" (org-gtd-get-task-category "task-1"))))

(deftest accessors/get-category-returns-projects ()
  "Returns Projects category."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* Project\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: project-1\n")
    (insert ":ORG_GTD: Projects\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-equal "Projects" (org-gtd-get-task-category "project-1"))))

;;;; Property Writers - org-gtd-set-task-state

(deftest accessors/set-state-todo-to-next ()
  "Changes task state from TODO to NEXT."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (org-gtd-set-task-state "task-1" "NEXT")
    (assert-equal "NEXT" (org-gtd-get-task-state "task-1"))))

(deftest accessors/set-state-next-to-done ()
  "Changes task state from NEXT to DONE."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (org-gtd-set-task-state "task-1" "DONE")
    (assert-equal "DONE" (org-gtd-get-task-state "task-1"))))

(deftest accessors/set-state-returns-nil-for-nonexistent ()
  "Returns nil for non-existent task."
  (accessors-eunit--setup)
  (assert-nil (org-gtd-set-task-state "non-existent" "DONE")))

;;;; Property Writers - org-gtd-add-to-multivalued-property

(deftest accessors/add-multivalued-first-value ()
  "Adds first value to empty property."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (org-gtd-add-to-multivalued-property "task-1" "ORG_GTD_DEPENDS_ON" "blocker-1")
    (assert-equal '("blocker-1") (org-gtd-get-task-dependencies "task-1"))))

(deftest accessors/add-multivalued-to-existing ()
  "Adds value to existing property."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":ORG_GTD_DEPENDS_ON: blocker-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (org-gtd-add-to-multivalued-property "task-1" "ORG_GTD_DEPENDS_ON" "blocker-2")
    (assert-equal '("blocker-1" "blocker-2") (org-gtd-get-task-dependencies "task-1"))))

(deftest accessors/add-multivalued-throws-for-nonexistent ()
  "Throws error for non-existent task."
  (accessors-eunit--setup)
  (assert-raises 'user-error
   (org-gtd-add-to-multivalued-property "non-existent" "ORG_GTD_DEPENDS_ON" "blocker-1")))

;;;; Property Writers - org-gtd-remove-from-multivalued-property

(deftest accessors/remove-multivalued-value ()
  "Removes value from property."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":ORG_GTD_DEPENDS_ON: blocker-1 blocker-2\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (org-gtd-remove-from-multivalued-property "task-1" "ORG_GTD_DEPENDS_ON" "blocker-1")
    (assert-equal '("blocker-2") (org-gtd-get-task-dependencies "task-1"))))

(deftest accessors/remove-multivalued-throws-for-nonexistent ()
  "Throws error for non-existent task."
  (accessors-eunit--setup)
  (assert-raises 'user-error
   (org-gtd-remove-from-multivalued-property "non-existent" "ORG_GTD_DEPENDS_ON" "blocker-1")))

;;;; Task Lookup - org-gtd-find-task-in-current-buffer

(deftest accessors/find-task-in-buffer ()
  "Finds task in current buffer."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task 1\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (insert "* TODO Task 2\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-2\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (let ((marker (org-gtd-find-task-in-current-buffer "task-2")))
      (assert-true marker)
      (assert-equal (current-buffer) (marker-buffer marker)))))

(deftest accessors/find-task-returns-nil-when-not-found ()
  "Returns nil for task not in current buffer."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-find-task-in-current-buffer "non-existent"))))

;;;; Task State Predicates - org-gtd-task-is-done-p

(deftest accessors/is-done-true-for-done ()
  "Returns truthy for DONE task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-true (org-gtd-task-is-done-p "task-1"))))

(deftest accessors/is-done-true-for-cncl ()
  "Returns truthy for CNCL task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* CNCL Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-true (org-gtd-task-is-done-p "task-1"))))

(deftest accessors/is-done-nil-for-todo ()
  "Returns nil for TODO task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-task-is-done-p "task-1"))))

(deftest accessors/is-done-nil-for-next ()
  "Returns nil for NEXT task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-task-is-done-p "task-1"))))

(deftest accessors/is-done-nil-for-nonexistent ()
  "Returns nil for non-existent task."
  (accessors-eunit--setup)
  (assert-nil (org-gtd-task-is-done-p "non-existent")))

;;;; Task State Predicates - org-gtd-task-is-active-p

(deftest accessors/is-active-true-for-todo ()
  "Returns truthy for TODO task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-true (org-gtd-task-is-active-p "task-1"))))

(deftest accessors/is-active-true-for-next ()
  "Returns truthy for NEXT task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-true (org-gtd-task-is-active-p "task-1"))))

(deftest accessors/is-active-nil-for-done ()
  "Returns nil for DONE task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* DONE Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-task-is-active-p "task-1"))))

(deftest accessors/is-active-nil-for-wait ()
  "Returns nil for WAIT task."
  (accessors-eunit--setup)
  (with-temp-buffer
    (org-mode)
    (insert "* WAIT Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-1\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-task-is-active-p "task-1"))))

(provide 'accessors-test)

;;; accessors-test.el ends here
