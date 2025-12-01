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

(provide 'accessors-test)

;;; accessors-test.el ends here
