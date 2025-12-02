;;; dependencies-test.el --- Unit tests for dependency service -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd dependency service.
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from test/dependencies-test.el (buttercup).
;;
;; Test Coverage:
;; - org-gtd-dependencies-has-path-p (DFS for cycle detection)
;; - org-gtd-dependencies-validate-acyclic (cycle validation)
;; - org-gtd-dependencies-create (bidirectional dependency creation)
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; org-gtd-dependencies-has-path-p

(deftest dependencies/has-path-nil-when-no-connection ()
  "Returns nil when tasks have no connection."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":END:\n")
    (insert "* TODO Task B\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-b\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-dependencies-has-path-p "task-a" "task-b"))))

(deftest dependencies/has-path-t-when-direct-dependency ()
  "Returns t when direct dependency exists."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":ORG_GTD_BLOCKS: task-b\n")
    (insert ":END:\n")
    (insert "* TODO Task B\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-b\n")
    (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-true (org-gtd-dependencies-has-path-p "task-a" "task-b"))))

(deftest dependencies/has-path-t-when-transitive-dependency ()
  "Returns t when transitive dependency exists (A->B->C)."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":ORG_GTD_BLOCKS: task-b\n")
    (insert ":END:\n")
    (insert "* TODO Task B\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-b\n")
    (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
    (insert ":ORG_GTD_BLOCKS: task-c\n")
    (insert ":END:\n")
    (insert "* TODO Task C\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-c\n")
    (insert ":ORG_GTD_DEPENDS_ON: task-b\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-true (org-gtd-dependencies-has-path-p "task-a" "task-c"))))

(deftest dependencies/has-path-nil-for-same-task ()
  "Returns nil for same task."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-nil (org-gtd-dependencies-has-path-p "task-a" "task-a"))))

;;; org-gtd-dependencies-validate-acyclic

(deftest dependencies/validate-acyclic-accepts-acyclic ()
  "Accepts acyclic dependency."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":END:\n")
    (insert "* TODO Task B\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-b\n")
    (insert ":END:\n")
    (goto-char (point-min))
    ;; Should not throw - just call and expect it to succeed
    (org-gtd-dependencies-validate-acyclic "task-a" "task-b")
    (assert-true t)))

(deftest dependencies/validate-acyclic-rejects-direct-cycle ()
  "Rejects direct cycle (A->B, B->A)."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":ORG_GTD_BLOCKS: task-b\n")
    (insert ":END:\n")
    (insert "* TODO Task B\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-b\n")
    (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-raises 'user-error
      (org-gtd-dependencies-validate-acyclic "task-b" "task-a"))))

(deftest dependencies/validate-acyclic-rejects-transitive-cycle ()
  "Rejects transitive cycle (A->B->C, C->A)."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":ORG_GTD_BLOCKS: task-b\n")
    (insert ":END:\n")
    (insert "* TODO Task B\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-b\n")
    (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
    (insert ":ORG_GTD_BLOCKS: task-c\n")
    (insert ":END:\n")
    (insert "* TODO Task C\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-c\n")
    (insert ":ORG_GTD_DEPENDS_ON: task-b\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-raises 'user-error
      (org-gtd-dependencies-validate-acyclic "task-c" "task-a"))))

;;; org-gtd-dependencies-create

(deftest dependencies/create-bidirectional-dependency ()
  "Creates bidirectional dependency."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":END:\n")
    (insert "* TODO Task B\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-b\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (org-gtd-dependencies-create "task-a" "task-b")
    (assert-equal '("task-b") (org-gtd-get-task-blockers "task-a"))
    (assert-equal '("task-a") (org-gtd-get-task-dependencies "task-b"))))

(deftest dependencies/create-prevents-cyclic ()
  "Prevents creating cyclic dependencies."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task A\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-a\n")
    (insert ":ORG_GTD_BLOCKS: task-b\n")
    (insert ":END:\n")
    (insert "* TODO Task B\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: task-b\n")
    (insert ":ORG_GTD_DEPENDS_ON: task-a\n")
    (insert ":END:\n")
    (goto-char (point-min))
    (assert-raises 'user-error
      (org-gtd-dependencies-create "task-b" "task-a"))))

(provide 'dependencies-test)

;;; dependencies-test.el ends here
