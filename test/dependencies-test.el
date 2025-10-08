;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)


;;; dependencies-test.el --- Unit tests for dependency service -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd dependency service.
;;
;; These tests focus on the dependency bounded context which provides graph
;; algorithms and services for managing task dependencies.
;;
;; Test Coverage:
;; - org-gtd-dependencies-has-path-p (DFS for cycle detection)
;; - org-gtd-dependencies-validate-acyclic (cycle validation)
;; - org-gtd-dependencies-create (bidirectional dependency creation)
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)
(require 'org-gtd-value-objects)
(require 'org-gtd-core)
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))

;;;; Test Setup

(defun dependencies-test--setup ()
  "Set up minimal test environment for dependency tests."
  (setq org-gtd-directory (make-temp-file "org-gtd-dep-test" t)
        org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL"))))

(defun dependencies-test--teardown ()
  "Clean up after dependency tests."
  (when (and org-gtd-directory (file-exists-p org-gtd-directory))
    (delete-directory org-gtd-directory t)))

(defun dependencies-test--create-task (id)
  "Create a minimal task with ID in a temp buffer for testing."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n")
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: %s\n" id))
    (insert ":ORG_GTD: Actions\n")
    (insert ":END:\n")
    (buffer-string)))

;;;; Cycle Detection

(describe "org-gtd-dependencies-has-path-p"

  (before-each (dependencies-test--setup))
  (after-each (dependencies-test--teardown))

  (it "returns nil when tasks have no connection"
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
      (expect (org-gtd-dependencies-has-path-p "task-a" "task-b") :to-be nil)))

  (it "returns t when direct dependency exists"
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
      (expect (org-gtd-dependencies-has-path-p "task-a" "task-b") :to-be-truthy)))

  (it "returns t when transitive dependency exists (A->B->C)"
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
      (expect (org-gtd-dependencies-has-path-p "task-a" "task-c") :to-be-truthy)))

  (it "returns nil for same task"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task A\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-a\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-dependencies-has-path-p "task-a" "task-a") :to-be nil))))

;;;; Cycle Validation

(describe "org-gtd-dependencies-validate-acyclic"

  (before-each (dependencies-test--setup))
  (after-each (dependencies-test--teardown))

  (it "accepts acyclic dependency"
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
      (expect (org-gtd-dependencies-validate-acyclic "task-a" "task-b")
              :not :to-throw)))

  (it "rejects direct cycle (A->B, B->A)"
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
      (expect (org-gtd-dependencies-validate-acyclic "task-b" "task-a")
              :to-throw)))

  (it "rejects transitive cycle (A->B->C, C->A)"
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
      (expect (org-gtd-dependencies-validate-acyclic "task-c" "task-a")
              :to-throw))))

;;;; Dependency Creation

(describe "org-gtd-dependencies-create"

  (before-each (dependencies-test--setup))
  (after-each (dependencies-test--teardown))

  (it "creates bidirectional dependency"
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
      (expect (org-gtd-get-task-blockers "task-a") :to-equal '("task-b"))
      (expect (org-gtd-get-task-dependencies "task-b") :to-equal '("task-a"))))

  (it "prevents creating cyclic dependencies"
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
      (expect (org-gtd-dependencies-create "task-b" "task-a")
              :to-throw))))

(provide 'dependencies-test)

;;; dependencies-test.el ends here
