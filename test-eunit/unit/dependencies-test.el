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

(require 'e-unit)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)
(require 'org-gtd-value-objects)
(require 'org-gtd-core)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Test Setup

(defun dependencies-eunit--setup ()
  "Set up minimal test environment for dependency tests."
  (setq org-gtd-directory (make-temp-file "org-gtd-dep-test" t)
        org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL"))))

(defun dependencies-eunit--teardown ()
  "Clean up after dependency tests."
  (when (and (boundp 'org-gtd-directory)
             org-gtd-directory
             (file-exists-p org-gtd-directory))
    (delete-directory org-gtd-directory t)))

;;; org-gtd-dependencies-has-path-p

(deftest dependencies/has-path-nil-when-no-connection ()
  "Returns nil when tasks have no connection."
  (dependencies-eunit--setup)
  (unwind-protect
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
        (assert-nil (org-gtd-dependencies-has-path-p "task-a" "task-b")))
    (dependencies-eunit--teardown)))

(deftest dependencies/has-path-t-when-direct-dependency ()
  "Returns t when direct dependency exists."
  (dependencies-eunit--setup)
  (unwind-protect
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
        (assert-true (org-gtd-dependencies-has-path-p "task-a" "task-b")))
    (dependencies-eunit--teardown)))

(deftest dependencies/has-path-t-when-transitive-dependency ()
  "Returns t when transitive dependency exists (A->B->C)."
  (dependencies-eunit--setup)
  (unwind-protect
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
        (assert-true (org-gtd-dependencies-has-path-p "task-a" "task-c")))
    (dependencies-eunit--teardown)))

(deftest dependencies/has-path-nil-for-same-task ()
  "Returns nil for same task."
  (dependencies-eunit--setup)
  (unwind-protect
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Task A\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: task-a\n")
        (insert ":END:\n")
        (goto-char (point-min))
        (assert-nil (org-gtd-dependencies-has-path-p "task-a" "task-a")))
    (dependencies-eunit--teardown)))

;;; org-gtd-dependencies-validate-acyclic

(deftest dependencies/validate-acyclic-accepts-acyclic ()
  "Accepts acyclic dependency."
  (dependencies-eunit--setup)
  (unwind-protect
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
        (assert-true t))
    (dependencies-eunit--teardown)))

(deftest dependencies/validate-acyclic-rejects-direct-cycle ()
  "Rejects direct cycle (A->B, B->A)."
  (dependencies-eunit--setup)
  (unwind-protect
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
          (org-gtd-dependencies-validate-acyclic "task-b" "task-a")))
    (dependencies-eunit--teardown)))

(deftest dependencies/validate-acyclic-rejects-transitive-cycle ()
  "Rejects transitive cycle (A->B->C, C->A)."
  (dependencies-eunit--setup)
  (unwind-protect
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
          (org-gtd-dependencies-validate-acyclic "task-c" "task-a")))
    (dependencies-eunit--teardown)))

;;; org-gtd-dependencies-create

(deftest dependencies/create-bidirectional-dependency ()
  "Creates bidirectional dependency."
  (dependencies-eunit--setup)
  (unwind-protect
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
        (assert-equal '("task-a") (org-gtd-get-task-dependencies "task-b")))
    (dependencies-eunit--teardown)))

(deftest dependencies/create-prevents-cyclic ()
  "Prevents creating cyclic dependencies."
  (dependencies-eunit--setup)
  (unwind-protect
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
          (org-gtd-dependencies-create "task-b" "task-a")))
    (dependencies-eunit--teardown)))

(provide 'dependencies-test)

;;; dependencies-test.el ends here
