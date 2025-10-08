;;; accessors-test.el --- Unit tests for accessor layer -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd accessor layer that wraps org-mode operations.
;;
;; These tests focus on the accessor layer's ability to read and write
;; properties, find tasks, and provide clean interfaces to org-mode state.
;;
;; Test Coverage:
;; - Property readers (get dependencies, blockers, projects, state, category)
;; - Property writers (set state, add/remove multivalued properties)
;; - Task lookup (find task markers)
;; - Task state predicates (is-done-p, is-active-p)
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd-accessors)
(require 'org-gtd-value-objects)
(require 'org-gtd-core)

;;;; Test Setup

(defun accessors-test--setup ()
  "Set up minimal test environment for accessor tests."
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL"))))

;;;; Property Readers

(describe "org-gtd-get-task-dependencies"

  (before-each (accessors-test--setup))

  (it "returns nil when task has no dependencies"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-dependencies "task-1") :to-be nil)))

  (it "returns single dependency"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":ORG_GTD_DEPENDS_ON: blocker-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-dependencies "task-1") :to-equal '("blocker-1"))))

  (it "returns multiple dependencies"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":ORG_GTD_DEPENDS_ON: blocker-1 blocker-2 blocker-3\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-dependencies "task-1")
              :to-equal '("blocker-1" "blocker-2" "blocker-3"))))

  (it "returns nil for non-existent task"
    (expect (org-gtd-get-task-dependencies "non-existent") :to-be nil)))

(describe "org-gtd-get-task-blockers"

  (before-each (accessors-test--setup))

  (it "returns nil when task blocks nothing"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-blockers "task-1") :to-be nil)))

  (it "returns list of tasks blocked by this task"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":ORG_GTD_BLOCKS: task-2 task-3\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-blockers "task-1") :to-equal '("task-2" "task-3")))))

(describe "org-gtd-get-task-state"

  (before-each (accessors-test--setup))

  (it "returns TODO state"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-state "task-1") :to-equal "TODO")))

  (it "returns NEXT state"
    (with-temp-buffer
      (org-mode)
      (insert "* NEXT Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-state "task-1") :to-equal "NEXT")))

  (it "returns DONE state"
    (with-temp-buffer
      (org-mode)
      (insert "* DONE Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-state "task-1") :to-equal "DONE")))

  (it "returns nil for non-existent task"
    (expect (org-gtd-get-task-state "non-existent") :to-be nil)))

(describe "org-gtd-get-task-category"

  (before-each (accessors-test--setup))

  (it "returns Actions category"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":ORG_GTD: Actions\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-category "task-1") :to-equal "Actions")))

  (it "returns Projects category"
    (with-temp-buffer
      (org-mode)
      (insert "* Project\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: project-1\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-get-task-category "project-1") :to-equal "Projects"))))

;;;; Property Writers

(describe "org-gtd-set-task-state"

  (before-each (accessors-test--setup))

  (it "changes task state from TODO to NEXT"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-gtd-set-task-state "task-1" "NEXT")
      (expect (org-gtd-get-task-state "task-1") :to-equal "NEXT")))

  (it "changes task state from NEXT to DONE"
    (with-temp-buffer
      (org-mode)
      (insert "* NEXT Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-gtd-set-task-state "task-1" "DONE")
      (expect (org-gtd-get-task-state "task-1") :to-equal "DONE")))

  (it "returns nil for non-existent task"
    (expect (org-gtd-set-task-state "non-existent" "DONE") :to-be nil)))

(describe "org-gtd-add-to-multivalued-property"

  (before-each (accessors-test--setup))

  (it "adds first value to empty property"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-gtd-add-to-multivalued-property "task-1" "ORG_GTD_DEPENDS_ON" "blocker-1")
      (expect (org-gtd-get-task-dependencies "task-1") :to-equal '("blocker-1"))))

  (it "adds value to existing property"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":ORG_GTD_DEPENDS_ON: blocker-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-gtd-add-to-multivalued-property "task-1" "ORG_GTD_DEPENDS_ON" "blocker-2")
      (expect (org-gtd-get-task-dependencies "task-1")
              :to-equal '("blocker-1" "blocker-2"))))

  (it "throws error for non-existent task"
    (expect (org-gtd-add-to-multivalued-property "non-existent" "ORG_GTD_DEPENDS_ON" "blocker-1")
            :to-throw 'user-error)))

(describe "org-gtd-remove-from-multivalued-property"

  (before-each (accessors-test--setup))

  (it "removes value from property"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":ORG_GTD_DEPENDS_ON: blocker-1 blocker-2\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (org-gtd-remove-from-multivalued-property "task-1" "ORG_GTD_DEPENDS_ON" "blocker-1")
      (expect (org-gtd-get-task-dependencies "task-1") :to-equal '("blocker-2"))))

  (it "throws error for non-existent task"
    (expect (org-gtd-remove-from-multivalued-property "non-existent" "ORG_GTD_DEPENDS_ON" "blocker-1")
            :to-throw 'user-error)))

;;;; Task Lookup

(describe "org-gtd-find-task-in-current-buffer"

  (before-each (accessors-test--setup))

  (it "finds task in current buffer"
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
        (expect marker :not :to-be nil)
        (expect (marker-buffer marker) :to-be (current-buffer)))))

  (it "returns nil for task not in current buffer"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-find-task-in-current-buffer "non-existent") :to-be nil))))

;;;; Task State Predicates

(describe "org-gtd-task-is-done-p"

  (before-each (accessors-test--setup))

  (it "returns truthy for DONE task"
    (with-temp-buffer
      (org-mode)
      (insert "* DONE Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-task-is-done-p "task-1") :to-be-truthy)))

  (it "returns truthy for CNCL task"
    (with-temp-buffer
      (org-mode)
      (insert "* CNCL Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-task-is-done-p "task-1") :to-be-truthy)))

  (it "returns nil for TODO task"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-task-is-done-p "task-1") :to-be nil)))

  (it "returns nil for NEXT task"
    (with-temp-buffer
      (org-mode)
      (insert "* NEXT Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-task-is-done-p "task-1") :to-be nil)))

  (it "returns nil for non-existent task"
    (expect (org-gtd-task-is-done-p "non-existent") :to-be nil)))

(describe "org-gtd-task-is-active-p"

  (before-each (accessors-test--setup))

  (it "returns truthy for TODO task"
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-task-is-active-p "task-1") :to-be-truthy)))

  (it "returns truthy for NEXT task"
    (with-temp-buffer
      (org-mode)
      (insert "* NEXT Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-task-is-active-p "task-1") :to-be-truthy)))

  (it "returns nil for DONE task"
    (with-temp-buffer
      (org-mode)
      (insert "* DONE Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-task-is-active-p "task-1") :to-be nil)))

  (it "returns nil for WAIT task"
    (with-temp-buffer
      (org-mode)
      (insert "* WAIT Task\n")
      (insert ":PROPERTIES:\n")
      (insert ":ID: task-1\n")
      (insert ":END:\n")
      (goto-char (point-min))
      (expect (org-gtd-task-is-active-p "task-1") :to-be nil))))

(provide 'accessors-test)

;;; accessors-test.el ends here
