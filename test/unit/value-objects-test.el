;;; value-objects-test.el --- Unit tests for value objects and domain predicates -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd value objects and domain predicates.
;; These are pure unit tests for business logic - no filesystem needed.
;;
;; Migrated from test/value-objects-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'e-unit-mock)
(require 'org-gtd-value-objects)
(require 'org-gtd-core)

;; Initialize e-unit short syntax and mocking
(e-unit-initialize)
(e-unit-mock-initialize)

;;; Test Setup

(defun value-objects-test--setup ()
  "Set up minimal test environment for value object tests."
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL"))))

;;; org-gtd-todo-state-is-active-p

(deftest todo-state-is-active-identifies-todo ()
  "org-gtd-todo-state-is-active-p identifies TODO as active."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-is-active-p "TODO")))

(deftest todo-state-is-active-identifies-next ()
  "org-gtd-todo-state-is-active-p identifies NEXT as active."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-is-active-p "NEXT")))

(deftest todo-state-is-active-done-not-active ()
  "org-gtd-todo-state-is-active-p identifies DONE as not active."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-is-active-p "DONE")))

(deftest todo-state-is-active-cncl-not-active ()
  "org-gtd-todo-state-is-active-p identifies CNCL as not active."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-is-active-p "CNCL")))

(deftest todo-state-is-active-wait-not-active ()
  "org-gtd-todo-state-is-active-p identifies WAIT as not active."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-is-active-p "WAIT")))

(deftest todo-state-is-active-nil-returns-nil ()
  "org-gtd-todo-state-is-active-p returns nil for nil state."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-is-active-p nil)))

(deftest todo-state-is-active-empty-string-is-active ()
  "org-gtd-todo-state-is-active-p treats empty string as active."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-is-active-p "")))

;;; org-gtd-todo-state-is-ready-p

(deftest todo-state-is-ready-identifies-next ()
  "org-gtd-todo-state-is-ready-p identifies NEXT as ready."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-is-ready-p "NEXT")))

(deftest todo-state-is-ready-identifies-todo ()
  "org-gtd-todo-state-is-ready-p identifies TODO as ready."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-is-ready-p "TODO")))

(deftest todo-state-is-ready-done-not-ready ()
  "org-gtd-todo-state-is-ready-p identifies DONE as not ready."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-is-ready-p "DONE")))

(deftest todo-state-is-ready-cncl-not-ready ()
  "org-gtd-todo-state-is-ready-p identifies CNCL as not ready."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-is-ready-p "CNCL")))

(deftest todo-state-is-ready-wait-not-ready ()
  "org-gtd-todo-state-is-ready-p identifies WAIT as not ready."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-is-ready-p "WAIT")))

(deftest todo-state-is-ready-nil-returns-nil ()
  "org-gtd-todo-state-is-ready-p returns nil for nil state."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-is-ready-p nil)))

;;; org-gtd-todo-state-blocks-others-p

(deftest todo-state-blocks-others-todo-blocks ()
  "TODO blocks dependent tasks from becoming NEXT."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-blocks-others-p "TODO")))

(deftest todo-state-blocks-others-next-blocks ()
  "NEXT blocks dependent tasks from becoming NEXT."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-blocks-others-p "NEXT")))

(deftest todo-state-blocks-others-wait-blocks ()
  "WAIT blocks dependent tasks from becoming NEXT."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-blocks-others-p "WAIT")))

(deftest todo-state-blocks-others-done-does-not-block ()
  "DONE does not block dependent tasks."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-blocks-others-p "DONE")))

(deftest todo-state-blocks-others-cncl-does-not-block ()
  "CNCL does not block dependent tasks."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-blocks-others-p "CNCL")))

(deftest todo-state-blocks-others-nil-returns-nil ()
  "org-gtd-todo-state-blocks-others-p returns nil for nil state."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-blocks-others-p nil)))

;;; org-gtd-todo-state-should-reset-p

(deftest todo-state-should-reset-todo-resets ()
  "TODO should be recalculated during project updates."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-should-reset-p "TODO")))

(deftest todo-state-should-reset-next-resets ()
  "NEXT should be recalculated during project updates."
  (value-objects-test--setup)
  (assert-true (org-gtd-todo-state-should-reset-p "NEXT")))

(deftest todo-state-should-reset-wait-preserves ()
  "WAIT preserves explicit user decision."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-should-reset-p "WAIT")))

(deftest todo-state-should-reset-cncl-preserves ()
  "CNCL preserves explicit user decision."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-should-reset-p "CNCL")))

(deftest todo-state-should-reset-done-preserves ()
  "DONE preserves explicit user decision."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-should-reset-p "DONE")))

(deftest todo-state-should-reset-nil-returns-nil ()
  "org-gtd-todo-state-should-reset-p returns nil for nil state."
  (value-objects-test--setup)
  (assert-nil (org-gtd-todo-state-should-reset-p nil)))

;;;; Task Dependency Value Object - org-gtd-task-deps--create

(deftest task-deps-create-with-depends-on ()
  "org-gtd-task-deps--create creates struct with depends-on list."
  (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
    (assert-equal '("task-1" "task-2") (org-gtd-task-deps-depends-on deps))))

(deftest task-deps-create-with-blocks ()
  "org-gtd-task-deps--create creates struct with blocks list."
  (let ((deps (org-gtd-task-deps--create :blocks '("task-3" "task-4"))))
    (assert-equal '("task-3" "task-4") (org-gtd-task-deps-blocks deps))))

(deftest task-deps-create-with-both-lists ()
  "org-gtd-task-deps--create creates struct with both lists."
  (let ((deps (org-gtd-task-deps--create
               :depends-on '("task-1")
               :blocks '("task-2" "task-3"))))
    (assert-equal '("task-1") (org-gtd-task-deps-depends-on deps))
    (assert-equal '("task-2" "task-3") (org-gtd-task-deps-blocks deps))))

(deftest task-deps-create-with-empty-lists ()
  "org-gtd-task-deps--create creates struct with empty lists."
  (let ((deps (org-gtd-task-deps--create)))
    (assert-nil (org-gtd-task-deps-depends-on deps))
    (assert-nil (org-gtd-task-deps-blocks deps))))

;;;; org-gtd-task-deps-is-blocked-p tests

(deftest task-deps-is-blocked-nil-when-no-dependencies ()
  "org-gtd-task-deps-is-blocked-p returns nil when task has no dependencies."
  (let ((deps (org-gtd-task-deps--create :depends-on nil)))
    (assert-nil (org-gtd-task-deps-is-blocked-p deps))))

(deftest task-deps-is-blocked-nil-when-all-done ()
  "org-gtd-task-deps-is-blocked-p returns nil when all dependencies are done."
  (with-stub org-gtd-task-is-done-p t
    (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
      (assert-nil (org-gtd-task-deps-is-blocked-p deps)))))

(deftest task-deps-is-blocked-t-when-some-not-done ()
  "org-gtd-task-deps-is-blocked-p returns t when some dependencies are not done."
  (with-fake org-gtd-task-is-done-p (lambda (id) (equal id "task-1"))
    (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
      (assert-true (org-gtd-task-deps-is-blocked-p deps)))))

(deftest task-deps-is-blocked-t-when-none-done ()
  "org-gtd-task-deps-is-blocked-p returns t when no dependencies are done."
  (with-stub org-gtd-task-is-done-p nil
    (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
      (assert-true (org-gtd-task-deps-is-blocked-p deps)))))

;;;; org-gtd-task-deps-is-ready-p tests

(deftest task-deps-is-ready-t-when-no-dependencies ()
  "org-gtd-task-deps-is-ready-p returns t when task has no dependencies."
  (let ((deps (org-gtd-task-deps--create :depends-on nil)))
    (assert-true (org-gtd-task-deps-is-ready-p deps))))

(deftest task-deps-is-ready-t-when-all-done ()
  "org-gtd-task-deps-is-ready-p returns t when all dependencies are done."
  (with-stub org-gtd-task-is-done-p t
    (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
      (assert-true (org-gtd-task-deps-is-ready-p deps)))))

(deftest task-deps-is-ready-nil-when-some-not-done ()
  "org-gtd-task-deps-is-ready-p returns nil when some dependencies are not done."
  (with-fake org-gtd-task-is-done-p (lambda (id) (equal id "task-1"))
    (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
      (assert-nil (org-gtd-task-deps-is-ready-p deps)))))

(deftest task-deps-ready-is-inverse-of-blocked ()
  "org-gtd-task-deps-is-ready-p is the logical inverse of is-blocked-p."
  (with-stub org-gtd-task-is-done-p nil
    (let ((deps (org-gtd-task-deps--create :depends-on '("task-1"))))
      (assert-true (org-gtd-task-deps-is-blocked-p deps))
      (assert-nil (org-gtd-task-deps-is-ready-p deps)))))

(provide 'value-objects-test)

;;; value-objects-test.el ends here
