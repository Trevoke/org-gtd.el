;;; value-objects-test.el --- Unit tests for value objects and domain predicates -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd value objects and domain predicates.
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd-value-objects)
(require 'org-gtd-core)

;; Tests migrated to test-eunit/unit/value-objects-test.el:
;; org-gtd-todo-state-is-active-p (7 tests)
;; org-gtd-todo-state-is-ready-p (6 tests)
;; org-gtd-todo-state-blocks-others-p (6 tests)
;; org-gtd-todo-state-should-reset-p (6 tests)
;; org-gtd-task-deps--create (4 tests)

;; Remaining tests (not yet migrated - use spy-on for mocking):

;;;; Task Dependency Value Object

(describe "org-gtd-task-deps value object"

  ;; org-gtd-task-deps--create tests migrated to e-unit

  (describe "org-gtd-task-deps-is-blocked-p"
    (it "returns nil when task has no dependencies"
      (let ((deps (org-gtd-task-deps--create :depends-on nil)))
        (expect (org-gtd-task-deps-is-blocked-p deps) :to-be nil)))

    (it "returns nil when all dependencies are done"
      (spy-on 'org-gtd-task-is-done-p :and-return-value t)
      (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
        (expect (org-gtd-task-deps-is-blocked-p deps) :to-be nil)))

    (it "returns t when some dependencies are not done"
      (spy-on 'org-gtd-task-is-done-p :and-call-fake
              (lambda (id) (equal id "task-1")))
      (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
        (expect (org-gtd-task-deps-is-blocked-p deps) :to-be t)))

    (it "returns t when no dependencies are done"
      (spy-on 'org-gtd-task-is-done-p :and-return-value nil)
      (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
        (expect (org-gtd-task-deps-is-blocked-p deps) :to-be t))))

  (describe "org-gtd-task-deps-is-ready-p"
    (it "returns t when task has no dependencies"
      (let ((deps (org-gtd-task-deps--create :depends-on nil)))
        (expect (org-gtd-task-deps-is-ready-p deps) :to-be t)))

    (it "returns t when all dependencies are done"
      (spy-on 'org-gtd-task-is-done-p :and-return-value t)
      (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
        (expect (org-gtd-task-deps-is-ready-p deps) :to-be t)))

    (it "returns nil when some dependencies are not done"
      (spy-on 'org-gtd-task-is-done-p :and-call-fake
              (lambda (id) (equal id "task-1")))
      (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
        (expect (org-gtd-task-deps-is-ready-p deps) :to-be nil)))

    (it "is the logical inverse of is-blocked-p"
      (spy-on 'org-gtd-task-is-done-p :and-return-value nil)
      (let ((deps (org-gtd-task-deps--create :depends-on '("task-1"))))
        (expect (org-gtd-task-deps-is-blocked-p deps) :to-be t)
        (expect (org-gtd-task-deps-is-ready-p deps) :to-be nil)))))

(provide 'value-objects-test)

;;; value-objects-test.el ends here
