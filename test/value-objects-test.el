;;; value-objects-test.el --- Unit tests for value objects and domain predicates -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd value objects and domain predicates.
;;
;; These tests focus on pure business logic and domain rules about TODO states
;; and task dependencies. They don't require full GTD setup or file operations.
;;
;; Test Coverage:
;; - TODO state predicates (active, ready, blocks others, should reset)
;; - Task dependency value object (blocked, ready)
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd-value-objects)
(require 'org-gtd-core)

;;;; Test Setup

(defun value-objects-test--setup ()
  "Set up minimal test environment for value object tests."
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-done-keywords '("DONE")
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (canceled . "CNCL"))))

;;;; TODO State Predicates

(describe "org-gtd-todo-state-is-active-p"

  (before-each (value-objects-test--setup))

  (describe "active states"
    (it "identifies TODO as active"
      (expect (org-gtd-todo-state-is-active-p "TODO") :to-be t))

    (it "identifies NEXT as active"
      (expect (org-gtd-todo-state-is-active-p "NEXT") :to-be t)))

  (describe "non-active states"
    (it "identifies DONE as not active"
      (expect (org-gtd-todo-state-is-active-p "DONE") :to-be nil))

    (it "identifies CNCL as not active"
      (expect (org-gtd-todo-state-is-active-p "CNCL") :to-be nil))

    (it "identifies WAIT as not active"
      (expect (org-gtd-todo-state-is-active-p "WAIT") :to-be nil)))

  (describe "edge cases"
    (it "returns nil for nil state"
      (expect (org-gtd-todo-state-is-active-p nil) :to-be nil))

    (it "treats empty string as active (not a standard keyword)"
      (expect (org-gtd-todo-state-is-active-p "") :to-be t))))

(describe "org-gtd-todo-state-is-ready-p"

  (before-each (value-objects-test--setup))

  (describe "ready states"
    (it "identifies NEXT as ready to work on"
      (expect (org-gtd-todo-state-is-ready-p "NEXT") :to-be t))

    (it "identifies TODO as ready to work on"
      (expect (org-gtd-todo-state-is-ready-p "TODO") :to-be t)))

  (describe "non-ready states"
    (it "identifies DONE as not ready"
      (expect (org-gtd-todo-state-is-ready-p "DONE") :to-be nil))

    (it "identifies CNCL as not ready"
      (expect (org-gtd-todo-state-is-ready-p "CNCL") :to-be nil))

    (it "identifies WAIT as not ready"
      (expect (org-gtd-todo-state-is-ready-p "WAIT") :to-be nil)))

  (describe "edge cases"
    (it "returns nil for nil state"
      (expect (org-gtd-todo-state-is-ready-p nil) :to-be nil))))

(describe "org-gtd-todo-state-blocks-others-p"

  (before-each (value-objects-test--setup))

  (describe "blocking states"
    (it "TODO blocks dependent tasks from becoming NEXT"
      (expect (org-gtd-todo-state-blocks-others-p "TODO") :to-be t))

    (it "NEXT blocks dependent tasks from becoming NEXT"
      (expect (org-gtd-todo-state-blocks-others-p "NEXT") :to-be t))

    (it "WAIT blocks dependent tasks from becoming NEXT"
      (expect (org-gtd-todo-state-blocks-others-p "WAIT") :to-be t)))

  (describe "non-blocking states"
    (it "DONE does not block dependent tasks"
      (expect (org-gtd-todo-state-blocks-others-p "DONE") :to-be nil))

    (it "CNCL does not block dependent tasks"
      (expect (org-gtd-todo-state-blocks-others-p "CNCL") :to-be nil)))

  (describe "business rule"
    (it "encodes rule that only completed/canceled tasks unblock dependents"
      (expect (org-gtd-todo-state-blocks-others-p "TODO") :to-be t)
      (expect (org-gtd-todo-state-blocks-others-p "NEXT") :to-be t)
      (expect (org-gtd-todo-state-blocks-others-p "WAIT") :to-be t)
      (expect (org-gtd-todo-state-blocks-others-p "DONE") :to-be nil)
      (expect (org-gtd-todo-state-blocks-others-p "CNCL") :to-be nil)))

  (describe "edge cases"
    (it "returns nil for nil state"
      (expect (org-gtd-todo-state-blocks-others-p nil) :to-be nil))))

(describe "org-gtd-todo-state-should-reset-p"

  (before-each (value-objects-test--setup))

  (describe "states that should reset"
    (it "TODO should be recalculated during project updates"
      (expect (org-gtd-todo-state-should-reset-p "TODO") :to-be t))

    (it "NEXT should be recalculated during project updates"
      (expect (org-gtd-todo-state-should-reset-p "NEXT") :to-be t)))

  (describe "states that should NOT reset"
    (it "WAIT preserves explicit user decision"
      (expect (org-gtd-todo-state-should-reset-p "WAIT") :to-be nil))

    (it "CNCL preserves explicit user decision"
      (expect (org-gtd-todo-state-should-reset-p "CNCL") :to-be nil))

    (it "DONE preserves explicit user decision"
      (expect (org-gtd-todo-state-should-reset-p "DONE") :to-be nil)))

  (describe "business rule"
    (it "preserves explicit user decisions about waiting and completion"
      (expect (org-gtd-todo-state-should-reset-p "WAIT") :to-be nil)
      (expect (org-gtd-todo-state-should-reset-p "DONE") :to-be nil)
      (expect (org-gtd-todo-state-should-reset-p "CNCL") :to-be nil))

    (it "recalculates automated states"
      (expect (org-gtd-todo-state-should-reset-p "TODO") :to-be t)
      (expect (org-gtd-todo-state-should-reset-p "NEXT") :to-be t)))

  (describe "edge cases"
    (it "returns nil for nil state"
      (expect (org-gtd-todo-state-should-reset-p nil) :to-be nil))))

;;;; Task Dependency Value Object

(describe "org-gtd-task-deps value object"

  (describe "org-gtd-task-deps--create"
    (it "creates struct with depends-on list"
      (let ((deps (org-gtd-task-deps--create :depends-on '("task-1" "task-2"))))
        (expect (org-gtd-task-deps-depends-on deps) :to-equal '("task-1" "task-2"))))

    (it "creates struct with blocks list"
      (let ((deps (org-gtd-task-deps--create :blocks '("task-3" "task-4"))))
        (expect (org-gtd-task-deps-blocks deps) :to-equal '("task-3" "task-4"))))

    (it "creates struct with both lists"
      (let ((deps (org-gtd-task-deps--create
                   :depends-on '("task-1")
                   :blocks '("task-2" "task-3"))))
        (expect (org-gtd-task-deps-depends-on deps) :to-equal '("task-1"))
        (expect (org-gtd-task-deps-blocks deps) :to-equal '("task-2" "task-3"))))

    (it "creates struct with empty lists"
      (let ((deps (org-gtd-task-deps--create)))
        (expect (org-gtd-task-deps-depends-on deps) :to-be nil)
        (expect (org-gtd-task-deps-blocks deps) :to-be nil))))

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
