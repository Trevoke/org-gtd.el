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
(require 'org-gtd-value-objects)
(require 'org-gtd-core)

;; Initialize e-unit short syntax
(e-unit-initialize)

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

;;; value-objects-test.el ends here
