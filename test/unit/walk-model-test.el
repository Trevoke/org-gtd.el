;;; walk-model-test.el --- Unit tests for the pure walk model -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tier 1 tests for the pure walk model (org-gtd-walk-model).
;; These are pure unit tests on plain lists: NO org, NO buffers, NO mock-fs,
;; NO ogt-eunit-with-mock-gtd.  They must not be able to flake.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-walk-model)

(e-unit-initialize)

;;; org-gtd-walk-model-create

(deftest walk-model-create-builds-a-fresh-model ()
  "create seeds entries, cursor 0, and meta."
  (let ((m (org-gtd-walk-model-create '("a" "b" "c") '(:tag foo))))
    (assert-equal '("a" "b" "c") (plist-get m :entries))
    (assert-same 0 (plist-get m :cursor))
    (assert-equal '(:tag foo) (plist-get m :meta))))

(deftest walk-model-create-defaults-meta-to-nil ()
  "meta defaults to nil when omitted."
  (let ((m (org-gtd-walk-model-create '("a"))))
    (assert-nil (plist-get m :meta))))

(deftest walk-model-create-copies-entries ()
  "create does not retain the caller's list object."
  (let* ((src (list "a" "b"))
         (m (org-gtd-walk-model-create src)))
    (setcar src "MUTATED")
    (assert-equal '("a" "b") (plist-get m :entries))))

;;; cursor queries

(deftest walk-model-current-returns-handle-at-cursor ()
  "current returns the entry at the cursor."
  (let ((m (org-gtd-walk-model-create '("a" "b" "c"))))
    (assert-equal "a" (org-gtd-walk-model-current m))))

(deftest walk-model-current-is-nil-when-done ()
  "current returns nil once the cursor is past the last entry."
  (let ((m (list :entries '("a") :cursor 1 :meta nil)))
    (assert-nil (org-gtd-walk-model-current m))))

(deftest walk-model-done-p-false-mid-walk ()
  "done-p is nil while the cursor points at an entry."
  (assert-nil (org-gtd-walk-model-done-p
               (org-gtd-walk-model-create '("a" "b")))))

(deftest walk-model-done-p-true-past-end ()
  "done-p is non-nil when cursor equals the entry count."
  (assert-true (org-gtd-walk-model-done-p (list :entries '("a") :cursor 1))))

(deftest walk-model-done-p-true-for-empty-entries ()
  "an empty walk is done immediately."
  (assert-true (org-gtd-walk-model-done-p (org-gtd-walk-model-create '()))))

(deftest walk-model-remaining-counts-pending-inclusive-of-current ()
  "remaining counts the current item plus everything after it."
  (assert-same 3 (org-gtd-walk-model-remaining
                  (org-gtd-walk-model-create '("a" "b" "c"))))
  (assert-same 1 (org-gtd-walk-model-remaining
                  (list :entries '("a" "b" "c") :cursor 2)))
  (assert-same 0 (org-gtd-walk-model-remaining
                  (list :entries '("a") :cursor 1))))

;;; advance

(deftest walk-model-advance-moves-cursor-forward ()
  "advance returns a model whose cursor is one greater."
  (let* ((m0 (org-gtd-walk-model-create '("a" "b" "c")))
         (m1 (org-gtd-walk-model-advance m0)))
    (assert-same 1 (plist-get m1 :cursor))
    (assert-equal "b" (org-gtd-walk-model-current m1))))

(deftest walk-model-advance-does-not-mutate-input ()
  "advance is pure: the original model is unchanged."
  (let ((m0 (org-gtd-walk-model-create '("a" "b"))))
    (org-gtd-walk-model-advance m0)
    (assert-same 0 (plist-get m0 :cursor))))

(deftest walk-model-advance-past-last-item-is-done ()
  "advancing off the last item makes the walk done, not out of bounds."
  (let* ((m (list :entries '("a") :cursor 0))
         (m1 (org-gtd-walk-model-advance m)))
    (assert-true (org-gtd-walk-model-done-p m1))
    (assert-same 1 (plist-get m1 :cursor))))

(deftest walk-model-advance-when-done-stays-done ()
  "advance never pushes the cursor beyond (length entries)."
  (let* ((m (list :entries '("a") :cursor 1))
         (m1 (org-gtd-walk-model-advance m)))
    (assert-same 1 (plist-get m1 :cursor))))

(provide 'walk-model-test)

;;; walk-model-test.el ends here
