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

(provide 'walk-model-test)

;;; walk-model-test.el ends here
