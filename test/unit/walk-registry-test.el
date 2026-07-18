;;; walk-registry-test.el --- Unit tests for the walk registry + spec -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for the org-gtd-walks registry and spec validation.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-walk)

(e-unit-initialize)

(deftest walk-register-then-get-round-trips ()
  "register stores a spec that get returns."
  (let ((org-gtd-walks nil)
        (spec '(:name demo :find ignore :render ignore :scope "s")))
    (org-gtd-walk-register 'demo spec)
    (assert-equal spec (org-gtd-walk-get 'demo))))

(deftest walk-get-unknown-returns-nil ()
  (let ((org-gtd-walks nil))
    (assert-nil (org-gtd-walk-get 'nope))))

(deftest walk-register-replaces-existing-name ()
  "registering the same name twice replaces, not duplicates."
  (let ((org-gtd-walks nil))
    (org-gtd-walk-register 'demo '(:name demo :v 1))
    (org-gtd-walk-register 'demo '(:name demo :v 2))
    (assert-same 2 (plist-get (org-gtd-walk-get 'demo) :v))
    (assert-same 1 (length org-gtd-walks))))

;;; spec validation

(defun walk-registry-test--good-spec ()
  "A minimally valid spec."
  (list :name 'demo
        :find #'ignore
        :render #'ignore
        :actions nil
        :on-finish nil
        :resumable nil
        :resolve nil
        :scope "scope-x"))

(deftest walk-spec-valid-p-accepts-a-good-spec ()
  (assert-true (org-gtd-walk-spec-valid-p (walk-registry-test--good-spec))))

(deftest walk-spec-valid-p-requires-a-symbol-name ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :name "demo"))))

(deftest walk-spec-valid-p-requires-callable-find ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :find 42))))

(deftest walk-spec-valid-p-requires-callable-render ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :render "nope"))))

(deftest walk-spec-valid-p-requires-scope ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :scope nil))))

(deftest walk-spec-valid-p-allows-nil-optional-fields ()
  "actions/on-finish/resolve may be nil; a lambda resolve is fine too."
  (let ((spec (plist-put (walk-registry-test--good-spec)
                         :resolve (lambda (_h) t))))
    (assert-true (org-gtd-walk-spec-valid-p spec))))

(deftest walk-spec-valid-p-rejects-non-callable-resolve ()
  (assert-nil (org-gtd-walk-spec-valid-p
               (plist-put (walk-registry-test--good-spec) :resolve 7))))

(provide 'walk-registry-test)

;;; walk-registry-test.el ends here
