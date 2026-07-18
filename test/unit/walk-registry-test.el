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

(deftest walks-registry-starts-empty ()
  "Phase 0 ships no registered consumers."
  (assert-nil org-gtd-walks))

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

(provide 'walk-registry-test)

;;; walk-registry-test.el ends here
