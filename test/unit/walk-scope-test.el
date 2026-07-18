;;; walk-scope-test.el --- Unit tests for walk scope identity + locking -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for scope canonicalization, the concurrency lock, and checkpoint
;; path derivation (design §5, §8).  These use plain strings and a temp
;; directory only -- no org, no mock-fs.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-walk)

(e-unit-initialize)

;;; scope-key

(deftest scope-key-of-string-is-the-string ()
  (assert-equal "/x/inbox.org" (org-gtd-walk--scope-key "/x/inbox.org")))

(deftest scope-key-of-list-is-order-independent ()
  "A file-set scope keys the same regardless of listing order."
  (assert-equal (org-gtd-walk--scope-key '("a.org" "b.org"))
                (org-gtd-walk--scope-key '("b.org" "a.org"))))

(deftest scope-key-distinguishes-different-scopes ()
  (assert-not-equal (org-gtd-walk--scope-key "a.org")
                    (org-gtd-walk--scope-key "b.org")))

;;; locking

(deftest scope-lock-lifecycle ()
  "lock makes a scope locked; unlock releases it."
  (let ((org-gtd-walk--locked-scopes nil))
    (assert-nil (org-gtd-walk--scope-locked-p "s"))
    (org-gtd-walk--lock-scope "s")
    (assert-true (org-gtd-walk--scope-locked-p "s"))
    (org-gtd-walk--unlock-scope "s")
    (assert-nil (org-gtd-walk--scope-locked-p "s"))))

(deftest scope-lock-is-per-container ()
  "Different scopes lock independently."
  (let ((org-gtd-walk--locked-scopes nil))
    (org-gtd-walk--lock-scope "a")
    (assert-nil (org-gtd-walk--scope-locked-p "b"))))

(deftest scope-lock-matches-order-independent-file-sets ()
  "A file-set locked in one order is seen as locked in another."
  (let ((org-gtd-walk--locked-scopes nil))
    (org-gtd-walk--lock-scope '("a.org" "b.org"))
    (assert-true (org-gtd-walk--scope-locked-p '("b.org" "a.org")))))

(provide 'walk-scope-test)

;;; walk-scope-test.el ends here
