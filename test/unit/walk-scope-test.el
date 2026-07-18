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

(provide 'walk-scope-test)

;;; walk-scope-test.el ends here
