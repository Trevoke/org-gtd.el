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

(defvar walk-scope-test--real-tmp temporary-file-directory
  "Real temp dir captured at load, before mock-fs rebinds
`temporary-file-directory' globally (test/helpers/setup.el).")

(around-each (proceed context)
  ;; Isolate from the mock-fs global leak of `temporary-file-directory'
  ;; so `make-temp-file' always creates a real (non-mock) directory,
  ;; regardless of which test ran before this one in the full suite.
  (let ((temporary-file-directory walk-scope-test--real-tmp))
    (funcall proceed context)))

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

;;; checkpoint file I/O

(deftest checkpoint-path-keys-on-name-and-resume-key ()
  "Different name or resume-key yields a different checkpoint path; same inputs match."
  (let ((org-gtd-directory "/tmp/gtd/"))
    (assert-not-equal (org-gtd-walk--checkpoint-path 'a "s")
                      (org-gtd-walk--checkpoint-path 'b "s"))
    (assert-not-equal (org-gtd-walk--checkpoint-path 'a "s")
                      (org-gtd-walk--checkpoint-path 'a "t"))
    (assert-equal (org-gtd-walk--checkpoint-path 'a "s")
                  (org-gtd-walk--checkpoint-path 'a "s"))))

(deftest checkpoint-save-load-delete-round-trip ()
  "A saved model reloads equal; delete removes the file."
  (let* ((org-gtd-directory (make-temp-file "walk-ckpt" t))
         (path (org-gtd-walk--checkpoint-path 'demo "scope"))
         (model (list :entries '("a" "b") :cursor 1 :meta nil)))
    (unwind-protect
        (progn
          (org-gtd-walk--save-checkpoint path model)
          (assert-true (file-exists-p path))
          (assert-equal model (org-gtd-walk--load-checkpoint path))
          (org-gtd-walk--delete-checkpoint path)
          (assert-nil (file-exists-p path)))
      (delete-directory org-gtd-directory t))))

(deftest checkpoint-load-missing-file-returns-nil ()
  (let ((org-gtd-directory (make-temp-file "walk-ckpt" t)))
    (unwind-protect
        (assert-nil (org-gtd-walk--load-checkpoint
                     (org-gtd-walk--checkpoint-path 'demo "scope")))
      (delete-directory org-gtd-directory t))))

(deftest checkpoint-load-corrupt-file-returns-nil ()
  "A garbage checkpoint file loads as nil (fresh-start fallback)."
  (let* ((org-gtd-directory (make-temp-file "walk-ckpt" t))
         (path (org-gtd-walk--checkpoint-path 'demo "scope")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "(:entries oops"))
          (assert-nil (org-gtd-walk--load-checkpoint path)))
      (delete-directory org-gtd-directory t))))

(provide 'walk-scope-test)

;;; walk-scope-test.el ends here
