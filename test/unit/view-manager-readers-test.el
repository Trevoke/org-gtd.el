;;; view-manager-readers-test.el --- Tests for infix readers -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the view-manager infix readers.  The interactive
;; `completing-read'/`read-string' readers get thin coverage; the pure
;; `org-gtd-view-manager--effort->dsl' transform (fail-soft parsing) is the
;; key surface exercised here.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-view-manager)

;; Initialize e-unit short syntax
(e-unit-initialize)

(deftest view-manager-reader/effort-parses-comparison ()
  "A `<30m' entry becomes the DSL shape (< \"30m\")."
  (assert-equal '(< "30m") (org-gtd-view-manager--effort->dsl "<30m"))
  (assert-equal '(> "1h")  (org-gtd-view-manager--effort->dsl ">1h")))

(deftest view-manager-reader/effort-rejects-garbage ()
  "A malformed effort raises the teaching error, not a stack trace."
  (let ((msg (condition-case err
                 (progn (org-gtd-view-manager--effort->dsl "banana") nil)
               (user-error (error-message-string err)))))
    (assert-true (and msg (string-match-p "duration like 30m" msg)))))

(deftest view-manager-reader/time-parses-literals-and-comparison ()
  "Literals become symbols; a comparison becomes the DSL list shape."
  (assert-equal 'today   (org-gtd-view-manager--time->dsl "today"))
  (assert-equal 'past    (org-gtd-view-manager--time->dsl "past"))
  (assert-equal 'future  (org-gtd-view-manager--time->dsl "future"))
  (assert-equal '(< "7d") (org-gtd-view-manager--time->dsl "<7d"))
  (assert-equal '(> "-2w") (org-gtd-view-manager--time->dsl ">-2w"))
  (assert-equal '(= "1M")  (org-gtd-view-manager--time->dsl "=1M")))

(deftest view-manager-reader/time-blank-unsets ()
  "A blank time entry returns nil (unset)."
  (assert-nil (org-gtd-view-manager--time->dsl ""))
  (assert-nil (org-gtd-view-manager--time->dsl "   ")))

(deftest view-manager-reader/time-rejects-garbage ()
  "A malformed time raises the teaching error, not a stack trace."
  (let ((msg (condition-case err
                 (progn (org-gtd-view-manager--time->dsl "banana") nil)
               (user-error (error-message-string err)))))
    (assert-true (and msg (string-match-p "past/today/future" msg)))))

(deftest view-manager-reader/prefix-parses-chain ()
  "A valid list literal becomes the fallback chain list."
  (assert-equal '(project area-of-focus "—")
                (org-gtd-view-manager--parse-prefix
                 "(project area-of-focus \"—\")")))

(deftest view-manager-reader/prefix-blank-unsets ()
  "A blank prefix returns nil so the DSL uses its default chain."
  (assert-nil (org-gtd-view-manager--parse-prefix ""))
  (assert-nil (org-gtd-view-manager--parse-prefix "   ")))

(deftest view-manager-reader/prefix-rejects-unbalanced ()
  "Unbalanced parens raise the teaching error, not an `end-of-file'."
  (let ((msg (condition-case err
                 (progn (org-gtd-view-manager--parse-prefix "(project area") nil)
               (user-error (error-message-string err)))))
    (assert-true (and msg (string-match-p "must be a list" msg)))))

(deftest view-manager-reader/prefix-rejects-bare-atom ()
  "A bare atom is not a chain and raises the teaching error."
  (let ((msg (condition-case err
                 (progn (org-gtd-view-manager--parse-prefix "project") nil)
               (user-error (error-message-string err)))))
    (assert-true (and msg (string-match-p "must be a list" msg)))))

(provide 'view-manager-readers-test)
;;; view-manager-readers-test.el ends here
