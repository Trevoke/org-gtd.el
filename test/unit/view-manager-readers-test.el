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

(provide 'view-manager-readers-test)
;;; view-manager-readers-test.el ends here
