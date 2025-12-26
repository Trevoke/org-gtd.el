;;; view-language-last-clocked-out-test.el --- Tests for last-clocked-out filter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for last-clocked-out filter integration in the View DSL.
;; Tests translation, skip function building, and error handling.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd)
(require 'org-gtd-view-language)

;;; Last-Clocked-Out Skip Predicate Integration Tests

(deftest view-lang/skip-last-clocked-out-nil-matches-never-clocked ()
  "Skip predicate with last-clocked-out=nil matches items never clocked."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n\nSome content here.\n")
    (goto-char (point-min))
    (re-search-forward "^\\* TODO Test")
    (org-back-to-heading t)
    (let* ((view-spec '((type . next-action)
                        (last-clocked-out . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; No clock entries = never clocked, should match
      (assert-nil result))))

(deftest view-lang/skip-last-clocked-out-greater-skips-never-clocked ()
  "Skip predicate with last-clocked-out=(> \"2d\") skips items never clocked."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n\nSome content here.\n")
    (goto-char (point-min))
    (re-search-forward "^\\* TODO Test")
    (org-back-to-heading t)
    (let* ((view-spec '((type . next-action)
                        (last-clocked-out . (> "2d"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Never clocked should be skipped for > comparison
      (assert-true (numberp result)))))

(deftest view-lang/skip-last-clocked-out-less-skips-never-clocked ()
  "Skip predicate with last-clocked-out=(< \"1w\") skips items never clocked."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n\nSome content here.\n")
    (goto-char (point-min))
    (re-search-forward "^\\* TODO Test")
    (org-back-to-heading t)
    (let* ((view-spec '((type . next-action)
                        (last-clocked-out . (< "1w"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Never clocked should be skipped for < comparison
      (assert-true (numberp result)))))

(provide 'view-language-last-clocked-out-test)

;;; view-language-last-clocked-out-test.el ends here
