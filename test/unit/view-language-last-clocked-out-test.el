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

;;; Last-Clocked-Out Filter Translation Tests

(deftest view-lang/last-clocked-out-greater-than ()
  "Translates last-clocked-out=(> \"2d\") filter."
  (let ((view-spec '((name . "Stale Tasks")
                     (type . next-action)
                     (last-clocked-out . (> "2d")))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      (assert-true (cl-find 'last-clocked-out (flatten-list query))))))

(deftest view-lang/last-clocked-out-less-than ()
  "Translates last-clocked-out=(< \"1w\") filter."
  (let ((view-spec '((name . "Recent Work")
                     (type . next-action)
                     (last-clocked-out . (< "1w")))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      (assert-true (cl-find 'last-clocked-out (flatten-list query))))))

(deftest view-lang/last-clocked-out-nil-never-clocked ()
  "Translates last-clocked-out=nil for never clocked items."
  (let ((view-spec '((name . "Never Worked")
                     (type . next-action)
                     (last-clocked-out . nil))))
    (let ((query (org-gtd-view-lang--translate-to-org-ql view-spec)))
      (assert-true (cl-find 'last-clocked-out-nil (flatten-list query))))))

(deftest view-lang/last-clocked-out-invalid-format-signals-error ()
  "Signals error for invalid last-clocked-out format."
  (let ((view-spec '((name . "Invalid")
                     (type . next-action)
                     (last-clocked-out . "invalid"))))
    (assert-raises 'user-error
      (org-gtd-view-lang--translate-to-org-ql view-spec))))

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

;;; Translation Function Direct Tests

(deftest view-lang/translate-last-clocked-out-greater-than ()
  "Translates (> \"2d\") to proper filter form."
  (let ((result (org-gtd-view-lang--translate-last-clocked-out-filter '(> "2d"))))
    (assert-equal '((last-clocked-out > "2d")) result)))

(deftest view-lang/translate-last-clocked-out-less-than ()
  "Translates (< \"1w\") to proper filter form."
  (let ((result (org-gtd-view-lang--translate-last-clocked-out-filter '(< "1w"))))
    (assert-equal '((last-clocked-out < "1w")) result)))

(deftest view-lang/translate-last-clocked-out-gte ()
  "Translates (>= \"3d\") to proper filter form."
  (let ((result (org-gtd-view-lang--translate-last-clocked-out-filter '(>= "3d"))))
    (assert-equal '((last-clocked-out >= "3d")) result)))

(deftest view-lang/translate-last-clocked-out-lte ()
  "Translates (<= \"5d\") to proper filter form."
  (let ((result (org-gtd-view-lang--translate-last-clocked-out-filter '(<= "5d"))))
    (assert-equal '((last-clocked-out <= "5d")) result)))

(deftest view-lang/translate-last-clocked-out-nil ()
  "Translates nil to never-clocked filter."
  (let ((result (org-gtd-view-lang--translate-last-clocked-out-filter nil)))
    (assert-equal '((last-clocked-out-nil)) result)))

(deftest view-lang/translate-last-clocked-out-invalid-signals-error ()
  "Signals user-error for invalid filter value."
  (assert-raises 'user-error
    (org-gtd-view-lang--translate-last-clocked-out-filter "invalid")))

(deftest view-lang/translate-last-clocked-out-invalid-op-signals-error ()
  "Signals user-error for invalid comparison operator."
  (assert-raises 'user-error
    (org-gtd-view-lang--translate-last-clocked-out-filter '(= "2d"))))

(provide 'view-language-last-clocked-out-test)

;;; view-language-last-clocked-out-test.el ends here
