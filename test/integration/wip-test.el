;;; wip-test.el --- Integration tests for WIP buffer helpers -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for the with-wip-buffer macro and ogt-get-wip-buffer function.
;; These tests use mock-fs via ogt-eunit-with-mock-gtd macro.
;;
;; Migrated from test/helpers/wip-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; ogt-get-wip-buffer Tests

(deftest wip/get-wip-buffer-returns-nil-when-none ()
  "Returns nil when no WIP buffer exists."
  (assert-nil (ogt-get-wip-buffer)))

(deftest wip/get-wip-buffer-returns-buffer-when-exists ()
  "Returns the WIP buffer when one exists."
  ;; Create a WIP buffer by starting to process an inbox item
  (org-gtd-capture nil "i")
  (insert "Test item")
  (org-capture-finalize)
  (org-gtd-process-inbox)

  ;; Now there should be a WIP buffer
  (let ((wip-buf (ogt-get-wip-buffer)))
    (assert-true wip-buf)
    (assert-true (buffer-live-p wip-buf))
    (assert-true (string-search org-gtd-wip--prefix (buffer-name wip-buf)))))

;;; with-wip-buffer Macro Tests

(deftest wip/macro-executes-body-in-wip-context ()
  "Executes body in WIP buffer context."
  ;; Create a WIP buffer
  (org-gtd-capture nil "i")
  (insert "Test item for macro")
  (org-capture-finalize)
  (org-gtd-process-inbox)

  ;; Use the macro to insert text
  (with-wip-buffer
    (goto-char (point-max))
    (insert "\n** Added by macro"))

  ;; Verify the text was added
  (let ((wip-buf (ogt-get-wip-buffer)))
    (with-current-buffer wip-buf
      (assert-match "Added by macro" (buffer-string)))))

(deftest wip/macro-returns-nil-when-no-wip-buffer ()
  "Returns nil when no WIP buffer exists."
  (assert-nil (with-wip-buffer (point))))

(deftest wip/macro-returns-last-form-value ()
  "Returns the value of the last form."
  ;; Create a WIP buffer
  (org-gtd-capture nil "i")
  (insert "Test item")
  (org-capture-finalize)
  (org-gtd-process-inbox)

  ;; The macro should return the last form's value
  (assert-equal 42 (with-wip-buffer 42))
  (assert-equal 6 (with-wip-buffer (+ 1 2 3))))

(provide 'wip-integration-test)

;;; wip-test.el ends here
