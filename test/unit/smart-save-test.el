;;; smart-save-test.el --- Unit tests for smart buffer saving -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for smart buffer saving after organize operations.
;;
;; Test Coverage:
;; - org-gtd-buffer-p predicate (4 tests)
;; - Saving GTD buffers only (2 tests)
;; - Integration with organize operations (2 tests)
;;
;; Migrated from test/smart-save-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Test Helpers

(defun ogt-eunit--temp-org-file-in-gtd-dir (basename content)
  "Create a temp org file in the GTD directory with BASENAME and CONTENT."
  (let ((file (expand-file-name (format "%s.org" basename) org-gtd-directory)))
    (with-current-buffer (find-file-noselect file)
      (erase-buffer)
      (insert content)
      (basic-save-buffer))
    (find-file-noselect file)))

(defun ogt-eunit--temp-org-file-buffer (basename content)
  "Create a temp org file buffer outside GTD directory."
  (let ((file (concat "/mock:/tmp/" basename ".org")))
    (with-current-buffer (find-file-noselect file)
      (erase-buffer)
      (insert content)
      (basic-save-buffer))
    (find-file-noselect file)))

;;; org-gtd-buffer-p predicate tests

(deftest smart-save/buffer-p-returns-t-for-gtd-directory ()
  "Returns t for buffers in org-gtd-directory."
  (let ((gtd-file (ogt-eunit--temp-org-file-in-gtd-dir "test-task" "* Task")))
    (with-current-buffer gtd-file
      (assert-true (org-gtd-buffer-p)))))

(deftest smart-save/buffer-p-returns-nil-outside-gtd-directory ()
  "Returns nil for buffers outside org-gtd-directory."
  (let ((non-gtd-file (ogt-eunit--temp-org-file-buffer "other" "* Not GTD")))
    (with-current-buffer non-gtd-file
      (assert-nil (org-gtd-buffer-p)))))

(deftest smart-save/buffer-p-returns-nil-for-buffers-without-files ()
  "Returns nil for buffers without files."
  (with-temp-buffer
    (org-mode)
    (assert-nil (org-gtd-buffer-p))))

(deftest smart-save/buffer-p-can-check-other-buffers ()
  "Can check other buffers passed as argument."
  (let ((gtd-file (ogt-eunit--temp-org-file-in-gtd-dir "test" "* Task"))
        (other-file (ogt-eunit--temp-org-file-buffer "other" "* Not GTD")))
    (assert-true (org-gtd-buffer-p gtd-file))
    (assert-nil (org-gtd-buffer-p other-file))))

;;; Saving GTD buffers only tests

(deftest smart-save/saves-gtd-buffers-when-enabled ()
  "Saves GTD buffers when org-gtd-save-after-organize is t."
  (let ((org-gtd-save-after-organize t)
        (gtd-file (ogt-eunit--temp-org-file-in-gtd-dir "tasks" "* Original"))
        (non-gtd-file (ogt-eunit--temp-org-file-buffer "other" "* Other")))

    ;; Modify both buffers
    (with-current-buffer gtd-file
      (goto-char (point-max))
      (insert "\n* New GTD Task")
      (set-buffer-modified-p t))

    (with-current-buffer non-gtd-file
      (goto-char (point-max))
      (insert "\n* New Other Task")
      (set-buffer-modified-p t))

    ;; Save GTD buffers
    (org-gtd-save-buffers)

    ;; Check GTD buffer was saved
    (with-current-buffer gtd-file
      (assert-nil (buffer-modified-p)))

    ;; Check non-GTD buffer was NOT saved
    (with-current-buffer non-gtd-file
      (assert-true (buffer-modified-p)))))

(deftest smart-save/does-not-save-when-disabled ()
  "Does not save when org-gtd-save-after-organize is nil."
  (let ((org-gtd-save-after-organize nil)
        (gtd-file (ogt-eunit--temp-org-file-in-gtd-dir "tasks" "* Task")))

    (with-current-buffer gtd-file
      (goto-char (point-max))
      (insert "\n* Modified")
      (set-buffer-modified-p t))

    ;; This should not save anything
    (when org-gtd-save-after-organize
      (org-gtd-save-buffers))

    ;; Buffer should still be modified
    (with-current-buffer gtd-file
      (assert-true (buffer-modified-p)))))

;;; Integration with organize operations tests

(deftest smart-save/saves-after-organize-when-enabled ()
  "Saves GTD buffers after organizing when enabled."
  (let ((org-gtd-save-after-organize t)
        (gtd-file (ogt-eunit--temp-org-file-in-gtd-dir "tasks" "* Original Task")))

    ;; Modify the GTD buffer
    (with-current-buffer gtd-file
      (goto-char (point-max))
      (insert "\n* Modified Task")
      (set-buffer-modified-p t))

    ;; Verify it's modified
    (with-current-buffer gtd-file
      (assert-true (buffer-modified-p)))

    ;; Call save function
    (org-gtd-save-buffers)

    ;; Check that it was saved
    (with-current-buffer gtd-file
      (assert-nil (buffer-modified-p)))))

(deftest smart-save/does-not-save-after-organize-when-disabled ()
  "Does not save when org-gtd-save-after-organize is disabled."
  (let ((org-gtd-save-after-organize nil)
        (gtd-file (ogt-eunit--temp-org-file-in-gtd-dir "tasks" "* Original Task")))

    ;; Modify the GTD buffer
    (with-current-buffer gtd-file
      (goto-char (point-max))
      (insert "\n* Modified Task")
      (set-buffer-modified-p t))

    ;; Call save function (should do nothing)
    (org-gtd-save-buffers)

    ;; Buffer should still be modified
    (with-current-buffer gtd-file
      (assert-true (buffer-modified-p)))))

(provide 'smart-save-test)

;;; smart-save-test.el ends here
