;;; organizing-test.el --- Tests for org-gtd organizing -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd organizing functionality.
;;
;; Test Coverage:
;; - Cleanup after organizing (4 tests)
;; - Hook filter helper (5 tests)
;; - Saving buffers after organizing (2 tests)
;; - Update-in-place (1 test)
;; - Transient infix (1 test)
;; - Skip-refile behavior (1 test)
;;
;; Migrated from test/organizing-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Cleanup tests

(deftest cleanup/returns-to-previous-window-layout ()
  "Returns to previous window layout after organizing item."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
        (window-config nil)
        (org-gtd-refile-to-any-target t))
    (set-buffer source-buffer)
    (org-gtd-clarify-item)
    (setq window-config org-gtd-clarify--window-config)
    (org-gtd-single-action)
    (assert-true (compare-window-configurations (current-window-configuration) window-config))))

(deftest cleanup/cleans-up-temporary-wip-buffer ()
  "Cleans up temporary WIP buffer after organizing item."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
        (org-gtd-refile-to-any-target t))
    (set-buffer source-buffer)
    (org-gtd-clarify-item)
    (org-gtd-single-action)
    (assert-nil (org-gtd-wip--get-buffers))))

(deftest cleanup/deletes-source-heading ()
  "Deletes the source heading after organizing."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
        (org-gtd-refile-to-any-target t))
    (set-buffer source-buffer)
    (org-gtd-clarify-item)
    (org-gtd-single-action)
    (assert-true (current-buffer-empty?))))

(deftest cleanup/triggers-only-relevant-hooks ()
  "Triggers only the relevant hooks based on organize type."
  (defun hook1 ()
    (if (org-gtd-organize-type-member-p '(quick-action))
        (org-entry-put (point) "HOOK1" "YES")))
  (defun hook2 ()
    (if (org-gtd-organize-type-member-p '(single-action))
        (org-entry-put (point) "HOOK2" "YES")))
  (unwind-protect
      (let* ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
             (org-gtd-refile-to-any-target t)
             (org-gtd-organize-hooks '(hook1 hook2)))
        (set-buffer source-buffer)
        (org-gtd-clarify-item)
        (organize-as-single-action)
        (assert-true (file-contains? (org-gtd--default-file) "HOOK2")))
    (fmakunbound 'hook1)
    (fmakunbound 'hook2)))

;;; Hook filter helper tests

(deftest hook-filter/treats-single-argument-as-list ()
  "Treats a single argument properly as a list."
  (assert-true (org-gtd-organize-type-member-p 'everything)))

(deftest hook-filter/truthy-when-everything-in-list ()
  "Is truthy as long as 'everything is in the list."
  (assert-true (org-gtd-organize-type-member-p '(tickler trash everything project-task))))

(deftest hook-filter/signals-error-for-invalid-type ()
  "Signals an error if any element in the list is not one of the expected members."
  (assert-raises 'org-gtd-invalid-organize-action-type-error
    (org-gtd-organize-type-member-p '(foobar))))

(deftest hook-filter/truthy-when-buffer-local-matches ()
  "Is truthy if the buffer-local variable is in the list."
  (with-temp-buffer
    (setq-local org-gtd--organize-type 'quick-action)
    (assert-true (org-gtd-organize-type-member-p '(tickler quick-action delegated)))))

(deftest hook-filter/falsey-when-buffer-local-not-in-list ()
  "Is falsey if the buffer-local variable is not in the list."
  (with-temp-buffer
    (setq-local org-gtd--organize-type 'trash)
    (assert-nil (org-gtd-organize-type-member-p '(tickler quick-action delegated)))))

;;; Saving buffers after organizing tests

(deftest save-buffers/saves-when-enabled ()
  "Saves buffers if org-gtd-save-after-organize is t."
  (let ((org-gtd-save-after-organize t)
        (test-buffer (get-buffer-create "*org-gtd-test*")))
    (unwind-protect
        (with-spy save-some-buffers spy
          (with-current-buffer test-buffer
            (org-mode)
            (insert "* Test heading\n")
            (goto-char (point-min))
            (org-gtd-organize--call (lambda () (insert "Test"))))
          (assert-true (spy-called-p spy))
          (let ((call-args (spy-last-call spy)))
            (assert-equal t (car call-args))
            (assert-equal #'org-gtd-buffer-p (cadr call-args))))
      (kill-buffer test-buffer))))

(deftest save-buffers/does-not-save-when-disabled ()
  "Does not save buffers if org-gtd-save-after-organize is nil."
  (let ((org-gtd-save-after-organize nil)
        (test-buffer (get-buffer-create "*org-gtd-test*")))
    (unwind-protect
        (with-spy save-some-buffers spy
          (with-current-buffer test-buffer
            (org-mode)
            (insert "* Test heading\n")
            (goto-char (point-min))
            (org-gtd-organize--call (lambda () (insert "Test"))))
          (assert-nil (spy-called-p spy)))
      (kill-buffer test-buffer))))

;;; Update-in-place tests

(deftest update-in-place/replaces-original-with-wip-content ()
  "Replaces original heading with WIP buffer content."
  (create-single-action "Original title")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Original title")
    (org-back-to-heading t)
    ;; Clarify the item (this sets org-gtd-clarify--source-heading-marker)
    (org-gtd-clarify-item)
    ;; Get the WIP buffer and modify content
    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        (goto-char (point-min))
        (search-forward "Original title")
        (replace-match "Modified title")
        ;; Call update-in-place
        (org-gtd-organize--update-in-place)))
    ;; Verify original location has new content
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (assert-true (search-forward "Modified title" nil t))
      (goto-char (point-min))
      (assert-nil (search-forward "Original title" nil t)))))

;;; Transient tests

(deftest transient/skip-refile-infix-defined ()
  "Has skip-refile toggle infix command defined."
  (assert-true (fboundp 'org-gtd-organize--skip-refile-infix)))

;;; Skip-refile behavior tests

(deftest skip-refile/updates-in-place-without-refiling ()
  "Updates single-action in place when skip-refile is set."
  (create-single-action "Update me")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Update me")
    (org-back-to-heading t)
    ;; Clarify with skip-refile (C-u prefix)
    (let ((current-prefix-arg '(4)))
      (org-gtd-clarify-item))
    ;; Use spy on org-refile to ensure it's NOT called when skip-refile is set
    (with-spy org-refile spy
      ;; Modify in WIP buffer and re-organize
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        (goto-char (point-min))
        (search-forward "Update me")
        (replace-match "Updated item")
        ;; With skip-refile, this should call update-in-place, not refile
        (org-gtd-single-action))
      ;; Verify org-refile was NOT called (skip-refile should use update-in-place)
      (assert-nil (spy-called-p spy)))
    ;; Verify item was updated in place in the default file
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      ;; Should find "Updated item" exactly once (updated in place)
      (assert-equal 1 (how-many "Updated item" (point-min) (point-max)))
      ;; Original should be gone
      (goto-char (point-min))
      (assert-nil (search-forward "Update me" nil t)))))

(provide 'organizing-test)

;;; organizing-test.el ends here
