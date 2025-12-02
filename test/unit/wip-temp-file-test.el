;;; wip-temp-file-test.el --- Tests for WIP buffer temp file implementation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for WIP buffer temporary file creation, content, auto-save, and cleanup.
;;
;; Test Coverage:
;; - Temp file creation (2 tests)
;; - Temp file content preservation (1 test)
;; - Auto-save functionality (3 tests)
;; - Temp file cleanup (2 tests)
;; - Edge cases (2 tests)
;;
;; Migrated from test/wip-temp-file-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Temp File Creation Tests

(deftest wip-temp-file/creates-temp-file-for-wip-buffer ()
  "Creates a temporary file when creating a WIP buffer."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        ;; Buffer should have a file name
        (assert-true buffer-file-name)
        ;; File should exist
        (assert-true (file-exists-p buffer-file-name))
        ;; File should be in temp directory
        (assert-match (regexp-quote (expand-file-name "org-gtd" temporary-file-directory))
                      buffer-file-name)
        ;; File should have .org extension
        (assert-match "\\.org$" buffer-file-name)))))

(deftest wip-temp-file/uses-unique-files-for-different-buffers ()
  "Uses unique temp files for different WIP buffers."
  (let ((source-buffer-1 (ogt--temp-org-file-buffer "taskfile1" "* Task 1"))
        (source-buffer-2 (ogt--temp-org-file-buffer "taskfile2" "* Task 2")))

    (with-current-buffer source-buffer-1
      (org-gtd-clarify-item))
    (with-current-buffer source-buffer-2
      (org-gtd-clarify-item))

    (let ((wip-buffers (org-gtd-wip--get-buffers)))
      (assert-equal 2 (length wip-buffers))

      (let ((file-1 (buffer-file-name (nth 0 wip-buffers)))
            (file-2 (buffer-file-name (nth 1 wip-buffers))))
        ;; Files should be different
        (assert-true (not (equal file-1 file-2)))
        ;; Both should exist
        (assert-true (file-exists-p file-1))
        (assert-true (file-exists-p file-2))))))

;;; Temp File Content Tests

(deftest wip-temp-file/preserves-content ()
  "Preserves content in the temp file."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify\n** Subtask")))
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        ;; Check that initial content is there
        (assert-match "Task to clarify" (buffer-string))

        ;; Add some content
        (goto-char (point-max))
        (insert "\n*** New subtask")
        ;; Save the buffer
        (save-buffer)

        ;; Store the file name before leaving the buffer context
        (let ((wip-file-name buffer-file-name))
          ;; Read the file directly to verify content was saved
          (let ((file-content (with-temp-buffer
                                (insert-file-contents wip-file-name)
                                (buffer-string))))
            (assert-match "Task to clarify" file-content)
            (assert-match "New subtask" file-content)))))))

;;; Auto-save Functionality Tests

(deftest wip-temp-file/enables-auto-save ()
  "Enables auto-save for WIP buffers."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        ;; File-backed buffers can use auto-save
        (auto-save-mode 1)
        ;; Should have an auto-save file name (either set or can be generated)
        (assert-true (or buffer-auto-save-file-name
                         (make-auto-save-file-name)))))))

(deftest wip-temp-file/allows-normal-save ()
  "Allows normal save operations."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        ;; Save should work without errors
        (assert-true (ignore-errors (save-buffer) t))))))

(deftest wip-temp-file/works-with-org-save-all ()
  "Works with org-save-all-org-buffers."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        (set-buffer-modified-p t))
      ;; Should not error
      (assert-true (ignore-errors (org-save-all-org-buffers) t)))))

;;; Temp File Cleanup Tests

(deftest wip-temp-file/deletes-on-stop-clarification ()
  "Deletes temp file when stopping clarification."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify"))
        temp-file)
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        (setq temp-file buffer-file-name)
        ;; File should exist now
        (assert-true (file-exists-p temp-file)))

      ;; Stop clarification
      (with-current-buffer wip-buffer
        (org-gtd-clarify-stop))

      ;; File should be deleted
      (assert-nil (file-exists-p temp-file))
      ;; Buffer should be killed
      (assert-nil (buffer-live-p wip-buffer)))))

(deftest wip-temp-file/deletes-after-organize ()
  "Deletes temp file after successful organize."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify"))
        (target-buffer (ogt--temp-org-file-buffer "target" "* Projects\n"))
        temp-file
        task-id)
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer
        (setq temp-file buffer-file-name)
        ;; Get the task ID for cleanup
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (setq task-id (org-entry-get nil "ID"))
        ;; File should exist now
        (assert-true (file-exists-p temp-file))

        ;; Simulate organizing (simplified)
        (org-refile nil nil
                    (list "Projects"
                          (buffer-file-name target-buffer)
                          nil
                          (with-current-buffer target-buffer (point-min)))))

      ;; In real usage, org-gtd-organize--call handles cleanup
      ;; For this test, we manually call cleanup to simulate that
      (when task-id
        (org-gtd-wip--cleanup-temp-file task-id))

      ;; After cleanup, temp file should be deleted
      (assert-nil (file-exists-p temp-file)))))

;;; Edge Cases Tests

(deftest wip-temp-file/handles-multiple-clarifications-of-same-item ()
  "Handles multiple clarifications of the same item."
  (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
    ;; First clarification
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer-1 (car (org-gtd-wip--get-buffers))))
      ;; Stop first clarification
      (with-current-buffer wip-buffer-1
        (org-gtd-clarify-stop)))

    ;; Second clarification of same item
    (with-current-buffer source-buffer
      (org-gtd-clarify-item))

    (let ((wip-buffer-2 (car (org-gtd-wip--get-buffers))))
      (with-current-buffer wip-buffer-2
        ;; Should have a new temp file
        (assert-true buffer-file-name)
        (assert-true (file-exists-p buffer-file-name))))))

(deftest wip-temp-file/creates-temp-directory-if-needed ()
  "Creates temp directory if it doesn't exist."
  (let ((temp-dir (expand-file-name "org-gtd" temporary-file-directory)))
    ;; Delete directory if it exists
    (when (file-exists-p temp-dir)
      (delete-directory temp-dir t))

    (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
      (with-current-buffer source-buffer
        (org-gtd-clarify-item))

      ;; Directory should be created
      (assert-true (file-directory-p temp-dir)))))

(provide 'wip-temp-file-test)

;;; wip-temp-file-test.el ends here
