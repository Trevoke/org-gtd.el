;;; wip-symlink-test.el --- Test WIP cleanup with symlinks -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Regression test for GitHub issue #271: WIP buffer cleanup fails when
;; find-file-visit-truename is t and the temp directory is accessed through
;; a symlink.
;;
;; This test file uses its own mock-fs setup (not the standard around-each)
;; to properly test symlink behavior.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

;; NOTE: No around-each here - this test manages its own mock-fs

(deftest wip-symlink/cleanup-works-with-symlinked-tmp-directory ()
  "Cleanup works when temp directory is accessed through a symlink.
Regression test for GitHub issue #271: when find-file-visit-truename is t
and /tmp is a symlink, get-file-buffer fails to find the buffer because
the stored path differs from buffer-file-name. Using find-buffer-visiting
fixes this."
  ;; Create a mock filesystem where /tmp is a symlink to /real-tmp
  ;; This simulates macOS where /tmp -> /private/tmp
  (ogt-eunit--clear-org-state)
  (with-mock-fs `(("/gtd/" . directory)
                  ("/gtd/inbox.org" . "")
                  ("/gtd/org-gtd-tasks.org" . "")
                  ("/gtd/org-gtd-calendar.org" . "")
                  ("/gtd/org-gtd-incubate.org" . "")
                  ("/real-tmp/" . directory)
                  ;; org-gtd creates temp files in /tmp/org-gtd/
                  ("/real-tmp/org-gtd/" . directory)
                  ;; /tmp is a symlink to /real-tmp
                  ("/tmp" . (:symlink . "/real-tmp")))
    (ogt-eunit--configure-emacs)
    (let ((find-file-visit-truename t))
      (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
        (unwind-protect
            (progn
              (with-current-buffer source-buffer
                (org-gtd-clarify-item))

              (let* ((wip-buffer (car (org-gtd-wip--get-buffers)))
                     (buffer-file (buffer-file-name wip-buffer))
                     (clarify-id (with-current-buffer wip-buffer
                                   org-gtd-clarify--clarify-id))
                     (stored-file (gethash clarify-id org-gtd-wip--temp-files)))
                ;; With find-file-visit-truename=t, buffer-file-name is the resolved path
                ;; The stored path uses /tmp (symlink), buffer uses /real-tmp (target)
                (assert-match "/real-tmp" buffer-file)

                ;; The hash table stores the original (symlink) path
                (assert-match "/tmp" stored-file)

                ;; Verify the paths are different (this is the bug scenario)
                (assert-true (not (string= stored-file buffer-file)))

                ;; get-file-buffer would fail here (the bug)
                (assert-nil (get-file-buffer stored-file))

                ;; find-buffer-visiting works (the fix)
                (assert-equal wip-buffer (find-buffer-visiting stored-file))

                ;; Now verify cleanup actually works
                (org-gtd-wip--cleanup-temp-file clarify-id)

                ;; Buffer should be killed
                (assert-nil (buffer-live-p wip-buffer))))
          ;; Cleanup: kill source buffer if still alive
          (when (buffer-live-p source-buffer)
            (with-current-buffer source-buffer
              (set-buffer-modified-p nil))
            (kill-buffer source-buffer))
          (ogt-eunit--cleanup))))))

(provide 'wip-symlink-test)

;;; wip-symlink-test.el ends here
