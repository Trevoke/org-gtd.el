;;; capture-test.el --- Tests for org-gtd capture functionality -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-capture, including the ORG_GTD_CAPTURED_AT timestamp.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-capture)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; ORG_GTD_CAPTURED_AT timestamp tests

(deftest capture/i-template-adds-inactive-timestamp ()
  "Capturing with 'i' template adds ORG_GTD_CAPTURED_AT as inactive timestamp."
  (let ((before-capture (current-time)))
    (capture-inbox-item "Test item")
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (let* ((captured-at (org-entry-get nil "ORG_GTD_CAPTURED_AT"))
             (parsed-time (org-parse-time-string captured-at))
             (captured-time (encode-time parsed-time)))
        ;; Property exists and is inactive timestamp format [...]
        (assert-match "^\\[.*\\]$" captured-at)
        ;; Time is approximately current (within 60 seconds)
        (assert-true (< (float-time (time-subtract captured-time before-capture)) 60))))))

(deftest capture/l-template-adds-inactive-timestamp ()
  "Capturing with 'l' (link) template adds ORG_GTD_CAPTURED_AT as inactive timestamp."
  (let ((before-capture (current-time))
        (inhibit-message t))
    ;; Use "l" template directly
    (org-gtd-capture nil "l")
    (insert "Item with link")
    (org-capture-finalize)
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (let* ((captured-at (org-entry-get nil "ORG_GTD_CAPTURED_AT"))
             (parsed-time (org-parse-time-string captured-at))
             (captured-time (encode-time parsed-time)))
        ;; Property exists and is inactive timestamp format [...]
        (assert-match "^\\[.*\\]$" captured-at)
        ;; Time is approximately current (within 60 seconds)
        (assert-true (< (float-time (time-subtract captured-time before-capture)) 60))))))

(deftest capture/multiple-captures-each-get-timestamp ()
  "Each captured item gets its own ORG_GTD_CAPTURED_AT timestamp."
  (capture-inbox-item "First item")
  (capture-inbox-item "Second item")
  (capture-inbox-item "Third item")
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    ;; Count items with ORG_GTD_CAPTURED_AT property
    (let ((count 0))
      (org-map-entries
       (lambda ()
         (when (org-entry-get nil "ORG_GTD_CAPTURED_AT")
           (setq count (1+ count)))))
      (assert-equal 3 count))))

(provide 'capture-test)

;;; capture-test.el ends here
