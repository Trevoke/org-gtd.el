;;; inbox-walk-test.el --- Tests for the inbox walk adapter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tier-3 tests for the inbox walk adapter (org-gtd-inbox-walk.el).
;; This is additive: none of these tests exercise `org-gtd-process-inbox'
;; or `org-gtd-walk-start' -- the adapter's `:find'/`:render' are driven
;; directly.  See docs/plans/2026-07-17-walk-engine-phase-4-plan.md
;; Tasks 1-3.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-inbox-walk)
(require 'org-gtd-walk)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Scan Tests (Task 1)

(deftest inbox-walk/scan-returns-tokens-for-all-inbox-headings ()
  "Scan returns one token per inbox heading, main inbox then additional."
  (capture-inbox-item "Main item one")
  (capture-inbox-item "Main item two")
  (let* ((additional-inbox-file (f-join org-gtd-directory "additional-inbox.org")))
    (with-current-buffer (find-file-noselect additional-inbox-file)
      (insert "* Additional item\n")
      (basic-save-buffer))
    (let* ((org-gtd-additional-inbox-files (list additional-inbox-file))
           (scanned (org-gtd-inbox-walk--scan))
           (tokens (car scanned))
           (meta (cdr scanned)))
      (assert-equal 3 (length tokens))
      (dolist (token tokens)
        (assert-true (stringp token))
        (assert-true (org-gtd-walk-model--handle-serializable-p token)))
      ;; Main inbox items come first, in document order, then additional.
      (let ((marker-1 (cdr (assoc (nth 0 tokens) meta)))
            (marker-2 (cdr (assoc (nth 1 tokens) meta)))
            (marker-3 (cdr (assoc (nth 2 tokens) meta))))
        (assert-true (markerp marker-1))
        (assert-true (markerp marker-2))
        (assert-true (markerp marker-3))
        (assert-match "Main item one" (org-with-point-at marker-1 (org-get-heading t t t t)))
        (assert-match "Main item two" (org-with-point-at marker-2 (org-get-heading t t t t)))
        (assert-match "Additional item" (org-with-point-at marker-3 (org-get-heading t t t t))))
      ;; The constructed model stays valid; meta-with-markers doesn't
      ;; break validity (validity only inspects entries + cursor).
      (let ((model (org-gtd-walk-model-create tokens meta)))
        (assert-true (org-gtd-walk-model-valid-p model))))))

(deftest inbox-walk/scan-skips-missing-additional-inbox-file ()
  "Scan skips an additional inbox file that does not exist."
  (capture-inbox-item "Only main item")
  (let* ((missing-file (f-join org-gtd-directory "does-not-exist.org"))
         (org-gtd-additional-inbox-files (list missing-file))
         (tokens (car (org-gtd-inbox-walk--scan))))
    (assert-equal 1 (length tokens))))

(deftest inbox-walk/scan-skips-empty-additional-inbox-file ()
  "Scan skips an additional inbox file with no headings."
  (capture-inbox-item "Only main item")
  (let* ((empty-file (f-join org-gtd-directory "empty-inbox.org")))
    (with-current-buffer (find-file-noselect empty-file)
      (basic-save-buffer))
    (let* ((org-gtd-additional-inbox-files (list empty-file))
           (tokens (car (org-gtd-inbox-walk--scan))))
      (assert-equal 1 (length tokens)))))

(deftest inbox-walk/scan-orders-multiple-additional-inboxes ()
  "Scan visits additional inbox files in the order listed."
  (let* ((inbox1-file (f-join org-gtd-directory "inbox1.org"))
         (inbox2-file (f-join org-gtd-directory "inbox2.org")))
    (with-current-buffer (find-file-noselect inbox1-file)
      (insert "* Item from inbox 1\n")
      (basic-save-buffer))
    (with-current-buffer (find-file-noselect inbox2-file)
      (insert "* Item from inbox 2\n")
      (basic-save-buffer))
    (let* ((org-gtd-additional-inbox-files (list inbox1-file inbox2-file))
           (scanned (org-gtd-inbox-walk--scan))
           (tokens (car scanned))
           (meta (cdr scanned)))
      (assert-equal 2 (length tokens))
      (assert-match "Item from inbox 1"
                    (org-with-point-at (cdr (assoc (nth 0 tokens) meta))
                      (org-get-heading t t t t)))
      (assert-match "Item from inbox 2"
                    (org-with-point-at (cdr (assoc (nth 1 tokens) meta))
                      (org-get-heading t t t t))))))

(deftest inbox-walk/marker-survives-cut-of-earlier-heading ()
  "The second item's marker still resolves after the first is cut.
This is the property (D2) that makes markers correct where captured
positions would be broken by in-session edits."
  (capture-inbox-item "First item")
  (capture-inbox-item "Second item")
  (let* ((scanned (org-gtd-inbox-walk--scan))
         (tokens (car scanned))
         (meta (cdr scanned))
         (marker-1 (cdr (assoc (nth 0 tokens) meta)))
         (marker-2 (cdr (assoc (nth 1 tokens) meta))))
    ;; Cut the first item's subtree out of its buffer.
    (with-current-buffer (marker-buffer marker-1)
      (org-with-point-at marker-1
        (org-cut-subtree)))
    ;; The second token's marker still resolves to the correct heading.
    (assert-true (marker-buffer marker-2))
    (assert-match "Second item" (org-with-point-at marker-2 (org-get-heading t t t t)))))

(deftest inbox-walk/build-model-seeds-meta-from-scan ()
  "The model builder combines scan tokens and meta into one valid model."
  (capture-inbox-item "Solo item")
  (let ((model (org-gtd-inbox-walk--build-model)))
    (assert-true (org-gtd-walk-model-valid-p model))
    (assert-equal 1 (length (plist-get model :entries)))
    (let ((marker (cdr (assoc (car (plist-get model :entries)) (plist-get model :meta)))))
      (assert-true (markerp marker))
      (assert-match "Solo item" (org-with-point-at marker (org-get-heading t t t t))))))

(provide 'inbox-walk-test)

;;; inbox-walk-test.el ends here
