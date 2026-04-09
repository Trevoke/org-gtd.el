;;; disposition-runner-test.el --- Disposition runner state contract -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Verifies the public contract of the done-and-archive and
;; cancel-and-archive dispositions: the disposition itself is
;; responsible for setting the TODO state before archiving, so a
;; custom :organize-fn that only sets ORG_GTD still produces a
;; correctly-stated archived item.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-organize-core)
(require 'org-gtd-types)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defun dispo-test--minimal-organize-fn (_type _config)
  "An :organize-fn that ONLY sets ORG_GTD (no TODO state).
Used to prove that the disposition runner, not the organize-fn,
owns the done/canceled state transition."
  (org-entry-put nil "ORG_GTD" "TestArchive"))

(deftest disposition/done-and-archive-sets-done-state ()
  "done-and-archive disposition must leave the item marked DONE
even when the type's :organize-fn does not set the state."
  (let ((org-gtd-user-types
         '((reference
            :organize-fn dispo-test--minimal-organize-fn))))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test heading\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      ;; Intercept archive so the heading stays in the buffer and
      ;; we can assert on its TODO state.
      (cl-letf (((symbol-function 'org-gtd-archive-item-at-point)
                 (lambda () nil)))
        (org-gtd-process-heading (point-marker) 'reference nil))
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--done)
                    (org-get-todo-state)))))

(deftest disposition/cancel-and-archive-sets-canceled-state ()
  "cancel-and-archive disposition must leave the item marked CNCL
even when the type's :organize-fn does not set the state."
  (let ((org-gtd-user-types
         '((trash
            :organize-fn dispo-test--minimal-organize-fn))))
    (with-temp-buffer
      (org-mode)
      (insert "* TODO Test heading\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (cl-letf (((symbol-function 'org-gtd-archive-item-at-point)
                 (lambda () nil)))
        (org-gtd-process-heading (point-marker) 'trash nil))
      (org-back-to-heading t)
      (assert-equal (org-gtd-keywords--canceled)
                    (org-get-todo-state)))))

(provide 'disposition-runner-test)

;;; disposition-runner-test.el ends here
