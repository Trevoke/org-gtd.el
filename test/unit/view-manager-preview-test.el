;;; view-manager-preview-test.el --- Tests for the live preview loop -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for `org-gtd-view-manager--preview-changed-p', the pure
;; skip-if-unchanged guard behind the debounced live preview.  The timer
;; and render paths are verified manually (see the Task 10 commit body).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-view-manager)

;; Initialize e-unit short syntax
(e-unit-initialize)

(deftest view-manager-preview/skips-when-spec-unchanged ()
  "The changed-check returns nil when the compiled spec equals the cache."
  (let ((org-gtd-view-manager--preview-last '((name . "x") (type . next-action))))
    (assert-nil (org-gtd-view-manager--preview-changed-p
                 '((name . "x") (type . next-action))))))

(deftest view-manager-preview/detects-change ()
  "The changed-check returns non-nil when the compiled spec differs."
  (let ((org-gtd-view-manager--preview-last '((name . "x") (type . next-action))))
    (assert-true (org-gtd-view-manager--preview-changed-p
                  '((name . "x") (type . delegated))))))

(provide 'view-manager-preview-test)
;;; view-manager-preview-test.el ends here
