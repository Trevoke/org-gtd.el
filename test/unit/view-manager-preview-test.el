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
(require 'cl-lib)
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

(deftest view-manager-preview/compile-does-not-alias-build-state ()
  "The compiled spec must not share mutable cons cells with the build state.
Regression: `--set-value' mutates an existing key in place via
`setf (alist-get ...)'.  When the compiled spec shared that cell,
`--preview-last' was silently corrupted and the value->value change
was reported as `unchanged', skipping the live-preview render."
  (let ((org-gtd-view-manager--build-state
         (list (cons 'name "x")
               (cons 'type 'next-action)
               (cons 'area-of-focus "Work"))))
    (let ((spec (org-gtd-view-manager--compile
                 org-gtd-view-manager--build-state)))
      ;; Mutate the live state exactly like `--set-value' does for an
      ;; existing key.  The already-compiled snapshot must NOT change.
      (setf (alist-get 'area-of-focus org-gtd-view-manager--build-state) "Home")
      (assert-equal "Work" (alist-get 'area-of-focus spec)))))

(deftest view-manager-preview/re-renders-on-value-change ()
  "A value->value filter change re-renders the live preview.
Simulates the debounced path: render Work, then mutate the value in
place to Home (as `--set-value' does) and fire `--preview-now' again.
Both renders must happen -- the second must not be skipped by a stale
`--preview-last' cache."
  (let ((count 0)
        (org-gtd-view-manager--preview-last nil)
        (org-gtd-view-manager--build-state
         (list (cons 'name "x")
               (cons 'type 'next-action)
               (cons 'area-of-focus "Work"))))
    (cl-letf (((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (&rest _) (cl-incf count))))
      (org-gtd-view-manager--preview-now)
      (setf (alist-get 'area-of-focus org-gtd-view-manager--build-state) "Home")
      (org-gtd-view-manager--preview-now))
    (assert-equal 2 count)))

(provide 'view-manager-preview-test)
;;; view-manager-preview-test.el ends here
