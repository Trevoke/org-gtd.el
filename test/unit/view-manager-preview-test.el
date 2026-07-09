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

(deftest view-manager-preview/ret-forces-render-when-cache-current ()
  "Explicit RET preview renders even when the cache equals the current spec.
RET is the user's recovery hatch: it must never be silenced by
`--preview-changed-p', otherwise a stale preview is unrecoverable."
  (let ((count 0)
        (org-gtd-view-manager--build-state
         (list (cons 'name "x") (cons 'type 'next-action))))
    (setq org-gtd-view-manager--preview-last
          (org-gtd-view-manager--compile org-gtd-view-manager--build-state))
    (cl-letf (((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (&rest _) (cl-incf count))))
      (org-gtd-view-manager--preview))
    (assert-equal 1 count)))

(deftest view-manager-preview/debounce-still-skips-unchanged ()
  "The debounced (non-forced) path still skips a genuinely unchanged spec.
Guards the DRY refactor: adding `force' to `--preview-now' must not
break the debounce's coalescing (design: at most one render per idle
window)."
  (let ((count 0)
        (org-gtd-view-manager--build-state
         (list (cons 'name "x") (cons 'type 'next-action))))
    (setq org-gtd-view-manager--preview-last
          (org-gtd-view-manager--compile org-gtd-view-manager--build-state))
    (cl-letf (((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (&rest _) (cl-incf count))))
      (org-gtd-view-manager--preview-now))
    (assert-equal 0 count)))

(deftest view-manager-preview/render-binds-nondisruptive-window-setup ()
  "The preview render runs under a non-window-tearing `org-agenda-window-setup'.
Regression: the default `reorganize-frame' deletes/rearranges the frame's
windows when the preview calls `org-agenda', destroying the builder's
transient panel on every refresh.  `--render-preview' must bind
`org-agenda-window-setup' to `current-window' so the agenda lands in the
selected window without touching the transient's window."
  (let ((captured 'unset)
        (org-agenda-files nil)) ;; force the sample-data branch too
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (&rest _) (setq captured org-agenda-window-setup)))
              ;; Stub the sample-file writer: the sample branch only needs a
              ;; path here, and doing real disk I/O makes the test fragile to
              ;; a leaked mock-fs `temporary-file-directory' in the full suite.
              ((symbol-function 'org-gtd-view-manager--sample-file)
               (lambda (&rest _) "/tmp/org-gtd-view-sample.org")))
      (org-gtd-view-manager--render-preview
       '((name . "x") (type . next-action))))
    (assert-equal 'current-window captured)))

(provide 'view-manager-preview-test)
;;; view-manager-preview-test.el ends here
