;;; walk-driver-test.el --- Tier 2 integration tests for the walk driver -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tier 2 driver tests against a stub spec: headless, deterministic, no org,
;; no mock-fs.  Proves the render/checkpoint/transition lifecycle exactly once
;; (design §6, §9, §10).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-walk)

(e-unit-initialize)

;;; Stub spec + harness

(defvar walk-driver-test--render-log nil
  "Handles the stub :render has been called with, newest first.")
(defvar walk-driver-test--finish-count 0
  "How many times the stub :on-finish ran.")

(defun walk-driver-test--stub-spec (&rest overrides)
  "A minimal valid spec; OVERRIDES are applied as plist puts."
  (let ((spec (list :name 'stub
                    :find (lambda () (list "a" "b" "c"))
                    :render (lambda (handle _surface)
                              (push handle walk-driver-test--render-log))
                    :actions nil
                    :on-finish (lambda ()
                                 (setq walk-driver-test--finish-count
                                       (1+ walk-driver-test--finish-count)))
                    :resumable nil
                    :resolve nil
                    :scope "stub-scope")))
    (while overrides
      (setq spec (plist-put spec (pop overrides) (pop overrides))))
    spec))

(defmacro walk-driver-test--with-harness (surface-var &rest body)
  "Run BODY with fresh driver state and SURFACE-VAR bound to a temp buffer."
  (declare (indent 1))
  `(let ((walk-driver-test--render-log nil)
         (walk-driver-test--finish-count 0)
         (org-gtd-walk--locked-scopes nil)
         (org-gtd-directory (make-temp-file "walk-drv" t))
         (,surface-var (generate-new-buffer " *walk-test*")))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p ,surface-var) (kill-buffer ,surface-var))
       (delete-directory org-gtd-directory t))))

;;; start

(deftest walk-start-renders-first-item-and-activates ()
  "start with a non-empty find renders the first handle and stores a session."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface)
    (assert-equal '("a") walk-driver-test--render-log)
    (with-current-buffer surface
      (assert-true org-gtd-walk--active)
      (assert-equal "a" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))))

(provide 'walk-driver-test)

;;; walk-driver-test.el ends here
