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

;;; advance

(deftest walk-advance-renders-next-item ()
  "advance moves the cursor and re-renders the new current handle."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface)
    (with-current-buffer surface (org-gtd-walk-advance))
    ;; newest render first: "b" after "a"
    (assert-equal '("b" "a") walk-driver-test--render-log)
    (with-current-buffer surface
      (assert-equal "b" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))))

(deftest walk-advance-off-end-finishes-and-clears-session ()
  "Running past the last item finishes: on-finish runs, session cleared, scope unlocked."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface) ; on "a"
    (with-current-buffer surface
      (org-gtd-walk-advance)   ; "b"
      (org-gtd-walk-advance)   ; "c"
      (org-gtd-walk-advance))  ; off end -> finish
    (assert-same 1 walk-driver-test--finish-count)
    (with-current-buffer surface (assert-nil org-gtd-walk--active))
    (assert-nil (org-gtd-walk--scope-locked-p "stub-scope"))))

;;; empty find

(deftest walk-empty-find-finishes-without-activating ()
  "An empty find runs on-finish, renders nothing, activates nothing, locks nothing."
  (walk-driver-test--with-harness surface
    (let ((result (org-gtd-walk-start
                   (walk-driver-test--stub-spec :find (lambda () '()))
                   surface)))
      (assert-nil result)
      (assert-nil walk-driver-test--render-log)
      (assert-same 1 walk-driver-test--finish-count)
      (with-current-buffer surface (assert-nil org-gtd-walk--active))
      (assert-nil (org-gtd-walk--scope-locked-p "stub-scope")))))

;;; enqueue

(deftest walk-enqueue-bottom-extends-without-moving-cursor ()
  "enqueue bottom adds a pending item and re-renders the (unchanged) current."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface) ; on "a"
    (with-current-buffer surface (org-gtd-walk-enqueue "z" 'bottom))
    (with-current-buffer surface
      (assert-equal '("a" "b" "c" "z")
                    (plist-get (plist-get org-gtd-walk--active :model) :entries))
      (assert-equal "a" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))
    ;; re-rendered current "a" again on top of the initial "a"
    (assert-equal '("a" "a") walk-driver-test--render-log)))

(deftest walk-enqueue-top-inserts-next-and-rerenders-current ()
  "enqueue top puts the handle right after the current item, which stays current."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start (walk-driver-test--stub-spec) surface) ; on "a"
    (with-current-buffer surface (org-gtd-walk-enqueue "z" 'top))
    (with-current-buffer surface
      (assert-equal '("a" "z" "b" "c")
                    (plist-get (plist-get org-gtd-walk--active :model) :entries))
      (assert-equal "a" (org-gtd-walk-model-current
                         (plist-get org-gtd-walk--active :model))))
    ;; current "a" re-rendered on top of the initial "a"
    (assert-equal '("a" "a") walk-driver-test--render-log)))

;;; stale-handle skipping

(deftest walk-resolve-skips-stale-handles-on-advance ()
  "A :resolve that rejects \"b\" auto-advances past it to \"c\" and counts the skip."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start
     (walk-driver-test--stub-spec :resolve (lambda (h) (not (equal h "b"))))
     surface) ; renders "a"
    (with-current-buffer surface (org-gtd-walk-advance)) ; would land "b", skips to "c"
    (assert-equal '("c" "a") walk-driver-test--render-log)
    (with-current-buffer surface
      (assert-same 1 (plist-get org-gtd-walk--active :skipped)))))

(deftest walk-resolve-all-stale-finishes ()
  "If every remaining handle is stale, settling finishes the walk."
  (walk-driver-test--with-harness surface
    (org-gtd-walk-start
     (walk-driver-test--stub-spec :resolve (lambda (h) (equal h "a")))
     surface) ; "a" resolves, renders "a"
    (with-current-buffer surface (org-gtd-walk-advance)) ; "b","c" stale -> finish
    (assert-same 1 walk-driver-test--finish-count)
    (with-current-buffer surface (assert-nil org-gtd-walk--active))))

(provide 'walk-driver-test)

;;; walk-driver-test.el ends here
