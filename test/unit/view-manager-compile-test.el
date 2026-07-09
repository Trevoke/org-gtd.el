;;; view-manager-compile-test.el --- Tests for compiling builder state -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for `org-gtd-view-manager--compile', which turns the builder's
;; key -> value state alist into a flat view spec `org-gtd-view-show' accepts.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-view-manager)

;; Initialize e-unit short syntax
(e-unit-initialize)

(deftest view-manager-compile/omits-unset-keys ()
  "Unset keys are absent from the compiled spec, not nil."
  (let ((spec (org-gtd-view-manager--compile-section
               '((name . "x") (type . next-action)))))
    (assert-nil (assq 'who spec))
    (assert-nil (assq 'effort spec))
    (assert-equal 'next-action (cdr (assq 'type spec)))))

(deftest view-manager-compile/effort-shape ()
  "Effort compiles to a comparison list like (< \"30m\")."
  (let ((spec (org-gtd-view-manager--compile-section
               '((name . "x") (type . next-action) (effort . (< "30m"))))))
    (assert-equal '(< "30m") (cdr (assq 'effort spec)))))

(deftest view-manager-compile/prefix-chain-shape ()
  "Prefix compiles to a fallback chain list, not a string."
  (let ((spec (org-gtd-view-manager--compile-section
               '((name . "x") (type . next-action)
                 (prefix . (project area-of-focus "—"))
                 (prefix-width . 12)))))
    (assert-equal '(project area-of-focus "—") (cdr (assq 'prefix spec)))
    (assert-equal 12 (cdr (assq 'prefix-width spec)))))

(deftest view-manager-compile/drops-nil-values ()
  "A key explicitly set to nil is dropped (treated as unset)."
  (let ((spec (org-gtd-view-manager--compile-section
               '((name . "x") (type . next-action) (who . nil)))))
    (assert-nil (assq 'who spec))))

(deftest view-manager-compile/drops-unknown-keys ()
  "A key not in the filter-spec allow-list is dropped from the compiled spec."
  (let ((spec (org-gtd-view-manager--compile-section
               '((name . "x") (type . next-action) (bogus-key . 1)))))
    (assert-nil (assq 'bogus-key spec))
    (assert-equal 'next-action (cdr (assq 'type spec)))))

(deftest view-manager-compile-section/drops-name ()
  "A section spec carries no name (the view name lives at view level)."
  (let ((sec (org-gtd-view-manager--compile-section
              '((name . "x") (type . next-action)))))
    (assert-nil (assq 'name sec))
    (assert-equal 'next-action (cdr (assq 'type sec)))))

(deftest view-manager-compile-view/one-section-is-flat ()
  "One section compiles to a FLAT spec (name at top, no `blocks')."
  (let ((spec (org-gtd-view-manager--compile-view
               "My View" '(((type . next-action) (area-of-focus . "Work"))))))
    (assert-equal "My View" (cdr (assq 'name spec)))
    (assert-nil (assq 'blocks spec))
    (assert-equal 'next-action (cdr (assq 'type spec)))
    (assert-equal "Work" (cdr (assq 'area-of-focus spec)))))

(deftest view-manager-compile-view/many-sections-use-blocks ()
  "Two+ sections compile to a `((name) (blocks (S0 S1 …)))' spec."
  (let* ((spec (org-gtd-view-manager--compile-view
                "Engage"
                '(((type . calendar))
                  ((type . next-action) (area-of-focus . "Work"))
                  ((type . delegated)))))
         (blocks (cdr (assq 'blocks spec))))
    (assert-equal "Engage" (cdr (assq 'name spec)))
    (assert-equal 3 (length blocks))
    (assert-equal 'calendar (cdr (assq 'type (nth 0 blocks))))
    (assert-equal "Work" (cdr (assq 'area-of-focus (nth 1 blocks))))
    (assert-equal 'delegated (cdr (assq 'type (nth 2 blocks))))
    ;; No section carries a name.
    (assert-nil (assq 'name (nth 0 blocks)))))

(provide 'view-manager-compile-test)
;;; view-manager-compile-test.el ends here
