;;; view-manager-filter-specs-test.el --- Tests for the filter-spec table -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the view-manager filter-spec metadata table.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-view-manager)

;; Initialize e-unit short syntax
(e-unit-initialize)

(deftest view-manager-filter-specs/keys-are-known-dsl-keys ()
  "Every key in the filter-spec table is a known DSL filter key."
  (dolist (entry org-gtd-view-manager--filter-specs)
    (assert-true (memq (car entry) org-gtd-view-lang--known-filter-keys))))

(deftest view-manager-filter-specs/excludes-structural-keys ()
  "The table never surfaces reserved structural keys as infixes."
  (dolist (structural '(view-type block-type group-by native filters
                        additional-blocks agenda-span show-habits
                        group-contexts prefix-format blocks))
    (assert-nil (assq structural org-gtd-view-manager--filter-specs))))

(deftest view-manager-filter-specs/not-habit-is-a-flag-infix ()
  "not-habit is exposed as a structural flag infix (key H), like not-done."
  (let ((entry (assq 'not-habit org-gtd-view-manager--filter-specs)))
    (assert-true entry)
    (assert-equal "H" (plist-get (cdr entry) :key))
    (assert-equal 'structural (plist-get (cdr entry) :group))))

(deftest view-manager-filter-specs/covers-five-groups ()
  "The five handoff groups are all represented."
  (let ((groups (delete-dups
                 (mapcar (lambda (e) (plist-get (cdr e) :group))
                         org-gtd-view-manager--filter-specs))))
    (dolist (g '(type time structural metadata prefix))
      (assert-true (memq g groups)))))

(deftest view-manager-filter-specs/type-candidates-from-dsl-constants ()
  "Type candidates come from the DSL simple+complex type constants."
  (let ((cands (org-gtd-view-manager--type-candidates)))
    (assert-true (memq 'next-action cands))
    (assert-true (memq 'stuck-project cands))
    (assert-true (memq 'quick-action cands))
    (assert-true (memq 'tickler cands))
    (assert-true (memq 'trash cands))))

(deftest view-manager-filter-specs/keys-are-unique ()
  "No two filter-spec entries share an infix letter."
  (let ((keys (mapcar (lambda (e) (plist-get (cdr e) :key))
                      org-gtd-view-manager--filter-specs)))
    (assert-equal (length keys) (length (delete-dups (copy-sequence keys))))))

(provide 'view-manager-filter-specs-test)
;;; view-manager-filter-specs-test.el ends here
