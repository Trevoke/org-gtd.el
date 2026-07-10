;;; view-manager-badge-test.el --- Tests for the spec badge/summary -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the view-manager badge/summary formatter.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-view-manager)

;; Initialize e-unit short syntax
(e-unit-initialize)

(deftest view-manager-badge/next-action-area-effort ()
  (assert-equal "next-action · Home · <30m"
                (org-gtd-view-manager--badge
                 '((name . "Weekend errands") (type . next-action)
                   (area-of-focus . "Home") (effort . (< "30m"))))))

(deftest view-manager-badge/delegated-who ()
  (assert-equal "delegated · who=Sam"
                (org-gtd-view-manager--badge
                 '((name . "x") (type . delegated) (who . "Sam")))))

(deftest view-manager-badge/project-not-done ()
  (assert-equal "project · not-done"
                (org-gtd-view-manager--badge
                 '((name . "x") (type . project) (not-done . t)))))

(deftest view-manager-badge/not-habit ()
  "The not-habit flag renders as its key name, like not-done."
  (assert-equal "next-action · not-habit"
                (org-gtd-view-manager--badge
                 '((name . "x") (type . next-action) (not-habit . t)))))

(deftest view-manager-badge/name-only-is-not-shown ()
  "The name is the row label, not part of the badge."
  (assert-equal "next-action"
                (org-gtd-view-manager--badge
                 '((name . "Anything") (type . next-action)))))

(deftest view-manager-badge/order-follows-table-not-input ()
  "Badge order follows filter-spec declaration order, not input order."
  (assert-equal "next-action · Home · <30m"
                (org-gtd-view-manager--badge
                 '((effort . (< "30m")) (area-of-focus . "Home")
                   (name . "x") (type . next-action)))))

(deftest view-manager-badge/blocks-spec-summarized ()
  "A blocks spec summarizes as `N sections: b0 · b1 · …', not an empty badge."
  (assert-equal
   "2 sections: calendar · next-action · Work"
   (org-gtd-view-manager--badge
    '((name . "Engage")
      (blocks . (((type . calendar))
                 ((type . next-action) (area-of-focus . "Work"))))))))

(deftest view-manager-badge/flat-spec-unchanged ()
  "A flat spec still badges via the per-section formatter."
  (assert-equal "next-action · Home"
                (org-gtd-view-manager--badge
                 '((name . "x") (type . next-action)
                   (area-of-focus . "Home")))))

(provide 'view-manager-badge-test)
;;; view-manager-badge-test.el ends here
