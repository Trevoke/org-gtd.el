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

(provide 'view-manager-badge-test)
;;; view-manager-badge-test.el ends here
