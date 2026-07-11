;;; view-manager-act-test.el --- Tests for the per-view action dispatch -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the select-then-act dispatch (`--act-open/edit/new/copy/
;; delete') scoped to `org-gtd-view-manager--selected'.
;;
;;; Code:

(require 'cl-lib)
(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defun ogt--seed-view (name spec)
  (org-gtd-view-manager--store-upsert name spec)
  (setq org-gtd-view-manager--selected name))

(deftest view-manager-act/open-shows-selected-spec ()
  "Open renders the selected view's stored spec."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (let (captured)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (spec &rest _) (setq captured spec))))
      (org-gtd-view-manager--act-open))
    (assert-equal 'next-action (alist-get 'type captured))))

(deftest view-manager-act/edit-builds-selected-spec ()
  "Edit opens the builder on the selected spec."
  (ogt--seed-view "E" '((name . "E") (type . project)))
  (let (captured)
    (cl-letf (((symbol-function 'org-gtd-view-manager--build)
               (lambda (&optional spec &rest _) (setq captured (or spec 'none)))))
      (org-gtd-view-manager--act-edit))
    (assert-equal 'project (alist-get 'type captured))))

(deftest view-manager-act/new-builds-fresh-ignoring-selection ()
  "New opens the builder with NO starting spec, even with a selection."
  (ogt--seed-view "E" '((name . "E") (type . project)))
  (let ((captured 'unset))
    (cl-letf (((symbol-function 'org-gtd-view-manager--build)
               (lambda (&optional spec &rest _) (setq captured spec))))
      (org-gtd-view-manager--act-new))
    (assert-nil captured)))

(deftest view-manager-act/copy-builds-named-copy ()
  "Copy opens the builder on a `<name> copy' spec."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (let (captured)
    (cl-letf (((symbol-function 'org-gtd-view-manager--build)
               (lambda (&optional spec &rest _) (setq captured spec))))
      (org-gtd-view-manager--act-copy))
    (assert-equal "E copy" (alist-get 'name captured))))

(deftest view-manager-act/delete-removes-and-repicks ()
  "Delete (confirmed) removes the view, then re-enters the manager."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (org-gtd-view-manager--store-upsert "F" '((name . "F") (type . project)))
  (let (repicked)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'org-gtd-view-manager)
               (lambda (&rest _) (setq repicked t))))
      (org-gtd-view-manager--act-delete))
    (assert-nil (assoc "E" (org-gtd-view-manager--store-read)))
    (assert-true repicked)))

(deftest view-manager-act/delete-last-messages-no-repick ()
  "Deleting the only view messages cleanly and does NOT re-pick or build."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (let (repicked built)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'org-gtd-view-manager)
               (lambda (&rest _) (setq repicked t)))
              ((symbol-function 'org-gtd-view-manager--build)
               (lambda (&rest _) (setq built t))))
      (org-gtd-view-manager--act-delete))
    (assert-nil (assoc "E" (org-gtd-view-manager--store-read)))
    (assert-nil repicked)
    (assert-nil built)))

(deftest view-manager-act/delete-declined-keeps-view ()
  "Declining the confirm leaves the view in the store."
  (ogt--seed-view "E" '((name . "E") (type . next-action)))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
    (org-gtd-view-manager--act-delete))
  (assert-true (assoc "E" (org-gtd-view-manager--store-read))))

(deftest view-manager-act/open-nil-selection-is-noop ()
  "With no selection, Open does nothing (no broken agenda render)."
  (setq org-gtd-view-manager--selected nil)
  (let (called)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (&rest _) (setq called t))))
      (org-gtd-view-manager--act-open))
    (assert-nil called)))

(deftest view-manager-act/edit-nil-selection-is-noop ()
  "With no selection, Edit does not open the builder."
  (setq org-gtd-view-manager--selected nil)
  (let (called)
    (cl-letf (((symbol-function 'org-gtd-view-manager--build)
               (lambda (&rest _) (setq called t))))
      (org-gtd-view-manager--act-edit))
    (assert-nil called)))

(deftest view-manager-act/copy-nil-selection-is-noop ()
  "With no selection, Copy does not open the builder."
  (setq org-gtd-view-manager--selected nil)
  (let (called)
    (cl-letf (((symbol-function 'org-gtd-view-manager--build)
               (lambda (&rest _) (setq called t))))
      (org-gtd-view-manager--act-copy))
    (assert-nil called)))

(deftest view-manager-act/transient-exists ()
  "The action transient prefix is defined."
  (assert-true (fboundp 'org-gtd-view-manager--act)))

(deftest view-manager-act/transient-keys ()
  "The action transient binds o/e/n/c/d/q."
  (dolist (key '("o" "e" "n" "c" "d" "q"))
    (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager--act key)))
      (assert-equal key (plist-get plist :key)))))

(deftest view-manager-act/description-shows-selection ()
  "The transient description names the selected view and its badge."
  (org-gtd-view-manager--store-upsert
   "E" '((name . "E") (type . next-action) (area-of-focus . "Home")))
  (setq org-gtd-view-manager--selected "E")
  (let ((desc (org-gtd-view-manager--act-description)))
    (assert-true (string-match-p "E" desc))
    (assert-true (string-match-p "next-action · Home" desc))))

(provide 'view-manager-act-test)
;;; view-manager-act-test.el ends here
