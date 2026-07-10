;;; view-manager-list-test.el --- Tests for the list transient -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Thin integration tests for the view-manager list transient.  The
;; interactive render/CRUD dispatch is verified manually (see the commit
;; body); here we only assert the transient exists and its keys are wired.
;; A pure test covers the rename-move save path.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest view-manager-list/is-a-transient-prefix ()
  "The manager is defined as a transient prefix command."
  (assert-true (fboundp 'org-gtd-view-manager)))

(deftest view-manager-list/has-create-key ()
  "The manager binds `c' to the create action."
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager "c")))
    (assert-equal "c" (plist-get plist :key))))

(deftest view-manager-list/has-delete-key ()
  "The manager binds `D' to the delete action."
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager "D")))
    (assert-equal "D" (plist-get plist :key))))

(deftest view-manager-list/rename-is-a-move-not-a-copy ()
  "Saving an edited view under a new name removes the old entry.
Editing seeds `--build-original-name'; a save under a changed name
must MOVE (delete the old), not leave an orphan behind."
  (org-gtd-view-manager--store-upsert
   "Old" (list (cons 'name "Old") (cons 'type 'next-action)))
  (org-gtd-view-manager--build-load '((name . "Old") (type . next-action)))
  (setq org-gtd-view-manager--build-original-name "Old")
  (setq org-gtd-view-manager--build-dirty t)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "New"))
            ((symbol-function 'org-gtd-view-manager--build-restore-windows)
             #'ignore))
    (org-gtd-view-manager--save))
  (let ((views (org-gtd-view-manager--store-read)))
    (assert-equal nil (assoc "Old" views))
    (assert-true (assoc "New" views))))

(deftest view-manager-list/edit-save-same-name-no-overwrite-prompt ()
  "Saving an edited view under its OWN name must not prompt to overwrite.
The overwrite guard finds the very view being edited; skip it when the
name equals `--build-original-name' so a plain edit-save is silent."
  (let ((prompted nil))
    (org-gtd-view-manager--store-upsert
     "A" (list (cons 'name "A") (cons 'type 'next-action)))
    (org-gtd-view-manager--build-load '((name . "A") (type . single-action)))
    (setq org-gtd-view-manager--build-original-name "A")
    (setq org-gtd-view-manager--build-dirty t)
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "A"))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _) (setq prompted t) t))
              ((symbol-function 'org-gtd-view-manager--build-restore-windows)
               #'ignore))
      (org-gtd-view-manager--save))
    (assert-equal nil prompted)
    (let ((views (org-gtd-view-manager--store-read)))
      (assert-true (assoc "A" views))
      (assert-equal 'single-action
                    (alist-get 'type (cdr (assoc "A" views)))))))

(deftest view-manager-list/multi-section-save-round-trips-to-blocks ()
  "Saving a two-section view stores a blocks spec that reloads verbatim."
  (org-gtd-view-manager--build-load '((name . "Engage") (type . calendar)))
  (org-gtd-view-manager--build-add-section) ;; section 2 = next-action
  (setf (alist-get 'area-of-focus org-gtd-view-manager--build-state) "Work")
  (setq org-gtd-view-manager--build-original-name "Engage")
  (setq org-gtd-view-manager--build-dirty t)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "Engage"))
            ((symbol-function 'org-gtd-view-manager--build-restore-windows)
             #'ignore))
    (org-gtd-view-manager--save))
  (let ((spec (org-gtd-view-manager--store-get "Engage")))
    (assert-equal 2 (length (cdr (assq 'blocks spec))))
    (assert-equal 'calendar
                  (cdr (assq 'type (nth 0 (cdr (assq 'blocks spec))))))
    (assert-equal "Work"
                  (cdr (assq 'area-of-focus
                             (nth 1 (cdr (assq 'blocks spec))))))))

(deftest view-manager-list/delete-to-one-section-round-trips-to-flat ()
  "Deleting back down to one section saves a FLAT spec, not a blocks spec.
Back-compat guard (design §8): a view built up to two sections and then
pruned to one must persist in the legacy single-section shape -- a
top-level `type' with no `blocks' key -- so a downgrade or an external
reader still sees a plain flat view."
  (org-gtd-view-manager--build-load
   '((name . "V")
     (blocks . (((type . calendar))
                ((type . next-action))))))
  (assert-equal 2 (length org-gtd-view-manager--build-sections))
  (org-gtd-view-manager--build-delete-section)
  (assert-equal 1 (length org-gtd-view-manager--build-sections))
  (setq org-gtd-view-manager--build-original-name "V")
  (setq org-gtd-view-manager--build-dirty t)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "V"))
            ((symbol-function 'org-gtd-view-manager--build-restore-windows)
             #'ignore))
    (org-gtd-view-manager--save))
  (let ((spec (org-gtd-view-manager--store-get "V")))
    (assert-nil (assq 'blocks spec))
    (assert-equal 'next-action (cdr (assq 'type spec)))))

(deftest view-manager-list/ret-action-labeled-open ()
  "The RET action reads as `Open', matching the empty-state hint, not `Render'."
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager "RET")))
    (assert-equal "Open" (plist-get plist :description))))

;;; view-manager-list-test.el ends here
