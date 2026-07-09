;;; view-manager-store-test.el --- Tests for the views.eld store -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the view-manager name -> spec store.
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest view-manager-store/round-trips-specs ()
  "Writing then reading the store preserves specs verbatim."
  (let ((views '(("Weekend errands" . ((name . "Weekend errands")
                                        (type . next-action)
                                        (area-of-focus . "Home")
                                        (effort . (< "30m"))))
                 ("Waiting on Sam" . ((name . "Waiting on Sam")
                                      (type . delegated)
                                      (who . "Sam"))))))
    (org-gtd-view-manager--store-write views)
    (assert-equal views (org-gtd-view-manager--store-read))))

(deftest view-manager-store/missing-file-reads-empty ()
  "Reading before any write returns nil (empty store), creating the file."
  (assert-nil (org-gtd-view-manager--store-read))
  (assert-true (f-exists-p (org-gtd-view-manager--store-path))))

(deftest view-manager-store/header-comment-present ()
  "The lazily created store carries a guidance-comment header."
  (org-gtd-view-manager--store-read)
  (assert-match "Managed by org-gtd"
                (f-read-text (org-gtd-view-manager--store-path))))

(deftest view-manager-store/truncated-store-reports-and-yields-nil ()
  "A truncated store returns nil AND emits a message (fail-soft)."
  (f-write-text ";; header\n((\"a\" . ((type" 'utf-8
                (org-gtd-view-manager--store-path))
  (let ((captured nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt (push (apply #'format fmt args) captured))
                 nil)))
      (assert-nil (org-gtd-view-manager--store-read)))
    (assert-true (seq-some (lambda (m) (string-match-p "views store" m))
                           captured))))

(deftest view-manager-store/upsert-adds-and-replaces ()
  "Upsert adds a new view and replaces an existing one by name."
  (org-gtd-view-manager--store-upsert "A" '((name . "A") (type . next-action)))
  (assert-equal '((name . "A") (type . next-action))
                (org-gtd-view-manager--store-get "A"))
  (org-gtd-view-manager--store-upsert "A" '((name . "A") (type . delegated)))
  (assert-equal '((name . "A") (type . delegated))
                (org-gtd-view-manager--store-get "A"))
  (assert-equal 1 (length (org-gtd-view-manager--store-read))))

(deftest view-manager-store/delete-removes-entry ()
  "Delete removes a named view; getting it then returns nil."
  (org-gtd-view-manager--store-upsert "A" '((name . "A")))
  (org-gtd-view-manager--store-delete "A")
  (assert-nil (org-gtd-view-manager--store-get "A")))

(provide 'view-manager-store-test)
;;; view-manager-store-test.el ends here
