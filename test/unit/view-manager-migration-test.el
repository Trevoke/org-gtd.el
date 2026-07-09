;;; view-manager-migration-test.el --- Tests for legacy view migration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the one-time, fail-soft import of the legacy
;; `org-gtd-reflect-missed-custom-views' defcustom into the views store.
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest view-manager-migration/flattens-nested-filters ()
  "A nested (filters . (...)) entry imports to a flat, editable spec."
  (let ((org-gtd-reflect-missed-custom-views
         '(((name . "My Custom View")
            (filters . ((type . delegated) (area-of-focus . "Work")))))))
    (org-gtd-view-manager--migrate)
    (assert-equal '((name . "My Custom View")
                    (type . delegated)
                    (area-of-focus . "Work"))
                  (org-gtd-view-manager--store-get "My Custom View"))))

(deftest view-manager-migration/already-flat-entry-imports ()
  "An entry already flat imports unchanged."
  (let ((org-gtd-reflect-missed-custom-views
         '(((name . "Flat") (type . next-action)))))
    (org-gtd-view-manager--migrate)
    (assert-equal '((name . "Flat") (type . next-action))
                  (org-gtd-view-manager--store-get "Flat"))))

(deftest view-manager-migration/bad-entry-skipped-not-fatal ()
  "An entry with an unknown key is skipped; good entries still import."
  (let ((org-gtd-reflect-missed-custom-views
         '(((name . "Bad") (filters . ((bogus-key . 1))))
           ((name . "Good") (type . next-action)))))
    (org-gtd-view-manager--migrate)
    (assert-nil (org-gtd-view-manager--store-get "Bad"))
    (assert-equal '((name . "Good") (type . next-action))
                  (org-gtd-view-manager--store-get "Good"))))

(provide 'view-manager-migration-test)
;;; view-manager-migration-test.el ends here
