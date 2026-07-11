;;; view-manager-run-test.el --- Tests for org-gtd-view-run recall -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for `org-gtd-view-run', the `completing-read' recall command
;; that renders a saved view by name via `org-gtd-view-show'.
;;
;;; Code:

(require 'cl-lib)
(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest view-manager-run/renders-selected-view ()
  "Selecting a saved name calls org-gtd-view-show with its spec."
  (org-gtd-view-manager--store-upsert
   "Errands" '((name . "Errands") (type . next-action)))
  (let (captured)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (spec &rest _) (setq captured spec))))
      (with-simulated-input "Errands RET"
        (org-gtd-view-run)))
    (assert-equal '((name . "Errands") (type . next-action)) captured)))

(deftest view-manager-run/empty-store-teaches ()
  "With no saved views, a teaching user-error fires, not a crash."
  (let ((msg (condition-case err
                 (progn (org-gtd-view-run) nil)
               (user-error (error-message-string err)))))
    (assert-true (and msg (string-match-p "No saved views" msg)))))

(deftest view-manager-run/uses-annotated-picker ()
  "Recall routes through the shared annotated picker helper."
  (org-gtd-view-manager--store-upsert
   "Errands" '((name . "Errands") (type . next-action)))
  (let (picked-views)
    (cl-letf (((symbol-function 'org-gtd-view-manager--pick-view)
               (lambda (views &rest _) (setq picked-views views) "Errands"))
              ((symbol-function 'org-gtd-view-show) #'ignore))
      (org-gtd-view-run))
    (assert-true (assoc "Errands" picked-views))))

(provide 'view-manager-run-test)
;;; view-manager-run-test.el ends here
