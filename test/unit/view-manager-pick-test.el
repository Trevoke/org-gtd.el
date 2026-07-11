;;; view-manager-pick-test.el --- Tests for the annotated view picker -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for `org-gtd-view-manager--annotate-view' and `--pick-view':
;; the shared completing-read that annotates each saved view with its badge.
;;
;;; Code:

(require 'cl-lib)
(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest view-manager-pick/annotate-shows-badge ()
  "The annotation is the view's badge, dimmed, with a leading gap."
  (let* ((views '(("Errands" . ((name . "Errands") (type . next-action)
                                (area-of-focus . "Home")))))
         (s (org-gtd-view-manager--annotate-view "Errands" views)))
    (assert-true (string-match-p "next-action · Home" s))
    (assert-equal 'completions-annotations
                  (get-text-property (1- (length s)) 'face s))))

(deftest view-manager-pick/annotate-summarizes-sections ()
  "A multi-section view annotates with the `N sections: …' badge."
  (let* ((views '(("Engage" . ((name . "Engage")
                               (blocks . (((type . calendar))
                                          ((type . next-action))))))))
         (s (org-gtd-view-manager--annotate-view "Engage" views)))
    (assert-true (string-match-p "2 sections: calendar · next-action" s))))

(deftest view-manager-pick/table-exposes-annotation-metadata ()
  "The completion table advertises an annotation-function and category."
  (let* ((views '(("A" . ((name . "A") (type . next-action)))))
         (table (org-gtd-view-manager--completion-table views))
         (meta (funcall table "" nil 'metadata))
         (md (cdr meta)))
    (assert-equal 'org-gtd-view (cdr (assq 'category md)))
    (assert-true (functionp (cdr (assq 'annotation-function md))))
    (assert-true (string-match-p
                  "next-action"
                  (funcall (cdr (assq 'annotation-function md)) "A")))))

(deftest view-manager-pick/pick-view-returns-selected-name ()
  "`--pick-view' returns the chosen name from the annotated completion."
  (let ((views '(("A" . ((name . "A") (type . next-action)))
                 ("B" . ((name . "B") (type . project))))))
    (assert-equal "B"
                  (with-simulated-input "B RET"
                    (org-gtd-view-manager--pick-view views)))))

(provide 'view-manager-pick-test)
;;; view-manager-pick-test.el ends here
