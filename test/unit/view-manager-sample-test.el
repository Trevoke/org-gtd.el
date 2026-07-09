;;; view-manager-sample-test.el --- Tests for sample-data preview -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the view-manager preview's empty-agenda -> sample-data path.
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'cl-lib)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest view-manager-sample/uses-sample-when-agenda-empty ()
  "With empty org-agenda-files, the preview binds the sample file."
  (let ((org-agenda-files nil) captured)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (&rest _) (setq captured org-agenda-files))))
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action))))
    (assert-true (and captured (cl-every #'stringp captured)))))

(deftest view-manager-sample/keeps-real-files-when-present ()
  "With real agenda files, the preview does not swap in the sample."
  (let ((org-agenda-files (list (org-gtd--path org-gtd-default-file-name)))
        captured)
    (cl-letf (((symbol-function 'org-gtd-view-show)
               (lambda (&rest _) (setq captured org-agenda-files))))
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action))))
    (assert-equal (list (org-gtd--path org-gtd-default-file-name)) captured)))

(provide 'view-manager-sample-test)
;;; view-manager-sample-test.el ends here
