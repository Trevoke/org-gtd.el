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

(deftest view-manager-sample/banner-shows-once-per-session ()
  "With empty agenda-files, the sample banner is messaged once, not per render.
Resetting the session flag re-arms it for the next builder-open."
  (let ((org-agenda-files nil)
        (org-gtd-view-manager--sample-banner-shown nil)
        (banner-count 0))
    (cl-letf (((symbol-function 'org-gtd-view-show) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest _)
                 (when (and (stringp fmt) (string-prefix-p "sample data" fmt))
                   (setq banner-count (1+ banner-count))))))
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action)))
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action)))
      (assert-equal 1 banner-count)
      ;; Re-arm as a fresh builder-open would, and confirm it shows again.
      (setq org-gtd-view-manager--sample-banner-shown nil)
      (org-gtd-view-manager--render-preview '((name . "x") (type . next-action)))
      (assert-equal 2 banner-count))))

(provide 'view-manager-sample-test)
;;; view-manager-sample-test.el ends here
