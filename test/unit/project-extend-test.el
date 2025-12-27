;;; project-extend-test.el --- Tests for add-to-project flow -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the add-to-project (project-extend) flow.
;; Tests the new flow: select project first, set ORG_GTD_PROJECT_IDS explicitly.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-projects)

;; Initialize e-unit short syntax
(e-unit-initialize)

(deftest project-extend/select-project-returns-id-and-marker ()
  "Selection helper returns cons of (project-id . project-marker)."
  (with-temp-buffer
    (org-mode)
    (insert "* Test Project\n:PROPERTIES:\n:ORG_GTD: Projects\n:ID: fake-project-id\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((project-id "fake-project-id")
           (expected-marker (point-marker)))
      ;; Mock completing-read to return "Test Project"
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Test Project"))
                ((symbol-function 'org-map-entries)
                 (lambda (fn match scope)
                   (when (string= match "+ORG_GTD=\"Projects\"")
                     (save-excursion
                       (goto-char (point-min))
                       (org-next-visible-heading 1)
                       (funcall fn)))))
                ((symbol-function 'org-id-find)
                 (lambda (id &optional markerp)
                   (when (and markerp (string= id "fake-project-id"))
                     expected-marker))))
        (let ((result (org-gtd-project-extend--select-project)))
          (assert-equal project-id (car result))
          (assert-true (markerp (cdr result))))))))

(deftest project-extend/move-to-project-refiles-under-project ()
  "Move helper calls org-refile with correct rfloc structure."
  (let ((refile-args nil))
    (cl-letf (((symbol-function 'org-refile)
               (lambda (&rest args) (setq refile-args args))))
      (with-temp-buffer
        (org-mode)
        (set-visited-file-name "/tmp/test-project.org" t)
        (insert "* Project A\n:PROPERTIES:\n:ORG_GTD: Projects\n:END:\n")
        (insert "* Task to move\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (let ((project-marker (point-marker))
              (project-pos (point)))
          ;; Go to task
          (org-next-visible-heading 1)
          (assert-equal "Task to move" (org-get-heading t t t t))
          ;; Move it
          (org-gtd-project-extend--move-to-project project-marker)
          ;; Verify org-refile was called with correct rfloc
          (assert-true refile-args)
          (let ((rfloc (nth 2 refile-args)))
            (assert-equal 4 (length rfloc))
            (assert-equal "/tmp/test-project.org" (nth 1 rfloc))
            (assert-equal project-pos (nth 3 rfloc))))))
    ;; Clean up
    (when (file-exists-p "/tmp/test-project.org")
      (delete-file "/tmp/test-project.org"))))

(provide 'project-extend-test)
;;; project-extend-test.el ends here
