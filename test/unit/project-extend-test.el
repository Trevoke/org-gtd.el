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

(deftest project-extend/apply-sets-project-ids-before-refile ()
  "The new flow sets ORG_GTD_PROJECT_IDS explicitly before any refile."
  (let ((project-ids-set-at nil)
        (refile-called-at nil)
        (first-tasks-updated-at nil)
        (call-order 0))
    (with-temp-buffer
      (org-mode)
      (insert "* Project\n:PROPERTIES:\n:ID: fake-project-id\n:END:\n")
      (insert "* Task\n")
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (let ((fake-marker (point-marker)))
        ;; Track when ORG_GTD_PROJECT_IDS is set vs when refile happens
        (cl-letf (((symbol-function 'org-gtd-project-extend--select-project)
                   (lambda ()
                     (cons "fake-project-id" fake-marker)))
                  ((symbol-function 'org-gtd-project--configure-single-task)
                   #'ignore)
                  ((symbol-function 'org-entry-put)
                   (lambda (pom prop val)
                     (when (string= prop org-gtd-prop-project-ids)
                       (setq project-ids-set-at (cl-incf call-order)))))
                  ((symbol-function 'org-entry-add-to-multivalued-property)
                   (lambda (pom prop val)
                     (when (string= prop "ORG_GTD_FIRST_TASKS")
                       (setq first-tasks-updated-at (cl-incf call-order)))))
                  ((symbol-function 'org-gtd-project-extend--move-to-project)
                   (lambda (_)
                     (setq refile-called-at (cl-incf call-order))))
                  ((symbol-function 'org-gtd-refile--with-user-targets)
                   (lambda ()
                     (setq refile-called-at (cl-incf call-order))))
                  ((symbol-function 'org-gtd-projects-fix-todo-keywords)
                   #'ignore)
                  ((symbol-function 'org-id-get-create)
                   (lambda () "fake-task-id"))
                  (org-gtd-clarify--skip-refile nil)
                  (org-gtd-refile-prompt-for-types nil))
          (goto-char (point-min))
          (org-next-visible-heading 1)
          (org-next-visible-heading 1)  ; Move to task
          (org-gtd-project-extend--apply))))
    ;; Verify the correct order: project-ids, first-tasks, then refile
    (assert-true project-ids-set-at)
    (assert-true first-tasks-updated-at)
    (assert-true refile-called-at)
    (assert-true (< project-ids-set-at first-tasks-updated-at))
    (assert-true (< first-tasks-updated-at refile-called-at))))

(deftest project-extend/apply-respects-skip-refile ()
  "When skip-refile is set, task stays in place with correct properties."
  (let ((refile-called nil))
    (cl-letf (((symbol-function 'org-gtd-project-extend--select-project)
               (lambda ()
                 (cons "fake-project-id" (point-marker))))
              ((symbol-function 'org-gtd-project--configure-single-task)
               #'ignore)
              ((symbol-function 'org-entry-put)
               #'ignore)
              ((symbol-function 'org-gtd-projects--set-project-name-on-task)
               #'ignore)
              ((symbol-function 'org-entry-add-to-multivalued-property)
               #'ignore)
              ((symbol-function 'org-gtd-project-extend--move-to-project)
               (lambda (_) (setq refile-called t)))
              ((symbol-function 'org-gtd-projects-fix-todo-keywords)
               #'ignore)
              ((symbol-function 'org-id-get-create)
               (lambda () "fake-task-id"))
              ((symbol-function 'org-with-point-at)
               (lambda (marker &rest body) nil))
              (org-gtd-clarify--skip-refile t))  ; Skip refile enabled
      (with-temp-buffer
        (org-mode)
        (insert "* Task\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd-project-extend--apply)))
    ;; Refile should NOT have been called
    (assert-nil refile-called)))

(provide 'project-extend-test)
;;; project-extend-test.el ends here
