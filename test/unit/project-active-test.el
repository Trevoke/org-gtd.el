;;; project-active-test.el --- Tests for project active status helpers -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-project--active-p and
;; org-gtd-project--task-last-active-project-p helpers.

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; org-gtd-project--active-p

(deftest project-active/active-project-returns-t ()
  "An active project (ORG_GTD=Projects, not done) returns t."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create)))
      (assert-true (org-gtd-project--active-p project-id)))))

(deftest project-active/ticklered-project-returns-nil ()
  "A ticklered project (ORG_GTD=Tickler) returns nil."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create)))
      (org-gtd-project-incubate (point-marker) "2025-12-01")
      (assert-nil (org-gtd-project--active-p project-id)))))

(deftest project-active/someday-project-returns-nil ()
  "A someday project (ORG_GTD=Someday) returns nil."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create)))
      (org-gtd-project-someday (point-marker))
      (assert-nil (org-gtd-project--active-p project-id)))))

(deftest project-active/done-project-returns-nil ()
  "A done project (TODO=DONE) returns nil."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create)))
      (org-todo "DONE")
      (assert-nil (org-gtd-project--active-p project-id)))))

(deftest project-active/cancelled-project-returns-nil ()
  "A cancelled project (TODO=CNCL) returns nil."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create)))
      (org-todo "CNCL")
      (assert-nil (org-gtd-project--active-p project-id)))))

;;; org-gtd-project--task-last-active-project-p

(deftest project-active/single-project-task-is-last-active ()
  "A task belonging to only one project always returns t."
  (create-project "Project A")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-a-id (org-id-get-create)))
      (org-next-visible-heading 1)  ; Task 1
      (assert-true (org-gtd-project--task-last-active-project-p
                    (point-marker) project-a-id)))))

(deftest project-active/multi-project-task-not-last-when-other-active ()
  "A multi-project task returns nil when another project is still active."
  (create-project "Project A")
  (create-project "Project B")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-a-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Project B")
      (org-back-to-heading t)
      (let ((project-b-id (org-id-get-create)))
        ;; Make Task 1 of Project A belong to both projects
        (goto-char (point-min))
        (search-forward "Project A")
        (org-next-visible-heading 1)  ; Task 1
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS"
                       (format "%s %s" project-a-id project-b-id))
        ;; Project B is still active, so this is NOT the last active project
        (assert-nil (org-gtd-project--task-last-active-project-p
                     (point-marker) project-a-id))))))

(deftest project-active/multi-project-task-is-last-when-other-ticklered ()
  "A multi-project task returns t when the other project is ticklered."
  (create-project "Project A")
  (create-project "Project B")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project A")
    (org-back-to-heading t)
    (let ((project-a-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Project B")
      (org-back-to-heading t)
      (let ((project-b-id (org-id-get-create)))
        ;; Make Task 1 of Project A belong to both projects
        (goto-char (point-min))
        (search-forward "Project A")
        (org-next-visible-heading 1)
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS"
                       (format "%s %s" project-a-id project-b-id))
        ;; Tickler Project B
        (goto-char (point-min))
        (search-forward "Project B")
        (org-back-to-heading t)
        (org-gtd-project-incubate (point-marker) "2025-12-01")
        ;; Now Project A is the last active project for this task
        (goto-char (point-min))
        (search-forward "Project A")
        (org-next-visible-heading 1)
        (assert-true (org-gtd-project--task-last-active-project-p
                      (point-marker) project-a-id))))))

(provide 'project-active-test)

;;; project-active-test.el ends here
