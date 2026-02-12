;;; project-someday-test.el --- Unit tests for project someday -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025, 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for project someday functionality including state preservation
;; and reactivation.

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

;;; Project Someday

(deftest project-someday/sets-heading-and-tasks-to-someday ()
  "Someday-ing a project sets ORG_GTD to Someday on heading and all tasks."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker)))

      ;; Someday the project
      (org-gtd-project-someday project-marker)

      ;; Verify project heading
      (org-with-point-at project-marker
        (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))
        (assert-equal "Projects" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
        (assert-nil (org-entry-get (point) "ORG_GTD_TIMESTAMP")))

      ;; Verify tasks
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))
      (assert-equal "Actions" (org-entry-get (point) "PREVIOUS_ORG_GTD"))
      (assert-nil (org-entry-get (point) "TODO"))
      (assert-equal "NEXT" (org-entry-get (point) "PREVIOUS_TODO")))))

(deftest project-someday/reactivates-from-someday ()
  "Reactivating a someday project restores all states."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker)))

      ;; Someday then reactivate
      (org-gtd-project-someday project-marker)
      (org-gtd-project-reactivate project-marker)

      ;; Verify project heading restored
      (org-with-point-at project-marker
        (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))
        (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD")))

      ;; Verify tasks restored
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
      (assert-nil (org-entry-get (point) "PREVIOUS_ORG_GTD"))
      (assert-true (org-entry-get (point) "TODO")))))

(deftest project-someday/sets-someday-list-on-heading ()
  "Someday-ing a project with lists configured sets ORG_GTD_SOMEDAY_LIST on heading."
  (let ((org-gtd-someday-lists '("Work Ideas" "Personal")))
    (create-project "Test project")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Test project")
      (org-back-to-heading t)
      (let ((project-marker (point-marker)))

        ;; Someday with list selection
        (with-simulated-input "Work SPC Ideas TAB RET"
          (org-gtd-project-someday project-marker))

        ;; Verify list is set on heading
        (org-with-point-at project-marker
          (assert-equal "Work Ideas" (org-entry-get (point) "ORG_GTD_SOMEDAY_LIST")))

        ;; Verify list is NOT set on tasks
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (assert-nil (org-entry-get (point) "ORG_GTD_SOMEDAY_LIST"))))))

(deftest project-someday/checks-external-dependencies ()
  "Someday-ing a project with no external deps succeeds without prompting."
  (create-project "Test project")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Test project")
    (org-back-to-heading t)
    (let ((project-marker (point-marker)))
      ;; No external deps, so it should succeed without prompting
      (org-gtd-project-someday project-marker)
      (org-with-point-at project-marker
        (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))))))

;;; Multi-Project Tasks

(deftest project-someday/skips-multi-project-task-when-other-active ()
  "Skips multi-project task when another project is still active."
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
        (let ((task-1-todo (org-entry-get (point) "TODO")))
          ;; Someday Project A (Project B still active)
          (goto-char (point-min))
          (search-forward "Project A")
          (org-back-to-heading t)
          (org-gtd-project-someday (point-marker))
          ;; Task 1 should NOT be someday'd
          (goto-char (point-min))
          (search-forward "Project A")
          (org-next-visible-heading 1)
          (assert-nil (equal "Someday" (org-entry-get (point) "ORG_GTD")))
          (assert-equal task-1-todo (org-entry-get (point) "TODO")))))))

(deftest project-someday/somedays-multi-project-task-when-last-active ()
  "Somedays multi-project task when this is the last active project."
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
        ;; Tickler Project B first
        (goto-char (point-min))
        (search-forward "Project B")
        (org-back-to-heading t)
        (org-gtd-project-incubate (point-marker) "2025-12-01")
        ;; Now someday Project A (last active project)
        (goto-char (point-min))
        (search-forward "Project A")
        (org-back-to-heading t)
        (org-gtd-project-someday (point-marker))
        ;; Task 1 SHOULD be someday'd
        (goto-char (point-min))
        (search-forward "Project A")
        (org-next-visible-heading 1)
        (assert-equal "Someday" (org-entry-get (point) "ORG_GTD"))
        (assert-nil (org-entry-get (point) "TODO"))))))

(provide 'project-someday-test)

;;; project-someday-test.el ends here
