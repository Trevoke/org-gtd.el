;;; project-tickler-advanced-test.el --- Integration tests for advanced tickler functionality -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for advanced project tickler functionality including
;; external dependency detection and multi-project task handling.
;;
;; Migrated from test/project-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; External Dependencies

(deftest tickler-adv/warns-about-external-dependencies ()
  "Warns about external dependencies before ticklering.
Creates two projects where Task in Project B depends on task in Project A.
Incubating Project A should detect Project B task as external dependency."
  (create-project "Project A")
  (create-project "Project B")

  (with-current-buffer (org-gtd--default-file)
    ;; Get IDs for tasks
    (goto-char (point-min))
    (re-search-forward "Project A")
    (org-next-visible-heading 1)  ; Task 1 of Project A
    (let ((task-a1-id (org-id-get-create)))

      (goto-char (point-min))
      (re-search-forward "Project B")
      (org-next-visible-heading 1)  ; Task 1 of Project B
      (let ((task-b1-id (org-id-get-create)))

        ;; Make Task B1 depend on Task A1 (external dependency)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a1-id)
        (goto-char (point-min))
        (re-search-forward "Project A")
        (org-next-visible-heading 1)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b1-id)

        ;; Check for external dependencies on Project A
        (goto-char (point-min))
        (re-search-forward "Project A")
        (org-back-to-heading t)
        (let ((external-deps (org-gtd-project--check-external-dependencies (point-marker))))
          ;; Should find Task B1 as external dependency
          (assert-equal 1 (length external-deps))
          (assert-equal task-b1-id
                        (org-with-point-at (car external-deps)
                          (org-id-get))))))))

;;; Multi-Project Tasks

(deftest tickler-adv/skips-multi-project-tasks ()
  "Skips multi-project tasks during tickler.
Tasks that belong to multiple projects should not be incubated when
only one project is ticklered."
  (create-project "Project A")
  (create-project "Project B")

  (with-current-buffer (org-gtd--default-file)
    ;; Make Task 1 belong to both projects
    (goto-char (point-min))
    (re-search-forward "Project A")
    (let ((project-a-id (org-id-get-create)))
      (goto-char (point-min))
      (re-search-forward "Project B")
      (let ((project-b-id (org-id-get-create)))

        ;; Add both project IDs to Task 1 of Project A
        (goto-char (point-min))
        (re-search-forward "Project A")
        (org-next-visible-heading 1)  ; Task 1
        (org-entry-put (point) "ORG_GTD_PROJECT_IDS" (format "%s %s" project-a-id project-b-id))
        (let ((task-1-todo (org-entry-get (point) "TODO")))

          ;; Incubate Project A
          (goto-char (point-min))
          (re-search-forward "Project A")
          (org-back-to-heading t)
          (org-gtd-project-incubate (point-marker) "2025-12-01")

          ;; Verify Task 1 was NOT incubated (it belongs to multiple projects)
          (goto-char (point-min))
          (re-search-forward "Project A")
          (org-next-visible-heading 1)
          (assert-nil (equal "Tickler" (org-entry-get (point) "ORG_GTD")))
          (assert-equal task-1-todo (org-entry-get (point) "TODO")))))))

(provide 'project-tickler-advanced-test)

;;; project-tickler-advanced-test.el ends here
