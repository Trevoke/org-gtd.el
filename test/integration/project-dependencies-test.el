;;; project-dependencies-test.el --- Integration tests for project dependency creation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for default sequential dependencies (Story 7)
;; and custom dependency preservation when organizing projects.
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

;;; Sequential Dependencies (Story 7)

(deftest deps/creates-sequential-dependencies-for-new-tasks ()
  "Creates sequential dependencies for tasks without existing relationships.
Verifies that when a project is created, tasks are linked sequentially:
Task 1 blocks Task 2, Task 2 blocks Task 3, etc."
  (create-project "sequential project")

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "sequential project")

    ;; Find Task 1 - should have no ORG_GTD_DEPENDS_ON but should BLOCK Task 2
    (search-forward "Task 1")
    (org-back-to-heading t)
    (let ((task1-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
          (task1-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (assert-nil task1-depends)  ;; First task depends on nothing
      (assert-equal 1 (length task1-blocks)))  ;; But blocks one task

    ;; Find Task 2 - should DEPEND_ON Task 1 and BLOCK Task 3
    (search-forward "Task 2")
    (org-back-to-heading t)
    (let ((task2-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
          (task2-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (assert-equal 1 (length task2-depends))  ;; Depends on Task 1
      (assert-equal 1 (length task2-blocks)))  ;; Blocks Task 3

    ;; Find Task 3 - should DEPEND_ON Task 2 but block nothing
    (search-forward "Task 3")
    (org-back-to-heading t)
    (let ((task3-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
          (task3-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (assert-equal 1 (length task3-depends))  ;; Depends on Task 2
      (assert-nil task3-blocks))))  ;; Last task blocks nothing

;;; Custom Dependencies Override Defaults (Story 8)

(deftest deps/preserves-existing-dependencies-when-organizing ()
  "Preserves existing dependencies when organizing project.
Tests Story 8: Custom Dependencies Override Defaults.
Create custom dependencies before organizing - they should be preserved
while default sequential dependencies are added for tasks without them."
  (capture-inbox-item "custom dependencies project")
  (org-gtd-process-inbox)

  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    ;; Use builder instead of string fixture
    (make-task "Task 1" :level 2)
    (make-task "Task 2" :level 2)
    (make-task "Task 3" :level 2)

    ;; Add custom dependency: Task 3 depends on Task 1 (skipping Task 2)
    (goto-char (point-min))
    (search-forward "Task 3")
    (org-back-to-heading t)
    (let ((task3-id (org-id-get-create)))
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (let ((task1-id (org-id-get-create)))
        ;; Create custom dependency: Task 3 depends on Task 1
        (goto-char (point-min))
        (search-forward "Task 3")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task1-id)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task3-id)))

    ;; Now organize as project
    (organize-as-project))

  ;; Verify custom dependencies are preserved
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "custom dependencies project")

    ;; Task 3 should still depend on Task 1
    (search-forward "Task 3")
    (org-back-to-heading t)
    (let ((task3-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
      (assert-equal 1 (length task3-depends)))

    ;; Task 1 should block both Task 3 (custom) and Task 2 (default sequential)
    (goto-char (point-min))
    (search-forward "Task 1")
    (org-back-to-heading t)
    (let ((task1-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (assert-equal 2 (length task1-blocks)))

    ;; Task 2 should get default sequential dependency (depends on Task 1)
    ;; since it has no custom dependencies
    (goto-char (point-min))
    (search-forward "Task 2")
    (org-back-to-heading t)
    (let ((task2-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
      (assert-equal 1 (length task2-depends)))))

(provide 'project-dependencies-test)

;;; project-dependencies-test.el ends here
