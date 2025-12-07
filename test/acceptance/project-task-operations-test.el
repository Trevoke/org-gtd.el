;;; project-task-operations-test.el --- Acceptance tests for project task operations -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for advanced project task operations including:
;; - Creating blocker/dependency relationships between tasks
;; - Removing blocker relationships
;; - Adding first tasks to existing projects
;;
;; Migrated from test/end-to-end-test.el (buttercup) to e-unit with mock-fs.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Add Blocker Relationship

(deftest add-blocker-relationship-creates-dependency ()
  "Creates dependency between existing tasks in a project."
  ;; Create project with tasks
  (capture-inbox-item "Website redesign")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Design wireframes" :level 2)
    (make-task "Get client approval" :level 2)
    (make-task "Build prototype" :level 2)
    (organize-as-project))

  ;; Create dependency: "Build prototype" depends on "Get client approval"
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Build prototype")
    (org-back-to-heading t)
    (let ((approval-id
           (save-excursion
             (goto-char (point-min))
             (search-forward "Get client approval")
             (org-back-to-heading t)
             (org-id-get-create))))
      ;; Add DEPENDS_ON to "Build prototype"
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" approval-id)
      ;; Add BLOCKS to "Get client approval"
      (save-excursion
        (goto-char (point-min))
        (search-forward "Get client approval")
        (org-back-to-heading t)
        (let ((prototype-id (save-excursion
                              (goto-char (point-min))
                              (search-forward "Build prototype")
                              (org-back-to-heading t)
                              (org-id-get-create))))
          (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" prototype-id)))))

  ;; Verify dependency was created
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Build prototype")
    (org-back-to-heading t)
    (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
      (refute-nil depends-on)
      (assert-equal 1 (length depends-on)))
    (goto-char (point-min))
    (search-forward "Get client approval")
    (org-back-to-heading t)
    (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
      (refute-nil blocks)
      (assert-equal 1 (length blocks)))))

;;; Remove Blocker Relationship

(deftest remove-blocker-relationship-removes-dependency ()
  "Removes existing dependency between tasks in a project."
  ;; Create project with tasks
  (capture-inbox-item "Product launch")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Write documentation" :level 2)
    (make-task "Record demo video" :level 2)
    (organize-as-project))

  ;; Create and then remove dependency
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Record demo video")
    (org-back-to-heading t)
    (let ((doc-id
           (save-excursion
             (goto-char (point-min))
             (search-forward "Write documentation")
             (org-back-to-heading t)
             (org-id-get-create)))
          (video-id (org-id-get-create)))
      ;; Create the dependency
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" doc-id)
      (save-excursion
        (goto-char (point-min))
        (search-forward "Write documentation")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" video-id))

      ;; Verify dependency exists
      (goto-char (point-min))
      (search-forward "Record demo video")
      (org-back-to-heading t)
      (refute-nil (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

      ;; Remove the dependency
      (org-entry-remove-from-multivalued-property (point) "ORG_GTD_DEPENDS_ON" doc-id)
      (save-excursion
        (goto-char (point-min))
        (search-forward "Write documentation")
        (org-back-to-heading t)
        (org-entry-remove-from-multivalued-property (point) "ORG_GTD_BLOCKS" video-id))

      ;; Verify dependency is removed
      (goto-char (point-min))
      (search-forward "Record demo video")
      (org-back-to-heading t)
      (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
      (goto-char (point-min))
      (search-forward "Write documentation")
      (org-back-to-heading t)
      (assert-nil (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))))

;;; Add First Task to Existing Project

(deftest add-first-task-to-existing-project ()
  "Adds new task to ORG_GTD_FIRST_TASKS of existing project."
  ;; Create project
  (capture-inbox-item "Marketing campaign")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Create content calendar" :level 2)
    (organize-as-project))

  ;; Add a new first task to the project using proper GTD configuration
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Marketing campaign")
    (org-back-to-heading t)
    (let ((project-id (org-id-get)))
      (org-insert-heading-after-current)
      (insert "Design landing page")
      (org-do-demote)
      ;; Configure as next-action properly (sets ORG_GTD, TODO state, etc.)
      (org-gtd-configure-as-type 'next-action)
      (let ((new-task-id (org-id-get-create)))
        ;; Link task to project
        (org-entry-put (point) "ORG_GTD_PROJECT" "Marketing campaign")
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)
        (org-entry-put (point) "TRIGGER" "org-gtd-update-project-after-task-done!")
        ;; Add to project's FIRST_TASKS
        (org-up-heading-safe)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" new-task-id)

        ;; Verify task was added to FIRST_TASKS
        (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
          (refute-nil (member new-task-id first-tasks)))

        ;; Verify new task shows in engage
        (org-gtd-engage)
        (assert-match "Design landing page" (agenda-raw-text))))))

;;; project-task-operations-test.el ends here
