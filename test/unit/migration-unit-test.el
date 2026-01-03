;;; migration-unit-test.el --- Unit tests for GTD migration functions -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for GTD migration functions including property addition
;; and GTD v3 structure migration.
;;
;; Migrated from test/migration-unit-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-upgrades)

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Migration Tests

(deftest migration/adds-org-gtd-property-to-level-2-tasks ()
  "Adds ORG_GTD property to level 2 tasks under Projects."
  (with-temp-buffer
    (org-mode)
    (insert "* Legacy Project\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (insert ":ID: legacy-project-id\n")
    (insert ":END:\n")
    (insert "** Legacy Task 1\n")  ;; Level 2 but no ORG_GTD property
    (insert ":PROPERTIES:\n")
    (insert ":ID: legacy-task1-id\n")
    (insert ":TODO: TODO\n")
    (insert ":END:\n")

    ;; Check that task doesn't have ORG_GTD property before migration
    (goto-char (point-min))
    (search-forward "Legacy Task 1")
    (org-back-to-heading t)
    (assert-nil (org-entry-get (point) "ORG_GTD"))

    ;; Apply migration - test the core logic directly
    (goto-char (point-min))
    (search-forward "Legacy Project")
    (org-back-to-heading t)
    (let ((project-level (org-current-level)))
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-current-level) project-level))
        (when (= (org-current-level) (1+ project-level))
          ;; This is a direct child (level 2 under project)
          (unless (org-entry-get (point) "ORG_GTD")
            (org-entry-put (point) "ORG_GTD" "Actions")))
        (outline-next-heading)))

    ;; Verify the task now has ORG_GTD property
    (goto-char (point-min))
    (search-forward "Legacy Task 1")
    (org-back-to-heading t)
    (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))))

(deftest migration/gtd-v3-structure-migration ()
  "Correctly migrates GTD v3 structure: project headings get ORG_GTD=Projects, tasks get ORG_GTD=Actions."
  ;; ACCEPTANCE TEST: This test defines the correct GTD v3 structure migration
  ;; Level 1: "Projects" (category heading) - already has ORG_GTD="Projects"
  ;; Level 2: PROJECT HEADINGS (individual projects) - should get ORG_GTD="Projects"
  ;; Level 3+: PROJECT TASKS (work items) - should get ORG_GTD="Actions"
  (let* ((org-agenda-skip-unavailable-files t)  ; Suppress prompts about missing files
         (temp-file (make-temp-file "org-gtd-migration-test" nil ".org"))
         (temp-buffer (find-file-noselect temp-file)))
    (unwind-protect
        (progn
          ;; Setup temp file with test data
          (with-current-buffer temp-buffer
            (org-mode)
            (erase-buffer)
            ;; Create the complete GTD v3 structure that needs migration
            (insert "* Projects\n")  ;; Level 1 - category heading (already correct)
            (insert ":PROPERTIES:\n")
            (insert ":ORG_GTD: Projects\n")
            (insert ":END:\n")
            (insert "** My Project\n")  ;; Level 2 - project heading (needs ORG_GTD="Projects")
            (insert ":PROPERTIES:\n")
            (insert ":ID: project-id\n")
            (insert ":END:\n")
            (insert "*** Task 1\n")  ;; Level 3 - project task (needs ORG_GTD="Actions")
            (insert ":PROPERTIES:\n")
            (insert ":ID: task1-id\n")
            (insert ":TODO: TODO\n")
            (insert ":END:\n")
            (insert "**** Subtask 1.1\n")  ;; Level 4 - project task (needs ORG_GTD="Actions")
            (insert ":PROPERTIES:\n")
            (insert ":ID: subtask11-id\n")
            (insert ":TODO: TODO\n")
            (insert ":END:\n")
            (insert "*** Task 2\n")  ;; Level 3 - project task (needs ORG_GTD="Actions")
            (insert ":PROPERTIES:\n")
            (insert ":ID: task2-id\n")
            (insert ":TODO: TODO\n")
            (insert ":END:\n")
            (save-buffer))

          ;; Set up org-agenda-files to include the temp file
          (let ((org-agenda-files (list temp-file)))

            ;; Before migration: verify nothing has ORG_GTD properties except category heading
            (with-current-buffer temp-buffer
              (goto-char (point-min))
              (search-forward "My Project")
              (org-back-to-heading t)
              (assert-nil (org-entry-get (point) "ORG_GTD"))

              (goto-char (point-min))
              (search-forward "Task 1")
              (org-back-to-heading t)
              (assert-nil (org-entry-get (point) "ORG_GTD"))

              (goto-char (point-min))
              (search-forward "Subtask 1.1")
              (org-back-to-heading t)
              (assert-nil (org-entry-get (point) "ORG_GTD"))

              (goto-char (point-min))
              (search-forward "Task 2")
              (org-back-to-heading t)
              (assert-nil (org-entry-get (point) "ORG_GTD")))

            ;; Apply corrected migration function (Step 1 only - property addition)
            (org-gtd-upgrade--add-org-gtd-properties)

            ;; After migration: verify correct ORG_GTD properties in the buffer
            ;; The migration function modifies buffers but doesn't save them
            ;; Project heading (level 2) should get ORG_GTD="Projects"
            (with-current-buffer temp-buffer
              (goto-char (point-min))
              (search-forward "My Project")
              (org-back-to-heading t)
              (assert-equal "Projects" (org-entry-get (point) "ORG_GTD"))

              ;; All project tasks (level 3+) should get ORG_GTD="Actions"
              (goto-char (point-min))
              (search-forward "Task 1")
              (org-back-to-heading t)
              (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

              (goto-char (point-min))
              (search-forward "Subtask 1.1")
              (org-back-to-heading t)
              (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))

              (goto-char (point-min))
              (search-forward "Task 2")
              (org-back-to-heading t)
              (assert-equal "Actions" (org-entry-get (point) "ORG_GTD")))))

      ;; Cleanup
      (when (buffer-live-p temp-buffer)
        (with-current-buffer temp-buffer
          (set-buffer-modified-p nil))
        (kill-buffer temp-buffer))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'migration-unit-test)

;;; migration-unit-test.el ends here
