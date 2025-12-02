;;; project-clarify-test.el --- Unit tests for project clarification -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for project clarification features including validation
;; guidance and project template insertion.
;;
;; Migrated from test/project-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Validation Guidance

(deftest clarify/shows-validation-guidance-on-error ()
  "Shows validation guidance when project structure is invalid."
  (with-simulated-input "SPC"
    (org-gtd-projects--show-error))
  (with-current-buffer "*Message*"
    (assert-match "First task" (buffer-substring-no-properties (point-min) (point-max)))))

;;; Project Template Insertion

(deftest clarify/inserts-project-template ()
  "Allows insertion of a project template."
  (let ((native-comp-enable-subr-trampolines nil)
        (org-gtd-clarify-project-templates
         '(("prepare a video" . "* think of topic\n* record video\n* edit video"))))
    (ogt-eunit-with-mock-gtd
      (capture-inbox-item "New project")
      (org-gtd-process-inbox)
      (with-simulated-input "prepare SPC a SPC video RET"
        (org-gtd-clarify-project-insert-template))
      (org-gtd-organize)
      (organize-as-project)

      ;; VERIFY: Project template created exactly three tasks linked to the project
      (with-current-buffer (org-gtd--default-file)
        ;; Get the project ID
        (goto-char (point-min))
        (search-forward "New project")
        (org-back-to-heading t)
        (let ((project-id (org-id-get (point)))
              (task-count 0))
          (assert-true project-id)

          ;; Count tasks with this project ID
          (goto-char (point-min))
          (while (re-search-forward "^\\*+ " nil t)
            (org-back-to-heading t)
            (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
              (when (member project-id project-ids)
                (setq task-count (1+ task-count))))
            (forward-line 1))

          ;; Verify exactly 3 tasks belong to this project
          (assert-equal 3 task-count)))

      ;; VERIFY: Template tasks appear in engage view
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (assert-match "think of topic" (current-buffer-raw-text))))))

(provide 'project-clarify-test)

;;; project-clarify-test.el ends here
