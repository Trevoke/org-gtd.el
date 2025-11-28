;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Areas of focus"


 (before-each (setq inhibit-message t)
              (ogt--configure-emacs)
              (setq org-gtd-areas-of-focus '("Health" "Home" "Career")))
 (after-each (ogt--close-and-delete-files)
             (setq org-gtd-areas-of-focus nil))

 (describe
  "org-mode CATEGORY"

 (before-each
  (add-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus))
 (after-each
  (remove-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus))

  (it "is set on clarified item from a customizable list"
      (capture-inbox-item "Medical Appointment")
      (org-gtd-process-inbox)
      (execute-kbd-macro (kbd "C-c c s H e a l t h RET"))
      (org-gtd-engage)
      (expect (ogt--buffer-string org-agenda-buffer)
              :to-match
              "Health.*Medical"))

  (it "is set on item at point from the areas of focus decoration"
      (with-current-buffer (get-buffer-create "temp.org")
        (org-mode)
        (insert "* A heading")
        (with-simulated-input "Health RET"
                              (org-gtd-area-of-focus-set-on-item-at-point))
        (expect (org-entry-get (point) "CATEGORY")
                :to-equal
                "Health")
        (kill-buffer))))

 (describe
  "through the agenda"
  (it "sets area of focus on single action task from agenda view"
      (create-single-action "foobar")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "foobar")
        (with-simulated-input
         "Home RET"
         (org-gtd-area-of-focus-set-on-agenda-item)))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "foobar")
        (expect (org-entry-get (point) "CATEGORY")
                :to-equal
                "Home")))

  (it "does NOT set category on Actions parent when setting on single action"
      ;; This test verifies the bug fix: single actions should get CATEGORY set
      ;; on themselves, NOT on the parent * Actions heading
      (create-single-action "my single action")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "my single action")
        (with-simulated-input
         "Career RET"
         (org-gtd-area-of-focus-set-on-agenda-item)))
      (with-current-buffer (org-gtd--default-file)
        ;; Verify CATEGORY is on the single action
        (goto-char (point-min))
        (search-forward "my single action")
        (expect (org-entry-get (point) "CATEGORY" nil)  ; nil = no inherit
                :to-equal
                "Career")
        ;; Verify CATEGORY on parent is NOT "Career" (the one we just set)
        (org-up-heading-safe)
        (expect (org-get-heading t t t t)
                :to-equal
                "Actions")
        (let ((parent-category (org-entry-get (point) "CATEGORY" nil)))
          (expect parent-category :not :to-equal "Career"))))

  (it "sets category on ALL tasks in project when setting on project task"
      ;; Changed behavior: instead of setting on project heading,
      ;; set CATEGORY on all tasks in the project DAG
      (create-project "my project")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "Task 1")
        (with-simulated-input
         "Home RET"
         (org-gtd-area-of-focus-set-on-agenda-item)))
      (with-current-buffer (org-gtd--default-file)
        ;; Verify CATEGORY is set on all three tasks
        (goto-char (point-min))
        (search-forward "Task 1")
        (expect (org-entry-get (point) "CATEGORY" nil)
                :to-equal
                "Home")
        (goto-char (point-min))
        (search-forward "Task 2")
        (expect (org-entry-get (point) "CATEGORY" nil)
                :to-equal
                "Home")
        (goto-char (point-min))
        (search-forward "Task 3")
        (expect (org-entry-get (point) "CATEGORY" nil)
                :to-equal
                "Home")))

  (it "prompts for project when task belongs to multiple projects"
      ;; Create two projects sharing a task
      (capture-inbox-item "Project Alpha")
      (org-gtd-process-inbox)
      (with-wip-buffer
        (goto-char (point-max))
        (newline)
        (make-task "Shared task" :level 2)
        (make-task "Alpha unique" :level 2)
        (organize-as-project))

      (capture-inbox-item "Project Beta")
      (org-gtd-process-inbox)
      (with-wip-buffer
        (goto-char (point-max))
        (newline)
        (make-task "Beta unique" :level 2)
        (organize-as-project))

      ;; Share "Shared task" with Project Beta
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Shared task")
        (org-back-to-heading t)
        (let ((shared-task-id (org-id-get-create)))
          (goto-char (point-min))
          (search-forward "Project Beta")
          (org-back-to-heading t)
          (let ((beta-id (org-id-get-create)))
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)
            (goto-char (point-min))
            (search-forward "Shared task")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" beta-id))))

      ;; Try to set area of focus on shared task - should prompt
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "Shared task")
        ;; Should prompt: "Which project?" with choices "Project Alpha" "Project Beta"
        ;; Then prompt for area of focus
        (with-simulated-input
         "Project SPC Alpha RET Career RET"
         (org-gtd-area-of-focus-set-on-agenda-item)))

      ;; Verify CATEGORY is set on Project Alpha's tasks only
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Shared task")
        (expect (org-entry-get (point) "CATEGORY" nil)
                :to-equal
                "Career")
        (goto-char (point-min))
        (search-forward "Alpha unique")
        (expect (org-entry-get (point) "CATEGORY" nil)
                :to-equal
                "Career")
        ;; Beta's task should NOT have "Career" CATEGORY
        (goto-char (point-min))
        (search-forward "Beta unique")
        (let ((beta-category (org-entry-get (point) "CATEGORY" nil)))
          (expect beta-category :not :to-equal "Career"))))

  (it "errors when item has no ORG_GTD property"
      ;; Create a single action, then corrupt it by removing ORG_GTD
      (create-single-action "corrupted action")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "corrupted action")
        ;; Remove ORG_GTD property to corrupt the item
        (org-entry-delete (point) "ORG_GTD"))

      ;; Try to set area of focus from agenda - should error
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "corrupted action")
        (expect (org-gtd-area-of-focus-set-on-agenda-item)
                :to-throw
                'user-error)))))
