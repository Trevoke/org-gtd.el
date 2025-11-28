;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

(describe
 "Flow for clarifying items"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "stores a marker to the original heading as local variable in the WIP buffer"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
       (with-current-buffer source-buffer
         (org-gtd-clarify-item))

       (let ((task-id (with-current-buffer source-buffer (org-id-get)))
             (wip-buffer (car (org-gtd-wip--get-buffers))))
         (with-current-buffer wip-buffer
           (expect (org-entry-get org-gtd-clarify--source-heading-marker "ID")
                   :to-equal task-id)))))
 (it "sets skip-refile flag when called with prefix arg"
     (create-single-action "Test item")
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Test item")
       (org-back-to-heading t)
       (let ((current-prefix-arg '(4)))
         (org-gtd-clarify-item))
       ;; Find the WIP buffer and check the flag
       (expect (ogt-get-wip-buffer) :not :to-be nil)
       (with-wip-buffer
         (expect org-gtd-clarify--skip-refile :to-be t))))

 (describe
  "organize help buffer"

  (it "has a content constant with all GTD organize types"
      (expect (boundp 'org-gtd-clarify-organize-help-content) :to-be t)
      (expect org-gtd-clarify-organize-help-content :to-match "Quick Action")
      (expect org-gtd-clarify-organize-help-content :to-match "Single Action")
      (expect org-gtd-clarify-organize-help-content :to-match "Project")
      (expect org-gtd-clarify-organize-help-content :to-match "Calendar")
      (expect org-gtd-clarify-organize-help-content :to-match "Delegate")
      (expect org-gtd-clarify-organize-help-content :to-match "Habit")
      (expect org-gtd-clarify-organize-help-content :to-match "Tickler")
      (expect org-gtd-clarify-organize-help-content :to-match "Someday")
      (expect org-gtd-clarify-organize-help-content :to-match "Knowledge")
      (expect org-gtd-clarify-organize-help-content :to-match "Trash"))

  (it "creates a buffer with help content in org-mode and read-only"
      (let ((buffer (org-gtd-clarify--get-or-create-organize-help-buffer)))
        (expect buffer :not :to-be nil)
        (expect (buffer-name buffer) :to-equal "*Org GTD Organize Help*")
        (with-current-buffer buffer
          (expect (buffer-string) :to-match "Quick Action")
          (expect major-mode :to-equal 'org-mode)
          (expect buffer-read-only :to-be t))
        (kill-buffer buffer)))

  (it "toggles the organize help window on and off"
      (let ((org-gtd-clarify-show-organize-help 'right))
        ;; Initially no window
        (expect (get-buffer-window "*Org GTD Organize Help*") :to-be nil)
        ;; Toggle on
        (org-gtd-clarify-toggle-organize-help)
        (expect (get-buffer-window "*Org GTD Organize Help*") :not :to-be nil)
        ;; Toggle off
        (org-gtd-clarify-toggle-organize-help)
        (expect (get-buffer-window "*Org GTD Organize Help*") :to-be nil)
        ;; Cleanup
        (when-let ((buf (get-buffer "*Org GTD Organize Help*")))
          (kill-buffer buf)))))

 (describe
  "through the agenda view"

  (it "converts tickler item to project with tasks via clarify-agenda-item"
      (create-deferred-item "projectify-me" (calendar-current-date))
      (org-gtd-engage)
      (set-buffer org-agenda-buffer)
      (goto-char (point-min))
      (search-forward "projectify")
      (org-gtd-clarify-agenda-item)
      (execute-kbd-macro (kbd "M-> RET"))
      ;; Create three simple tasks using builder
      (make-task "Task 1" :level 2)
      (make-task "Task 2" :level 2)
      (make-task "Task 3" :level 2)
      (organize-as-project)
      (kill-buffer org-agenda-buffer)
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (expect (current-buffer-raw-text)
                :to-match
                "Task 1"))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward ":ORG_GTD_REFILE: Tickler")
        (org-narrow-to-subtree)
        (expect (current-buffer-raw-text)
                :not :to-match
                "projectify")
        (widen)
        (search-forward ":ORG_GTD_REFILE: Projects")
        (org-narrow-to-subtree)
        (expect (current-buffer-raw-text)
                :to-match
                "projectify")
        (widen)))

  (it "sets skip-refile flag when called with prefix arg"
      (create-single-action "Agenda test item")
      (ogt--save-all-buffers)
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "Agenda test item")
        ;; Simulate interactive call: in real usage, current-prefix-arg
        ;; is set by the command loop before the function is called
        (let ((current-prefix-arg '(4)))
          (org-gtd-clarify-agenda-item))
        ;; Find the WIP buffer and check the flag
        (expect (ogt-get-wip-buffer) :not :to-be nil)
        (with-wip-buffer
          (expect org-gtd-clarify--skip-refile :to-be t))))))
