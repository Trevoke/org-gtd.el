;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)
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
 (describe
  "through the agenda view"

  (it "handles the target heading"
      (ogt-capture-and-process-incubated-item "projectify-me" (calendar-current-date))
      (org-gtd-engage)
      (set-buffer org-agenda-buffer)
      (goto-char (point-min))
      (search-forward "projectify")
      (org-gtd-clarify-agenda-item)
      (execute-kbd-macro (kbd "M-> RET"))
      (insert ogt--project-text)
      (ogt-clarify-as-project)
      (kill-buffer org-agenda-buffer)
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (expect (ogt--current-buffer-raw-text)
                :to-match
                "Task 1"))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward ":ORG_GTD: Incubated")
        (org-narrow-to-subtree)
        (expect (ogt--current-buffer-raw-text)
                :not :to-match
                "projectify")
        (widen)
        (search-forward ":ORG_GTD: Projects")
        (org-narrow-to-subtree)
        (expect (ogt--current-buffer-raw-text)
                :to-match
                "projectify")
        (widen)))))
