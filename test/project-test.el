;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Project management"

  :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files)
             ;; TODO figure out if this can / should be removed
             (remove-hook 'post-command-hook 'org-add-log-note))

 (describe
  "marks all undone tasks of a canceled project as canceled"
  (it "on a task in the agenda"
      (ogt-capture-and-process-project "project headline")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-gtd-project-cancel-from-agenda)
        (org-gtd-archive-completed-items))

      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "project headline")))

  (it "when on the heading"
      (ogt-capture-and-process-project "project tailline")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "project tailline")
        (org-gtd-project-cancel)
        (org-gtd-archive-completed-items)
        (basic-save-buffer))

      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "project tailline"))))

 (describe
  "displaying the guide when the project is poorly shaped"
  (it "does it"
      (with-simulated-input "SPC"
                            (org-gtd-projects--show-error))
      (expect (ogt--buffer-string "*Message*")
              :to-match "** First Task"))))

(describe
 "Clarifying a project"

  :var ((inhibit-message t))

 (before-each (ogt--configure-emacs)
              (setq org-gtd-clarify-project-templates
         '(("prepare a video" . "* think of topic\n* record video\n* edit video"))))
 (after-each (ogt--close-and-delete-files)
             (setq org-gtd-clarify-project-templates nil)
             ;; TODO figure out if this can / should be removed
             ;(remove-hook 'post-command-hook 'org-add-log-note)
             )

 (it "allows insertion of a project template"
     (ogt-capture-single-item "New project")
     (org-gtd-process-inbox)
     (with-simulated-input "prepare SPC a SPC video RET"
                           (org-gtd-clarify-project-insert-template))
     (org-gtd-organize)
     (ogt-clarify-as-project)
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (expect (ogt--current-buffer-raw-text)
               :to-match
               "think of topic"))))

;;; TODO uncomment this test
;;; TODO stat cookie should support percent and tally versions

;; (it "safely adds the stats cookie"
;;     (setq org-gtd-organize-hooks '(org-set-tags-command org-priority))
;;     (ogt-capture-single-item "project headline")
;;     (org-gtd-process-inbox)
;;     (execute-kbd-macro (kbd "M-> RET"))
;;     (insert ogt--project-text)
;;     (execute-kbd-macro (kbd "C-c c p headline_tag RET A task_1_tag RET B task_2_tag RET C task_3_tag RET A"))
;;     (ogt--save-all-buffers)
;;     (with-current-buffer (org-gtd--default-file)
;;       (goto-char (point-min))
;;       (expect (ogt--current-buffer-raw-text)
;;               :to-match "[0/3]")
;;       (search-forward "project headline")
;;       (expect (member "headline_tag" (org-get-tags)))
;;       (expect (org-element-property :priority (org-element-at-point))
;;               :to-equal (org-priority-to-value "A"))
;;       (search-forward "Task 1")
;;       (expect (member "task_1_tag" (org-get-tags)))
;;       (expect (org-element-property :priority (org-element-at-point))
;;               :to-equal (org-priority-to-value "B"))
;;       (search-forward "Task 2")
;;       (expect (member "task_2_tag" (org-get-tags)))
;;       (expect (org-element-property :priority (org-element-at-point))
;;               :to-equal (org-priority-to-value "C"))
;;       (search-forward "Task 3")
;;       (expect (member "task_3_tag" (org-get-tags)))
;;       (expect (org-element-property :priority (org-element-at-point))
;;               :to-equal (org-priority-to-value "A"))))
;; )
