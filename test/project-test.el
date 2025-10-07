;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Project management"


 (before-each (setq inhibit-message t)
              (ogt--configure-emacs)
              ;; Clear org-id locations to prevent pollution from previous tests
              (setq org-id-locations nil)
              (setq org-id-files nil))
 (after-each (ogt--close-and-delete-files)
             ;; Clear org-id locations after tests
             (setq org-id-locations nil)
             (setq org-id-files nil)
             ;; TODO figure out if this can / should be removed
             (remove-hook 'post-command-hook 'org-add-log-note))

 (describe
  "marks all undone tasks of a canceled project as canceled"
  (it "on a task in the agenda"
      (create-project "project headline")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-gtd-project-cancel-from-agenda)
        (org-gtd-archive-completed-items))

      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "project headline")))

  (it "when on the heading"
      (create-project "project tailline")
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


 (before-each (setq inhibit-message t)
              (ogt--configure-emacs)
              (setq org-gtd-clarify-project-templates
         '(("prepare a video" . "* think of topic\n* record video\n* edit video"))))
 (after-each (ogt--close-and-delete-files)
             (setq org-gtd-clarify-project-templates nil)
             ;; TODO figure out if this can / should be removed
             ;(remove-hook 'post-command-hook 'org-add-log-note)
             )

 (it "allows insertion of a project template"
     (capture-inbox-item "New project")
     (org-gtd-process-inbox)
     (with-simulated-input "prepare SPC a SPC video RET"
                           (org-gtd-clarify-project-insert-template))
     (org-gtd-organize)
     (organize-as-project)
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (expect (ogt--current-buffer-raw-text)
               :to-match
               "think of topic"))))

;;; TODO uncomment this test
;;; TODO stat cookie should support percent and tally versions

;; (it "safely adds the stats cookie"
;;     (setq org-gtd-organize-hooks '(org-set-tags-command org-priority))
;;     (capture-inbox-item "project headline")
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

(describe
 "Default sequential dependencies (Story 7)"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files)
             ;; TODO figure out if this can / should be removed
             (remove-hook 'post-command-hook 'org-add-log-note))

 (it "creates sequential dependencies for tasks without existing relationships"
     ;; Create a project with 3 tasks and verify sequential dependencies are created
     (create-project "sequential project")
     
     ;; Verify the project was created and check the sequential dependencies
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "sequential project")
       
       ;; Find Task 1 - should have no ORG_GTD_DEPENDS_ON but should BLOCK Task 2
       (search-forward "Task 1")
       (org-back-to-heading t)
       (let ((task1-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
             (task1-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
         (expect task1-depends :to-be nil)  ;; First task depends on nothing
         (expect (length task1-blocks) :to-equal 1))  ;; But blocks one task

       ;; Find Task 2 - should DEPEND_ON Task 1 and BLOCK Task 3
       (search-forward "Task 2")
       (org-back-to-heading t)
       (let ((task2-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
             (task2-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
         (expect (length task2-depends) :to-equal 1)  ;; Depends on Task 1
         (expect (length task2-blocks) :to-equal 1))  ;; Blocks Task 3

       ;; Find Task 3 - should DEPEND_ON Task 2 but block nothing
       (search-forward "Task 3")
       (org-back-to-heading t)
       (let ((task3-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
             (task3-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
         (expect (length task3-depends) :to-equal 1)  ;; Depends on Task 2
         (expect task3-blocks :to-be nil))))  ;; Last task blocks nothing

 (it "preserves existing dependencies when organizing project"
     ;; This tests Story 8: Custom Dependencies Override Defaults
     ;; Create a project, add custom dependencies, then organize - custom dependencies should be preserved
     (capture-inbox-item "custom dependencies project")
     (org-gtd-process-inbox)
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
     (let ((task3-id (org-gtd-id-get-create)))
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (let ((task1-id (org-gtd-id-get-create)))
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
     (organize-as-project)

     ;; Verify custom dependencies are preserved
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "custom dependencies project")

       ;; Task 3 should still depend on Task 1
       (search-forward "Task 3")
       (org-back-to-heading t)
       (let ((task3-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
         (expect (length task3-depends) :to-equal 1))

       ;; Task 1 should block both Task 3 (custom) and Task 2 (default sequential)
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (let ((task1-blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
         (expect (length task1-blocks) :to-equal 2))

       ;; Task 2 should get default sequential dependency (depends on Task 1)
       ;; since it has no custom dependencies
       (goto-char (point-min))
       (search-forward "Task 2")
       (org-back-to-heading t)
       (let ((task2-depends (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
         (expect (length task2-depends) :to-equal 1)))))
