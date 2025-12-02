;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

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
;;       (expect (current-buffer-raw-text)
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

;; Sequential dependency tests migrated to test-eunit/integration/project-dependencies-test.el

;; Fix TODO keywords tests migrated to test-eunit/integration/project-fix-keywords-test.el
;; Pending test kept here until all pending tests are addressed:

(describe
 "Fix TODO keywords for all projects (Story: multi-project task state management)"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (xit "keeps multi-project task TODO when ready in SOME but not ALL projects"
     ;; Simpler scenario: Create two projects sharing one task     ;; Project Gamma: Task A -> Task B (blocked)
     ;; Project Delta: Task B only (ready)
     ;; Task B belongs to both, but blocked in Gamma, ready in Delta
     ;; Expected: Task B stays TODO (AND semantics: must be ready in ALL)

     ;; Create Project Gamma: Task A -> Task B
     (create-project "Project Gamma")
     (with-current-buffer (org-gtd--default-file)
       ;; Get IDs for both tasks
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (let ((task-a-id (org-id-get-create)))

         (goto-char (point-min))
         (search-forward "Task 2")
         (org-back-to-heading t)
         (let ((task-b-id (org-id-get-create)))

           ;; Make Task A block Task B using BLOCKS/DEPENDS_ON
           (goto-char (point-min))
           (search-forward "Task 1")
           (org-back-to-heading t)
           (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)
           (goto-char (point-min))
           (search-forward "Task 2")
           (org-back-to-heading t)
           (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id))))

     ;; Create Project Delta empty
     (capture-inbox-item "Project Delta")
     (org-gtd-process-inbox)
     (with-wip-buffer
       (goto-char (point-max))
       (newline)
       (organize-as-project))

     ;; Share Task 2 with Project Delta
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Project Gamma")
       (org-back-to-heading t)
       (let ((gamma-id (org-id-get-create)))
         (goto-char (point-min))
         (search-forward "Project Delta")
         (org-back-to-heading t)
         (let ((delta-id (org-id-get-create)))
           (goto-char (point-min))
           (search-forward "Task 2")
           (org-back-to-heading t)
           (let ((task-2-id (org-id-get-create)))

             ;; Add Task 2 as first task of Delta
             (goto-char (point-min))
             (search-forward "Project Delta")
             (org-back-to-heading t)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-2-id)

             ;; Add BOTH project IDs to Task 2
             (goto-char (point-min))
             (search-forward "Task 2")
             (org-back-to-heading t)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" gamma-id)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" delta-id)))))

     ;; Run the global fix function
     (org-gtd-projects-fix-all-todo-keywords)

     ;; Verify: Task 2 should be TODO (blocked in Gamma, ready in Delta)
     ;; AND semantics: must be ready in ALL projects
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 2")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "TODO"))

     ;; Verify: Task 1 should be NEXT (ready in Gamma)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "NEXT")))

)
 ;; Other active tests (preserves WAIT, simple branching, independent DAG paths)
 ;; migrated to test-eunit/integration/project-fix-keywords-test.el

;; State preservation and basic tickler tests migrated to test-eunit/unit/project-tickler-test.el
;; Advanced tickler tests migrated to test-eunit/integration/project-tickler-advanced-test.el

