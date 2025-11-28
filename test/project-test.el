;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
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
             ;; Save cleared state to disk to prevent test pollution
             (org-id-locations-save)
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
        (expect archived-projects :to-match "project tailline")))

  (it "errors when called on a single action (not a project task)"
      ;; Bug fix: calling project-cancel on a single action should error,
      ;; not try to cancel the parent Actions heading
      (create-single-action "not a project task")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "not a project task")
        ;; Should error because this is not a project task
        (expect (org-gtd-project-cancel-from-agenda)
                :to-throw
                'user-error)))

  (it "cancels project with task in different file (not outline child)"
      ;; Multi-file DAG: project task doesn't have to be outline child of project
      ;; Must find project via ORG_GTD_PROJECT_IDS, not via org-up-heading-safe
      (capture-inbox-item "Multi-file project")
      (org-gtd-process-inbox)

      (with-wip-buffer
        (goto-char (point-max))
        (newline)
        (make-task "Task in main file" :level 2)
        (organize-as-project))

      ;; Create task in different file
      (let ((second-file (org-gtd--path "other-file")))
        (with-temp-file second-file
          (make-task "Task in other file" :id "other-file-task" :level 1 :status 'next))

        (with-current-buffer (find-file-noselect second-file)
          (org-mode)
          (goto-char (point-min))
          (search-forward "Task in other file")
          (org-back-to-heading t)
          (org-id-add-location "other-file-task" second-file))

        ;; Link task from second file to project
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Multi-file project")
          (org-back-to-heading t)
          (let ((project-id (org-id-get-create)))
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "other-file-task")
            ;; Add project ID to task in other file
            (with-current-buffer (find-file-noselect second-file)
              (goto-char (point-min))
              (search-forward "Task in other file")
              (org-back-to-heading t)
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)
              (basic-save-buffer))))

        ;; Cancel project from agenda via task in OTHER file
        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))
          (org-gtd-engage)
          (with-current-buffer org-agenda-buffer
            (goto-char (point-min))
            (search-forward "Task in other file")
            ;; This should cancel the whole project, even though task is not outline child
            (org-gtd-project-cancel-from-agenda))

          ;; Verify project was canceled by checking that tasks are CNCL
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task in main file")
            (org-back-to-heading t)
            (expect (org-entry-get (point) "TODO")
                    :to-equal
                    "CNCL"))

          ;; Also verify task in OTHER file was canceled
          (with-current-buffer (find-file-noselect second-file)
            (goto-char (point-min))
            (search-forward "Task in other file")
            (org-back-to-heading t)
            (expect (org-entry-get (point) "TODO")
                    :to-equal
                    "CNCL"))))))

 (describe
  "displaying the guide when the project is poorly shaped"
  (it "shows validation guidance when project structure is invalid"
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

     ;; VERIFY: Project template created exactly three tasks linked to the project
     (with-current-buffer (org-gtd--default-file)
       ;; Get the project ID
       (goto-char (point-min))
       (search-forward "New project")
       (org-back-to-heading t)
       (let ((project-id (org-id-get (point)))
             (task-count 0))
         (expect project-id :not :to-be nil)

         ;; Count tasks with this project ID
         (goto-char (point-min))
         (while (re-search-forward "^\\*+ " nil t)
           (org-back-to-heading t)
           (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
             (when (member project-id project-ids)
               (setq task-count (1+ task-count))))
           (forward-line 1))

         ;; Verify exactly 3 tasks belong to this project
         (expect task-count :to-equal 3)))

     ;; VERIFY: Template tasks appear in engage view
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (expect (current-buffer-raw-text)
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

(describe
 "Fix TODO keywords for all projects (Story: multi-project task state management)"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "handles single project correctly (baseline - ensures no regression)"
     ;; Create a simple project with sequential dependencies
     (create-project "Single Project")

     ;; Manually mark Task 1 as DONE
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (org-entry-put (point) "TODO" "DONE"))

     ;; Run the global fix function
     (org-gtd-projects-fix-all-todo-keywords)

     ;; Task 1: DONE (preserved)
     ;; Task 2: should be NEXT (dependency satisfied)
     ;; Task 3: should be TODO (depends on Task 2)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "DONE")

       (goto-char (point-min))
       (search-forward "Task 2")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "NEXT")

       (goto-char (point-min))
       (search-forward "Task 3")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "TODO")))

 (it "marks multi-project task NEXT when ready in ALL projects (AND semantics)"
     ;; Create Project A with shared task
     (capture-inbox-item "Project Alpha")
     (org-gtd-process-inbox)
     (with-wip-buffer
       (goto-char (point-max))
       (newline)
       (make-task "Shared Task" :level 2)
       (make-task "Alpha Task" :level 2)
       (organize-as-project))

     ;; Create Project Beta with shared task
     (capture-inbox-item "Project Beta")
     (org-gtd-process-inbox)
     (with-wip-buffer
       (goto-char (point-max))
       (newline)
       (make-task "Beta Task" :level 2)
       (organize-as-project))

     ;; Share "Shared Task" with Project Beta
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Shared Task")
       (org-back-to-heading t)
       (let ((shared-task-id (org-id-get-create)))
         (goto-char (point-min))
         (search-forward "Project Beta")
         (org-back-to-heading t)
         (let ((beta-id (org-id-get-create)))
           (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)
           (goto-char (point-min))
           (search-forward "Shared Task")
           (org-back-to-heading t)
           (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" beta-id))))

     ;; Run the global fix function
     (org-gtd-projects-fix-all-todo-keywords)

     ;; Verify: Shared Task should be NEXT (ready in BOTH projects)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Shared Task")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "NEXT")))

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

 (it "preserves WAIT tasks (never changes them to TODO or NEXT)"
     ;; Create a project with 3 tasks in sequence
     (create-project "Project With Wait")
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (let ((task-1-id (org-id-get-create)))
         (goto-char (point-min))
         (search-forward "Task 2")
         (org-back-to-heading t)
         (let ((task-2-id (org-id-get-create)))
           (goto-char (point-min))
           (search-forward "Task 3")
           (org-back-to-heading t)
           (let ((task-3-id (org-id-get-create)))
             ;; Set up dependencies: Task 1 -> Task 2 -> Task 3
             (goto-char (point-min))
             (search-forward "Task 1")
             (org-back-to-heading t)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-2-id)
             (goto-char (point-min))
             (search-forward "Task 2")
             (org-back-to-heading t)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-1-id)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-3-id)
             (goto-char (point-min))
             (search-forward "Task 3")
             (org-back-to-heading t)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-2-id)))))

     ;; Mark Task 2 as WAIT (user explicitly marked it)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 2")
       (org-back-to-heading t)
       (org-entry-put (point) "TODO" "WAIT"))

     ;; Complete Task 1, so Task 2 would normally become NEXT
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (org-entry-put (point) "TODO" "DONE"))

     ;; Run the global fix function
     (org-gtd-projects-fix-all-todo-keywords)

     ;; Verify: Task 2 should still be WAIT (never auto-changed)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 2")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "WAIT"))

     ;; Verify: Task 3 should be TODO (blocked by WAIT task)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 3")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "TODO")))

 (it "handles simple branching paths (A -> B and A -> D)"
     ;; Simpler test without WAIT to isolate branching logic
     ;;   Task A -> Task B
     ;;   Task A -> Task D
     ;; When A is DONE: both B and D should become NEXT

     (create-project "Simple Branch")
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (let ((task-1-id (org-id-get-create)))
         (goto-char (point-min))
         (search-forward "Task 2")
         (org-back-to-heading t)
         (let ((task-2-id (org-id-get-create)))
           (goto-char (point-min))
           (search-forward "Task 3")
           (org-back-to-heading t)
           (let ((task-3-id (org-id-get-create)))

             ;; Clear default sequential dependencies created by organize-as-project
             (goto-char (point-min))
             (search-forward "Task 1")
             (org-back-to-heading t)
             (org-entry-delete (point) "ORG_GTD_BLOCKS")
             (goto-char (point-min))
             (search-forward "Task 2")
             (org-back-to-heading t)
             (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")
             (org-entry-delete (point) "ORG_GTD_BLOCKS")
             (goto-char (point-min))
             (search-forward "Task 3")
             (org-back-to-heading t)
             (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")

             ;; Set up: Task 1 -> Task 2, Task 1 -> Task 3 (branching paths)
             (goto-char (point-min))
             (search-forward "Task 1")
             (org-back-to-heading t)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-2-id)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-3-id)

             (goto-char (point-min))
             (search-forward "Task 2")
             (org-back-to-heading t)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-1-id)

             (goto-char (point-min))
             (search-forward "Task 3")
             (org-back-to-heading t)
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-1-id)

             ;; Set Task 1 as the only first task
             (goto-char (point-min))
             (search-forward "Simple Branch")
             (org-back-to-heading t)
             (org-entry-delete (point) "ORG_GTD_FIRST_TASKS")
             (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-1-id)))))

     ;; Complete Task 1
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 1")
       (org-back-to-heading t)
       (org-entry-put (point) "TODO" "DONE"))

     ;; Run fix
     (org-gtd-projects-fix-all-todo-keywords)

     ;; Verify: Task 2 should be NEXT
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 2")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "NEXT"))

     ;; Verify: Task 3 should be NEXT
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task 3")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "NEXT")))

 (it "allows NEXT in independent DAG paths when another path has WAIT"
     ;; Create a project with branching paths:
     ;;   Task A -> Task B (WAIT) -> Task C
     ;;   Task A -> Task D
     ;; When A is DONE and B is WAIT:
     ;;   - B stays WAIT
     ;;   - C stays TODO (blocked by WAIT B)
     ;;   - D becomes NEXT (independent path, only depends on A)

     (capture-inbox-item "Project With Branches")
     (org-gtd-process-inbox)
     (with-wip-buffer
       (goto-char (point-max))
       (newline)
       (make-task "Task A" :level 2)
       (make-task "Task B" :level 2)
       (make-task "Task C" :level 2)
       (make-task "Task D" :level 2)
       (organize-as-project))

     ;; Set up the DAG structure
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task A")
       (org-back-to-heading t)
       (let ((task-a-id (org-id-get-create)))
         (goto-char (point-min))
         (search-forward "Task B")
         (org-back-to-heading t)
         (let ((task-b-id (org-id-get-create)))
           (goto-char (point-min))
           (search-forward "Task C")
           (org-back-to-heading t)
           (let ((task-c-id (org-id-get-create)))
             (goto-char (point-min))
             (search-forward "Task D")
             (org-back-to-heading t)
             (let ((task-d-id (org-id-get-create)))

               ;; Clear default sequential dependencies
               (goto-char (point-min))
               (search-forward "Task A")
               (org-back-to-heading t)
               (org-entry-delete (point) "ORG_GTD_BLOCKS")
               (goto-char (point-min))
               (search-forward "Task B")
               (org-back-to-heading t)
               (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")
               (org-entry-delete (point) "ORG_GTD_BLOCKS")
               (goto-char (point-min))
               (search-forward "Task C")
               (org-back-to-heading t)
               (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")
               (org-entry-delete (point) "ORG_GTD_BLOCKS")
               (goto-char (point-min))
               (search-forward "Task D")
               (org-back-to-heading t)
               (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")

               ;; A -> B -> C
               (goto-char (point-min))
               (search-forward "Task A")
               (org-back-to-heading t)
               (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)
               (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-d-id)

               (goto-char (point-min))
               (search-forward "Task B")
               (org-back-to-heading t)
               (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)
               (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-c-id)

               (goto-char (point-min))
               (search-forward "Task C")
               (org-back-to-heading t)
               (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-b-id)

               ;; A -> D (independent path)
               (goto-char (point-min))
               (search-forward "Task D")
               (org-back-to-heading t)
               (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)

               ;; Set Task A as the only first task of the project
               (goto-char (point-min))
               (search-forward "Project With Branches")
               (org-back-to-heading t)
               (org-entry-delete (point) "ORG_GTD_FIRST_TASKS")
               (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-a-id))))))

     ;; Complete Task A
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task A")
       (org-back-to-heading t)
       (org-entry-put (point) "TODO" "DONE"))

     ;; Mark Task B as WAIT
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task B")
       (org-back-to-heading t)
       (org-entry-put (point) "TODO" "WAIT"))

     ;; Run the global fix function
     (org-gtd-projects-fix-all-todo-keywords)

     ;; Verify: Task B stays WAIT
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task B")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "WAIT"))

     ;; Verify: Task C stays TODO (blocked by WAIT B)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task C")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "TODO"))

     ;; Verify: Task D becomes NEXT (independent path from B)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task D")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "TODO") :to-equal "NEXT")))
)

(describe "State preservation for tickler"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "saves ORG_GTD and TODO state to PREVIOUS_* properties"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)

        ;; Set up initial state
        (org-entry-put (point) "ORG_GTD" "Actions")
        (org-entry-put (point) "TODO" "NEXT")

        ;; Save state
        (org-gtd-project--save-state (point))

        ;; Verify state was saved
        (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Actions")
        (expect (org-entry-get (point) "PREVIOUS_TODO") :to-equal "NEXT")

        ;; Verify ORG_GTD changed to Tickler
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")

        ;; Verify TODO keyword was cleared
        (expect (org-entry-get (point) "TODO") :to-be nil)))

  (it "restores ORG_GTD and TODO state from PREVIOUS_* properties"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)

        ;; Set up tickler state with saved previous values
        (org-entry-put (point) "ORG_GTD" "Tickler")
        (org-entry-put (point) "PREVIOUS_ORG_GTD" "Actions")
        (org-entry-put (point) "PREVIOUS_TODO" "NEXT")
        (org-todo 'none)  ; Clear TODO keyword

        ;; Restore state
        (org-gtd-project--restore-state (point))

        ;; Verify state was restored
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
        (expect (org-entry-get (point) "TODO") :to-equal "NEXT")

        ;; Verify PREVIOUS_* properties were removed
        (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)
        (expect (org-entry-get (point) "PREVIOUS_TODO") :to-be nil)))

  (it "collects all tasks in a project by graph traversal"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        (org-back-to-heading t)
        (let ((project-marker (point-marker))
              (task-ids '()))

          ;; Get all task IDs
          (goto-char (point-min))
          (search-forward "Task 1")
          (org-back-to-heading t)
          (push (org-id-get-create) task-ids)
          (goto-char (point-min))
          (search-forward "Task 2")
          (org-back-to-heading t)
          (push (org-id-get-create) task-ids)
          (goto-char (point-min))
          (search-forward "Task 3")
          (org-back-to-heading t)
          (push (org-id-get-create) task-ids)

          (setq task-ids (nreverse task-ids))

          ;; Get tasks via helper
          (let ((task-markers (org-gtd-project--get-all-tasks project-marker)))
            ;; Should return 3 task markers
            (expect (length task-markers) :to-equal 3)
            ;; Each marker should point to a task with matching ID
            (dolist (marker task-markers)
              (org-with-point-at marker
                (expect (member (org-id-get) task-ids) :to-be-truthy)))))))

(describe "Project tickler"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "ticklers a project with review date"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        (org-back-to-heading t)
        (let ((project-marker (point-marker)))

          ;; Incubate with review date
          (org-gtd-project-incubate project-marker "2025-12-01")

          ;; Verify project heading is incubated
          (org-with-point-at project-marker
            (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")
            (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Projects")
            (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2025-12-01>"))

          ;; Verify all tasks are incubated
          (goto-char (point-min))
          (search-forward "Task 1")
          (org-back-to-heading t)
          (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")
          (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Actions")
          (expect (org-entry-get (point) "TODO") :to-be nil)
          (expect (org-entry-get (point) "PREVIOUS_TODO") :to-equal "NEXT"))))

  (it "reactivates a tickler project"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        (org-back-to-heading t)
        (let ((project-marker (point-marker)))

          ;; First incubate it
          (org-gtd-project-incubate project-marker "2025-12-01")

          ;; Verify it's incubated
          (org-with-point-at project-marker
            (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler"))

          ;; Now reactivate it
          (org-gtd-project-reactivate project-marker)

          ;; Verify project heading is restored
          (org-with-point-at project-marker
            (expect (org-entry-get (point) "ORG_GTD") :to-equal "Projects")
            (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)
            (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-be nil))

          ;; Verify tasks are restored
          (goto-char (point-min))
          (search-forward "Task 1")
          (org-back-to-heading t)
          (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
          (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil)
          ;; TODO keyword should be restored (will be NEXT after recalculation)
          (expect (org-entry-get (point) "TODO") :not :to-be nil))))

  (it "warns about external dependencies before ticklering"
      ;; Create two projects: Project A and Project B
      ;; Task in Project B depends on task in Project A
      ;; Incubating Project A should warn about Project B task

      (create-project "Project A")
      (create-project "Project B")

      (with-current-buffer (org-gtd--default-file)
        ;; Get IDs for tasks
        (goto-char (point-min))
        (re-search-forward "Project A")
        (org-next-visible-heading 1)  ; Task 1 of Project A
        (let ((task-a1-id (org-id-get-create)))

          (goto-char (point-min))
          (re-search-forward "Project B")
          (org-next-visible-heading 1)  ; Task 1 of Project B
          (let ((task-b1-id (org-id-get-create)))

            ;; Make Task B1 depend on Task A1 (external dependency)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a1-id)
            (goto-char (point-min))
            (re-search-forward "Project A")
            (org-next-visible-heading 1)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b1-id)

            ;; Check for external dependencies on Project A
            (goto-char (point-min))
            (re-search-forward "Project A")
            (org-back-to-heading t)
            (let ((external-deps (org-gtd-project--check-external-dependencies (point-marker))))
              ;; Should find Task B1 as external dependency
              (expect (length external-deps) :to-equal 1)
              (expect (org-with-point-at (car external-deps)
                        (org-id-get))
                      :to-equal task-b1-id)))))))

  (it "skips multi-project tasks during tickler"
      ;; Create project with a task that belongs to multiple projects
      (create-project "Project A")
      (create-project "Project B")

      (with-current-buffer (org-gtd--default-file)
        ;; Make Task 1 belong to both projects
        (goto-char (point-min))
        (re-search-forward "Project A")
        (let ((project-a-id (org-id-get-create)))
          (goto-char (point-min))
          (re-search-forward "Project B")
          (let ((project-b-id (org-id-get-create)))

            ;; Add both project IDs to Task 1 of Project A
            (goto-char (point-min))
            (re-search-forward "Project A")
            (org-next-visible-heading 1)  ; Task 1
            (org-entry-put (point) "ORG_GTD_PROJECT_IDS" (format "%s %s" project-a-id project-b-id))
            (let ((task-1-todo (org-entry-get (point) "TODO")))

              ;; Incubate Project A
              (goto-char (point-min))
              (re-search-forward "Project A")
              (org-back-to-heading t)
              (org-gtd-project-incubate (point-marker) "2025-12-01")

              ;; Verify Task 1 was NOT incubated (it belongs to multiple projects)
              (goto-char (point-min))
              (re-search-forward "Project A")
              (org-next-visible-heading 1)
              (expect (org-entry-get (point) "ORG_GTD") :not :to-equal "Tickler")
              (expect (org-entry-get (point) "TODO") :to-equal task-1-todo))))))
)

(describe "org-gtd-project--count-tasks"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each
    (ogt--close-and-delete-files))

  (it "returns (completed . total) for project tasks"
    (create-project "Count test")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Count test")
      (org-back-to-heading t)
      (let* ((project-id (org-entry-get (point) "ID"))
             (counts (org-gtd-project--count-tasks project-id)))
        ;; Initially all tasks are TODO/NEXT, none completed
        (expect (car counts) :to-equal 0)
        (expect (cdr counts) :to-equal 3))))

  (it "counts completed tasks correctly"
    (create-project "Done test")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Done test")
      (org-back-to-heading t)
      (let ((project-id (org-entry-get (point) "ID")))
        ;; Mark first task as DONE
        (search-forward "Task 1")
        (org-todo "DONE")
        (goto-char (point-min))
        (search-forward "Done test")
        (let ((counts (org-gtd-project--count-tasks project-id)))
          (expect (car counts) :to-equal 1)
          (expect (cdr counts) :to-equal 3))))))

(describe "org-gtd-project--format-cookies"
  (it "formats cookies as [completed/total][percent%]"
    (expect (org-gtd-project--format-cookies 3 7) :to-equal "[3/7][42%]"))

  (it "handles zero total gracefully"
    (expect (org-gtd-project--format-cookies 0 0) :to-equal "[0/0][0%]"))

  (it "handles 100% completion"
    (expect (org-gtd-project--format-cookies 5 5) :to-equal "[5/5][100%]")))

(describe "org-gtd-project--set-cookies"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each
    (ogt--close-and-delete-files))

  (describe "with position 'end"
    (before-each
      (setq org-gtd-project-progress-cookie-position 'end))

    (it "inserts cookies at end of heading"
      (with-temp-buffer
        (org-mode)
        (insert "* PROJ Buy a house\n")
        (goto-char (point-min))
        (org-gtd-project--set-cookies 3 7)
        (expect (buffer-substring-no-properties (point-min) (line-end-position))
                :to-match "Buy a house \\[3/7\\]\\[42%\\]")))

    (it "inserts cookies before tags"
      (with-temp-buffer
        (org-mode)
        (insert "* PROJ Buy a house  :home:\n")
        (goto-char (point-min))
        (org-gtd-project--set-cookies 3 7)
        (let ((line (buffer-substring-no-properties (point-min) (line-end-position))))
          (expect line :to-match "\\[3/7\\]\\[42%\\].*:home:")))))

  (describe "with position 'start"
    (before-each
      (setq org-gtd-project-progress-cookie-position 'start))

    (it "inserts cookies at start of heading"
      (with-temp-buffer
        (org-mode)
        (insert "* PROJ Buy a house\n")
        (goto-char (point-min))
        (org-gtd-project--set-cookies 3 7)
        (expect (buffer-substring-no-properties (point-min) (line-end-position))
                :to-match "PROJ \\[3/7\\]\\[42%\\] Buy a house"))))

  (describe "with position nil"
    (before-each
      (setq org-gtd-project-progress-cookie-position nil))

    (it "does nothing when disabled"
      (with-temp-buffer
        (org-mode)
        (insert "* PROJ Buy a house\n")
        (goto-char (point-min))
        (org-gtd-project--set-cookies 3 7)
        (expect (buffer-substring-no-properties (point-min) (line-end-position))
                :to-equal "* PROJ Buy a house")))))

(describe "org-gtd-project--remove-cookies"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each
    (ogt--close-and-delete-files))

  (it "removes cookies from end of heading"
    (with-temp-buffer
      (org-mode)
      (insert "* PROJ Buy a house [3/7][42%]\n")
      (goto-char (point-min))
      (org-gtd-project--remove-cookies)
      (expect (org-get-heading t t t t) :to-equal "PROJ Buy a house")))

  (it "removes cookies from start of heading"
    (with-temp-buffer
      (org-mode)
      (insert "* PROJ [3/7][42%] Buy a house\n")
      (goto-char (point-min))
      (org-gtd-project--remove-cookies)
      (expect (org-get-heading t t t t) :to-equal "PROJ Buy a house")))

  (it "handles heading with no cookies"
    (with-temp-buffer
      (org-mode)
      (insert "* PROJ Buy a house\n")
      (goto-char (point-min))
      (org-gtd-project--remove-cookies)
      (expect (org-get-heading t t t t) :to-equal "PROJ Buy a house")))

  (it "preserves tags when removing cookies"
    (with-temp-buffer
      (org-mode)
      (insert "* PROJ Buy a house [3/7][42%]  :home:finance:\n")
      (goto-char (point-min))
      (org-gtd-project--remove-cookies)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-match ":home:finance:"))))

(describe "org-gtd-project-update-cookies"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs)
    (setq org-gtd-project-progress-cookie-position 'end))
  (after-each
    (ogt--close-and-delete-files))

  (it "updates cookies on project heading"
    (create-project "Cookie test")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Cookie test")
      (org-back-to-heading t)
      (let ((project-id (org-entry-get (point) "ID")))
        (org-gtd-project-update-cookies project-id)
        (expect (buffer-substring-no-properties (point) (line-end-position))
                :to-match "\\[0/3\\]\\[0%\\]")))))

(describe "org-gtd-project--maybe-update-cookies"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs)
    (setq org-gtd-project-progress-cookie-position 'end))
  (after-each
    (ogt--close-and-delete-files))

  (it "updates cookies when task state changes"
    (create-project "Auto cookie")
    (with-current-buffer (org-gtd--default-file)
      ;; Get project ID
      (goto-char (point-min))
      (search-forward "Auto cookie")
      (org-back-to-heading t)
      (let ((project-id (org-entry-get (point) "ID")))
        ;; Set initial cookies manually
        (org-gtd-project-update-cookies project-id)
        (goto-char (point-min))
        (search-forward "Auto cookie")
        (org-back-to-heading t)
        ;; Verify initial state - 0 tasks done
        (expect (buffer-substring-no-properties (point) (line-end-position))
                :to-match "\\[0/3\\]\\[0%\\]")

        ;; Mark first task DONE - this SHOULD trigger hook and update cookies
        (goto-char (point-min))
        (re-search-forward "^\\*+ \\(NEXT\\|TODO\\) ")
        (org-back-to-heading t)
        (org-todo "DONE")

        ;; With hook, cookies SHOULD update to show 1 task done
        (goto-char (point-min))
        (search-forward "Auto cookie")
        (org-back-to-heading t)
        (expect (buffer-substring-no-properties (point) (line-end-position))
                :to-match "\\[1/3\\]\\[33%\\]")

        ;; Mark another task DONE
        (goto-char (point-min))
        (re-search-forward "^\\*+ \\(NEXT\\|TODO\\) ")
        (org-back-to-heading t)
        (org-todo "DONE")

        ;; Cookies should now show 2 tasks done
        (goto-char (point-min))
        (search-forward "Auto cookie")
        (org-back-to-heading t)
        (expect (buffer-substring-no-properties (point) (line-end-position))
                :to-match "\\[2/3\\]\\[66%\\]")))))

(describe "org-gtd-project-update-all-cookies"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs)
    (setq org-gtd-project-progress-cookie-position 'end))
  (after-each
    (ogt--close-and-delete-files))

  (it "updates cookies for all projects"
    (create-project "Project A")
    (create-project "Project B")
    (org-gtd-project-update-all-cookies)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Project A")
      (expect (buffer-substring-no-properties (point) (line-end-position))
              :to-match "\\[[0-9]+/[0-9]+\\]")
      (goto-char (point-min))
      (search-forward "Project B")
      (expect (buffer-substring-no-properties (point) (line-end-position))
              :to-match "\\[[0-9]+/[0-9]+\\]"))))

(describe "fix todo keywords updates cookies"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs)
    (setq org-gtd-project-progress-cookie-position 'end))
  (after-each
    (ogt--close-and-delete-files))

  (it "updates cookies after fixing keywords"
    ;; Create project - it should have cookies initially
    (create-project "Fix keywords test")
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Fix keywords test")
      (org-back-to-heading t)
      (let ((line-after-creation (buffer-substring-no-properties (point) (line-end-position))))
        ;; Project should have cookies from creation
        (expect line-after-creation :to-match "\\[[0-9]+/[0-9]+\\]")))
    ;; Now call fix-all-todo-keywords - cookies should still be there
    (org-gtd-projects-fix-all-todo-keywords)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Fix keywords test")
      (org-back-to-heading t)
      (let ((line-after-fix (buffer-substring-no-properties (point) (line-end-position))))
        ;; Cookies should still be present after fix
        (expect line-after-fix :to-match "\\[[0-9]+/[0-9]+\\]")))))

(describe "project progress cookies integration"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs)
    (setq org-gtd-project-progress-cookie-position 'end))
  (after-each
    (ogt--close-and-delete-files))

  (it "full lifecycle: create, complete tasks, verify cookies"
    ;; Create project
    (create-project "Lifecycle test")

    (with-current-buffer (org-gtd--default-file)
      ;; Count how many tasks the project has
      (goto-char (point-min))
      (search-forward "Lifecycle test")
      (org-back-to-heading t)
      (let* ((project-id (org-entry-get (point) "ID"))
             (initial-counts (org-gtd-project--count-tasks project-id))
             (total-tasks (cdr initial-counts)))
        ;; Verify initial cookies show 0 completed
        (expect (buffer-substring-no-properties (point) (line-end-position))
                :to-match (format "\\[0/%d\\]\\[0%%\\]" total-tasks))

        ;; Find and complete first task
        (goto-char (point-min))
        (re-search-forward "^\\*+ \\(NEXT\\|TODO\\) ")
        (org-back-to-heading t)
        ;; Make sure we're on a task belonging to this project
        (when (member project-id (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))
          (org-todo "DONE"))

        ;; Verify cookies updated to show 1 completed
        (goto-char (point-min))
        (search-forward "Lifecycle test")
        (org-back-to-heading t)
        (expect (buffer-substring-no-properties (point) (line-end-position))
                :to-match (format "\\[1/%d\\]" total-tasks))

        ;; Complete all remaining tasks
        (let ((completed 1))
          (while (< completed total-tasks)
            (goto-char (point-min))
            (when (re-search-forward "^\\*+ \\(NEXT\\|TODO\\) " nil t)
              (org-back-to-heading t)
              (when (member project-id (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))
                (org-todo "DONE")
                (setq completed (1+ completed))))))

        ;; Verify 100% completion
        (goto-char (point-min))
        (search-forward "Lifecycle test")
        (org-back-to-heading t)
        (expect (buffer-substring-no-properties (point) (line-end-position))
                :to-match (format "\\[%d/%d\\]\\[100%%\\]" total-tasks total-tasks))))))
