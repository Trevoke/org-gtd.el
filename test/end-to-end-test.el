;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

;; Basic workflow tests migrated to test-eunit/acceptance/basic-workflows-test.el
;; Cancel and archive tests migrated to test-eunit/acceptance/cancel-archive-test.el
;; Review flow tests migrated to test-eunit/acceptance/review-flow-test.el (including multi-file)
;; Habit flow tests migrated to test-eunit/acceptance/habit-flow-test.el
;; Multi-file DAG tests migrated to test-eunit/acceptance/multi-file-dag-test.el
;; NOTE: The buttercup tests had incomplete setup - they didn't set ORG_GTD_PROJECT_IDS
;;       on secondary file tasks. The e-unit versions properly set up bi-directional
;;       relationships so archiving works correctly across files.

(describe "Advanced Project Task Operations"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Add blocker relationship"
    (it "creates dependency between existing tasks"
        (capture-inbox-item "Website redesign")
        (org-gtd-process-inbox)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Design wireframes" :level 2)
          (make-task "Get client approval" :level 2)
          (make-task "Build prototype" :level 2)
          (organize-as-project))
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
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" approval-id)
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
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Build prototype")
          (org-back-to-heading t)
          (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
            (expect depends-on :not :to-be nil)
            (expect (length depends-on) :to-equal 1))
          (goto-char (point-min))
          (search-forward "Get client approval")
          (org-back-to-heading t)
          (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
            (expect blocks :not :to-be nil)
            (expect (length blocks) :to-equal 1)))))

  (describe "Remove blocker relationship"
    (it "removes existing dependency between tasks"
        (capture-inbox-item "Product launch")
        (org-gtd-process-inbox)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Write documentation" :level 2)
          (make-task "Record demo video" :level 2)
          (organize-as-project))
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
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" doc-id)
            (save-excursion
              (goto-char (point-min))
              (search-forward "Write documentation")
              (org-back-to-heading t)
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" video-id))
            (goto-char (point-min))
            (search-forward "Record demo video")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :not :to-be nil)
            (org-entry-remove-from-multivalued-property (point) "ORG_GTD_DEPENDS_ON" doc-id)
            (save-excursion
              (goto-char (point-min))
              (search-forward "Write documentation")
              (org-back-to-heading t)
              (org-entry-remove-from-multivalued-property (point) "ORG_GTD_BLOCKS" video-id))
            (goto-char (point-min))
            (search-forward "Record demo video")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-be nil)
            (goto-char (point-min))
            (search-forward "Write documentation")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-be nil)))))

  (describe "Extract task from project (convert to single action)"
    (xit "removes task from project and creates standalone single action"
        "Feature not implemented: extract task from project to single action"))

  (describe "Add first task to existing project"
    (it "adds new task to ORG_GTD_FIRST_TASKS of existing project"
        (capture-inbox-item "Marketing campaign")
        (org-gtd-process-inbox)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Create content calendar" :level 2)
          (organize-as-project))
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Marketing campaign")
          (org-back-to-heading t)
          (org-insert-heading-after-current)
          (insert "Design landing page")
          (org-do-demote)
          (org-todo "NEXT")  ; Mark the new task as NEXT so it shows in agenda
          (let ((new-task-id (org-id-get-create)))
            (org-up-heading-safe)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" new-task-id)
            (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
              (expect first-tasks :to-contain new-task-id))
            (org-gtd-engage)
            (expect (agenda-raw-text)
                    :to-match "Design landing page")))))

  (describe "Move task within project"
    (xit "changes task's position in dependency graph"
        "Feature not implemented: move task within project DAG")))

(describe "Orphaned Task Detection"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Orphaned task in project"
    (xit "detects task unreachable from project's ORG_GTD_FIRST_TASKS"
        ;; NOTE: org-gtd-validate-project-dependencies has a bug - it calls
        ;; org-gtd-agenda-files which doesn't exist. The function should call
        ;; org-gtd-core--agenda-files or org-agenda-files instead.
        ;; This test is marked as pending until the product bug is fixed.

        "Product bug: org-gtd-validate-project-dependencies calls non-existent org-gtd-agenda-files")))

(describe "Graph Validation Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Circular dependency detection during organize"
    (xit "prevents creating A→B→C→A cycle"
        ;; NOTE: Circular dependency detection is NOT currently implemented for
        ;; manual property manipulation. org-entry-add-to-multivalued-property
        ;; doesn't check for cycles. A validation command exists
        ;; (org-gtd-validate-project-dependencies) but it has a bug.
        ;;
        ;; EXPECTED: Adding a dependency that creates a cycle should raise user-error
        ;; ACTUAL: No error is raised; cycles can be created
        ;;
        ;; This test documents the expected behavior for when it's implemented.

        "Feature not implemented: circular dependency prevention during property manipulation"))

  (describe "Circular dependency detection in existing project"
    (xit "validates project and detects manually introduced cycle"
        ;; NOTE: This relies on org-gtd-validate-project-dependencies which currently
        ;; has a bug (calls non-existent org-gtd-agenda-files). Pending fix.

        "Product bug: org-gtd-validate-project-dependencies needs fixing"))

  (describe "Orphaned task detection via validation"
    (xit "runs validation command and finds unreachable tasks"
        ;; NOTE: This also relies on org-gtd-validate-project-dependencies

        "Product bug: org-gtd-validate-project-dependencies needs fixing")))

(describe "Multi-project Task Sharing Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Share task between projects"
    (it "creates two projects with same task ID in both ORG_GTD_FIRST_TASKS"
        (capture-inbox-item "Project Alpha")
        (org-gtd-process-inbox)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Design database schema" :level 2)
          (organize-as-project))
        (capture-inbox-item "Project Beta")
        (org-gtd-process-inbox)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Implement API" :level 2)
          (organize-as-project))
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Design database schema")
          (org-back-to-heading t)
          (let ((shared-task-id (org-id-get-create)))
            (goto-char (point-min))
            (search-forward "Project Beta")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)
            (org-gtd-engage)
            (let ((agenda-content (agenda-raw-text)))
              (expect agenda-content :to-match "Design database schema")
              (expect agenda-content :to-match "Implement API"))
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Project Alpha")
              (org-back-to-heading t)
              (let ((alpha-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
                (expect alpha-tasks :to-contain shared-task-id))
              (goto-char (point-min))
              (search-forward "Project Beta")
              (org-back-to-heading t)
              (let ((beta-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
                (expect beta-tasks :to-contain shared-task-id)))))))

  (describe "Complete shared task"
    (it "marks task complete and verifies both projects recognize it as done"
        ;; Create two projects and share a task between them via ORG_GTD_FIRST_TASKS
        (capture-inbox-item "Project Alpha")
        (org-gtd-process-inbox)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Shared Task" :level 2)
          (organize-as-project))

        (capture-inbox-item "Project Beta")
        (org-gtd-process-inbox)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Another Task" :level 2)
          (organize-as-project))

        ;; Share "Shared Task" with Project Beta
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Shared Task")
          (org-back-to-heading t)
          (let ((shared-task-id (org-id-get-create))
                (project-beta-id nil))

            ;; Get Project Beta's ID
            (goto-char (point-min))
            (search-forward "Project Beta")
            (org-back-to-heading t)
            (setq project-beta-id (org-id-get-create))

            ;; Add shared task to Project Beta's FIRST_TASKS
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)

            ;; Add Project Beta's ID to shared task's PROJECT_IDS
            (goto-char (point-min))
            (search-forward "Shared Task")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-beta-id)

            ;; Complete the shared task
            (org-todo "DONE")

            ;; Verify both projects have no pending next actions
            (org-gtd-engage)
            (let ((agenda-content (agenda-raw-text)))
              ;; Shared task should not appear (it's DONE)
              (expect agenda-content :not :to-match "Shared Task")
              ;; "Another Task" from Project Beta should still appear
              (expect agenda-content :to-match "Another Task")))))))

(describe "Project Cancellation and Archive Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Project tasks: cancel (within project DAG)"
    (it "organizes project, cancels one task, verifies not in engage, then archives"
        ;; This test verifies that when you cancel a task within a project:
        ;; 1. The canceled task doesn't appear in engage views
        ;; 2. The project continues to function with remaining tasks
        ;; 3. After completing remaining tasks, the entire project can be archived

        ;; 1. CAPTURE
        (capture-inbox-item "Organize conference")

        ;; 2. PROCESS
        (org-gtd-process-inbox)

        ;; 3. ORGANIZE (project with tasks)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Book conference room" :level 2)
          (make-task "Send speaker invites" :level 2)
          (make-task "Order catering" :level 2)
          (organize-as-project))

        ;; 4. VERIFY all tasks appear in engage before cancellation
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :to-match "Organize co")  ; Project heading (truncated)
          (expect agenda-content :to-match "Book conference room"))

        ;; 5. CANCEL one task (Order catering)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Order catering")
          (org-todo "CNCL"))

        ;; 6. VERIFY canceled task doesn't show in engage (CRITICAL GTD REQUIREMENT)
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          ;; Project heading should still show (has incomplete tasks)
          (expect agenda-content :to-match "Organize co")
          ;; Canceled task should NOT show
          (expect agenda-content :not :to-match "Order catering")
          ;; Other tasks should still show
          (expect agenda-content :to-match "Book conference room"))

        ;; 7. COMPLETE remaining tasks
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Book conference room")
          (org-todo "DONE")
          (goto-char (point-min))
          (re-search-forward "Send speaker invites")
          (org-todo "DONE"))

        ;; 8. ARCHIVE - project should archive even with one canceled task
        (org-gtd-archive-completed-items)

        ;; 9. VERIFY project and all tasks are archived
        (with-current-buffer (org-gtd--default-file)
          (expect (current-buffer-raw-text)
                  :not :to-match "Organize conference"))))

  (describe "Project: archive (cncl) - DAG same file"
    (it "organizes project, cancels all tasks, verifies nothing in engage, then archives"
        ;; This test verifies that:
        ;; 1. Projects with ALL tasks canceled don't appear in engage
        ;; 2. Such projects can be properly archived
        ;; 3. All tasks are in the same file (simple DAG case)

        ;; 1. CAPTURE
        (capture-inbox-item "Abandoned initiative")

        ;; 2. PROCESS
        (org-gtd-process-inbox)

        ;; 3. ORGANIZE (project with multiple tasks in same file)
        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Research market" :level 2)
          (make-task "Build prototype" :level 2)
          (make-task "Pitch to investors" :level 2)
          (organize-as-project))

        ;; 4. VERIFY all tasks appear in engage before cancellation
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :to-match "Abandoned i")  ; Project heading (truncated)
          (expect agenda-content :to-match "Research market"))

        ;; 5. CANCEL all tasks
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Research market")
          (org-todo "CNCL")
          (goto-char (point-min))
          (re-search-forward "Build prototype")
          (org-todo "CNCL")
          (goto-char (point-min))
          (re-search-forward "Pitch to investors")
          (org-todo "CNCL"))

        ;; 6. VERIFY nothing shows in engage after canceling all tasks
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :not :to-match "Abandoned i")
          (expect agenda-content :not :to-match "Research market")
          (expect agenda-content :not :to-match "Build prototype")
          (expect agenda-content :not :to-match "Pitch to investors"))

        ;; 7. ARCHIVE
        (org-gtd-archive-completed-items)

        ;; 8. VERIFY project and all tasks are archived
        (with-current-buffer (org-gtd--default-file)
          (let ((content (current-buffer-raw-text)))
            (expect content :not :to-match "Abandoned initiative")
            (expect content :not :to-match "Research market")
            (expect content :not :to-match "Build prototype")
            (expect content :not :to-match "Pitch to investors")))

        ;; 9. VERIFY items are in archive
        (let ((archived-content (ogt--archive-string)))
          (expect archived-content :to-match "Abandoned initiative")
          (expect archived-content :to-match "Research market")
          (expect archived-content :to-match "Build prototype")
          (expect archived-content :to-match "Pitch to investors"))))

  (describe "Project: archive (cncl) - DAG multiple files"
    (it "organizes project with multi-file tasks, cancels all tasks, archives"
        ;; This test verifies that:
        ;; 1. Projects with tasks across multiple files can have all tasks canceled
        ;; 2. All canceled tasks (across files) don't appear in engage
        ;; 3. The entire multi-file project can be archived

        ;; 1. CAPTURE and ORGANIZE project in main file
        (capture-inbox-item "Cross-file abandoned project")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Main file task" :level 2)
          (organize-as-project))

        ;; 2. Create second file with related task
        (let ((second-file (org-gtd--path "abandoned-secondary")))
          (with-temp-file second-file
            (make-task "Secondary file task" :id "abandoned-task-id" :level 1))

          (with-current-buffer (find-file-noselect second-file)
            (org-mode)
            (goto-char (point-min))
            (search-forward "Secondary file task")
            (org-back-to-heading t)
            (org-id-add-location "abandoned-task-id" second-file)
            (org-todo "NEXT"))

          (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

            ;; 3. Link task from second file to project
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Cross-file abandoned project")
              (org-back-to-heading t)
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "abandoned-task-id"))

            ;; 4. VERIFY both tasks show in engage before cancellation
            (org-gtd-engage)
            (let ((agenda-content (agenda-raw-text)))
              (expect agenda-content :to-match "Main file task")
              (expect agenda-content :to-match "Secondary file task"))

            ;; 5. CANCEL all tasks
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Main file task")
              (org-todo "CNCL"))

            (with-current-buffer (find-file-noselect second-file)
              (goto-char (point-min))
              (search-forward "Secondary file task")
              (org-todo "CNCL"))

            ;; 6. VERIFY nothing shows in engage after canceling all tasks
            (org-gtd-engage)
            (let ((agenda-content (agenda-raw-text)))
              (expect agenda-content :not :to-match "Cross-file abandoned")
              (expect agenda-content :not :to-match "Main file task")
              (expect agenda-content :not :to-match "Secondary file task"))

            ;; 7. ARCHIVE
            (org-gtd-archive-completed-items)

            ;; 8. VERIFY project is archived from main file
            (with-current-buffer (org-gtd--default-file)
              (expect (current-buffer-raw-text)
                      :not :to-match "Cross-file abandoned project"))

            ;; 9. VERIFY task in second file is archived
            (with-current-buffer (find-file-noselect second-file)
              (expect (current-buffer-raw-text)
                      :not :to-match "Secondary file task"))))))

  (describe "Project tasks: cancel (multi-project)"
    (it "organizes 2 projects with shared task, cancels task, verifies both projects"
        ;; This test verifies that:
        ;; 1. A task can be shared between multiple projects via ORG_GTD_FIRST_TASKS
        ;; 2. When the shared task is canceled, it doesn't appear in either project's engage view
        ;; 3. Both projects continue to function with their remaining tasks

        ;; 1. CAPTURE and ORGANIZE first project
        (capture-inbox-item "Project Alpha")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Alpha task 1" :level 2)
          (make-task "Shared task" :level 2)
          (organize-as-project))

        ;; 2. CAPTURE and ORGANIZE second project
        (capture-inbox-item "Project Beta")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Beta task 1" :level 2)
          (organize-as-project))

        ;; 3. Share "Shared task" with Project Beta
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Shared task")
          (org-back-to-heading t)
          (let ((shared-task-id (org-id-get-create))
                (project-alpha-id nil)
                (project-beta-id nil))

            ;; Get Project Alpha's ID
            (goto-char (point-min))
            (search-forward "Project Alpha")
            (org-back-to-heading t)
            (setq project-alpha-id (org-id-get-create))

            ;; Get Project Beta's ID
            (goto-char (point-min))
            (search-forward "Project Beta")
            (org-back-to-heading t)
            (setq project-beta-id (org-id-get-create))

            ;; Add shared task to Project Beta's FIRST_TASKS
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)

            ;; Add both project IDs to shared task's PROJECT_IDS
            (goto-char (point-min))
            (search-forward "Shared task")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-alpha-id)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-beta-id)
            ;; Set task to NEXT so it appears in engage
            (org-todo "NEXT")

            ;; 4. VERIFY shared task appears in engage
            (org-gtd-engage)
            (let ((agenda-content (agenda-raw-text)))
              (expect agenda-content :to-match "Shared task")
              (expect agenda-content :to-match "Alpha task 1")
              (expect agenda-content :to-match "Beta task 1"))

            ;; 5. CANCEL the shared task
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Shared task")
              (org-todo "CNCL"))

            ;; 6. VERIFY shared task doesn't appear in engage, but other tasks do
            (org-gtd-engage)
            (let ((agenda-content (agenda-raw-text)))
              (expect agenda-content :not :to-match "Shared task")
              (expect agenda-content :to-match "Alpha task 1")
              (expect agenda-content :to-match "Beta task 1"))

            ;; 7. COMPLETE remaining tasks to verify projects still work
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Alpha task 1")
              (org-todo "DONE")
              (goto-char (point-min))
              (search-forward "Beta task 1")
              (org-todo "DONE"))

            ;; 8. ARCHIVE
            (org-gtd-archive-completed-items)

            ;; 9. VERIFY both projects are archived
            (with-current-buffer (org-gtd--default-file)
              (expect (current-buffer-raw-text)
                      :not :to-match "Project Alpha")
              (expect (current-buffer-raw-text)
                      :not :to-match "Project Beta")))))

  (describe "Project: archive (cncl) - DAG with shared tasks"
    (it "organizes 2 projects, shares task, cancels all, verifies partial archive"
        ;; This test verifies that:
        ;; 1. Two projects can share a task
        ;; 2. When all tasks in one project are canceled, that project can be archived
        ;; 3. The shared task remains if the other project still needs it
        ;; 4. When both projects have all tasks canceled, both can be archived

        ;; 1. CAPTURE and ORGANIZE first project
        (capture-inbox-item "Project Gamma")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Gamma unique task" :level 2)
          (make-task "Shared infrastructure task" :level 2)
          (organize-as-project))

        ;; 2. CAPTURE and ORGANIZE second project
        (capture-inbox-item "Project Delta")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Delta unique task" :level 2)
          (organize-as-project))

        ;; 3. Share "Shared infrastructure task" with Project Delta
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Shared infrastructure task")
          (org-back-to-heading t)
          (let ((shared-task-id (org-id-get-create))
                (project-gamma-id nil)
                (project-delta-id nil))

            ;; Get Project Gamma's ID
            (goto-char (point-min))
            (search-forward "Project Gamma")
            (org-back-to-heading t)
            (setq project-gamma-id (org-id-get-create))

            ;; Get Project Delta's ID
            (goto-char (point-min))
            (search-forward "Project Delta")
            (org-back-to-heading t)
            (setq project-delta-id (org-id-get-create))

            ;; Add shared task to Project Delta's FIRST_TASKS
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" shared-task-id)

            ;; Add both project IDs to shared task's PROJECT_IDS
            (goto-char (point-min))
            (search-forward "Shared infrastructure task")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-gamma-id)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-delta-id)

            ;; 4. CANCEL all tasks in Project Gamma only
            (goto-char (point-min))
            (search-forward "Gamma unique task")
            (org-todo "CNCL")
            (goto-char (point-min))
            (search-forward "Shared infrastructure task")
            (org-todo "CNCL")

            ;; 5. VERIFY shared task doesn't appear in engage (both instances canceled)
            (org-gtd-engage)
            (let ((agenda-content (agenda-raw-text)))
              (expect agenda-content :not :to-match "Gamma unique task")
              (expect agenda-content :not :to-match "Shared infrastructure task")
              ;; Delta's task should still appear
              (expect agenda-content :to-match "Delta unique task"))

            ;; 6. Try to ARCHIVE - Project Gamma should archive
            (org-gtd-archive-completed-items)

            (with-current-buffer (org-gtd--default-file)
              (let ((content (current-buffer-raw-text)))
                ;; Project Gamma project heading should be archived
                (expect content :not :to-match "\\* Project Gamma")
                ;; Shared task should REMAIN (Project Delta still needs it)
                (expect content :to-match "Shared infrastructure task")
                ;; Project Delta should still exist (has incomplete task)
                (expect content :to-match "Project Delta")
                (expect content :to-match "Delta unique task")))

            ;; 7. Now CANCEL Project Delta's remaining task
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Delta unique task")
              (org-todo "CNCL"))

            ;; 8. ARCHIVE again
            (org-gtd-archive-completed-items)

            ;; 9. VERIFY everything is archived
            (with-current-buffer (org-gtd--default-file)
              (let ((content (current-buffer-raw-text)))
                (expect content :not :to-match "Project Gamma")
                (expect content :not :to-match "Project Delta")
                (expect content :not :to-match "Gamma unique task")
                (expect content :not :to-match "Delta unique task")
                (expect content :not :to-match "Shared infrastructure task")))))))

  (describe "Project: stuck projects do NOT get archived"
    (it "verifies stuck project remains in main file, then archives when completed"
        ;; This test verifies the critical distinction between stuck and completed projects:
        ;; STUCK PROJECT (has TODO tasks, no NEXT/WAIT):
        ;;   - Has work remaining but no actionable tasks
        ;;   - Should NOT be archived (needs attention to plan next steps)
        ;; COMPLETED PROJECT (all tasks DONE/CNCL):
        ;;   - All work finished
        ;;   - SHOULD be archived

        ;; 1. CAPTURE and ORGANIZE project
        (capture-inbox-item "Marketing Campaign")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Research target audience" :level 2)
          (make-task "Create content" :level 2)
          (make-task "Launch campaign" :level 2)
          (organize-as-project))

        ;; 2. Make project STUCK by ensuring all tasks are TODO (not NEXT or WAIT)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Research target audience")
          (org-todo "TODO")
          (goto-char (point-min))
          (re-search-forward "Create content")
          (org-todo "TODO")
          (goto-char (point-min))
          (re-search-forward "Launch campaign")
          (org-todo "TODO"))

        ;; 3. VERIFY project appears in stuck projects review
        (org-gtd-review-stuck-projects)
        (expect (agenda-raw-text)
                :to-match "Marketing C")  ; Project heading (truncated)

        ;; 4. Try to ARCHIVE - stuck project should NOT be archived
        (org-gtd-archive-completed-items)

        ;; 5. VERIFY project is still in main file (NOT archived)
        (with-current-buffer (org-gtd--default-file)
          (let ((content (current-buffer-raw-text)))
            (expect content :to-match "Marketing Campaign")
            (expect content :to-match "Research target audience")
            (expect content :to-match "Create content")
            (expect content :to-match "Launch campaign")))

        ;; 6. VERIFY project is NOT in archive file
        (let ((archived-content (ogt--archive-string)))
          (expect archived-content :not :to-match "Marketing Campaign"))

        ;; 7. Now COMPLETE all tasks to make project archivable
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Research target audience")
          (org-todo "DONE")
          (goto-char (point-min))
          (re-search-forward "Create content")
          (org-todo "DONE")
          (goto-char (point-min))
          (re-search-forward "Launch campaign")
          (org-todo "DONE"))

        ;; 8. VERIFY project does NOT appear in stuck projects review anymore
        (org-gtd-review-stuck-projects)
        (expect (agenda-raw-text)
                :not :to-match "Marketing C")

        ;; 9. ARCHIVE again - completed project SHOULD now be archived
        (org-gtd-archive-completed-items)

        ;; 10. VERIFY project is NO LONGER in main file
        (with-current-buffer (org-gtd--default-file)
          (let ((content (current-buffer-raw-text)))
            (expect content :not :to-match "Marketing Campaign")
            (expect content :not :to-match "Research target audience")
            (expect content :not :to-match "Create content")
            (expect content :not :to-match "Launch campaign")))

        ;; 11. VERIFY project IS now in archive file
        (let ((archived-content (ogt--archive-string)))
          (expect archived-content :to-match "Marketing Campaign"))))
))

(describe "Multi-file Review and Validation Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Project: view stuck (DAG multiple files)"
    (it "detects stuck project with tasks in multiple files"
        ;; This test verifies that stuck project detection works correctly when:
        ;; 1. Project heading is in main GTD file
        ;; 2. Tasks are distributed across multiple files
        ;; 3. All tasks are in TODO state (none in NEXT), making project stuck

        ;; 1. CAPTURE and ORGANIZE project in main file
        (capture-inbox-item "Multi-file stuck project")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Task in main file" :level 2)
          (organize-as-project))

        ;; 2. Create second file with task
        (let ((second-file (org-gtd--path "stuck-secondary")))
          (with-temp-file second-file
            (make-task "Task in second file" :id "stuck-task-id" :level 1))

          (with-current-buffer (find-file-noselect second-file)
            (org-mode)
            (goto-char (point-min))
            (search-forward "Task in second file")
            (org-back-to-heading t)
            (org-id-add-location "stuck-task-id" second-file)
            (org-todo "TODO"))  ; Make it TODO, not NEXT - this makes project stuck

          (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

            ;; 3. Link task from second file to project
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Multi-file stuck project")
              (org-back-to-heading t)
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "stuck-task-id"))

            ;; 4. Make main file task TODO (not NEXT) so project is stuck
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Task in main file")
              (org-todo "TODO"))

            ;; 5. VERIFY project appears in stuck projects review
            (org-gtd-review-stuck-projects)
            (expect (agenda-raw-text)
                    :to-match "Multi-file stuck project")))))

  (describe "Project: validate graph (DAG multiple files)"
    (it "detects broken references in multi-file projects"
        ;; This test verifies that validation detects broken task references when:
        ;; 1. Project spans multiple files
        ;; 2. A task has a BLOCKS property pointing to a non-existent ID
        ;; 3. Validation correctly identifies the broken reference

        ;; 1. CAPTURE and ORGANIZE project in main file
        (capture-inbox-item "Multi-file validation test")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Main file task" :level 2)
          (organize-as-project))

        ;; 2. Create second file with task that has broken reference
        (let ((second-file (org-gtd--path "validation-secondary")))
          (with-temp-file second-file
            (make-task "Task with broken ref" :id "valid-task-id" :blocks "non-existent-id" :level 1))

          (with-current-buffer (find-file-noselect second-file)
            (org-mode)
            (goto-char (point-min))
            (search-forward "Task with broken ref")
            (org-back-to-heading t)
            (org-id-add-location "valid-task-id" second-file)
            (org-todo "NEXT"))

          (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

            ;; 3. Link task from second file to project
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Multi-file validation test")
              (org-back-to-heading t)
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "valid-task-id"))

            ;; 4. Run validation
            (let* ((health-results (org-gtd-validate-project-dependencies)))

              ;; 5. VERIFY broken reference is detected
              (expect (plist-get health-results :broken-references) :not :to-be nil)
              (let ((broken-refs (plist-get health-results :broken-references)))
                (expect (cl-some (lambda (ref)
                                   (string= (plist-get ref :missing-task) "non-existent-id"))
                                 broken-refs) :to-be-truthy))))))))

(describe "Project Task Operation Tests (Manual Workflows)"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Adding tasks to project DAG"

    (it "Test 1: inserts task B between existing A→C chain to create A→B→C"
        ;; TODO: This test documents the manual workflow required until org-gtd-project-modify-structure exists
        ;;
        ;; MANUAL WORKFLOW REQUIRED (until org-gtd-project-modify-structure exists):
        ;; User must execute these M-x commands in sequence:
        ;; 1. M-x org-gtd-project-extend → add task B to project
        ;; 2. On task B: M-x org-gtd-task-add-blockers → select task A as parent
        ;; 3. On task B: M-x org-gtd-task-add-dependents → select task C as child
        ;; 4. On task C: M-x org-gtd-task-remove-blockers → deselect task A
        ;; Result: A → B → C dependency chain established
        ;;
        ;; Test setup: Create project with tasks A and C where A→C
        ;; Test execution: Add task B and establish A→B→C using commands above
        ;; Test verification: Only A is NEXT, B and C are TODO, completing A makes B NEXT

        ;; 1. CAPTURE and ORGANIZE initial project with tasks A and C
        (capture-inbox-item "Project Alpha")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Task A" :level 2)
          (make-task "Task C" :level 2)
          (organize-as-project))

        ;; 2. Manually create A→C dependency in initial setup
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task C")
          (org-back-to-heading t)
          (let ((task-c-id (org-id-get-create)))

            ;; Add A as blocker of C
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (let ((task-a-id (org-id-get-create)))
              ;; Create A→C relationship
              (goto-char (point-min))
              (search-forward "Task C")
              (org-back-to-heading t)
              (org-entry-put (point) "ORG_GTD_DEPENDS_ON" task-a-id)
              (goto-char (point-min))
              (search-forward "Task A")
              (org-back-to-heading t)
              (org-entry-put (point) "ORG_GTD_BLOCKS" task-c-id))))

        ;; 3. Verify initial state: A is NEXT, C is TODO
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :to-match "Task A")
          (expect agenda-content :not :to-match "Task C"))

        ;; 4. SIMULATE MANUAL WORKFLOW: Add task B using org-gtd-project-extend
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        ;; Mock the project selection in org-gtd-project-extend
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Alpha")
          (org-back-to-heading t)
          (let ((project-id (org-id-get-create))
                (project-point (point-marker)))

            ;; Simulate org-gtd-project-extend by refiling Task B under Project Alpha
            (with-wip-buffer
              (goto-char (point-min))
              (search-forward "Task B")
              (org-back-to-heading t)
              (spy-on 'org-refile-get-location
                      :and-return-value (list "Project Alpha"
                                              (buffer-file-name (marker-buffer project-point))
                                              nil
                                              (marker-position project-point)))
              (org-gtd-project-extend))))

        ;; 5. Get IDs for all tasks
        (let ((task-a-id nil)
              (task-b-id nil)
              (task-c-id nil))
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (setq task-a-id (org-entry-get (point) "ID"))

            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (setq task-b-id (org-entry-get (point) "ID"))

            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (setq task-c-id (org-entry-get (point) "ID"))

            ;; 6. STEP 2: On task B, add A as blocker (manually using ORG_GTD_ properties)
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)

            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)

            ;; 7. STEP 3: On task B, add C as dependent (manually using ORG_GTD_ properties)
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-c-id)

            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            ;; First remove A as blocker, then add B
            (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-b-id)

            ;; Also remove B from A's blocks list
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-entry-remove-from-multivalued-property (point) "ORG_GTD_BLOCKS" task-c-id)

            ;; Update project's first tasks and fix TODO keywords
            (goto-char (point-min))
            (search-forward "Project Alpha")
            (org-back-to-heading t)
            (org-gtd-projects--set-first-tasks)
            (org-gtd-projects-fix-todo-keywords (point-marker)))

          ;; 9. VERIFY: A→B→C chain established
          (with-current-buffer (org-gtd--default-file)
            ;; Task A blocks B
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-equal (list task-b-id))

            ;; Task B depends on A and blocks C
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal (list task-a-id))
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-equal (list task-c-id))

            ;; Task C depends on B (not A)
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal (list task-b-id)))

          ;; 10. VERIFY agenda: Only A is NEXT
          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (expect agenda-content :to-match "Task A")
            (expect agenda-content :not :to-match "Task B")
            (expect agenda-content :not :to-match "Task C"))

          ;; 11. VERIFY workflow: Complete A, then B becomes NEXT
          ;; org-edna automatically updates TODO states when dependencies are satisfied
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-todo "DONE"))
          ;; org-edna should automatically mark Task B as NEXT here

          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (expect agenda-content :not :to-match "Task A")
            (expect agenda-content :to-match "Task B")
            (expect agenda-content :not :to-match "Task C"))))

    (it "Test 2: adds task C as leaf at end of A→B dependency chain"
        ;; TODO: This test documents the manual workflow required until org-gtd-project-modify-structure exists
        ;;
        ;; MANUAL WORKFLOW REQUIRED:
        ;; 1. M-x org-gtd-project-extend → add new leaf task
        ;; 2. On new task: M-x org-gtd-task-add-blockers → select last task in chain as parent
        ;; Result: Task added at end of chain
        ;;
        ;; Test setup: Create project with A→B chain
        ;; Test execution: Add task C as leaf using commands above
        ;; Test verification: A→B→C chain, only A is NEXT

        ;; 1. CAPTURE and ORGANIZE initial project with A→B
        (capture-inbox-item "Project Beta")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Task A" :level 2)
          (make-task "Task B" :level 2)
          (organize-as-project))

        ;; 2. Create A→B dependency
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task A")
          (org-back-to-heading t)
          (let ((task-a-id (org-id-get-create)))
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (let ((task-b-id (org-id-get-create)))
              (org-entry-put (point) "ORG_GTD_DEPENDS_ON" task-a-id)
              (goto-char (point-min))
              (search-forward "Task A")
              (org-back-to-heading t)
              (org-entry-put (point) "ORG_GTD_BLOCKS" task-b-id))))

        ;; 3. Add Task C using project-extend
        (capture-inbox-item "Task C")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Beta")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))

            ;; Refile Task C to project
            (with-wip-buffer
              (goto-char (point-min))
              (search-forward "Task C")
              (org-back-to-heading t)
              (spy-on 'org-refile-get-location
                      :and-return-value (list "Project Beta"
                                              (buffer-file-name (marker-buffer project-point))
                                              nil
                                              (marker-position project-point)))
              (org-gtd-project-extend))))

        ;; 4. SIMULATE MANUAL WORKFLOW: Add B as blocker of C
        (let ((task-b-id nil)
              (task-c-id nil))
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (setq task-b-id (org-entry-get (point) "ID"))

            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (setq task-c-id (org-id-get-create))

            ;; Add B as blocker of C
            (spy-on 'org-gtd-task-management--select-multiple-task-ids
                    :and-return-value (list task-b-id))
            (org-gtd-task-add-blockers))

          ;; 5. VERIFY A→B→C chain
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal (list task-b-id))

            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-equal (list task-c-id)))

          ;; 6. VERIFY only A is NEXT
          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (expect agenda-content :to-match "Task A")
            (expect agenda-content :not :to-match "Task B")
            (expect agenda-content :not :to-match "Task C"))))

    (it "Test 3: adds parallel tasks B and C as children of A in same file"
        ;; TODO: This test documents the manual workflow required until org-gtd-project-modify-structure exists
        ;;
        ;; MANUAL WORKFLOW REQUIRED:
        ;; 1. M-x org-gtd-project-extend → add task B
        ;; 2. M-x org-gtd-project-extend → add task C
        ;; 3. On task B: M-x org-gtd-task-add-blockers → select task A as parent
        ;; 4. On task C: M-x org-gtd-task-add-blockers → select task A as parent
        ;; Result: A has two children (B and C) that can run concurrently
        ;;
        ;; Test setup: Create project with task A
        ;; Test execution: Add B and C as parallel children of A
        ;; Test verification: All three tasks NEXT simultaneously in engage view

        ;; 1. CAPTURE and ORGANIZE initial project with A
        (capture-inbox-item "Project Gamma")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Task A" :level 2)
          (organize-as-project))

        ;; 2. Add Task B using project-extend
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Gamma")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))
            (with-wip-buffer
              (goto-char (point-min))
              (search-forward "Task B")
              (org-back-to-heading t)
              (spy-on 'org-refile-get-location
                      :and-return-value (list "Project Gamma"
                                              (buffer-file-name (marker-buffer project-point))
                                              nil
                                              (marker-position project-point)))
              (org-gtd-project-extend))))

        ;; 3. Add Task C using project-extend
        (capture-inbox-item "Task C")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Gamma")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))
            (with-wip-buffer
              (goto-char (point-min))
              (search-forward "Task C")
              (org-back-to-heading t)
              (spy-on 'org-refile-get-location
                      :and-return-value (list "Project Gamma"
                                              (buffer-file-name (marker-buffer project-point))
                                              nil
                                              (marker-position project-point)))
              (org-gtd-project-extend))))

        ;; 4. SIMULATE MANUAL WORKFLOW: Make A parent of both B and C (using ORG_GTD_ properties)
        (let ((task-a-id nil)
              (task-b-id nil)
              (task-c-id nil))
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (setq task-a-id (org-id-get-create))

            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (setq task-b-id (org-id-get-create))

            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (setq task-c-id (org-id-get-create))

            ;; Add A as blocker of B
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)

            ;; Add A as blocker of C
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)

            ;; Update A's BLOCKS to include both B and C
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-c-id)

            ;; Update project's first tasks and fix TODO keywords
            (goto-char (point-min))
            (search-forward "Project Gamma")
            (org-back-to-heading t)
            (org-gtd-projects--set-first-tasks)
            (org-gtd-projects-fix-todo-keywords (point-marker)))

          ;; 5. VERIFY: A blocks both B and C
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (let ((blocks-list (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
              (expect (length blocks-list) :to-equal 2)))

          ;; 6. VERIFY agenda: Only A is NEXT (blocks B and C)
          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (expect agenda-content :to-match "Task A")
            (expect agenda-content :not :to-match "Task B")
            (expect agenda-content :not :to-match "Task C"))

          ;; 7. VERIFY workflow: Complete A, then B and C both become NEXT (parallel execution)
          ;; org-edna automatically updates TODO states when dependencies are satisfied
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-todo "DONE"))
          ;; org-edna should automatically mark BOTH Task B and Task C as NEXT here (parallel tasks)

          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (expect agenda-content :not :to-match "Task A")
            (expect agenda-content :to-match "Task B")
            (expect agenda-content :to-match "Task C"))))

    (it "Test 4: adds parallel tasks B and C as children of A across multiple files"
        ;; TODO: This test documents the manual workflow required until org-gtd-project-modify-structure exists
        ;;
        ;; MANUAL WORKFLOW REQUIRED:
        ;; 1. M-x org-gtd-project-extend → add task B to main file
        ;; 2. M-x org-gtd-project-extend → add task C to secondary file
        ;; 3. On task B: M-x org-gtd-task-add-blockers → select task A as parent
        ;; 4. On task C: M-x org-gtd-task-add-blockers → select task A as parent
        ;; Result: A has two children (B and C) in different files that can run concurrently
        ;;
        ;; Test setup: Create project with task A in main file
        ;; Test execution: Add B in main file, C in secondary file, both as children of A
        ;; Test verification: All three NEXT, across files

        ;; 1. CAPTURE and ORGANIZE initial project with A
        (capture-inbox-item "Project Delta")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Task A" :level 2)
          (organize-as-project))

        ;; 2. Add Task B using project-extend (same file)
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Delta")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))
            (with-wip-buffer
              (goto-char (point-min))
              (search-forward "Task B")
              (org-back-to-heading t)
              (spy-on 'org-refile-get-location
                      :and-return-value (list "Project Delta"
                                              (buffer-file-name (marker-buffer project-point))
                                              nil
                                              (marker-position project-point)))
              (org-gtd-project-extend))))

        ;; 3. Create Task C in secondary file
        (let ((second-file (org-gtd--path "delta-secondary")))
          (with-temp-file second-file
            (make-task "Task C" :id "task-c-delta-id" :level 1))

          (with-current-buffer (find-file-noselect second-file)
            (org-mode)
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (org-id-add-location "task-c-delta-id" second-file)
            (org-todo "NEXT"))

          (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

            ;; 4. Link task C from second file to project
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Project Delta")
              (org-back-to-heading t)
              (let ((project-id (org-id-get-create)))
                ;; Add C to project's first tasks
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "task-c-delta-id")
                ;; Add project ID to C's project list
                (with-current-buffer (find-file-noselect second-file)
                  (goto-char (point-min))
                  (search-forward "Task C")
                  (org-back-to-heading t)
                  (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id))))

            ;; 5. SIMULATE MANUAL WORKFLOW: Make A parent of both B and C (using ORG_GTD_ properties)
            (let ((task-a-id nil)
                  (task-b-id nil))
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Task A")
                (org-back-to-heading t)
                (setq task-a-id (org-id-get-create))

                (goto-char (point-min))
                (search-forward "Task B")
                (org-back-to-heading t)
                (setq task-b-id (org-id-get-create))

                ;; Add A as blocker of B
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)

                ;; Add A as blocker of C (in other file)
                (with-current-buffer (find-file-noselect second-file)
                  (goto-char (point-min))
                  (search-forward "Task C")
                  (org-back-to-heading t)
                  (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id))

                ;; Update A's BLOCKS to include both B and C
                (goto-char (point-min))
                (search-forward "Task A")
                (org-back-to-heading t)
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" "task-c-delta-id")

                ;; Update project's first tasks and fix TODO keywords
                (goto-char (point-min))
                (search-forward "Project Delta")
                (org-back-to-heading t)
                (org-gtd-projects--set-first-tasks)
                (org-gtd-projects-fix-todo-keywords (point-marker)))

              ;; 6. VERIFY: A blocks both B and C
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Task A")
                (org-back-to-heading t)
                (let ((blocks-list (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
                  (expect (length blocks-list) :to-equal 2)))

              ;; 7. VERIFY agenda: Only A is NEXT (blocks B and C)
              (org-gtd-engage)
              (let ((agenda-content (agenda-raw-text)))
                (expect agenda-content :to-match "Task A")
                (expect agenda-content :not :to-match "Task B")
                (expect agenda-content :not :to-match "Task C"))

              ;; 8. VERIFY workflow: Complete A, then B and C both become NEXT (parallel execution across files)
              ;; org-edna automatically updates TODO states when dependencies are satisfied
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Task A")
                (org-back-to-heading t)
                (org-todo "DONE"))
              ;; org-edna should automatically mark BOTH Task B and Task C as NEXT here (parallel tasks across files)

              (org-gtd-engage)
              (let ((agenda-content (agenda-raw-text)))
                (expect agenda-content :not :to-match "Task A")
                (expect agenda-content :to-match "Task B")
                (expect agenda-content :to-match "Task C"))))))

    (it "Test 5: adds sequential tasks B and C chained after A in same file"
        ;; TODO: This test documents the manual workflow required until org-gtd-project-modify-structure exists
        ;;
        ;; MANUAL WORKFLOW REQUIRED:
        ;; 1. M-x org-gtd-project-extend → add task B
        ;; 2. M-x org-gtd-project-extend → add task C
        ;; 3. On task B: M-x org-gtd-task-add-blockers → select task A
        ;; 4. On task C: M-x org-gtd-task-add-blockers → select task B
        ;; Result: Sequential chain A → B → C
        ;;
        ;; Test setup: Create project with task A
        ;; Test execution: Add B and C, chain them
        ;; Test verification: Only A is NEXT, others TODO

        ;; 1. CAPTURE and ORGANIZE initial project with A
        (capture-inbox-item "Project Epsilon")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Task A" :level 2)
          (organize-as-project))

        ;; 2. Add Task B using project-extend
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Epsilon")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))
            (with-wip-buffer
              (goto-char (point-min))
              (search-forward "Task B")
              (org-back-to-heading t)
              (spy-on 'org-refile-get-location
                      :and-return-value (list "Project Epsilon"
                                              (buffer-file-name (marker-buffer project-point))
                                              nil
                                              (marker-position project-point)))
              (org-gtd-project-extend))))

        ;; 3. Add Task C using project-extend
        (capture-inbox-item "Task C")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Epsilon")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))
            (with-wip-buffer
              (goto-char (point-min))
              (search-forward "Task C")
              (org-back-to-heading t)
              (spy-on 'org-refile-get-location
                      :and-return-value (list "Project Epsilon"
                                              (buffer-file-name (marker-buffer project-point))
                                              nil
                                              (marker-position project-point)))
              (org-gtd-project-extend))))

        ;; 4. SIMULATE MANUAL WORKFLOW: Create A→B→C chain
        (let ((task-a-id nil)
              (task-b-id nil))
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (setq task-a-id (org-id-get-create))

            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (setq task-b-id (org-id-get-create))

            ;; Add A as blocker of B
            (spy-on 'org-gtd-task-management--select-multiple-task-ids
                    :and-return-value (list task-a-id))
            (org-gtd-task-add-blockers)

            ;; Add B as blocker of C
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (spy-on 'org-gtd-task-management--select-multiple-task-ids
                    :and-return-value (list task-b-id))
            (org-gtd-task-add-blockers))

          ;; 5. VERIFY sequential chain
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-equal (list task-b-id))

            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal (list task-a-id)))

          ;; 6. VERIFY only A is NEXT
          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (expect agenda-content :to-match "Task A")
            (expect agenda-content :not :to-match "Task B")
            (expect agenda-content :not :to-match "Task C"))

          ;; 7. VERIFY workflow: Complete A, then B becomes NEXT, complete B, then C becomes NEXT
          ;; org-edna automatically updates TODO states when dependencies are satisfied
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-todo "DONE"))
          ;; org-edna should automatically mark Task B as NEXT here

          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (expect agenda-content :not :to-match "Task A")
            (expect agenda-content :to-match "Task B")
            (expect agenda-content :not :to-match "Task C"))

          ;; Complete B, verify C becomes NEXT
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (org-todo "DONE"))
          ;; org-edna should automatically mark Task C as NEXT here

          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (expect agenda-content :not :to-match "Task A")
            (expect agenda-content :not :to-match "Task B")
            (expect agenda-content :to-match "Task C"))))

    (it "Test 6: adds sequential tasks B and C chained after A across multiple files"
        ;; TODO: This test documents the manual workflow required until org-gtd-project-modify-structure exists
        ;;
        ;; MANUAL WORKFLOW REQUIRED:
        ;; 1. M-x org-gtd-project-extend → add task B to main file
        ;; 2. M-x org-gtd-project-extend → add task C to secondary file
        ;; 3. On task B: M-x org-gtd-task-add-blockers → select task A
        ;; 4. On task C: M-x org-gtd-task-add-blockers → select task B
        ;; Result: Sequential chain A → B → C across files
        ;;
        ;; Test setup: Create project with A in main file
        ;; Test execution: Add B in main file, C in secondary file, chain them
        ;; Test verification: Only A is NEXT, others TODO across files

        ;; 1. CAPTURE and ORGANIZE initial project with A
        (capture-inbox-item "Project Zeta")
        (org-gtd-process-inbox)

        (with-wip-buffer
          (goto-char (point-max))
          (newline)
          (make-task "Task A" :level 2)
          (organize-as-project))

        ;; 2. Add Task B using project-extend (same file)
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Zeta")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))
            (with-wip-buffer
              (goto-char (point-min))
              (search-forward "Task B")
              (org-back-to-heading t)
              (spy-on 'org-refile-get-location
                      :and-return-value (list "Project Zeta"
                                              (buffer-file-name (marker-buffer project-point))
                                              nil
                                              (marker-position project-point)))
              (org-gtd-project-extend))))

        ;; 3. Create Task C in secondary file
        (let ((second-file (org-gtd--path "zeta-secondary")))
          (with-temp-file second-file
            (make-task "Task C" :id "task-c-zeta-id" :level 1))

          (with-current-buffer (find-file-noselect second-file)
            (org-mode)
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (org-id-add-location "task-c-zeta-id" second-file)
            (org-todo "TODO"))

          (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

            ;; 4. Link task C from second file to project
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Project Zeta")
              (org-back-to-heading t)
              (let ((project-id (org-id-get-create)))
                ;; Add C to project's first tasks
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "task-c-zeta-id")
                ;; Add project ID to C's project list
                (with-current-buffer (find-file-noselect second-file)
                  (goto-char (point-min))
                  (search-forward "Task C")
                  (org-back-to-heading t)
                  (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id))))

            ;; 5. SIMULATE MANUAL WORKFLOW: Create A→B→C chain
            (let ((task-a-id nil)
                  (task-b-id nil))
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Task A")
                (org-back-to-heading t)
                (setq task-a-id (org-id-get-create))

                (goto-char (point-min))
                (search-forward "Task B")
                (org-back-to-heading t)
                (setq task-b-id (org-id-get-create))

                ;; Add A as blocker of B
                (spy-on 'org-gtd-task-management--select-multiple-task-ids
                        :and-return-value (list task-a-id))
                (org-gtd-task-add-blockers)

                ;; Add B as blocker of C (in other file)
                (with-current-buffer (find-file-noselect second-file)
                  (goto-char (point-min))
                  (search-forward "Task C")
                  (org-back-to-heading t)
                  (spy-on 'org-gtd-task-management--select-multiple-task-ids
                          :and-return-value (list task-b-id))
                  (org-gtd-task-add-blockers)))

              ;; 6. VERIFY sequential chain across files
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Task A")
                (org-back-to-heading t)
                (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-equal (list task-b-id))

                (goto-char (point-min))
                (search-forward "Task B")
                (org-back-to-heading t)
                (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal (list task-a-id)))

              (with-current-buffer (find-file-noselect second-file)
                (goto-char (point-min))
                (search-forward "Task C")
                (org-back-to-heading t)
                (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal (list task-b-id)))

              ;; 7. VERIFY only A is NEXT across files
              (org-gtd-engage)
              (let ((agenda-content (agenda-raw-text)))
                (expect agenda-content :to-match "Task A")
                (expect agenda-content :not :to-match "Task B")
                (expect agenda-content :not :to-match "Task C"))

              ;; 8. VERIFY workflow: Complete A, then B becomes NEXT, complete B, then C becomes NEXT
              ;; org-edna automatically updates TODO states when dependencies are satisfied
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Task A")
                (org-back-to-heading t)
                (org-todo "DONE"))
              ;; org-edna should automatically mark Task B as NEXT here

              (org-gtd-engage)
              (let ((agenda-content (agenda-raw-text)))
                (expect agenda-content :not :to-match "Task A")
                (expect agenda-content :to-match "Task B")
                (expect agenda-content :not :to-match "Task C"))

              ;; Complete B, verify C becomes NEXT
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Task B")
                (org-back-to-heading t)
                (org-todo "DONE"))
              ;; org-edna should automatically mark Task C as NEXT here

              (org-gtd-engage)
              (let ((agenda-content (agenda-raw-text)))
                (expect agenda-content :not :to-match "Task A")
                (expect agenda-content :not :to-match "Task B")
                (expect agenda-content :to-match "Task C"))))))))

(describe "Ticklering and reactivating projects (end-to-end)"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "full incubation → reactivation cycle preserves project state"
      ;; Create a project with dependencies
      (create-project "Future project")

      (with-current-buffer (org-gtd--default-file)
        ;; Verify initial state: Task 1 is NEXT
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal "NEXT")

        ;; Incubate the project
        (goto-char (point-min))
        (search-forward "Future project")
        (org-back-to-heading t)
        (org-gtd-tickler "2025-12-01")

        ;; Verify project is tickler
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")
        (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2025-12-01>")

        ;; Verify tasks are tickler (no TODO keywords)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")
        (expect (org-entry-get (point) "TODO") :to-be nil)
        (expect (org-entry-get (point) "PREVIOUS_TODO") :to-equal "NEXT"))

      ;; Verify it doesn't appear in engage view
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (expect (buffer-string) :not :to-match "Task 1"))

      ;; Reactivate the project
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Future project")
        (org-back-to-heading t)
        (org-gtd-reactivate)

        ;; Verify project is reactivated
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Projects")
        (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-be nil)

        ;; Verify tasks are reactivated with TODO keywords
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")
        (expect (org-entry-get (point) "TODO") :to-equal "NEXT"))

      ;; Verify it appears in engage view again
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (expect (buffer-string) :to-match "Task 1"))))

;;; end-to-end-test.el ends here
