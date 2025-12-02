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
;; Advanced project task operations migrated to test-eunit/acceptance/project-task-operations-test.el
;; (Only active tests migrated; xit tests for unimplemented features not migrated)
;; Multi-project task sharing tests migrated to test-eunit/acceptance/multi-project-sharing-test.el
;; Project cancellation and archive tests migrated to test-eunit/acceptance/project-cancellation-test.el
;; Ticklering and reactivating projects migrated to test-eunit/acceptance/tickler-flow-test.el
;; Area of focus review with tickler projects migrated to test-eunit/acceptance/review-flow-test.el

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

;; Multi-file Review and Validation Tests migrated to test-eunit/integration/end-to-end-test.el

(describe "Project Task Operation Tests (Manual Workflows)"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Adding tasks to project DAG"

    ;; Tests 1-2 migrated to test-eunit/integration/dag-operations-test.el

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

;; Ticklering and reactivating projects test migrated to test-eunit/integration/end-to-end-test.el

;;; end-to-end-test.el ends here
