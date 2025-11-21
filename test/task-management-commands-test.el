;;; task-management-commands-test.el --- Tests for task management commands -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for interactive commands that manage task dependencies (BLOCKS/BLOCKED_BY relationships)
;;

;;; Code:

;;;; Requirements

(require 'org-gtd)
(require 'org-gtd-task-management)
(require 'buttercup)

;; Load test helpers
(load (expand-file-name "helpers/setup.el"
                        (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory)))
(load (expand-file-name "helpers/builders.el"
                        (if load-file-name
                            (file-name-directory load-file-name)
                          default-directory)))

;;;; Test Setup

(describe
 "task dependency commands"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "org-gtd-task-add-blockers command (Story 1)"

  (it "creates bidirectional BLOCKS/DEPENDS_ON relationship between tasks"
      ;; Test the new org-gtd-task-add-blockers command with bidirectional properties
      (with-temp-buffer
        (org-mode)
        ;; Use builders instead of manual insertion
        (make-task "Task A" :id "task-a-id" :level 1)
        (insert "\n")
        (make-task "Task B" :id "task-b-id" :level 1)
        (insert "\n")

        ;; Go to Task B and add Task A as blocker (Task A blocks Task B)
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)

        ;; Mock the task selection to return single task as list
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-a-id"))
        (org-gtd-task-add-blockers)

        ;; Verify Task B has DEPENDS_ON property
        (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
          (expect depends-on :to-equal '("task-a-id")))

        ;; Verify Task A has BLOCKS property
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          (expect blocks :to-equal '("task-b-id")))))

  (it "creates bidirectional BLOCKS/DEPENDS_ON relationship with multiple blockers (Story 2)"
      ;; Test multi-select functionality
      (with-temp-buffer
        (org-mode)
        ;; Use builders instead of manual insertion
        (make-task "Task A" :id "task-a-id" :level 1)
        (insert "\n")
        (make-task "Task B" :id "task-b-id" :level 1)
        (insert "\n")
        (make-task "Task C" :id "task-c-id" :level 1)
        (insert "\n")

        ;; Go to Task C and add both Task A and B as blockers
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)

        ;; Mock the task selection to return multiple IDs
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-a-id" "task-b-id"))
        (org-gtd-task-add-blockers)

        ;; Verify Task C has DEPENDS_ON property with both IDs
        (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
          (expect (sort depends-on 'string<) :to-equal '("task-a-id" "task-b-id")))

        ;; Verify Task A has BLOCKS property
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          (expect blocks :to-equal '("task-c-id")))

        ;; Verify Task B has BLOCKS property
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          (expect blocks :to-equal '("task-c-id"))))))



 (describe
  "cross-project dependencies (Story 5)"

  (it "shows tasks from different projects with project labels when selecting blockers"
      ;; This test verifies the acceptance criteria: "tasks from all existing agenda files clearly labeled by project"
      ;; Set up Project A in main GTD tasks file
      (capture-inbox-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1")
      (organize-as-project)

      ;; Create a second GTD file with Project B
      (let ((second-file (org-gtd--path "secondary-project")))
        (with-temp-file second-file
          (insert "* Project B\n** Task B1\n:PROPERTIES:\n:ID: task-b1-id\n:END:\n"))

        ;; Ensure the ID is registered in the org-id system by visiting the file
        (with-current-buffer (find-file-noselect second-file)
          (org-mode) ; Make sure org-mode is active
          (goto-char (point-min))
          (search-forward "Task B1")
          (org-back-to-heading t)
          ;; Force org-id to recognize this ID
          (org-id-add-location "task-b1-id" second-file))

        ;; Add second file to org-agenda-files for this test
        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

          ;; Capture what task selection interface shows
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A1")
            (org-back-to-heading t)

            ;; Mock the completing-read to capture the options presented
            (let ((presented-options nil)
                  (call-count 0))
              (spy-on 'completing-read
                      :and-call-fake
                      (lambda (_prompt collection &rest _args)
                        (when (= call-count 0) ; Only capture on first call
                          (setq presented-options collection))
                        (setq call-count (1+ call-count))
                        ;; Return empty string on second call to exit multi-select loop
                        (if (= call-count 1) "task-b1-id" "")))
              (org-gtd-task-management--select-multiple-task-ids "Test prompt: ")

              ;; Verify that tasks are labeled with project information
              ;; The collection should include project context for cross-project tasks
              (let ((has-project-labels (cl-some (lambda (option)
                                                   ;; option is a cons cell (display . value)
                                                   (string-match-p "Project B" (car option)))
                                                 presented-options)))
                (expect has-project-labels :to-be-truthy)))))))


  (it "creates dependencies between tasks in different projects"
      ;; Test cross-project blocking relationships
      ;; Set up Project A in main GTD tasks file
      (capture-inbox-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1")
      (organize-as-project)

      ;; Create a second GTD file manually
      (let ((second-file (org-gtd--path "secondary-project")))
        ;; Set up Project B in the secondary file
        (with-temp-file second-file
          (insert "* Project B\n** Task B1\n:PROPERTIES:\n:ID: task-b1-id\n:END:\n"))

        ;; Ensure the ID is registered in the org-id system by visiting the file
        (with-current-buffer (find-file-noselect second-file)
          (org-mode) ; Make sure org-mode is active
          (goto-char (point-min))
          (search-forward "Task B1")
          (org-back-to-heading t)
          ;; Force org-id to recognize this ID
          (org-id-add-location "task-b1-id" second-file))

        ;; Add second file to org-agenda-files for this test
        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

          ;; Now create cross-project dependency: Task A1 depends on Task B1
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A1")
            (org-back-to-heading t)

            ;; Mock task selection to return Task B1 from different project
            (spy-on 'org-gtd-task-management--select-multiple-task-ids
                    :and-return-value '("task-b1-id"))
            (org-gtd-task-add-blockers)

            ;; Verify Task A1 has DEPENDS_ON pointing to Task B1
            (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
              (expect depends-on :to-equal '("task-b1-id"))))

          ;; Verify Task B1 in the other file has BLOCKS pointing to Task A1
          (with-current-buffer (find-file-noselect second-file)
            (goto-char (point-min))
            (search-forward "Task B1")
            (org-back-to-heading t)
            (let ((task-a1-id (with-current-buffer (org-gtd--default-file)
                                (goto-char (point-min))
                                (search-forward "Task A1")
                                (org-back-to-heading t)
                                (org-entry-get (point) "ID")))
                  (blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
              (expect blocks :to-equal (list task-a1-id))))))))



 (describe
  "automatic next action updates (Story 15)"

  (it "automatically makes dependent tasks NEXT when blocker is marked DONE"
      ;; Test the core acceptance criteria: When I mark a task as DONE,
      ;; all tasks that were blocked by this task automatically become NEXT
      (with-temp-buffer
        (org-mode)
        ;; Ensure our task management module is loaded
        (require 'org-gtd-task-management)

        (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")
        (insert "* TODO Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")

        ;; Add ORG_GTD_BLOCKS properties to Task A (bidirectional relationship)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" "task-b-id")
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" "task-c-id")

        ;; Set up buffer with file association and register IDs
        (let ((temp-file (make-temp-file "org-gtd-test" nil ".org")))
          (set-visited-file-name temp-file)
          (save-buffer)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
            (org-id-add-location (match-string 1) temp-file)))

        ;; Verify initial state: Task B and C should be TODO (waiting)
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal "TODO")

        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal "TODO")

        ;; Mark Task A as DONE - this should trigger automatic updates
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (org-todo "DONE")

        ;; Update the file with the new state
        (save-buffer)


        ;; Verify Task B and C automatically became NEXT
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal (org-gtd-keywords--next))

        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal (org-gtd-keywords--next))))

  (it "leaves tasks with remaining dependencies as TODO when only one blocker is completed"
      ;; Test that tasks with multiple dependencies only become NEXT when ALL blockers are complete
      (with-temp-buffer
        (org-mode)
        ;; Ensure our task management module is loaded
        (require 'org-gtd-task-management)

        (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-c-id\n:END:\n\n")
        (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_BLOCKS: task-c-id\n:END:\n\n")
        (insert "* TODO Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id task-b-id\n:END:\n\n")

        ;; Set up buffer with file association and register IDs
        (let ((temp-file (make-temp-file "org-gtd-test-multi" nil ".org")))
          (set-visited-file-name temp-file)
          (save-buffer)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
            (org-id-add-location (match-string 1) temp-file)))

        ;; Mark only Task A as DONE (Task B still TODO)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (org-todo "DONE")
        (save-buffer)

        ;; Verify Task C remains TODO (still blocked by Task B)
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal "TODO")

        ;; Now mark Task B as DONE too
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (org-todo "DONE")
        (save-buffer)

        ;; Now Task C should become NEXT (all dependencies satisfied)
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal (org-gtd-keywords--next))))

  (it "updates org-agenda to reflect automatic state changes immediately"
      ;; Test that changes are visible in org-agenda immediately
      ;; This is a placeholder for the agenda integration test
      ;; The actual implementation will need to ensure agenda refresh
      (with-temp-buffer
        (org-mode)
        ;; Ensure our task management module is loaded
        (require 'org-gtd-task-management)

        (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-b-id\n:END:\n\n")
        (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")

        ;; Set up buffer with file association and register IDs
        (let ((temp-file (make-temp-file "org-gtd-test-agenda" nil ".org")))
          (set-visited-file-name temp-file)
          (save-buffer)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
            (org-id-add-location (match-string 1) temp-file)))

        ;; Mark Task A as DONE
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (org-todo "DONE")
        (save-buffer)

        ;; Verify Task B became NEXT
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TODO") :to-equal (org-gtd-keywords--next))

        ;; Note: In real implementation, this would also test agenda visibility
        ;; but that requires more complex agenda mock setup
        )))

 (describe
  "circular dependency detection (Story 13)"

  (it "prevents direct circular dependency (A blocks B, B cannot block A)"
      ;; Test the core acceptance criteria: system prevents circular dependency creation
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")

        ;; First create A blocks B relationship
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)

        ;; Mock the task selection to return task A as blocker for B
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-a-id"))
        (org-gtd-task-add-blockers)

        ;; Verify A blocks B relationship was created
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-equal '("task-b-id"))

        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal '("task-a-id"))

        ;; Now try to create reverse relationship: B blocks A (should fail)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)

        ;; Mock the task selection to return task B as blocker for A (circular!)
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-b-id"))
        ;; This should signal an error due to circular dependency
        (expect (org-gtd-task-add-blockers) :to-throw 'error '("Circular dependency detected: task-a-id -> task-b-id -> task-a-id"))

        ;; Verify that the circular relationship was NOT created
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-be nil)

        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-be nil)))

  (it "prevents indirect circular dependency (A -> B -> C -> A)"
      ;; Test more complex circular dependency: A blocks B, B blocks C, trying to make C block A
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
        (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")

        ;; Create A blocks B
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-a-id"))
        (org-gtd-task-add-blockers)

        ;; Create B blocks C
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-b-id"))
        (org-gtd-task-add-blockers)

        ;; Now try to create C blocks A (would create cycle: A -> B -> C -> A)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-c-id"))
        ;; This should signal an error due to circular dependency
        (expect (org-gtd-task-add-blockers) :to-throw 'error '("Circular dependency detected: task-a-id -> task-b-id -> task-c-id -> task-a-id"))

        ;; Verify that the circular relationship was NOT created
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-be nil)

        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          ;; C should not block A (circular dependency prevented)
          (expect (member "task-a-id" blocks) :to-be nil)))))

 (describe
  "remove task blockers (Story 3)"

  (it "removes selected blocking relationships from task with existing blockers"
      ;; Test Story 3 acceptance criteria: remove specific blockers from task
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-c-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_BLOCKS: task-c-id\n:END:\n\n")
        (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id task-b-id\n:END:\n\n")

        ;; Go to Task C (which has blockers A and B)
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)

        ;; Mock the task selection to remove just Task A as blocker
        (spy-on 'org-gtd-task-management--select-multiple-blocking-task-ids
                :and-return-value '("task-a-id"))
        (org-gtd-task-remove-blockers)

        ;; Verify Task C no longer depends on Task A, but still depends on Task B
        (let ((depends-on (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))
          (expect depends-on :to-equal '("task-b-id")))

        ;; Verify Task A no longer blocks Task C
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          (expect blocks :to-be nil))

        ;; Verify Task B still blocks Task C
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          (expect blocks :to-equal '("task-c-id")))))

  (it "handles task with no blockers gracefully"
      ;; Test edge case: trying to remove blockers from task that has none
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")

        ;; Go to Task A (which has no blockers)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)

        ;; This should not error, just show a helpful message
        (spy-on 'message :and-return-value nil)
        (org-gtd-task-remove-blockers)
        (expect 'message :to-have-been-called-with "Task '%s' has no blockers to remove" "Task A"))))

 (describe
  "org-gtd-task-add-dependents command (Story 4)"

  (it "creates bidirectional BLOCKS/DEPENDS_ON relationship where current task blocks selected tasks"
      ;; Test the new org-gtd-task-add-dependents command (plural) - the inverse of add-blockers
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
        (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")

        ;; Go to Task A and add Tasks B and C as dependents (Task A blocks Tasks B & C)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)

        ;; Mock the multi-task selection to return Task B and C
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-b-id" "task-c-id"))

        ;; Mock the message display
        (let ((message-output ""))
          (spy-on 'message
                  :and-call-fake
                  (lambda (format-string &rest args)
                    (setq message-output (apply #'format format-string args))))

          ;; Call the new command
          (org-gtd-task-add-dependents)

          ;; Verify Task A has BLOCKS property with both Task B and C
          (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")
                  :to-equal '("task-b-id" "task-c-id"))

          ;; Verify Task B has DEPENDS_ON property with Task A
          (goto-char (point-min))
          (re-search-forward "Task B")
          (org-back-to-heading t)
          (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")
                  :to-equal '("task-a-id"))

          ;; Verify Task C has DEPENDS_ON property with Task A
          (goto-char (point-min))
          (re-search-forward "Task C")
          (org-back-to-heading t)
          (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")
                  :to-equal '("task-a-id"))

          ;; Verify confirmation message is shown
          (expect message-output :to-match "Added dependent relationships:.*Task A.*blocks.*Task B.*Task C")))))

 (describe
  "direct task dependency management from regular org files (Story 6)"

  (it "adds task dependencies in regular org files with confirmation message"
      ;; Test that org-gtd-task-add-blockers works in regular project files
      ;; (not just WIP buffers) and provides user confirmation
      (with-temp-buffer
        (org-mode)
        (insert "* Project Example\n")
        (insert "** Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "** Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")

        ;; Go to Task B and add Task A as blocker
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)

        ;; Capture the confirmation message
        (spy-on 'message :and-return-value nil)
        (spy-on 'org-gtd-task-management--select-multiple-task-ids
                :and-return-value '("task-a-id"))
        (org-gtd-task-add-blockers)

        ;; Verify confirmation message was shown
        (expect 'message :to-have-been-called)

        ;; Verify the relationships were created in this regular org file
        ;; Task B should have DEPENDS_ON
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-contain "task-a-id")

        ;; Task A should have BLOCKS
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-contain "task-b-id"))))

 (describe
  "clear all task relationships (Story 12)"

  (it "removes all BLOCKS and DEPENDS_ON properties from task and updates related tasks"
      (with-temp-buffer
        (org-mode)

        ;; Create multiple tasks with complex relationships
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-b-id task-c-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:ORG_GTD_BLOCKS: task-d-id\n:END:\n\n")
        (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")
        (insert "* Task D\n:PROPERTIES:\n:ID: task-d-id\n:ORG_GTD_DEPENDS_ON: task-b-id\n:END:\n\n")

        ;; Move to Task B (which has both BLOCKS and DEPENDS_ON)
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)

        ;; Call the clear relationships command
        (org-gtd-task-clear-relationships)

        ;; Verify Task B has no BLOCKS or DEPENDS_ON properties
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-equal nil)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal nil)

        ;; Verify Task A no longer blocks Task B (but still blocks Task C)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks-list (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          (expect blocks-list :not :to-contain "task-b-id")
          (expect blocks-list :to-contain "task-c-id"))

        ;; Verify Task D no longer depends on Task B
        (goto-char (point-min))
        (re-search-forward "Task D")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal nil)

        ;; Verify Task C is unchanged (still depends on Task A)
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-equal '("task-a-id")))))

  (it "shows confirmation message about cleared relationships"
      (with-temp-buffer
        (org-mode)

        ;; Create task with relationships
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-b-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")

        ;; Move to Task A
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)

        ;; Capture message output
        (spy-on 'message :and-return-value nil)

        ;; Call the clear relationships command
        (org-gtd-task-clear-relationships)

        ;; Verify confirmation message (Task A blocks 1, depends on 0)
        (expect 'message :to-have-been-called)))

  (it "handles task with no relationships gracefully"
      (with-temp-buffer
        (org-mode)

        ;; Create task without relationships
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")

        ;; Move to Task A
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)

        ;; Capture message output
        (spy-on 'message :and-return-value nil)

        ;; Call the clear relationships command
        (org-gtd-task-clear-relationships)

        ;; Verify appropriate message
        (expect 'message :to-have-been-called-with "Task %s has no relationships to clear" "Task A")))

 ) ; Close the main "task dependency commands" describe block

(describe
 "lazy ID creation (Story 9)"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "automatically creates IDs for tasks without them when adding dependencies"
     ;; Test that tasks without pre-existing IDs get IDs created automatically
     (with-temp-buffer
       (org-mode)
       ;; Create tasks WITHOUT IDs initially
       (insert "* Task A\n\n")
       (insert "* Task B\n\n")

       ;; Go to Task B (no ID) and add Task A (no ID) as blocker
       (goto-char (point-min))
       (re-search-forward "Task B")
       (org-back-to-heading t)

       ;; Capture the initial state - no IDs should exist
       (expect (org-entry-get (point) "ID") :to-be nil)
       (goto-char (point-min))
       (re-search-forward "Task A")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "ID") :to-be nil)

       ;; Mock the task selection - first we need to collect available tasks
       ;; The system should create IDs as needed during this process
       (goto-char (point-min))
       (re-search-forward "Task B")
       (org-back-to-heading t)

       (let* ((task-a-id nil)
              (task-b-id nil))
         (spy-on 'org-gtd-task-management--select-multiple-task-ids
                 :and-call-fake
                 (lambda (_prompt)
                   ;; First trigger the real task collection to create IDs
                   (org-gtd-task-management--collect-all-task-info)
                   ;; Now capture Task A's ID after it gets created during collection
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "Task A")
                     (org-back-to-heading t)
                     (setq task-a-id (org-entry-get (point) "ID")))
                   (list task-a-id)))
         (org-gtd-task-add-blockers)

         ;; After the command, both tasks should have IDs
         ;; Task B should have an ID (current task)
         (setq task-b-id (org-entry-get (point) "ID"))
         (expect task-b-id :not :to-be nil)
         (expect (string-match "^[a-z0-9-]+$" task-b-id) :to-be 0) ; org-gtd ID format

         ;; Task A should have an ID (selected as blocker)
         (goto-char (point-min))
         (re-search-forward "Task A")
         (org-back-to-heading t)
         (setq task-a-id (org-entry-get (point) "ID"))
         (expect task-a-id :not :to-be nil)
         (expect (string-match "^[a-z0-9-]+$" task-a-id) :to-be 0) ; org-gtd ID format

         ;; Verify the blocking relationships were created correctly
         (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-contain task-b-id)

         (goto-char (point-min))
         (re-search-forward "Task B")
         (org-back-to-heading t)
         (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-contain task-a-id))))

 (it "creates IDs automatically for tasks without them when adding dependencies"
     ;; Test mixed scenario: some tasks have IDs, some don't
     (with-temp-buffer
       (org-mode)
       ;; Task A has no ID, Task B has pre-existing ID
       (insert "* Task A\n\n")
       (insert "* Task B\n:PROPERTIES:\n:ID: existing-task-b-id\n:END:\n\n")

       ;; Go to Task B (has ID) and add Task A (no ID) as blocker
       (goto-char (point-min))
       (re-search-forward "Task B")
       (org-back-to-heading t)

       ;; Verify initial state
       (expect (org-entry-get (point) "ID") :to-equal "existing-task-b-id")
       (goto-char (point-min))
       (re-search-forward "Task A")
       (org-back-to-heading t)
       (expect (org-entry-get (point) "ID") :to-be nil)

       (let ((task-a-id nil))
         (spy-on 'org-gtd-task-management--select-multiple-task-ids
                 :and-call-fake
                    (lambda (_prompt)
                      ;; First trigger the real task collection to create IDs
                      (org-gtd-task-management--collect-all-task-info)
                      ;; The selection should create ID for Task A during collection
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "Task A")
                        (org-back-to-heading t)
                        (setq task-a-id (org-entry-get (point) "ID")))
                      (list task-a-id)))

         ;; Go back to Task B to run the command
         (goto-char (point-min))
         (re-search-forward "Task B")
         (org-back-to-heading t)
         (org-gtd-task-add-blockers)

         ;; Task A should now have an ID
         (goto-char (point-min))
         (re-search-forward "Task A")
         (org-back-to-heading t)
         (setq task-a-id (org-entry-get (point) "ID"))
         (expect task-a-id :not :to-be nil)

         ;; Task B should keep its pre-existing ID
         (goto-char (point-min))
         (re-search-forward "Task B")
         (org-back-to-heading t)
         (expect (org-entry-get (point) "ID") :to-equal "existing-task-b-id")

         ;; Verify relationships were created
         (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON") :to-contain task-a-id)
         (goto-char (point-min))
         (re-search-forward "Task A")
         (org-back-to-heading t)
         (expect (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS") :to-contain "existing-task-b-id"))))

     ) ; Close lazy ID creation describe block

 (describe "relationship visualization (Story 10)"
   (it "shows task dependency relationships in a formatted display"
       (with-temp-buffer
         (org-mode)
         ;; Set up test scenario with complex relationships
         (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:ORG_GTD_BLOCKS: task-b-id task-c-id\n:END:\n\n")
         (insert "* Task B  \n:PROPERTIES:\n:ID: task-b-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:ORG_GTD_BLOCKS: task-d-id\n:END:\n\n")
         (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:ORG_GTD_DEPENDS_ON: task-a-id\n:END:\n\n")
         (insert "* Task D\n:PROPERTIES:\n:ID: task-d-id\n:ORG_GTD_DEPENDS_ON: task-b-id\n:END:\n\n")

         ;; Position on Task B (has both blocking and blocked relationships)
         (goto-char (point-min))
         (re-search-forward "Task B")
         (org-back-to-heading t)

         ;; Mock task name resolution function
         (spy-on 'org-gtd-task-management--get-heading-for-id
                 :and-call-fake
                 (lambda (id)
                   (cond
                    ((string= id "task-a-id") "Task A")
                    ((string= id "task-b-id") "Task B")
                    ((string= id "task-c-id") "Task C")
                    ((string= id "task-d-id") "Task D")
                    (t "Unknown Task"))))

         ;; Call the relationship visualization command
         (let ((relationship-display (org-gtd-task-show-relationships)))
           ;; Expect formatted display showing:
           ;; - What this task depends on (blockers)
           ;; - What this task blocks (dependents)
           (expect relationship-display :to-match "Task B Dependencies:")
           (expect relationship-display :to-match "Blocked by:.*Task A")
           (expect relationship-display :to-match "Blocks:.*Task D")
           (expect relationship-display :not :to-match "Task C")))) ; Task C is not directly related to Task B

   (it "shows clear message when task has no relationships"
       (with-temp-buffer
         (org-mode)
         (insert "* Isolated Task\n:PROPERTIES:\n:ID: isolated-task-id\n:END:\n\n")

         ;; Position on the isolated task
         (goto-char (point-min))
         (re-search-forward "Isolated Task")
         (org-back-to-heading t)

         ;; Call the relationship visualization command
         (let ((relationship-display (org-gtd-task-show-relationships)))
           ;; Should show clear message for no relationships
           (expect relationship-display :to-match "Isolated Task.*no dependency relationships")))))

(describe "dependency helper window (Story 11)"
  (it "displays live dependency view in WIP buffer when org-gtd-clarify-display-helper-buffer is enabled"
    (with-temp-buffer
      ;; Create a WIP buffer with multiple tasks
      (org-mode)
      (org-gtd-wip-mode)

      ;; Insert project structure with dependencies (disable ID overlays during insert)
      (let ((org-gtd-id-overlay-mode nil))
        (insert "* Build Deck\n")
        (insert "** Get permits\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD_BLOCKS: get-materials\n")
        (insert ":ID: get-permits\n")
        (insert ":END:\n")
        (insert "** Get materials\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD_DEPENDS_ON: get-permits\n")
        (insert ":ORG_GTD_BLOCKS: install-decking\n")
        (insert ":ID: get-materials\n")
        (insert ":END:\n")
        (insert "** Install decking\n")
        (insert ":PROPERTIES:\n")
        (insert ":ORG_GTD_DEPENDS_ON: get-materials\n")
        (insert ":ID: install-decking\n")
        (insert ":END:\n")
        (insert "** Paint deck (orphaned task)\n")
        (insert ":PROPERTIES:\n")
        (insert ":ID: paint-deck\n")
        (insert ":END:\n"))

      ;; Enable the helper buffer feature
      (let ((org-gtd-clarify-display-helper-buffer t))
        ;; Trigger the helper window display
        (org-gtd-clarify-display-dependency-helper)

        ;; Check that helper window was created
        (let ((helper-buffer (get-buffer "*Org GTD Project Dependencies*")))
          (expect helper-buffer :not :to-be nil)
          (with-current-buffer helper-buffer
            (let ((helper-content (buffer-string)))
              ;; Should show project name
              (expect helper-content :to-match "Project name: Build Deck")
              ;; Should show task relationships in format: (depends_on, ...) -> task -> (blocks, ...)
              (expect helper-content :to-match "() -> Get permits -> (Get materials)")
              (expect helper-content :to-match "(Get permits) -> Get materials -> (Install decking)")
              (expect helper-content :to-match "(Get materials) -> Install decking -> ()")
              ;; Should show orphaned tasks section
              (expect helper-content :to-match "Orphaned tasks:")
              (expect helper-content :to-match "Paint deck (orphaned task)"))))))))

(describe "broken project detection (Story 14)"
  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "identifies projects with non-existent task references and provides guidance"
    (let* ((temp-file (make-temp-file "org-gtd-test-" nil ".org"))
           (temp-buffer (find-file-noselect temp-file)))
      (unwind-protect
          (progn
            ;; Create a project with broken dependency reference
            (with-current-buffer temp-buffer
              (insert "* Build Deck Project\n")
              (insert "** Get permits\n")
              (insert ":PROPERTIES:\n")
              (insert ":ID: get-permits-id\n")
              (insert ":ORG_GTD_BLOCKS: non-existent-task-id\n")  ; Broken reference
              (insert ":END:\n")
              (insert "** Install decking\n")
              (insert ":PROPERTIES:\n")
              (insert ":ID: install-decking-id\n")
              (insert ":ORG_GTD_DEPENDS_ON: another-missing-task-id\n")  ; Another broken reference
              (insert ":END:\n")
              (save-buffer))

            ;; Mock org-agenda-files to include temp file
            (spy-on 'org-agenda-files :and-return-value (list temp-file))

            ;; Run the project health check
            (let* ((health-results (org-gtd-validate-project-dependencies)))

              ;; Should identify broken references
              (expect health-results :not :to-be nil)
              (expect (plist-get health-results :broken-references) :not :to-be nil)

              ;; Should identify the specific broken references
              (let ((broken-refs (plist-get health-results :broken-references)))
                (expect (length broken-refs) :to-be 2)

                ;; Should contain information about the broken references
                (expect (cl-some (lambda (ref)
                                   (and (string= (plist-get ref :referencing-task) "get-permits-id")
                                        (string= (plist-get ref :missing-task) "non-existent-task-id")
                                        (string= (plist-get ref :property) "ORG_GTD_BLOCKS")))
                                 broken-refs) :to-be-truthy)

                (expect (cl-some (lambda (ref)
                                   (and (string= (plist-get ref :referencing-task) "install-decking-id")
                                        (string= (plist-get ref :missing-task) "another-missing-task-id")
                                        (string= (plist-get ref :property) "ORG_GTD_DEPENDS_ON")))
                                 broken-refs) :to-be-truthy))

              ;; Should provide guidance for fixing broken references
              (expect (plist-get health-results :guidance) :not :to-be nil)
              (let ((guidance (plist-get health-results :guidance)))
                (expect guidance :to-match "broken.*reference")
                (expect guidance :to-match "remove.*invalid.*property"))))
        ;; Cleanup
        (when (buffer-live-p temp-buffer)
          (with-current-buffer temp-buffer
            (set-buffer-modified-p nil))
          (kill-buffer temp-buffer))
        (when (file-exists-p temp-file)
          (delete-file temp-file)))))


  (it "identifies orphaned tasks with dependencies that aren't in proper projects"
    (let* ((temp-file (make-temp-file "org-gtd-test-" nil ".org"))
           (temp-buffer (find-file-noselect temp-file)))
      (unwind-protect
          (progn
            ;; Create orphaned tasks with dependencies outside of projects
            (with-current-buffer temp-buffer
              (insert "* Random Task with Dependencies\n")
              (insert ":PROPERTIES:\n")
              (insert ":ID: orphaned-task-id\n")
              (insert ":ORG_GTD_DEPENDS_ON: some-other-task-id\n")  ; This task has dependencies but isn't in a project structure
              (insert ":END:\n")
              (insert "* Another Task\n")
              (insert ":PROPERTIES:\n")
              (insert ":ID: some-other-task-id\n")
              (insert ":END:\n")
              (save-buffer))

            ;; Mock org-agenda-files to include temp file
            (spy-on 'org-agenda-files :and-return-value (list temp-file))

            ;; Run the health check
            (let* ((health-results (org-gtd-validate-project-dependencies)))

              ;; Should identify orphaned tasks
              (expect health-results :not :to-be nil)
              (expect (plist-get health-results :orphaned-tasks) :not :to-be nil)

              (let ((orphaned-tasks (plist-get health-results :orphaned-tasks)))
                (expect (length orphaned-tasks) :to-be-greater-than 0)

                ;; Should identify the orphaned task with dependencies
                (expect (cl-some (lambda (task)
                                   (string= (plist-get task :id) "orphaned-task-id"))
                                 orphaned-tasks) :to-be-truthy))

              ;; Should provide guidance for fixing orphaned tasks
              (expect (plist-get health-results :guidance) :not :to-be nil)
              (let ((guidance (plist-get health-results :guidance)))
                (expect guidance :to-match "orphaned.*task")
                (expect guidance :to-match "organize.*into.*project"))))
        ;; Cleanup
        (when (buffer-live-p temp-buffer)
          (with-current-buffer temp-buffer
            (set-buffer-modified-p nil))
          (kill-buffer temp-buffer))
        (when (file-exists-p temp-file)
          (delete-file temp-file))))))

(describe
 "org-gtd-task-management--find-project-heading for multi-file DAG"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "finds project heading when task is in same file as project"
     ;; Standard case - task is outline child of project
     (capture-inbox-item "Same-file Project")
     (org-gtd-process-inbox)
     (goto-char (point-max))
     (newline)
     (make-task "Task in same file" :level 2)
     (organize-as-project)

     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Task in same file")
       (org-back-to-heading t)

       ;; Should find the project heading via outline navigation
       (let ((project-heading (org-gtd-task-management--find-project-heading)))
         (expect project-heading :to-be-truthy)
         (expect project-heading :to-match "Same-file Project"))))

 (it "finds project heading when task is in different file (multi-file DAG)"
     ;; Create project in main file
     (capture-inbox-item "Multi-file DAG Project")
     (org-gtd-process-inbox)
     (goto-char (point-max))
     (newline)
     (make-task "Task in main file" :level 2)
     (organize-as-project)

     ;; Get project ID
     (let (project-id second-file)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Multi-file DAG Project")
         (org-back-to-heading t)
         (setq project-id (org-entry-get (point) "ID")))

       ;; Create task in different file with ORG_GTD_PROJECT_IDS set
       (setq second-file (org-gtd--path "other-tasks"))
       (with-temp-file second-file
         (insert "* Task in other file\n")
         (insert ":PROPERTIES:\n")
         (insert ":ID: task-other-123\n")
         (insert ":ORG_GTD: Actions\n")
         (insert (format ":ORG_GTD_PROJECT_IDS: %s\n" project-id))
         (insert ":END:\n"))

       ;; Should find project heading via ORG_GTD_PROJECT_IDS
       (with-current-buffer (find-file-noselect second-file)
         (org-mode)
         (goto-char (point-min))
         (search-forward "Task in other file")
         (org-back-to-heading t)

         (let ((project-heading (org-gtd-task-management--find-project-heading)))
           (expect project-heading :to-be-truthy)
           (expect project-heading :to-match "Multi-file DAG Project"))))))


;;; task-management-commands-test.el ends here
