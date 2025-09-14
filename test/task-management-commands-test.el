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
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")

        ;; Go to Task B and add Task A as blocker (Task A blocks Task B)
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)

        ;; Mock the task selection to return single task as list
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-a-id"))))
          (org-gtd-task-add-blockers))

        ;; Verify Task B has DEPENDS_ON property
        (let ((depends-on (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
          (expect depends-on :to-equal '("task-a-id")))

        ;; Verify Task A has BLOCKS property
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
          (expect blocks :to-equal '("task-b-id")))))

  (it "creates bidirectional BLOCKS/DEPENDS_ON relationship with multiple blockers (Story 2)"
      ;; Test multi-select functionality
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n\n")
        (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:END:\n\n")

        ;; Go to Task C and add both Task A and B as blockers
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)

        ;; Mock the task selection to return multiple IDs
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-a-id" "task-b-id"))))
          (org-gtd-task-add-blockers))

        ;; Verify Task C has DEPENDS_ON property with both IDs
        (let ((depends-on (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
          (expect (sort depends-on 'string<) :to-equal '("task-a-id" "task-b-id")))

        ;; Verify Task A has BLOCKS property
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
          (expect blocks :to-equal '("task-c-id")))

        ;; Verify Task B has BLOCKS property
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
          (expect blocks :to-equal '("task-c-id"))))))



 (describe
  "cross-project dependencies (Story 5)"

  (it "shows tasks from different projects with project labels when selecting blockers"
      ;; This test verifies the acceptance criteria: "tasks from all existing agenda files clearly labeled by project"
      ;; Set up Project A in main GTD tasks file
      (ogt-capture-single-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1")
      (ogt-clarify-as-project)

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
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt collection &rest _args)
                           (when (= call-count 0) ; Only capture on first call
                             (setq presented-options collection))
                           (setq call-count (1+ call-count))
                           ;; Return empty string on second call to exit multi-select loop
                           (if (= call-count 1) "task-b1-id" ""))))
                (org-gtd-task-management--select-multiple-task-ids "Test prompt: "))

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
      (ogt-capture-single-item "Project A")
      (org-gtd-process-inbox)
      (goto-char (point-max))
      (newline)
      (insert "** Task A1")
      (ogt-clarify-as-project)

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
            (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                       (lambda (_prompt) '("task-b1-id"))))
              (org-gtd-task-add-blockers))

            ;; Verify Task A1 has DEPENDS_ON pointing to Task B1
            (let ((depends-on (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
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
                  (blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
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
        (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:DEPENDS_ON: task-a-id\n:END:\n\n")
        (insert "* TODO Task C\n:PROPERTIES:\n:ID: task-c-id\n:DEPENDS_ON: task-a-id\n:END:\n\n")

        ;; Add BLOCKS properties to Task A (bidirectional relationship)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (org-entry-put (point) "BLOCKS" "task-b-id task-c-id")

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

        (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:BLOCKS: task-c-id\n:END:\n\n")
        (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:BLOCKS: task-c-id\n:END:\n\n")
        (insert "* TODO Task C\n:PROPERTIES:\n:ID: task-c-id\n:DEPENDS_ON: task-a-id task-b-id\n:END:\n\n")

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

        (insert "* TODO Task A\n:PROPERTIES:\n:ID: task-a-id\n:BLOCKS: task-b-id\n:END:\n\n")
        (insert "* TODO Task B\n:PROPERTIES:\n:ID: task-b-id\n:DEPENDS_ON: task-a-id\n:END:\n\n")

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
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-a-id"))))
          (org-gtd-task-add-blockers))

        ;; Verify A blocks B relationship was created
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "BLOCKS") :to-equal '("task-b-id"))

        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "DEPENDS_ON") :to-equal '("task-a-id"))

        ;; Now try to create reverse relationship: B blocks A (should fail)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)

        ;; Mock the task selection to return task B as blocker for A (circular!)
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-b-id"))))
          ;; This should signal an error due to circular dependency
          (expect (org-gtd-task-add-blockers) :to-throw 'error '("Circular dependency detected: task-a-id -> task-b-id -> task-a-id")))

        ;; Verify that the circular relationship was NOT created
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "DEPENDS_ON") :to-be nil)

        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "BLOCKS") :to-be nil)))

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
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-a-id"))))
          (org-gtd-task-add-blockers))

        ;; Create B blocks C
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-b-id"))))
          (org-gtd-task-add-blockers))

        ;; Now try to create C blocks A (would create cycle: A -> B -> C -> A)
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                   (lambda (_prompt) '("task-c-id"))))
          ;; This should signal an error due to circular dependency
          (expect (org-gtd-task-add-blockers) :to-throw 'error '("Circular dependency detected: task-a-id -> task-b-id -> task-c-id -> task-a-id")))

        ;; Verify that the circular relationship was NOT created
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "DEPENDS_ON") :to-be nil)

        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
          ;; C should not block A (circular dependency prevented)
          (expect (member "task-a-id" blocks) :to-be nil)))))

 (describe
  "remove task blockers (Story 3)"

  (it "removes selected blocking relationships from task with existing blockers"
      ;; Test Story 3 acceptance criteria: remove specific blockers from task
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:BLOCKS: task-c-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:BLOCKS: task-c-id\n:END:\n\n")
        (insert "* Task C\n:PROPERTIES:\n:ID: task-c-id\n:DEPENDS_ON: task-a-id task-b-id\n:END:\n\n")

        ;; Go to Task C (which has blockers A and B)
        (goto-char (point-min))
        (re-search-forward "Task C")
        (org-back-to-heading t)

        ;; Mock the task selection to remove just Task A as blocker
        (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-blocking-task-ids)
                   (lambda (_prompt _current-blockers) '("task-a-id"))))
          (org-gtd-task-remove-blockers))

        ;; Verify Task C no longer depends on Task A, but still depends on Task B
        (let ((depends-on (org-entry-get-multivalued-property (point) "DEPENDS_ON")))
          (expect depends-on :to-equal '("task-b-id")))

        ;; Verify Task A no longer blocks Task C
        (goto-char (point-min))
        (re-search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
          (expect blocks :to-be nil))

        ;; Verify Task B still blocks Task C
        (goto-char (point-min))
        (re-search-forward "Task B")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "BLOCKS")))
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
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args)))))
            (org-gtd-task-remove-blockers)
            (expect message-output :to-equal "Task 'Task A' has no blockers to remove"))))))

 ;; Note: org-gtd-task-add-dependent tests will be implemented in Story 4
 ;; For now focusing on Stories 1-2: Add Task Blockers (Single + Multiple Selection)

 (describe
  "direct task dependency management from regular org files (Story 6)"

  (it "works in regular org-mode files with same interface and shows confirmation message"
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
        (let ((message-output nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq message-output (apply #'format format-string args))))
                    ((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                     (lambda (_prompt) '("task-a-id"))))
            (org-gtd-task-add-blockers)

            ;; Verify confirmation message was shown
            (expect message-output :to-match "Added blocker relationships:.*Task A.*block.*Task B"))

          ;; Verify the relationships were created in this regular org file
          ;; Task B should have DEPENDS_ON
          (expect (org-entry-get-multivalued-property (point) "DEPENDS_ON") :to-contain "task-a-id")

          ;; Task A should have BLOCKS
          (goto-char (point-min))
          (re-search-forward "Task A")
          (org-back-to-heading t)
          (expect (org-entry-get-multivalued-property (point) "BLOCKS") :to-contain "task-b-id")))))

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
         (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                    (lambda (_prompt)
                      ;; First trigger the real task collection to create IDs
                      (org-gtd-task-management--collect-all-task-info)
                      ;; Now capture Task A's ID after it gets created during collection
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "Task A")
                        (org-back-to-heading t)
                        (setq task-a-id (org-entry-get (point) "ID")))
                      (list task-a-id))))
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
           (expect (org-entry-get-multivalued-property (point) "BLOCKS") :to-contain task-b-id)

           (goto-char (point-min))
           (re-search-forward "Task B")
           (org-back-to-heading t)
           (expect (org-entry-get-multivalued-property (point) "DEPENDS_ON") :to-contain task-a-id)))))

 (it "works with mix of tasks with and without pre-existing IDs"
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
         (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                    (lambda (_prompt)
                      ;; First trigger the real task collection to create IDs
                      (org-gtd-task-management--collect-all-task-info)
                      ;; The selection should create ID for Task A during collection
                      (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "Task A")
                        (org-back-to-heading t)
                        (setq task-a-id (org-entry-get (point) "ID")))
                      (list task-a-id))))

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
           (expect (org-entry-get-multivalued-property (point) "DEPENDS_ON") :to-contain task-a-id)
           (goto-char (point-min))
           (re-search-forward "Task A")
           (org-back-to-heading t)
           (expect (org-entry-get-multivalued-property (point) "BLOCKS") :to-contain "existing-task-b-id"))))

     )) ; Close lazy ID creation describe block

;;; task-management-commands-test.el ends here
