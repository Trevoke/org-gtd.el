;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Processing items"

 :var ((inhibit-message t))

 (before-each
  (ogt--configure-emacs)
  (capture-inbox-item))
 (after-each (ogt--close-and-delete-files))

 (it "processes and organizes all inbox items leaving inbox empty"
     ;; Capture some additional items (note: before-each already captures one)
     (capture-inbox-item "test project")
     (capture-inbox-item "test calendar item")
     (capture-inbox-item "test delegated item")
     (capture-inbox-item "test tickler item")
     (capture-inbox-item "test single action")
     (capture-inbox-item "test knowledge item")

     ;; Process them all using helper functions (7 items total)
     (org-gtd-process-inbox)
     ;; First item from before-each (single action)
     (organize-as-single-action)

     ;; Items we captured in this test - create project using builder
     (with-wip-buffer
       (goto-char (point-max))
       (newline)
       (make-task "Task 1" :level 2)
       (make-task "Task 2" :level 2)
       (make-task "Task 3" :level 2)
       (organize-as-project))

     (schedule-item (calendar-current-date))
     (delegate-item "Someone" (calendar-current-date))
     (defer-item (calendar-current-date))
     (organize-as-single-action)
     (archive-as-reference)

     ;; Check inbox is empty
     (with-current-buffer (ogt-inbox-buffer)
       (expect (current-buffer-raw-text)
               :not :to-match
               "test")))

 (it "uses configurable decorations on the processed items"
     ;; Define a simple test hook that adds a priority
     (defun test-hook-add-priority ()
       "Test hook that adds priority A without user input."
       (org-priority ?A))

     (let ((org-gtd-organize-hooks '(test-hook-add-priority)))
       (org-gtd-process-inbox)
       (organize-as-single-action))

     (org-gtd-engage)
     (let ((ogt-agenda-string (agenda-raw-text)))
       (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string)
               :to-be-truthy)))

 (it "displays organized item in daily agenda after processing"
     (org-gtd-process-inbox)
     (organize-as-single-action)
     (expect (buffer-modified-p (org-gtd--default-file)) :to-equal t)

     (org-gtd-engage)
     (let ((ogt-agenda-string (agenda-raw-text)))
       (expect (string-match "single action" ogt-agenda-string)
               :to-be-truthy)))

 (describe
  "error management"

  (it "allows organizing a valid project with tasks"
      ;; This test verifies the happy path works
      (org-gtd-process-inbox)
      ;; Add tasks to make it a valid project
      (with-wip-buffer
        (goto-char (point-max))
        (newline)
        (make-task "First task" :level 2)
        (make-task "Second task" :level 2))
      (organize-as-project)
      ;; Should succeed and not be in WIP buffer
      (expect (buffer-name) :not :to-match org-gtd-wip--prefix))

  (it "rejects project with no tasks and returns to editing"
      ;; Simulate user interaction with simulated-input
      (with-simulated-input "RET" ;; Press enter to dismiss error message
        (org-gtd-process-inbox)
        ;; Don't add any tasks - project has no child headings
        (organize-as-project)
        ;; Should stay in WIP buffer after error
        (expect (buffer-name) :to-match org-gtd-wip--prefix))))
 )
