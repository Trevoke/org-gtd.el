;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(load "test/helpers/processing.el")
(load "test/helpers/clarifying.el")
(require 'org-gtd)
(require 'buttercup)

(describe
 "End-to-End GTD Workflow Tests"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "Single Action workflow"
   (it "captures, processes, organizes, shows in agenda, and archives"
       ;; 1. CAPTURE
       (ogt-capture-single-item "Buy groceries")
       
       ;; 2. PROCESS
       (org-gtd-process-inbox)
       
       ;; 3. ORGANIZE (single action)
       (ogt-clarify-as-single-action)
       
       ;; 4. VERIFY in agenda
       (org-gtd-engage)
       (expect (ogt--buffer-string org-agenda-buffer)
               :to-match "Buy groceries")
       
       ;; 5. COMPLETE and verify archival
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (re-search-forward "Buy groceries")
         (org-todo "DONE"))
       
       ;; 6. ARCHIVE
       (org-gtd-archive-completed-items)
       
       ;; Verify item is archived
       (with-current-buffer (org-gtd--default-file)
         (expect (ogt--current-buffer-raw-text)
                 :not :to-match "Buy groceries"))))

 (describe "Project workflow"
   (it "captures, processes, organizes, shows in agenda, and archives"
       ;; 1. CAPTURE
       (ogt-capture-single-item "Plan vacation")
       
       ;; 2. PROCESS  
       (org-gtd-process-inbox)
       
       ;; 3. ORGANIZE (project with tasks)
       (let ((wip-buffers (seq-filter (lambda (buf) 
                                        (string-match-p org-gtd-wip--prefix (buffer-name buf))) 
                                      (buffer-list))))
         (when wip-buffers
           (with-current-buffer (car wip-buffers)
             (goto-char (point-max))
             (newline)
             (insert "** Research destinations\n** Book flights\n** Reserve hotel")
             (ogt-clarify-as-project))))
       
       ;; 4. VERIFY in agenda
       (org-gtd-engage)
       (let ((agenda-content (ogt--buffer-string org-agenda-buffer)))
         (expect agenda-content :to-match "Plan vacati")  ; Truncated in agenda display
         (expect agenda-content :to-match "Research destinations"))
       
       ;; 5. COMPLETE project and verify archival
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (re-search-forward "Research destinations")
         (org-todo "DONE")
         (re-search-forward "Book flights") 
         (org-todo "DONE")
         (re-search-forward "Reserve hotel")
         (org-todo "DONE"))
       
       ;; 6. ARCHIVE
       (org-gtd-archive-completed-items)
       
       ;; Verify project is archived
       (with-current-buffer (org-gtd--default-file)
         (expect (ogt--current-buffer-raw-text)
                 :not :to-match "Plan vacation"))))

 (describe "Calendar workflow"
   (it "captures, processes, organizes, shows in agenda, and archives"
       ;; 1. CAPTURE
       (ogt-capture-single-item "Doctor appointment")
       
       ;; 2. PROCESS
       (org-gtd-process-inbox)
       
       ;; 3. ORGANIZE (calendar item)
       (ogt-clarify-as-calendar-item (calendar-current-date))
       
       ;; 4. VERIFY in agenda 
       (org-gtd-engage)
       (expect (ogt--buffer-string org-agenda-buffer)
               :to-match "Doctor appointment")
       
       ;; 5. COMPLETE and archive
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (re-search-forward "Doctor appointment")
         (org-todo "DONE"))
       
       (org-gtd-archive-completed-items)
       
       ;; Verify archived
       (with-current-buffer (org-gtd--default-file)
         (expect (ogt--current-buffer-raw-text)
                 :not :to-match "Doctor appointment"))))

 (describe "Delegated workflow"
   (it "captures, processes, organizes, shows in agenda, and archives"
       ;; 1. CAPTURE
       (ogt-capture-single-item "Get report from John")
       
       ;; 2. PROCESS
       (org-gtd-process-inbox)
       
       ;; 3. ORGANIZE (delegate)
       (ogt-clarify-as-delegated-item "John" (calendar-current-date))
       
       ;; 4. VERIFY in agenda
       (org-gtd-engage)
       (expect (ogt--buffer-string org-agenda-buffer)
               :to-match "Get report from John")
       
       ;; 5. COMPLETE and archive
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (re-search-forward "Get report from John")
         (org-todo "DONE"))
       
       (org-gtd-archive-completed-items)
       
       ;; Verify archived
       (with-current-buffer (org-gtd--default-file)
         (expect (ogt--current-buffer-raw-text)
                 :not :to-match "Get report from John"))))

 (describe "Incubated workflow"
   (it "captures, processes, organizes, and handles someday/maybe items"
       ;; 1. CAPTURE
       (ogt-capture-single-item "Learn Spanish")
       
       ;; 2. PROCESS
       (org-gtd-process-inbox)
       
       ;; 3. ORGANIZE (incubate)
       (ogt-clarify-as-incubated-item (calendar-current-date))
       
       ;; 4. VERIFY stored in main GTD file under Incubated
       (with-current-buffer (org-gtd--default-file)
         (expect (ogt--current-buffer-raw-text)
                 :to-match "Learn Spanish"))
       
       ;; Incubated items DO show in agenda with their scheduled date
       (org-gtd-engage)
       (expect (ogt--buffer-string org-agenda-buffer)
               :to-match "Learn Spanish")))

 (describe "Knowledge workflow"
   (it "captures, processes, organizes, and stores reference material"
       ;; 1. CAPTURE
       (ogt-capture-single-item "Git commands reference")
       
       ;; 2. PROCESS
       (org-gtd-process-inbox)
       
       ;; 3. ORGANIZE (knowledge) - ensure we're in WIP buffer
       (let ((wip-buffers (seq-filter (lambda (buf) 
                                        (string-match-p org-gtd-wip--prefix (buffer-name buf))) 
                                      (buffer-list))))
         (when wip-buffers
           (with-current-buffer (car wip-buffers)
             (ogt-clarify-as-knowledge-item)))
         (unless wip-buffers
           (ogt-clarify-as-knowledge-item)))  ; Fallback
       
       ;; 4. VERIFY that knowledge items are archived immediately
       ;; (they don't stay in the main GTD file)
       (with-current-buffer (org-gtd--default-file)
         (expect (ogt--current-buffer-raw-text)
                 :not :to-match "Git commands reference"))
       
       ;; Knowledge items don't show in daily agenda (they're archived)
       (org-gtd-engage)
       (expect (ogt--buffer-string org-agenda-buffer)
               :not :to-match "Git commands reference")))

 (describe "Multiple item processing"
   (it "processes multiple different item types in sequence"
       ;; Capture multiple items
       (ogt-capture-single-item "Call mom")           ; single action
       (ogt-capture-single-item "Team meeting")       ; calendar
       (ogt-capture-single-item "Organize garage")    ; project
       (ogt-capture-single-item "Wine recipe")        ; knowledge
       
       ;; Process them all
       (org-gtd-process-inbox)
       
       ;; Organize each appropriately
       (ogt-clarify-as-single-action)                 ; Call mom
       (ogt-clarify-as-calendar-item (calendar-current-date)) ; Team meeting
       
       ;; Project with subtasks
       (let ((wip-buffers (seq-filter (lambda (buf) 
                                        (string-match-p org-gtd-wip--prefix (buffer-name buf))) 
                                      (buffer-list))))
         (when wip-buffers
           (with-current-buffer (car wip-buffers)
             (goto-char (point-max))
             (newline)
             (insert "** Sort items\n** Donate old items\n** Clean floor")
             (ogt-clarify-as-project))))
       
       (ogt-clarify-as-knowledge-item)               ; Wine recipe
       
       ;; Verify all items are properly organized
       (org-gtd-engage)
       (let ((agenda-content (ogt--buffer-string org-agenda-buffer)))
         (expect agenda-content :to-match "Call mom")
         (expect agenda-content :to-match "Team meeting") 
         (expect agenda-content :to-match "Organize ga")  ; Truncated in agenda display
         (expect agenda-content :to-match "Sort items")
         ;; Knowledge item should not appear in agenda (archived immediately)
         (expect agenda-content :not :to-match "Wine recipe"))
       
       ;; Verify inbox is empty after processing
       (with-current-buffer (ogt-inbox-buffer)
         (expect (ogt--current-buffer-raw-text)
                 :not :to-match "Call mom\\|Team meeting\\|Organize garage\\|Wine recipe"))))

 (describe "Complete GTD workflow integration"
   (it "tests capture -> process -> organize -> engage -> archive cycle"
       ;; Start with empty system (inbox has default comment)
       (expect (with-current-buffer (ogt-inbox-buffer)
                 (ogt--current-buffer-raw-text))
               :not :to-match "Review budget\\|Birthday party\\|Emacs manual")
       
       ;; Capture multiple items throughout "day"
       (ogt-capture-single-item "Review budget")
       (ogt-capture-single-item "Birthday party planning")
       (ogt-capture-single-item "Read Emacs manual chapter")
       
       ;; Process inbox
       (org-gtd-process-inbox)
       
       ;; Organize items
       (ogt-clarify-as-single-action)  ; Review budget
       
       ;; Project with multiple tasks
       (let ((wip-buffers (seq-filter (lambda (buf) 
                                        (string-match-p org-gtd-wip--prefix (buffer-name buf))) 
                                      (buffer-list))))
         (when wip-buffers
           (with-current-buffer (car wip-buffers)
             (goto-char (point-max))
             (newline)
             (insert "** Choose venue\n** Send invitations\n** Order cake")
             (ogt-clarify-as-project))))
       
       (ogt-clarify-as-knowledge-item)  ; Emacs manual
       
       ;; Engage with agenda
       (org-gtd-engage)
       (let ((agenda-content (ogt--buffer-string org-agenda-buffer)))
         (expect agenda-content :to-match "Review budget")
         (expect agenda-content :to-match "Birthday pa")  ; Truncated in agenda display
         (expect agenda-content :to-match "Choose venue"))
       
       ;; Complete some work
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (re-search-forward "Review budget")
         (org-todo "DONE")
         (re-search-forward "Choose venue")
         (org-todo "DONE"))
       
       ;; Archive completed items
       (org-gtd-archive-completed-items)
       
       ;; Verify system state
       (with-current-buffer (org-gtd--default-file)
         (let ((content (ogt--current-buffer-raw-text)))
           ;; Completed single actions should be archived
           (expect content :not :to-match "Review budget")
           ;; Choose venue is part of an incomplete project, so it remains visible
           ;; Incomplete items should remain
           (expect content :to-match "Birthday party planning")
           (expect content :to-match "Send invitations")
           (expect content :to-match "Order cake")
           ;; Knowledge items are archived immediately, so they won't remain
           (expect content :not :to-match "Read Emacs manual chapter")))
       
       ;; Inbox should be empty (except for default comment)
       (with-current-buffer (ogt-inbox-buffer)
         (expect (ogt--current-buffer-raw-text)
                 :not :to-match "Review budget\\|Birthday party\\|Emacs manual")))))