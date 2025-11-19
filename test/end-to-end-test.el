;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



;; Load test helpers via setup.el (which now uses require internally)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "End-to-End GTD Workflow Tests"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "Single Action workflow"
   (it "captures, processes, organizes, shows in agenda, and archives"
       ;; 1. CAPTURE
       (capture-inbox-item "Buy groceries")

       ;; 2. PROCESS
       (org-gtd-process-inbox)

       ;; 3. ORGANIZE (single action)
       (organize-as-single-action)

       ;; 4. VERIFY in agenda
       (org-gtd-engage)
       (expect (agenda-raw-text)
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
         (expect (current-buffer-raw-text)
                 :not :to-match "Buy groceries"))))

 (describe "Project workflow"
   (it "captures, processes, organizes, shows in agenda, and archives"
       ;; 1. CAPTURE
       (capture-inbox-item "Plan vacation")

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
             (make-task "Research destinations" :level 2)
             (make-task "Book flights" :level 2)
             (make-task "Reserve hotel" :level 2)
             (organize-as-project))))

       ;; 4. VERIFY in agenda
       (org-gtd-engage)
       (let ((agenda-content (agenda-raw-text)))
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
         (expect (current-buffer-raw-text)
                 :not :to-match "Plan vacation"))))

 (describe "Calendar workflow"
   (it "captures, processes, organizes, shows in agenda, and archives"
       ;; 1. CAPTURE
       (capture-inbox-item "Doctor appointment")

       ;; 2. PROCESS
       (org-gtd-process-inbox)

       ;; 3. ORGANIZE (calendar item)
       (schedule-item (calendar-current-date))

       ;; 4. VERIFY in agenda
       (org-gtd-engage)
       (expect (agenda-raw-text)
               :to-match "Doctor appointment")

       ;; 5. COMPLETE and archive
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (re-search-forward "Doctor appointment")
         (org-todo "DONE"))

       (org-gtd-archive-completed-items)

       ;; Verify archived
       (with-current-buffer (org-gtd--default-file)
         (expect (current-buffer-raw-text)
                 :not :to-match "Doctor appointment"))))

 (describe "Delegated workflow"
   (it "captures, processes, organizes, shows in agenda, and archives"
       ;; 1. CAPTURE
       (capture-inbox-item "Get report from John")

       ;; 2. PROCESS
       (org-gtd-process-inbox)

       ;; 3. ORGANIZE (delegate)
       (delegate-item "John" (calendar-current-date))

       ;; 4. VERIFY in agenda
       (org-gtd-engage)
       (expect (agenda-raw-text)
               :to-match "Get report from John")

       ;; 5. COMPLETE and archive
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (re-search-forward "Get report from John")
         (org-todo "DONE"))

       (org-gtd-archive-completed-items)

       ;; Verify archived
       (with-current-buffer (org-gtd--default-file)
         (expect (current-buffer-raw-text)
                 :not :to-match "Get report from John"))))

 (describe "Incubated workflow"
   (it "captures, processes, organizes, and handles someday/maybe items"
       ;; 1. CAPTURE
       (capture-inbox-item "Learn Spanish")

       ;; 2. PROCESS
       (org-gtd-process-inbox)

       ;; 3. ORGANIZE (incubate)
       (defer-item (calendar-current-date))

       ;; 4. VERIFY stored in main GTD file under Incubated
       (with-current-buffer (org-gtd--default-file)
         (expect (current-buffer-raw-text)
                 :to-match "Learn Spanish"))

       ;; Incubated items DO show in agenda with their scheduled date
       (org-gtd-engage)
       (expect (agenda-raw-text)
               :to-match "Learn Spanish")))

 (describe "Knowledge workflow"
   (it "captures, processes, organizes, and stores reference material"
       ;; 1. CAPTURE
       (capture-inbox-item "Git commands reference")

       ;; 2. PROCESS
       (org-gtd-process-inbox)

       ;; 3. ORGANIZE (knowledge) - ensure we're in WIP buffer
       (let ((wip-buffers (seq-filter (lambda (buf)
                                        (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                      (buffer-list))))
         (when wip-buffers
           (with-current-buffer (car wip-buffers)
             (archive-as-reference)))
         (unless wip-buffers
           (archive-as-reference)))  ; Fallback

       ;; 4. VERIFY that knowledge items are archived immediately
       ;; (they don't stay in the main GTD file)
       (with-current-buffer (org-gtd--default-file)
         (expect (current-buffer-raw-text)
                 :not :to-match "Git commands reference"))

       ;; Knowledge items don't show in daily agenda (they're archived)
       (org-gtd-engage)
       (expect (agenda-raw-text)
               :not :to-match "Git commands reference")))

 (describe "Multiple item processing"
   (it "processes multiple different item types in sequence"
       ;; Capture multiple items
       (capture-inbox-item "Call mom")           ; single action
       (capture-inbox-item "Team meeting")       ; calendar
       (capture-inbox-item "Organize garage")    ; project
       (capture-inbox-item "Wine recipe")        ; knowledge

       ;; Process them all
       (org-gtd-process-inbox)

       ;; Organize each appropriately
       (organize-as-single-action)                 ; Call mom
       (schedule-item (calendar-current-date)) ; Team meeting

       ;; Project with subtasks
       (let ((wip-buffers (seq-filter (lambda (buf)
                                        (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                      (buffer-list))))
         (when wip-buffers
           (with-current-buffer (car wip-buffers)
             (goto-char (point-max))
             (newline)
             (make-task "Sort items" :level 2)
             (make-task "Donate old items" :level 2)
             (make-task "Clean floor" :level 2)
             (organize-as-project))))

       (archive-as-reference)               ; Wine recipe

       ;; Verify all items are properly organized
       (org-gtd-engage)
       (let ((agenda-content (agenda-raw-text)))
         (expect agenda-content :to-match "Call mom")
         (expect agenda-content :to-match "Team meeting")
         (expect agenda-content :to-match "Organize ga")  ; Truncated in agenda display
         (expect agenda-content :to-match "Sort items")
         ;; Knowledge item should not appear in agenda (archived immediately)
         (expect agenda-content :not :to-match "Wine recipe"))

       ;; Verify inbox is empty after processing
       (with-current-buffer (ogt-inbox-buffer)
         (expect (current-buffer-raw-text)
                 :not :to-match "Call mom\\|Team meeting\\|Organize garage\\|Wine recipe"))))

 (describe "Complete GTD workflow integration"
   (it "tests capture -> process -> organize -> engage -> archive cycle"
       ;; Start with empty system (inbox has default comment)
       (expect (with-current-buffer (ogt-inbox-buffer)
                 (current-buffer-raw-text))
               :not :to-match "Review budget\\|Birthday party\\|Emacs manual")

       ;; Capture multiple items throughout "day"
       (capture-inbox-item "Review budget")
       (capture-inbox-item "Birthday party planning")
       (capture-inbox-item "Read Emacs manual chapter")

       ;; Process inbox
       (org-gtd-process-inbox)

       ;; Organize items
       (organize-as-single-action)  ; Review budget

       ;; Project with multiple tasks
       (let ((wip-buffers (seq-filter (lambda (buf)
                                        (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                      (buffer-list))))
         (when wip-buffers
           (with-current-buffer (car wip-buffers)
             (goto-char (point-max))
             (newline)
             (make-task "Choose venue" :level 2)
             (make-task "Send invitations" :level 2)
             (make-task "Order cake" :level 2)
             (organize-as-project))))

       (archive-as-reference)  ; Emacs manual

       ;; Engage with agenda
       (org-gtd-engage)
       (let ((agenda-content (agenda-raw-text)))
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
         (let ((content (current-buffer-raw-text)))
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
         (expect (current-buffer-raw-text)
                 :not :to-match "Review budget\\|Birthday party\\|Emacs manual")))))

(describe "Cancel and Archive Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Single action cancellation"
    (it "captures, organizes as single action, marks CNCL, verifies doesn't show in engage, then archives"
        ;; 1. CAPTURE
        (capture-inbox-item "Buy concert tickets")

        ;; 2. PROCESS
        (org-gtd-process-inbox)

        ;; 3. ORGANIZE (single action)
        (organize-as-single-action)

        ;; 4. VERIFY in agenda before cancellation
        (org-gtd-engage)
        (expect (agenda-raw-text)
                :to-match "Buy concert tickets")

        ;; 5. CANCEL the action
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Buy concert tickets")
          (org-todo "CNCL"))

        ;; 6. VERIFY doesn't show in engage after cancellation
        (org-gtd-engage)
        (expect (agenda-raw-text)
                :not :to-match "Buy concert tickets")

        ;; 7. ARCHIVE
        (org-gtd-archive-completed-items)

        ;; 8. Verify item is NOT in main file after archive
        (with-current-buffer (org-gtd--default-file)
          (expect (current-buffer-raw-text)
                  :not :to-match "Buy concert tickets"))

        ;; 9. Verify item IS in archive file
        (let ((archived-content (ogt--archive-string)))
          (expect archived-content :to-match "Buy concert tickets"))))

  (describe "Calendar item cancellation"
    (it "captures, organizes as calendar, marks CNCL, and archives"
        ;; Calendar items that are canceled should NOT appear in engage views.
        ;; This test verifies the product requirement: "Canceled items don't show in engage views".

        ;; 1. CAPTURE
        (capture-inbox-item "Dentist appointment")

        ;; 2. PROCESS
        (org-gtd-process-inbox)

        ;; 3. ORGANIZE (calendar item)
        (schedule-item (calendar-current-date))

        ;; 4. VERIFY in agenda before cancellation
        (org-gtd-engage)
        (expect (agenda-raw-text)
                :to-match "Dentist appointment")

        ;; 5. CANCEL the appointment
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Dentist appointment")
          (org-todo "CNCL"))

        ;; 6. VERIFY does NOT show in engage after cancellation
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :not :to-match "Dentist appointment"))

        ;; 7. ARCHIVE - canceled items CAN be archived
        (org-gtd-archive-completed-items)

        ;; 8. Verify item is NOT in main file after archive
        (with-current-buffer (org-gtd--default-file)
          (expect (current-buffer-raw-text)
                  :not :to-match "Dentist appointment"))

        ;; 9. Verify item IS in archive file
        (let ((archived-content (ogt--archive-string)))
          (expect archived-content :to-match "Dentist appointment"))))

  (describe "Delegated item cancellation"
    (it "captures, organizes as delegated, marks CNCL, and archives"
        ;; Delegated items that are canceled should NOT appear in engage views.
        ;; This test verifies the product requirement: "Canceled items don't show in engage views".

        ;; 1. CAPTURE
        (capture-inbox-item "Get feedback from Sarah")

        ;; 2. PROCESS
        (org-gtd-process-inbox)

        ;; 3. ORGANIZE (delegate)
        (delegate-item "Sarah" (calendar-current-date))

        ;; 4. VERIFY in agenda before cancellation
        (org-gtd-engage)
        (expect (agenda-raw-text)
                :to-match "Get feedback from Sarah")

        ;; 5. CANCEL the delegation
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Get feedback from Sarah")
          (org-todo "CNCL"))

        ;; 6. VERIFY does NOT show in engage after cancellation
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :not :to-match "Get feedback from Sarah"))

        ;; 7. ARCHIVE - canceled items CAN be archived
        (org-gtd-archive-completed-items)

        ;; 8. Verify item is NOT in main file after archive
        (with-current-buffer (org-gtd--default-file)
          (expect (current-buffer-raw-text)
                  :not :to-match "Get feedback from Sarah"))

        ;; 9. Verify item IS in archive file
        (let ((archived-content (ogt--archive-string)))
          (expect archived-content :to-match "Get feedback from Sarah"))))

  (describe "Project task cancellation"
    (it "creates project, cancels one task, verifies project still works, completes remaining tasks, then archives"
        ;; 1. CAPTURE
        (capture-inbox-item "Organize workshop")

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
              (make-task "Book venue" :level 2)
              (make-task "Prepare materials" :level 2)
              (make-task "Send invitations" :level 2)
              (organize-as-project))))

        ;; 4. VERIFY all tasks in agenda
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :to-match "Organize wo")  ; Truncated
          (expect agenda-content :to-match "Book venue"))

        ;; 5. CANCEL one task (Prepare materials)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Prepare materials")
          (org-todo "CNCL"))

        ;; 6. VERIFY project still works - canceled task doesn't show, but project continues
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          ;; Project heading should still show (has incomplete tasks)
          (expect agenda-content :to-match "Organize wo")
          ;; Canceled task should not show
          (expect agenda-content :not :to-match "Prepare materials")
          ;; Other tasks should still show
          (expect agenda-content :to-match "Book venue"))

        ;; 7. COMPLETE remaining tasks (Book venue and Send invitations)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Book venue")
          (org-todo "DONE")
          (re-search-forward "Send invitations")
          (org-todo "DONE"))

        ;; 8. ARCHIVE - project should archive even with canceled task
        (org-gtd-archive-completed-items)

        ;; 9. Verify project is archived (including canceled task)
        (with-current-buffer (org-gtd--default-file)
          (expect (current-buffer-raw-text)
                  :not :to-match "Organize workshop"))))

  (describe "Project with all tasks canceled (same file)"
    (it "creates project, cancels all tasks, verifies nothing shows in engage, then archives"
        ;; This test verifies the critical requirement: Canceled items MUST NOT appear in engage.
        ;; It also verifies that projects with all tasks canceled can be properly archived.

        ;; 1. CAPTURE
        (capture-inbox-item "Build garden shed")

        ;; 2. PROCESS
        (org-gtd-process-inbox)

        ;; 3. ORGANIZE (project with multiple tasks in same file)
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Buy lumber" :level 2)
              (make-task "Purchase tools" :level 2)
              (make-task "Build foundation" :level 2)
              (organize-as-project))))

        ;; 4. VERIFY all tasks appear in engage before cancellation
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :to-match "Build garde")  ; Project heading (truncated)
          (expect agenda-content :to-match "Buy lumber"))

        ;; 5. CANCEL all tasks manually
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Buy lumber")
          (org-todo "CNCL")
          (goto-char (point-min))
          (re-search-forward "Purchase tools")
          (org-todo "CNCL")
          (goto-char (point-min))
          (re-search-forward "Build foundation")
          (org-todo "CNCL"))

        ;; 6. VERIFY NO tasks appear in engage after cancellation (CRITICAL REQUIREMENT)
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (expect agenda-content :not :to-match "Build garde")
          (expect agenda-content :not :to-match "Buy lumber")
          (expect agenda-content :not :to-match "Purchase tools")
          (expect agenda-content :not :to-match "Build foundation"))

        ;; 7. ARCHIVE - project with all canceled tasks should be archivable
        (org-gtd-archive-completed-items)

        ;; 8. VERIFY project and all tasks are NOT in main file after archive
        (with-current-buffer (org-gtd--default-file)
          (let ((content (current-buffer-raw-text)))
            (expect content :not :to-match "Build garden shed")
            (expect content :not :to-match "Buy lumber")
            (expect content :not :to-match "Purchase tools")
            (expect content :not :to-match "Build foundation")))

        ;; 9. VERIFY project and tasks ARE in archive file
        (let ((archived-content (ogt--archive-string)))
          (expect archived-content :to-match "Build garden shed")
          (expect archived-content :to-match "Buy lumber")
          (expect archived-content :to-match "Purchase tools")
          (expect archived-content :to-match "Build foundation")))))

(describe "Review Flow Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Review of active items"
    (it "verifies single action appears in all next actions view"
        ;; 1. CAPTURE and ORGANIZE single action
        (capture-inbox-item "Review quarterly goals")
        (org-gtd-process-inbox)
        (organize-as-single-action)

        ;; 2. VERIFY appears in all next actions view
        (org-gtd-show-all-next)
        (expect (agenda-raw-text)
                :to-match "Review quarterly goals")))

  (describe "Review of missed items"
    (it "verifies delegated item with past date shows in missed review"
        ;; Create past date (7 days ago)
        (let* ((past-date-time (time-subtract (current-time) (days-to-time 7)))
               (past-date (decode-time past-date-time))
               (past-calendar-date (list (nth 4 past-date)  ; month
                                        (nth 3 past-date)  ; day
                                        (nth 5 past-date))))  ; year

          ;; 1. CAPTURE and ORGANIZE delegated item with past date
          (capture-inbox-item "Get contract from legal")
          (org-gtd-process-inbox)
          (delegate-item "Legal" past-calendar-date)

          ;; 2. VERIFY appears in missed items review
          (org-gtd-review-missed-items)
          (expect (agenda-raw-text)
                  :to-match "Get contract from legal")))

    (it "verifies calendar item with past date shows in missed review"
        ;; Create past date (3 days ago)
        (let* ((past-date-time (time-subtract (current-time) (days-to-time 3)))
               (past-date (decode-time past-date-time))
               (past-calendar-date (list (nth 4 past-date)  ; month
                                        (nth 3 past-date)  ; day
                                        (nth 5 past-date))))  ; year

          ;; 1. CAPTURE and ORGANIZE calendar item with past date
          (capture-inbox-item "Client presentation")
          (org-gtd-process-inbox)
          (schedule-item past-calendar-date)

          ;; 2. VERIFY appears in missed items review
          (org-gtd-review-missed-items)
          (expect (agenda-raw-text)
                  :to-match "Client presentation")))

    (it "verifies incubated item with past date shows in missed review"
        ;; Create past date (5 days ago)
        (let* ((past-date-time (time-subtract (current-time) (days-to-time 5)))
               (past-date (decode-time past-date-time))
               (past-calendar-date (list (nth 4 past-date)  ; month
                                        (nth 3 past-date)  ; day
                                        (nth 5 past-date))))  ; year

          ;; 1. CAPTURE and ORGANIZE incubated item with past date
          (capture-inbox-item "Review investment portfolio")
          (org-gtd-process-inbox)
          (defer-item past-calendar-date)

          ;; 2. VERIFY appears in missed items review
          (org-gtd-review-missed-items)
          (expect (agenda-raw-text)
                  :to-match "Review investment portfolio"))))

  (describe "Review of projects"
    (it "verifies project shows in stuck projects review when it has no NEXT actions"
        ;; NOTE: This test documents expected behavior for stuck projects.
        ;; A stuck project is one that has TODO state but no NEXT or WAIT tasks.
        ;; Since org-gtd creates projects with NEXT tasks by default via edna triggers,
        ;; we need to transition tasks to TODO (without NEXT) to make the project stuck.

        ;; 1. CAPTURE and ORGANIZE project
        (capture-inbox-item "Launch new website")
        (org-gtd-process-inbox)

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Design mockups" :level 2)
              (make-task "Write content" :level 2)
              (make-task "Deploy site" :level 2)
              (organize-as-project))))

        ;; 2. Make project stuck by transitioning NEXT tasks back to TODO
        ;; This creates a project with tasks but no next actions available
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Design mockups")
          (org-todo "TODO")
          (re-search-forward "Write content")
          (org-todo "TODO")
          (re-search-forward "Deploy site")
          (org-todo "TODO"))

        ;; 3. VERIFY project appears in stuck projects review
        (org-gtd-review-stuck-projects)
        (expect (agenda-raw-text)
                :to-match "Launch new website"))

    (it "verifies completed project does NOT appear in stuck projects review"
        ;; NOTE: This test verifies the disambiguation between stuck and completed projects.
        ;; A completed project (all tasks DONE/CNCL) should:
        ;; - NOT appear in stuck projects review (has no work remaining)
        ;; - SHOULD appear in completed projects review (ready for archiving)

        ;; 1. CAPTURE and ORGANIZE project
        (capture-inbox-item "Finished Campaign")
        (org-gtd-process-inbox)

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Research audience" :level 2)
              (make-task "Create content" :level 2)
              (make-task "Launch campaign" :level 2)
              (organize-as-project))))

        ;; 2. Complete all tasks (mark as DONE)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Research audience")
          (org-todo "DONE")
          (goto-char (point-min))
          (re-search-forward "Create content")
          (org-todo "DONE")
          (goto-char (point-min))
          (re-search-forward "Launch campaign")
          (org-todo "DONE"))

        ;; 3. VERIFY project does NOT appear in stuck projects review
        (org-gtd-review-stuck-projects)
        (expect (agenda-raw-text)
                :not :to-match "Finished Campaign")

        ;; 4. VERIFY project DOES appear in completed projects review
        (org-gtd-review-completed-projects)
        (expect (agenda-raw-text)
                :to-match "Finished Campaign")))

  (describe "Engage view for habits"
    (it "verifies habit appears in engage view"
        ;; 1. CAPTURE and ORGANIZE habit with daily repeater
        (capture-inbox-item "Daily meditation")
        (org-gtd-process-inbox)
        (organize-as-habit "+1d")

        ;; 2. VERIFY appears in engage view
        (org-gtd-engage)
        (expect (agenda-raw-text)
                :to-match "Daily meditation")))

  (describe "Review of habits"
    (it "verifies habit appears in area of focus review"
        ;; Habits should appear in review views to ensure they're being maintained
        (let ((org-gtd-areas-of-focus '("Personal" "Work" "Health")))

          ;; 1. CAPTURE and ORGANIZE habit with daily repeater
          (capture-inbox-item "Morning workout")
          (org-gtd-process-inbox)
          (organize-as-habit "+1d")

          ;; 2. Set area of focus (uses CATEGORY property)
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (re-search-forward "Morning workout")
            (org-set-property "CATEGORY" "Health"))

          ;; 3. VERIFY appears in area of focus review
          (org-gtd-review-area-of-focus "Health")
          (expect (agenda-raw-text)
                  :to-match "Morning workout"))))

  (describe "Review of multi-file projects"
    (it "verifies multi-file project does NOT appear stuck when it has NEXT task in other file"
        ;; NOTE: This test verifies the IMPROVED behavior with DSL-based stuck detection.
        ;; The new implementation uses org-gtd-projects--is-stuck-p which traverses
        ;; the dependency graph across files via ORG_GTD_FIRST_TASKS relationships.
        ;; Projects with NEXT/WAIT tasks in other files are correctly identified as
        ;; NOT stuck, which is an improvement over org-mode's native stuck projects
        ;; view that only looks at children of the project heading.

        ;; 1. CAPTURE and ORGANIZE project in main file
        (capture-inbox-item "Multi-file project review")
        (org-gtd-process-inbox)

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task in main file" :level 2)
              (organize-as-project))))

        ;; 2. Create second file with NEXT task
        (let ((second-file (org-gtd--path "review-secondary")))
          (with-temp-file second-file
            (make-task "Task in second file" :id "review-task-id" :level 1))

          (with-current-buffer (find-file-noselect second-file)
            (org-mode)
            (goto-char (point-min))
            (search-forward "Task in second file")
            (org-back-to-heading t)
            (org-id-add-location "review-task-id" second-file)
            (org-todo "NEXT"))

          (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

            ;; 3. Link task from second file to project
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Multi-file project review")
              (org-back-to-heading t)
              (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "review-task-id"))

            ;; 4. Make main file task TODO (project has work, but also has NEXT in other file)
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Task in main file")
              (org-todo "TODO"))

            ;; 5. VERIFY project does NOT appear as stuck (it has a NEXT task in other file)
            (org-gtd-review-stuck-projects)
            (expect (agenda-raw-text)
                    :not :to-match "Multi-file project review")))))

  (describe "Review of incubated items"
    (it "verifies incubated item appears in area of focus review"
        ;; Create future date (7 days ahead)
        (let* ((future-date-time (time-add (current-time) (days-to-time 7)))
               (future-date (decode-time future-date-time))
               (future-calendar-date (list (nth 4 future-date)  ; month
                                          (nth 3 future-date)  ; day
                                          (nth 5 future-date)))  ; year
               (org-gtd-areas-of-focus '("Personal" "Work" "Health")))

          ;; 1. CAPTURE and ORGANIZE incubated item with future date
          (capture-inbox-item "Learn Italian")
          (org-gtd-process-inbox)
          (defer-item future-calendar-date)

          ;; 2. Set area of focus (uses CATEGORY property, not AREA_OF_FOCUS)
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (re-search-forward "Learn Italian")
            (org-set-property "CATEGORY" "Personal"))

          ;; 3. VERIFY appears in area of focus review
          (org-gtd-review-area-of-focus "Personal")
          (expect (agenda-raw-text)
                  :to-match "Learn Italian")))

    (it "verifies incubated item can be archived after completion"
        ;; 1. CAPTURE and ORGANIZE incubated item
        (capture-inbox-item "Research vacation spots")
        (org-gtd-process-inbox)
        (defer-item (calendar-current-date))

        ;; 2. MARK DONE
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Research vacation spots")
          (org-todo "DONE"))

        ;; 3. ARCHIVE
        (org-gtd-archive-completed-items)

        ;; 4. VERIFY archived
        (with-current-buffer (org-gtd--default-file)
          (expect (current-buffer-raw-text)
                  :not :to-match "Research vacation spots")))))

(describe "Habit Flow Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Habit workflow"
    (it "captures, organizes as habit, shows in engage, completes without archiving"
        ;; 1. CAPTURE
        (capture-inbox-item "Exercise daily")

        ;; 2. PROCESS
        (org-gtd-process-inbox)

        ;; 3. ORGANIZE as habit with daily repeater
        (organize-as-habit "+1d")

        ;; 4. VERIFY shows in engage
        (org-gtd-engage)
        (expect (agenda-raw-text)
                :to-match "Exercise daily")

        ;; 5. Mark habit DONE
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Exercise daily")
          (org-todo "DONE"))

        ;; 6. VERIFY habit is NOT archived (it reschedules instead)
        (org-gtd-archive-completed-items)
        (with-current-buffer (org-gtd--default-file)
          ;; Habit should still be in the file (not archived)
          (expect (current-buffer-raw-text)
                  :to-match "Exercise daily"))

        ;; 7. VERIFY habit still shows in engage at new date
        ;; Note: The habit reschedules to tomorrow, so it might not show in today's agenda
        ;; depending on the agenda view configuration. We verify it exists in the file.
        (org-gtd-engage)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Exercise daily")
          ;; Verify it has a SCHEDULED timestamp (proof it rescheduled)
          (expect (org-entry-get (point) "SCHEDULED")
                  :not :to-be nil)))

    (it "cancels habit, verifies it doesn't show in engage"
        ;; NOTE: This test documents a product limitation with habits.
        ;; When you mark a habit CNCL, org-mode removes the TODO keyword and logs
        ;; it in LAST_REPEAT, similar to when marking it DONE. This means:
        ;; 1. The habit doesn't actually have CNCL state (state is nil)
        ;; 2. The habit won't be archived by org-gtd-archive-completed-items
        ;; 3. Users need to manually delete or archive unwanted habits
        ;; This is org-mode behavior, not specific to org-gtd.

        ;; 1. CAPTURE and ORGANIZE habit
        (capture-inbox-item "Read before bed")
        (org-gtd-process-inbox)
        (organize-as-habit "+1d")

        ;; 2. VERIFY shows in engage before cancellation
        (org-gtd-engage)
        (expect (agenda-raw-text)
                :to-match "Read before bed")

        ;; 3. CANCEL the habit
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Read before bed")
          (org-todo "CNCL"))

        ;; 4. VERIFY canceled habit does NOT show in engage
        ;; (Good! Habits behave correctly - canceled ones don't show)
        (org-gtd-engage)
        (expect (agenda-raw-text)
                :not :to-match "Read before bed")

        ;; 5. VERIFY habit state is nil (org-mode limitation)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (re-search-forward "Read before bed")
          (expect (org-get-todo-state) :to-be nil))

        ;; 6. VERIFY habit is NOT archived (because it has no done keyword)
        (org-gtd-archive-completed-items)
        (with-current-buffer (org-gtd--default-file)
          ;; Habit remains in file (not archived due to org-mode behavior)
          (expect (current-buffer-raw-text)
                  :to-match "Read before bed")))))
(describe "Multi-file DAG Tests"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Project with tasks across multiple files"
    (it "organizes project, engages, completes tasks, and archives across files"
        ;; NOTE: This test documents a product limitation - org-gtd-archive-completed-items
        ;; currently only archives items from the main GTD files, NOT from secondary
        ;; org-agenda-files. Tasks in secondary files remain even after being DONE.
        ;; This is a known limitation that should be fixed in the future.

        ;; 1. CAPTURE and ORGANIZE project in main file
        (capture-inbox-item "Multi-file project")
        (org-gtd-process-inbox)

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task in main file" :level 2)
              (organize-as-project))))

        ;; 2. Create second file with related task
        (let ((second-file (org-gtd--path "secondary-tasks")))
          (with-temp-file second-file
            (make-task "Task in second file" :id "task-second-id" :level 1))

          ;; Ensure ID is registered
          (with-current-buffer (find-file-noselect second-file)
            (org-mode)
            (goto-char (point-min))
            (search-forward "Task in second file")
            (org-back-to-heading t)
            (org-id-add-location "task-second-id" second-file)
            (org-todo "NEXT"))

          ;; Add second file to org-agenda-files
          (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

            ;; 3. Link the task in second file to the project
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Multi-file project")
              (org-back-to-heading t)
              (let ((project-id (org-id-get-create)))
                ;; Add second file task to project's first tasks
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "task-second-id")))

            ;; 4. VERIFY both tasks show in engage
            (org-gtd-engage)
            (let ((agenda-content (agenda-raw-text)))
              ;; Multi-file projects may not show project heading, just task names
              (expect agenda-content :to-match "Task in main file")
              (expect agenda-content :to-match "Task in second file"))

            ;; 5. COMPLETE both tasks
            (with-current-buffer (org-gtd--default-file)
              (goto-char (point-min))
              (search-forward "Task in main file")
              (org-todo "DONE"))

            (with-current-buffer (find-file-noselect second-file)
              (goto-char (point-min))
              (search-forward "Task in second file")
              (org-todo "DONE"))

            ;; 6. ARCHIVE - project should archive
            (org-gtd-archive-completed-items)

            ;; 7. VERIFY project is archived from main file
            (with-current-buffer (org-gtd--default-file)
              (expect (current-buffer-raw-text)
                      :not :to-match "Multi-file project"))

            ;; 8. VERIFY task in second file - LIMITATION: NOT archived
            ;; Due to current product limitation, tasks in secondary files are NOT archived
            (with-current-buffer (find-file-noselect second-file)
              (expect (current-buffer-raw-text)
                      :not :to-match "Task in second file")))))))
(describe "Project cancellation with multi-file DAG"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "cancels tasks in different files and verifies archiving works"
      ;; 1. CAPTURE and ORGANIZE project
      (capture-inbox-item "Distributed project")
      (org-gtd-process-inbox)

      (let ((wip-buffers (seq-filter (lambda (buf)
                                       (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                     (buffer-list))))
        (when wip-buffers
          (with-current-buffer (car wip-buffers)
            (goto-char (point-max))
            (newline)
            (make-task "Local task one" :level 2)
            (make-task "Local task two" :level 2)
            (organize-as-project))))

      ;; 2. Create second file with task
      (let ((second-file (org-gtd--path "other-tasks")))
        (with-temp-file second-file
          (make-task "Remote task" :id "remote-task-id" :level 1))

        (with-current-buffer (find-file-noselect second-file)
          (org-mode)
          (goto-char (point-min))
          (search-forward "Remote task")
          (org-back-to-heading t)
          (org-id-add-location "remote-task-id" second-file)
          (org-todo "NEXT"))

        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

          ;; 3. Link remote task to project
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Distributed project")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "remote-task-id"))

          ;; 4. CANCEL one local task and the remote task
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Local task one")
            (org-todo "CNCL"))

          (with-current-buffer (find-file-noselect second-file)
            (goto-char (point-min))
            (search-forward "Remote task")
            (org-todo "CNCL"))

          ;; 5. COMPLETE remaining local task
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Local task two")
            (org-todo "DONE"))

          ;; 6. ARCHIVE - project should archive with mix of CNCL and DONE
          (org-gtd-archive-completed-items)

          ;; 7. VERIFY project is archived
          (with-current-buffer (org-gtd--default-file)
            (expect (current-buffer-raw-text)
                    :not :to-match "Distributed project"))

          ;; 8. VERIFY remote canceled task is archived
          (with-current-buffer (find-file-noselect second-file)
            (expect (current-buffer-raw-text)
                    :not :to-match "Remote task"))))))
(describe "Multi-file project extension"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "adds task to completed project where original tasks are in different files"
      ;; NOTE: This test verifies that after archiving a project with multi-file tasks,
      ;; we can create a NEW project and link tasks from multiple files to it.

      ;; 1. CAPTURE and ORGANIZE initial project
      (capture-inbox-item "Expandable project")
      (org-gtd-process-inbox)

      (let ((wip-buffers (seq-filter (lambda (buf)
                                       (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                     (buffer-list))))
        (when wip-buffers
          (with-current-buffer (car wip-buffers)
            (goto-char (point-max))
            (newline)
            (make-task "Initial task" :level 2)
            (organize-as-project))))

      ;; 2. Create second file with another task
      (let ((second-file (org-gtd--path "additional-tasks")))
        (with-temp-file second-file
          (make-task "External task" :id "external-task-id" :level 1))

        (with-current-buffer (find-file-noselect second-file)
          (org-mode)
          (goto-char (point-min))
          (search-forward "External task")
          (org-back-to-heading t)
          (org-id-add-location "external-task-id" second-file)
          (org-todo "NEXT"))

        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

          ;; 3. Link external task to project
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Expandable project")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "external-task-id"))

          ;; 4. COMPLETE both tasks
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Initial task")
            (org-todo "DONE"))

          (with-current-buffer (find-file-noselect second-file)
            (goto-char (point-min))
            (search-forward "External task")
            (org-todo "DONE"))

          ;; 5. ARCHIVE - project should be complete and archived
          (org-gtd-archive-completed-items)

          (with-current-buffer (org-gtd--default-file)
            (expect (current-buffer-raw-text)
                    :not :to-match "Expandable project"))

          ;; 6. Now create a NEW project that links tasks from multiple files
          ;; First, create a new task in a third file
          (let ((third-file (org-gtd--path "new-extension-tasks")))
            (with-temp-file third-file
              (make-task "New extension task" :id "extension-task-id" :level 1))

            (with-current-buffer (find-file-noselect third-file)
              (org-mode)
              (goto-char (point-min))
              (search-forward "New extension task")
              (org-back-to-heading t)
              (org-id-add-location "extension-task-id" third-file)
              (org-todo "NEXT"))

            ;; Extended org-agenda-files to include third file
            (let ((org-agenda-files (append org-agenda-files (list third-file))))

              ;; 7. Create new project with task in main file
              (capture-inbox-item "New multi-file project")
              (org-gtd-process-inbox)

              (let ((wip-buffers (seq-filter (lambda (buf)
                                               (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                             (buffer-list))))
                (when wip-buffers
                  (with-current-buffer (car wip-buffers)
                    (goto-char (point-max))
                    (newline)
                    (make-task "Another new task" :level 2)
                    (organize-as-project))))

              ;; 8. Link the extension task from third file
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "New multi-file project")
                (org-back-to-heading t)
                (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" "extension-task-id"))

              ;; 9. VERIFY new project shows in engage with tasks from two files
              (org-gtd-engage)
              (let ((agenda-content (agenda-raw-text)))
                ;; Multi-file projects may not show project heading, just task names
                (expect agenda-content :to-match "Another new task")
                (expect agenda-content :to-match "New extension task"))

              ;; 10. COMPLETE the new tasks
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-min))
                (search-forward "Another new task")
                (org-todo "DONE"))

              (with-current-buffer (find-file-noselect third-file)
                (goto-char (point-min))
                (search-forward "New extension task")
                (org-todo "DONE"))

              ;; 11. ARCHIVE - new project should archive
              (org-gtd-archive-completed-items)

              (with-current-buffer (org-gtd--default-file)
                (expect (current-buffer-raw-text)
                        :not :to-match "New multi-file project"))

              (with-current-buffer (find-file-noselect third-file)
                (expect (current-buffer-raw-text)
                        :not :to-match "New extension task"))))))))

(describe "Advanced Project Task Operations"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "Add blocker relationship"
    (it "creates dependency between existing tasks"
        (capture-inbox-item "Website redesign")
        (org-gtd-process-inbox)
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Design wireframes" :level 2)
              (make-task "Get client approval" :level 2)
              (make-task "Build prototype" :level 2)
              (organize-as-project))))
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
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Write documentation" :level 2)
              (make-task "Record demo video" :level 2)
              (organize-as-project))))
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
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Create content calendar" :level 2)
              (organize-as-project))))
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
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Design database schema" :level 2)
              (organize-as-project))))
        (capture-inbox-item "Project Beta")
        (org-gtd-process-inbox)
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Implement API" :level 2)
              (organize-as-project))))
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
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Shared Task" :level 2)
              (organize-as-project))))

        (capture-inbox-item "Project Beta")
        (org-gtd-process-inbox)
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Another Task" :level 2)
              (organize-as-project))))

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
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Book conference room" :level 2)
              (make-task "Send speaker invites" :level 2)
              (make-task "Order catering" :level 2)
              (organize-as-project))))

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
        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Research market" :level 2)
              (make-task "Build prototype" :level 2)
              (make-task "Pitch to investors" :level 2)
              (organize-as-project))))

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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Main file task" :level 2)
              (organize-as-project))))

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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Alpha task 1" :level 2)
              (make-task "Shared task" :level 2)
              (organize-as-project))))

        ;; 2. CAPTURE and ORGANIZE second project
        (capture-inbox-item "Project Beta")
        (org-gtd-process-inbox)

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Beta task 1" :level 2)
              (organize-as-project))))

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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Gamma unique task" :level 2)
              (make-task "Shared infrastructure task" :level 2)
              (organize-as-project))))

        ;; 2. CAPTURE and ORGANIZE second project
        (capture-inbox-item "Project Delta")
        (org-gtd-process-inbox)

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Delta unique task" :level 2)
              (organize-as-project))))

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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Research target audience" :level 2)
              (make-task "Create content" :level 2)
              (make-task "Launch campaign" :level 2)
              (organize-as-project))))

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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task in main file" :level 2)
              (organize-as-project))))

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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Main file task" :level 2)
              (organize-as-project))))

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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task A" :level 2)
              (make-task "Task C" :level 2)
              (organize-as-project))))

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
            (let ((wip-buffers (seq-filter (lambda (buf)
                                             (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                           (buffer-list))))
              (when wip-buffers
                (with-current-buffer (car wip-buffers)
                  (goto-char (point-min))
                  (search-forward "Task B")
                  (org-back-to-heading t)
                  (spy-on 'org-refile-get-location
                          :and-return-value (list "Project Alpha"
                                                  (buffer-file-name (marker-buffer project-point))
                                                  nil
                                                  (marker-position project-point)))
                  (org-gtd-project-extend))))))

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
          ;; NOTE: org-edna should automatically update TODO states when dependencies are satisfied
          ;; TODO: This test will pass once org-edna supports ORG_GTD_BLOCKS/ORG_GTD_DEPENDS_ON properties
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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task A" :level 2)
              (make-task "Task B" :level 2)
              (organize-as-project))))

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
            (let ((wip-buffers (seq-filter (lambda (buf)
                                             (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                           (buffer-list))))
              (when wip-buffers
                (with-current-buffer (car wip-buffers)
                  (goto-char (point-min))
                  (search-forward "Task C")
                  (org-back-to-heading t)
                  (spy-on 'org-refile-get-location
                          :and-return-value (list "Project Beta"
                                                  (buffer-file-name (marker-buffer project-point))
                                                  nil
                                                  (marker-position project-point)))
                  (org-gtd-project-extend))))))

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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task A" :level 2)
              (organize-as-project))))

        ;; 2. Add Task B using project-extend
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Gamma")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))

            (let ((wip-buffers (seq-filter (lambda (buf)
                                             (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                           (buffer-list))))
              (when wip-buffers
                (with-current-buffer (car wip-buffers)
                  (goto-char (point-min))
                  (search-forward "Task B")
                  (org-back-to-heading t)
                  (spy-on 'org-refile-get-location
                          :and-return-value (list "Project Gamma"
                                                  (buffer-file-name (marker-buffer project-point))
                                                  nil
                                                  (marker-position project-point)))
                  (org-gtd-project-extend))))))

        ;; 3. Add Task C using project-extend
        (capture-inbox-item "Task C")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Gamma")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))

            (let ((wip-buffers (seq-filter (lambda (buf)
                                             (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                           (buffer-list))))
              (when wip-buffers
                (with-current-buffer (car wip-buffers)
                  (goto-char (point-min))
                  (search-forward "Task C")
                  (org-back-to-heading t)
                  (spy-on 'org-refile-get-location
                          :and-return-value (list "Project Gamma"
                                                  (buffer-file-name (marker-buffer project-point))
                                                  nil
                                                  (marker-position project-point)))
                  (org-gtd-project-extend))))))

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
          ;; NOTE: Automatic TODO state updates require org-edna integration with ORG_GTD_ properties
          ;; NOTE: org-edna should automatically update TODO states when dependencies are satisfied
          ;; TODO: This test will pass once org-edna supports ORG_GTD_BLOCKS/ORG_GTD_DEPENDS_ON properties
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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task A" :level 2)
              (organize-as-project))))

        ;; 2. Add Task B using project-extend (same file)
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Delta")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))

            (let ((wip-buffers (seq-filter (lambda (buf)
                                             (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                           (buffer-list))))
              (when wip-buffers
                (with-current-buffer (car wip-buffers)
                  (goto-char (point-min))
                  (search-forward "Task B")
                  (org-back-to-heading t)
                  (spy-on 'org-refile-get-location
                          :and-return-value (list "Project Delta"
                                                  (buffer-file-name (marker-buffer project-point))
                                                  nil
                                                  (marker-position project-point)))
                  (org-gtd-project-extend))))))

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
              ;; NOTE: org-edna should automatically update TODO states when dependencies are satisfied
              ;; TODO: This test will pass once org-edna supports ORG_GTD_BLOCKS/ORG_GTD_DEPENDS_ON properties
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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task A" :level 2)
              (organize-as-project))))

        ;; 2. Add Task B using project-extend
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Epsilon")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))

            (let ((wip-buffers (seq-filter (lambda (buf)
                                             (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                           (buffer-list))))
              (when wip-buffers
                (with-current-buffer (car wip-buffers)
                  (goto-char (point-min))
                  (search-forward "Task B")
                  (org-back-to-heading t)
                  (spy-on 'org-refile-get-location
                          :and-return-value (list "Project Epsilon"
                                                  (buffer-file-name (marker-buffer project-point))
                                                  nil
                                                  (marker-position project-point)))
                  (org-gtd-project-extend))))))

        ;; 3. Add Task C using project-extend
        (capture-inbox-item "Task C")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Epsilon")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))

            (let ((wip-buffers (seq-filter (lambda (buf)
                                             (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                           (buffer-list))))
              (when wip-buffers
                (with-current-buffer (car wip-buffers)
                  (goto-char (point-min))
                  (search-forward "Task C")
                  (org-back-to-heading t)
                  (spy-on 'org-refile-get-location
                          :and-return-value (list "Project Epsilon"
                                                  (buffer-file-name (marker-buffer project-point))
                                                  nil
                                                  (marker-position project-point)))
                  (org-gtd-project-extend))))))

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
          ;; NOTE: org-edna should automatically update TODO states when dependencies are satisfied
          ;; TODO: This test will pass once org-edna supports ORG_GTD_BLOCKS/ORG_GTD_DEPENDS_ON properties
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

        (let ((wip-buffers (seq-filter (lambda (buf)
                                         (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                       (buffer-list))))
          (when wip-buffers
            (with-current-buffer (car wip-buffers)
              (goto-char (point-max))
              (newline)
              (make-task "Task A" :level 2)
              (organize-as-project))))

        ;; 2. Add Task B using project-extend (same file)
        (capture-inbox-item "Task B")
        (org-gtd-process-inbox)

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Project Zeta")
          (org-back-to-heading t)
          (let ((project-point (point-marker)))

            (let ((wip-buffers (seq-filter (lambda (buf)
                                             (string-match-p org-gtd-wip--prefix (buffer-name buf)))
                                           (buffer-list))))
              (when wip-buffers
                (with-current-buffer (car wip-buffers)
                  (goto-char (point-min))
                  (search-forward "Task B")
                  (org-back-to-heading t)
                  (spy-on 'org-refile-get-location
                          :and-return-value (list "Project Zeta"
                                                  (buffer-file-name (marker-buffer project-point))
                                                  nil
                                                  (marker-position project-point)))
                  (org-gtd-project-extend))))))

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
              ;; NOTE: org-edna should automatically update TODO states when dependencies are satisfied
              ;; TODO: This test will pass once org-edna supports ORG_GTD_BLOCKS/ORG_GTD_DEPENDS_ON properties
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

;;; end-to-end-test.el ends here
