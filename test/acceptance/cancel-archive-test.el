;;; cancel-archive-test.el --- Acceptance tests for cancellation and archiving -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Acceptance tests for canceling items (CNCL) and verifying they:
;; - Don't appear in engage views
;; - Can be properly archived
;;
;; Migrated from test/end-to-end-test.el (buttercup) to e-unit with mock-fs.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Single Action Cancellation

(deftest single-action-cancellation ()
  "Captures, organizes as single action, marks CNCL, verifies doesn't show in engage, then archives."
  ;; 1. CAPTURE
  (capture-inbox-item "Buy concert tickets")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (single action)
  (organize-as-single-action)

  ;; 4. VERIFY in agenda before cancellation
  (org-gtd-engage)
  (assert-match "Buy concert tickets" (agenda-raw-text))

  ;; 5. CANCEL the action
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Buy concert tickets")
    (org-todo "CNCL"))

  ;; 6. VERIFY doesn't show in engage after cancellation
  (org-gtd-engage)
  (refute-match "Buy concert tickets" (agenda-raw-text))

  ;; 7. ARCHIVE
  (org-gtd-archive-completed-items)

  ;; 8. Verify item is NOT in main file after archive
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Buy concert tickets" (current-buffer-raw-text)))

  ;; 9. Verify item IS in archive file
  (assert-match "Buy concert tickets" (archive-raw-text)))

;;; Calendar Item Cancellation

(deftest calendar-item-cancellation ()
  "Captures, organizes as calendar, marks CNCL, and archives."
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
  (assert-match "Dentist appointment" (agenda-raw-text))

  ;; 5. CANCEL the appointment
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Dentist appointment")
    (org-todo "CNCL"))

  ;; 6. VERIFY does NOT show in engage after cancellation
  (org-gtd-engage)
  (refute-match "Dentist appointment" (agenda-raw-text))

  ;; 7. ARCHIVE - canceled items CAN be archived
  (org-gtd-archive-completed-items)

  ;; 8. Verify item is NOT in main file after archive
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Dentist appointment" (current-buffer-raw-text)))

  ;; 9. Verify item IS in archive file
  (assert-match "Dentist appointment" (archive-raw-text)))

;;; Delegated Item Cancellation

(deftest delegated-item-cancellation ()
  "Captures, organizes as delegated, marks CNCL, and archives."
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
  (assert-match "Get feedback from Sarah" (agenda-raw-text))

  ;; 5. CANCEL the delegation
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Get feedback from Sarah")
    (org-todo "CNCL"))

  ;; 6. VERIFY does NOT show in engage after cancellation
  (org-gtd-engage)
  (refute-match "Get feedback from Sarah" (agenda-raw-text))

  ;; 7. ARCHIVE - canceled items CAN be archived
  (org-gtd-archive-completed-items)

  ;; 8. Verify item is NOT in main file after archive
  (with-current-buffer (org-gtd--default-file)
    (refute-match "Get feedback from Sarah" (current-buffer-raw-text)))

  ;; 9. Verify item IS in archive file
  (assert-match "Get feedback from Sarah" (archive-raw-text)))

;;; Project Task Cancellation

(deftest project-task-cancellation ()
  "Creates project, cancels one task, verifies project still works, completes remaining tasks, then archives."
  ;; 1. CAPTURE
  (capture-inbox-item "Organize workshop")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (project with tasks)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Book venue" :level 2)
    (make-task "Prepare materials" :level 2)
    (make-task "Send invitations" :level 2)
    (organize-as-project))

  ;; 4. VERIFY all tasks in agenda
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "Organize wo" agenda-content)  ; Truncated
    (assert-match "Book venue" agenda-content))

  ;; 5. CANCEL one task (Prepare materials)
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (re-search-forward "Prepare materials")
    (org-todo "CNCL"))

  ;; 6. VERIFY project still works - canceled task doesn't show, but project continues
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    ;; Project heading should still show (has incomplete tasks)
    (assert-match "Organize wo" agenda-content)
    ;; Canceled task should not show
    (refute-match "Prepare materials" agenda-content)
    ;; Other tasks should still show
    (assert-match "Book venue" agenda-content))

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
    (refute-match "Organize workshop" (current-buffer-raw-text))))

;;; Project with All Tasks Canceled

(deftest project-all-tasks-canceled ()
  "Creates project, cancels all tasks, verifies nothing shows in engage, then archives."
  ;; This test verifies the critical requirement: Canceled items MUST NOT appear in engage.
  ;; It also verifies that projects with all tasks canceled can be properly archived.

  ;; 1. CAPTURE
  (capture-inbox-item "Build garden shed")

  ;; 2. PROCESS
  (org-gtd-process-inbox)

  ;; 3. ORGANIZE (project with multiple tasks in same file)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (make-task "Buy lumber" :level 2)
    (make-task "Purchase tools" :level 2)
    (make-task "Build foundation" :level 2)
    (organize-as-project))

  ;; 4. VERIFY all tasks appear in engage before cancellation
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "Build garde" agenda-content)  ; Project heading (truncated)
    (assert-match "Buy lumber" agenda-content))

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
    (refute-match "Build garde" agenda-content)
    (refute-match "Buy lumber" agenda-content)
    (refute-match "Purchase tools" agenda-content)
    (refute-match "Build foundation" agenda-content))

  ;; 7. ARCHIVE - project with all canceled tasks should be archivable
  (org-gtd-archive-completed-items)

  ;; 8. VERIFY project and all tasks are NOT in main file after archive
  (with-current-buffer (org-gtd--default-file)
    (let ((content (current-buffer-raw-text)))
      (refute-match "Build garden shed" content)
      (refute-match "Buy lumber" content)
      (refute-match "Purchase tools" content)
      (refute-match "Build foundation" content)))

  ;; 9. VERIFY project and tasks ARE in archive file
  (let ((archived-content (archive-raw-text)))
    (assert-match "Build garden shed" archived-content)
    (assert-match "Buy lumber" archived-content)
    (assert-match "Purchase tools" archived-content)
    (assert-match "Build foundation" archived-content)))

;;; Selective Archiving Tests

(deftest archives-completed-single-action-preserving-active ()
  "Verifies completed single action is archived while active one is preserved."
  ;; 1. CREATE two single actions
  (create-single-action "action one")
  (create-single-action "action two")

  ;; 2. VERIFY both show in engage view
  (org-gtd-engage)
  (let ((agenda-content (agenda-raw-text)))
    (assert-match "action one" agenda-content)
    (assert-match "action two" agenda-content))

  ;; 3. COMPLETE first action
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "action one")
    (org-todo "DONE"))

  ;; 4. ARCHIVE completed items
  (org-gtd-archive-completed-items)

  ;; 5. VERIFY active action is still in main file
  (with-current-buffer (org-gtd--default-file)
    (assert-match "action two" (current-buffer-raw-text)))

  ;; 6. VERIFY completed action is NOT in main file
  (with-current-buffer (org-gtd--default-file)
    (refute-match "DONE action one" (current-buffer-raw-text)))

  ;; 7. VERIFY completed action IS in archive
  (assert-match "action one" (archive-raw-text)))

;;; cancel-archive-test.el ends here
