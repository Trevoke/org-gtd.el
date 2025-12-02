;;; dag-operations-test.el --- Integration tests for DAG task operations -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for manual DAG task operations: inserting tasks
;; between existing chains, adding leaf tasks, etc.
;;
;; Migrated from test/end-to-end-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Helper for mocking org-refile-get-location

(defmacro with-mock-refile-location (project-name buffer pos &rest body)
  "Execute BODY with `org-refile-get-location' returning mock values.
Returns (PROJECT-NAME BUFFER-FILE-NAME nil POS)."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'org-refile-get-location)
              (lambda (&rest _)
                (list ,project-name
                      (buffer-file-name ,buffer)
                      nil
                      ,pos))))
     ,@body))

;;; Test 1: Insert task between existing A→C chain

(deftest dag-ops/insert-task-between-chain ()
  "Inserts task B between existing A→C chain to create A→B→C.
Manual workflow: Add task B, make A block B, make B block C, remove A→C link."
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
    (assert-match "Task A" agenda-content)
    (assert-nil (string-match-p "Task C" agenda-content)))

  ;; 4. Add task B using org-gtd-project-extend
  (capture-inbox-item "Task B")
  (org-gtd-process-inbox)

  ;; Mock the project selection in org-gtd-project-extend
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project Alpha")
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create))
          (project-buf (current-buffer))
          (project-pos (point)))

      ;; Simulate org-gtd-project-extend by refiling Task B under Project Alpha
      (with-wip-buffer
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (with-mock-refile-location "Project Alpha" project-buf project-pos
          (org-gtd-project-extend)))))

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

      ;; 6. On task B, add A as blocker
      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)

      (goto-char (point-min))
      (search-forward "Task A")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)

      ;; 7. On task B, add C as dependent
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

      ;; Also remove C from A's blocks list
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

    ;; 8. VERIFY: A→B→C chain established
    (with-current-buffer (org-gtd--default-file)
      ;; Task A blocks B
      (goto-char (point-min))
      (search-forward "Task A")
      (org-back-to-heading t)
      (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

      ;; Task B depends on A and blocks C
      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal (list task-a-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
      (assert-equal (list task-c-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

      ;; Task C depends on B (not A)
      (goto-char (point-min))
      (search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

    ;; 9. VERIFY agenda: Only A is NEXT
    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-match "Task A" agenda-content)
      (assert-nil (string-match-p "Task B" agenda-content))
      (assert-nil (string-match-p "Task C" agenda-content)))

    ;; 10. VERIFY workflow: Complete A, then B becomes NEXT
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE"))

    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-nil (string-match-p "Task A" agenda-content))
      (assert-match "Task B" agenda-content)
      (assert-nil (string-match-p "Task C" agenda-content)))))

;;; Test 2: Add task as leaf at end of chain

(deftest dag-ops/add-leaf-task-to-chain ()
  "Adds task C as leaf at end of A→B dependency chain.
Manual workflow: Add task C, make B block C."
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
    (let ((project-buf (current-buffer))
          (project-pos (point)))

      ;; Refile Task C to project
      (with-wip-buffer
        (goto-char (point-min))
        (search-forward "Task C")
        (org-back-to-heading t)
        (with-mock-refile-location "Project Beta" project-buf project-pos
          (org-gtd-project-extend)))))

  ;; 4. Add B as blocker of C
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

      ;; Add B as blocker of C using the actual function with mocked selection
      (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                 (lambda (&rest _) (list task-b-id))))
        (org-gtd-task-add-blockers)))

    ;; 5. VERIFY A→B→C chain
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task C")
      (org-back-to-heading t)
      (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal (list task-c-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))

    ;; 6. VERIFY only A is NEXT
    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-match "Task A" agenda-content)
      (assert-nil (string-match-p "Task B" agenda-content))
      (assert-nil (string-match-p "Task C" agenda-content)))))

;;; Test 3: Parallel children of A in same file

(deftest dag-ops/parallel-children-same-file ()
  "Adds parallel tasks B and C as children of A in same file.
After completing A, both B and C become NEXT (parallel execution)."
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
    (let ((project-buf (current-buffer))
          (project-pos (point)))
      (with-wip-buffer
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (with-mock-refile-location "Project Gamma" project-buf project-pos
          (org-gtd-project-extend)))))

  ;; 3. Add Task C using project-extend
  (capture-inbox-item "Task C")
  (org-gtd-process-inbox)

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project Gamma")
    (org-back-to-heading t)
    (let ((project-buf (current-buffer))
          (project-pos (point)))
      (with-wip-buffer
        (goto-char (point-min))
        (search-forward "Task C")
        (org-back-to-heading t)
        (with-mock-refile-location "Project Gamma" project-buf project-pos
          (org-gtd-project-extend)))))

  ;; 4. Make A parent of both B and C
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
        (assert-equal 2 (length blocks-list))))

    ;; 6. VERIFY agenda: Only A is NEXT (blocks B and C)
    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-match "Task A" agenda-content)
      (assert-nil (string-match-p "Task B" agenda-content))
      (assert-nil (string-match-p "Task C" agenda-content)))

    ;; 7. VERIFY workflow: Complete A, then B and C both become NEXT (parallel)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE"))

    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-nil (string-match-p "Task A" agenda-content))
      (assert-match "Task B" agenda-content)
      (assert-match "Task C" agenda-content))))

;;; Test 5: Sequential chain A→B→C in same file

(deftest dag-ops/sequential-chain-same-file ()
  "Adds sequential tasks B and C chained after A in same file.
Creates A→B→C chain where only A is NEXT initially."
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
    (let ((project-buf (current-buffer))
          (project-pos (point)))
      (with-wip-buffer
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (with-mock-refile-location "Project Epsilon" project-buf project-pos
          (org-gtd-project-extend)))))

  ;; 3. Add Task C using project-extend
  (capture-inbox-item "Task C")
  (org-gtd-process-inbox)

  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-min))
    (search-forward "Project Epsilon")
    (org-back-to-heading t)
    (let ((project-buf (current-buffer))
          (project-pos (point)))
      (with-wip-buffer
        (goto-char (point-min))
        (search-forward "Task C")
        (org-back-to-heading t)
        (with-mock-refile-location "Project Epsilon" project-buf project-pos
          (org-gtd-project-extend)))))

  ;; 4. Create A→B→C chain
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
      (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                 (lambda (&rest _) (list task-a-id))))
        (org-gtd-task-add-blockers))

      ;; Add B as blocker of C
      (goto-char (point-min))
      (search-forward "Task C")
      (org-back-to-heading t)
      (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                 (lambda (&rest _) (list task-b-id))))
        (org-gtd-task-add-blockers)))

    ;; 5. VERIFY sequential chain
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task A")
      (org-back-to-heading t)
      (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (assert-equal (list task-a-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

    ;; 6. VERIFY only A is NEXT
    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-match "Task A" agenda-content)
      (assert-nil (string-match-p "Task B" agenda-content))
      (assert-nil (string-match-p "Task C" agenda-content)))

    ;; 7. Complete A, verify B becomes NEXT
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task A")
      (org-back-to-heading t)
      (org-todo "DONE"))

    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-nil (string-match-p "Task A" agenda-content))
      (assert-match "Task B" agenda-content)
      (assert-nil (string-match-p "Task C" agenda-content)))

    ;; 8. Complete B, verify C becomes NEXT
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (org-todo "DONE"))

    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-nil (string-match-p "Task A" agenda-content))
      (assert-nil (string-match-p "Task B" agenda-content))
      (assert-match "Task C" agenda-content))))

;;; Test 4: Parallel children across multiple files

(deftest dag-ops/parallel-children-multi-file ()
  "Adds parallel tasks B and C as children of A across multiple files.
Task B in main file, Task C in secondary file, both depend on A."
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
    (let ((project-buf (current-buffer))
          (project-pos (point)))
      (with-wip-buffer
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (with-mock-refile-location "Project Delta" project-buf project-pos
          (org-gtd-project-extend)))))

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

      ;; 5. Make A parent of both B and C
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
            (assert-equal 2 (length blocks-list))))

        ;; 7. VERIFY agenda: Only A is NEXT (blocks B and C)
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (assert-match "Task A" agenda-content)
          (assert-nil (string-match-p "Task B" agenda-content))
          (assert-nil (string-match-p "Task C" agenda-content)))

        ;; 8. Complete A, both B and C become NEXT (parallel across files)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task A")
          (org-back-to-heading t)
          (org-todo "DONE"))

        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (assert-nil (string-match-p "Task A" agenda-content))
          (assert-match "Task B" agenda-content)
          (assert-match "Task C" agenda-content))))))

;;; Test 6: Sequential chain across multiple files

(deftest dag-ops/sequential-chain-multi-file ()
  "Adds sequential tasks B and C chained after A across multiple files.
Task B in main file, Task C in secondary file, chain A→B→C."
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
    (let ((project-buf (current-buffer))
          (project-pos (point)))
      (with-wip-buffer
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (with-mock-refile-location "Project Zeta" project-buf project-pos
          (org-gtd-project-extend)))))

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

      ;; 5. Create A→B→C chain
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
          (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                     (lambda (&rest _) (list task-a-id))))
            (org-gtd-task-add-blockers))

          ;; Add B as blocker of C (in other file)
          (with-current-buffer (find-file-noselect second-file)
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (cl-letf (((symbol-function 'org-gtd-task-management--select-multiple-task-ids)
                       (lambda (&rest _) (list task-b-id))))
              (org-gtd-task-add-blockers))))

        ;; 6. VERIFY sequential chain across files
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task A")
          (org-back-to-heading t)
          (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

          (goto-char (point-min))
          (search-forward "Task B")
          (org-back-to-heading t)
          (assert-equal (list task-a-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

        (with-current-buffer (find-file-noselect second-file)
          (goto-char (point-min))
          (search-forward "Task C")
          (org-back-to-heading t)
          (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

        ;; 7. VERIFY only A is NEXT across files
        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (assert-match "Task A" agenda-content)
          (assert-nil (string-match-p "Task B" agenda-content))
          (assert-nil (string-match-p "Task C" agenda-content)))

        ;; 8. Complete A, verify B becomes NEXT
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task A")
          (org-back-to-heading t)
          (org-todo "DONE"))

        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (assert-nil (string-match-p "Task A" agenda-content))
          (assert-match "Task B" agenda-content)
          (assert-nil (string-match-p "Task C" agenda-content)))

        ;; 9. Complete B, verify C becomes NEXT
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task B")
          (org-back-to-heading t)
          (org-todo "DONE"))

        (org-gtd-engage)
        (let ((agenda-content (agenda-raw-text)))
          (assert-nil (string-match-p "Task A" agenda-content))
          (assert-nil (string-match-p "Task B" agenda-content))
          (assert-match "Task C" agenda-content))))))

(provide 'dag-operations-test)

;;; dag-operations-test.el ends here
