;;; dag-operations-test.el --- Integration tests for DAG task operations -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for manual DAG task operations: inserting tasks
;; between existing chains, adding leaf tasks, etc.
;;
;; These tests use the builder pattern from test/helpers/builders.el
;; to create properly-configured GTD data.

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

;;; Test 1: Insert task between existing A→C chain

(deftest dag-ops/insert-task-between-chain ()
  "Inserts task B between existing A→C chain to create A→B→C.
Uses builders for initial project, then manually modifies dependencies."

  ;; 1. Create project with A→C chain using builder
  (let ((project-data (make-chain-project "Project Alpha"
                                          :tasks '("Task A" "Task C"))))
    ;; Fix TODO keywords to set initial states
    (org-gtd-projects-fix-todo-keywords (alist-get 'project-marker project-data))

    ;; 2. Verify initial state: Only A is NEXT (C depends on A)
    (org-gtd-engage)
    (let ((agenda-content (agenda-raw-text)))
      (assert-match "Task A" agenda-content)
      (assert-nil (string-match-p "Task C" agenda-content)))

    ;; 3. Add task B to the project
    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-c-id (alist-get 'task-2-id project-data))
          (project-id (alist-get 'project-id project-data))
          task-b-id)

      (with-current-buffer (org-gtd--default-file)
        ;; Insert Task B as a child of the project
        (goto-char (point-min))
        (search-forward "Project Alpha")
        (org-back-to-heading t)
        (org-end-of-subtree)
        (newline)
        (setq task-b-id (ogt-builder--generate-id "task"))
        (insert (format "** TODO Task B\n:PROPERTIES:\n:ID: %s\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: %s\n:END:\n"
                        task-b-id project-id))

        ;; 4. Create A→B→C chain: A blocks B, B blocks C
        ;; Remove the existing A→C dependency
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (org-entry-delete (point) "ORG_GTD_BLOCKS")

        (goto-char (point-min))
        (search-forward "Task C")
        (org-back-to-heading t)
        (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")

        ;; Create new chain: A→B→C
        (org-gtd-dependencies-create task-a-id task-b-id)
        (org-gtd-dependencies-create task-b-id task-c-id)

        ;; Update project state
        (goto-char (point-min))
        (search-forward "Project Alpha")
        (org-back-to-heading t)
        (org-gtd-projects--set-first-tasks)
        (org-gtd-projects-fix-todo-keywords (point-marker)))

      ;; 5. VERIFY: A→B→C chain established
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

      ;; 6. VERIFY agenda: Only A is NEXT
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-match "Task A" agenda-content)
        (assert-nil (string-match-p "Task B" agenda-content))
        (assert-nil (string-match-p "Task C" agenda-content)))

      ;; 7. VERIFY workflow: Complete A, then B becomes NEXT
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (org-todo "DONE"))

      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-nil (string-match-p "Task A" agenda-content))
        (assert-match "Task B" agenda-content)
        (assert-nil (string-match-p "Task C" agenda-content))))))

;;; Test 2: Add task as leaf at end of chain

(deftest dag-ops/add-leaf-task-to-chain ()
  "Adds task C as leaf at end of A→B dependency chain.
Uses builders, then extends with new leaf task."

  ;; 1. Create project with A→B chain using builder
  (let ((project-data (make-chain-project "Project Beta"
                                          :tasks '("Task A" "Task B"))))
    ;; Fix TODO keywords to set initial states
    (org-gtd-projects-fix-todo-keywords (alist-get 'project-marker project-data))

    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-b-id (alist-get 'task-2-id project-data))
          (project-id (alist-get 'project-id project-data))
          task-c-id)

      ;; 2. Add Task C to the project
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Project Beta")
        (org-back-to-heading t)
        (org-end-of-subtree)
        (newline)
        (setq task-c-id (ogt-builder--generate-id "task"))
        (insert (format "** TODO Task C\n:PROPERTIES:\n:ID: %s\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: %s\n:END:\n"
                        task-c-id project-id))

        ;; 3. Make B block C
        (org-gtd-dependencies-create task-b-id task-c-id)

        ;; Update project state
        (goto-char (point-min))
        (search-forward "Project Beta")
        (org-back-to-heading t)
        (org-gtd-projects--set-first-tasks)
        (org-gtd-projects-fix-todo-keywords (point-marker)))

      ;; 4. VERIFY A→B→C chain
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task C")
        (org-back-to-heading t)
        (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))

        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal (list task-c-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))

      ;; 5. VERIFY only A is NEXT
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-match "Task A" agenda-content)
        (assert-nil (string-match-p "Task B" agenda-content))
        (assert-nil (string-match-p "Task C" agenda-content))))))

;;; Test 3: Parallel children of A in same file

(deftest dag-ops/parallel-children-same-file ()
  "Adds parallel tasks B and C as children of A in same file.
After completing A, both B and C become NEXT (parallel execution)."

  ;; 1. Create project with A as root, B and C as parallel children
  (let ((project-data (make-diamond-project "Project Gamma"
                                            :root-tasks '("Task A")
                                            :middle-task "Task B"
                                            :leaf-task "Task C")))
    ;; The diamond builder creates A→B→C and A→C structure
    ;; We need to modify to create A→B and A→C (parallel)

    ;; Get the IDs
    (let ((task-a-id (alist-get 'task-a-id project-data))
          (task-b-id (alist-get 'task-c-id project-data))  ;; middle is "B" conceptually
          (task-c-id (alist-get 'task-d-id project-data))) ;; leaf is "C" conceptually

      ;; Fix the diamond to be a simple fan-out: A blocks both B and C
      (with-current-buffer (org-gtd--default-file)
        ;; Remove B→C dependency to make B and C parallel
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (org-entry-delete (point) "ORG_GTD_BLOCKS")

        (goto-char (point-min))
        (search-forward "Task C")
        (org-back-to-heading t)
        ;; C should only depend on A, not B
        (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_DEPENDS_ON" task-a-id)

        ;; Fix A's blocks list
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (org-entry-delete (point) "ORG_GTD_BLOCKS")
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-b-id)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_BLOCKS" task-c-id)

        ;; Update project state
        (goto-char (point-min))
        (search-forward "Project Gamma")
        (org-back-to-heading t)
        (org-gtd-projects--set-first-tasks)
        (org-gtd-projects-fix-todo-keywords (point-marker)))

      ;; 2. VERIFY: A blocks both B and C
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (let ((blocks-list (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          (assert-equal 2 (length blocks-list))))

      ;; 3. VERIFY agenda: Only A is NEXT (blocks B and C)
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-match "Task A" agenda-content)
        (assert-nil (string-match-p "Task B" agenda-content))
        (assert-nil (string-match-p "Task C" agenda-content)))

      ;; 4. VERIFY workflow: Complete A, then B and C both become NEXT (parallel)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (org-todo "DONE"))

      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-nil (string-match-p "Task A" agenda-content))
        (assert-match "Task B" agenda-content)
        (assert-match "Task C" agenda-content)))))

;;; Test 5: Sequential chain A→B→C in same file

(deftest dag-ops/sequential-chain-same-file ()
  "Creates A→B→C chain where only A is NEXT initially.
Uses chain builder directly."

  ;; 1. Create project with A→B→C chain using builder
  (let ((project-data (make-chain-project "Project Epsilon"
                                          :tasks '("Task A" "Task B" "Task C"))))
    ;; Fix TODO keywords to set initial states
    (org-gtd-projects-fix-todo-keywords (alist-get 'project-marker project-data))

    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-b-id (alist-get 'task-2-id project-data))
          (task-c-id (alist-get 'task-3-id project-data)))

      ;; 2. VERIFY sequential chain
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task A")
        (org-back-to-heading t)
        (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (assert-equal (list task-a-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

      ;; 3. VERIFY only A is NEXT
      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-match "Task A" agenda-content)
        (assert-nil (string-match-p "Task B" agenda-content))
        (assert-nil (string-match-p "Task C" agenda-content)))

      ;; 4. Complete A, verify B becomes NEXT
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

      ;; 5. Complete B, verify C becomes NEXT
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task B")
        (org-back-to-heading t)
        (org-todo "DONE"))

      (org-gtd-engage)
      (let ((agenda-content (agenda-raw-text)))
        (assert-nil (string-match-p "Task A" agenda-content))
        (assert-nil (string-match-p "Task B" agenda-content))
        (assert-match "Task C" agenda-content)))))

;;; Test 4: Parallel children across multiple files

(deftest dag-ops/parallel-children-multi-file ()
  "Adds parallel tasks B and C as children of A across multiple files.
Task A and B in main file, Task C in secondary file, both B and C depend on A."

  ;; 1. Create initial project with A→B chain using builder
  (let ((project-data (make-chain-project "Project Delta"
                                          :tasks '("Task A" "Task B"))))

    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-b-id (alist-get 'task-2-id project-data))
          (project-id (alist-get 'project-id project-data)))

      ;; 2. Create Task C in secondary file
      (let ((second-file (org-gtd--path "delta-secondary"))
            (task-c-id (ogt-builder--generate-id "task")))

        (with-current-buffer (find-file-noselect second-file)
          (erase-buffer)
          (insert (format "* TODO Task C\n:PROPERTIES:\n:ID: %s\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: %s\n:END:\n"
                          task-c-id project-id))
          (org-mode)
          (save-buffer))

        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

          ;; 3. Modify to create A→B and A→C (parallel)
          (with-current-buffer (org-gtd--default-file)
            ;; Remove B's dependency on A (we'll re-add it properly)
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (org-entry-delete (point) "ORG_GTD_DEPENDS_ON")

            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-entry-delete (point) "ORG_GTD_BLOCKS")

            ;; Create A→B and A→C dependencies
            (org-gtd-dependencies-create task-a-id task-b-id)
            (org-gtd-dependencies-create task-a-id task-c-id)

            ;; Update project to include C in first tasks
            (goto-char (point-min))
            (search-forward "Project Delta")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-c-id)
            (org-gtd-projects--set-first-tasks)
            (org-gtd-projects-fix-todo-keywords (point-marker)))

          ;; 4. VERIFY: A blocks both B and C
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (let ((blocks-list (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
              (assert-equal 2 (length blocks-list))))

          ;; 5. VERIFY agenda: Only A is NEXT (blocks B and C)
          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (assert-match "Task A" agenda-content)
            (assert-nil (string-match-p "Task B" agenda-content))
            (assert-nil (string-match-p "Task C" agenda-content)))

          ;; 6. Complete A, both B and C become NEXT (parallel across files)
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (org-todo "DONE"))

          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (assert-nil (string-match-p "Task A" agenda-content))
            (assert-match "Task B" agenda-content)
            (assert-match "Task C" agenda-content)))))))

;;; Test 6: Sequential chain across multiple files

(deftest dag-ops/sequential-chain-multi-file ()
  "Creates A→B→C chain across multiple files.
Task A and B in main file, Task C in secondary file."

  ;; 1. Create initial project with A→B chain using builder
  (let ((project-data (make-chain-project "Project Zeta"
                                          :tasks '("Task A" "Task B"))))

    (let ((task-a-id (alist-get 'task-1-id project-data))
          (task-b-id (alist-get 'task-2-id project-data))
          (project-id (alist-get 'project-id project-data)))

      ;; 2. Create Task C in secondary file
      (let ((second-file (org-gtd--path "zeta-secondary"))
            (task-c-id (ogt-builder--generate-id "task")))

        (with-current-buffer (find-file-noselect second-file)
          (erase-buffer)
          (insert (format "* TODO Task C\n:PROPERTIES:\n:ID: %s\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: %s\n:END:\n"
                          task-c-id project-id))
          (org-mode)
          (save-buffer))

        (let ((org-agenda-files (append (org-agenda-files) (list second-file))))

          ;; 3. Create B→C dependency to extend the chain
          (org-gtd-dependencies-create task-b-id task-c-id)

          ;; Update project state
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Project Zeta")
            (org-back-to-heading t)
            (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-c-id)
            (org-gtd-projects--set-first-tasks)
            (org-gtd-projects-fix-todo-keywords (point-marker)))

          ;; 4. VERIFY sequential chain across files
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task A")
            (org-back-to-heading t)
            (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS"))

            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (assert-equal (list task-a-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON"))
            (assert-equal (list task-c-id) (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))

          (with-current-buffer (find-file-noselect second-file)
            (goto-char (point-min))
            (search-forward "Task C")
            (org-back-to-heading t)
            (assert-equal (list task-b-id) (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")))

          ;; 5. VERIFY only A is NEXT across files
          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (assert-match "Task A" agenda-content)
            (assert-nil (string-match-p "Task B" agenda-content))
            (assert-nil (string-match-p "Task C" agenda-content)))

          ;; 6. Complete A, verify B becomes NEXT
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

          ;; 7. Complete B, verify C becomes NEXT
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-min))
            (search-forward "Task B")
            (org-back-to-heading t)
            (org-todo "DONE"))

          (org-gtd-engage)
          (let ((agenda-content (agenda-raw-text)))
            (assert-nil (string-match-p "Task A" agenda-content))
            (assert-nil (string-match-p "Task B" agenda-content))
            (assert-match "Task C" agenda-content)))))))

(provide 'dag-operations-test)

;;; dag-operations-test.el ends here
