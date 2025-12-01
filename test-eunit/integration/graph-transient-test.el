;;; graph-transient-test.el --- Integration tests for graph transient menu -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for org-gtd-graph-transient menu system.
;; Tests add-child, add-root, insert-before, insert-after, remove-task, trash-task,
;; and helper functions.
;;
;; Migrated from test/org-gtd-graph-transient-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

(require 'org-gtd-graph-transient)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-ui)
(require 'with-simulated-input)

;;; Test Helpers

(defun ogt-graph-transient-test--create-project (title)
  "Create a test project with TITLE and return its marker."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    ;; Create project heading
    (insert (format "* %s\n" title))
    (forward-line -1)
    (org-back-to-heading t)
    (let ((project-id (org-id-get-create)))
      (org-entry-put (point) "ORG_GTD" "Projects")

      ;; Create task heading
      (org-end-of-subtree t t)
      (insert "** TODO Task 1\n")
      (forward-line -1)
      (org-back-to-heading t)
      (let ((task-id (org-id-get-create)))
        (org-entry-put (point) "ORG_GTD" "Actions")

        ;; Link task to project
        (org-gtd-add-to-multivalued-property task-id org-gtd-prop-project-ids project-id)

        ;; Add task to project's FIRST_TASKS
        (goto-char (point-min))
        (search-forward title)
        (org-back-to-heading t)
        (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-id)
        (basic-save-buffer)
        (point-marker)))))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; org-gtd-graph-transient-add-child Tests

(deftest graph-transient/add-child-creates-under-selected-node ()
  "Creates a child task under selected node."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Test Project"))
         (buffer (get-buffer-create "*Org GTD Graph: test*"))
         parent-task-id)

    ;; Get the project's first task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq parent-task-id (org-entry-get (point) "ID")))

    ;; Set up graph view buffer with selected node
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id parent-task-id)
      ;; Mock refresh function
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (with-simulated-input "Child C-q SPC Task RET"
          (org-gtd-graph-transient-add-child))))

    ;; Verify child task was created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      ;; Move to end of parent subtree to find child
      (org-end-of-subtree t t)
      (org-back-to-heading t)
      (let ((heading (org-get-heading t t t t)))
        (assert-equal "Child Task" heading)
        ;; Verify it's a child (level 3)
        (assert-equal 3 (org-current-level))))))

(deftest graph-transient/add-child-creates-dependency-relationship ()
  "Creates dependency relationship with parent."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Dep Project"))
         (buffer (get-buffer-create "*Org GTD Graph: dep-test*"))
         parent-task-id child-task-id)

    ;; Get the parent task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq parent-task-id (org-entry-get (point) "ID")))

    ;; Set up graph view and create child
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id parent-task-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (with-simulated-input "Dependent C-q SPC Child RET"
          (org-gtd-graph-transient-add-child))))

    ;; Get child task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Dependent Child")
      (org-back-to-heading t)
      (setq child-task-id (org-entry-get (point) "ID")))

    ;; Verify dependency: parent blocks child
    (assert-equal (list parent-task-id) (org-gtd-get-task-dependencies child-task-id))))

(deftest graph-transient/add-child-refreshes-graph ()
  "Refreshes the graph after creation."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Refresh Project"))
         (buffer (get-buffer-create "*Org GTD Graph: refresh-test*"))
         (refresh-called nil)
         parent-task-id)

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq parent-task-id (org-entry-get (point) "ID")))

    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id parent-task-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh)
                 (lambda () (setq refresh-called t))))
        (with-simulated-input "Refresh C-q SPC Child RET"
          (org-gtd-graph-transient-add-child))))

    (assert-true refresh-called)))

(deftest graph-transient/add-child-prioritizes-current-project-tasks ()
  "Prioritizes current project tasks in completion list."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Priority Test"))
         (buffer (get-buffer-create "*Org GTD Graph: priority-test*"))
         project-id task-in-project-id task-outside-id)

    ;; Get project ID and in-project task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Priority Test")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Task 1 is already in project
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-in-project-id (org-entry-get (point) "ID")))

    ;; Create a task OUTSIDE the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO Outside Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-outside-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Spy on completing-read to capture collection
    (with-spy-call-through completing-read calls
      ;; Call add-child
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-in-project-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewChild RET"
            (org-gtd-graph-transient-add-child))))

      ;; Verify completing-read was called
      (assert-true (spy-called-p calls))

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((first-call-args (spy-first-call calls))
             (collection (nth 1 first-call-args)))
        ;; Verify collection exists and has expected structure
        (assert-true collection)
        (assert-true (listp collection))
        (assert-true (> (length collection) 0))
        ;; First entry should be the in-project task
        (let ((first-id (cdar collection)))
          (assert-equal task-in-project-id first-id))))))

;;; org-gtd-graph-transient-add-root Tests

(deftest graph-transient/add-root-creates-new-root-task ()
  "Creates a new root task in the project."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Root Project"))
         (buffer (get-buffer-create "*Org GTD Graph: root-test*"))
         project-id)

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Root Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID")))

    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (with-simulated-input "New C-q SPC Root RET"
          (org-gtd-graph-transient-add-root))))

    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "New Root")
      (org-back-to-heading t)
      (let ((task-id (org-entry-get (point) "ID")))
        (assert-true task-id)
        (goto-char (point-min))
        (search-forward "Root Project")
        (org-back-to-heading t)
        (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
          (assert-true (member task-id first-tasks)))))))

(deftest graph-transient/add-root-excludes-current-project-tasks ()
  "Excludes current project tasks from completion list."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Exclude Root Project"))
         (buffer (get-buffer-create "*Org GTD Graph: exclude-root-test*"))
         project-id task-in-project-id task-outside-id)

    ;; Get project ID and in-project task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Exclude Root Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Task 1 is already in project
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-in-project-id (org-entry-get (point) "ID")))

    ;; Create a task OUTSIDE the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO Outside Root Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-outside-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Spy on completing-read to capture collection
    (with-spy-call-through completing-read calls
      ;; Call add-root
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewRoot RET"
            (org-gtd-graph-transient-add-root))))

      ;; Verify completing-read was called
      (assert-true (spy-called-p calls))

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((first-call-args (spy-first-call calls))
             (collection (nth 1 first-call-args)))
        ;; Verify collection exists and has expected structure
        (assert-true collection)
        (assert-true (listp collection))
        ;; Collection should NOT contain task-in-project-id
        (let ((ids (mapcar #'cdr collection)))
          (assert-nil (member task-in-project-id ids))
          ;; But SHOULD contain outside task
          (assert-true (member task-outside-id ids)))))))

;;; org-gtd-graph-insert-before Tests

(deftest graph-transient/insert-before-root-makes-new-task-root ()
  "Inserts before root task (no blockers) and makes new task the root."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Insert Before Root"))
         (buffer (get-buffer-create "*Org GTD Graph: insert-before-root*"))
         project-id selected-task-id new-task-id)

    ;; Get project ID and task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Insert Before Root")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq selected-task-id (org-entry-get (point) "ID")))

    ;; Verify selected task is root
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Insert Before Root")
      (org-back-to-heading t)
      (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
        (assert-true (member selected-task-id first-tasks))))

    ;; Set up graph view and insert before
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id selected-task-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (with-simulated-input "NewPredecessor RET"
          (org-gtd-graph-insert-before))))

    ;; Verify new task created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "NewPredecessor")
      (org-back-to-heading t)
      (setq new-task-id (org-entry-get (point) "ID"))
      (assert-true new-task-id))

    ;; Verify new task is now root (in FIRST_TASKS)
    (org-with-point-at project-marker
      (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
        (assert-true (member new-task-id first-tasks))
        (assert-nil (member selected-task-id first-tasks))))

    ;; Verify dependency: new → selected
    (assert-equal (list new-task-id) (org-gtd-get-task-dependencies selected-task-id))))

(deftest graph-transient/insert-before-single-blocker-rewires ()
  "Inserts before task with single blocker and rewires dependencies."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Single Blocker Project"))
         (buffer (get-buffer-create "*Org GTD Graph: single-blocker*"))
         project-id task-a-id task-c-id new-task-id)

    ;; Create Task A and Task C with A → C dependency
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Single Blocker Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Create Task A
      (org-end-of-subtree t t)
      (insert "** TODO Task A\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create Task C
      (goto-char (point-min))
      (search-forward "Single Blocker Project")
      (org-end-of-subtree t t)
      (insert "** TODO Task C\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-c-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create A → C dependency
      (org-gtd-dependencies-create task-a-id task-c-id)

      ;; Make A a root task
      (goto-char (point-min))
      (search-forward "Single Blocker Project")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-a-id)
      (basic-save-buffer))

    ;; Verify initial state: A → C
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-c-id))
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers task-a-id))

    ;; Set up graph view and insert B before C
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id task-c-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (with-simulated-input "Task C-q SPC B RET"
          (org-gtd-graph-insert-before))))

    ;; Get new task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (setq new-task-id (org-entry-get (point) "ID")))

    ;; Verify final state: A → B → C
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies new-task-id))
    (assert-equal (list new-task-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list new-task-id) (org-gtd-get-task-dependencies task-c-id))
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers new-task-id))))

(deftest graph-transient/insert-before-multi-blocker-prompts ()
  "Inserts before task with multiple blockers and prompts user."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Multi Blocker Project"))
         (buffer (get-buffer-create "*Org GTD Graph: multi-blocker*"))
         project-id task-a-id task-b-id task-c-id new-task-id)

    ;; Create Task A, Task B, Task C with A → C and B → C dependencies
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Multi Blocker Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Create Task A
      (org-end-of-subtree t t)
      (insert "** TODO Task A\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create Task B
      (goto-char (point-min))
      (search-forward "Multi Blocker Project")
      (org-end-of-subtree t t)
      (insert "** TODO Task B\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-b-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create Task C
      (goto-char (point-min))
      (search-forward "Multi Blocker Project")
      (org-end-of-subtree t t)
      (insert "** TODO Task C\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-c-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create A → C and B → C dependencies
      (org-gtd-dependencies-create task-a-id task-c-id)
      (org-gtd-dependencies-create task-b-id task-c-id)

      ;; Make A and B root tasks
      (goto-char (point-min))
      (search-forward "Multi Blocker Project")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-a-id)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-b-id)
      (basic-save-buffer))

    ;; Verify initial state: A → C, B → C
    (let ((c-deps (org-gtd-get-task-dependencies task-c-id)))
      (assert-true (member task-a-id c-deps))
      (assert-true (member task-b-id c-deps)))

    ;; Set up graph view and insert X before C, choosing B as the blocker to replace
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id task-c-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        ;; First input: task name, Second input: which blocker to insert between
        (with-simulated-input "Task C-q SPC X RET Task C-q SPC B RET"
          (org-gtd-graph-insert-before))))

    ;; Get new task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task X")
      (org-back-to-heading t)
      (setq new-task-id (org-entry-get (point) "ID")))

    ;; Verify final state: A → C (unchanged), B → X → C
    (let ((c-deps (org-gtd-get-task-dependencies task-c-id)))
      (assert-true (member task-a-id c-deps))          ; A → C still exists
      (assert-nil (member task-b-id c-deps))           ; B → C removed
      (assert-true (member new-task-id c-deps)))       ; X → C created
    (assert-equal (list task-b-id) (org-gtd-get-task-dependencies new-task-id))  ; B → X
    (assert-equal (list new-task-id) (org-gtd-get-task-blockers task-b-id))))

(deftest graph-transient/insert-before-prioritizes-current-project ()
  "Prioritizes current project tasks in completion list."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Insert Before Priority"))
         (buffer (get-buffer-create "*Org GTD Graph: insert-before-priority*"))
         project-id task-in-project-id task-outside-id)

    ;; Get project ID and in-project task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Insert Before Priority")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Task 1 is already in project
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-in-project-id (org-entry-get (point) "ID")))

    ;; Create a task OUTSIDE the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO Outside Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-outside-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Spy on completing-read to capture collection
    (with-spy-call-through completing-read calls
      ;; Call insert-before
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-in-project-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewPredecessor RET"
            (org-gtd-graph-insert-before))))

      ;; Verify completing-read was called
      (assert-true (spy-called-p calls))

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((first-call-args (spy-first-call calls))
             (collection (nth 1 first-call-args)))
        ;; Collection should be a list of (display . id) cons cells
        ;; First entry should be the in-project task
        (when (and collection (listp collection) (> (length collection) 0))
          (let ((first-id (cdar collection)))
            (assert-equal task-in-project-id first-id)))))))

(deftest graph-transient/insert-before-existing-rewires ()
  "Inserts existing task between parent and selected with rewiring."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Existing Insert Project"))
         (buffer (get-buffer-create "*Org GTD Graph: existing*"))
         project-id task-a-id task-c-id task-b-id)

    ;; Create project with tasks A and C with A → C dependency
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Existing Insert Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Create Task A
      (org-end-of-subtree t t)
      (insert "** TODO Task A\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create Task C
      (goto-char (point-min))
      (search-forward "Existing Insert Project")
      (org-end-of-subtree t t)
      (insert "** TODO Task C\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-c-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create A → C dependency
      (org-gtd-dependencies-create task-a-id task-c-id)

      ;; Make A a root task
      (goto-char (point-min))
      (search-forward "Existing Insert Project")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-a-id)
      (basic-save-buffer))

    ;; Create Task B outside project (independent action)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO Task B\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-b-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Verify initial state: A → C
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-c-id))
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers task-a-id))

    ;; Set up graph view and insert existing B before C
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id task-c-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (with-simulated-input "Task C-q SPC B C-q SPC (Actions) RET"
          (org-gtd-graph-insert-before))))

    ;; Verify final state: A → B → C
    (assert-equal (list task-b-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-b-id))
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers task-b-id))
    (assert-equal (list task-b-id) (org-gtd-get-task-dependencies task-c-id))
    (assert-true (member project-id (org-gtd-get-task-projects task-b-id)))))

;;; org-gtd-graph-insert-after Tests

(deftest graph-transient/insert-after-leaf-creates-dependency ()
  "Inserts after leaf task (no successors) and creates simple dependency."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Insert After Leaf"))
         (buffer (get-buffer-create "*Org GTD Graph: insert-after-leaf*"))
         project-id selected-task-id new-task-id)

    ;; Get project ID and task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Insert After Leaf")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq selected-task-id (org-entry-get (point) "ID")))

    ;; Verify selected task has no successors
    (assert-nil (org-gtd-get-task-blockers selected-task-id))

    ;; Set up graph view and insert after
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id selected-task-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (with-simulated-input "NewSuccessor RET"
          (org-gtd-graph-insert-after))))

    ;; Verify new task created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "NewSuccessor")
      (org-back-to-heading t)
      (setq new-task-id (org-entry-get (point) "ID"))
      (assert-true new-task-id))

    ;; Verify dependency: selected → new
    (assert-equal (list new-task-id) (org-gtd-get-task-blockers selected-task-id))
    (assert-equal (list selected-task-id) (org-gtd-get-task-dependencies new-task-id))))

(deftest graph-transient/insert-after-single-successor-rewires ()
  "Inserts after task with single successor and rewires dependencies."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Single Successor Project"))
         (buffer (get-buffer-create "*Org GTD Graph: single-successor*"))
         project-id task-a-id task-c-id new-task-id)

    ;; Create Task A and Task C with A → C dependency
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Single Successor Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Create Task A
      (org-end-of-subtree t t)
      (insert "** TODO Task A\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create Task C
      (goto-char (point-min))
      (search-forward "Single Successor Project")
      (org-end-of-subtree t t)
      (insert "** TODO Task C\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-c-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create A → C dependency
      (org-gtd-dependencies-create task-a-id task-c-id)

      ;; Make A a root task
      (goto-char (point-min))
      (search-forward "Single Successor Project")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-a-id)
      (basic-save-buffer))

    ;; Verify initial state: A → C
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies task-c-id))

    ;; Set up graph view and insert B after A
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id task-a-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (with-simulated-input "Task C-q SPC B RET"
          (org-gtd-graph-insert-after))))

    ;; Get new task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task B")
      (org-back-to-heading t)
      (setq new-task-id (org-entry-get (point) "ID")))

    ;; Verify final state: A → B → C
    (assert-equal (list new-task-id) (org-gtd-get-task-blockers task-a-id))
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies new-task-id))
    (assert-equal (list task-c-id) (org-gtd-get-task-blockers new-task-id))
    (assert-equal (list new-task-id) (org-gtd-get-task-dependencies task-c-id))))

(deftest graph-transient/insert-after-multi-successor-prompts ()
  "Inserts after task with multiple successors and prompts user."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Multi Successor Project"))
         (buffer (get-buffer-create "*Org GTD Graph: multi-successor*"))
         project-id task-a-id task-c-id task-d-id new-task-id)

    ;; Create Task A, Task C, Task D with A → C and A → D dependencies
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Multi Successor Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Create Task A
      (org-end-of-subtree t t)
      (insert "** TODO Task A\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create Task C
      (goto-char (point-min))
      (search-forward "Multi Successor Project")
      (org-end-of-subtree t t)
      (insert "** TODO Task C\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-c-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create Task D
      (goto-char (point-min))
      (search-forward "Multi Successor Project")
      (org-end-of-subtree t t)
      (insert "** TODO Task D\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-d-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Create A → C and A → D dependencies
      (org-gtd-dependencies-create task-a-id task-c-id)
      (org-gtd-dependencies-create task-a-id task-d-id)

      ;; Make A a root task
      (goto-char (point-min))
      (search-forward "Multi Successor Project")
      (org-back-to-heading t)
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_FIRST_TASKS" task-a-id)
      (basic-save-buffer))

    ;; Verify initial state: A → C, A → D
    (let ((a-blocks (org-gtd-get-task-blockers task-a-id)))
      (assert-true (member task-c-id a-blocks))
      (assert-true (member task-d-id a-blocks)))

    ;; Set up graph view and insert X after A, choosing D as the successor to rewire
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-ui--selected-node-id task-a-id)
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        ;; First input: task name, Second input: which successor to insert before
        (with-simulated-input "Task C-q SPC X RET Task C-q SPC D RET"
          (org-gtd-graph-insert-after))))

    ;; Get new task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task X")
      (org-back-to-heading t)
      (setq new-task-id (org-entry-get (point) "ID")))

    ;; Verify final state: A → C (unchanged), A → X → D
    (let ((a-blocks (org-gtd-get-task-blockers task-a-id)))
      (assert-true (member task-c-id a-blocks))      ; A → C still exists
      (assert-nil (member task-d-id a-blocks))       ; A → D removed
      (assert-true (member new-task-id a-blocks)))   ; A → X created
    (assert-equal (list task-d-id) (org-gtd-get-task-blockers new-task-id))  ; X → D
    (assert-equal (list task-a-id) (org-gtd-get-task-dependencies new-task-id))))  ; A → X

(deftest graph-transient/insert-after-prioritizes-current-project ()
  "Prioritizes current project tasks in completion list."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Insert After Priority"))
         (buffer (get-buffer-create "*Org GTD Graph: insert-after-priority*"))
         project-id task-in-project-id task-outside-id)

    ;; Get project ID and in-project task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Insert After Priority")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Task 1 is already in project
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-in-project-id (org-entry-get (point) "ID")))

    ;; Create a task OUTSIDE the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO Outside Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-outside-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Spy on completing-read to capture collection
    (with-spy-call-through completing-read calls
      ;; Call insert-after
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-in-project-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewSuccessor RET"
            (org-gtd-graph-insert-after))))

      ;; Verify completing-read was called
      (assert-true (spy-called-p calls))

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((first-call-args (spy-first-call calls))
             (collection (nth 1 first-call-args)))
        ;; Collection should be a list of (display . id) cons cells
        ;; First entry should be the in-project task
        (when (and collection (listp collection) (> (length collection) 0))
          (let ((first-id (cdar collection)))
            (assert-equal task-in-project-id first-id)))))))

;;; Helper Function Tests

(deftest graph-transient/select-prioritizing-returns-current-first ()
  "Returns current project tasks first, then others."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Priority Project"))
         project-id task-in-project-id task-outside-id)

    ;; Get project ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Priority Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Task 1 already exists in project (from create-project helper)
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-in-project-id (org-entry-get (point) "ID")))

    ;; Create a task OUTSIDE the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO Outside Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-outside-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Call helper and check ordering
    (let ((result (org-gtd-graph--select-or-create-task-prioritizing-current
                   "Select task: "
                   project-marker)))
      ;; result should be list of (display . id) pairs
      ;; First entry should be the in-project task
      (let ((first-id (cdr (car result))))
        (assert-equal task-in-project-id first-id)))))

(deftest graph-transient/select-prioritizing-works-with-no-project-tasks ()
  "Works when no current project tasks exist."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Empty Project"))
         project-id task-id)

    ;; Get project ID and remove Task 1
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Empty Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Delete Task 1
      (search-forward "Task 1")
      (org-back-to-heading t)
      (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))
      (basic-save-buffer))

    ;; Create external task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO External Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Should return external tasks
    (let ((result (org-gtd-graph--select-or-create-task-prioritizing-current
                   "Select task: "
                   project-marker)))
      (assert-true (> (length result) 0)))))

(deftest graph-transient/select-prioritizing-handles-multi-project-task ()
  "Handles task in multiple projects including current."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Multi Project"))
         project-id task-id)

    ;; Get Task 1 (already in project)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Multi Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-id (org-entry-get (point) "ID"))

      ;; Add this task to another project too
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "fake-other-project-id")
      (basic-save-buffer))

    ;; Should still appear in current project section
    (let ((result (org-gtd-graph--select-or-create-task-prioritizing-current
                   "Select task: "
                   project-marker)))
      (let ((first-id (cdr (car result))))
        (assert-equal task-id first-id)))))

(deftest graph-transient/select-excluding-excludes-current-tasks ()
  "Excludes tasks already in current project."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Exclude Project"))
         project-id task-in-project-id task-outside-id)

    ;; Get project ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Exclude Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Task 1 already exists in project (from create-project helper)
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-in-project-id (org-entry-get (point) "ID")))

    ;; Create a task OUTSIDE the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO Outside Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-outside-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Call helper and verify current project task is excluded
    (let ((result (org-gtd-graph--select-or-create-task-excluding-current
                   "Select task: "
                   project-marker)))
      ;; Result should NOT contain task-in-project-id
      (let ((ids (mapcar #'cdr result)))
        (assert-nil (member task-in-project-id ids))
        ;; But SHOULD contain outside task
        (assert-true (member task-outside-id ids))))))

(deftest graph-transient/select-excluding-returns-all-when-empty ()
  "Returns all tasks when current project has none."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Empty Exclude Project"))
         project-id task-id)

    ;; Get project ID and remove Task 1
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Empty Exclude Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      ;; Delete Task 1
      (search-forward "Task 1")
      (org-back-to-heading t)
      (delete-region (point) (save-excursion (org-end-of-subtree t t) (point)))
      (basic-save-buffer))

    ;; Create external task
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "** TODO External Task\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (basic-save-buffer))

    ;; Should return external tasks since project is empty
    (let ((result (org-gtd-graph--select-or-create-task-excluding-current
                   "Select task: "
                   project-marker)))
      (assert-true (> (length result) 0))
      (let ((ids (mapcar #'cdr result)))
        (assert-true (member task-id ids))))))

(deftest graph-transient/select-excluding-excludes-multi-project ()
  "Excludes task in multiple projects if it's in current."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Multi Exclude Project"))
         project-id task-id)

    ;; Get Task 1 (already in project)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Multi Exclude Project")
      (org-back-to-heading t)
      (setq project-id (org-entry-get (point) "ID"))

      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-id (org-entry-get (point) "ID"))

      ;; Add this task to another project too
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "fake-other-project-id")
      (basic-save-buffer))

    ;; Should still be excluded because it's in current project
    (let ((result (org-gtd-graph--select-or-create-task-excluding-current
                   "Select task: "
                   project-marker)))
      (let ((ids (mapcar #'cdr result)))
        (assert-nil (member task-id ids))))))

;;; org-gtd-graph-remove-task Tests

(deftest graph-transient/remove-task-no-trash-option-single-project ()
  "Does not offer trash option when task in one project."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Remove Test"))
         (buffer (get-buffer-create "*Org GTD Graph: remove-test*"))
         task-id)

    ;; Create a task in the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-id (org-entry-get (point) "ID")))

    ;; Spy on completing-read to capture choices
    (with-spy completing-read calls "Remove from this project and keep as independent item"
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (org-gtd-graph-remove-task)))

      ;; Verify completing-read was called
      (assert-true (spy-called-p calls))

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((first-call-args (spy-first-call calls))
             (collection (nth 1 first-call-args)))
        (assert-true collection)
        ;; Should NOT contain "Trash" in any option
        (dolist (option collection)
          (assert-nil (string-match-p "Trash\\|trash\\|delete" option)))))))

(deftest graph-transient/remove-task-no-trash-option-multi-project ()
  "Does not offer trash option when task in multiple projects."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Multi Remove Test"))
         (buffer (get-buffer-create "*Org GTD Graph: multi-remove-test*"))
         task-id)

    ;; Create a task in the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-id (org-entry-get (point) "ID"))
      ;; Add task to another project
      (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" "fake-other-project-id")
      (basic-save-buffer))

    ;; Spy on completing-read to capture choices
    (with-spy completing-read calls "Remove from this project only"
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (org-gtd-graph-remove-task)))

      ;; Verify completing-read was called
      (assert-true (spy-called-p calls))

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((first-call-args (spy-first-call calls))
             (collection (nth 1 first-call-args)))
        (assert-true collection)
        ;; Should NOT contain "Trash" in any option
        (dolist (option collection)
          (assert-nil (string-match-p "Trash\\|trash\\|delete" option)))))))

;;; org-gtd-graph-trash-task Tests

(deftest graph-transient/trash-task-marks-canceled ()
  "Trashes the selected task and marks it as canceled."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Trash Test"))
         (buffer (get-buffer-create "*Org GTD Graph: trash-test*"))
         task-id)

    ;; Create a task in the project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-id (org-entry-get (point) "ID")))

    ;; Spy on yes-or-no-p to auto-confirm
    (with-spy yes-or-no-p calls t
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (org-gtd-graph-trash-task))))

    ;; Verify task is marked as canceled
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (let ((todo-state (org-get-todo-state)))
        (assert-equal (org-gtd-keywords--canceled) todo-state)))))

(provide 'graph-transient-test)

;;; graph-transient-test.el ends here
