;; -*- lexical-binding: t; coding: utf-8 -*-

;;; org-gtd-graph-transient-test.el --- Unit tests for graph transient menu -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-transient menu system.
;;
;; Test Coverage:
;; - org-gtd-graph-transient-main (main menu)
;; - Add commands (child, sibling, root)
;; - Edit commands (properties, todo, schedule, deadline)
;; - Placeholder commands for future phases
;;

;;; Code:

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'org-gtd-graph-transient)
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-ui)
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))
(require 'with-simulated-input)

(defun org-gtd-graph-transient-test--create-project (title)
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

;; Tests migrated to test-eunit/unit/transient-test.el:
;; - org-gtd-graph-transient-main Tests (2 tests)
;; - Placeholder Command Tests (6 tests)

;;;; org-gtd-graph-transient-add-child Tests

(describe "org-gtd-graph-transient-add-child"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "creates a child task under selected node"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Test Project"))
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
          (expect heading :to-equal "Child Task")
          ;; Verify it's a child (level 3)
          (expect (org-current-level) :to-equal 3)))))

  (it "creates dependency relationship with parent"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Dep Project"))
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
      (expect (org-gtd-get-task-dependencies child-task-id) :to-equal (list parent-task-id))))

  (it "refreshes the graph after creation"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Refresh Project"))
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

      (expect refresh-called :to-be-truthy)))

  (it "prioritizes current project tasks in completion list"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Priority Test"))
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
      (spy-on 'completing-read :and-call-through)

      ;; Call add-child
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-in-project-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewChild RET"
            (org-gtd-graph-transient-add-child))))

      ;; Verify completing-read was called
      (expect 'completing-read :to-have-been-called)

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((all-args (spy-calls-args-for 'completing-read 0))
             (collection (nth 1 all-args)))
        ;; Verify collection exists and has expected structure
        (expect collection :to-be-truthy)
        (expect (listp collection) :to-be-truthy)
        (expect (length collection) :to-be-greater-than 0)
        ;; First entry should be the in-project task
        (let ((first-id (cdar collection)))
          (expect first-id :to-equal task-in-project-id))))))

;;;; org-gtd-graph-transient-add-root Tests

(describe "org-gtd-graph-transient-add-root"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "creates a new root task in the project"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Root Project"))
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
          (expect task-id :not :to-be nil)
          (goto-char (point-min))
          (search-forward "Root Project")
          (org-back-to-heading t)
          (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
            (expect (member task-id first-tasks) :to-be-truthy))))))

  (it "excludes current project tasks from completion list"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Exclude Root Project"))
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
      (spy-on 'completing-read :and-call-through)

      ;; Call add-root
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewRoot RET"
            (org-gtd-graph-transient-add-root))))

      ;; Verify completing-read was called
      (expect 'completing-read :to-have-been-called)

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((all-args (spy-calls-args-for 'completing-read 0))
             (collection (nth 1 all-args)))
        ;; Verify collection exists and has expected structure
        (expect collection :to-be-truthy)
        (expect (listp collection) :to-be-truthy)
        ;; Collection should NOT contain task-in-project-id
        (let ((ids (mapcar #'cdr collection)))
          (expect (member task-in-project-id ids) :not :to-be-truthy)
          ;; But SHOULD contain outside task
          (expect (member task-outside-id ids) :to-be-truthy))))))

;;;; org-gtd-graph-insert-before Tests

(describe "org-gtd-graph-insert-before"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "inserts before root task (no blockers) and makes new task the root"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Insert Before Root"))
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
          (expect (member selected-task-id first-tasks) :to-be-truthy)))

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
        (expect new-task-id :not :to-be nil))

      ;; Verify new task is now root (in FIRST_TASKS)
      ;; Use the same marker context instead of switching buffers
      (org-with-point-at project-marker
        (let ((first-tasks (org-entry-get-multivalued-property (point) "ORG_GTD_FIRST_TASKS")))
          (expect (member new-task-id first-tasks) :to-be-truthy)
          (expect (member selected-task-id first-tasks) :not :to-be-truthy)))

      ;; Verify dependency: new → selected
      (expect (org-gtd-get-task-dependencies selected-task-id) :to-equal (list new-task-id))))

  (it "inserts before task with single blocker and rewires dependencies"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Single Blocker Project"))
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
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list task-a-id))
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-c-id))

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
      (expect (org-gtd-get-task-dependencies new-task-id) :to-equal (list task-a-id))
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list new-task-id))
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list new-task-id))
      (expect (org-gtd-get-task-blockers new-task-id) :to-equal (list task-c-id))))

  (it "inserts before task with multiple blockers and prompts user"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Multi Blocker Project"))
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
        (expect (member task-a-id c-deps) :to-be-truthy)
        (expect (member task-b-id c-deps) :to-be-truthy))

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
        (expect (member task-a-id c-deps) :to-be-truthy)  ; A → C still exists
        (expect (member task-b-id c-deps) :not :to-be-truthy)  ; B → C removed
        (expect (member new-task-id c-deps) :to-be-truthy))  ; X → C created
      (expect (org-gtd-get-task-dependencies new-task-id) :to-equal (list task-b-id))  ; B → X
      (expect (org-gtd-get-task-blockers task-b-id) :to-equal (list new-task-id))))

  (it "prioritizes current project tasks in completion list"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Insert Before Priority"))
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
      (spy-on 'completing-read :and-call-through)

      ;; Call insert-before
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-in-project-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewPredecessor RET"
            (org-gtd-graph-insert-before))))

      ;; Verify completing-read was called
      (expect 'completing-read :to-have-been-called)

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((all-args (spy-calls-args-for 'completing-read 0))
             (collection (nth 1 all-args)))
        ;; Collection should be a list of (display . id) cons cells
        ;; First entry should be the in-project task
        (when (and collection (listp collection) (> (length collection) 0))
          (let ((first-id (cdar collection)))
            (expect first-id :to-equal task-in-project-id))))))

  (it "inserts existing task between parent and selected with rewiring"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Existing Insert Project"))
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
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list task-a-id))
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-c-id))

      ;; Set up graph view and insert existing B before C
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-c-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "Task C-q SPC B C-q SPC (Actions) RET"
            (org-gtd-graph-insert-before))))

      ;; Verify final state: A → B → C
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-b-id))
      (expect (org-gtd-get-task-dependencies task-b-id) :to-equal (list task-a-id))
      (expect (org-gtd-get-task-blockers task-b-id) :to-equal (list task-c-id))
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list task-b-id))
      (expect (org-gtd-get-task-projects task-b-id) :to-contain project-id))))

(describe "org-gtd-graph-insert-after"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "inserts after leaf task (no successors) and creates simple dependency"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Insert After Leaf"))
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
      (expect (org-gtd-get-task-blockers selected-task-id) :to-equal nil)

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
        (expect new-task-id :not :to-be nil))

      ;; Verify dependency: selected → new
      (expect (org-gtd-get-task-blockers selected-task-id) :to-equal (list new-task-id))
      (expect (org-gtd-get-task-dependencies new-task-id) :to-equal (list selected-task-id))))

  (it "inserts after task with single successor and rewires dependencies"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Single Successor Project"))
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
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list task-c-id))
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list task-a-id))

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
      (expect (org-gtd-get-task-blockers task-a-id) :to-equal (list new-task-id))
      (expect (org-gtd-get-task-dependencies new-task-id) :to-equal (list task-a-id))
      (expect (org-gtd-get-task-blockers new-task-id) :to-equal (list task-c-id))
      (expect (org-gtd-get-task-dependencies task-c-id) :to-equal (list new-task-id))))

  (it "inserts after task with multiple successors and prompts user"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Multi Successor Project"))
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
        (expect (member task-c-id a-blocks) :to-be-truthy)
        (expect (member task-d-id a-blocks) :to-be-truthy))

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
        (expect (member task-c-id a-blocks) :to-be-truthy)  ; A → C still exists
        (expect (member task-d-id a-blocks) :not :to-be-truthy)  ; A → D removed
        (expect (member new-task-id a-blocks) :to-be-truthy))  ; A → X created
      (expect (org-gtd-get-task-blockers new-task-id) :to-equal (list task-d-id))  ; X → D
      (expect (org-gtd-get-task-dependencies new-task-id) :to-equal (list task-a-id))))  ; A → X

  (it "prioritizes current project tasks in completion list"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Insert After Priority"))
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
      (spy-on 'completing-read :and-call-through)

      ;; Call insert-after
      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-in-project-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (with-simulated-input "NewSuccessor RET"
            (org-gtd-graph-insert-after))))

      ;; Verify completing-read was called
      (expect 'completing-read :to-have-been-called)

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((all-args (spy-calls-args-for 'completing-read 0))
             (collection (nth 1 all-args)))
        ;; Collection should be a list of (display . id) cons cells
        ;; First entry should be the in-project task
        (when (and collection (listp collection) (> (length collection) 0))
          (let ((first-id (cdar collection)))
            (expect first-id :to-equal task-in-project-id)))))))

;;;; Helper Function Tests

(describe "org-gtd-graph--select-or-create-task-prioritizing-current"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "returns current project tasks first, then others"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Priority Project"))
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
          (expect first-id :to-equal task-in-project-id)))))

  (it "works when no current project tasks exist"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Empty Project"))
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
        (expect (length result) :to-be-greater-than 0))))

  (it "handles task in multiple projects including current"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Multi Project"))
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
          (expect first-id :to-equal task-id))))))

(describe "org-gtd-graph--select-or-create-task-excluding-current"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "excludes tasks already in current project"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Exclude Project"))
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
          (expect (member task-in-project-id ids) :not :to-be-truthy)
          ;; But SHOULD contain outside task
          (expect (member task-outside-id ids) :to-be-truthy)))))

  (it "returns all tasks when current project has none"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Empty Exclude Project"))
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
        (expect (length result) :to-be-greater-than 0)
        (let ((ids (mapcar #'cdr result)))
          (expect (member task-id ids) :to-be-truthy)))))

  (it "excludes task in multiple projects if it's in current"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Multi Exclude Project"))
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
          (expect (member task-id ids) :not :to-be-truthy))))))

;;;; org-gtd-graph-remove-task Tests

(describe "org-gtd-graph-remove-task"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "does not offer trash option when task in one project"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Remove Test"))
           (buffer (get-buffer-create "*Org GTD Graph: remove-test*"))
           task-id)

      ;; Create a task in the project
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (setq task-id (org-entry-get (point) "ID")))

      ;; Spy on completing-read to capture choices
      (spy-on 'completing-read :and-return-value "Remove from this project and keep as independent item")

      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (org-gtd-graph-remove-task)))

      ;; Verify completing-read was called
      (expect 'completing-read :to-have-been-called)

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((all-args (spy-calls-args-for 'completing-read 0))
             (collection (nth 1 all-args)))
        (expect collection :to-be-truthy)
        ;; Should NOT contain "Trash" in any option
        (dolist (option collection)
          (expect (string-match-p "Trash\\|trash\\|delete" option) :not :to-be-truthy)))))

  (it "does not offer trash option when task in multiple projects"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Multi Remove Test"))
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
      (spy-on 'completing-read :and-return-value "Remove from this project only")

      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (org-gtd-graph-remove-task)))

      ;; Verify completing-read was called
      (expect 'completing-read :to-have-been-called)

      ;; Get the COLLECTION argument (2nd argument to completing-read)
      (let* ((all-args (spy-calls-args-for 'completing-read 0))
             (collection (nth 1 all-args)))
        (expect collection :to-be-truthy)
        ;; Should NOT contain "Trash" in any option
        (dolist (option collection)
          (expect (string-match-p "Trash\\|trash\\|delete" option) :not :to-be-truthy))))))

;;;; org-gtd-graph-trash-task Tests

(describe "org-gtd-graph-trash-task"

  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "trashes the selected task and marks it as canceled"
    (let* ((project-marker (org-gtd-graph-transient-test--create-project "Trash Test"))
           (buffer (get-buffer-create "*Org GTD Graph: trash-test*"))
           task-id)

      ;; Create a task in the project
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (setq task-id (org-entry-get (point) "ID")))

      ;; Spy on yes-or-no-p to auto-confirm
      (spy-on 'yes-or-no-p :and-return-value t)

      (with-current-buffer buffer
        (org-gtd-graph-view-mode)
        (setq org-gtd-graph-view--project-marker project-marker)
        (setq org-gtd-graph-ui--selected-node-id task-id)
        (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
          (org-gtd-graph-trash-task)))

      ;; Verify task is marked as canceled
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        (org-back-to-heading t)
        (let ((todo-state (org-get-todo-state)))
          (expect todo-state :to-equal (org-gtd-keywords--canceled)))))))

(provide 'org-gtd-graph-transient-test)

;;; org-gtd-graph-transient-test.el ends here
