;;; graph-transient-test.el --- Integration tests for graph transient menu -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for org-gtd-graph-transient menu system.
;; Tests add-root, add-successor, add-blocker, remove-task, trash-task,
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
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

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

(deftest graph-transient/remove-task-confirms-before-removing ()
  "Remove task asks for confirmation before removing from project."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Remove Test"))
         (buffer (get-buffer-create "*Org GTD Graph: remove-test*"))
         task-id
         project-id)

    ;; Get task and project IDs
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-id (org-entry-get (point) "ID"))
      (org-with-point-at project-marker
        (setq project-id (org-entry-get (point) "ID"))))

    ;; Mock yes-or-no-p to return t (confirm removal)
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-view--graph
            (org-gtd-graph-data--extract-from-project project-marker))
      (setq org-gtd-graph-ui--selected-node-id task-id)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (org-gtd-graph-remove-task)))

    ;; Verify task was removed from project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
        (assert-nil (member project-id project-ids))))))

(deftest graph-transient/remove-task-preserves-task-when-denied ()
  "Remove task preserves project membership when user denies."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Deny Remove Test"))
         (buffer (get-buffer-create "*Org GTD Graph: deny-remove-test*"))
         task-id
         project-id)

    ;; Get task and project IDs
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-id (org-entry-get (point) "ID"))
      (org-with-point-at project-marker
        (setq project-id (org-entry-get (point) "ID"))))

    ;; Mock yes-or-no-p to return nil (deny removal)
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-view--graph
            (org-gtd-graph-data--extract-from-project project-marker))
      (setq org-gtd-graph-ui--selected-node-id task-id)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil))
                ((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (org-gtd-graph-remove-task)))

    ;; Verify task is still in project
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
        (assert-true (member project-id project-ids))))))

(deftest graph-transient/remove-task-selects-nearby-node ()
  "After removing a task, a nearby node (predecessor or project) is selected.
This prevents the graph view from having a stale selected-node-id."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Select After Remove"))
         (buffer (get-buffer-create "*Org GTD Graph: select-after-remove*"))
         task-1-id task-2-id project-id)

    ;; Get task 1 ID and project ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-1-id (org-entry-get (point) "ID"))
      (org-with-point-at project-marker
        (setq project-id (org-entry-get (point) "ID")))

      ;; Add a second task that depends on task 1
      (org-end-of-subtree t t)
      (insert "** TODO Task 2\n")
      (forward-line -1)
      (org-back-to-heading t)
      (setq task-2-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD" "Actions")
      (org-gtd-add-to-multivalued-property task-2-id org-gtd-prop-project-ids project-id)
      ;; Task 2 depends on Task 1 (Task 1 blocks Task 2)
      (org-gtd-dependencies-create task-1-id task-2-id)
      (basic-save-buffer))

    ;; Set up graph view with Task 2 selected
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      ;; Extract graph so predecessors/successors can be computed
      (setq org-gtd-graph-view--graph
            (org-gtd-graph-data--extract-from-project project-marker))
      (setq org-gtd-graph-ui--selected-node-id task-2-id)

      ;; Remove Task 2 (which has Task 1 as predecessor)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (org-gtd-graph-remove-task))

      ;; Verify selected node is now Task 1 (the predecessor)
      (assert-equal task-1-id org-gtd-graph-ui--selected-node-id))))

(deftest graph-transient/remove-task-selects-project-when-no-nearby-tasks ()
  "When removing the only task, the project node is selected."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Select Project After Remove"))
         (buffer (get-buffer-create "*Org GTD Graph: select-project*"))
         task-id project-id)

    ;; Get task and project IDs
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq task-id (org-entry-get (point) "ID"))
      (org-with-point-at project-marker
        (setq project-id (org-entry-get (point) "ID"))))

    ;; Set up graph view
    (with-current-buffer buffer
      (org-gtd-graph-view-mode)
      (setq org-gtd-graph-view--project-marker project-marker)
      (setq org-gtd-graph-view--graph
            (org-gtd-graph-data--extract-from-project project-marker))
      (setq org-gtd-graph-ui--selected-node-id task-id)

      ;; Remove the only task
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t))
                ((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (org-gtd-graph-remove-task))

      ;; Verify selected node is now the project
      (assert-equal project-id org-gtd-graph-ui--selected-node-id))))

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

;;; Unified Add-Successor Command Tests

(deftest graph-transient/add-successor-creates-blocking-relationship ()
  "Add-successor creates new task that is blocked by selected task(s).
The new successor depends on the selected predecessors."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Add Successor Project"))
         selected-task-id new-task-id)

    ;; Get task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq selected-task-id (org-entry-get (point) "ID")))

    ;; Use internal function directly (bypasses transient UI which can't be tested)
    (setq new-task-id (org-gtd-graph--add-successor-internal
                       "NewSuccessor"
                       (list selected-task-id)
                       project-marker))

    ;; Verify new task created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "NewSuccessor")
      (org-back-to-heading t)
      (assert-equal new-task-id (org-entry-get (point) "ID")))

    ;; Verify dependency: selected-task blocks new-task
    ;; So new-task depends on selected-task
    (assert-true (member selected-task-id (org-gtd-get-task-dependencies new-task-id)))))

;;; Unified Add-Blocker Command Tests

(deftest graph-transient/add-blocker-creates-dependency-relationship ()
  "Add-blocker creates new task that blocks selected task(s).
Unified command replacing insert-before and add-blocker."
  (let* ((project-marker (ogt-graph-transient-test--create-project "Add Blocker Project"))
         selected-task-id new-task-id)

    ;; Get task ID
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "Task 1")
      (org-back-to-heading t)
      (setq selected-task-id (org-entry-get (point) "ID")))

    ;; Use internal function directly (bypasses transient UI which can't be tested)
    (setq new-task-id (org-gtd-graph--add-blocker-internal
                       "NewBlocker"
                       (list selected-task-id)
                       project-marker))

    ;; Verify new task created
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "NewBlocker")
      (org-back-to-heading t)
      (assert-equal new-task-id (org-entry-get (point) "ID")))

    ;; Verify dependency: new-task blocks selected-task
    ;; So selected-task depends on new-task
    (assert-true (member new-task-id (org-gtd-get-task-dependencies selected-task-id)))))

;;; Custom Transient Prefix Class Tests

(deftest graph-transient/custom-prefix-class-exists ()
  "Custom transient prefix class should exist with edge-selection slot.
This verifies the proper transient architecture using EIEIO objects."
  ;; Verify class exists
  (assert-true (find-class 'org-gtd-graph-transient-prefix))

  ;; Verify it inherits from transient-prefix
  (assert-true (child-of-class-p 'org-gtd-graph-transient-prefix 'transient-prefix))

  ;; Create an instance and verify edge-selection slot works
  (let* ((edge-data (list (cons "task-1" t) (cons "task-2" nil)))
         (obj (make-instance 'org-gtd-graph-transient-prefix
                             :edge-selection edge-data
                             :command 'test-command)))
    ;; Verify slot access with oref
    (assert-equal edge-data (oref obj edge-selection))

    ;; Verify slot mutation with oset
    (let ((new-data (list (cons "task-1" nil) (cons "task-2" t))))
      (oset obj edge-selection new-data)
      (assert-equal new-data (oref obj edge-selection)))))

(provide 'graph-transient-test)

;;; graph-transient-test.el ends here
