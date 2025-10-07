;;; builders.el --- Builder pattern for test data -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Builder functions for creating test data in an expressive, maintainable way.
;; These builders replace brittle string-based fixtures with composable functions
;; that are self-documenting and type-safe.
;;
;; Usage Examples:
;;
;;   ;; Simple task
;;   (make-task "Write documentation"
;;              :id "doc-task-id"
;;              :status 'next)
;;
;;   ;; Task with dependencies
;;   (make-task "Deploy to production"
;;              :id "deploy-id"
;;              :depends-on '("test-id" "review-id")
;;              :status 'todo)
;;
;;   ;; Simple project
;;   (make-project "Launch Product"
;;                 :id "launch-id"
;;                 :tasks '("Plan features" "Build MVP" "Get feedback"))
;;
;;   ;; DAG-structured project
;;   (make-dag-project "Complex Project"
;;                     :id "complex-id"
;;                     :task-specs '(("Task A" :id "a-id" :blocks ("b-id" "d-id"))
;;                                   ("Task B" :id "b-id" :depends-on ("a-id") :blocks ("c-id"))
;;                                   ("Task C" :id "c-id" :depends-on ("b-id"))
;;                                   ("Task D" :id "d-id" :depends-on ("a-id"))))
;;
;;   ;; Sequential project (tasks depend on previous task)
;;   (make-sequential-project "Write Book"
;;                           :tasks '("Outline" "Draft" "Edit" "Publish"))
;;


;;; Code:

(require 'org)
(require 'org-gtd-core)
(require 'f)

;;;; ID Generation

(defvar ogt-builder--id-counter 0
  "Counter for generating unique test IDs.")

(defun ogt-builder--generate-id (prefix)
  "Generate a unique ID with PREFIX for testing."
  (setq ogt-builder--id-counter (1+ ogt-builder--id-counter))
  (format "%s-%d" prefix ogt-builder--id-counter))

;;;; Task Builder

(cl-defun make-task (description &key
                                  id
                                  (status 'todo)
                                  depends-on
                                  blocks
                                  delegated-to
                                  timestamp
                                  tags
                                  (level 2)
                                  properties
                                  project-name
                                  project-ids)
  "Build a test task with DESCRIPTION and options.

OPTIONS (keyword arguments):
  :id           - Task ID (auto-generated if not provided)
  :status       - TODO keyword (todo, next, done, cncl - defaults to todo)
  :depends-on   - List of task IDs this task depends on
  :blocks       - List of task IDs this task blocks
  :delegated-to - Person this task is delegated to
  :timestamp    - Timestamp string for the task
  :tags         - List of tags
  :level        - Heading level (default 2 for project tasks)
  :properties   - Additional properties as alist
  :project-name - Name of the project this task belongs to
  :project-ids  - List of project IDs this task belongs to (for multi-project tasks)

Returns a marker pointing to the created task heading."
  (let* ((task-id (or id (ogt-builder--generate-id "task")))
         (keyword (pcase status
                    ('todo "TODO")
                    ('next "NEXT")
                    ('done "DONE")
                    ('cncl "CNCL")
                    (_ (upcase (symbol-name status)))))
         (stars (make-string level ?*))
         (tag-string (if tags
                         (format " :%s:" (string-join tags ":"))
                       "")))

    ;; Insert heading
    (insert (format "%s %s %s%s\n" stars keyword description tag-string))

    ;; Insert properties
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: %s\n" task-id))
    (insert ":ORG_GTD: Actions\n")

    (when depends-on
      (let ((deps (if (listp depends-on) depends-on (list depends-on))))
        (insert (format ":ORG_GTD_DEPENDS_ON: %s\n" (string-join deps " ")))))

    (when blocks
      (let ((blks (if (listp blocks) blocks (list blocks))))
        (insert (format ":ORG_GTD_BLOCKS: %s\n" (string-join blks " ")))))

    (when delegated-to
      (insert (format ":DELEGATED_TO: %s\n" delegated-to)))

    (when timestamp
      (insert (format ":ORG_GTD_TIMESTAMP: %s\n" timestamp)))

    (when project-name
      (insert (format ":ORG_GTD_PROJECT: %s\n" project-name))
      (insert ":TRIGGER: org-gtd-update-project-after-task-done!\n"))

    (when project-ids
      (let ((proj-ids (if (listp project-ids) project-ids (list project-ids))))
        (insert (format ":ORG_GTD_PROJECT_IDS: %s\n" (string-join proj-ids " ")))))

    ;; Additional properties
    (when properties
      (dolist (prop properties)
        (insert (format ":%s: %s\n" (car prop) (cdr prop)))))

    (insert ":END:\n")

    ;; Return marker to task heading
    (save-excursion
      (forward-line -1)
      (while (not (looking-at "^\\*"))
        (forward-line -1))
      (point-marker))))

;;;; Project Builder

(cl-defun make-project (name &key
                             id
                             (status nil)
                             tasks
                             first-tasks
                             (level 1)
                             tags
                             properties)
  "Build a test project with NAME and options.

OPTIONS (keyword arguments):
  :id          - Project ID (auto-generated if not provided)
  :status      - Progress cookie status (e.g., \"[2/3]\")
  :tasks       - List of task descriptions (strings) or task specs (plists)
  :first-tasks - List of root task IDs (auto-detected if not provided)
  :level       - Heading level (default 1 for top-level projects)
  :tags        - List of tags
  :properties  - Additional properties as alist

TASKS can be:
  - Simple strings: '(\"Task 1\" \"Task 2\" \"Task 3\")
  - Task specs (plists): '((:description \"Task 1\" :id \"t1\" :status done)
                           (:description \"Task 2\" :depends-on (\"t1\")))

Returns a plist with:
  :marker       - Marker to project heading
  :id           - Project ID
  :task-ids     - List of task IDs created
  :task-markers - List of markers to created tasks"
  (let* ((project-id (or id (ogt-builder--generate-id "project")))
         (stars (make-string level ?*))
         (tag-string (if tags
                         (format " :%s:" (string-join tags ":"))
                       ""))
         (heading (if status
                      (format "%s %s %s%s\n" stars status name tag-string)
                    (format "%s %s%s\n" stars name tag-string)))
         (task-ids '())
         (task-markers '())
         (detected-first-tasks '()))

    ;; Insert project heading
    (let ((project-start (point)))
      (insert heading)

      ;; Insert properties
      (insert ":PROPERTIES:\n")
      (insert (format ":ID: %s\n" project-id))
      (insert ":ORG_GTD: Projects\n")

      ;; Additional properties
      (when properties
        (dolist (prop properties)
          (insert (format ":%s: %s\n" (car prop) (cdr prop)))))

      ;; Insert FIRST_TASKS if explicitly provided
      (when first-tasks
        (insert (format ":ORG_GTD_FIRST_TASKS: %s\n" (string-join first-tasks " "))))

      ;; Close properties drawer
      (insert ":END:\n")

      ;; Insert tasks
      (when tasks
        (dolist (task-spec tasks)
          (let* ((task-desc (if (stringp task-spec)
                                task-spec
                              (plist-get task-spec :description)))
                 (task-id (if (stringp task-spec)
                             nil
                           (plist-get task-spec :id)))
                 (task-status (if (stringp task-spec)
                                 'todo
                               (or (plist-get task-spec :status) 'todo)))
                 (task-depends (if (stringp task-spec)
                                  nil
                                (plist-get task-spec :depends-on)))
                 (task-blocks (if (stringp task-spec)
                                 nil
                               (plist-get task-spec :blocks)))
                 (task-tags (if (stringp task-spec)
                               nil
                             (plist-get task-spec :tags)))
                 (task-level (if (stringp task-spec)
                                (1+ level)
                              (or (plist-get task-spec :level) (1+ level))))
                 (task-project-ids (if (stringp task-spec)
                                      nil
                                    (plist-get task-spec :project-ids)))
                 (marker (make-task task-desc
                                   :id task-id
                                   :status task-status
                                   :depends-on task-depends
                                   :blocks task-blocks
                                   :tags task-tags
                                   :level task-level
                                   :project-name name
                                   :project-ids (or task-project-ids (list project-id)))))

            (push (org-entry-get marker "ID") task-ids)
            (push marker task-markers)

            ;; Track potential first tasks (no dependencies)
            (when (not task-depends)
              (push (org-entry-get marker "ID") detected-first-tasks)))))

      ;; If first-tasks wasn't provided but we have tasks, add auto-detected first tasks
      (when (and (not first-tasks) detected-first-tasks)
        (goto-char project-start)
        (search-forward ":END:")
        (forward-line 0)  ; Go to beginning of line
        (insert (format ":ORG_GTD_FIRST_TASKS: %s\n" (string-join (nreverse detected-first-tasks) " "))))

      ;; Return project info
      (list :marker (copy-marker project-start)
            :id project-id
            :task-ids (nreverse task-ids)
            :task-markers (nreverse task-markers)))))

;;;; Sequential Project Builder

(cl-defun make-sequential-project (name &key
                                        id
                                        tasks
                                        (level 1)
                                        tags)
  "Build a project with TASKS in sequential dependency order.

Each task automatically blocks the next task in the list, creating
a linear dependency chain.

OPTIONS (keyword arguments):
  :id    - Project ID (auto-generated if not provided)
  :tasks - List of task descriptions (required)
  :level - Heading level (default 1)
  :tags  - List of tags

TASKS must be a list of strings representing task descriptions.

Returns same plist as make-project."
  (unless tasks
    (error "make-sequential-project requires :tasks argument"))

  (let* ((task-specs '())
         (prev-id nil))

    ;; Build task specs with sequential dependencies
    (dolist (task-desc tasks)
      (let* ((task-id (ogt-builder--generate-id "seq-task"))
             (task-spec `(:description ,task-desc
                                       :id ,task-id)))
        (when prev-id
          (setq task-spec (plist-put task-spec :depends-on (list prev-id))))
        (push task-spec task-specs)
        (setq prev-id task-id)))

    ;; Create project with sequential task specs
    (make-project name
                  :id id
                  :tasks (nreverse task-specs)
                  :level level
                  :tags tags)))

;;;; DAG Project Builder

(cl-defun make-dag-project (name &key
                                  id
                                  task-specs
                                  (level 1)
                                  tags)
  "Build a project with TASK-SPECS forming a directed acyclic graph (DAG).

OPTIONS (keyword arguments):
  :id         - Project ID (auto-generated if not provided)
  :task-specs - List of task specifications with dependencies (required)
  :level      - Heading level (default 1)
  :tags       - List of tags

TASK-SPECS format: Each spec is a list starting with description, followed by plist:
  '((\"Task A\" :id \"a\" :blocks (\"b\" \"d\"))
    (\"Task B\" :id \"b\" :depends-on (\"a\") :blocks (\"c\"))
    (\"Task C\" :id \"c\" :depends-on (\"b\"))
    (\"Task D\" :id \"d\" :depends-on (\"a\")))

Returns same plist as make-project."
  (unless task-specs
    (error "make-dag-project requires :task-specs argument"))

  (let ((formatted-specs '()))
    ;; Convert task-specs to plist format
    ;; Handle two formats:
    ;; 1. Old format: '(("Task A" :id "a" :blocks ("b"))...)
    ;; 2. New format: '((:description "Task A" :id "a" :blocks ("b"))...)
    (dolist (spec task-specs)
      (let* ((is-plist (keywordp (car spec)))
             (desc (if is-plist
                       (plist-get spec :description)
                     (car spec)))
             (plist (if is-plist
                       spec
                     (cdr spec)))
             (task-spec `(:description ,desc)))
        ;; Copy over all plist properties (except :description if already present)
        (while plist
          (let ((key (pop plist))
                (val (pop plist)))
            (unless (eq key :description)
              (setq task-spec (plist-put task-spec key val)))))
        (push task-spec formatted-specs)))

    (make-project name
                  :id id
                  :tasks (nreverse formatted-specs)
                  :level level
                  :tags tags)))

;;;; Completed Project Builder

(cl-defun make-completed-project (name &key
                                        id
                                        tasks
                                        (level 1)
                                        tags)
  "Build a project where all TASKS are marked as DONE.

OPTIONS: Same as make-project, but all tasks default to :status 'done.

Returns same plist as make-project."
  (let ((done-tasks (mapcar (lambda (task)
                              (if (stringp task)
                                  `(:description ,task :status done)
                                (plist-put task :status 'done)))
                            tasks)))
    (make-project name
                  :id id
                  :tasks done-tasks
                  :level level
                  :tags tags)))

;;;; Canceled Project Builder

(cl-defun make-canceled-project (name &key
                                       id
                                       tasks
                                       (level 1)
                                       tags)
  "Build a project where all TASKS are marked as CNCL (canceled).

OPTIONS: Same as make-project, but all tasks default to :status 'cncl.

Returns same plist as make-project."
  (let ((canceled-tasks (mapcar (lambda (task)
                                  (if (stringp task)
                                      `(:description ,task :status cncl)
                                    (plist-put task :status 'cncl)))
                                tasks)))
    (make-project name
                  :id id
                  :tasks canceled-tasks
                  :level level
                  :tags tags)))

(provide 'org-gtd-test-helper-builders)

;;; builders.el ends here
