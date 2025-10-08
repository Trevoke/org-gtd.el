;;; assertions.el --- Domain-focused assertion helpers for org-gtd tests -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Domain assertion helpers that express GTD concepts rather than
;; org-mode implementation details.
;;
;; These helpers make tests more readable by hiding low-level buffer
;; manipulation and property checking behind domain-focused functions.
;;
;; Usage:
;;   (require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
;;
;;   (expect (inbox-contains? "Buy groceries") :to-be t)
;;   (expect (task-type (current-task)) :to-equal 'single-action)
;;   (expect (agenda-contains? "Review document") :to-be t)

;;; Code:

(require 'org-gtd)
(require 'org-gtd-files)

;;;; Inbox Assertions

(defun inbox-contains? (description)
  "Check if inbox contains item with DESCRIPTION.
Returns non-nil if the inbox file contains the text DESCRIPTION."
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    (save-excursion
      (goto-char (point-min))
      (search-forward description nil t))))

(defun inbox-empty? ()
  "Check if inbox is empty.
Returns t if the inbox file has no content."
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    (= (buffer-size) 0)))

(defun inbox-count ()
  "Get number of items in inbox.
Counts top-level org headings in the inbox file."
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (hl)
        (when (= (org-element-property :level hl) 1)
          hl))
      nil nil nil)))

;;;; File Assertions

(defun file-contains? (file text)
  "Check if FILE contains TEXT.
FILE can be a file path or a buffer."
  (let ((buf (if (bufferp file)
                 file
               (find-file-noselect file))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (search-forward text nil t)))))

(defun file-empty? (file)
  "Check if FILE is empty.
FILE can be a file path or a buffer."
  (let ((buf (if (bufferp file)
                 file
               (find-file-noselect file))))
    (with-current-buffer buf
      (= (buffer-size) 0))))

(defun file-raw-text (file)
  "Get raw text content from FILE without text properties.
FILE can be a file path or a buffer."
  (let ((buf (if (bufferp file)
                 file
               (find-file-noselect file))))
    (with-current-buffer buf
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;; Task Assertions

(defun task-exists? (description)
  "Check if task with DESCRIPTION exists in the main GTD file.
Searches for DESCRIPTION in the org-gtd tasks file."
  (with-current-buffer (org-gtd--default-file)
    (save-excursion
      (goto-char (point-min))
      (search-forward description nil t))))

(defun task-type (task-marker-or-point)
  "Get the GTD type of task at TASK-MARKER-OR-POINT.
Returns a symbol representing the task type:
  - 'single-action
  - 'project
  - 'delegated
  - 'calendar
  - 'incubated
  - 'habit
  - 'knowledge
  - nil if not categorized

TASK-MARKER-OR-POINT can be a marker, point, or nil (uses current point)."
  (org-with-point-at (or task-marker-or-point (point))
    (let ((org-gtd-value (org-entry-get (point) "ORG_GTD")))
      (cond
       ((null org-gtd-value) nil)
       ((string= org-gtd-value "Actions")
        (cond
         ((org-entry-get (point) "DELEGATED_TO") 'delegated)
         ((org-entry-get (point) "STYLE") 'habit)
         (t 'single-action)))
       ((string= org-gtd-value "Projects") 'project)
       ((string= org-gtd-value "Calendar") 'calendar)
       ((string= org-gtd-value "Incubated") 'incubated)
       ((string= org-gtd-value "Knowledge") 'knowledge)
       (t nil)))))

(defun task-status (task-marker-or-point)
  "Get TODO keyword of task at TASK-MARKER-OR-POINT.
Returns the TODO keyword as a string (e.g., \"TODO\", \"NEXT\", \"DONE\")
or nil if the heading has no TODO keyword."
  (org-with-point-at (or task-marker-or-point (point))
    (org-get-todo-state)))

(defun task-delegated-to (task-marker-or-point)
  "Get the person TASK-MARKER-OR-POINT is delegated to.
Returns the value of the DELEGATED_TO property, or nil if not delegated."
  (org-with-point-at (or task-marker-or-point (point))
    (org-entry-get (point) "DELEGATED_TO")))

(defun task-has-timestamp? (task-marker-or-point)
  "Check if task at TASK-MARKER-OR-POINT has a timestamp.
Returns non-nil if the ORG_GTD_TIMESTAMP property is set."
  (org-with-point-at (or task-marker-or-point (point))
    (let ((timestamp (org-entry-get (point) "ORG_GTD_TIMESTAMP")))
      (and timestamp (not (string-empty-p timestamp))))))

(defun task-timestamp (task-marker-or-point)
  "Get the timestamp value for task at TASK-MARKER-OR-POINT.
Returns the ORG_GTD_TIMESTAMP property value, or nil if not set."
  (org-with-point-at (or task-marker-or-point (point))
    (org-entry-get (point) "ORG_GTD_TIMESTAMP")))

(defun task-has-id? (task-description-or-marker)
  "Check if task has an ID property.
TASK-DESCRIPTION-OR-MARKER can be:
  - A string (task description): searches for task, then checks for ID
  - A marker or point: checks for ID at that location

Returns non-nil if the task has an ID property."
  (if (stringp task-description-or-marker)
      ;; Search for task by description
      (with-current-buffer (org-gtd--default-file)
        (save-excursion
          (goto-char (point-min))
          (when (search-forward task-description-or-marker nil t)
            (org-entry-get (point) "ID"))))
    ;; Use marker/point directly
    (org-with-point-at task-description-or-marker
      (org-entry-get (point) "ID"))))

(defun task-id (task-marker-or-point)
  "Get the ID property of task at TASK-MARKER-OR-POINT.
Returns the ID property value, or nil if not set."
  (org-with-point-at (or task-marker-or-point (point))
    (org-entry-get (point) "ID")))

(defun current-task ()
  "Get the current task as a marker.
Returns a marker pointing to the current org heading.
This is useful for passing to other task assertion functions."
  (point-marker))

(defun task-property (task-marker-or-point property)
  "Get PROPERTY value for task at TASK-MARKER-OR-POINT.
PROPERTY is a property name string (e.g., \"ORG_GTD\", \"DELEGATED_TO\").
Returns the property value, or nil if not set."
  (org-with-point-at (or task-marker-or-point (point))
    (org-entry-get (point) property)))

;;;; Project Assertions

(defun project-exists? (name)
  "Check if a project with NAME exists.
Searches for a heading with NAME that has ORG_GTD property set to 'Projects'."
  (with-current-buffer (org-gtd--default-file)
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (search-forward name nil t)
          (when (and (org-at-heading-p)
                     (string= (org-entry-get (point) "ORG_GTD") "Projects"))
            (throw 'found t)))
        nil))))

(defun project-complete-p (project-marker-or-name)
  "Check if project is complete (all tasks done).
PROJECT-MARKER-OR-NAME can be:
  - A marker or point: checks project at that location
  - A string: searches for project by name

Returns t if all tasks in the project are marked DONE or CNCL."
  (let ((marker (if (stringp project-marker-or-name)
                    (with-current-buffer (org-gtd--default-file)
                      (save-excursion
                        (goto-char (point-min))
                        (when (search-forward project-marker-or-name nil t)
                          (point-marker))))
                  project-marker-or-name)))
    (when marker
      (org-with-point-at marker
        (org-gtd--all-project-tasks-done-p)))))

(defun project-stuck-p (project-marker-or-name)
  "Check if project is stuck (no NEXT actions available).
PROJECT-MARKER-OR-NAME can be a marker/point or project name string.

Returns t if the project has no tasks with TODO keyword NEXT."
  (let ((marker (if (stringp project-marker-or-name)
                    (with-current-buffer (org-gtd--default-file)
                      (save-excursion
                        (goto-char (point-min))
                        (when (search-forward project-marker-or-name nil t)
                          (point-marker))))
                  project-marker-or-name)))
    (when marker
      (org-with-point-at marker
        (let ((tasks (org-gtd-projects--collect-tasks-by-graph marker)))
          (not (seq-some (lambda (task-marker)
                          (org-with-point-at task-marker
                            (string= (org-get-todo-state) "NEXT")))
                        tasks)))))))

(defun project-task-count (project-marker-or-name)
  "Get the number of tasks in a project.
PROJECT-MARKER-OR-NAME can be a marker/point or project name string.

Returns the count of tasks in the project's task graph."
  (let ((marker (if (stringp project-marker-or-name)
                    (with-current-buffer (org-gtd--default-file)
                      (save-excursion
                        (goto-char (point-min))
                        (when (search-forward project-marker-or-name nil t)
                          (point-marker))))
                  project-marker-or-name)))
    (when marker
      (org-with-point-at marker
        (length (org-gtd-projects--collect-tasks-by-graph marker))))))

(defun project-has-tasks? (project-marker-or-name)
  "Check if project has any tasks.
PROJECT-MARKER-OR-NAME can be a marker/point or project name string.

Returns t if the project has at least one task in its graph."
  (let ((task-count (project-task-count project-marker-or-name)))
    (and task-count (> task-count 0))))

;;;; Agenda Assertions

(defun agenda-contains? (text)
  "Check if the current agenda buffer contains TEXT.
Returns non-nil if TEXT is found in the org-agenda-buffer."
  (when (and (boundp 'org-agenda-buffer)
             org-agenda-buffer
             (buffer-live-p org-agenda-buffer))
    (with-current-buffer org-agenda-buffer
      (save-excursion
        (goto-char (point-min))
        (search-forward text nil t)))))

(defun agenda-empty? ()
  "Check if the agenda is empty.
Returns t if the org-agenda-buffer has no content."
  (if (and (boundp 'org-agenda-buffer)
           org-agenda-buffer
           (buffer-live-p org-agenda-buffer))
      (with-current-buffer org-agenda-buffer
        (= (buffer-size) 0))
    t))

(defun agenda-raw-text ()
  "Get the raw text content of the agenda buffer.
Returns the agenda buffer content without text properties,
or empty string if agenda buffer doesn't exist."
  (if (and (boundp 'org-agenda-buffer)
           org-agenda-buffer
           (buffer-live-p org-agenda-buffer))
      (with-current-buffer org-agenda-buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

;;;; Archive Assertions

(defun archived? (item-description-or-marker)
  "Check if an item is in the archive.
ITEM-DESCRIPTION-OR-MARKER can be:
  - A string: searches for item in archive by description
  - A marker: not supported (item must be in archive, not active file)

Returns non-nil if the item is found in the archive file."
  (let ((archive-buffer (ogt--archive)))
    (when archive-buffer
      (with-current-buffer archive-buffer
        (save-excursion
          (goto-char (point-min))
          (search-forward (if (stringp item-description-or-marker)
                             item-description-or-marker
                           (org-with-point-at item-description-or-marker
                             (org-get-heading t t t t)))
                         nil t))))))

(defun archive-contains? (text)
  "Check if the archive contains TEXT.
Returns non-nil if TEXT is found in the archive file."
  (let ((archive-buffer (ogt--archive)))
    (when archive-buffer
      (with-current-buffer archive-buffer
        (save-excursion
          (goto-char (point-min))
          (search-forward text nil t))))))

(defun archive-raw-text ()
  "Get the raw text content of the archive file.
Returns the archive file content without text properties."
  (let ((archive-buffer (ogt--archive)))
    (if archive-buffer
        (with-current-buffer archive-buffer
          (buffer-substring-no-properties (point-min) (point-max)))
      "")))

;;;; Current Buffer Convenience Functions
;; These functions operate on the current buffer, making it easier to write
;; assertions without explicitly passing (current-buffer) every time.

(defun current-buffer-contains? (text)
  "Check if current buffer contains TEXT.
Returns non-nil if TEXT is found in the current buffer."
  (file-contains? (current-buffer) text))

(defun current-buffer-empty? ()
  "Check if current buffer is empty.
Returns t if the current buffer has no content."
  (file-empty? (current-buffer)))

(defun current-buffer-raw-text ()
  "Get raw text content from current buffer without text properties.
Returns the current buffer content as a string."
  (file-raw-text (current-buffer)))

;;;; Helper function from utils.el

(defun ogt--archive ()
  "Create or return the buffer to the archive file."
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    (find-file-noselect
     (car (with-org-gtd-context
              (org-archive--compute-location
               (funcall org-gtd-archive-location)))))))

;;; Provide feature

(provide 'ogt-assertions)
;;; assertions.el ends here
