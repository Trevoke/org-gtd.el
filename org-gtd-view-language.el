;;; org-gtd-view-language.el --- GTD view specification language -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This module provides a declarative language for specifying GTD views
;; that translate to native org-agenda blocks.
;;
;; GTD View Language Specification:
;;
;; A GTD view is an alist with a name and filter keys:
;;
;;   '((name . "View Name")
;;     (type . delegated)
;;     (when . past))
;;
;; GTD Type Filters:
;;   (type . next-action)       - Single actions ready to do
;;   (type . delegated)         - Items delegated to others
;;   (type . calendar)          - Time-specific appointments
;;   (type . tickler)           - Time-based reminders
;;   (type . someday)           - Someday/Maybe items
;;   (type . project)           - Multi-step outcomes
;;   (type . habit)             - Recurring routines
;;   (type . reference)         - Reference/knowledge items
;;   (type . trash)             - Discarded items
;;   (type . quick-action)      - 2-minute actions
;;
;; Computed Types (special state queries):
;;   (type . stuck-project)     - Projects with no NEXT/WAIT tasks
;;   (type . active-project)    - Projects with active tasks
;;   (type . completed-project) - Projects with all tasks done
;;   (type . tickler-project)   - Projects moved to tickler
;;   (type . incubated-project) - Projects in Tickler or Someday/Maybe
;;   (type . stuck-delegated)   - Delegated items missing timestamp or who
;;   (type . stuck-calendar)    - Calendar items missing timestamp
;;   (type . stuck-tickler)     - Tickler items missing timestamp
;;   (type . stuck-habit)       - Habit items missing timestamp
;;
;; Time Filters (semantic - resolved based on type):
;;   (when . past)              - Type's timestamp in the past
;;   (when . future)            - Type's timestamp in the future
;;   (when . today)             - Type's timestamp is today
;;   (deadline . past)          - Deadline in the past
;;   (deadline . future)        - Deadline in the future
;;   (deadline . today)         - Deadline is today
;;   (scheduled . past)         - Scheduled in the past
;;   (scheduled . future)       - Scheduled in the future
;;   (scheduled . today)        - Scheduled for today
;;
;; Structural Filters:
;;   (todo . ("TODO" "NEXT"))   - Specific TODO keywords
;;   (done . t)                 - Any completed item
;;   (done . recent)            - Completed in last 7 days
;;   (done . today)             - Completed today
;;   (done . past-day)          - Completed in last day
;;   (done . past-week)         - Completed in last week
;;   (done . past-month)        - Completed in last month
;;   (done . past-year)         - Completed in last year
;;   (not-done . t)             - Incomplete items
;;
;; Metadata Filters:
;;   (area-of-focus . "Work")   - Specific area of focus
;;   (who . "Alice")            - Delegated to specific person
;;   (who . nil)                - Missing delegation recipient
;;
;; Clock Time Filters:
;;   (last-clocked-out . (> "2d"))  - Not worked on in 2+ days
;;   (last-clocked-out . (< "1w"))  - Worked on within past week
;;   (last-clocked-out . nil)       - Never clocked
;;
;; Tag Filters:
;;   (tags . ("@work"))         - Match specific tags
;;
;; Native Blocks (Escape Hatch):
;;
;; For features not yet abstracted by the DSL, you can embed raw
;; org-agenda blocks using the `native' keyword:
;;
;;   ((native . (BLOCK-TYPE MATCH-STRING SETTINGS)))
;;
;; The value is passed directly to org-agenda without transformation.
;; This is useful for:
;; - Custom sorting: ((org-agenda-sorting-strategy '(priority-down)))
;; - Advanced skip functions
;; - Any org-agenda settings the DSL doesn't abstract
;;
;; Native blocks can be mixed with DSL blocks:
;;
;;   '((blocks . (((type . next-action))                  ; DSL
;;                ((native . (tags-todo "+urgent"         ; Native
;;                            ((org-agenda-sorting-strategy '(effort-up)))))))))
;;
;; Examples:
;;
;;   ;; All next actions
;;   '((name . "Next Actions")
;;     (type . next-action))
;;
;;   ;; Overdue delegated items
;;   '((name . "Overdue Delegated")
;;     (type . delegated)
;;     (when . past))
;;
;;   ;; Overdue work projects
;;   '((name . "Overdue Work")
;;     (type . project)
;;     (deadline . past)
;;     (area-of-focus . "Work"))
;;
;;   ;; Stuck delegated items (missing timestamp or who)
;;   '((name . "Stuck Delegated")
;;     (type . stuck-delegated))
;;
;;; Code:
(require 'org-gtd-core)
(require 'org-gtd-skip)
(require 'org-gtd-types)
(require 'org-gtd-agenda)

;; Note: org-gtd-prefix-width is defined in org-gtd-core.el
;; and serves as the default width for prefix elements

(defconst org-gtd-view-lang--known-filter-keys
  '(name type when deadline scheduled todo done not-done
    area-of-focus who tags priority effort clocked last-clocked-out
    blocks prefix prefix-width view-type agenda-span show-habits
    additional-blocks filters not-habit property
    block-type group-contexts group-by todo-keyword prefix-format
    native)
  "Known filter keys in view specs.
Includes both filter keys and reserved structural keys.")

(defconst org-gtd-view-lang--type-defaults
  '((calendar . ((when . today) (name . "Calendar")))
    (delegated . ((when . today) (name . "Delegated")))
    (tickler . ((when . today) (name . "Tickler")))
    (habit . ((name . "Habits")))
    (next-action . ((name . "Next Actions")))
    (project . ((name . "Projects")))
    (someday . ((name . "Someday/Maybe")))
    (stuck-calendar . ((name . "Calendar (Needs Attention)")))
    (stuck-delegated . ((name . "Delegated (Needs Attention)")))
    (stuck-habit . ((name . "Habits (Needs Attention)")))
    (stuck-tickler . ((name . "Tickler (Needs Attention)")))
    (stuck-project . ((name . "Projects (Needs Attention)")))
    (stuck-single-action . ((name . "Single Actions (Needs Attention)")))
    (tickler-project . ((name . "Tickler Projects")))
    (completed-project . ((name . "Completed Projects"))))
  "Smart defaults for each GTD type.
Each entry maps a type symbol to an alist of default values.
Types with time-sensitive semantics (calendar, delegated, tickler)
default to `when' of `today'.")

(defconst org-gtd-view-lang--default-prefix
  '(project area-of-focus file-name)
  "Default prefix fallback chain when none specified.
Elements are tried in order: project name, then area of focus,
then file name.")

(defconst org-gtd-view-lang--simple-types
  '(next-action delegated calendar tickler project someday habit reference trash quick-action)
  "List of simple GTD types that can be handled by native agenda blocks.
These types have straightforward ORG_GTD property matches.")

(defconst org-gtd-view-lang--complex-types
  '(stuck-project active-project completed-project
    tickler-project incubated-project
    stuck-delegated stuck-calendar stuck-tickler stuck-habit
    stuck-single-action)
  "List of complex GTD types that require special predicate handling.
These are computed types that need additional logic beyond property matches.")

(defun org-gtd-view-lang--simple-type-p (type-name)
  "Return non-nil if TYPE-NAME is a simple type for native handling."
  (memq type-name org-gtd-view-lang--simple-types))

(defun org-gtd-view-lang--complex-type-p (type-name)
  "Return non-nil if TYPE-NAME is a complex type for native handling."
  (memq type-name org-gtd-view-lang--complex-types))

(defun org-gtd-view-lang--native-type-p (type-name)
  "Return non-nil if TYPE-NAME can be handled by native agenda blocks."
  (or (org-gtd-view-lang--simple-type-p type-name)
      (org-gtd-view-lang--complex-type-p type-name)))

(defun org-gtd-view-lang--create-agenda-block (gtd-view-spec &optional inherited-prefix-format)
  "Create an agenda block from GTD-VIEW-SPEC.
If block-type is \\='calendar-day, creates a native agenda filtered to
Calendar/Habit.
If block-type is \\='todo, creates a native todo block.
If view-type is \\='agenda, creates a native agenda block.
If view-type is \\='tags-grouped, creates grouped views.
If type filter is present, creates a native tags-todo block.
If done filter is present, creates a native tags block for completed items.
INHERITED-PREFIX-FORMAT is optionally passed from parent view spec."
  ;; Check for native escape hatch first
  (let ((native-block (alist-get 'native gtd-view-spec)))
    (if native-block
        ;; Pass through the raw org-agenda block tuple unchanged
        native-block
      ;; Existing DSL processing (wrap existing body in this else branch)
      (let* ((name (alist-get 'name gtd-view-spec))
             (block-type (alist-get 'block-type gtd-view-spec))
             (view-type (alist-get 'view-type gtd-view-spec))
             (type-filter (alist-get 'type gtd-view-spec))
             (done-filter (alist-get 'done gtd-view-spec))
             ;; Use new prefix DSL or fall back to inherited format
             (prefix-format (org-gtd-view-lang--get-prefix-format gtd-view-spec inherited-prefix-format)))
        (cond
         ((eq block-type 'calendar-day)
          (org-gtd-view-lang--create-calendar-day-block gtd-view-spec))
         ((eq block-type 'todo)
          (org-gtd-view-lang--create-todo-block gtd-view-spec prefix-format))
         ((eq view-type 'agenda)
          (org-gtd-view-lang--create-native-agenda-block gtd-view-spec))
         ((eq view-type 'tags-grouped)
          (org-gtd-view-lang--create-grouped-views gtd-view-spec))
         ;; Route type filters to native blocks (both simple and complex)
         ((and type-filter (org-gtd-view-lang--native-type-p type-filter))
          (org-gtd-view-lang--translate-to-native-block gtd-view-spec prefix-format))
         ;; Route done filters to native blocks
         (done-filter
          (org-gtd-view-lang--create-done-filter-block gtd-view-spec prefix-format))
         (t
          (user-error "Unsupported view spec: %S" gtd-view-spec)))))))

(defun org-gtd-view-lang--skip-unless-calendar-or-habit ()
  "Skip function to filter agenda to only Calendar and Habit items.
Also skips items that are done or cancelled.
Returns nil to include item, or end of entry point to skip."
  (let ((org-gtd-value (org-entry-get (point) "ORG_GTD")))
    (if (and (or (equal org-gtd-value (org-gtd-type-org-gtd-value 'calendar))
                 (equal org-gtd-value (org-gtd-type-org-gtd-value 'habit)))
             (not (org-entry-is-done-p)))
        nil
      (org-entry-end-position))))

(defun org-gtd-view-lang--create-calendar-day-block (_gtd-view-spec)
  "Create a calendar-day agenda block from GTD-VIEW-SPEC.
This is a native org-agenda day view filtered to show only Calendar and
Habit items."
  (let ((settings '((org-agenda-span 1)
                    (org-agenda-start-day nil)
                    (org-agenda-skip-additional-timestamps-same-entry t)
                    (org-agenda-skip-function 'org-gtd-view-lang--skip-unless-calendar-or-habit))))
    `(agenda "" ,settings)))

(defun org-gtd-view-lang--create-todo-block (gtd-view-spec prefix-format)
  "Create a native todo block from GTD-VIEW-SPEC with PREFIX-FORMAT."
  (let* ((name (alist-get 'name gtd-view-spec))
         (todo-keyword (alist-get 'todo-keyword gtd-view-spec))
         (settings `((org-agenda-overriding-header ,name))))
    (when prefix-format
      (push `(org-agenda-prefix-format '((todo . ,prefix-format))) settings))
    `(todo ,todo-keyword ,settings)))

(defun org-gtd-view-lang--create-native-agenda-block (gtd-view-spec)
  "Create a native agenda block from GTD-VIEW-SPEC."
  (let* ((agenda-span (alist-get 'agenda-span gtd-view-spec 1))
         (show-habits (alist-get 'show-habits gtd-view-spec))
         (settings '((org-agenda-start-day nil)
                    (org-agenda-skip-additional-timestamps-same-entry t)
                    (org-agenda-skip-function 'org-gtd-skip-unless-in-progress))))
    (when agenda-span
      (push `(org-agenda-span ,agenda-span) settings))
    (when (and (assoc 'show-habits gtd-view-spec) (not show-habits))
      (push '(org-agenda-include-all-todo nil) settings))
    `(agenda "" ,settings)))

(defun org-gtd-view-lang--create-additional-blocks (gtd-view-spec)
  "Create additional agenda blocks from GTD-VIEW-SPEC.
Returns a list of additional blocks like TODO lists."
  (let ((additional-blocks (alist-get 'additional-blocks gtd-view-spec))
        (prefix-format (org-gtd-view-lang--get-prefix-format gtd-view-spec)))
    (mapcar (lambda (block-spec)
              (let ((block-type (car block-spec))
                    (block-value (cdr block-spec)))
                (cond
                 ((eq block-type 'todo)
                  (let ((settings '((org-agenda-overriding-header "All actions ready to be executed."))))
                    (when prefix-format
                      (push `(org-agenda-prefix-format '((todo . ,prefix-format))) settings))
                    `(todo ,block-value ,settings)))
                 (t (error "Unknown additional block type: %s" block-type)))))
            additional-blocks)))

(defun org-gtd-view-lang--create-custom-commands (view-specs &optional key title)
  "Create `org-agenda-custom-commands' from VIEW-SPECS list.
KEY defaults to \"o\", TITLE defaults to \"GTD Views\".
Supports both single-block and multi-block view specs.
Multi-block specs have a \\='blocks key containing a list of block specs."
  (let* ((command-key (or key "o"))
         (command-title (or title "GTD Views"))
         (blocks (mapcan (lambda (view-spec)
                           (let ((blocks-list (alist-get 'blocks view-spec))
                                 ;; Get parent's prefix format (from new DSL or nil)
                                 (parent-prefix-format (org-gtd-view-lang--get-prefix-format view-spec)))
                             (if blocks-list
                                 ;; Multi-block spec: process each block, passing parent prefix
                                 (mapcar (lambda (block)
                                           (org-gtd-view-lang--create-agenda-block block parent-prefix-format))
                                         blocks-list)
                               ;; Single-block spec: process as-is
                               (let ((main-block (org-gtd-view-lang--create-agenda-block view-spec))
                                     (additional-blocks (org-gtd-view-lang--create-additional-blocks view-spec)))
                                 (if additional-blocks
                                     (cons main-block additional-blocks)
                                   (list main-block))))))
                         view-specs)))
    `((,command-key ,command-title ,blocks))))






















(defun org-gtd-view-lang--create-grouped-views (gtd-view-spec)
  "Create grouped views from GTD-VIEW-SPEC.
Handle both simple grouped views with \\='group-contexts and
dynamic grouped views with \\='group-by."
  (let ((group-contexts (alist-get 'group-contexts gtd-view-spec))
        (group-by (alist-get 'group-by gtd-view-spec))
        (filters (alist-get 'filters gtd-view-spec)))
    (cond
     ;; Simple grouped views with pre-defined contexts
     (group-contexts
      (org-gtd-view-lang--create-simple-grouped-views group-contexts filters))
     ;; Dynamic grouped views by context
     ((eq group-by 'context)
      (org-gtd-view-lang--create-context-grouped-views filters))
     (t (error "Unknown grouping specification in view spec")))))

(defun org-gtd-view-lang--create-simple-grouped-views (contexts filters)
  "Create simple grouped views for each context in CONTEXTS with FILTERS."
  (mapcar (lambda (context)
            (let ((search-string (org-gtd-view-lang--build-tags-search-string context filters)))
              `(tags ,search-string
                     ((org-agenda-overriding-header ,context)))))
          contexts))

(defun org-gtd-view-lang--extract-contexts-from-agenda (filters)
  "Extract all context tags (prefixed with @) from agenda files based on FILTERS."
  (let ((todo-filter (seq-find (lambda (f) (eq (car f) 'todo)) filters)))
    (when todo-filter
      (let ((todo-keyword (let ((value (cdr todo-filter)))
                            (if (listp value) (car value) value))))
        (seq-map
         (lambda (x) (substring-no-properties x))
         (seq-uniq
          (flatten-list
           (org-map-entries
            (lambda () org-scanner-tags)
            (format "{^@}+TODO=\"%s\"" todo-keyword)
            'agenda))))))))

(defun org-gtd-view-lang--create-context-grouped-views (filters)
  "Create context-grouped views with dynamic context detection from FILTERS."
  (let ((contexts (org-gtd-view-lang--extract-contexts-from-agenda filters)))
    (mapcar (lambda (context)
              (let ((search-string (org-gtd-view-lang--build-tags-search-string context filters)))
                `(,context . ((tags ,search-string
                                    ((org-agenda-overriding-header ,context)))))))
            contexts)))

(defun org-gtd-view-lang--build-tags-search-string (context filters)
  "Build agenda tags search string for CONTEXT with FILTERS."
  (let ((search-parts (list (concat "+" context))))
    (dolist (filter filters)
      (let ((filter-type (car filter))
            (filter-value (cdr filter)))
        (cond
         ((eq filter-type 'todo)
          ;; Handle multiple TODO keywords - for now just take first
          (let ((todo-keyword (if (listp filter-value) (car filter-value) filter-value)))
            (push (concat "+TODO=\"" todo-keyword "\"") search-parts)))
         ;; Add other filter types as needed
         )))
    (string-join (reverse search-parts) "")))

;;;; Native Block Translation

(defun org-gtd-view-lang--complex-type-base-property (type-name)
  "Return the base ORG_GTD property value for complex TYPE-NAME.
Returns nil for types that need OR logic (handled by skip function)."
  (cond
   ;; Project computed types all match ORG_GTD=\"Projects\"
   ((memq type-name '(stuck-project active-project completed-project))
    org-gtd-projects)
   ;; Tickler-project matches ORG_GTD=\"Tickler\"
   ((eq type-name 'tickler-project)
    org-gtd-tickler)
   ;; Incubated-project needs OR (Tickler OR Someday), handled by skip fn
   ((eq type-name 'incubated-project)
    nil)
   ;; Stuck types match their base type's ORG_GTD value
   ((eq type-name 'stuck-delegated)
    (org-gtd-type-org-gtd-value 'delegated))
   ((eq type-name 'stuck-calendar)
    (org-gtd-type-org-gtd-value 'calendar))
   ((eq type-name 'stuck-tickler)
    (org-gtd-type-org-gtd-value 'tickler))
   ((eq type-name 'stuck-habit)
    (org-gtd-type-org-gtd-value 'habit))
   ((eq type-name 'stuck-single-action)
    (org-gtd-type-org-gtd-value 'next-action))
   (t nil)))

(defun org-gtd-view-lang--build-match-string (gtd-view-spec)
  "Build an org-agenda match string from GTD-VIEW-SPEC.
The match string is used with tags agenda blocks.
Returns a string like \"LEVEL>0+ORG_GTD=\\\"Calendar\\\"/TODO=\\\"NEXT\\\"\"."
  (let* ((type-filter (alist-get 'type gtd-view-spec))
         (todo-filter (alist-get 'todo gtd-view-spec))
         ;; For simple types, use org-gtd-type-org-gtd-value
         ;; For complex types, use the base property lookup
         (org-gtd-val (cond
                       ((org-gtd-view-lang--simple-type-p type-filter)
                        (org-gtd-type-org-gtd-value type-filter))
                       ((org-gtd-view-lang--complex-type-p type-filter)
                        (org-gtd-view-lang--complex-type-base-property type-filter))
                       (t nil)))
         (property-part (if org-gtd-val
                           (format "LEVEL>0+ORG_GTD=\"%s\"" org-gtd-val)
                         "LEVEL>0+ORG_GTD<>\"\""))
         (todo-part nil))
    (cond
     ;; Explicit todo filter overrides implicit type-based TODO keyword
     (todo-filter
      nil)  ; Don't add todo-part - let skip function handle it
     ;; Add TODO keyword for types that have implied keywords
     (type-filter
      (cond
       ((eq type-filter 'next-action)
        (setq todo-part (org-gtd-keywords--next)))
       ((eq type-filter 'delegated)
        (setq todo-part (org-gtd-keywords--wait))))))
    ;; Build match string:
    ;; - Use LEVEL>0 as base (matches all headlines)
    ;; - Use ORG_GTD="value" to filter to specific type
    ;; - For types with specific TODO keywords, add /KEYWORD
    ;;   Note: the /KEYWORD syntax matches the TODO state, not a regexp
    (if todo-part
        (concat property-part "/" todo-part)
      property-part)))

(defun org-gtd-view-lang--build-skip-function-for-stuck-type (base-type)
  "Build a skip function for stuck items of BASE-TYPE.
Stuck items are included if they have ANY missing/invalid metadata.
Returns a function suitable for `org-agenda-skip-function'."
  (let ((org-gtd-val (org-gtd-type-org-gtd-value base-type))
        (when-prop (org-gtd-type-property base-type :when))
        (who-prop (org-gtd-type-property base-type :who)))
    (lambda ()
      (let ((end (org-entry-end-position)))
        ;; Must match the base type
        (if (not (equal (org-entry-get (point) "ORG_GTD") org-gtd-val))
            end  ; Skip - wrong type
          ;; Item is stuck if ANY of the following are true:
          ;; - Invalid timestamp (when type has :when property)
          ;; - Missing/empty who (when type has :who property)
          (let ((is-stuck nil))
            (when when-prop
              (let ((ts-value (org-entry-get (point) when-prop)))
                (when (or (not ts-value)
                          (string-empty-p (string-trim ts-value))
                          (not (string-match org-ts-regexp-both ts-value)))
                  (setq is-stuck t))))
            (when (and who-prop (not is-stuck))
              (let ((who-value (org-entry-get (point) who-prop)))
                (when (or (not who-value)
                          (string-empty-p (string-trim who-value)))
                  (setq is-stuck t))))
            (if is-stuck
                nil   ; Include - item is stuck
              end))))))) ; Skip - item is not stuck

(defun org-gtd-view-lang--build-skip-function-for-project-type (project-type)
  "Build a skip function for computed PROJECT-TYPE.
PROJECT-TYPE is one of stuck-project, active-project, completed-project."
  (lambda ()
    (let ((end (org-entry-end-position)))
      ;; Must be a project
      (if (not (equal (org-entry-get (point) "ORG_GTD") org-gtd-projects))
          end  ; Skip - not a project
        (cond
         ((eq project-type 'stuck-project)
          (if (funcall (org-gtd-pred--project-is-stuck))
              nil    ; Include - project is stuck
            end))    ; Skip - project is not stuck
         ((eq project-type 'active-project)
          (if (funcall (org-gtd-pred--project-has-active-tasks))
              nil    ; Include - has active tasks
            end))    ; Skip - no active tasks
         ((eq project-type 'completed-project)
          (if (not (funcall (org-gtd-pred--project-has-active-tasks)))
              nil    ; Include - no active tasks (completed)
            end))    ; Skip - still has active tasks
         (t end))))))

(defun org-gtd-view-lang--build-skip-function-for-tickler-project ()
  "Build a skip function for tickler-project type.
Matches items in Tickler that were previously Projects."
  (lambda ()
    (let ((end (org-entry-end-position)))
      (if (and (equal (org-entry-get (point) "ORG_GTD") org-gtd-tickler)
               (equal (org-entry-get (point) org-gtd-prop-previous-category) org-gtd-projects))
          nil    ; Include - tickler item that was a project
        end))))  ; Skip - not matching

(defun org-gtd-view-lang--build-skip-function-for-incubated-project ()
  "Build a skip function for incubated-project type.
Matches items in Tickler OR Someday that were previously Projects."
  (lambda ()
    (let ((end (org-entry-end-position))
          (org-gtd-val (org-entry-get (point) "ORG_GTD"))
          (prev-val (org-entry-get (point) org-gtd-prop-previous-category)))
      (if (and (or (equal org-gtd-val org-gtd-tickler)
                   (equal org-gtd-val org-gtd-someday))
               (equal prev-val org-gtd-projects))
          nil    ; Include - incubated project
        end))))  ; Skip - not matching

(defun org-gtd-view-lang--build-skip-function-for-stuck-single-action ()
  "Build a skip function for stuck-single-action type.
Matches undone single actions (ORG_GTD=Actions) that are not in NEXT state.
These need attention because single actions should always be in NEXT."
  (let ((actions-val (org-gtd-type-org-gtd-value 'next-action))
        (next-keyword (org-gtd-keywords--next)))
    (lambda ()
      (let ((end (org-entry-end-position)))
        ;; Must be an Actions item
        (if (not (equal (org-entry-get (point) "ORG_GTD") actions-val))
            end  ; Skip - not a single action
          ;; Must not be done
          (if (org-entry-is-done-p)
              end  ; Skip - done
            ;; Is stuck if NOT in NEXT state
            (let ((todo-state (org-entry-get (point) "TODO")))
              (if (equal todo-state next-keyword)
                  end   ; Skip - properly in NEXT state
                nil)))))))) ; Include - stuck (not in NEXT)

(defun org-gtd-view-lang--done-filter-days (done-value)
  "Return the number of days to look back for DONE-VALUE filter.
Returns nil for (done . t) which means any done item."
  (cond
   ((eq done-value t) nil)
   ((numberp done-value) done-value)
   ((eq done-value 'recent) 7)
   ((eq done-value 'today) 0)
   ((eq done-value 'past-day) 1)
   ((eq done-value 'past-week) 7)
   ((eq done-value 'past-month) 30)
   ((eq done-value 'past-year) 365)
   (t nil)))

(defun org-gtd-view-lang--build-skip-function-for-done-filter (done-value)
  "Build a skip function for done filter with DONE-VALUE.
Matches items that are done and closed within the time range."
  (let ((days-back (org-gtd-view-lang--done-filter-days done-value)))
    (lambda ()
      (let ((end (org-entry-end-position)))
        ;; Must have a done TODO state
        (if (not (org-entry-is-done-p))
            end  ; Skip - not done
          ;; If no time constraint, include all done items
          (if (not days-back)
              nil  ; Include - any done item
            ;; Check CLOSED timestamp is within range
            (let ((closed-ts (org-entry-get (point) "CLOSED")))
              (if (not closed-ts)
                  end  ; Skip - no CLOSED timestamp
                (let ((closed-time (org-time-string-to-time closed-ts))
                      (cutoff-time (time-subtract (current-time)
                                                  (days-to-time days-back))))
                  (if (time-less-p cutoff-time closed-time)
                      nil    ; Include - closed within range
                    end))))))))))  ; Skip - closed too long ago

(defun org-gtd-view-lang--create-done-filter-block (gtd-view-spec &optional prefix-format)
  "Create a native block for done filter from GTD-VIEW-SPEC.
Optional PREFIX-FORMAT is applied for display formatting."
  (let* ((name (alist-get 'name gtd-view-spec))
         (done-value (alist-get 'done gtd-view-spec))
         (skip-fn (org-gtd-view-lang--build-skip-function-for-done-filter done-value))
         ;; Match any ORG_GTD item at level > 0
         (match-string "LEVEL>0+ORG_GTD<>\"\"")
         (settings `((org-agenda-overriding-header ,name))))
    ;; Add skip function (quoted for Emacs 29 compatibility - closures aren't self-evaluating)
    (push `(org-agenda-skip-function ',skip-fn) settings)
    ;; Add prefix format if provided
    (when prefix-format
      (push `(org-agenda-prefix-format '((tags . ,prefix-format)
                                          (todo . ,prefix-format))) settings))
    ;; Use 'tags' block since done items match any TODO state in org-done-keywords
    `(tags ,match-string ,settings)))

(defconst org-gtd-view-lang--valid-comparison-ops '(< > =)
  "Valid comparison operators for duration expressions.")

(defconst org-gtd-view-lang--duration-regexp "^[+-]?[0-9]+[mhdwMy]$"
  "Regexp matching valid duration strings.")

(defun org-gtd-view-lang--validate-comparison-expr (expr)
  "Validate comparison expression EXPR like (< \"14d\").
Returns nil if valid, signals user-error if invalid."
  (unless (and (listp expr) (>= (length expr) 2))
    (user-error "Comparison expression must be (OP DURATION), got: %S" expr))
  (let ((op (car expr))
        (duration (cadr expr)))
    (unless (memq op org-gtd-view-lang--valid-comparison-ops)
      (user-error "Invalid comparison operator '%s', must be one of: < > =" op))
    (unless (or (string-equal duration "today")
                (string-match org-gtd-view-lang--duration-regexp duration))
      (user-error "Invalid duration format '%s', expected pattern like '14d', '-7d', '2w'" duration)))
  nil)

(defun org-gtd-view-lang--build-skip-function (gtd-view-spec)
  "Build a skip function from GTD-VIEW-SPEC.
Returns a function suitable for `org-agenda-skip-function'.
The function composes predicates from the view spec filters."
  ;; Validate filter keys
  (let ((unknown-keys (cl-set-difference
                       (mapcar #'car gtd-view-spec)
                       org-gtd-view-lang--known-filter-keys)))
    (when unknown-keys
      (user-error "Unknown filter key(s): %s" unknown-keys)))
  (let* ((type-filter (alist-get 'type gtd-view-spec))
         (when-filter (alist-get 'when gtd-view-spec))
         (area-filter (alist-get 'area-of-focus gtd-view-spec)))
    ;; Validate when filter requirements
    (when when-filter
      (unless type-filter
        (user-error "The 'when' filter requires a 'type' filter"))
      (unless (org-gtd-type-property type-filter :when)
        (user-error "Type '%s' does not support the 'when' filter" type-filter)))
    ;; Handle complex types with specialized skip functions
    (cond
     ;; Stuck types - need OR logic for missing metadata
     ((eq type-filter 'stuck-calendar)
      (org-gtd-view-lang--build-skip-function-for-stuck-type 'calendar))
     ((eq type-filter 'stuck-delegated)
      (org-gtd-view-lang--build-skip-function-for-stuck-type 'delegated))
     ((eq type-filter 'stuck-tickler)
      (org-gtd-view-lang--build-skip-function-for-stuck-type 'tickler))
     ((eq type-filter 'stuck-habit)
      (org-gtd-view-lang--build-skip-function-for-stuck-type 'habit))
     ;; Stuck single actions - not in NEXT state
     ((eq type-filter 'stuck-single-action)
      (org-gtd-view-lang--build-skip-function-for-stuck-single-action))
     ;; Project computed types
     ((memq type-filter '(stuck-project active-project completed-project))
      (org-gtd-view-lang--build-skip-function-for-project-type type-filter))
     ;; Tickler/incubated project types
     ((eq type-filter 'tickler-project)
      (org-gtd-view-lang--build-skip-function-for-tickler-project))
     ((eq type-filter 'incubated-project)
      (org-gtd-view-lang--build-skip-function-for-incubated-project))
     ;; Simple types - use predicate composition
     (t
      (let ((predicates '()))
        ;; Add type predicate
        (when type-filter
          (let ((org-gtd-val (org-gtd-type-org-gtd-value type-filter)))
            (when org-gtd-val
              (push (org-gtd-pred--property-equals "ORG_GTD" org-gtd-val) predicates))))
        ;; Add area-of-focus predicate (uses CATEGORY property)
        (when area-filter
          (push (org-gtd-pred--property-equals org-gtd-prop-area-of-focus area-filter) predicates))
        ;; Add when predicate based on type's semantic property
        (when (and when-filter type-filter)
          (let ((when-prop (org-gtd-type-property type-filter :when)))
            (when when-prop
              (cond
               ((eq when-filter 'past)
                (push (org-gtd-pred--property-ts< when-prop "today") predicates))
               ((eq when-filter 'today)
                (push (org-gtd-pred--property-ts= when-prop "today") predicates))
               ((eq when-filter 'future)
                (push (org-gtd-pred--property-ts> when-prop "today") predicates))))))
        ;; Add priority predicate
        (when-let ((priority-filter (alist-get 'priority gtd-view-spec)))
          (push (org-gtd-pred--priority-matches priority-filter) predicates))
        ;; Handle priority=nil explicitly (not filtered by when-let)
        (when (and (assq 'priority gtd-view-spec)
                   (null (alist-get 'priority gtd-view-spec)))
          (push (org-gtd-pred--priority-matches nil) predicates))
        ;; Add effort predicate
        (when-let ((effort-filter (alist-get 'effort gtd-view-spec)))
          (push (org-gtd-pred--effort-matches effort-filter) predicates))
        ;; Handle effort=nil explicitly
        (when (and (assq 'effort gtd-view-spec)
                   (null (alist-get 'effort gtd-view-spec)))
          (push (org-gtd-pred--effort-matches nil) predicates))
        ;; Add clocked predicate
        (when-let ((clocked-filter (alist-get 'clocked gtd-view-spec)))
          (push (org-gtd-pred--clocked-matches clocked-filter) predicates))
        ;; Handle clocked=nil explicitly
        (when (and (assq 'clocked gtd-view-spec)
                   (null (alist-get 'clocked gtd-view-spec)))
          (push (org-gtd-pred--clocked-matches nil) predicates))
        ;; Add last-clocked-out predicate
        (when-let ((last-clocked-filter (alist-get 'last-clocked-out gtd-view-spec)))
          (push (org-gtd-pred--last-clocked-out-matches last-clocked-filter) predicates))
        ;; Handle last-clocked-out=nil explicitly
        (when (and (assq 'last-clocked-out gtd-view-spec)
                   (null (alist-get 'last-clocked-out gtd-view-spec)))
          (push (org-gtd-pred--last-clocked-out-matches nil) predicates))
        ;; Add tags predicate
        (when-let ((tags-filter (alist-get 'tags gtd-view-spec)))
          (push (org-gtd-pred--tags-matches tags-filter) predicates))
        ;; Add who predicate (requires type for property lookup)
        (when (assq 'who gtd-view-spec)
          (let ((who-filter (alist-get 'who gtd-view-spec)))
            (when type-filter
              (when-let ((who-prop (org-gtd-type-property type-filter :who)))
                (if (or (null who-filter) (and (stringp who-filter) (string-empty-p who-filter)))
                    ;; nil or "" means find items with missing/empty :who
                    (push (org-gtd-pred--property-empty-or-missing who-prop) predicates)
                  ;; Otherwise filter by specific value
                  (push (org-gtd-pred--property-equals who-prop who-filter) predicates))))))
        ;; Add deadline predicate
        (when-let ((deadline-filter (alist-get 'deadline gtd-view-spec)))
          (push (org-gtd-pred--deadline-matches deadline-filter) predicates))
        ;; Add scheduled predicate
        (when-let ((scheduled-filter (alist-get 'scheduled gtd-view-spec)))
          (push (org-gtd-pred--scheduled-matches scheduled-filter) predicates))
        ;; Add todo keyword predicate
        (when-let ((todo-filter (alist-get 'todo gtd-view-spec)))
          (push (org-gtd-pred--todo-matches todo-filter) predicates))
        ;; Always exclude done items from native blocks
        (push (org-gtd-pred--not-done) predicates)
        ;; Compose predicates into skip function
        (org-gtd-skip--compose (nreverse predicates)))))))

(defun org-gtd-view-lang--translate-to-native-block (gtd-view-spec &optional prefix-format)
  "Translate GTD-VIEW-SPEC to a native org-agenda block.
Returns a tags block with match string and skip function.
Optional PREFIX-FORMAT is applied for project name display."
  (let* ((name (alist-get 'name gtd-view-spec))
         (type-filter (alist-get 'type gtd-view-spec))
         (match-string (org-gtd-view-lang--build-match-string gtd-view-spec))
         (skip-fn (org-gtd-view-lang--build-skip-function gtd-view-spec))
         (settings `((org-agenda-overriding-header ,name))))
    ;; Add skip function (quoted for Emacs 29 compatibility - closures aren't self-evaluating)
    (push `(org-agenda-skip-function ',skip-fn) settings)
    ;; Add prefix format if provided
    (when prefix-format
      (push `(org-agenda-prefix-format '((tags . ,prefix-format)
                                          (todo . ,prefix-format))) settings))
    ;; Use 'tags' for property-only matches (items without TODO keywords)
    ;; Use 'tags-todo' for items with specific TODO requirements
    (let ((block-type (if (memq type-filter '(next-action delegated))
                          'tags-todo
                        'tags)))
      `(,block-type ,match-string ,settings))))

;;;; Prefix DSL Expansion

(defun org-gtd-view-lang--expand-prefix (prefix-elements width)
  "Expand PREFIX-ELEMENTS fallback chain to org-agenda-prefix-format string.
PREFIX-ELEMENTS is a list of symbols and/or strings, tried in order.
WIDTH is the column width for the prefix.
Returns a format string suitable for `org-agenda-prefix-format'."
  (let ((elements-str (org-gtd-view-lang--quote-prefix-elements prefix-elements)))
    (format " %%i %%-%d:(org-gtd-agenda--resolve-prefix-chain '%s %d) "
            width elements-str width)))

(defun org-gtd-view-lang--quote-prefix-elements (elements)
  "Convert ELEMENTS list to a string suitable for embedding in format.
Symbols are kept as-is, strings are quoted."
  (format "(%s)"
          (mapconcat
           (lambda (el)
             (if (stringp el)
                 (format "\"%s\"" el)
               (symbol-name el)))
           elements " ")))

(defun org-gtd-view-lang--get-prefix-format (gtd-view-spec &optional inherited-prefix-format)
  "Get the prefix format string from GTD-VIEW-SPEC or inherited value.
If GTD-VIEW-SPEC has a `prefix' key, expand it to a format string.
Otherwise use INHERITED-PREFIX-FORMAT if provided."
  (let ((prefix-elements (alist-get 'prefix gtd-view-spec))
        (prefix-width (alist-get 'prefix-width gtd-view-spec
                                 org-gtd-prefix-width)))
    (cond
     ;; New prefix DSL takes precedence
     (prefix-elements
      (org-gtd-view-lang--expand-prefix prefix-elements prefix-width))
     ;; Fall back to inherited format
     (inherited-prefix-format
      inherited-prefix-format)
     ;; No prefix specified
     (t nil))))

;;;; Implicit Blocks Expansion

(defconst org-gtd-view-lang--reserved-keys
  '(name blocks block-type prefix prefix-width view-type
    agenda-span show-habits additional-blocks
    group-contexts group-by todo-keyword filters)
  "Keys that are processed specially and not inherited to blocks.")

(defun org-gtd-view-lang--extract-type-keys (spec)
  "Extract all type values from SPEC.
Returns a list of type symbols in order of appearance."
  (let ((types '()))
    (dolist (pair spec)
      (when (eq (car pair) 'type)
        (push (cdr pair) types)))
    (nreverse types)))

(defun org-gtd-view-lang--extract-top-level-keys (spec)
  "Extract inheritable keys from SPEC.
Filters out reserved keys and type keys, returning an alist
of keys that should be inherited to child blocks."
  (seq-filter (lambda (pair)
                (not (or (memq (car pair) org-gtd-view-lang--reserved-keys)
                         (eq (car pair) 'type))))
              spec))

(defun org-gtd-view-lang--apply-defaults (block-spec type top-level-keys)
  "Apply four-tier precedence to BLOCK-SPEC for TYPE with TOP-LEVEL-KEYS.
Precedence (highest to lowest):
1. Block-explicit - key in BLOCK-SPEC
2. Top-level explicit - key in TOP-LEVEL-KEYS
3. Type smart default - from `org-gtd-view-lang--type-defaults'
4. Global defaults - prefix, prefix-width"
  (let* ((type-defaults (alist-get type org-gtd-view-lang--type-defaults))
         (result (copy-alist block-spec)))
    ;; Apply type defaults first (tier 3) - lowest precedence of the three
    ;; These will be overridden by top-level if present
    (dolist (default type-defaults)
      (unless (assq (car default) result)
        (push default result)))
    ;; Apply top-level keys (tier 2) - these override type defaults
    ;; We need to REPLACE any existing key, not just add if missing
    (dolist (top-key top-level-keys)
      (let ((key (car top-key)))
        ;; If key exists in result (from block or type-default), check if block had it
        ;; Block-explicit (tier 1) wins, so only override if NOT in original block-spec
        (unless (assq key block-spec)
          ;; Not in original block, so top-level takes precedence
          ;; Remove any existing (from type-defaults) and add top-level
          (setq result (assq-delete-all key result))
          (push top-key result))))
    result))

(defun org-gtd-view-lang--expand-implicit-blocks (spec)
  "Transform SPEC with multiple type keys into explicit blocks form.
If SPEC has explicit `blocks' key, returns it unchanged.
If SPEC has single type key, returns it unchanged but with defaults applied.
If SPEC has multiple type keys, expands to blocks structure."
  (let ((has-blocks (assq 'blocks spec))
        (types (org-gtd-view-lang--extract-type-keys spec)))
    (cond
     ;; Already has explicit blocks - return unchanged
     (has-blocks spec)
     ;; Single type or no types - return with global defaults applied
     ((<= (length types) 1)
      (let ((result (copy-alist spec)))
        ;; Add default prefix if none specified
        (unless (assq 'prefix result)
          (push `(prefix . ,org-gtd-view-lang--default-prefix) result))
        result))
     ;; Multiple types - expand to blocks
     (t
      (let* ((top-level-keys (org-gtd-view-lang--extract-top-level-keys spec))
             (blocks (mapcar
                      (lambda (type)
                        (org-gtd-view-lang--apply-defaults
                         `((type . ,type))
                         type
                         top-level-keys))
                      types))
             ;; Build result with blocks, preserving name and prefix keys
             (result `((blocks . ,blocks))))
        ;; Add name if present
        (when-let ((name (alist-get 'name spec)))
          (push `(name . ,name) result))
        ;; Add prefix or default
        (if-let ((prefix (alist-get 'prefix spec)))
            (push `(prefix . ,prefix) result)
          (push `(prefix . ,org-gtd-view-lang--default-prefix) result))
        ;; Add prefix-width if present
        (when-let ((width (alist-get 'prefix-width spec)))
          (push `(prefix-width . ,width) result))
        result)))))

;;;; Public API

;;;###autoload
(defun org-gtd-view-show (view-spec-or-specs &optional keys)
  "Display an org-gtd agenda view from VIEW-SPEC-OR-SPECS.

VIEW-SPEC-OR-SPECS can be either:
- A single view specification alist
- A list of view specification alists (shown as multiple blocks)

A view specification is an alist with name and filters at top level:

  \\='((name . \"View Name\")
    (type . delegated)
    (when . past))

Simple example - show all next actions:

  (org-gtd-view-show
   \\='((name . \"All My Next Actions\")
     (type . next-action)))

Multiple views example - show several related views:

  (org-gtd-view-show
   \\='(((name . \"Active projects\")
      (type . active-project))
     ((name . \"Stuck projects\")
      (type . stuck-project))))

Implicit blocks example - multiple type keys auto-expand to blocks:

  (org-gtd-view-show
   \\='((name . \"Health Review\")
     (area-of-focus . \"Health\")
     (type . calendar)
     (type . next-action)))

KEYS is an optional string used as the agenda dispatch key.
Defaults to \"g\".  When `org-agenda-sticky' is non-nil, using
different KEYS values allows multiple independent agenda views
to be displayed simultaneously, each in its own buffer named
*Org Agenda(KEYS)*.

Example with multiple views:

  (setq org-agenda-sticky t)
  (org-gtd-view-show \\='((name . \"Actions\") (type . next-action)) \"a\")
  (org-gtd-view-show \\='((name . \"Calendar\") (type . calendar)) \"c\")
  ;; Creates buffers *Org Agenda(a)* and *Org Agenda(c)*

See the module commentary or Info manual for complete filter
documentation including type, time, area-of-focus, done, and tag filters."
  (interactive)
  ;; v4: Users configure org-agenda-files directly
  (let* ((key (or keys "g"))
         (view-specs (if (and (listp view-spec-or-specs)
                              (listp (car view-spec-or-specs))
                              (symbolp (caar view-spec-or-specs)))
                         ;; Single view-spec (first element is a cons like (name . "..."))
                         (list view-spec-or-specs)
                       ;; Multiple view-specs (list of alists)
                       view-spec-or-specs))
         ;; Expand implicit blocks for each spec
         (expanded-specs (mapcar #'org-gtd-view-lang--expand-implicit-blocks view-specs))
         (title (alist-get 'name (car expanded-specs)))
         (org-agenda-custom-commands
          (org-gtd-view-lang--create-custom-commands
           expanded-specs
           key
           title)))
    (org-agenda nil key)
    (goto-char (point-min))))

;;;; Footer

(provide 'org-gtd-view-language)

;;; org-gtd-view-language.el ends here