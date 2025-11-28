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
;; that can be automatically translated to performant org-ql queries.
;; This replaces the complex skip functions with a more maintainable
;; and user-friendly declarative approach.
;;
;; GTD View Language Specification:
;;
;; A GTD view is defined as an alist with the following structure:
;;
;; '((name . "View Name")
;;   (filters . ((filter-type . filter-value) ...)))
;;
;; Available Filter Types:
;;
;; GTD Type Filters (PRIMARY - use these for filtering by GTD item type):
;;   (type . next-action)      - Single actions ready to do
;;   (type . delegated)        - Items delegated to others
;;   (type . calendar)         - Time-specific appointments/reminders
;;   (type . incubated)        - Someday/maybe items
;;   (type . project)          - Multi-step outcomes
;;   (type . habit)            - Recurring routines
;;   (type . reference)        - Reference/knowledge items
;;   (type . trash)            - Discarded items
;;   (type . quick-action)     - 2-minute actions (done immediately)
;;
;; Computed Project Types (special project state queries):
;;   (type . stuck-project)     - Projects with no NEXT/WAIT tasks
;;   (type . active-project)    - Projects with at least one active task
;;   (type . completed-project) - Projects with all tasks done
;;   (type . incubated-project) - Projects moved to someday/maybe
;;
;; Time-based Filters:
;;   (timestamp . past)       - ORG_GTD_TIMESTAMP in the past
;;   (timestamp . future)     - ORG_GTD_TIMESTAMP in the future
;;   (deadline . past)        - Deadline in the past (auto-adds "not done")
;;   (scheduled . past)       - Scheduled in the past (auto-adds "not done")
;;   (scheduled . future)     - Scheduled in the future
;;   (scheduled . today)      - Scheduled for today (for habits)
;;   (closed . recent)        - Closed in last 7 days
;;   (closed . past-day)      - Closed in last day
;;   (closed . past-week)     - Closed in last week
;;   (closed . past-month)    - Closed in last month
;;   (closed . past-year)     - Closed in last year
;;   (closed . today)         - Closed today
;;
;; Structural Filters:
;;   (level . N)              - Heading level N
;;   (todo . ("TODO" "NEXT")) - Specific TODO keywords
;;   (done . t)               - Items with done TODO states
;;   (not-done . t)           - Exclude items with done TODO states
;;
;; Metadata Filters:
;;   (area-of-focus . "Work") - Specific area of focus (CATEGORY property)
;;   (not-habit . t)          - Exclude habits (STYLE != "habit")
;;
;; Data Validation Filters:
;;   (invalid-timestamp . t)  - Items with missing or invalid ORG_GTD_TIMESTAMP
;;
;; Tag Filters:
;;   (tags . ("@work" "@computer")) - Match specific tags
;;   (tags-match . "{^@}")    - Match tags using org-mode tag expressions
;;
;; View Configuration:
;;   (view-type . agenda)     - Create native agenda view (default: org-ql)
;;   (agenda-span . N)        - Number of days in agenda view (default: 1)
;;   (show-habits . t/nil)    - Control habit visibility in agenda
;;   (additional-blocks . ((todo . "NEXT"))) - Additional blocks to include
;;
;; Examples:
;;
;; Simple Example - Show all next actions:
;;
;; '((name . "All My Next Actions")
;;   (filters . ((type . next-action))))
;;
;; This is the simplest possible view - just show all single-action items.
;;
;; Complex Example - Show overdue work projects:
;;
;; '((name . "Overdue Work Projects")
;;   (filters . ((type . project)
;;               (deadline . past)
;;               (area-of-focus . "Work"))))
;;
;; This combines multiple filters to narrow down to specific items.
;;
;; Computed Type Example - Show stuck projects:
;;
;; '((name . "Stuck Projects")
;;   (filters . ((type . stuck-project))))
;;
;; Computed types handle complex queries like finding projects without
;; next actions.
;;
;;; Code:
(require 'org-gtd-core)
(require 'org-gtd-ql)
(require 'org-gtd-skip)
(require 'org-gtd-types)

(defun org-gtd-view-lang--create-agenda-block (gtd-view-spec)
  "Create an agenda block from GTD-VIEW-SPEC.
If view-type is \\='agenda, creates a native agenda block.
If view-type is \\='tags-grouped, creates grouped views.
Otherwise creates an org-ql agenda block."
  (let* ((name (alist-get 'name gtd-view-spec))
         (view-type (alist-get 'view-type gtd-view-spec)))
    (cond
     ((eq view-type 'agenda)
      (org-gtd-view-lang--create-native-agenda-block gtd-view-spec))
     ((eq view-type 'tags-grouped)
      (org-gtd-view-lang--create-grouped-views gtd-view-spec))
     (t
      (let ((query (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)))
        `(org-ql-block ',query
                       ((org-ql-block-header ,name))))))))

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
        (prefix-format (alist-get 'prefix-format gtd-view-spec)))
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
  "Create org-agenda-custom-commands from VIEW-SPECS list.
KEY defaults to \"o\", TITLE defaults to \"GTD Views\".
Supports both single-block and multi-block view specs.
Multi-block specs have a \\='blocks key containing a list of block specs."
  (let* ((command-key (or key "o"))
         (command-title (or title "GTD Views"))
         (blocks (mapcan (lambda (view-spec)
                           (let ((blocks-list (alist-get 'blocks view-spec)))
                             (if blocks-list
                                 ;; Multi-block spec: process each block
                                 (mapcar #'org-gtd-view-lang--create-agenda-block blocks-list)
                               ;; Single-block spec: process as-is
                               (let ((main-block (org-gtd-view-lang--create-agenda-block view-spec))
                                     (additional-blocks (org-gtd-view-lang--create-additional-blocks view-spec)))
                                 (if additional-blocks
                                     (cons main-block additional-blocks)
                                   (list main-block))))))
                         view-specs)))
    `((,command-key ,command-title ,blocks))))

(defun org-gtd-view-lang--translate-filter (filter)
  "Translate a single FILTER specification to org-ql syntax."
  (let ((filter-type (car filter))
        (filter-value (cdr filter)))
    (cond
     ((eq filter-type 'category)
      (org-gtd-view-lang--translate-category-filter filter-value))
     ((eq filter-type 'timestamp)
      (org-gtd-view-lang--translate-timestamp-filter filter-value))
     ((eq filter-type 'deadline)
      (org-gtd-view-lang--translate-deadline-filter filter-value))
     ((eq filter-type 'scheduled)
      (org-gtd-view-lang--translate-scheduled-filter filter-value))
     ((eq filter-type 'closed)
      (org-gtd-view-lang--translate-closed-filter filter-value))
     ((eq filter-type 'not-habit)
      (org-gtd-view-lang--translate-not-habit-filter filter-value))
     ((eq filter-type 'area-of-focus)
      (org-gtd-view-lang--translate-area-of-focus-filter filter-value))
     ((eq filter-type 'todo)
      (org-gtd-view-lang--translate-todo-filter filter-value))
     ((eq filter-type 'done)
      (org-gtd-view-lang--translate-done-filter filter-value))
     ((eq filter-type 'not-done)
      (org-gtd-view-lang--translate-not-done-filter filter-value))
     ((eq filter-type 'invalid-timestamp)
      (org-gtd-view-lang--translate-invalid-timestamp-filter filter-value))
     ((eq filter-type 'tags)
      (org-gtd-view-lang--translate-tags-filter filter-value))
     ((eq filter-type 'tags-match)
      (org-gtd-view-lang--translate-tags-match-filter filter-value))
     ((eq filter-type 'property)
      (org-gtd-view-lang--translate-property-filter filter-value))
     ((eq filter-type 'level)
      (org-gtd-view-lang--translate-level-filter filter-value))
     ((eq filter-type 'type)
      (org-gtd-view-lang--translate-type-filter filter-value))
     ((eq filter-type 'previous-type)
      (org-gtd-view-lang--translate-previous-type-filter filter-value))
     ((eq filter-type 'when)
      (org-gtd-view-lang--translate-when-filter filter-value))
     ((keywordp filter-type)
      (org-gtd-view-lang--translate-semantic-property-filter filter-type filter-value))
     (t (error "Unknown GTD filter: %s" filter-type)))))

(defun org-gtd-view-lang--translate-category-filter (category)
  "Translate category CATEGORY to org-ql property filter."
  (cond
   ((eq category 'delegated)
    (list `(property ,org-gtd-prop-category ,org-gtd-delegated)))
   ((eq category 'calendar)
    (list `(property ,org-gtd-prop-category ,org-gtd-calendar)))
   ((eq category 'actions)
    (list `(property ,org-gtd-prop-category ,org-gtd-action)))
   ((eq category 'projects)
    (list `(property ,org-gtd-prop-category ,org-gtd-projects)))
   ((eq category 'active-projects)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (project-has-active-tasks))))
   ((eq category 'completed-projects)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (level 2)
                (not (project-has-active-tasks)))))
   ((eq category 'stuck-projects)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (level 2)
                (project-is-stuck))))
   ((eq category 'incubate)
    (list `(property ,org-gtd-prop-category ,org-gtd-incubate)))
   ((eq category 'incubated-projects)
    ;; Match headings with ORG_GTD: Incubated AND PREVIOUS_ORG_GTD: Projects
    (list `(and (property ,org-gtd-prop-category ,org-gtd-incubate)
                (property ,org-gtd-prop-previous-category ,org-gtd-projects))))
   ((eq category 'incubated)
    ;; Match any item with ORG_GTD: Incubated
    (list `(property ,org-gtd-prop-category ,org-gtd-incubate)))
   ((eq category 'habit)
    (list `(property ,org-gtd-prop-style ,org-gtd-prop-style-value-habit)))
   (t (error "Unknown category: %s" category))))

(defun org-gtd-view-lang--translate-timestamp-filter (time-spec)
  "Translate timestamp TIME-SPEC to org-ql time filter."
  (cond
   ((eq time-spec 'past)
    (list `(property-ts< ,org-gtd-timestamp "today") '(not (done))))
   ((eq time-spec 'today)
    (list `(property-ts= ,org-gtd-timestamp ,(format-time-string "%Y-%m-%d"))))
   ((eq time-spec 'future)
    (list `(property-ts> ,org-gtd-timestamp "today")))
   (t (error "Unknown timestamp spec: %s" time-spec))))

(defun org-gtd-view-lang--translate-when-filter (time-spec)
  "Translate when TIME-SPEC using semantic property lookup.
Requires a type filter to be present for property resolution."
  (unless org-gtd-view-lang--current-type
    (user-error "The 'when' filter requires a 'type' filter"))
  (let ((org-prop (org-gtd-type-property org-gtd-view-lang--current-type :when)))
    (unless org-prop
      (user-error "Type %s does not have a :when property" org-gtd-view-lang--current-type))
    (cond
     ((eq time-spec 'past)
      (list `(property-ts< ,org-prop "today") '(not (done))))
     ((eq time-spec 'today)
      (list `(property-ts= ,org-prop ,(format-time-string "%Y-%m-%d"))))
     ((eq time-spec 'future)
      (list `(property-ts> ,org-prop "today")))
     (t (user-error "Unknown when spec: %s" time-spec)))))

(defun org-gtd-view-lang--translate-deadline-filter (time-spec)
  "Translate deadline TIME-SPEC to org-ql deadline filter."
  (cond
   ((eq time-spec 'past)
    (list '(deadline :to "today") '(not (done))))
   (t (error "Unknown deadline spec: %s" time-spec))))

(defun org-gtd-view-lang--translate-scheduled-filter (time-spec)
  "Translate scheduled TIME-SPEC to org-ql scheduled filter."
  (cond
   ((eq time-spec 'past)
    (list '(scheduled :to "today") '(not (done))))
   ((eq time-spec 'future)
    (list '(scheduled :from "today")))
   ((eq time-spec 'today)
    (list '(scheduled :on "today")))
   (t (error "Unknown scheduled spec: %s" time-spec))))

(defun org-gtd-view-lang--translate-closed-filter (time-spec)
  "Translate closed TIME-SPEC to org-ql closed filter."
  (cond
   ((eq time-spec 'recent)
    (list '(closed :from "-7d")))
   ((eq time-spec 'past-day)
    (list '(closed :from "-1d")))
   ((eq time-spec 'past-week)
    (list '(closed :from "-1w")))
   ((eq time-spec 'past-month)
    (list '(closed :from "-1m")))
   ((eq time-spec 'past-year)
    (list '(closed :from "-1y")))
   ((eq time-spec 'today)
    (list '(closed :on "today")))
   (t (error "Unknown closed spec: %s" time-spec))))

(defun org-gtd-view-lang--translate-done-filter (value)
  "Translate done VALUE to org-ql done filter."
  (when value
    (list '(done))))

(defun org-gtd-view-lang--translate-not-done-filter (value)
  "Translate not-done VALUE to org-ql not-done filter."
  (when value
    (list '(not (done)))))

(defun org-gtd-view-lang--translate-invalid-timestamp-filter (value)
  "Translate invalid-timestamp VALUE to org-ql filter.
Uses property-invalid-timestamp predicate to find items with missing or
malformed ORG_GTD_TIMESTAMP properties."
  (when value
    (list `(property-invalid-timestamp ,org-gtd-timestamp))))

(defun org-gtd-view-lang--translate-not-habit-filter (value)
  "Translate not-habit VALUE to org-ql filter."
  (when value
    (list `(not (property ,org-gtd-prop-style ,org-gtd-prop-style-value-habit)))))

(defun org-gtd-view-lang--translate-area-of-focus-filter (area)
  "Translate area-of-focus AREA to org-ql category filter."
  (list `(property ,org-gtd-prop-area-of-focus ,area)))

(defun org-gtd-view-lang--translate-todo-filter (keywords)
  "Translate todo KEYWORDS to org-ql todo filter."
  (list `(todo ,@keywords)))

(defun org-gtd-view-lang--translate-tags-filter (tags)
  "Translate tags TAGS to org-ql tags filter."
  (list `(tags ,@tags)))

(defun org-gtd-view-lang--translate-tags-match-filter (pattern)
  "Translate tags-match PATTERN to org-ql tags filter."
  (list `(tags ,pattern)))

(defun org-gtd-view-lang--translate-property-filter (property-spec)
  "Translate property PROPERTY-SPEC to org-ql property filter.
PROPERTY-SPEC should be an alist with property name and value pairs,
e.g., \\='((\"ORG_GTD\" . \"Actions\"))."
  (mapcar (lambda (prop-pair)
            `(property ,(car prop-pair) ,(cdr prop-pair)))
          property-spec))

(defun org-gtd-view-lang--translate-level-filter (level-num)
  "Translate level LEVEL-NUM to org-ql level filter."
  (list `(level ,level-num)))

(defun org-gtd-view-lang--translate-type-filter (type-name)
  "Translate TYPE-NAME to org-ql property filter using org-gtd-types.
TYPE-NAME should be a symbol like \\='next-action, \\='delegated, \\='calendar, etc.
Also supports computed types:
  - \\='stuck-project - Projects with no NEXT/WAIT tasks
  - \\='active-project - Projects with at least one active task
  - \\='completed-project - Projects with all tasks done
  - \\='incubated-project - Incubated items that were projects"
  (cond
   ;; Computed project types
   ((eq type-name 'stuck-project)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (project-is-stuck))))
   ((eq type-name 'active-project)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (project-has-active-tasks))))
   ((eq type-name 'completed-project)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-projects)
                (not (project-has-active-tasks)))))
   ((eq type-name 'incubated-project)
    (list `(and (property ,org-gtd-prop-category ,org-gtd-incubate)
                (property ,org-gtd-prop-previous-category ,org-gtd-projects))))
   ;; Types with implied TODO keywords
   ((eq type-name 'next-action)
    (list `(property "ORG_GTD" ,(org-gtd-type-org-gtd-value 'next-action))
          `(todo ,(org-gtd-keywords--next))))
   ((eq type-name 'delegated)
    (list `(property "ORG_GTD" ,(org-gtd-type-org-gtd-value 'delegated))
          `(todo ,(org-gtd-keywords--wait))))
   ;; Standard types from org-gtd-types
   (t
    (let ((org-gtd-val (org-gtd-type-org-gtd-value type-name)))
      (unless org-gtd-val
        (user-error "Unknown GTD type: %s" type-name))
      (list `(property "ORG_GTD" ,org-gtd-val))))))

(defun org-gtd-view-lang--translate-previous-type-filter (type-name)
  "Translate previous-type TYPE-NAME to PREVIOUS_ORG_GTD property filter.
TYPE-NAME should be a symbol like \\='delegated, \\='next-action, \\='project, etc.
This is used for incubated items to filter by their original type."
  (let ((org-gtd-val (org-gtd-type-org-gtd-value type-name)))
    (unless org-gtd-val
      (user-error "Unknown GTD type: %s" type-name))
    (list `(property "PREVIOUS_ORG_GTD" ,org-gtd-val))))

(defvar org-gtd-view-lang--current-type nil
  "Track the current type filter for semantic property resolution.")

(defun org-gtd-view-lang--translate-to-org-ql (gtd-view-spec)
  "Translate GTD-VIEW-SPEC to an org-ql query expression.
GTD-VIEW-SPEC should be an alist with \\='name and either:
- \\='filters key containing filter alist (legacy format)
- Filter keys directly at top level (new flat format)"
  (let* ((explicit-filters (alist-get 'filters gtd-view-spec))
         ;; Reserved keys that are not filters
         (reserved-keys '(name blocks block-type prefix-format view-type
                          agenda-span show-habits additional-blocks
                          group-contexts group-by tags-match))
         ;; Extract filters: either from 'filters key or from top-level keys
         (filters (or explicit-filters
                      (seq-filter (lambda (pair)
                                    (not (memq (car pair) reserved-keys)))
                                  gtd-view-spec)))
         ;; Extract type filter for semantic property resolution
         (type-filter (seq-find (lambda (f) (eq (car f) 'type)) filters))
         (org-gtd-view-lang--current-type (when type-filter (cdr type-filter)))
         (has-future-time-filter (seq-some (lambda (filter)
                                             (and (memq (car filter) '(deadline scheduled))
                                                  (eq (cdr filter) 'future)))
                                           filters))
         (all-conditions (apply #'append (mapcar #'org-gtd-view-lang--translate-filter filters)))
         (not-done-conditions (seq-filter (lambda (cond) (equal cond '(not (done)))) all-conditions))
         (other-conditions (seq-remove (lambda (cond) (equal cond '(not (done)))) all-conditions)))
    (if has-future-time-filter
        `(and ,@other-conditions)
      `(and ,@other-conditions ,@not-done-conditions))))

(defun org-gtd-view-lang--translate-semantic-property-filter (semantic-name time-spec)
  "Translate SEMANTIC-NAME (like :when) with TIME-SPEC for current type.
Uses org-gtd-types to resolve the semantic property to the actual org property."
  (unless org-gtd-view-lang--current-type
    (user-error "Semantic property filter %s requires a type filter" semantic-name))
  (let ((org-prop (org-gtd-type-property org-gtd-view-lang--current-type semantic-name)))
    (unless org-prop
      (user-error "Type %s does not have semantic property %s"
                  org-gtd-view-lang--current-type semantic-name))
    (cond
     ((eq time-spec 'past)
      (list `(property-ts< ,org-prop "today") '(not (done))))
     ((eq time-spec 'future)
      (list `(property-ts> ,org-prop "today")))
     ((eq time-spec 'within-week)
      (list `(property-ts< ,org-prop "+1w")))
     (t (user-error "Unknown time spec: %s" time-spec)))))

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

;;;; Public API

;;;###autoload
(defun org-gtd-view-show (view-spec-or-specs)
  "Display an org-gtd agenda view from VIEW-SPEC-OR-SPECS.

VIEW-SPEC-OR-SPECS can be either:
- A single view specification alist
- A list of view specification alists

A view specification is an alist with the following structure:
  ((name . \"View Name\")
   (filters . ((filter-type . filter-value) ...)))

Simple example - show all next actions:
  (org-gtd-view-show
   \\='((name . \"All My Next Actions\")
     (filters . ((type . next-action)))))

Multiple views example - show several related views:
  (org-gtd-view-show
   \\='(((name . \"Active projects\")
      (filters . ((type . project))))
     ((name . \"Stuck projects\")
      (filters . ((type . stuck-project))))))

See the module commentary for complete filter documentation and more examples."
  (interactive)
  (org-gtd-core-prepare-agenda-buffers)
  ;; v4: Users configure org-agenda-files directly, no need for with-org-gtd-context
  (let* ((view-specs (if (and (listp view-spec-or-specs)
                              (listp (car view-spec-or-specs))
                              (symbolp (caar view-spec-or-specs)))
                         ;; Single view-spec (first element is a cons like (name . "..."))
                         (list view-spec-or-specs)
                       ;; Multiple view-specs (list of alists)
                       view-spec-or-specs))
         (title (alist-get 'name (car view-specs)))
         (org-agenda-custom-commands
          (org-gtd-view-lang--create-custom-commands
           view-specs
           "g"
           title)))
    (org-agenda nil "g")
    (goto-char (point-min))))

;;;; Footer

(provide 'org-gtd-view-language)

;;; org-gtd-view-language.el ends here