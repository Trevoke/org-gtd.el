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
;; Category Filters:
;;   (category . delegated)   - Items with DELEGATED_TO property
;;   (category . calendar)    - Calendar items (ORG_GTD="Calendar")
;;   (category . projects)    - Project items (ORG_GTD="Projects")
;;   (category . incubate)    - Incubated items (ORG_GTD="Incubated")
;;   (category . habit)       - Habit items (STYLE="habit")
;;
;; Time-based Filters:
;;   (timestamp . past)       - ORG_GTD_TIMESTAMP in the past
;;   (timestamp . future)     - ORG_GTD_TIMESTAMP in the future
;;   (deadline . past)        - Deadline in the past (auto-adds "not done")
;;   (scheduled . past)       - Scheduled in the past (auto-adds "not done")
;;   (scheduled . future)     - Scheduled in the future
;;   (scheduled . today)      - Scheduled for today (for habits)
;;
;; Structural Filters:
;;   (level . N)              - Heading level N
;;   (todo . ("TODO" "NEXT")) - Specific TODO keywords
;;
;; Metadata Filters:
;;   (area-of-focus . "Work") - Specific area of focus (CATEGORY property)
;;   (not-habit . t)          - Exclude habits (STYLE != "habit")
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
;; Example GTD View:
;;
;; '((name . "Overdue Work Projects")
;;   (filters . ((category . projects)
;;               (level . 2)
;;               (deadline . past)
;;               (area-of-focus . "Work"))))
;;
;; This translates to org-ql query:
;; '(and (property "ORG_GTD" "Projects")
;;       (level 2)
;;       (deadline :to "today")
;;       (property "CATEGORY" "Work")
;;       (not (done)))
;;
;;; Code:
(require 'org-gtd-core)
(require 'org-gtd-ql)

(defun org-gtd-view-lang--create-agenda-block (gtd-view-spec)
  "Create an agenda block from GTD-VIEW-SPEC.
If view-type is 'agenda, creates a native agenda block.
If view-type is 'tags-grouped, creates grouped views.
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
                    (org-agenda-skip-additional-timestamps-same-entry t))))
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
KEY defaults to \"o\", TITLE defaults to \"GTD Views\"."
  (let* ((command-key (or key "o"))
         (command-title (or title "GTD Views"))
         (blocks (mapcan (lambda (view-spec)
                          (let ((main-block (org-gtd-view-lang--create-agenda-block view-spec))
                                (additional-blocks (org-gtd-view-lang--create-additional-blocks view-spec)))
                            (if additional-blocks
                                (cons main-block additional-blocks)
                              (list main-block))))
                        view-specs)))
    `((,command-key ,command-title ,blocks))))

(defun org-gtd-view-lang--translate-to-org-ql (gtd-view-spec)
  "Translate GTD-VIEW-SPEC to an org-ql query expression.
GTD-VIEW-SPEC should be an alist with 'name and 'filters keys."
  (let* ((filters (alist-get 'filters gtd-view-spec))
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
     ((eq filter-type 'not-habit)
      (org-gtd-view-lang--translate-not-habit-filter filter-value))
     ((eq filter-type 'area-of-focus)
      (org-gtd-view-lang--translate-area-of-focus-filter filter-value))
     ((eq filter-type 'todo)
      (org-gtd-view-lang--translate-todo-filter filter-value))
     ((eq filter-type 'tags)
      (org-gtd-view-lang--translate-tags-filter filter-value))
     ((eq filter-type 'tags-match)
      (org-gtd-view-lang--translate-tags-match-filter filter-value))
     ((eq filter-type 'property)
      (org-gtd-view-lang--translate-property-filter filter-value))
     ((eq filter-type 'level)
      (org-gtd-view-lang--translate-level-filter filter-value))
     (t (error "Unknown GTD filter: %s" filter-type)))))

(defun org-gtd-view-lang--translate-category-filter (category)
  "Translate category CATEGORY to org-ql property filter."
  (cond
   ((eq category 'delegated)
    (list '(property "DELEGATED_TO")))
   ((eq category 'calendar)
    (list '(property "ORG_GTD" "Calendar")))
   ((eq category 'projects)
    (list '(property "ORG_GTD" "Projects")))
   ((eq category 'incubate)
    (list '(property "ORG_GTD" "Incubated")))
   ((eq category 'habit)
    (list '(property "STYLE" "habit")))
   (t (error "Unknown category: %s" category))))

(defun org-gtd-view-lang--translate-timestamp-filter (time-spec)
  "Translate timestamp TIME-SPEC to org-ql time filter."
  (cond
   ((eq time-spec 'past)
    (list '(property-ts< "ORG_GTD_TIMESTAMP" "today") '(not (done))))
   ((eq time-spec 'future)
    (list '(property-ts> "ORG_GTD_TIMESTAMP" "today")))
   (t (error "Unknown timestamp spec: %s" time-spec))))


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

(defun org-gtd-view-lang--translate-not-habit-filter (value)
  "Translate not-habit VALUE to org-ql filter."
  (when value
    (list '(not (property "STYLE" "habit")))))

(defun org-gtd-view-lang--translate-area-of-focus-filter (area)
  "Translate area-of-focus AREA to org-ql category filter."
  (list `(property "CATEGORY" ,area)))

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
e.g., '((\"ORG_GTD\" . \"Actions\"))."
  (mapcar (lambda (prop-pair)
            `(property ,(car prop-pair) ,(cdr prop-pair)))
          property-spec))

(defun org-gtd-view-lang--translate-level-filter (level-num)
  "Translate level LEVEL-NUM to org-ql level filter."
  (list `(level ,level-num)))

(defun org-gtd-view-lang--create-grouped-views (gtd-view-spec)
  "Create grouped views from GTD-VIEW-SPEC.
Handle both simple grouped views with 'group-contexts and
dynamic grouped views with 'group-by."
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

;;;; Footer

(provide 'org-gtd-view-language)

;;; org-gtd-view-language.el ends here