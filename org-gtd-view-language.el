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
;;
;; Time-based Filters:
;;   (timestamp . past)       - ORG_GTD_TIMESTAMP in the past
;;   (deadline . past)        - Deadline in the past (auto-adds "not done")
;;   (scheduled . past)       - Scheduled in the past (auto-adds "not done")
;;   (scheduled . future)     - Scheduled in the future
;;
;; Structural Filters:
;;   (level . N)              - Heading level N
;;   (todo . ("TODO" "NEXT")) - Specific TODO keywords
;;
;; Metadata Filters:
;;   (area-of-focus . "Work") - Specific area of focus (CATEGORY property)
;;   (not-habit . t)          - Exclude habits (STYLE != "habit")
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
  "Create an org-ql agenda block from GTD-VIEW-SPEC."
  (let* ((name (alist-get 'name gtd-view-spec))
         (query (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)))
    `(org-ql-block ',query
                   ((org-ql-block-header ,name)))))

(defun org-gtd-view-lang--create-custom-commands (view-specs)
  "Create org-agenda-custom-commands from VIEW-SPECS list."
  (let ((blocks (mapcar #'org-gtd-view-lang--create-agenda-block view-specs)))
    `(("o" "GTD Oops Views" ,blocks))))

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
     ((eq filter-type 'level)
      (org-gtd-view-lang--translate-level-filter filter-value))
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
     (t (error "Unknown GTD filter: %s" filter-type)))))

(defun org-gtd-view-lang--translate-category-filter (category)
  "Translate category CATEGORY to org-ql property filter."
  (cond
   ((eq category 'delegated)
    (list '(property "DELEGATED_TO" ".+")))
   ((eq category 'calendar)
    (list '(property "ORG_GTD" "Calendar")))
   ((eq category 'projects)
    (list '(property "ORG_GTD" "Projects")))
   (t (error "Unknown category: %s" category))))

(defun org-gtd-view-lang--translate-timestamp-filter (time-spec)
  "Translate timestamp TIME-SPEC to org-ql time filter."
  (cond
   ((eq time-spec 'past)
    (list '(property-ts< "ORG_GTD_TIMESTAMP" "today")))
   (t (error "Unknown timestamp spec: %s" time-spec))))

(defun org-gtd-view-lang--translate-level-filter (level-value)
  "Translate level LEVEL-VALUE to org-ql level filter."
  (list `(level ,level-value)))

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

;;;; Footer

(provide 'org-gtd-view-language)

;;; org-gtd-view-language.el ends here