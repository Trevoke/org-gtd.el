;;; org-gtd-incubate.el --- Define incubated items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Incubated items have their own logic, defined here
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)

(declare-function org-gtd-organize--call 'org-gtd-organize)
(declare-function org-gtd-organize-apply-hooks 'org-gtd-organize)
(declare-function org-gtd-organize--update-in-place 'org-gtd-organize)

;;;; Constants

(defconst org-gtd-incubate-func #'org-gtd-incubate--apply
  "Function called when organizing item as incubated.")

(defconst org-gtd-incubate-template
  (format "* Incubate
:PROPERTIES:
:%s: %s
:END:
" org-gtd-prop-refile org-gtd-incubate)
  "Template for the GTD someday/maybe list.")

;;;; Commands

(defun org-gtd-incubate (&optional reminder-date)
  "Decorate, organize and refile item at point as incubated.

Smart dispatcher that detects context:
- On project heading (ORG_GTD: Projects): incubate entire project
- On project task (has ORG_GTD_PROJECT_IDS): incubate project(s)
- On single item: use existing single-item incubation logic

If you want to call this non-interactively,
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."
  (interactive)

  ;; Get the actual marker - works from both org buffers and agenda buffers
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (point-marker))))
    (org-with-point-at marker
      ;; Detect context
      (let* ((org-gtd-value (org-entry-get (point) "ORG_GTD"))
             (project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS"))
             (is-project-heading (string= org-gtd-value "Projects"))
             (is-project-task (> (length project-ids) 0)))

        (cond
         ;; Case 1: On project heading - incubate the project
         (is-project-heading
          (require 'org-gtd-projects)
          (let ((review-date (or reminder-date
                                 (org-read-date nil nil nil "Review date: "))))
            (org-gtd-project-incubate (point-marker) review-date)))

         ;; Case 2: On project task - incubate the project(s)
         ;; For now, just incubate the first project (multi-project selection added later)
         (is-project-task
          (require 'org-gtd-projects)
          (let* ((project-id (car project-ids))
                 (project-marker (org-id-find project-id t))
                 (review-date (or reminder-date
                                  (org-read-date nil nil nil "Review date: "))))
            (if project-marker
                (org-gtd-project-incubate project-marker review-date)
              (user-error "Cannot find project with ID: %s" project-id))))

         ;; Case 3: Single item - use existing logic
         (t
          (let ((config-override (when reminder-date
                                   `(('active-timestamp . ,(lambda (_x) (format "<%s>" reminder-date)))))))
            (org-gtd-organize--call
             (lambda () (org-gtd-incubate--apply config-override))))))))))

;;;###autoload
(defun org-gtd-reactivate ()
  "Reactivate an incubated GTD item or project at point.
Restores the item to active status, returning it to your GTD workflow.

Smart dispatcher that detects context:
- On incubated project heading: reactivate entire project
- On incubated single item: reactivate that item (future enhancement)"
  (interactive)

  ;; Check if item is incubated
  (let ((org-gtd-value (org-entry-get (point) "ORG_GTD")))
    (unless (string= org-gtd-value "Incubated")
      (user-error "Item at point is not incubated (ORG_GTD: %s)" org-gtd-value))

    ;; Detect if this is a project heading by checking PREVIOUS_ORG_GTD
    (let ((previous-org-gtd (org-entry-get (point) "PREVIOUS_ORG_GTD")))
      (cond
       ;; Case 1: Was a project heading - reactivate project
       ((string= previous-org-gtd "Projects")
        (require 'org-gtd-projects)
        (org-gtd-project-reactivate (point-marker)))

       ;; Case 2: Was a single item - reactivate that item
       ;; TODO: Implement single item reactivation logic (future enhancement)
       (t
        (user-error "Single item reactivation not yet implemented"))))))

;;;; Functions

;;;;; Public

(defun org-gtd-incubate-create (topic reminder-date)
  "Automatically create a delegated task in the GTD flow.

TOPIC is the string you want to see in the `org-agenda' view.
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd")
        (config-override `(('active-timestamp . ,(lambda (_x) (format "<%s>" reminder-date))))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-incubate--apply config-override))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-incubate--configure (&optional config-override)
  "Configure item at point as incubated.

CONFIG-OVERRIDE can provide input configuration to override default
prompting behavior."
  (org-gtd-configure-as-type 'incubated
                             (when config-override
                               `((:when . ,(funcall (alist-get '(quote active-timestamp) config-override nil nil #'equal) nil))))))

(defun org-gtd-incubate--finalize ()
  "Finalize incubated item organization and refile."
  (setq-local org-gtd--organize-type 'incubated)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-incubate org-gtd-incubate-template)))

(defun org-gtd-incubate--apply (&optional config-override)
  "Process GTD inbox item by transforming it into an incubated item.

Orchestrates the incubate organization workflow:
1. Configure with incubate settings
2. Finalize and refile to incubate file

CONFIG-OVERRIDE can provide input configuration to override default
prompting behavior."
  (org-gtd-incubate--configure config-override)
  (org-gtd-incubate--finalize))

;;;; Footer

(provide 'org-gtd-incubate)

;;; org-gtd-incubate.el ends here
