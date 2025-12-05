;;; org-gtd-tickler.el --- Define tickler items in org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Tickler items have their own logic, defined here
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-clarify)
(require 'org-gtd-refile)
(require 'org-gtd-configure)
(require 'org-gtd-reactivate)
(require 'org-gtd-projects)
(require 'org-gtd-organize-core)

;;;; Constants

(defconst org-gtd-tickler-func #'org-gtd-tickler--apply
  "Function called when organizing item as tickler.")

(defconst org-gtd-tickler-template
  (format "* Tickler
:PROPERTIES:
:%s: %s
:END:
" org-gtd-prop-refile org-gtd-tickler)
  "Template for the GTD tickler list.")

;;;; Commands

(defun org-gtd-tickler (&optional reminder-date)
  "Decorate, organize and refile item at point as tickler.

Smart dispatcher that detects context:
- On project heading (ORG_GTD: Projects): tickler entire project
- On project task (has ORG_GTD_PROJECT_IDS): tickler project(s)
- On single item: use existing single-item tickler logic

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
         ;; Case 1: On project heading - tickler the project
         (is-project-heading
          (require 'org-gtd-projects)
          (let ((review-date (or reminder-date
                                 (org-read-date nil nil nil "Review date: "))))
            (org-gtd-project-incubate (point-marker) review-date)))

         ;; Case 2: On project task - tickler the project(s)
         ;; For now, just tickler the first project (multi-project selection added later)
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
             (lambda () (org-gtd-tickler--apply config-override))))))))))

;;;; Functions

;;;;; Public

(defun org-gtd-tickler-create (topic reminder-date)
  "Automatically create a tickler task in the GTD flow.

TOPIC is the string you want to see in the `org-agenda' view.
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."
  (let ((buffer (generate-new-buffer "Org GTD programmatic temp buffer"))
        (org-id-overriding-file-name "org-gtd")
        (config-override `(('active-timestamp . ,(lambda (_x) (format "<%s>" reminder-date))))))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-tickler--apply config-override))
    (kill-buffer buffer)))

;;;;; Private

(defun org-gtd-tickler--configure (&optional config-override)
  "Configure item at point as tickler.

Saves current state to PREVIOUS_* properties before setting type.
CONFIG-OVERRIDE can provide input configuration to override default
prompting behavior."
  ;; Save current state before changing type
  (org-gtd-save-state)
  (org-gtd-configure-as-type 'tickler
                             (when config-override
                               `((:when . ,(funcall (alist-get '(quote active-timestamp) config-override nil nil #'equal) nil))))))

(defun org-gtd-tickler--finalize ()
  "Finalize tickler item organization and refile."
  (setq-local org-gtd--organize-type 'tickler)
  (org-gtd-organize-apply-hooks)
  (if org-gtd-clarify--skip-refile
      (org-gtd-organize--update-in-place)
    (org-gtd-refile--do org-gtd-tickler org-gtd-tickler-template)))

(defun org-gtd-tickler--apply (&optional config-override)
  "Process GTD inbox item by transforming it into a tickler item.

Orchestrates the tickler organization workflow:
1. Configure with tickler settings
2. Finalize and refile to tickler file

CONFIG-OVERRIDE can provide input configuration to override default
prompting behavior."
  (org-gtd-tickler--configure config-override)
  (org-gtd-tickler--finalize))

;;;; Backward Compatibility Aliases

;; Incubate → Tickler rename (v4.0)
;;;###autoload
(define-obsolete-function-alias 'org-gtd-incubate
  #'org-gtd-tickler "4.0")

;;;###autoload
(define-obsolete-function-alias 'org-gtd-incubate-create
  #'org-gtd-tickler-create "4.0")

;;;; Footer

(provide 'org-gtd-tickler)

;;; org-gtd-tickler.el ends here
