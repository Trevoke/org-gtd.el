;;; org-gtd-organize-core.el --- Core organizing functions -*- lexical-binding: t; coding: utf-8 -*-
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
;; Core organizing functions shared by category modules.
;;
;; This module exists to break the cycle between org-gtd-organize and the
;; category modules (calendar, delegate, habit, etc.). Category modules need
;; these functions but org-gtd-organize requires all category modules to build
;; its transient menu.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-wip)
(require 'org-gtd-clarify)
(require 'org-gtd-types)
(require 'org-gtd-hooks)
(require 'org-gtd-refile)

;;;; Customization

(defgroup org-gtd-organize nil
  "Manage the functions for organizing the GTD actions."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-organize-hooks '(org-set-tags-command)
  "Enhancements to add to each item as they get processed from the inbox.

This is a list of functions that modify an org element.  The default value has
one function: setting org tags on the item.  Some built-in examples are
provided as options here.  You can create your own functions to further organize
the items once they have been processed and add them to that list.

To run a hook only for specific item types, use
`org-gtd-organize-type-member-p' in your hook function.  For example:

  (defun my-add-effort ()
    (when (org-gtd-organize-type-member-p \\='(single-action project-heading))
      (call-interactively #\\='org-set-effort)))

Once you have your ground items managed, you might like to set the variable
`org-gtd-areas-of-focus' and add `org-gtd-set-area-of-focus' to these hooks."
  :group 'org-gtd-organize
  :options '(org-set-tags-command org-set-effort org-priority)
  :package-version '(org-gtd . "1.0.4")
  :type 'hook)

;;;; Functions

;;;;; Public

(defun org-gtd-organize-apply-hooks ()
  "Apply hooks to add metadata to a given GTD item."
  (dolist (hook org-gtd-organize-hooks)
    (save-excursion
      (goto-char (point-min))
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1))
      (save-restriction (funcall hook)))))

;;;;; Private

(defun org-gtd-organize--update-in-place ()
  "Replace original heading with configured content from WIP buffer.
Uses `org-gtd-clarify--source-heading-marker' to find the original location."
  (let ((new-content (save-excursion
                       (goto-char (point-min))
                       (when (org-before-first-heading-p)
                         (org-next-visible-heading 1))
                       (org-copy-subtree)
                       (current-kill 0)))
        ;; Capture marker value while still in WIP buffer
        (source-marker org-gtd-clarify--source-heading-marker))
    (when (and (boundp 'org-gtd-clarify--source-heading-marker)
               source-marker
               (markerp source-marker)
               (marker-buffer source-marker))
      (with-current-buffer (marker-buffer source-marker)
        (goto-char source-marker)
        (org-back-to-heading t)
        (org-cut-subtree)
        (insert new-content)
        (save-buffer)))))

(defun org-gtd-organize--call (func)
  "Wrap FUNC, which does the real work, to keep Emacs clean.
This handles the internal bits of `org-gtd'."
  (goto-char (point-min))
  (when (org-before-first-heading-p)
    (org-next-visible-heading 1))
  ;; v4: Users configure org-agenda-files directly, no need for with-org-gtd-context
  (let ((error-caught
         (catch 'org-gtd-error
           (save-excursion (funcall func))
           nil))) ;; Return nil when no error was thrown
    (unless error-caught
      ;; Only run cleanup if no error was thrown
      ;; Capture buffer-local variables before buffer is killed
      (let ((continuation org-gtd-clarify--continuation)
            (task-id org-gtd-clarify--clarify-id)
            (window-config org-gtd-clarify--window-config)
            (skip-refile org-gtd-clarify--skip-refile)
            (duplicate-queue (copy-sequence org-gtd-clarify--duplicate-queue)))
        ;; Clear queue so kill-buffer hooks don't prompt
        (setq org-gtd-clarify--duplicate-queue nil)
        ;; Only cut original if we refiled (not updated in place)
        (unless skip-refile
          (when (and (boundp 'org-gtd-clarify--source-heading-marker)
                     org-gtd-clarify--source-heading-marker
                     (markerp org-gtd-clarify--source-heading-marker))
            (let ((buffer (marker-buffer org-gtd-clarify--source-heading-marker))
                  (position (marker-position org-gtd-clarify--source-heading-marker)))
              (when (and buffer position)
                (with-current-buffer buffer
                  (goto-char position)
                  (with-temp-message ""
                    (org-cut-subtree)))))))
        ;; Check if we have queued duplicates to process
        (if duplicate-queue
            ;; Reuse current buffer for next queued item
            (org-gtd-clarify--process-next-queued-item
             duplicate-queue window-config continuation task-id)
          ;; No queue - clean up and proceed with normal flow
          (when task-id
            (org-gtd-wip--cleanup-temp-file task-id))
          (when window-config
            (set-window-configuration window-config))
          (when continuation (funcall continuation)))
        ;; Save GTD buffers after organizing
        (org-gtd-save-buffers)
        ;; Clean up horizons view for one-off clarification
        (unless continuation
          (org-gtd-clarify--cleanup-horizons-view))))))

;;;;; Pipeline primitives

(defun org-gtd--clear-foreign-properties (new-type)
  "Remove org-properties belonging to the prior type at point.

A property is considered foreign when it appears in the previously
active type's :properties list but NOT in NEW-TYPE's :properties list.
The previously active type is derived from the current `ORG_GTD'
property value via `org-gtd-type-from-org-gtd-value'.

Does nothing when the heading has no `ORG_GTD' property, or when the
value does not name a registered type.  Operates on the entry at point."
  (let* ((prev-value (org-entry-get (point) "ORG_GTD"))
         (prev-type (and prev-value
                         (org-gtd-type-from-org-gtd-value prev-value))))
    (when prev-type
      (let ((new-props (mapcar (lambda (p) (plist-get (cdr p) :org-property))
                               (org-gtd-type-properties new-type)))
            (old-props (mapcar (lambda (p) (plist-get (cdr p) :org-property))
                               (org-gtd-type-properties prev-type))))
        (dolist (prop old-props)
          (unless (member prop new-props)
            (org-entry-delete (point) prop)))))))

(defun org-gtd--default-refile-template (type)
  "Construct a default refile-target-element string for TYPE.
Uses the type's :org-gtd value and `org-gtd-prop-refile' so that the
refile engine can create the top-level heading if it is missing."
  (let ((value (org-gtd-type-org-gtd-value type)))
    (format "* %s\n:PROPERTIES:\n:%s: %s\n:END:\n"
            value org-gtd-prop-refile value)))

(defun org-gtd--run-disposition (type _pom)
  "Dispatch on TYPE's :disposition.
Honors `org-gtd-clarify--skip-refile' by calling
`org-gtd-organize--update-in-place' instead of the type-specific
disposition.  POM is currently unused but reserved for future
dispositions that need it."
  (if (and (boundp 'org-gtd-clarify--skip-refile)
           org-gtd-clarify--skip-refile)
      (org-gtd-organize--update-in-place)
    (let ((disp (org-gtd-type-disposition type)))
      (cond
       ((eq disp 'list)
        (org-gtd-refile--do (org-gtd-type-org-gtd-value type)
                            (org-gtd--default-refile-template type)))
       ((eq disp 'done-and-archive)
        (error "Disposition done-and-archive not implemented yet (type %s)" type))
       ((eq disp 'cancel-and-archive)
        (error "Disposition cancel-and-archive not implemented yet (type %s)" type))
       ((eq disp 'externalize)
        (error "Disposition externalize not implemented yet (type %s)" type))
       (t
        (error "Unknown disposition %s for type %s" disp type))))))

(defun org-gtd-process-heading (pom type &optional config)
  "Organize the heading at POM as TYPE, running the full hook pipeline.

POM is a point or marker identifying the heading to organize.  TYPE is
a symbol naming a registered org-gtd type.  CONFIG is an optional alist
forwarded to the type's :organize-fn for non-interactive invocation.

Pipeline, executed with point at POM:

  1. If TYPE declares :supports reactivate, call `org-gtd-save-state'.
  2. Clear properties belonging to the previously-active type
     (`org-gtd--clear-foreign-properties').
  3. Run :before-organize hooks (global then local).
  4. Call the type's :organize-fn with TYPE and CONFIG.
  5. Apply the user's classic `org-gtd-organize-hooks' (tags/effort/etc.)
     via `org-gtd-organize-apply-hooks'.  Runs for every type.
  6. Run :after-organize hooks.
  7. Run :before-file hooks.
  8. Run the type's disposition (`org-gtd--run-disposition').
  9. Run :after-file hooks."
  (org-with-point-at pom
    (when (org-gtd-type-supports-p type 'reactivate)
      (when (fboundp 'org-gtd-save-state)
        (org-gtd-save-state)))
    (org-gtd--clear-foreign-properties type)
    (org-gtd-hooks-run :before-organize type pom)
    (funcall (org-gtd-type-organize-fn type) type config)
    (setq-local org-gtd--organize-type type)
    (org-gtd-organize-apply-hooks)
    (org-gtd-hooks-run :after-organize type pom)
    (org-gtd-hooks-run :before-file type pom)
    (org-gtd--run-disposition type pom)
    (org-gtd-hooks-run :after-file type pom)))

(defun org-gtd-process-project (pom type &optional config)
  "Reclassify the project at POM as TYPE using the type's :project-fn.

POM is a point or marker identifying a project heading or a task that
belongs to a project.  TYPE must declare `:supports project-handler'
and supply a `:project-fn', which is called with POM and CONFIG.
Signals a `user-error' if TYPE does not declare project-handler
support or lacks a `:project-fn'."
  (unless (org-gtd-type-supports-p type 'project-handler)
    (user-error "Type %s does not support project-level handling" type))
  (let ((fn (org-gtd-type-project-fn type)))
    (unless fn
      (user-error "Type %s has project-handler support but no :project-fn" type))
    (funcall fn pom config)))

(defun org-gtd--dispatch (type)
  "Dispatch a per-type command for TYPE based on the heading at point.

Reads the marker at beginning-of-line first (for agenda compatibility)
or falls back to point-marker.  At the resolved marker, routes to:

- `org-gtd-process-project' when ORG_GTD=Projects and TYPE supports
  project-handler.
- `org-gtd-process-project' with a user-selected project marker when
  the heading is a task belonging to at least one project and TYPE
  supports project-handler.
- `org-gtd-process-heading' otherwise (plain headings, and project
  tasks when TYPE does not declare project-handler support)."
  ;; `org-gtd-projects' requires `org-gtd-organize-core', so we cannot
  ;; require it here without creating a cycle.  Load it lazily and rely
  ;; on `fboundp' as a defensive guard.
  (let* ((marker (or (org-get-at-bol 'org-marker)
                     (point-marker))))
    (org-with-point-at marker
      (let* ((org-gtd-value (org-entry-get (point) "ORG_GTD"))
             (project-ids (org-entry-get-multivalued-property
                           (point) "ORG_GTD_PROJECT_IDS"))
             (is-project-heading (string= org-gtd-value "Projects"))
             (is-project-task (> (length project-ids) 0))
             (supports-project (org-gtd-type-supports-p type 'project-handler)))
        (cond
         ((and is-project-heading supports-project)
          (org-gtd-process-project (point-marker) type))
         ((and is-project-task supports-project)
          (require 'org-gtd-projects)
          (unless (fboundp 'org-gtd-project--get-marker-at-point)
            (error "org-gtd-project--get-marker-at-point unavailable"))
          (let ((project-marker
                 (org-gtd-project--get-marker-at-point
                  (format "Which project to process as %s? " type))))
            (org-gtd-process-project project-marker type)))
         (t
          (org-gtd-process-heading (point-marker) type)))))))

;;;; Footer

(provide 'org-gtd-organize-core)

;;; org-gtd-organize-core.el ends here
