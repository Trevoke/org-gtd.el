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
      (let ((continuation org-gtd-clarify--continuation)
            (task-id org-gtd-clarify--clarify-id)
            (window-config org-gtd-clarify--window-config)
            (skip-refile org-gtd-clarify--skip-refile))
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
        (when task-id
          (org-gtd-wip--cleanup-temp-file task-id))
        (when window-config
          (set-window-configuration window-config))
        (when continuation (funcall continuation))
        ;; Save GTD buffers after organizing
        (org-gtd-save-buffers)
        ;; Clean up horizons view for one-off clarification
        (unless continuation
          (org-gtd-clarify--cleanup-horizons-view))))))

;;;; Footer

(provide 'org-gtd-organize-core)

;;; org-gtd-organize-core.el ends here
