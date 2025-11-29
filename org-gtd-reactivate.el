;;; org-gtd-reactivate.el --- Save and restore GTD state -*- lexical-binding: t; coding: utf-8 -*-
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
;; Save and restore GTD item state when moving to/from someday/tickler.
;; Uses the type system to determine which properties to preserve.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-types)

;;;; Functions

(defun org-gtd-save-state ()
  "Save current GTD state to PREVIOUS_* properties.

Only saves if item has ORG_GTD that is not someday/tickler.
Saves ORG_GTD, TODO state, and all type-specific properties."
  (let* ((current-org-gtd (org-entry-get (point) "ORG_GTD"))
         (current-type (when current-org-gtd
                         (org-gtd-type-from-org-gtd-value current-org-gtd))))
    ;; Only save if not already someday/tickler
    (when (and current-org-gtd
               (not (member current-type '(someday tickler))))
      ;; Save ORG_GTD
      (org-entry-put (point) "PREVIOUS_ORG_GTD" current-org-gtd)
      ;; Save TODO state
      (when-let ((todo (org-entry-get (point) "TODO")))
        (org-entry-put (point) "PREVIOUS_TODO" todo))
      ;; Save type-specific properties
      (dolist (prop (org-gtd-type-properties current-type))
        (let ((org-prop (plist-get (cdr prop) :org-property)))
          (when-let ((val (org-entry-get (point) org-prop)))
            (org-entry-put (point) (concat "PREVIOUS_" org-prop) val)))))))

(declare-function org-gtd-clarify-item "org-gtd-clarify")

(defun org-gtd-restore-state ()
  "Restore GTD state from PREVIOUS_* properties.

If PREVIOUS_ORG_GTD is nil, clears ORG_GTD and calls `org-gtd-clarify-item'.
Otherwise, restores ORG_GTD, TODO, and type-specific properties.
Prompts user to confirm/update each type-specific property."
  (let ((previous-org-gtd (org-entry-get (point) "PREVIOUS_ORG_GTD")))

    ;; Clear someday/tickler-specific properties
    (org-entry-delete (point) "ORG_GTD_TIMESTAMP")

    (if (null previous-org-gtd)
        ;; No saved state - re-clarify
        (progn
          (org-entry-delete (point) "ORG_GTD")
          (require 'org-gtd-clarify)
          (org-gtd-clarify-item))

      ;; Has saved state - restore with prompts
      (let ((previous-type (org-gtd-type-from-org-gtd-value previous-org-gtd)))
        ;; Restore ORG_GTD
        (org-entry-put (point) "ORG_GTD" previous-org-gtd)
        (org-entry-delete (point) "PREVIOUS_ORG_GTD")

        ;; Restore TODO state
        (when-let ((previous-todo (org-entry-get (point) "PREVIOUS_TODO")))
          (org-todo previous-todo)
          (org-entry-delete (point) "PREVIOUS_TODO"))

        ;; Restore type-specific properties with prompts
        (dolist (prop (org-gtd-type-properties previous-type))
          (let* ((prop-def (cdr prop))
                 (org-prop (plist-get prop-def :org-property))
                 (prompt (plist-get prop-def :prompt))
                 (previous-key (concat "PREVIOUS_" org-prop))
                 (previous-val (org-entry-get (point) previous-key)))
            (when previous-val
              (let ((new-val (read-string
                              (format "%s [%s]: " prompt previous-val)
                              nil nil previous-val)))
                (org-entry-put (point) org-prop new-val))
              (org-entry-delete (point) previous-key))))))))

(declare-function org-gtd-project-reactivate "org-gtd-projects")

;;;###autoload
(defun org-gtd-reactivate ()
  "Reactivate a someday/tickler item at point.

Restores the item to its previous GTD state, prompting to confirm
or update each type-specific property (dates, delegated-to, etc.).

For projects, uses `org-gtd-project-reactivate' which iterates over
all tasks and recalculates dependencies."
  (interactive)
  (let ((org-gtd-value (org-entry-get (point) "ORG_GTD")))
    (unless (member org-gtd-value (list org-gtd-someday org-gtd-tickler))
      (user-error "Item is not someday/tickler (ORG_GTD: %s)" org-gtd-value))
    ;; Check if this was a project
    (let ((previous-org-gtd (org-entry-get (point) "PREVIOUS_ORG_GTD")))
      (if (string= previous-org-gtd "Projects")
          ;; Projects need special handling for task iteration + dependencies
          (progn
            (require 'org-gtd-projects)
            (org-gtd-project-reactivate (point-marker)))
        ;; Non-project items use generic restore
        (org-gtd-restore-state)))))

;;;; Footer

(provide 'org-gtd-reactivate)

;;; org-gtd-reactivate.el ends here
