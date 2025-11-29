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

;;;; Footer

(provide 'org-gtd-reactivate)

;;; org-gtd-reactivate.el ends here
