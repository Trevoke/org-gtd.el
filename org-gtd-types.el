;;; org-gtd-types.el --- GTD type definitions -*- lexical-binding: t; coding: utf-8 -*-
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
;; GTD Type System - single source of truth for GTD item types.
;; Each type defines its ORG_GTD value, TODO state semantic, and
;; semantic properties that map to `org-mode' properties.
;;
;;; Code:

;;;; Type Definitions

(defconst org-gtd-types
  '((next-action
     :org-gtd "Actions"
     :state :next
     :properties nil)

    (delegated
     :org-gtd "Delegated"
     :state :wait
     :properties
     ((:who  :org-property "DELEGATED_TO"      :type text      :required t
             :prompt "Who will do this?")
      (:when :org-property "ORG_GTD_TIMESTAMP" :type timestamp :required t
             :prompt "When to check in?")))

    (calendar
     :org-gtd "Calendar"
     :state nil
     :properties
     ((:when :org-property "ORG_GTD_TIMESTAMP" :type timestamp :required t
             :prompt "When is this happening?")))

    (tickler
     :org-gtd "Tickler"
     :state nil
     :properties
     ((:when :org-property "ORG_GTD_TIMESTAMP" :type timestamp :required t
             :prompt "When to revisit?")))

    (someday
     :org-gtd "Someday"
     :state nil
     :properties nil)

    (project
     :org-gtd "Projects"
     :state nil
     :properties nil)

    (habit
     :org-gtd "Habit"
     :state nil
     :properties
     ((:when :org-property "SCHEDULED" :type repeating-timestamp :required t
             :prompt "When and how often?")
      (:style :org-property "STYLE" :type text :required t :default "habit")))

    (reference
     :org-gtd "Reference"
     :state :done
     :properties nil)

    (trash
     :org-gtd "Trash"
     :state :canceled
     :properties nil)

    (quick-action
     :org-gtd "Quick"
     :state :done
     :properties nil))
  "GTD type definitions.
Each type is a cons of (TYPE-NAME . PLIST) where PLIST contains:
- :org-gtd - The ORG_GTD property value for this type
- :state - The semantic TODO state (:next, :wait, :done, :canceled, or nil)
- :properties - List of semantic property definitions")

(defcustom org-gtd-user-types '()
  "User customizations for built-in GTD types.

This alist allows overriding properties of existing types defined in
`org-gtd-types'.  You cannot add new types, only customize existing ones.

Each entry is (TYPE-NAME . PLIST) where TYPE-NAME must be one of the
built-in types: next-action, delegated, calendar, tickler, project,
habit, reference, trash, quick-action.

PLIST can contain:
- :properties - List of property definitions to merge/override
- :state - Override the TODO state semantic (rarely needed)

Property definitions support these attributes:
- :org-property - The `org-mode' property name (string)
- :type - Input type: text, timestamp, repeating-timestamp
- :required - Whether property is required (t or nil)
- :prompt - Prompt string for interactive input
- :default - Default value (skips prompting)
- :input-fn - Custom function for input (receives prompt, returns value)

Example: Use EBDB contacts for delegation:

  (setq org-gtd-user-types
        \\='((delegated
           :properties
           ((:who :org-property \"DELEGATED_TO\" :type text :required t
                  :prompt \"Delegate to\"
                  :input-fn my/ebdb-completing-read)))))"
  :group 'org-gtd
  :type '(alist :key-type symbol :value-type plist))

;;;; Merge Helpers

(defun org-gtd--merge-properties (builtin-props user-props)
  "Merge USER-PROPS into BUILTIN-PROPS by semantic name.
User properties with same semantic name replace builtin ones."
  (if (null user-props)
      builtin-props
    (let ((result (copy-sequence builtin-props)))
      (dolist (user-prop user-props)
        (let* ((semantic-name (car user-prop))
               (existing (seq-find (lambda (p) (eq (car p) semantic-name)) result)))
          (if existing
              ;; Replace existing property
              (setq result (mapcar (lambda (p)
                                     (if (eq (car p) semantic-name)
                                         user-prop
                                       p))
                                   result))
            ;; Add new property
            (push user-prop result))))
      result)))

(defun org-gtd--merge-type-definitions (builtin user)
  "Merge USER type definition into BUILTIN.
Properties are merged by semantic name.  User properties override builtin.
:org-gtd is never overridden from user config."
  (let* ((builtin-plist (cdr builtin))
         (user-plist (cdr user))
         (type-name (car builtin))
         ;; Never allow overriding :org-gtd
         (org-gtd-val (plist-get builtin-plist :org-gtd))
         ;; Allow overriding :state
         (state (or (plist-get user-plist :state)
                    (plist-get builtin-plist :state)))
         ;; Merge properties
         (builtin-props (plist-get builtin-plist :properties))
         (user-props (plist-get user-plist :properties))
         (merged-props (org-gtd--merge-properties builtin-props user-props)))
    (cons type-name
          (list :org-gtd org-gtd-val
                :state state
                :properties merged-props))))

;;;; Accessor Functions

(defun org-gtd-type-get (type-name)
  "Get type definition for TYPE-NAME with user overrides merged.
Returns the full type entry (TYPE-NAME . PLIST) or nil if not found."
  (when-let ((builtin (assq type-name org-gtd-types)))
    (let ((user-override (assq type-name org-gtd-user-types)))
      (if user-override
          (org-gtd--merge-type-definitions builtin user-override)
        builtin))))

(defun org-gtd-type-org-gtd-value (type-name)
  "Get the ORG_GTD property value for TYPE-NAME.
Returns nil if type not found."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (plist-get (cdr type-def) :org-gtd)))

(defun org-gtd-type-state (type-name)
  "Get the TODO state semantic for TYPE-NAME.
Returns :next, :wait, :done, :canceled, or nil."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (plist-get (cdr type-def) :state)))

(defun org-gtd-type-properties (type-name)
  "Get the list of semantic properties for TYPE-NAME.
Returns nil if type has no properties or type not found."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (plist-get (cdr type-def) :properties)))

(defun org-gtd-type-property (type-name semantic-name)
  "Get org property name for SEMANTIC-NAME in TYPE-NAME.
Returns the `org-mode' property name string, or nil if not found."
  (when-let ((props (org-gtd-type-properties type-name)))
    (when-let ((prop (seq-find (lambda (p) (eq (car p) semantic-name)) props)))
      (plist-get (cdr prop) :org-property))))

(defun org-gtd-type-from-org-gtd-value (org-gtd-value)
  "Get type name for ORG_GTD property ORG-GTD-VALUE.
Returns the type symbol or nil if not found."
  (car (seq-find (lambda (type)
                   (equal (plist-get (cdr type) :org-gtd) org-gtd-value))
                 org-gtd-types)))

;;;; Footer

(provide 'org-gtd-types)

;;; org-gtd-types.el ends here
