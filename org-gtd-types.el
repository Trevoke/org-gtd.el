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
     :disposition list
     :transient-key "s"
     :prompt-to-refile t
     :properties nil)

    (delegated
     :org-gtd "Delegated"
     :refile-target "Actions"
     :state :wait
     :disposition list
     :transient-key "d"
     :prompt-to-refile t
     :organize-fn org-gtd-delegate--organize
     :properties
     ((:who  :org-property "DELEGATED_TO"      :type text      :required t
             :prompt "Who will do this?")
      (:when :org-property "ORG_GTD_TIMESTAMP" :type timestamp :required t
             :prompt "When to check in?")))

    (calendar
     :org-gtd "Calendar"
     :state nil
     :disposition list
     :transient-key "c"
     :prompt-to-refile t
     :properties
     ((:when :org-property "ORG_GTD_TIMESTAMP" :type timestamp :required t
             :prompt "When is this happening?")))

    (tickler
     :org-gtd "Tickler"
     :disposition list
     :transient-key "i"
     :prompt-to-refile t
     :organize-fn org-gtd-tickler--organize
     :organize-project-fn org-gtd-tickler--organize-project
     :state nil
     :properties
     ((:when :org-property "ORG_GTD_TIMESTAMP" :type timestamp :required t
             :prompt "When to revisit?")))

    (someday
     :org-gtd "Someday"
     :disposition list
     :transient-key "y"
     :prompt-to-refile t
     :organize-fn org-gtd-someday--organize
     :organize-project-fn org-gtd-someday--organize-project
     :state nil
     :properties nil)

    (project
     :org-gtd "Projects"
     :state nil
     :properties nil)

    (habit
     :org-gtd "Habit"
     :refile-target "Habits"
     :disposition list
     :transient-key "h"
     :prompt-to-refile t
     :state nil
     :properties
     ((:when :org-property "SCHEDULED" :type repeating-timestamp :required t
             :prompt "When and how often?")
      (:style :org-property "STYLE" :type text :required t :default "habit")))

    (reference
     :org-gtd "Reference"
     :disposition done-and-archive
     :transient-key "k"
     :state :done
     :properties nil)

    (trash
     :org-gtd "Trash"
     :disposition cancel-and-archive
     :transient-key "t"
     :state :canceled
     :properties nil)

    (quick-action
     :org-gtd "Quick"
     :disposition done-and-archive
     :transient-key "q"
     :state :done
     :properties nil))
  "GTD type definitions.
Each type is a cons of (TYPE-NAME . PLIST).  Recognized PLIST keys:

- :org-gtd          The ORG_GTD property value (string).  Stored on
                    headings and used as the default refile-target
                    heading.  Never user-overridable.
- :state            Semantic TODO state: :next, :wait, :done, :canceled,
                    or nil.
- :properties       List of semantic property descriptors that the
                    clarify flow prompts for.
- :disposition      How items leave the clarify flow: `list',
                    `done-and-archive', `cancel-and-archive', or
                    `externalize'.  Defaults to `list'.
- :transient-key    Key string that exposes this type in the
                    `org-gtd-organize' transient menu.
- :prompt-to-refile Whether refile should prompt for a destination.
- :refile-target    Heading string this type refiles into.  Falls back
                    to :org-gtd when absent.
- :organize-fn      Function called to configure the heading as this
                    type.  Defaults to `org-gtd-configure-as-type'.
- :organize-project-fn  Function called when the dispatch lands on a
                        project heading.  When set, the type is
                        project-capable; absence means project-level
                        routing is not supported.
- :hooks            Plist of per-stage local hooks (:before-clarify,
                    :after-clarify, :before-organize, :after-organize,
                    :before-file, :after-file).")

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

(defconst org-gtd--type-scalar-fields
  '(:state :organize-fn :disposition :organize-project-fn :prompt-to-refile
    :transient-key)
  "Type-plist keys where a user value replaces the builtin value.
Note: :org-gtd is intentionally excluded and can never be overridden.")

(defun org-gtd--merge-hooks (builtin-hooks user-hooks)
  "Merge two :hooks plists by appending per-stage function lists.
BUILTIN-HOOKS functions come first, USER-HOOKS functions appended."
  (let ((result (copy-sequence builtin-hooks))
        (tail user-hooks))
    (while tail
      (let ((stage (car tail))
            (fns (cadr tail)))
        (setq result
              (plist-put result stage
                         (append (plist-get result stage) fns))))
      (setq tail (cddr tail)))
    result))

(defun org-gtd--merge-type-definitions (builtin user)
  "Merge USER type definition into BUILTIN.
:org-gtd is never overridden from user config.  Scalar wiring fields
replace, :properties merge by semantic name, and :hooks merge per stage
(builtin first)."
  (let* ((type-name (car builtin))
         (builtin-plist (cdr builtin))
         (user-plist (cdr user))
         (out (copy-sequence builtin-plist)))
    ;; Scalar replace: only when user actually mentions the key, so an
    ;; explicit nil from the user can clear a builtin value but a missing
    ;; key leaves the builtin alone.
    (dolist (k org-gtd--type-scalar-fields)
      (when (plist-member user-plist k)
        (setq out (plist-put out k (plist-get user-plist k)))))
    ;; Properties: merge by semantic name (existing helper).
    (when (plist-member user-plist :properties)
      (setq out (plist-put out :properties
                           (org-gtd--merge-properties
                            (plist-get builtin-plist :properties)
                            (plist-get user-plist :properties)))))
    ;; Hooks: per-stage append.
    (when (plist-member user-plist :hooks)
      (setq out (plist-put out :hooks
                           (org-gtd--merge-hooks
                            (plist-get builtin-plist :hooks)
                            (plist-get user-plist :hooks)))))
    ;; :org-gtd is never overridden — copy-sequence preserved the builtin.
    (cons type-name out)))

;;;; Customization

;;;###autoload
(defun org-gtd-customize-type (name-or-names &rest plist)
  "Merge PLIST into the type definition(s) named by NAME-OR-NAMES.

NAME-OR-NAMES is either a type symbol or a list of type symbols.
When it is a list, PLIST is applied to each named type using the
same merge rules.

Merge rules (see `org-gtd--merge-type-definitions'):
- Scalar fields (:state, :organize-fn, :disposition, :organize-project-fn,
  :prompt-to-refile, :transient-key) replace the existing value.
- :properties merge by semantic name.
- :hooks merge per stage -- each stage's function list appends.
- :org-gtd is never changed.

Signals an error if any named type is not registered."
  (dolist (name (if (listp name-or-names) name-or-names (list name-or-names)))
    (let ((existing (assq name org-gtd-types)))
      (unless existing
        (error "Unknown org-gtd type: %s" name))
      (let ((merged (org-gtd--merge-type-definitions existing (cons name plist))))
        (setcdr existing (cdr merged))))))

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

(defun org-gtd-type-refile-target (type-name)
  "Return the refile-target heading string for TYPE-NAME.
Returns the type's :refile-target plist value when set, otherwise
falls back to the :org-gtd value.  Returns nil if TYPE-NAME is not
a registered type."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (or (plist-get (cdr type-def) :refile-target)
        (plist-get (cdr type-def) :org-gtd))))

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

(declare-function org-gtd-configure-as-type "org-gtd-configure")

(defun org-gtd-type-organize-fn (type-name)
  "Return the :organize-fn for TYPE-NAME.
Defaults to `org-gtd-configure-as-type' when the type exists but does
not declare one.  Returns nil if TYPE-NAME is not a registered type."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (or (plist-get (cdr type-def) :organize-fn)
        #'org-gtd-configure-as-type)))

(defun org-gtd-type-disposition (type-name)
  "Return the :disposition for TYPE-NAME.
Defaults to `list' when the type exists but does not declare one.
Returns nil if TYPE-NAME is not a registered type."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (or (plist-get (cdr type-def) :disposition)
        'list)))

(defun org-gtd-type-organize-project-fn (type-name)
  "Return the :organize-project-fn declared on TYPE-NAME, or nil."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (plist-get (cdr type-def) :organize-project-fn)))

(defun org-gtd-type-prompt-to-refile (type-name)
  "Return the :prompt-to-refile flag for TYPE-NAME, or nil."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (plist-get (cdr type-def) :prompt-to-refile)))

(defun org-gtd-type-prompt-to-refile-set-p (type-name)
  "Return non-nil if TYPE-NAME explicitly declares :prompt-to-refile.
Distinguishes an explicit nil value from an absent key."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (and (plist-member (cdr type-def) :prompt-to-refile) t)))

(defun org-gtd-type-transient-key (type-name)
  "Return the :transient-key for TYPE-NAME, or nil."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (plist-get (cdr type-def) :transient-key)))

(defun org-gtd-type-hooks (type-name)
  "Return the :hooks plist declared on TYPE-NAME, or nil."
  (when-let ((type-def (org-gtd-type-get type-name)))
    (plist-get (cdr type-def) :hooks)))

(defun org-gtd-type-from-org-gtd-value (org-gtd-value)
  "Get type name for ORG_GTD property ORG-GTD-VALUE.
Returns the type symbol or nil if not found."
  (car (seq-find (lambda (type)
                   (equal (plist-get (cdr type) :org-gtd) org-gtd-value))
                 org-gtd-types)))

;;;; Footer

(provide 'org-gtd-types)

;;; org-gtd-types.el ends here
