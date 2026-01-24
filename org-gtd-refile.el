;;; org-gtd-refile.el --- refiling logic for org gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Refiling logic for org-gtd.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-refile)
(require 'org-element)

(require 'org-gtd-core)
(require 'org-gtd-files)

;;;; Customization

(defcustom org-gtd-refile-prompt-for-types nil
  "List of GTD item types that should prompt for refile target selection.

By default this is nil, meaning all items auto-refile to the first
available target.  This provides a turnkey experience where organizing
is fast and frictionless.

To control where specific item types are filed, add them to this list.
When an item's type is in the list, org-gtd prompts you to choose from
available refile targets.

Example - prompt only for projects (most common customization):

  (setq org-gtd-refile-prompt-for-types
        \\='(project-heading project-task))

Example - prompt for everything (maximum control):

  (setq org-gtd-refile-prompt-for-types
        \\='(single-action project-heading project-task calendar
          someday delegated tickler habit knowledge quick-action trash))

Valid type symbols:
  single-action   - One-off tasks
  project-heading - New project containers
  project-task    - Tasks added to existing projects
  calendar        - Date/time specific items
  someday         - Someday/maybe items
  delegated       - Items waiting on others
  tickler         - Items to resurface later
  habit           - Recurring habits
  knowledge       - Reference material
  quick-action    - <2 minute tasks (done immediately)
  trash           - Items to discard"
  :group 'org-gtd-organize
  :package-version '(org-gtd . "5.0.0")
  :type '(repeat symbol))

;;;; Variables

(defvar org-gtd--organize-type)


;;;; Functions

;;;;; Helper

(defun org-gtd-refile--wip-file-p (file)
  "Return non-nil if FILE is in the WIP temp directory.
WIP temp files are used during clarification and should never be
valid refile targets."
  (when file
    (let ((wip-temp-dir (expand-file-name "org-gtd" temporary-file-directory)))
      (string-prefix-p (file-name-as-directory wip-temp-dir)
                       (expand-file-name file)))))

(defun org-gtd-refile--make-verify-function (type)
  "Create a verify function for refile targets of TYPE.
Returns a function suitable for `org-refile-target-verify-function'.

The function filters targets as follows:
- WIP temp files: always rejected (prevents self-refile errors)
- Inbox file: always rejected (inbox is for capture, not refile)
- Files in `org-gtd-directory': must have matching ORG_GTD_REFILE property
- Other files: accepted (allows user's custom refile targets)"
  (lambda ()
    (let* ((file (buffer-file-name))
           (gtd-dir (expand-file-name org-gtd-directory))
           (inbox-path (expand-file-name (org-gtd--path "inbox")))
           (in-wip-dir (org-gtd-refile--wip-file-p file))
           (is-inbox (and file (string-equal (expand-file-name file) inbox-path)))
           (in-gtd-dir (and file (string-prefix-p gtd-dir (expand-file-name file))))
           (refile-prop (org-element-property :ORG_GTD_REFILE (org-element-at-point))))
      (cond
       ;; WIP temp files should never be refile targets
       (in-wip-dir nil)
       ;; Inbox file should never be a refile target
       (is-inbox nil)
       ;; Files in GTD dir must have matching ORG_GTD_REFILE property (multivalue)
       (in-gtd-dir (and refile-prop
                        (member type (split-string refile-prop))))
       ;; Other files (user's custom targets) are accepted
       (t t)))))

(defun org-gtd-refile--should-prompt-p (type)
  "Return non-nil if refiling TYPE should prompt for target selection.
Checks if TYPE is in `org-gtd-refile-prompt-for-types'."
  (memq type org-gtd-refile-prompt-for-types))

;;;;; Private

(defun org-gtd-refile--do (type refile-target-element)
  "Refile an item to an appropriate GTD location.

TYPE is one of the org-gtd action types (e.g., `org-gtd-projects').
REFILE-TARGET-ELEMENT is a string template for creating a new target if needed.

When prompting (type in `org-gtd-refile-prompt-for-types'), shows merged
targets from user's `org-refile-targets' and GTD files.

When auto-refiling (type not in prompt list), only uses GTD targets in
`org-gtd-tasks.org', ignoring user configuration."
  ;; Check GTD-only targets for existence (not merged targets)
  (unless (org-gtd-refile--get-gtd-targets type)
    (org-gtd-refile--add-target refile-target-element))

  (if (org-gtd-refile--should-prompt-p org-gtd--organize-type)
      ;; Prompt mode: use merged targets (user + GTD)
      (let ((org-refile-target-verify-function (org-gtd-refile--make-verify-function type))
            (org-refile-targets (append org-refile-targets
                                        '((org-agenda-files :maxlevel . 9))))
            (org-refile-use-outline-path t)
            (org-outline-path-complete-in-steps nil))
        (org-refile nil nil nil "Finish organizing task under: "))
    ;; Auto-refile: GTD-only targets
    (org-refile nil nil (car (org-gtd-refile--get-gtd-targets type)))))

(defun org-gtd-refile--get-targets (type)
  "Get refile targets for TYPE, merging user's targets with org-gtd's.

Returns the list of refile targets that would be available when refiling
an item of TYPE (e.g., `org-gtd-projects', variable `org-gtd-tickler')."
  (let ((org-refile-target-verify-function (org-gtd-refile--make-verify-function type))
        (org-refile-targets (append org-refile-targets
                                    '((org-agenda-files :maxlevel . 9))))
        (org-refile-use-outline-path t)
        (org-outline-path-complete-in-steps nil))
    (org-refile-get-targets)))

(defun org-gtd-refile--get-gtd-targets (type)
  "Get GTD-only refile targets for TYPE.
Only looks in `org-gtd-tasks.org', ignoring user's `org-refile-targets'
and other `org-agenda-files'.  This ensures auto-refile always goes to
the GTD file, regardless of user configuration."
  (let* ((gtd-file (org-gtd--path org-gtd-default-file-name))
         (org-refile-target-verify-function (org-gtd-refile--make-verify-function type))
         (org-refile-targets `(((,gtd-file) :maxlevel . 9)))
         (org-refile-use-outline-path t)
         (org-outline-path-complete-in-steps nil))
    (org-refile-get-targets)))

(defun org-gtd-refile--add-target (refile-target-element)
  "Create a missing org-gtd refile target in the default GTD file.

REFILE-TARGET-ELEMENT is a string version of a valid org-heading target.

Creates the target in `org-gtd--default-file' to ensure consistent location.
In future, this could be enhanced to prompt for file location when multiple
org-gtd files exist."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (newline)
    (insert refile-target-element)
    (basic-save-buffer)))

(defun org-gtd-refile--group-p (type)
  "Determine whether the current heading is of a given gtd TYPE.
TYPE can be one of multiple space-separated values in ORG_GTD_REFILE."
  (let ((refile-prop (org-element-property :ORG_GTD_REFILE (org-element-at-point))))
    (and refile-prop
         (member type (split-string refile-prop)))))

;;;; Footer

(provide 'org-gtd-refile)

;;; org-gtd-refile.el ends here
