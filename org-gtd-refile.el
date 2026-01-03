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

(defcustom org-gtd-refile-to-any-target t
  "Set to true if you do not need to choose where to refile processed items.

When this is true, org-gtd will refile to the first target it finds, or creates
it if necessary, without confirmation.  When this is false, it will ask for
confirmation regardless of the number of options.  Note that setting this to
false does not mean you can safely create new targets.  See the documentation
to create new refile targets.

Defaults to true to carry over pre-2.0 behavior.  You will need to change this
setting as part of following the instructions to add your own refile targets."
  :group 'org-gtd-organize
  :package-version '(org-gtd . "2.0.0")
  :type 'boolean)

(make-obsolete-variable 'org-gtd-refile-to-any-target
                        'org-gtd-refile-prompt-for-types
                        "4.0.0")

(defcustom org-gtd-refile-prompt-for-types
  '(single-action project-heading project-task calendar someday delegated tickler habit)
  "List of GTD types that should prompt for refile target selection.

When refiling an item, if its type is in this list, org-gtd will prompt
you to choose from available refile targets (ORG_GTD_REFILE targets +
user's `org-refile-targets').  If the type is not in this list, org-gtd
will automatically refile to the first available target.

This variable is only consulted when `org-gtd-refile-to-any-target' is nil.

Valid type symbols (same as `org-gtd-organize-type-member-p'):
  single-action, project-heading, project-task, calendar, someday,
  delegated, tickler, habit, knowledge, quick-action, trash"
  :group 'org-gtd-organize
  :package-version '(org-gtd . "4.0.0")
  :type '(repeat symbol))

;;;; Variables

(defvar org-gtd--organize-type)

(defvar org-gtd-refile--deprecated-warning-shown nil
  "Non-nil if deprecation warning for `org-gtd-refile-to-any-target' was shown.")


;;;; Functions

;;;;; Helper

(defun org-gtd-refile--should-prompt-p (type)
  "Return non-nil if refiling TYPE should prompt for target selection.

Checks `org-gtd-refile-to-any-target' first (deprecated, takes precedence
when non-nil).  If nil, checks if TYPE is in `org-gtd-refile-prompt-for-types'."
  (if org-gtd-refile-to-any-target
      (progn
        (unless org-gtd-refile--deprecated-warning-shown
          (display-warning 'org-gtd
                           "`org-gtd-refile-to-any-target' is deprecated as of 4.0.0.
Set it to nil and customize `org-gtd-refile-prompt-for-types' instead.")
          (setq org-gtd-refile--deprecated-warning-shown t))
        nil)
    (memq type org-gtd-refile-prompt-for-types)))

;;;;; Private

(defun org-gtd-refile--do (type refile-target-element)
  "Refile an item to an appropriate GTD location.

TYPE is one of the org-gtd action types (e.g., `org-gtd-projects').
REFILE-TARGET-ELEMENT is a string template for creating a new target if needed.

Merges user's `org-refile-targets' with org-gtd's ORG_GTD_REFILE targets.
User's targets appear first, then org-gtd's property-based targets.
Files in `org-gtd-directory' are filtered by ORG_GTD_REFILE property;
files outside are accepted without filtering."
  (unless (org-gtd-refile--get-targets type)
    (org-gtd-refile--add-target refile-target-element))
  (let ((org-refile-target-verify-function
         (lambda ()
           (let* ((file (buffer-file-name))
                  (gtd-dir (expand-file-name org-gtd-directory))
                  (in-gtd-dir (and file (string-prefix-p gtd-dir (expand-file-name file))))
                  (refile-prop (org-element-property :ORG_GTD_REFILE (org-element-at-point))))
             (if in-gtd-dir
                 (string-equal type refile-prop)
               t))))
        (org-refile-targets (append org-refile-targets
                                    '((org-agenda-files :maxlevel . 9))))
        (org-refile-use-outline-path t)
        (org-outline-path-complete-in-steps nil))
    (if (org-gtd-refile--should-prompt-p org-gtd--organize-type)
        (org-refile nil nil nil "Finish organizing task under: ")
      (org-refile nil nil (car (org-refile-get-targets))))))

(defun org-gtd-refile--get-targets (type)
  "Get refile targets for TYPE, merging user's targets with org-gtd's.

Returns the list of refile targets that would be available when refiling
an item of TYPE (e.g., `org-gtd-projects', variable `org-gtd-tickler')."
  (let ((org-refile-target-verify-function
         (lambda ()
           (let* ((file (buffer-file-name))
                  (gtd-dir (expand-file-name org-gtd-directory))
                  (in-gtd-dir (and file (string-prefix-p gtd-dir (expand-file-name file))))
                  (refile-prop (org-element-property :ORG_GTD_REFILE (org-element-at-point))))
             (if in-gtd-dir
                 (string-equal type refile-prop)
               t))))
        (org-refile-targets (append org-refile-targets
                                    '((org-agenda-files :maxlevel . 9))))
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
  "Determine whether the current heading is of a given gtd TYPE."
  (string-equal type
                (org-element-property :ORG_GTD_REFILE (org-element-at-point))))

;;;; Footer

(provide 'org-gtd-refile)

;;; org-gtd-refile.el ends here
