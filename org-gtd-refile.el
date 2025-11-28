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


;;;; Functions

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
    (if org-gtd-refile-to-any-target
        (org-refile nil nil (car (org-refile-get-targets)))
      (org-refile nil nil nil "Finish organizing task under: "))))

(defun org-gtd-refile--do-project-task ()
  "Refile a task into an existing project.

Merges user's `org-refile-targets' with org-gtd's project targets.
User's targets appear first, then level-2 headings with ORG_GTD=Projects.
Files in `org-gtd-directory' are filtered for project headings;
files outside are accepted without filtering."
  (let ((org-gtd-refile-to-any-target nil)
        (org-refile-use-outline-path t)
        (org-outline-path-complete-in-steps nil)
        (org-refile-allow-creating-parent-nodes nil)
        ;; MERGE: user's targets first, then org-gtd's project targets
        (org-refile-targets (append org-refile-targets
                                    '((org-agenda-files :level . 2))))
        (org-refile-target-verify-function
         (lambda ()
           (let* ((file (buffer-file-name))
                  (gtd-dir (expand-file-name org-gtd-directory))
                  (in-gtd-dir (and file (string-prefix-p gtd-dir (expand-file-name file)))))
             (if in-gtd-dir
                 ;; GTD files: require ORG_GTD=Projects
                 (string-equal org-gtd-projects (org-entry-get nil "ORG_GTD"))
               ;; Non-GTD files: allow all (user's targets)
               t)))))
    (org-refile 3 nil nil "Which project should this task go to? ")))

(defun org-gtd-refile--get-targets (type)
  "Get refile targets for TYPE, merging user's targets with org-gtd's.

Returns the list of refile targets that would be available when refiling
an item of TYPE (e.g., `org-gtd-projects', `org-gtd-tickler')."
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

Creates the target in org-gtd--default-file to ensure consistent location.
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
