;;; org-gtd-refile.el --- refiling logic for org gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

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

;;;; Macros

(defmacro with-org-gtd-refile (type &rest body)
  "Macro to refile specifically within org-gtd context.

TYPE is the org-gtd action type.  BODY... is the rest of the code."
  (declare (debug t) (indent 1))
  `(let ((org-refile-target-verify-function (lambda () (org-gtd-refile--group-p ,type)))
         (org-refile-targets '((org-agenda-files :level . 1)))
         (org-refile-use-outline-path nil)
         (org-outline-path-complete-in-steps nil))
     (unwind-protect
         (with-org-gtd-context (progn ,@body)))))

(defmacro with-org-gtd-refile-project-task (&rest body)
  "Refile specifically into an existing project.

BODY... is the rest of the code."
  (declare (debug t) (indent 1))
  `(let ((org-gtd-refile-to-any-target nil)
        (org-refile-use-outline-path t)
        (org-outline-path-complete-in-steps nil)
        (org-refile-allow-creating-parent-nodes nil)
        (org-refile-targets '((org-agenda-files :level . 2)))
        (org-refile-target-verify-function
         (lambda () (string-equal org-gtd-projects
                                  (org-entry-get nil "ORG_GTD" t)))))
    (unwind-protect
        (with-org-gtd-context (progn ,@body)))))

;;;; Functions

;;;;; Private

(defun org-gtd-refile--do (type refile-target-element)
  "Refile an item to the single action file.

TYPE is one of the org-gtd action types.
REFILE-TARGET-ELEMENT is a string version of a valid org-heading target."

  (with-org-gtd-refile type
      (unless (org-refile-get-targets)
        (org-gtd-refile--add-target refile-target-element))

    (if org-gtd-refile-to-any-target
        (org-refile nil nil (car (org-refile-get-targets)))
      (org-refile nil nil nil "Finish organizing task under: "))))

(defun org-gtd-refile--do-project-task ()
  (with-org-gtd-refile-project-task
      (org-refile 3 nil nil "Which project should this task go to? ")))

(defun org-gtd-refile--add-target (refile-target-element)
  "Private function used to create a missing org-gtd refile target.

GTD-TYPE is an action type.
REFILE-TARGET-ELEMENT is a string version of a valid org-heading target."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (newline)
    (insert refile-target-element)
    (basic-save-buffer)))

(defun org-gtd-refile--group-p (type)
  "Determine whether the current heading is of a given gtd TYPE."
  (string-equal type
                (org-element-property :ORG_GTD (org-element-at-point))))

;;;; Footer

(provide 'org-gtd-refile)

;;; org-gtd-refile.el ends here
