;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Version: 0.1
;; URL: https://github.com/trevoke/org-gtd
;; Package-Requires: ((emacs "26.1") (org-edna "1.0.2") (org-brain "0.8") (f "0.20.0") (org "9.3.1") (org-agenda-property "1.3.1"))

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package aims to provide a pre-packaged GTD workflow.
;; See README.org for more information.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'org)
(require 'org-edna)
(require 'org-brain)
(require 'org-agenda-property)

(defvar org-stuck-projects)

(defconst org-gtd--package-path (f-dirname (f-this-file)))

(defconst org-gtd-actionable "actionable")
(defconst org-gtd-inbox      "inbox")
(defconst org-gtd-someday    "someday")

(defconst org-gtd-actions   ".*Actions")
(defconst org-gtd-delegated ".*Delegated")
(defconst org-gtd-later     ".*Someday.*")
(defconst org-gtd-scheduled ".*Scheduled")
(defconst org-gtd-projects  ".*Projects")

(defconst org-gtd-stuck-projects '("+LEVEL=2-DONE+CATEGORY=\"Projects\""
				   ("TODO" "NEXT" "WAIT")
				   nil
				   ""))

(defconst org-gtd-complete-projects "+LEVEL=2+CATEGORY=\"Projects\"")

(defun org-gtd--refile-targets ()
  "Return the refile targets specific to org-gtd."
  `((,(org-gtd--path org-gtd-someday) :maxlevel . 2)
    (,(org-gtd--path org-gtd-actionable) :maxlevel . 1)))

(defgroup org-gtd nil "Customize the org-gtd package."
  :version 0.1 :group 'emacs)

(defcustom org-gtd-directory "~/gtd/"
  "The directory where the org files for GTD will live."
  :type 'directory)

(defun org-gtd-capture ()
  "Wrap `org-capture' to make sure the gtd inbox exists."
  (interactive)
  (kill-buffer (org-gtd--inbox))
  (org-capture))

(defun org-gtd-show-all-next ()
  "Show all the NEXT items in a single list."
  (interactive)
  (org-todo-list "NEXT"))

(defun org-gtd-show-stuck-projects ()
  "Show all GTD projects that do not have an upcoming or waiting action."
  (interactive)
  (let* ((user-stuck-projects org-stuck-projects)
	 (org-stuck-projects org-gtd-stuck-projects)
	 (stuck-projects-buffer (org-agenda-list-stuck-projects))
	 (org-stuck-projects user-stuck-projects))
    stuck-projects-buffer))

(defun org-gtd-archive-complete-projects ()
  "Archive all projects for which all children headlines are marked DONE."
  (interactive)
  (org-map-entries
   (lambda ()
     (if (org-gtd--project-complete-p)
	 (progn
	   (setq org-map-continue-from (org-element-property
					:begin
					(org-element-at-point)))
	   (org-archive-subtree-default))))
   org-gtd-complete-projects))

(defun org-gtd-process-inbox ()
  "Use this once a day: process every element in the inbox."
  (interactive)

  (set-buffer (org-gtd--inbox))
  (display-buffer-same-window (org-gtd--inbox) '())
  (delete-other-windows)

  ;; laugh all you want, all this statefulness is killing me.
  (org-gtd--actionable)
  (org-gtd--someday)

  (org-map-entries
   (lambda ()
     (setq org-map-continue-from (org-element-property
				  :begin
				  (org-element-at-point)))
     (org-narrow-to-element)
     (org-show-subtree)
     (org-gtd--process-inbox-element)
     (widen)))

  (setq-local header-line-format nil)

  (mapcar
   (lambda (buffer) (with-current-buffer buffer (save-buffer)))
   `(,(org-gtd--actionable) ,(org-gtd--someday) ,(org-gtd--inbox))))

(defun org-gtd--process-inbox-element ()
  "With mark on an org heading, choose which GTD action to take."
  (let ((action
	 (read-multiple-choice
	  "What to do with this item?"
	  '((?q "quick" "quick item: < 2 minutes, done!")
	    (?t "throw out" "this has no value to me")
	    (?p "project" "multiple steps required to completion")
	    (?c "calendar" "do this at a certain time")
	    (?d "delegate it" "give it to someone")
	    (?s "single action" "do this when possible")
	    (?a "archive this knowledge" "add this to the brain")
	    (?i "incubate it" "remind me of this possibility later")))))

    (cl-case (car action)
      (?q (org-gtd--quick-action))
      (?t (org-gtd--trash))
      (?p (org-gtd--project))
      (?c (org-gtd--calendar))
      (?d (org-gtd--delegate))
      (?s (org-gtd--single-action))
      (?a (org-gtd--archive))
      (?i (org-gtd--incubate)))

(defun org-gtd--incubate ()
  "Process element and move it to the someday file."
  (org-gtd--edit-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target org-gtd-later)))

(defun org-gtd--archive ()
  "Process element and move it to the brain."
  (org-brain-add-resource nil nil "What's the link? " nil)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--trash ()
  "Archive element."
  (org-gtd--edit-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "CANCELED")
  (org-archive-subtree))

(defun org-gtd--single-action ()
  "Process element and move it to the Actionable file."
  (org-gtd--edit-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "NEXT")
  (org-refile nil nil (org-gtd--refile-target org-gtd-actions)))

(defun org-gtd--delegate ()
  "Process element and move it to the Actionable file."
  (org-gtd--edit-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "WAIT")
  (org-set-property "DELEGATED_TO" (read-string "Who will do this? "))
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target org-gtd-delegated)))

(defun org-gtd--calendar ()
  "Process element and move it to the tickler file."
  (org-gtd--edit-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "NEXT")
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target org-gtd-scheduled)))

(defun org-gtd--quick-action ()
  "Process element and archive it."
  (org-gtd--edit-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--project ()
  "Process element and transform it into a project."
  (org-gtd--edit-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-gtd--nextify)
  (org-refile nil nil (org-gtd--refile-target org-gtd-projects))

  (with-current-buffer (org-gtd--actionable)
    (org-update-statistics-cookies t)))

(defun org-gtd--refile-target (heading-regexp)
  "HEADING-REGEXP is a regular expression for one of the desired GTD refile
locations. See `org-refile'."
  (let* ((user-refile-targets org-refile-targets)
	 (org-refile-targets (org-gtd--refile-targets))
	 (results   (cl-find-if
		     (lambda (rfloc)
		       (string-match heading-regexp
				     (car rfloc)))
		     (org-refile-get-targets)))
	 (org-refile-targets user-refile-targets))
    results))

(defun org-gtd--nextify ()
  "Add NEXT and TODO as keywords on all the relevant headlines."

  (destructuring-bind
      (first-entry . rest-entries)
      (cdr (org-map-entries (lambda () (org-element-at-point)) t 'tree))
    (org-element-map
	(reverse rest-entries)
	'headline
      (lambda (myelt)
	(org-entry-put (org-gtd--org-element-pom myelt) "TODO" "TODO")))
    (org-entry-put (org-gtd--org-element-pom first-entry) "TODO" "NEXT")))

(defun org-gtd--project-complete-p ()
  "Return t if all project children are DONE, f if any aren't."
  (let ((entries (cdr (org-map-entries
		       (lambda ()
			 (org-entry-get
			  (org-gtd--org-element-pom (org-element-at-point))
			  "TODO"))
		       t
		       'tree))))
    (seq-every-p (lambda (x) (string-equal x "DONE")) entries)))

(defun org-gtd--org-element-pom (element)
  "Return buffer position for start of org ELEMENT."
  (org-element-property :begin element))

(defun org-gtd--path (file)
  "Return the full path to FILE.org assuming it is in the GTD framework."
  (f-join org-gtd-directory (concat file ".org")))

(defun org-gtd--template-path (file)
  "Return full path to FILE_template.org."
  (f-join org-gtd--package-path
	  (concat file "_template.org")))

(defun org-gtd--gtd-file (gtd-type)
  "Return a buffer for GTD-TYPE.org. create the file and template first if it
doesn't already exist."
  (let* ((file-path (org-gtd--path gtd-type))
	 (file-buffer (find-file-noselect file-path)))
    (or (f-file-p file-path)
	(with-current-buffer file-buffer
	  (insert-file-contents (org-gtd--template-path gtd-type) nil nil nil t)
	  (save-buffer)))
    file-buffer))

(defun org-gtd--actionable ()
  "Create or return the buffer for the actionable GTD buffer."
  (org-gtd--gtd-file org-gtd-actionable))

(defun org-gtd--inbox ()
  "Create or return the buffer for the inbox GTD buffer."
  (org-gtd--gtd-file org-gtd-inbox))

(defun org-gtd--someday ()
  "Create or return the buffer for the someday GTD buffer."
  (org-gtd--gtd-file org-gtd-someday))

(defun org-gtd--edit-item ()
  "Friendly user interaction to refine the current inbox item"
  (org-gtd-user-input-mode 1)
  (recursive-edit))

;;; Minor mode

(defvar org-gtd-user-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'org-gtd-finish-editing)
    map)
  "Keymap for `org-gtd-user-input-mode', a minor mode.")

(defun org-gtd-finish-editing ()
  "Get out of the manual edit flow."
  (interactive)
  (org-gtd-user-input-mode -1)
  (exit-recursive-edit))

(define-minor-mode org-gtd-user-input-mode
  "Minor mode for special key bindings when editing an individual inbox item."
  nil "GTD " org-gtd-user-input-mode-map
  (setq-local header-line-format
	      (substitute-command-keys
	       "\\<org-gtd-user-input-mode-map>Edit inbox item. Finish \
`\\[org-gtd-finish-editing]'.")))

(provide 'org-gtd)

;;; org-gtd.el ends here
