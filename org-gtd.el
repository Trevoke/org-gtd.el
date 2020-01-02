;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Version: 0.1
;; URL: https://github.com/trevoke/org-gtd
;; Package-Requires: ((emacs "26.1") (org-edna "1.0.2") (org-brain "0.8") (f "0.20.0"))

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is a highly opinionated, destructive implementation of GTD.
;;
;; Highly opinionated because it aims to follow the pure idea of the GTD framework.
;; Destructive because it overrides a number of org-level variables, so if you
;; have some settings you want to keep, this might make you unhappy.

;;; Code:

(require 'org-edna)
(require 'cl-lib)

(setq org-edna-use-inheritance t)
(org-edna-load)

(defconst org-gtd--types '(actionable timely inbox someday))
(defconst org-gtd--package-path (f-dirname (f-this-file)))

(defgroup org-gtd nil "Customize the org-gtd package. After changing these values, call `org-gtd-init'."
  :version 0.1 :group 'emacs)

;; TODO how to intelligently wire up the customize tools?
(defcustom org-gtd-directory "~/gtd/"
  "The directory where the org files for GTD will live. Ends with a /."
  :type 'directory)

(defcustom org-gtd-actionable-file "Actionable.org"
  "Name of the file that holds the projects. Should end in .org."
  :type 'file)

(defcustom org-gtd-inbox-file "Inbox.org"
  "Name of the file that holds the inbox. Should end in .org."
  :type 'file)

(defcustom org-gtd-timely-file "Timely.org"
  "Name of the file that holds the scheduled items, including reminders. Should end in .org."
  :type 'file)

(defcustom org-gtd-someday-file "Someday.org"
  "Name of the file holding deferred thoughts (come back to this someday). Should end in .org."
  :type 'file)

(defun org-gtd--project-buffer ()
  "Private function. Get or create the buffer to transform an inbox item into a project."
  (get-buffer-create "*org-gtd-project*"))

(defun org-gtd-show-all-next ()
  "Show all the NEXT items in a single list."
  (interactive)
  (org-todo-list "NEXT"))

(defun org-gtd-init ()
  "Initialize the org-gtd package based on configuration."
  (interactive)

  (org-gtd--init-actionable-file 'org-gtd-actionable-file org-gtd-actionable-file)
  (org-gtd--init-inbox-file 'org-gtd-inbox-file org-gtd-inbox-file)
  (org-gtd--init-someday-file 'org-gtd-someday-file org-gtd-someday-file)
  (org-gtd--init-timely-file 'org-gtd-timely-file org-gtd-timely-file)

  (setq org-agenda-window-setup 'other-window)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-span 'day)

  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes t)

  (setq org-stuck-projects '("+LEVEL=2-notproject/-DONE"
			     ("TODO" "NEXT" "WAIT")
			     nil ""))

  (setq org-agenda-files `(,org-gtd-directory))

  (setq org-refile-targets `((,org-gtd-someday :maxlevel . 2)
			     (,org-gtd-actionable :maxlevel . 1)
			     (,org-gtd-timely :maxlevel . 1)))

  (setq org-capture-templates `(("i" "Inbox"
				 entry (file ,org-gtd-inbox)
				 "* TODO %?\n  %i"
				 :kill-buffer t)
				("t" "Todo with link"
				 entry (file ,org-gtd-inbox)
				 "* TODO %?\n  %i\n  %a"
				 :kill-buffer t))))

(defun org-gtd-process-inbox ()
  "Use this once a day: process every element in the inbox."
  (interactive)
  (let ((inbox-buffer (progn (find-file org-gtd-inbox)
			     (get-file-buffer org-gtd-inbox))))
    (set-buffer inbox-buffer)
    (display-buffer-same-window inbox-buffer '())
    (delete-other-windows)

    (org-map-entries
     (lambda ()
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))
       (org-narrow-to-element)
       (org-gtd--process-inbox-element inbox-buffer)
       (widen)))))

(defun org-gtd--process-inbox-element (inbox-buffer)
  "Private function. INBOX-BUFFER is the buffer with the org gtd inbox."
  (let ((action
	 (read-multiple-choice
	  "What are we doing with this item?"
	  '((?q "quick" "quick item: < 2 minutes, done!")
	    (?p "project" "multiple steps required to completion")
	    (?s "schedule" "do this at a certain time")
	    (?d "delegate" "give it to someone")
	    (?w "whenever" "do this when possible")
	    (?g "garbage" "throw this away")
	    (?r "reference" "add this to the brain")
	    (?l "later" "remind me of this possibility at some point")
	    (?t "tickler" "I need to be reminded of this at a given time")))))
    (cl-case (car action)
      (?q (org-gtd--quick-action))
      (?p (org-gtd--project inbox-buffer))
      (?s (org-gtd--schedule))
      (?d (org-gtd--delegate))
      (?w (org-gtd--whenever))
      (?g (org-gtd--garbage))
      (?r (org-gtd--reference))
      (?l (org-gtd--later))
      (?t (org-gtd--tickler)))))

(defun org-gtd--tickler ()
  "Private function. Process element and move it to the tickler file."
  (move-end-of-line 1)
  (open-line 1)
  (forward-line)
  (org-time-stamp nil)
  (org-refile nil nil (org-gtd--refile-target ".*Reminders")))

(defun org-gtd--later ()
  "Private function. Process element and move it to the someday file."
  (move-end-of-line 1)
  (open-line 1)
  (forward-line)
  (org-time-stamp nil)
  (org-refile))

(defun org-gtd--reference ()
  "Private function. Process element and move it to the brain."
  (org-brain-add-resource nil nil "What's the link? " nil)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--garbage ()
  "Private function. Archive element."
  (org-todo "CANCELED")
  (org-archive-subtree))

(defun org-gtd--whenever ()
  "Private function. Process element and move it to the Actionable file."
  (org-set-tags-command)
  (org-todo "NEXT")
  (org-refile nil nil (org-gtd--refile-target ".*One-Offs")))

(defun org-gtd--delegate ()
  "Private function. Process element and move it to the Actionable file."
  (org-todo "WAIT")
  (org-set-property "DELEGATED_TO" (read-string "Who will do this? "))
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target ".*Delegated")))

(defun org-gtd--schedule ()
  "Private function. Process element and move it to the tickler file."
  (org-todo "TODO")
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target ".*Scheduled")))

(defun org-gtd--quick-action ()
  "Private function. Process element and archive it."
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--project (inbox-buffer)
  "Private function. Process element and transform it into a project. INBOX-BUFFER is the buffer holding the org-gtd inbox."
  (with-current-buffer (org-gtd--project-buffer)
    (erase-buffer)
    (org-mode)
    (display-buffer-same-window (org-gtd--project-buffer) '())
    (delete-other-windows)
    (insert-buffer-substring inbox-buffer)
    (recursive-edit)
    (goto-char (point-min))
    (org-refile nil nil (org-gtd--refile-target ".*Projects")))

  (with-current-buffer (get-file-buffer org-gtd-actionable)
    (org-update-statistics-cookies t))

  (display-buffer-same-window inbox-buffer '())
  (org-archive-subtree))

(defun org-gtd--refile-target (heading-regexp)
  "Private function. HEADING-REGEXP is a regular expression for one of the desired GTD refile locations. See `org-refile'."
  (cl-find-if
   (lambda (rfloc)
     (string-match heading-regexp
		   (car rfloc)))
   (org-refile-get-targets)))

;; helper functions
;; and dragons

(defun org-gtd--path (file)
  "Private function. FILE is a filename. Return the full path to it assuming it is in the GTD framework."
  (f-join org-gtd-directory file))

(defun org-gtd--template-path (file)
  "Private function. FILE is a template filename. Return full path to it."
  (f-join org-gtd--package-path file))

(defun org-gtd--set-file-path (filename value)
  "Private function. takes FILENAME and VALUE."
  (set-default filename value)
  (let ((var (intern (replace-regexp-in-string "-file"
					       ""
					       (symbol-name filename)))))
    (set var (org-gtd--path value))))

(defun org-gtd--init-gtd-file (varname value gtd-type)
  "Private function. VARNAME and VALUE are things inherited from customize, and GTD-TYPE is one of `org-gtd--types'. Here be dragons."
  (unless (member gtd-type org-gtd--types)
    (error "Unknown gtd-type argument"))
  (let* ((file (org-gtd--set-file-path varname value))
	 (buffer (find-file file))
	 ;; TODO move the _template.org bit inside `org-gtd--template-path'.
	 (template (concat (symbol-name gtd-type) "_template.org")))
    (or (f-file-p file)
	(with-current-buffer buffer
	  (insert-file-contents (org-gtd--template-path template) nil nil nil t)
	  (save-buffer)))
    (kill-buffer buffer)))

(defun org-gtd--init-actionable-file (varname value)
  "Private function. VARNAME and VALUE get added to a symbol to initialize one of the org-gtd files."
  (org-gtd--init-gtd-file varname value 'actionable))

(defun org-gtd--init-inbox-file (varname value)
  "Private function. VARNAME and VALUE get added to a symbol to initialize one of the org-gtd files."
  (org-gtd--init-gtd-file varname value 'inbox))

(defun org-gtd--init-someday-file (varname value)
  "Private function. VARNAME and VALUE get added to a symbol to initialize one of the org-gtd files."
  (org-gtd--init-gtd-file varname value 'someday))

(defun org-gtd--init-timely-file (varname value)
  "Private function. VARNAME and VALUE get added to a symbol to initialize one of the org-gtd files."
  (org-gtd--init-gtd-file varname value 'timely))

(provide 'org-gtd)

;;; org-gtd.el ends here
