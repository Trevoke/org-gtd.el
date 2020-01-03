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

(defconst org-gtd--package-path (f-dirname (f-this-file)))

(defconst org-gtd-actionable "actionable")
(defconst org-gtd-inbox "inbox")
(defconst org-gtd-someday "someday")
(defconst org-gtd-timely "timely")

(defgroup org-gtd nil "Customize the org-gtd package."
  :version 0.1 :group 'emacs)

(defcustom org-gtd-directory "~/gtd/"
  "The directory where the org files for GTD will live."
  :type 'directory)

(defun org-gtd--path (file)
  "Return the full path to FILE.org assuming it is in the GTD framework."
  (f-join org-gtd-directory (concat file ".org")))

(defun org-gtd--template-path (file)
  "Return full path to FILE_template.org."
  (f-join org-gtd--package-path
	  (concat file "_template.org")))

(defun org-gtd--gtd-file (gtd-type)
  "Return a buffer for GTD-TYPE.org. create the file and template first if it doesn't already exist."
  (let* ((file-path (org-gtd--path org-gtd-actionable))
	 (file-buffer (find-file-noselect file-path)))
    (or (f-file-p file-path)
	(with-current-buffer file-buffer
	  (insert-file-contents (org-gtd--template-path org-gtd-actionable ) nil nil nil t)
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

(defun org-gtd--timely ()
  "Create or return the buffer for the timely GTD buffer."
  (org-gtd--gtd-file org-gtd-timely))

(defun org-gtd--project-buffer ()
  "Get or create the buffer to transform an inbox item into a project."
  (get-buffer-create "*org-gtd-project*"))

(defun org-gtd-show-all-next ()
  "Show all the NEXT items in a single list."
  (interactive)
  (org-todo-list "NEXT"))

(defun org-gtd-show-stuck-projects ()
  (interactive)
  (let* ((user-stuck-projects org-stuck-projects)
	 (org-stuck-projects '("+LEVEL=2-notproject/-DONE"
			       ("TODO" "NEXT" "WAIT")
			       nil ""))
	 (stuck-projects-buffer (org-agenda-list-stuck-projects))
	 (org-stuck-projects user-stuck-projects))
    stuck-projects-buffer))

(defun org-gtd-process-inbox ()
  "Use this once a day: process every element in the inbox."
  (interactive)
  (let ((inbox-buffer (find-file-noselect org-gtd-inbox)))
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
  "INBOX-BUFFER is the buffer with the org gtd inbox."
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
  "Process element and move it to the tickler file."
  (move-end-of-line 1)
  (open-line 1)
  (forward-line)
  (org-time-stamp nil)
  (org-refile nil nil (org-gtd--refile-target ".*Reminders")))

(defun org-gtd--later ()
  "Process element and move it to the someday file."
  (move-end-of-line 1)
  (open-line 1)
  (forward-line)
  (org-time-stamp nil)
  (org-refile))

(defun org-gtd--reference ()
  "Process element and move it to the brain."
  (org-brain-add-resource nil nil "What's the link? " nil)
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--garbage ()
  "Archive element."
  (org-todo "CANCELED")
  (org-archive-subtree))

(defun org-gtd--whenever ()
  "Process element and move it to the Actionable file."
  (org-set-tags-command)
  (org-todo "NEXT")
  (org-refile nil nil (org-gtd--refile-target ".*One-Offs")))

(defun org-gtd--delegate ()
  "Process element and move it to the Actionable file."
  (org-todo "WAIT")
  (org-set-property "DELEGATED_TO" (read-string "Who will do this? "))
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target ".*Delegated")))

(defun org-gtd--schedule ()
  "Process element and move it to the tickler file."
  (org-todo "TODO")
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target ".*Scheduled")))

(defun org-gtd--quick-action ()
  "Process element and archive it."
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--project (inbox-buffer)
  "Process element and transform it into a project. INBOX-BUFFER is the buffer holding the org-gtd inbox."
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
  "HEADING-REGEXP is a regular expression for one of the desired GTD refile locations. See `org-refile'."
  (let* ((user-refile-targets org-refile-targets)
	 (org-refile-targets `((,(org-gtd--path org-gtd-someday) :maxlevel . 2)
			     (,(org-gtd--path org-gtd-actionable) :maxlevel . 1)
			     (,(org-gtd--path org-gtd-timely) :maxlevel . 1)))
	 (results   (cl-find-if
		     (lambda (rfloc)
		       (string-match heading-regexp
				     (car rfloc)))
		     (org-refile-get-targets)))
	 (org-refile-targets user-refile-targets))
    results))

(defun org-gtd-init ()
  "Initialize the org-gtd package based on configuration."
  (interactive)



)

(provide 'org-gtd)

;;; org-gtd.el ends here
