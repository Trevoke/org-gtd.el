;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Version: 0.1
;; URL: https://github.com/trevoke/org-gtd
;; Package-Requires: ((org-edna "1.0.2") (org-brain "0.8"))

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
;; Highly opinionated because it follows the pure idea of the GTD framework.
;; Destructive because it overrides a number of org-level variables, so if you
;; have some settings you want to keep, this might make you unhappy.

;;; Code:

(require 'org-edna)

(setq org-edna-use-inheritance t)
(org-edna-load)

(defgroup org-gtd nil "Make it your own GTD")

(defun org-gtd--directory ()
  "Private function - get or initialize the org-gtd-directory variable."
  (or org-gtd-directory (org-gtd-init)))

(defun org-gtd--path (file)
  "Private function. take FILE as the name of a file and return the full path assuming it is in the GTD framework."
  (concat (org-gtd--directory) file))

(defun org-gtd--set-file-path (filename value)
  "Private function. takes FILENAME and VALUE."
  (set-default filename value)
  (let ((var (intern (replace-regexp-in-string
                      "-file"
                      ""
                      (symbol-name filename)))))
    (set var (org-gtd--path value))))


;; TODO changing the directory should change the value of all the paths
(defcustom org-gtd-directory nil
  "The directory where the org files for GTD will live. Ends with a /."
  :risky t
  :group 'org-gtd
  :type 'directory)

(defcustom org-gtd-projects-file "Projects.org"
  "Name of the file that holds the projects. Should end in .org."
  :risky t
  :group 'org-gtd
  :type 'file
  :set-after '(org-gtd-directory)
  :set #'org-gtd--set-file-path)

(defcustom org-gtd-inbox-file "Inbox.org"
  "Name of the file that holds the inbox. Should end in .org."
  :risky t
  :group 'org-gtd
  :type 'file
  :set-after '(org-gtd-directory)
  :set #'org-gtd--set-file-path)

(defcustom org-gtd-tickler-file "Tickler.org"
  "Name of the file that holds the tickler. Should end in .org."
  :risky t
  :group 'org-gtd
  :type 'file
  :set-after '(org-gtd-directory)
  :set #'org-gtd--set-file-path)

(defcustom org-gtd-someday-file "Someday.org"
  "Name of the file holding deferred thoughts (come back to this someday). Should end in .org."
  :risky t
  :group 'org-gtd
  :type 'file
  :set-after '(org-gtd-directory)
  :set #'org-gtd--set-file-path)

(defun org-gtd--project-buffer ()
  "Private function. Get or create the buffer to transform an inbox item into a project."
  (get-buffer-create "*org-gtd-project*"))

(setq org-agenda-window-setup 'other-window)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 'day)

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-log-refile 'time)

(setq org-todo-keywords '("TODO(t)"
                          "NEXT(n)"
                          "WAIT(w@/!)"
                          "|"
                          "DONE(d!)"
                          "CANCELED(c@)"))

(setq org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((agenda "" nil)
          (alltodo "" nil))
         nil)
        ("N" "All NEXT actions" todo "NEXT"
         ((org-agenda-overriding-header "")))))

(setq org-stuck-projects '("+LEVEL=1/-DONE"
                           ("TODO" "NEXT" "NEXTACTION")
                           nil ""))

(defun org-gtd-init ()
  "Initialize the org-gtd package based on configuration."
  (interactive)
  (defvar org-gtd-directory)
  (customize-save-variable 'org-gtd-directory
                           (file-name-as-directory (read-directory-name
                                                    "What is the root GTD directory? "
                                                    "~/")))

  (defvar org-gtd-projects (org-gtd--path org-gtd-projects-file))

  (setq org-agenda-files `((',(org-gtd--directory))))

  (setq org-refile-targets '((org-gtd-projects :maxlevel . 1)
                             (org-gtd-someday :maxlevel . 1)
                             (org-gtd-tickler :maxlevel . 1)))

  (setq org-capture-templates
        `(
          ("i" "Inbox"
           entry (file ,org-gtd-inbox)
           "* TODO %?\n  %i"
           :kill-buffer t)
          ("t" "Todo with link"
           entry (file ,org-gtd-inbox)
           "* TODO %?\n  %i\n  %a"
           :kill-buffer t))))

;; TODO - update statistics cookies in project file
;; (org-update-statistics-cookies t)
;; (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))
;; (org-set-tags-command)

  ;; (org-map-entries
  ;;  (lambda ()

  ;;    (org-narrow-to-element)
  ;;    (org-gtd--process-an-item)
  ;;    (widen)
  ;;    (org-archive-subtree))
  ;;  nil
  ;;  `(,org-gtd-inbox))

(defun org-gtd-process-inbox ()
  "Use this once a day: process every element in the inbox."
  (interactive)
  (let ((inbox-buffer (progn (find-file org-gtd-inbox)
                             (get-file-buffer org-gtd-inbox))))
    (set-buffer org-gtd-inbox-buffer)
    (display-buffer-same-window org-gtd-inbox-buffer '())

    (goto-char (point-min))
    (org-next-visible-heading 1)
    (org-narrow-to-element)
    (org-gtd--process-inbox-element inbox-buffer)
    (widen)))

(defun org-gtd--process-inbox-element (inbox-buffer)
  (setq action
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
           (?t "tickler" "I need to be reminded of this at a given time"))))
  (case (car action)
    (?q (org-gtd--quick-action))
    (?p (org-gtd--project inbox-buffer))
    (?s (org-gtd--schedule))
    (?d (org-gtd--delegate))
    (?w (org-gtd--whenever))
    (?g (org-gtd--garbage))
    (?r (org-gtd--reference))
    (?l (org-gtd--later))
    (?t (org-gtd--tickler))
    )
  )

(defun org-gtd--tickler ()
  (org-time-stamp)
  (org-refile nil nil `("" ,org-gtd-tickler)))

;; TODO should be categorizable, e.g. to read, to watch, to eat
;; so maybe let user choose where to refile in the someday file
;; and maybe add tags
(defun org-gtd--later ()
  (org-time-stamp)
  (org-refile nil nil `("" ,org-gtd-someday)))

(defun org-gtd--reference ()
  (org-brain-add-resource nil nil "What's the link? " nil)
  (org-todo "DONE")
  (org-archive-subtree)
  )

(defun org-gtd--garbage ()
  (org-todo "CANCELED")
  (org-archive-subtree))

;; TODO this file is not customizable yet
;; And it's here because I think of the edna customization in projects
;; as being file-wide but I think I can do it per-header
(defun org-gtd--whenever ()
  (org-set-tags)
  (org-todo "NEXT")
  (org-refile nil nil '("" "/tmp/single-actions.org")))

;; TODO is this a good idea? This could be non-private, could be applicable to single actions or actions in projects
;; Maybe I don't create another file for this
;; After all I can search by property across files
(defun org-gtd--delegate ()
  (org-todo "WAIT")
  (org-set-property "DELEGATED_TO" (read-string "Who will do this? "))
  (org-schedule 0)
  (org-refile nil nil '("" "/tmp/delegated.org")))

;; TODO this file isn't customizable
(defun org-gtd--schedule ()
  (org-todo "TODO")
  (org-schedule 0)
  (org-refile nil nil '("" "/tmp/org-cal.org")))

(defun org-gtd--quick-action ()
  (org-todo "DONE")
  (org-archive-subtree))

(defun org-gtd--project (inbox-buffer)
  (save-excursion
    (set-buffer (org-gtd--project-buffer))
    (erase-buffer)
    (display-buffer-same-window (org-gtd-project-buffer) '())
    (insert-buffer inbox-buffer)
    (recursive-edit)
    (goto-char (point-min))
    (org-refile nil nil `("" ,org-gtd-projects)))

  (display-buffer-same-window inbox-buffer '())
  (org-archive-subtree))


(provide 'org-gtd)

;;; org-gtd.el ends here
