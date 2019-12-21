;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Aldric Giacomoni

;; Author: Dmitry Gutov <trevoke@gmail.com>
;; Version: 0.1
;; URL: https://github.com/trevoke/org-gtd
;; Package-Requires: ((org-edna "1.0.2") (transient "20191206.1306"))

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
(require 'transient)

(setq org-edna-use-inheritance t)
(org-edna-load)

(defgroup org-gtd nil "Make it your own GTD")

(defcustom org-gtd-directory nil
  "The directory where the org files for GTD will live. Ends with a / .")

(defcustom org-gtd-projects-file "Projects.org"
  "Name of the file that holds the projects. Should end in .org .")

(defcustom org-gtd-inbox-file "Inbox.org"
  "Name of the file that holds the inbox. Should end in .org .")

(defcustom org-gtd-tickler-file "Tickler.org"
  "Name of the file that holds the tickler. Should end in .org .")

(defcustom org-gtd-someday-file "Someday.org"
  "Name of the file holding deferred thoughts (come back to this someday). Should end in .org .")

(defcustom org-gtd-incubate-file "Incubate.org"
  "Name of the file that holds the projects that are being put together. Should end in .org .")

(defcustom org-gtd-diary-file "diary.org"
  "The name Emacs will use for its diary features. Follow `diary-file' to read up on it.")

(defcustom org-gtd-fallback-file "capture-fallback.org"
  "The file in which Emacs will store notes if not otherwise configured.")

(defun org-gtd--path (file)
  "Private function. take FILE as the name of a file and return the full path assuming it is in the GTD framework."
  (concat (org-gtd--directory) file))

(defvar org-gtd-projects (org-gtd--path org-gtd-projects-file))

(setq org-agenda-files `((',(org-gtd--directory))))

(setq org-default-notes-file (org-gtd--path org-gtd-fallback-file))
(setq diary-file (org-gtd--path org-gtd-diary-file))
(setq org-agenda-diary-file 'diary-file)

(defvar org-refile-targets '())
(add-to-list 'org-refile-targets '((org-gtd-projects-file :maxlevel . 1)
                                   (org-gtd-incubate-file :maxlevel . 1)
                                   (org-gtd-tickler-file :maxlevel . 1)))


(setq org-agenda-include-diary t)
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-span 'day)

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-log-refile 'time)



(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(org-defkey org-mode-map "\C-cr" 'org-refile)



(defun org-gtd-init ()
  "Private function - initialize the org-gtd-directory variable."
  (interactive)
  (customize-save-variable
   'org-gtd-directory
   (file-name-as-directory (read-directory-name
                            "What is the root GTD directory? "
                            "~/"))))

(defun org-gtd--directory ()
  "Private function - get or initialize the org-gtd-directory variable."
  (or org-gtd-directory (org-gtd-init)))





(setq org-todo-keywords '( "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))

(setq org-capture-templates
      `(
        ("i" "Inbox"
         entry (file ,org-gtd-inbox-file)
         "* TODO %?\n  %i"
         :kill-buffer t)
        ("t" "Todo with link"
         entry (file ,org-gtd-inbox-file)
         "* TODO %?\n  %i\n  %a"
         :kill-buffer t)
        ("s" "Someday"
         entry (file ,org-gtd-someday-file)
         "* %i%? \n %U"
         :kill-buffer t)
        ("r" "Remind me"
         entry (file ,org-gtd-tickler-file)
         "* TODO %?\nSCHEDULED: %^{Remind me on:}t"
         :kill-buffer t)))


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


(defun org-gtd-refile ()
  "Custom refiling which includes setting a tag."
  (interactive)
  (org-set-tags-command)
  (org-refile))


;; ---------

(setq org-gtd-dir org-directory)
(setq org-gtd-next (concat org-gtd-dir "test-next.org"))
(setq org-gtd-projects (concat org-gtd-dir "test-projects.org"))
(setq org-gtd-references (concat org-gtd-dir "test-references.org"))
(setq org-gtd-incubate (concat org-gtd-dir "test-incubate.org"))
(setq org-gtd-calendar (concat org-gtd-dir "test-calendar.org"))

(setq org-refile-targets '((org-gtd-projects :maxlevel . 1)
                           (org-gtd-calendar :maxlevel . 1)
                           (org-gtd-next :maxlevel . 1)
                           (org-gtd-incubate :maxlevel . 1)))

;(add-to-list 'org-refile-targets `(,org-gtd-next :level 1))


;; TODO - update statistics cookies in project file
;; (org-update-statistics-cookies t)
(defun org-gtd-process-inbox ()
  "Use this once a day: process every element in the inbox."
  (interactive)
  (require 'winner)
  (pop-to-buffer-same-window "test.org")
  (delete-other-windows)
  (org-map-entries
   (lambda ()
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point)))
     (org-narrow-to-element)
     (org-gtd--process-inbox-item)
     (widen)
     (org-archive-subtree)))
  (winner-undo))


(define-suffix-command org-gtd--schedule ()
  (interactive)
  (org-schedule nil)
  (org-refile nil nil `("One-off" ,org-gtd-calendar)))


(define-suffix-command org-gtd--when-i-can ()
  (interactive)
  (org-todo "NEXT")
  (org-refile nil nil `("" ,org-gtd-next)))

(define-suffix-command org-gtd--quick-item ()
  (interactive)
  (org-todo "DONE")
  (org-archive-subtree))

(define-suffix-command org-gtd--trash ()
  (interactive)
  (org-todo "CANCELED")
  (org-archive-subtree))

(define-transient-command org-gtd--process-an-item ()
  "this is my super duper docstring"
  ["Actionable"
   ;; actionable
   ("q" "< 2 minutes, done!" org-gtd--quick-item)
   ("n" "To do when I can" org-gtd--when-i-can)
   ;("d" "Delegate" org-gtd--delegate)
   ("s" "Schedule" org-gtd--schedule)
   ;("p" "Project" org-gtd--project)
   ]
   ;; not actionable
[  "Non actionable" ("t" "Trash" org-gtd--trash)
    ;("i" "Incubate" )
    ;("r" "Reference" )
    ]

  )


(provide 'org-gtd)

;;; org-gtd.el ends here
