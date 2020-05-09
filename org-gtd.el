;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Version: 0.2
;; URL: https://github.com/trevoke/org-gtd
;; Package-Requires: ((emacs "26.1") (org-edna "1.0.2") (f "0.20.0") (org "9.3.1") (org-agenda-property "1.3.1"))

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

;; This package tries to replicate as closely as possible the GTD workflow.
;; This package, and this readme, assume familiarity with GTD. There are many
;; resources out there to learn how to use the framework. If you are new to GTD,
;; this package may be unpleasant to use.
;;
;; Assuming the keybindings below are used, here is how you could use org-gtd:
;; GTD uses one basic axiom: everything that comes your way goes into the inbox.
;; You do this with ~C-c d c~.
;; You also have to regularly process the inbox, which you do with ~C-c d p~.
;;
;; When you process the inbox, you will see each inbox item, one at a time,
;; with an interface letting you decide what to do with the item:
;;
;; - *Quick Action* :: You've taken care of this action just now. Choose this to mark the item as DONΕ and archive it.
;; - *Throw out* :: This is not actionable and it's not knowledge for later. Choose this to mark the item as CANCELED and archive it.
;; - *Project* :: This is a multi-step action. I'll describe how to handle these below.
;; - *Calendar* :: This is a single item to be done at a given date or time. You'll be presented with org-mode's date picker, then it'll refile the item. You'll find this in the agenda later.
;; - *Delegate* :: Let someone else do this. Write the name of the person doing it, and choose a time to check up on that item.
;; - *Single action* :: This is a one-off to be done when possible. You can add tags to help you.
;; - *Reference* :: This is knowledge to be stored away. I'll describe how to handle these below.
;; - *Incubate* :: no action now, review later
;;
;; When processing each item you'll get a chance to add tags and other such
;; metadata. This package will add keywords (e.g. NEXT, TODO, DONE) for you,
;; so don't worry about them. Do the work that only you can do, and let this
;; package handle the bookkeeping.
;;
;; A "project" is defined as an org heading with a set of children headings.
;;
;; When you are processing the inbox and creating a project, Emacs enters a
;; recursive edit mode to let you define and refine the project.
;; When finished, press ~C-c C-c~ to exit the recursive edit and go back to
;; processing the inbox.
;;
;; One of the ways to see what's next for you to do is to see all the next
;; actions ( ~C-c d n~ ).
;;
;; Sometimes things break. Use ~C-c d s~ to find all projects that don't have a
;; NEXT item, which is to say, all projects that the package will not surface
;; and help you finish.
;;
;; Here's a commented block showing a possible
;; configuration for this package.
;;
;;   ;; these are the interactive functions you're likely to want to use as you go about GTD.
;;   (global-set-key (kbd "C-c d c") 'org-gtd-capture) ;; add item to inbox
;;   (global-set-key (kbd "C-c d p") 'org-gtd-process-inbox) ;; process entire inbox
;;   (global-set-key (kbd "C-c d a") 'org-agenda-list) ;; see what's on your plate today
;;   (global-set-key (kbd "C-c d n") 'org-gtd-show-all-next) ;; see all NEXT items
;;   (global-set-key (kbd "C-c d s") 'org-gtd-show-stuck-projects) ;; see projects that don't have a NEXT item
;;
;;   (setq org-gtd-directory "~/gtd/") ;; where org-gtd will put its files
;;   ;; the above happens to also be the default location, if left uncustomized.
;;
;;   ;; assuming you don't have another setup, use this line as written
;;   ;; otherwise, push the org-gtd-directory to your existing agenda files
;;   (setq org-agenda-files `(,org-gtd-directory))
;;
;;   ;; assuming you don't have existing capture templates
;;   ;; otherwise, push these to your existing capture templates
;;   ;; and of course, you can adjust the keys "i" and "l"
;;   (setq org-capture-templates `(("i" "GTD item"
;;                                  entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
;;                                  "* %?\n%U\n\n  %i"
;;                                  :kill-buffer t)
;;                                 ("l" "GTD item with link to where you are in Emacs now"
;;                                  entry (file ,(org-gtd--path org-gtd-inbox-file-basename))
;;                                  "* %?\n%U\n\n  %i\n  %a"
;;                                  :kill-buffer t)))
;;
;;   ;; package: https://www.nongnu.org/org-edna-el/
;;   ;; org-edna is used to make sure that when a project task gets DONE,
;;   ;; the next TODO is automatically changed to NEXT.
;;   (setq org-edna-use-inheritance t)
;;   (org-edna-load)
;;
;;   ;; package: https://github.com/Malabarba/org-agenda-property
;;   ;; this is so you can see who an item was delegated to in the agenda
;;   (setq org-agenda-property-list '("DELEGATED_TO"))
;;   ;; I think this makes the agenda easier to read
;;   (setq org-agenda-property-position 'next-line)

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'org)
(require 'org-edna)
(require 'org-agenda-property)

(defvar org-stuck-projects)

(defconst org-gtd--package-path (f-dirname (f-this-file)))

(defconst org-gtd-actionable-file-basename "actionable")
(defconst org-gtd-inbox-file-basename      "inbox")
(defconst org-gtd-incubate-file-basename   "incubate")

(defconst org-gtd-actions   ".*Actions")
(defconst org-gtd-delegated ".*Delegated")
(defconst org-gtd-incubate  ".*Incubate.*")
(defconst org-gtd-scheduled ".*Scheduled")
(defconst org-gtd-projects  ".*Projects")

(defconst org-gtd-stuck-projects '("+LEVEL=2-DONE+CATEGORY=\"Projects\""
                                   ("TODO" "NEXT" "WAIT")
                                   nil
                                   ""))

(defconst org-gtd-complete-projects "+LEVEL=2+CATEGORY=\"Projects\"")

(defconst org-gtd-inbox-template
  "#+STARTUP: overview hidestars logrefile indent logdone
#+TODO: NEXT TODO WAIT | DONE CANCELED TRASH
#+begin_comment
This is the inbox. Everything goes in here when you capture it.
#+end_comment
")

(defconst org-gtd-actionable-template
  "#+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CANCELED(c@)

* Actions
:PROPERTIES:
:CATEGORY: Action
:END:

* Delegated
:PROPERTIES:
:CATEGORY: Delegated
:END:

* Scheduled
:PROPERTIES:
:CATEGORY: Scheduled
:END:

* Projects
:PROPERTIES:
:TRIGGER:  next-sibling todo!(NEXT)
:CATEGORY: Projects
:END:
")

(defconst org-gtd-incubate-template
  "#+begin_comment
Here go the things you want to think about someday. Review this file as often
as you feel the need: every two months? Every six months? Every year?
It's suggested that you categorize the items in here somehow, such as:
\"to read\", \"to buy\", \"to eat\", etc - whatever works best for your mind!
#+end_comment
")

(defun org-gtd--refile-targets ()
  "Return the refile targets specific to org-gtd."
  `((,(org-gtd--path org-gtd-incubate-file-basename) :maxlevel . 2)
    (,(org-gtd--path org-gtd-actionable-file-basename) :maxlevel . 1)))

(defgroup org-gtd nil "Customize the org-gtd package."
  :version 0.1 :group 'emacs)

(defcustom org-gtd-directory "~/gtd/"
  "The directory where the org files for GTD will live."
  :type 'directory)

(defun org-gtd-capture ()
  "Wrap `org-capture' to make sure the gtd inbox exists."
  (interactive)
  (kill-buffer (org-gtd--inbox-file))
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

  (set-buffer (org-gtd--inbox-file))
  (display-buffer-same-window (org-gtd--inbox-file) '())
  (delete-other-windows)

  ;; laugh all you want, all this statefulness is killing me.
  (org-gtd--actionable-file)
  (org-gtd--incubate-file)

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
   `(,(org-gtd--actionable-file) ,(org-gtd--incubate-file) ,(org-gtd--inbox-file))))

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
            (?a "archive this knowledge" "Store this where you store knowledge")
            (?i "incubate it" "I'll come back to this later")))))

    (cl-case (car action)
      (?q (org-gtd--quick-action))
      (?t (org-gtd--trash))
      (?p (org-gtd--project))
      (?c (org-gtd--calendar))
      (?d (org-gtd--delegate))
      (?s (org-gtd--single-action))
      (?a (org-gtd--archive))
      (?i (org-gtd--incubate)))))

(defun org-gtd--incubate ()
  "Process element and move it to the incubate file."
  (org-gtd--edit-item)
  (goto-char (point-min))
  (org-set-tags-command)
  (org-schedule 0)
  (org-refile nil nil (org-gtd--refile-target org-gtd-incubate)))

(defun org-gtd--archive ()
  "Process element - completely user-defined action. Store this as a reference.

Do not remove the item from the inbox, it will be archived."
  (org-gtd--edit-item)
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

  (with-current-buffer (org-gtd--actionable-file)
    (org-update-statistics-cookies t)))

(defun org-gtd--refile-target (heading-regexp)
  "Refile to one of the `org-gtd' refile locations.

HEADING-REGEXP is a regular expression. See `org-refile'."
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

  (cl-destructuring-bind
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

;;; file management

(defun org-gtd--path (file)
  "Return the full path to FILE.org assuming it is in the GTD framework."
  (f-join org-gtd-directory (concat file ".org")))

(defun org-gtd--gtd-file (gtd-type)
  "Return a buffer for GTD-TYPE.org.

create the file and template first if it doesn't already exist."
  (let* ((file-path (org-gtd--path gtd-type))
         (file-buffer (find-file-noselect file-path)))
    (or (f-file-p file-path)
        (with-current-buffer file-buffer
          (insert (symbol-value
                   (intern
                    (string-join
                     `("org-gtd-" ,gtd-type "-template")))))
          (save-buffer)))
    file-buffer))

(defun org-gtd--actionable-file ()
  "Create or return the buffer for the actionable GTD buffer."
  (org-gtd--gtd-file org-gtd-actionable-file-basename))

(defun org-gtd--inbox-file ()
  "Create or return the buffer for the inbox GTD buffer."
  (org-gtd--gtd-file org-gtd-inbox-file-basename))

(defun org-gtd--incubate-file ()
  "Create or return the buffer for the incubate GTD buffer."
  (org-gtd--gtd-file org-gtd-incubate-file-basename))

(defun org-gtd--edit-item ()
  "Friendly user interaction to refine the current inbox item."
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
