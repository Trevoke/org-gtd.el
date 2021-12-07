;;; org-gtd-customize.el --- Custom variables for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2021 Aldric Giacomoni

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
;; User-customizable options for org-gtd.
;;
;;; Code:

(defgroup org-gtd nil
  "Customize the org-gtd package."
  :link '(url-link "https://github.com/Trevoke/org-gtd.el")
  :package-version "0.1"
  :group 'org)

(defcustom org-gtd-directory "~/gtd/"
  "Directory for org-gtd.

The package will use this directory for all its functionality, whether it is
building the agenda or refiling items."
  :group 'org-gtd
  :package-version "0.1"
  :type 'directory)

(defcustom org-gtd-process-item-hooks '(org-set-tags-command)
  "Enhancements to add to each item as they get processed from the inbox.

This is a list of functions that modify an org element. Some built-in examples
are provided. You can create your own functions and add them to that list"
  :group 'org-gtd
  :package-version "1.0.4"
  :type 'hook
  :options '(org-set-tags-command org-set-effort org-priority))

(defcustom org-gtd-archive-location
  (lambda ()
   (let ((year (number-to-string (caddr (calendar-current-date)))))
     (string-join `("gtd_archive_" ,year "::datetree/"))))
  "Function to generate archive location for org gtd.

This function takes zero arguments. By default this generates a file
called gtd_archive_<currentyear> in org-gtd-directory and puts the entries
into a datetree."
  :group 'org-gtd
  :type 'sexp
  :package-version "2.0.0")

(defcustom org-gtd-capture-templates
  '(("i" "Inbox"
     entry (file (lambda () (org-gtd-inbox-path)))
                 "* %?\n%U\n\n  %i"
                 :kill-buffer t)
     ("t" "Todo with link"
      entry (file (lambda () (org-gtd-inbox-path)))
      "* %?\n%U\n\n  %i\n  %a"
      :kill-buffer t))
  "Capture templates to be used when adding something to the inbox.

The safe thing to modify here is the string template. Make sure there is a
top-level heading in your template (it starts with a single asterisk) or org-gtd
cannot be guaranteed to work well."
  :group 'org-gtd
  :type 'sexp
  :package-version "2.0.0")

(defcustom org-gtd-agenda-custom-commands
  '(("g" "Scheduled today and all NEXT items"
     ((agenda "" ((org-agenda-span 1))) (todo "NEXT|WAIT"))))
  "Agenda custom commands to be used for org-gtd."
  :group 'org-gtd
  :type 'sexp
  :package-version "2.0.0")

(defcustom org-gtd-refile-to-any-target t
  "Set this to true if you do not need to choose where to refile processed items.

Defaults to true to carry over pre-2.0 behavior. You will need to change this
settig if you follow the instructions to add your own refile targets."
  :group 'org-gtd
  :type 'boolean
  :package-version "2.0.0")

(provide 'org-gtd-customize)
;;; org-gtd-customize.el ends here
