;; -*- lexical-binding: t; -*-
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
  "Directory of Org based GTD files.
This is the directory where to look for the files used in
this Org-mode based GTD implementation."
  :group 'org-gtd
  :package-version "0.1"
  :type 'directory)

(defcustom org-gtd-process-item-hooks '(org-set-tags-command)
  "Enhancements to add to each item as they get processed from the inbox."
  :group 'org-gtd
  :package-version "1.0.4"
  :type 'hook
  :options '(org-set-tags-command org-set-effort org-priority))

(defcustom org-gtd-archive-location
  (lambda ()
   (let ((year (number-to-string (caddr (calendar-current-date)))))
     (string-join `("gtd_archive_" ,year "::datetree/"))))
  "Function to generate archive location for org gtd"
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
  "Capture templates to be used when adding something to the inbox"
  :group 'org-gtd
  :type 'sexp
  :package-version "2.0.0")

(provide 'org-gtd-customize)
