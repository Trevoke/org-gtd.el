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
  :version "0.1"
  :group 'org)

(defcustom org-gtd-directory "~/gtd/"
  "Directory of Org based GTD files.
This is the directory where to look for the files used in
this Org-mode based GTD implementation."
  :version "0.1"
  :type 'directory)

(defcustom org-gtd-process-item-hooks '(org-set-tags-command)
  "Enhancements to add to each item as they get processed from the inbox."
  :version "1.0.4"
  :type 'hook
  :options '(org-set-tags-command org-set-effort org-priority))

(provide 'org-gtd-customize)
