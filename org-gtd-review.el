;;; org-gtd-review.el --- GTD review logic for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Reviews are a crucial part of GTD. This code determines how to use
;; the agenda views for review purposes.
;;
;;; Code:

(require 'org)
(require 'org-gtd-areas-of-focus)
(require 'org-gtd-agenda)

(defun org-gtd-review-area-of-focus (&optional area)
  ""
  (interactive (list (completing-read
                      "Which area of focus would you like to review? "
                      org-gtd-areas-of-focus
                      nil
                      t)))
  (if (not (member area org-gtd-areas-of-focus))
      (print (format "`%s' is not a member of %s" area org-gtd-areas-of-focus))
    (message "fuck right off")))

(provide 'org-gtd-review)
;;; org-gtd-review.el ends here
