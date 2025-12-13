;;; org-gtd-someday-review.el --- Review someday/maybe items -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

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
;; Iterative review of someday/maybe items.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-gtd-core)

;;;; Functions

;;;;; Private

(defun org-gtd-someday-review--add-reviewed-entry ()
  "Add a 'Reviewed' entry to the LOGBOOK drawer at point."
  (save-excursion
    (org-back-to-heading t)
    (let* ((drawer-pos (org-log-beginning t))
           (has-drawer (save-excursion
                         (goto-char drawer-pos)
                         (looking-back ":LOGBOOK:\n" (line-beginning-position 0)))))
      (goto-char drawer-pos)
      (unless has-drawer
        (insert ":LOGBOOK:\n")
        (save-excursion
          (insert ":END:\n")))
      (insert (format "- Reviewed %s\n"
                      (format-time-string "[%Y-%m-%d %a %H:%M]"))))))

;;;; Footer

(provide 'org-gtd-someday-review)

;;; org-gtd-someday-review.el ends here
