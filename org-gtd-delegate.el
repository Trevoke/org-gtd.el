;;; org-gtd-delegate.el --- logic to delegate items -*- lexical-binding: t; coding: utf-8 -*-
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
;; Item delegation logic for Org GTD.
;;
;;; Code:

(require 'org)

;;;###autoload
(defun org-gtd-delegate ()
  "Delegate item at point."
  (interactive)
  (let ((delegated-to (read-string "Who will do this? "))
        (org-inhibit-logging 'note))
    (org-set-property "DELEGATED_TO" delegated-to)
    (org-todo "WAIT")
    (org-schedule 0)
    (save-excursion
      (goto-char (org-log-beginning t))
      (insert (format "programmatically delegated to %s\n" delegated-to)))))

(provide 'org-gtd-delegate)
;;; org-gtd-delegate.el ends here
