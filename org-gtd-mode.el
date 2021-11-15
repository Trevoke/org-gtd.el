;;; org-gtd-mode --- global minor mode for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Archiving logic for org-gtd
;;
;;; Code:

;;;###autoload
(define-minor-mode org-gtd-mode
  "global minor mode to force org-agenda to be bounded to the org-gtd settings"
  :lighter " GTD"
  :global t
  (if org-gtd-mode
      (org-gtd--override-agenda)
    (org-gtd--restore-agenda)))

(defun org-gtd--wrap (fun &rest r)
  (with-org-gtd-context (apply fun r)))

(defun org-gtd--override-agenda ()
  (advice-add 'org-agenda :around #'org-gtd--wrap)
  (advice-add 'org-agenda-list-stuck-projects :around #'org-gtd--wrap))

(defun org-gtd--restore-agenda ()
  (advice-remove 'org-agenda #'org-gtd--wrap)
  (advice-remove 'org-agenda-list-stuck-projects #'org-gtd--wrap))

(provide 'org-gtd-mode)
;;; org-gtd-mode ends here
