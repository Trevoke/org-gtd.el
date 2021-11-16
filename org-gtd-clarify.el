;;; org-gtd-clarify.el --- Inbox management for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Minor mode used while clarifying an inbox item
;;
;;; Code:

(defvar org-gtd-clarify-map (make-sparse-keymap)
  "Keymap for function `org-gtd-clarify-user-input-mode', a minor mode.")

(define-minor-mode org-gtd-clarify-user-input-mode
  "Minor mode for org-gtd."
  nil "GTD " org-gtd-clarify-map
  (setq-local header-line-format
              (substitute-command-keys
               "\\<org-gtd-clarify-map>Clarify buffer.  Finish \
`\\[org-gtd-clarify-finalize]'.")))

(defun org-gtd-clarify--clarify-item ()
  "User interface to reflect on and clarify the current inbox item."
  (org-gtd-clarify-user-input-mode 1)
  (recursive-edit))

;;;###autoload
(defun org-gtd-clarify-finalize ()
  "Finalize the clarify process."
  (interactive)
  (org-gtd-clarify-user-input-mode -1)
  (exit-recursive-edit))

(provide 'org-gtd-clarify)
;;; org-gtd-clarify.el ends here
