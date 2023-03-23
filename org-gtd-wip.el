;;; org-gtd-wip.el --- manage WIP -*- lexical-binding: t; coding: utf-8 -*-
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
;; Code to manage WIP as a state. Since emacs is a GUI and we must provide
;; the user with as much control as possible, we must allow for interruptions
;; in the flow, and that means we must allow the user to resume their work.
;;
;;; Code:

(defconst org-gtd-wip--prefix "Org-GTD WIP")

(defvar-local org-gtd--window-config nil
  "Store window configuration prior to clarifying task")

(defvar-local org-gtd--stuff-marker nil
  "Store marker to item that is being clarified")

(defun org-gtd-wip--buffer-name (id)
  "Retrieve the name of the WIP buffer for this particular ID."
  (format "*%s: %s*" org-gtd-wip--prefix id))

(defun org-gtd-wip-select ()
  "Prompt the user to choose one of the existing WIP buffers."
  (interactive)
  (let ((buf-names (mapcar #'buffer-name (org-gtd-wip--get-buffers))))
    (if buf-names
        (let ((chosen-buf-name (completing-read "Choose a buffer: " buf-names)))
          (get-buffer chosen-buf-name))
      (message "There are no Org-GTD WIP buffers."))))

(defun org-gtd-wip--get-buffers ()
  "Retrieve a list of Org GTD WIP buffers."
  (seq-filter (lambda (buf)
                (string-match-p org-gtd-wip--prefix (buffer-name buf)))
              (buffer-list)))

(defun org-gtd-wip--get-buffer ()
  "Get or create a WIP buffer for heading at point."
  (org-gtd-id-get-create)
  (let ((buffer (get-buffer-create (org-gtd-wip--buffer-name (org-id-get)))))
    (with-current-buffer buffer
      (org-mode)
      (org-gtd-process-mode 1))
    buffer))

(provide 'org-gtd-wip)
;;; org-gtd-wip.el ends here
