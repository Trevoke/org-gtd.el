;;; org-gtd-clarify.el --- Handle clarifying tasks -*- lexical-binding: t; coding: utf-8 -*-
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
;; Set up emacs to helpfully clarify tasks so they can then be organized.
;;
;;; Code:

(require 'org-gtd-id)

(defconst org-gtd-clarify--prefix "Org-GTD WIP")

(defvar-local org-gtd-clarify--window-config nil
  "Store window configuration prior to clarifying task")

(defvar-local org-gtd-clarify--stuff-marker nil
  "Store marker to item that is being clarified")

(defvar-local org-gtd-clarify--clarify-id nil
  "Reference to the org id of the heading currently in the WIP buffer")

(defvar-local org-gtd-clarify--inbox-p nil
  "Used to separate a one-off clarify from the inbox clarification.")

;;;###autoload
(defvar org-gtd-clarify-map (make-sparse-keymap)
  "Keymap for command `org-gtd-clarify-mode', a minor mode.")

;;;###autoload
(define-minor-mode org-gtd-clarify-mode
  "Minor mode for org-gtd."
  :lighter " GPM"
  :keymap org-gtd-clarify-map
  (if org-gtd-clarify-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-gtd-clarify-map>Clarify item.  Use `\\[org-gtd-organize]' to file it appropriately when finished."))
    (setq-local header-line-format nil)))

;;;###autoload
(defun org-gtd-clarify-item ()
  "Process item at point through org-gtd."
  (interactive)
  (let ((processing-buffer (org-gtd-clarify--get-buffer))
        (window-config (current-window-configuration))
        (stuff-marker (point-marker)))
    (message "Entering org-gtd-clarify-item")
    (when (= (buffer-size processing-buffer) 0)
      (org-copy-subtree)
      (with-current-buffer processing-buffer
        (message ".................................................")
        (mapc (lambda (x) (message "---------\n %s \n-----------" x)) kill-ring)
        (message ".................................................")

        (org-paste-subtree)))
    (message "stuffmarker is: %s" stuff-marker)
    (with-current-buffer processing-buffer
      (setq-local org-gtd-clarify--window-config window-config
                org-gtd-clarify--stuff-marker stuff-marker
                org-gtd-clarify--clarify-id (org-id-get)))
    (set-buffer processing-buffer)
    (display-buffer processing-buffer)
    (delete-other-windows (get-buffer-window processing-buffer))))

;;;###autoload
(defun org-gtd-clarify-inbox-item ()
  "Process item at point through org-gtd. This function is called through the
inbox clarification process."
  (interactive)
  (org-gtd-clarify-item)
  (setq-local org-gtd-clarify--inbox-p t))

;;;###autoload
(defun org-gtd-clarify-select ()
  "Prompt the user to choose one of the existing WIP buffers."
  (interactive)
  (let ((buf-names (mapcar #'buffer-name (org-gtd-clarify--get-buffers))))
    (if buf-names
        (let ((chosen-buf-name (completing-read "Choose a buffer: " buf-names)))
          (get-buffer chosen-buf-name))
      (message "There are no Org-GTD WIP buffers."))))

(defun org-gtd-clarify--buffer-name (id)
  "Retrieve the name of the WIP buffer for this particular ID."
  (format "*%s: %s*" org-gtd-clarify--prefix id))

(defun org-gtd-clarify--get-buffers ()
  "Retrieve a list of Org GTD WIP buffers."
  (seq-filter (lambda (buf)
                (string-match-p org-gtd-clarify--prefix (buffer-name buf)))
              (buffer-list)))

(defun org-gtd-clarify--get-buffer ()
  "Get or create a WIP buffer for heading at point."
  (org-gtd-id-get-create)
  (let ((buffer (get-buffer-create (org-gtd-clarify--buffer-name (org-id-get)))))
    (with-current-buffer buffer
      (let ((org-todo-keywords '((sequence "NEXT" "TODO" "WAIT" "|" "DONE" "CNCL" "TRASH"))))
        (org-mode)
        (org-gtd-clarify-mode 1))
      buffer)))

(provide 'org-gtd-clarify)
;;; org-gtd-clarify.el ends here
