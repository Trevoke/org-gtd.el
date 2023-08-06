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
;; Set up Emacs to helpfully clarify tasks so they can then be organized.
;;
;;; Code:

;;;; Requirements

(require 'org-agenda)

(require 'org-gtd-core)
(require 'org-gtd-wip)
(require 'org-gtd-horizons)

;;;; Customization

(defgroup org-gtd-clarify nil
  "Customize the behavior when clarifying an item."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0"))

(defcustom org-gtd-clarify-project-templates nil
  "This is an alist of (\"template title\" . \"template\").

Used by `org-gtd-clarify-projects-insert-template', when clarifying an item
which turns out to be a project."
  :group 'org-gtd-clarify
  :type '(alist :key-type string :value-type string)
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-clarify-show-horizons nil
  "If non-nil, show a side buffer with the horizons during item clarification.
The values can be: nil, top, right, left, bottom.

The file shown can be configured in `org-gtd-horizons-file'."
  :group 'org-gtd-clarify
  :options '('right 'top 'left 'bottom 'nil)
  :package-version '(org-gtd . "3.0")
  :type 'symbol)


;;;; Variables

(defvar-local org-gtd-clarify--clarify-id nil
  "Reference to the org id of the heading currently in the WIP buffer.")

(defvar-local org-gtd-clarify--inbox-p nil
  "Used to separate a one-off clarify from the inbox clarification.")

(defvar-local org-gtd-clarify--source-heading-marker nil
  "Store marker to item that is being clarified.")

(defvar-local org-gtd-clarify--window-config nil
  "Store window configuration prior to clarifying task.")

;;;;; Keymaps

(defvar org-gtd-clarify-map (make-sparse-keymap))

;; code to make windows atomic, from emacs manual
;; (let ((window (split-window-right)))
;;   (window-make-atom (window-parent window))
;;   (display-buffer-in-atom-window
;;    (get-buffer-create "*Messages*")
;;    `((window . ,(window-parent window)) (window-height . 5))))

;; code to make windows non-atomic
;; (walk-window-subtree (lambda (window) (set-window-parameter window 'window-atom nil)) (window-parent (get-buffer-window (current-buffer))) t)

;; dedicated side window
;; (display-buffer-in-side-window (get-buffer "horizons.org") '((side . right) (dedicated . t)))

;;;; Macros

;;;###autoload
(define-minor-mode org-gtd-clarify-mode
  "Minor mode for org-gtd."
  :lighter " GPM"
  :keymap org-gtd-clarify-map
  :group 'org-gtd-clarify
  (if org-gtd-clarify-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-gtd-clarify-map>Clarify item.  Use `\\[org-gtd-organize]' to file it appropriately when finished."))
    (setq-local header-line-format nil)))

;;;; Commands

(defun org-gtd-clarify-agenda-item ()
  "Process item at point on agenda view."
  (declare (modes org-agenda-mode)) ;; for 27.2 compatibility
  (interactive nil)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let ((heading-marker (or (org-get-at-bol 'org-marker)
                            (org-agenda-error))))
    (org-gtd-clarify-item heading-marker
                          (current-window-configuration))))

;;;###autoload
(defun org-gtd-clarify-item (&optional marker window-config)
  "Process item at point through org-gtd.

MARKER must be a marker pointing to an org heading.
WINDOW-CONFIG is the window config to set after clarification finishes."
  (declare (modes org-mode)) ;; for 27.2 compatibility
  (interactive)
  (let* ((window-config (or window-config (current-window-configuration)))
         (source-heading-marker (or marker (point-marker)))
         (clarify-id (org-gtd-id-get-create source-heading-marker))
         (processing-buffer (org-gtd-wip--get-buffer clarify-id)))
    (org-gtd-wip--maybe-initialize-buffer-contents source-heading-marker processing-buffer)
    (with-current-buffer processing-buffer
      (org-gtd-clarify-mode 1)
      (setq-local org-gtd-clarify--window-config window-config
                  org-gtd-clarify--source-heading-marker source-heading-marker
                  org-gtd-clarify--clarify-id clarify-id))
    (org-gtd-clarify-setup-windows processing-buffer)))

(defun org-gtd-clarify-switch-to-buffer ()
  "Prompt the user to choose one of the existing WIP buffers."
  (declare (modes org-gtd-clarify-mode)) ;; for 27.2 compatibility
  (interactive)
  (let ((buf-names (mapcar #'buffer-name (org-gtd-wip--get-buffers))))
    (if buf-names
        (let ((chosen-buf-name (completing-read "Choose a buffer: " buf-names nil t)))
          (org-gtd-clarify-setup-windows chosen-buf-name))
      (message "There are no Org-GTD WIP buffers."))))

(defun org-gtd-clarify-toggle-horizons-window ()
  "Toggle the window with the horizons buffer."
  (interactive)
  (let* ((buffer (org-gtd--horizons-file))
         (window (get-buffer-window buffer)))
    (if window
        (quit-window nil window)
      (org-gtd-clarify--display-horizons-window))))

;;;; Functions

;;;;; Public

(defun org-gtd-clarify-inbox-item (marker window-config)
  "Process item at point through org-gtd.
This function is called through the inbox clarification process.

MARKER must be a marker pointing to an org heading.
WINDOW-CONFIG is the window config to set after clarification finishes."
  (org-gtd-clarify-item marker window-config)
  (setq-local org-gtd-clarify--inbox-p t))

(defun org-gtd-clarify-project-insert-template ()
  "Insert user-provided template under item at point."
  (interactive)
  (let* ((choice (completing-read
                  "Choose a project template to insert: "
                  org-gtd-clarify-project-templates nil t))
         (chosen-template (alist-get
                           choice
                           org-gtd-clarify-project-templates nil nil 'equal)))
    (save-excursion
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1))
      (when (equal (point-min) (point))
        (goto-char 2))
      (org-paste-subtree 2 chosen-template))))

(defun org-gtd-clarify-setup-windows (buffer-or-name)
  "Setup clarifying windows around BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (set-buffer buffer)
    (display-buffer buffer)
    (delete-other-windows (get-buffer-window buffer))
    (if org-gtd-clarify-show-horizons
        (org-gtd-clarify--display-horizons-window))))

;;;;; Private

(defun org-gtd-clarify--display-horizons-window ()
  "Display horizons window."
  (let ((horizons-side (or org-gtd-clarify-show-horizons 'right)))
    (display-buffer (org-gtd--horizons-file)
                    `(display-buffer-in-side-window . ((side . ,horizons-side))))))

;;;; Footer

(provide 'org-gtd-clarify)

;;; org-gtd-clarify.el ends here
