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
(require 'org-gtd-id)
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

;;;; Constants

(defconst org-gtd-clarify--prefix "Org-GTD WIP")

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

;;;###autoload
(defvar org-gtd-clarify-map (make-sparse-keymap)
  "Keymap for command `org-gtd-clarify-mode', a minor mode.")

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
    (org-gtd-clarify-item-at-marker heading-marker)))

;;;###autoload
(defun org-gtd-clarify-item ()
  "Process item at point through org-gtd."
  (declare (modes org-mode)) ;; for 27.2 compatibility
  (interactive)
  (let ((processing-buffer (org-gtd-clarify--get-buffer))
        (window-config (current-window-configuration))
        (source-heading-marker (point-marker)))
    (org-gtd-clarify--maybe-initialize-buffer-contents processing-buffer)
    (with-current-buffer processing-buffer
      (setq-local org-gtd-clarify--window-config window-config
                  org-gtd-clarify--source-heading-marker source-heading-marker
                  org-gtd-clarify--clarify-id (org-gtd-id-get-create)))
    (org-gtd-clarify-setup-windows processing-buffer)))

(defun org-gtd-clarify-switch-to-buffer ()
  "Prompt the user to choose one of the existing WIP buffers."
  (declare (modes org-gtd-clarify-mode)) ;; for 27.2 compatibility
  (interactive)
  (let ((buf-names (mapcar #'buffer-name (org-gtd-clarify--get-buffers))))
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

(defun org-gtd-clarify-inbox-item ()
  "Process item at point through org-gtd.
This function is called through the inbox clarification process."
  (org-gtd-clarify-item)
  (setq-local org-gtd-clarify--inbox-p t))

(defun org-gtd-clarify-item-at-marker (marker)
  "MARKER must be a marker pointing to an org heading."
  (let ((heading-buffer (marker-buffer marker))
        (heading-position (marker-position marker)))
    (with-current-buffer heading-buffer
      (goto-char heading-position)
      (org-gtd-clarify-item))))

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

(defun org-gtd-clarify--buffer-name (id)
  "Retrieve the name of the WIP buffer for this particular ID."
  (format "*%s: %s*" org-gtd-clarify--prefix id))

(defun org-gtd-clarify--display-horizons-window ()
  "Display horizons window."
  (let ((horizons-side (or org-gtd-clarify-show-horizons 'right)))
    (display-buffer (org-gtd--horizons-file)
                    `(display-buffer-in-side-window . ((side . ,horizons-side))))))

(defun org-gtd-clarify--get-buffer ()
  "Get or create a WIP buffer for heading at point."
  (let* ((org-id (org-gtd-id-get-create))
         (buffer (get-buffer-create (org-gtd-clarify--buffer-name org-id))))
    (with-current-buffer buffer
      (unless (eq major-mode 'org-mode) (org-mode))
      (org-gtd-core-prepare-buffer)
      (org-gtd-clarify-mode 1)
      buffer)))

(defun org-gtd-clarify--get-buffers ()
  "Retrieve a list of Org GTD WIP buffers."
  (seq-filter (lambda (buf)
                (string-match-p org-gtd-clarify--prefix (buffer-name buf)))
              (buffer-list)))

(defun org-gtd-clarify--maybe-initialize-buffer-contents (buffer)
  "If BUFFER is empty, then copy org heading at point and paste inside buffer."
  (with-temp-message ""
    (when (= (buffer-size buffer) 0)
      (let ((last-command nil))
        (org-copy-subtree))
      (with-current-buffer buffer
        (org-paste-subtree)
        (org-entry-delete (point) org-gtd-timestamp)
        (org-entry-delete (point) org-gtd-delegate-property)
        (org-entry-delete (point) "STYLE")))))

;;;; Footer

(provide 'org-gtd-clarify)

;;; org-gtd-clarify.el ends here
