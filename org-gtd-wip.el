;;; org-gtd-wip.el --- WIP buffer logic -*- lexical-binding: t; coding: utf-8 -*-
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
;; The WIP buffer needs a custom major mode.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-id)

;;;; Customizations

;;;; Constants

(defconst org-gtd-wip--prefix "Org-GTD WIP")

;;;; Variables

;;;; Modes

;;;###autoload
(define-derived-mode org-gtd-wip-mode org-mode "GTD-WIP"
  "Inherits from org-mode to allow for an org buffer that isn't connected to
a file without breaking users' setups.

\\[org-gtd-wip-mode-map]"
  (setq-local org-gtd--loading-p t)
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<org-gtd-wip-mode-map>Clarify item.  Use `\\[org-gtd-organize]' to file it appropriately when finished.")))

;;;; Macros

;;;; Commands

;;;; Functions

;;;;; Public

;;;;; Private

(defun org-gtd-wip--buffer-name (id)
  "Retrieve the name of the WIP buffer for this particular ID."
  (format "*%s: %s*" org-gtd-wip--prefix id))

(defun org-gtd-wip--get-buffer (org-id)
  "Get or create a WIP buffer with name based on ORG-ID."
  (let* ((buffer (get-buffer-create (org-gtd-wip--buffer-name org-id))))
    (with-current-buffer buffer
      (unless (eq major-mode #'org-gtd-wip-mode)
        (with-org-gtd-context (org-gtd-wip-mode)))
      buffer)))

(defun org-gtd-wip--get-buffers ()
  "Retrieve a list of Org GTD WIP buffers."
  (seq-filter (lambda (buf)
                (string-match-p org-gtd-wip--prefix (buffer-name buf)))
              (buffer-list)))

(defun org-gtd-wip--maybe-initialize-buffer-contents (marker buffer)
  "If BUFFER is empty, then copy org heading at marker and paste inside buffer."
  (with-temp-message ""
    (when (= (buffer-size buffer) 0)
      (let ((last-command nil))
        (org-with-point-at marker
          (org-copy-subtree)))
      (with-current-buffer buffer
        (org-paste-subtree)
        (org-entry-delete (point) org-gtd-timestamp)
        (org-entry-delete (point) org-gtd-delegate-property)
        (org-entry-delete (point) "STYLE")))))

;;;; Footer

(provide 'org-gtd-wip)

;;; org-gtd-wip.el ends here
