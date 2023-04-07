;;; org-gtd-agenda.el --- Manage the agenda view -*- lexical-binding: t; coding: utf-8 -*-
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
;; Agenda management for org-gtd.
;;
;;; Code:

(require 'winner)
(require 'org-agenda)
(require 'org-gtd-core)
(require 'org-gtd-backward-compatibility)

(defgroup org-gtd-agenda nil
  "Options for org-gtd agenda views."
  :package-version '(org-gtd . "3.0.0")
  :group 'org-gtd)

(defcustom org-gtd-agenda-custom-commands
  `(("g" "Scheduled today and all NEXT items"
     (
      (agenda "" ((org-agenda-span 1)
                  (org-agenda-start-day nil)))
      (todo org-gtd-next ((org-agenda-overriding-header "All actions ready to be executed.")
                          (org-agenda-prefix-format '((todo . " %i %-12:(org-gtd-agenda--prefix-format)")))))
      (todo org-gtd-wait ((org-agenda-todo-ignore-with-date t)
                          (org-agenda-overriding-header "Delegated/Blocked items")
                          (org-agenda-prefix-format '((todo . " %i %-12 (org-gtd-agenda--prefix-format)"))))))))
  "Agenda custom commands to be used for org-gtd.

The provided default is to show the agenda for today and all TODOs marked as
`org-gtd-next' or `org-gtd-wait'.  See documentation for
`org-agenda-custom-commands' to customize this further.

NOTE! The function `org-gtd-engage' assumes the 'g' shortcut exists.
It's recommended you add to this list without modifying this first entry.  You
can leverage this customization feature with command `org-gtd-mode'
or by wrapping your own custom functions with `with-org-gtd-context'."
  :group 'org-gtd-agenda
  :type 'sexp
  :package-version '(org-gtd . "2.0.0"))

;;;###autoload
(defun org-gtd-engage ()
  "Display `org-agenda' customized by org-gtd."
  (interactive)
  (org-gtd-core-prepare-agenda-buffers)
  (with-org-gtd-context
      (org-agenda nil "g")))

;;;###autoload
(defun org-gtd-show-all-next ()
  "Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files."
  (interactive)
  (with-org-gtd-context
      (org-gtd-core-prepare-agenda-buffers)
      (org-todo-list org-gtd-next)))

;;;###autoload
(defun org-gtd-agenda--apply (func)
  "Run FUNC on a single task shown in the agenda view."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-gtd-agenda-projectify nil t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
                      (org-agenda-error)))
          (buffer (marker-buffer marker))
          (pos (marker-position marker)))
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
         (widen)
         (goto-char pos)
         (org-up-element)
         (org-narrow-to-element)
         (org-show-subtree)
         (display-buffer-same-window buffer '())
         (org-gtd--project)
         (widen)
         (winner-undo))
       (org-agenda-show-tags)))))

;;; TODO: replace this with a call to clarify item
;;;###autoload
(defun org-gtd-agenda-projectify ()
  "Transform the current agenda item into a gtd project.

This function is intended to be used on incubated items that come up."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-gtd-agenda-projectify nil t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
                      (org-agenda-error)))
          (buffer (marker-buffer marker))
          (pos (marker-position marker)))
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
         (widen)
         (goto-char pos)
         (org-up-element)
         (org-narrow-to-element)
         (org-show-subtree)
         (display-buffer-same-window buffer '())
         (org-gtd--project)
         (widen)
         (winner-undo))
       (org-agenda-show-tags)))))

;;;###autoload
(defun org-gtd-agenda-delegate ()
  "Delegate current agenda task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-gtd-agenda-delegate nil t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
                      (org-agenda-error)))
          (buffer (marker-buffer marker))
          (pos (marker-position marker)))
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
         (widen)
         (goto-char pos)
         (org-gtd-delegate))
       (org-agenda-show-tags)))))

;;;###autoload
(defun org-gtd-agenda-cancel-project ()
  "Cancel the project that has the highlighted task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'org-gtd-agenda-cancel-project nil t nil
   (let* ((marker (or (org-get-at-bol 'org-marker)
                      (org-agenda-error)))
          (buffer (marker-buffer marker))
          (pos (marker-position marker)))
     (set-marker-insertion-type marker t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
         (widen)
         (goto-char pos)
         (org-up-heading-safe)
         (org-gtd-cancel-project))
       (org-agenda-show-tags)))))

(defun org-gtd-agenda--prefix-format ()
  "format prefix for items in buffer"
  (let* ((elt (org-element-at-point))
         (level (org-element-property :level elt))
         (category (org-entry-get (point) "CATEGORY" t))
         (parent-title (org-element-property
                        :raw-value
                        (org-element-property :parent elt)))
         (tally-cookie-regexp "\[[[:digit:]]+/[[:digit:]]+\][[:space:]]*"))

    (cond
     ((eq level 3)
      (concat
       (substring (string-pad
                   (replace-regexp-in-string tally-cookie-regexp "" parent-title)
                   11)
                  0 10)
       "…"))
     (category (concat (substring (string-pad category 11) 0 10) "…"))
     (t "Simple task"))))

(provide 'org-gtd-agenda)
;;; org-gtd-agenda.el ends here
