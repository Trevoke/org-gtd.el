;;; org-gtd-init.el --- First-time setup for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2026 Aldric Giacomoni

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
;; Idempotent first-time setup concierge.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-files)
(require 'org-gtd-capture)
(require 'org-gtd-checklist)
(require 'org-gtd-review)

;;;; Commands

;;;###autoload
(defun org-gtd-init-system ()
  "Set up org-gtd for first use.  Safe to run again at any time.
Ensures the GTD files exist (seeding starter checklist templates) and offers
to schedule a recurring Weekly Review.  Every step reports and skips
when already satisfied — lazy initialization elsewhere is untouched."
  (interactive)
  (condition-case err
      (progn
        (unless (file-directory-p org-gtd-directory)
          (make-directory org-gtd-directory t))
        (org-gtd--default-file)
        (org-gtd-inbox-path)
        (org-gtd-checklist-template--file-buffer))
    (file-error
     (user-error "Could not create GTD files (%s) — check that %s is writable, or customize org-gtd-directory"
                 (error-message-string err)
                 (abbreviate-file-name org-gtd-directory))))
  (if (org-gtd-review--reminder-exists-p)
      (message "✓ GTD files ready in %s — a review reminder is already scheduled"
               (abbreviate-file-name org-gtd-directory))
    (if (y-or-n-p "Schedule a recurring Weekly Review reminder? ")
        (call-interactively #'org-gtd-review-schedule)
      (message "✓ GTD files ready in %s"
               (abbreviate-file-name org-gtd-directory)))))

;;;; Footer

(provide 'org-gtd-init)

;;; org-gtd-init.el ends here
