;;; org-gtd-wip.el --- WIP buffer logic -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Provides helper functions for managing temporary clarification buffers.
;; The actual major mode is in org-gtd-clarify.el.
;;
;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-gtd-types)
(require 'org-gtd-id)

;;;; Customizations

;;;; Constants

(defconst org-gtd-wip--prefix "Org-GTD Clarify")

(defconst org-gtd-wip--max-filename-id-length 80
  "Maximum length for ID portion in temp filenames.
Keeps total filename well under the 255-byte NAME_MAX limit.")

;;;; Variables

(defvar org-gtd-wip--temp-files (make-hash-table :test 'equal)
  "Hash table mapping buffer IDs to temporary file paths.")

;;;; Macros

;;;; Commands

;;;; Functions

;;;;; Public

;;;;; Private

(defun org-gtd-wip--truncate-for-filename (str)
  "Truncate STR to safe length for use in filenames.
Ensures the ID portion stays under `org-gtd-wip--max-filename-id-length'."
  (if (<= (length str) org-gtd-wip--max-filename-id-length)
      str
    (substring str 0 org-gtd-wip--max-filename-id-length)))

(defun org-gtd-wip--buffer-name (id)
  "Retrieve the name of the WIP buffer for this particular ID."
  (format "*%s: %s*" org-gtd-wip--prefix id))

(defun org-gtd-wip--get-buffer (org-id)
  "Get or create a WIP buffer with temp file for ORG-ID.
Returns a buffer in `org-mode'.  The caller is responsible for
activating any additional modes (e.g., `org-gtd-clarify-mode')."
  ;; Check if we already have a temp file for this ID
  (let ((existing-file (gethash org-id org-gtd-wip--temp-files)))
    (if (and existing-file (file-exists-p existing-file))
        ;; Reuse existing temp file
        (let ((buffer (find-file-noselect existing-file)))
          (with-current-buffer buffer
            ;; Ensure buffer has the correct name
            (rename-buffer (org-gtd-wip--buffer-name org-id) t))
          buffer)
      ;; Create new temp file
      (let* ((temp-dir (expand-file-name "org-gtd" temporary-file-directory))
             (_ (unless (file-exists-p temp-dir)
                  (make-directory temp-dir t)))
             ;; Truncate ID for filename to stay under NAME_MAX (255 bytes)
             (safe-id (org-gtd-wip--truncate-for-filename org-id))
             (temp-file (make-temp-file
                         (expand-file-name
                          (format "wip-%s-" safe-id)
                          temp-dir)
                         nil ".org"))
             (buffer (find-file-noselect temp-file)))
        ;; Track temp file for cleanup
        (puthash org-id temp-file org-gtd-wip--temp-files)
        (with-current-buffer buffer
          ;; Set a more user-friendly buffer name
          (rename-buffer (org-gtd-wip--buffer-name org-id) t))
        buffer))))

(defun org-gtd-wip--get-buffers ()
  "Retrieve a list of Org GTD WIP buffers."
  (seq-filter (lambda (buf)
                (string-search org-gtd-wip--prefix
                               (buffer-name buf)))
              (buffer-list)))

(defun org-gtd-wip--cleanup-temp-file (org-id)
  "Clean up temp file for ORG-ID."
  (let ((temp-file (gethash org-id org-gtd-wip--temp-files)))
    (when temp-file
      ;; Kill buffer if open
      (let ((buffer (get-file-buffer temp-file)))
        (when buffer
          (with-current-buffer buffer
            (set-buffer-modified-p nil)) ; Don't prompt for save
          (kill-buffer buffer)))
      ;; Delete temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      ;; Remove from tracking
      (remhash org-id org-gtd-wip--temp-files))))

(defun org-gtd-wip--cleanup-all-temp-files ()
  "Clean up all temp files (for emergency cleanup or exit)."
  (maphash
   (lambda (id _file)
     (org-gtd-wip--cleanup-temp-file id))
   org-gtd-wip--temp-files))

;;;; Footer

(provide 'org-gtd-wip)

;;; org-gtd-wip.el ends here
