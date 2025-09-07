;;; org-gtd-id-overlay.el --- ID overlay functionality for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

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
;; This module provides overlay functionality for org-gtd ID properties.
;; It replaces cryptic ID values with human-readable, truncated heading text
;; to improve the user experience during GTD clarification workflows.
;;
;; The overlays are clickable and provide navigation to the referenced headings.
;; The feature integrates seamlessly with org-gtd-wip-mode and clarification.

;;; Code:

;;;; Requirements

(require 'org-gtd-core)
(require 'org-id)
(require 'org)

;;;; Customization

(defgroup org-gtd-id-overlay nil
  "ID overlay display for org-gtd."
  :group 'org-gtd
  :package-version '(org-gtd . "3.1"))

(defcustom org-gtd-id-overlay-truncate-length 20
  "Maximum length for displayed heading text in ID overlays."
  :group 'org-gtd-id-overlay
  :type 'integer
  :package-version '(org-gtd . "3.1"))

(defcustom org-gtd-id-overlay-ellipsis "..."
  "String to append to truncated heading text."
  :group 'org-gtd-id-overlay
  :type 'string
  :package-version '(org-gtd . "3.1"))

(defface org-gtd-id-overlay-face
  '((t (:foreground "blue" :underline t)))
  "Face for ID overlay text."
  :group 'org-gtd-id-overlay
  :package-version '(org-gtd . "3.1"))

;;;; Variables

(defvar-local org-gtd-id-overlay--overlays nil
  "List of overlays created by org-gtd-id-overlay mode in this buffer.")

(defvar org-gtd-id-overlay--cache (make-hash-table :test 'equal)
  "Hash table caching ID -> heading text mappings.")

(defvar org-gtd-id-overlay--cache-max-size 1000
  "Maximum number of entries in ID overlay cache.")

;;;; Keymap

(defvar org-gtd-id-overlay-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for org-gtd-id-overlay minor mode.")

;;;; Commands

;;;###autoload
(define-minor-mode org-gtd-id-overlay-mode
  "Minor mode to display ID properties as heading text overlays."
  :lighter " ID-Overlay"
  :keymap org-gtd-id-overlay-mode-map
  :group 'org-gtd-id-overlay
  (if org-gtd-id-overlay-mode
      (org-gtd-id-overlay--enable)
    (org-gtd-id-overlay--disable)))

;;;###autoload
(defun org-gtd-id-overlay-refresh-buffer ()
  "Refresh all ID overlays in the current buffer."
  (interactive)
  (when org-gtd-id-overlay-mode
    (org-gtd-id-overlay--remove-overlays)
    (org-gtd-id-overlay--create-overlays)))

;;;###autoload
(defun org-gtd-id-overlay-clear-buffer ()
  "Remove all ID overlays from the current buffer."
  (interactive)
  (org-gtd-id-overlay--remove-overlays))

(defun org-gtd-id-overlay-goto-heading-at-point ()
  "Navigate to the heading referenced by the ID overlay at point."
  (interactive)
  (let* ((overlay (seq-find
                   (lambda (ov)
                     (overlay-get ov 'org-gtd-id-overlay))
                   (overlays-at (point))))
         (id (when overlay (overlay-get overlay 'org-gtd-id))))
    (if id
        (org-gtd-id-overlay--navigate-to-id id)
      (message "No ID overlay at point"))))

;;;; Functions

;;;;; Private - Mode Management

(defun org-gtd-id-overlay--enable ()
  "Enable ID overlays in current buffer."
  (org-gtd-id-overlay--remove-overlays)
  (org-gtd-id-overlay--create-overlays)
  (add-hook 'after-change-functions #'org-gtd-id-overlay--after-change nil t)
  (add-hook 'org-property-changed-functions #'org-gtd-id-overlay--property-changed nil t))

(defun org-gtd-id-overlay--disable ()
  "Disable ID overlays in current buffer."
  (org-gtd-id-overlay--remove-overlays)
  (remove-hook 'after-change-functions #'org-gtd-id-overlay--after-change t)
  (remove-hook 'org-property-changed-functions #'org-gtd-id-overlay--property-changed t))

;;;;; Private - Text Processing

(defun org-gtd-id-overlay--extract-heading-text (heading-text)
  "Extract clean heading text from HEADING-TEXT.
Removes TODO keywords and statistics cookies."
  (let ((text (string-trim heading-text)))
    ;; Remove common TODO keywords (with or without trailing space)
    (setq text (replace-regexp-in-string 
                "^\\(TODO\\|DONE\\|NEXT\\|WAIT\\|PROJ\\|CNCL\\|CANCELED\\|CANCELLED\\)\\(\\s-+\\|$\\)" 
                "" text))
    ;; Remove statistics cookies [n/m] and [n%]
    (setq text (replace-regexp-in-string "\\[\\([0-9]+/[0-9]+\\|[0-9]+%\\)\\]" "" text))
    ;; Clean up extra whitespace
    (string-trim (replace-regexp-in-string "\\s-+" " " text))))

(defun org-gtd-id-overlay--truncate-text (text limit)
  "Truncate TEXT to LIMIT characters, adding ellipsis if needed."
  (if (and text (> (length text) limit))
      (concat (substring text 0 limit) org-gtd-id-overlay-ellipsis)
    (or text "")))

;;;;; Private - Overlay Management

(defun org-gtd-id-overlay--create-overlays ()
  "Create overlays for all ID properties in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" nil t)
      (let* ((id (match-string 1))
             (id-start (match-beginning 1))
             (id-end (match-end 1)))
        (org-gtd-id-overlay--create-overlay id id-start id-end)))))

(defun org-gtd-id-overlay--create-overlay (id start end)
  "Create an overlay for ID between START and END positions."
  (when (and id (not (string-empty-p (string-trim id))))
    (let* ((heading-text (org-gtd-id-overlay--get-heading-text id))
           (display-text (org-gtd-id-overlay--truncate-text 
                         heading-text 
                         org-gtd-id-overlay-truncate-length))
           (overlay (make-overlay start end)))
      ;; Set overlay properties
      (overlay-put overlay 'org-gtd-id-overlay t)
      (overlay-put overlay 'org-gtd-id id)
      (overlay-put overlay 'display display-text)
      (overlay-put overlay 'face 'org-gtd-id-overlay-face)
      (overlay-put overlay 'help-echo (format "ID: %s\nClick to navigate" id))
      ;; Add click handler
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap [mouse-1] 'org-gtd-id-overlay-goto-heading-at-point)
        (define-key keymap (kbd "RET") 'org-gtd-id-overlay-goto-heading-at-point)
        (overlay-put overlay 'keymap keymap))
      ;; Track overlay for cleanup
      (push overlay org-gtd-id-overlay--overlays)
      overlay)))

(defun org-gtd-id-overlay--remove-overlays ()
  "Remove all ID overlays from the current buffer."
  (dolist (overlay org-gtd-id-overlay--overlays)
    (when (overlayp overlay)
      (delete-overlay overlay)))
  (setq org-gtd-id-overlay--overlays nil))

;;;;; Private - ID Resolution

(defun org-gtd-id-overlay--get-heading-text (id)
  "Get heading text for ID with fallback strategies."
  (or (org-gtd-id-overlay--cache-get id)
      (let ((heading-text (or (org-gtd-id-overlay--resolve-from-current-buffer id)
                              (org-gtd-id-overlay--resolve-from-org-id id)
                              (format "Unknown ID: %s" 
                                    (org-gtd-id-overlay--truncate-text id 8)))))
        (org-gtd-id-overlay--cache-put id heading-text)
        heading-text)))

(defun org-gtd-id-overlay--resolve-from-org-id (id)
  "Resolve heading text for ID using org-id system."
  (let ((marker (org-id-find id t)))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)
          (when (org-at-heading-p)
            (org-gtd-id-overlay--extract-heading-text 
             (nth 4 (org-heading-components)))))))))

(defun org-gtd-id-overlay--resolve-from-current-buffer (id)
  "Resolve heading text for ID by searching current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^[ \t]*:ID:[ \t]+%s" (regexp-quote id)) nil t)
      ;; Go to the beginning of the property block, then find the heading
      (beginning-of-line)
      (while (and (not (org-at-heading-p)) (not (bobp)))
        (forward-line -1))
      (when (org-at-heading-p)
        (org-gtd-id-overlay--extract-heading-text 
         (nth 4 (org-heading-components)))))))

;;;;; Private - Navigation

(defun org-gtd-id-overlay--navigate-to-id (id)
  "Navigate to the heading with the given ID."
  (let ((marker (org-id-find id t)))
    (if marker
        (progn
          (switch-to-buffer (marker-buffer marker))
          (goto-char marker)
          (org-reveal)
          (org-fold-show-entry))
      ;; Fallback: search in current buffer
      (let ((pos (org-gtd-id-overlay--find-id-in-current-buffer id)))
        (if pos
            (progn
              (goto-char pos)
              (org-reveal)
              (org-fold-show-entry))
          (message "Cannot find heading with ID: %s" id))))))

(defun org-gtd-id-overlay--find-id-in-current-buffer (id)
  "Find position of heading with ID in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^[ \t]*:ID:[ \t]+%s" (regexp-quote id)) nil t)
      (org-up-heading-safe)
      (point))))

;;;;; Private - Cache Management

(defun org-gtd-id-overlay--cache-get (id)
  "Get heading text for ID from cache."
  (gethash id org-gtd-id-overlay--cache))

(defun org-gtd-id-overlay--cache-put (id heading-text)
  "Put ID and HEADING-TEXT into cache with LRU eviction."
  (when (> (hash-table-count org-gtd-id-overlay--cache) 
           org-gtd-id-overlay--cache-max-size)
    (org-gtd-id-overlay--cache-evict-lru))
  (puthash id (list heading-text (current-time)) org-gtd-id-overlay--cache))

(defun org-gtd-id-overlay--cache-evict-lru ()
  "Evict least recently used entries from cache."
  ;; Simple implementation: clear half the cache
  ;; More sophisticated LRU could be implemented later if needed
  (let ((entries (hash-table-count org-gtd-id-overlay--cache)))
    (when (> entries (/ org-gtd-id-overlay--cache-max-size 2))
      (clrhash org-gtd-id-overlay--cache))))

(defun org-gtd-id-overlay--cache-remove (id)
  "Remove ID from cache."
  (remhash id org-gtd-id-overlay--cache))

;;;;; Private - Event Handlers

(defun org-gtd-id-overlay--after-change (beg end _len)
  "Update overlays after buffer changes between BEG and END.
_LEN is the length of the deleted text (unused)."
  (when org-gtd-id-overlay-mode
    ;; Simple approach: refresh overlays in the changed region
    (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
          (line-end (save-excursion (goto-char end) (line-end-position))))
      (org-gtd-id-overlay--refresh-region line-beg line-end))))

(defun org-gtd-id-overlay--refresh-region (start end)
  "Refresh overlays in region between START and END."
  ;; Remove overlays in region
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'org-gtd-id-overlay)
      (setq org-gtd-id-overlay--overlays 
            (delq overlay org-gtd-id-overlay--overlays))
      (delete-overlay overlay)))
  ;; Recreate overlays in region
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward "^[ \t]*:ID:[ \t]+\\(\\S-+\\)" end t))
      (let* ((id (match-string 1))
             (id-start (match-beginning 1))
             (id-end (match-end 1)))
        (org-gtd-id-overlay--create-overlay id id-start id-end)))))

(defun org-gtd-id-overlay--property-changed (property _value &optional begin end)
  "Handle property changes, refreshing overlays if ID property changed.
PROPERTY is the property name, _VALUE is the new value (unused).
BEGIN and END mark the affected region."
  (when (and org-gtd-id-overlay-mode 
             (string-equal property "ID"))
    ;; Refresh the current heading area
    (save-excursion
      (let ((heading-start (or begin (progn (org-back-to-heading t) (point))))
            (heading-end (or end (progn (org-end-of-subtree t) (point)))))
        (org-gtd-id-overlay--refresh-region heading-start heading-end)))))

;;;; Integration with org-gtd-wip-mode

;;;###autoload
(defun org-gtd-id-overlay-maybe-enable ()
  "Enable org-gtd-id-overlay-mode in appropriate contexts."
  (when (and (derived-mode-p 'org-mode)
             (eq major-mode 'org-gtd-wip-mode))
    (org-gtd-id-overlay-mode 1)))

;; Add hook for automatic enabling in WIP mode
;;;###autoload
(add-hook 'org-gtd-wip-mode-hook #'org-gtd-id-overlay-maybe-enable)

;;;; Footer

(provide 'org-gtd-id-overlay)

;;; org-gtd-id-overlay.el ends here