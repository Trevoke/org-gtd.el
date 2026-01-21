;;; org-gtd-mode.el --- global minor mode for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Archiving logic for org-gtd
;;
;;; Code:

;;;; Requirements

(require 'org-agenda)
(require 'org-edna)

(require 'org-gtd-core)
(require 'org-gtd-capture)

(declare-function org-gtd-project--maybe-update-cookies "org-gtd-projects")
(declare-function org-gtd-agenda-property-add-properties "org-gtd-agenda-property")
(declare-function org-gtd-wip--cleanup-all-temp-files "org-gtd-wip")
(declare-function org-gtd-single-action--maybe-convert-to-delegated "org-gtd-single-action")

;;;; Constants

(defconst org-gtd--agenda-functions (apropos-internal "org-agenda" #'commandp)
  "List of commands available to the user through `org-agenda'.
Org-gtd wraps these functions with its own context when command `org-gtd-mode'
is enabled.")

;;;; Variables

(defvar org-gtd-edna nil "Private.")

(defvar org-gtd-mode--refresh-timer nil
  "Timer for periodic inbox count refresh.")

;;;; Customization

(defcustom org-gtd-mode-update-interval 60
  "Seconds between automatic inbox count refreshes.
Set to nil to disable periodic refresh (count still updates on buffer changes).
Only applies when `org-gtd-mode' is enabled."
  :group 'org-gtd
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Disable periodic refresh" nil)))

(defcustom org-gtd-mode-lighter-display 'always
  "When to display the org-gtd-mode lighter in the mode-line.

- `always': Always show (e.g., GTD[0], GTD[5]) - default behavior
- `never': Never show the lighter
- `when-non-zero': Only show when inbox has items (hides GTD[0])"
  :group 'org-gtd
  :type '(choice (const :tag "Always show" always)
                 (const :tag "Never show" never)
                 (const :tag "Only when inbox non-empty" when-non-zero)))

;;;; External variables

(defvar org-gtd-additional-inbox-files)

;;;; Modes

;;;###autoload
(define-minor-mode org-gtd-mode
  "Global minor mode for org-gtd integration.

When enabled, this mode:
- Displays inbox item count in the mode-line (e.g., GTD[5])
- Enables org-edna for task dependencies
- Periodically refreshes the inbox count for external file changes

The mode-line lighter shows the total count of items across all inbox files,
including the main inbox and any files in `org-gtd-additional-inbox-files'."
  :lighter (:eval (org-gtd-mode-lighter))
  :global t
  :group 'org-gtd
  (if org-gtd-mode
      (org-gtd--enable-org-gtd-mode)
    (org-gtd--disable-org-gtd-mode)))

;;;; Functions

;;;;; Private

(defun org-gtd--disable-org-gtd-mode ()
  "Private function.

`org-gtd-mode' uses this to restore the overridden settings to their
previous values."
  (mapc
   (lambda (x) (advice-remove x #'org-gtd--wrap))
   org-gtd--agenda-functions)
  (org-edna-mode org-gtd-edna)
  ;; Remove CLOSED timestamp hook
  (remove-hook 'org-after-todo-state-change-hook #'org-gtd--add-closed-timestamp)
  ;; Remove project cookies hook
  (remove-hook 'org-after-todo-state-change-hook #'org-gtd-project--maybe-update-cookies)
  ;; Remove single action WAIT conversion hook
  (remove-hook 'org-after-todo-state-change-hook #'org-gtd-single-action--maybe-convert-to-delegated)
  ;; Remove agenda property hooks
  (remove-hook 'org-agenda-finalize-hook #'org-gtd-agenda-property-add-properties)
  (remove-hook 'org-finalize-agenda-hook #'org-gtd-agenda-property-add-properties)
  ;; Remove WIP temp file cleanup hook
  (remove-hook 'kill-emacs-hook #'org-gtd-wip--cleanup-all-temp-files)
  ;; Cancel refresh timer
  (when org-gtd-mode--refresh-timer
    (cancel-timer org-gtd-mode--refresh-timer)
    (setq org-gtd-mode--refresh-timer nil)))

(defun org-gtd--enable-org-gtd-mode ()
  "Private function.

`org-gtd-mode' uses this to override a number of settings in Emacs.
Disabling the mode reverts the settings to their previous values.
It should be safe to turn this on if you do not have extensive `org-mode'
configuration."
  (mapc
   (lambda (x) (advice-add x :around #'org-gtd--wrap))
   org-gtd--agenda-functions)
  (setq org-gtd-edna org-edna-mode)
  (org-edna-mode 1)
  ;; Add CLOSED timestamp hook for GTD items
  (add-hook 'org-after-todo-state-change-hook #'org-gtd--add-closed-timestamp)
  ;; Add project cookies hook
  (add-hook 'org-after-todo-state-change-hook #'org-gtd-project--maybe-update-cookies)
  ;; Add single action WAIT conversion hook
  (add-hook 'org-after-todo-state-change-hook #'org-gtd-single-action--maybe-convert-to-delegated)
  ;; Add agenda property hooks (support both old and new hook names)
  (if (boundp 'org-agenda-finalize-hook)
      (add-hook 'org-agenda-finalize-hook #'org-gtd-agenda-property-add-properties)
    (add-hook 'org-finalize-agenda-hook #'org-gtd-agenda-property-add-properties))
  ;; Add WIP temp file cleanup hook
  (add-hook 'kill-emacs-hook #'org-gtd-wip--cleanup-all-temp-files)
  ;; Start refresh timer for external file changes
  (org-gtd-mode--start-refresh-timer))

(defun org-gtd--wrap (fun &rest r)
  "Private function.

Programmatic wrapper to add org-gtd context to any FUN using `defadvice'.
Argument R is there to be passed through.

v4: Now a simple pass-through since users configure agenda files directly."
  (apply fun r))

;;;;; CLOSED Timestamp Hook

(defun org-gtd--add-closed-timestamp ()
  "Add CLOSED timestamp to GTD items when marked done.
Only affects headings with an ORG_GTD property.  Does not overwrite
existing CLOSED timestamps.  Intended for `org-after-todo-state-change-hook'."
  (when (and (org-entry-get nil "ORG_GTD")
             (org-entry-is-done-p)
             (not (org-entry-get nil "CLOSED")))
    (org-add-planning-info 'closed (current-time))))

;;;;; Inbox Count

(defun org-gtd-inbox-count ()
  "Return total count of top-level headings across all inbox files.
Counts items in main inbox plus any files in `org-gtd-additional-inbox-files'."
  (let ((count 0))
    ;; Count main inbox
    (let ((inbox-path (org-gtd-inbox-path)))
      (when (file-exists-p inbox-path)
        (setq count (+ count (org-gtd--count-headings-in-file inbox-path)))))
    ;; Count additional inboxes
    (when (bound-and-true-p org-gtd-additional-inbox-files)
      (dolist (file org-gtd-additional-inbox-files)
        (when (file-exists-p file)
          (setq count (+ count (org-gtd--count-headings-in-file file))))))
    count))

(defun org-gtd--count-headings-in-file (file)
  "Count top-level org headings in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward "^\\* " nil t)
          (setq count (1+ count)))
        count))))

(defun org-gtd-mode-lighter ()
  "Return the mode-line lighter string with inbox count."
  (format " GTD[%d]" (org-gtd-inbox-count)))

(defun org-gtd-mode--start-refresh-timer ()
  "Start the periodic refresh timer if configured."
  (when org-gtd-mode--refresh-timer
    (cancel-timer org-gtd-mode--refresh-timer))
  (when org-gtd-mode-update-interval
    (setq org-gtd-mode--refresh-timer
          (run-with-timer org-gtd-mode-update-interval
                          org-gtd-mode-update-interval
                          #'org-gtd-mode--refresh))))

(defun org-gtd-mode--refresh ()
  "Refresh the mode-line display.
Called periodically to catch external file changes."
  (force-mode-line-update t))

;;;; Footer

(provide 'org-gtd-mode)

;;; org-gtd-mode.el ends here
