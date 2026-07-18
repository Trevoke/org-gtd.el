;;; org-gtd-someday-review.el --- Review someday/maybe items -*- lexical-binding: t; coding: utf-8 -*-
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
;; Iterative review of someday/maybe items.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-gtd-core)
(require 'org-gtd-wip)
(require 'org-gtd-reactivate)
(require 'org-gtd-someday)
(require 'org-gtd-walk)

;;;; External Function Declarations

;; Evil functions (only called inside with-eval-after-load 'evil)
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-emacs-state "evil-states")

;;;; Variables

(defconst org-gtd-someday-review--surface-key "someday-review"
  "Fixed WIP key for the single someday-review surface buffer.")

(defvar-local org-gtd-someday-review--counters nil
  "Buffer-local plist (:reviewed N :clarified N) for the active surface.")

;;;; Keymaps

(defvar org-gtd-someday-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "d") #'org-gtd-someday-review-defer)
    (define-key map (kbd "c") #'org-gtd-someday-review-clarify)
    (define-key map (kbd "q") #'org-gtd-someday-review-quit)
    map)
  "Keymap for `org-gtd-someday-review-mode'.")

;;;; Functions

;;;;; Private

(defun org-gtd-someday-review--find-items (list-filter)
  "Find someday items, optionally filtered by LIST-FILTER.
LIST-FILTER can be:
  - nil: find all someday items
  - a string: find items with matching ORG_GTD_SOMEDAY_LIST
  - symbol `unassigned': find items without ORG_GTD_SOMEDAY_LIST"
  (let ((items '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (when (string= (org-entry-get (point) "ORG_GTD") org-gtd-someday)
               (let ((item-list (org-entry-get (point) org-gtd-prop-someday-list)))
                 (when (org-gtd-someday-review--item-matches-filter-p item-list list-filter)
                   (push (org-id-get-create) items)))))))))
    (nreverse items)))

(defun org-gtd-someday-review--make-find (list-filter)
  "Return a nullary :find closure yielding someday ids for LIST-FILTER."
  (lambda () (org-gtd-someday-review--find-items list-filter)))

(defun org-gtd-someday-review--resolve (id)
  "Return non-nil when ID still resolves to a live heading marker."
  (org-id-find id 'marker))

(defun org-gtd-someday-review--render (id surface)
  "Render the someday item ID into SURFACE (the walk :render contract).
Resolves ID to a marker, refills SURFACE with the subtree, and sets the
review mode, read-only state, header-line, and display."
  (let ((marker (org-id-find id 'marker)))
    (when marker
      (with-current-buffer surface
        ;; `org-gtd-walk--active' is permanent-local (org-gtd-walk.el), so it
        ;; survives the major-mode (re-)activation below even if it fires.
        ;; In practice the mode is already active by the time :render runs
        ;; (see `org-gtd-someday-review--surface'), so the guard below is a
        ;; no-op on the walk-driven path; it stays for direct callers.
        (let ((inhibit-read-only t)
              ;; `org-paste-subtree' unconditionally calls
              ;; `org-id-paste-tracker' on the pasted text (see org.el),
              ;; which re-registers every pasted :ID: property to *this*
              ;; buffer's file in `org-id-locations'.  SURFACE is a
              ;; disposable read-only review copy, not the item's real
              ;; location; letting that registration through would corrupt
              ;; the global id -> file map so a later `org-id-find' (from
              ;; `defer'/`clarify', run with SURFACE current) resolves back
              ;; into this copy instead of the real source heading.
              ;; Suppress tracking for the duration of the copy/paste.
              (org-id-track-globally nil))
          (erase-buffer)
          (org-gtd--without-kill-merge
            (org-with-point-at marker (org-copy-subtree)))
          (org-paste-subtree)
          (goto-char (point-min)))
        (unless (eq major-mode 'org-gtd-someday-review-mode)
          (org-gtd-someday-review-mode))
        (setq buffer-read-only t)
        (let* ((model (plist-get org-gtd-walk--active :model))
               (pos (1+ (plist-get model :cursor)))
               (total (length (plist-get model :entries))))
          (setq header-line-format
                (format "[d] Defer  [c] Clarify  [q] Quit  (%d/%d)" pos total)))
        (pop-to-buffer surface)))))

(defun org-gtd-someday-review--item-matches-filter-p (item-list list-filter)
  "Return t if ITEM-LIST matches LIST-FILTER.
ITEM-LIST is the value of ORG_GTD_SOMEDAY_LIST property (or nil).
LIST-FILTER is nil (match all), a string (match exact), or `unassigned'."
  (cond
   ((null list-filter) t)
   ((eq list-filter 'unassigned) (null item-list))
   ((stringp list-filter) (equal item-list list-filter))
   (t nil)))

(defun org-gtd-someday-review--add-reviewed-entry ()
  "Add a \\='Reviewed\\=' entry to the LOGBOOK drawer at point."
  (save-excursion
    (org-back-to-heading t)
    (let* ((drawer-pos (org-log-beginning t))
           (has-drawer (save-excursion
                         (goto-char drawer-pos)
                         (looking-back ":LOGBOOK:\n" (line-beginning-position 0)))))
      (goto-char drawer-pos)
      (unless has-drawer
        (insert ":LOGBOOK:\n")
        (save-excursion
          (insert ":END:\n")))
      (insert (format "- Reviewed %s\n"
                      (format-time-string "[%F %a %R]"))))))

;;;;; Walk Surface

(defun org-gtd-someday-review--surface ()
  "Return the fresh single WIP surface buffer for a someday-review walk.
Activates `org-gtd-someday-review-mode' before setting the buffer-local
counters: (re-)running a major mode calls `kill-all-local-variables',
which would silently wipe them.  Doing this here means the mode is
already active by the time `org-gtd-walk-start' calls :render, so
:render's own mode-activation guard never fires and the counters
survive the whole walk."
  (let ((buf (org-gtd-wip--get-buffer org-gtd-someday-review--surface-key)))
    (with-current-buffer buf
      (org-gtd-someday-review-mode)
      (setq-local org-gtd-someday-review--counters (list :reviewed 0 :clarified 0)))
    buf))

(defun org-gtd-someday-review--bump (key)
  "Increment counter KEY (:reviewed or :clarified) on the surface buffer."
  (setq org-gtd-someday-review--counters
        (plist-put org-gtd-someday-review--counters key
                   (1+ (plist-get org-gtd-someday-review--counters key)))))

(defun org-gtd-someday-review--on-finish ()
  "End-of-walk: report the summary and clean up the surface buffer.
Runs in the surface buffer after the engine has cleared its session."
  (let ((reviewed (plist-get org-gtd-someday-review--counters :reviewed))
        (clarified (plist-get org-gtd-someday-review--counters :clarified)))
    (org-gtd-wip--cleanup-temp-file org-gtd-someday-review--surface-key)
    (message "Review complete. %d items reviewed, %d clarified."
             reviewed clarified)))

(defun org-gtd-someday-review--spec ()
  "Return the someday-review walk spec template (default :find = all items)."
  (list :name 'someday-review
        :find (org-gtd-someday-review--make-find nil)
        :render #'org-gtd-someday-review--render
        :actions org-gtd-someday-review-mode-map
        :on-finish #'org-gtd-someday-review--on-finish
        :resumable nil
        :resolve #'org-gtd-someday-review--resolve
        :scope (org-agenda-files)))

(org-gtd-walk-register 'someday-review (org-gtd-someday-review--spec))

;;;; Modes

;;;###autoload
(define-derived-mode org-gtd-someday-review-mode org-mode "GTD-Review"
  "Major mode for reviewing someday/maybe items.
Derived from `org-mode' and provides read-only review interface
with keybindings for defer, clarify, and quit actions.

\\{org-gtd-someday-review-mode-map}"
  :group 'org-gtd
  ;; Note: buffer is made read-only in display function, not here
  )

;;;; Evil-mode Integration

;; When evil-mode is loaded, start someday-review-mode in emacs state.
;; This mode is for read-only review with simple keybindings (d/c/q),
;; so emacs state provides better UX than normal state.
;;
;; We use both evil-set-initial-state AND a mode hook for robustness:
;; - evil-set-initial-state: handles new buffers entering this mode
;; - mode hook: forces emacs state even if evil-collection or user config
;;   has set a different state for org-mode (our parent)
(with-eval-after-load 'evil
  (evil-set-initial-state 'org-gtd-someday-review-mode 'emacs)
  (add-hook 'org-gtd-someday-review-mode-hook #'evil-emacs-state))

;;;; Entry Point

;;;###autoload
(defun org-gtd-reflect-someday-review (&optional list)
  "Review someday/maybe items one at a time.
With optional LIST argument, review only items in that list.
When `org-gtd-someday-lists' is configured, prompts for list selection.
Adds \\='Unassigned\\=' option for items without a list."
  (interactive
   (list (when org-gtd-someday-lists
           (completing-read "Review which list? "
                            (append org-gtd-someday-lists '("Unassigned"))
                            nil t))))
  (let* ((list-filter (cond
                       ((equal list "Unassigned") 'unassigned)
                       ((and list (not (string-empty-p list))) list)
                       (t nil)))
         (items (org-gtd-someday-review--find-items list-filter)))
    (if (null items)
        (message "No someday items to review.")
      (let ((spec (org-gtd-someday-review--spec)))
        (setq spec (plist-put spec :find (lambda () items)))
        (setq spec (plist-put spec :scope (org-agenda-files)))
        (org-gtd-walk-start spec (org-gtd-someday-review--surface))))))

;;;; Commands

(defun org-gtd-someday-review-defer ()
  "Defer the current item (log a review) and advance."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (org-with-point-at marker
           (org-gtd-someday-review--add-reviewed-entry)
           (save-buffer)))
       (org-gtd-someday-review--bump :reviewed)
       (org-gtd-walk-advance)))))

(defun org-gtd-someday-review-clarify ()
  "Reactivate the current item and advance."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (org-with-point-at marker (org-gtd-reactivate)))
       (org-gtd-someday-review--bump :clarified)
       (org-gtd-walk-advance)))))

(defun org-gtd-someday-review-quit ()
  "Abandon the review: report the summary, clean up, tear down the walk."
  (interactive)
  (let ((reviewed (plist-get org-gtd-someday-review--counters :reviewed))
        (clarified (plist-get org-gtd-someday-review--counters :clarified)))
    (org-gtd-walk-quit)
    (org-gtd-wip--cleanup-temp-file org-gtd-someday-review--surface-key)
    (message "Review complete. %d items reviewed, %d clarified." reviewed clarified)))

;;;; Footer

(provide 'org-gtd-someday-review)

;;; org-gtd-someday-review.el ends here
