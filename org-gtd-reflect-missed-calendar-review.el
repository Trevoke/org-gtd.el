;;; org-gtd-reflect-missed-calendar-review.el --- Actionable overdue-calendar review -*- lexical-binding: t; coding: utf-8 -*-
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
;; The actionable counterpart of the read-only `org-gtd-reflect-missed-calendar'
;; view: a walk that shows each overdue Calendar item one at a time and lets the
;; user decide -- with consent -- what each one becomes now (done, migrate to a
;; next action, reschedule, trash, clarify, or skip).  A walk consumer,
;; structurally identical to `org-gtd-someday-review'.  See
;; docs/plans/2026-07-19-overdue-calendar-review-design.md.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-gtd-core)
(require 'org-gtd-wip)
(require 'org-gtd-skip)
(require 'org-gtd-types)
(require 'org-gtd-organize-core)
(require 'org-gtd-archive)
(require 'org-gtd-clarify)
(require 'org-gtd-walk-model)
(require 'org-gtd-walk)

;;;; External Function Declarations

;; Evil functions (only called inside with-eval-after-load 'evil)
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-emacs-state "evil-states")

;;;; Variables

(defconst org-gtd-reflect-missed-calendar-review--surface-key "missed-calendar-review"
  "Fixed WIP key for the single missed-calendar-review surface buffer.")

(defvar-local org-gtd-reflect-missed-calendar-review--counters nil
  "Buffer-local plist of tallies for the active surface:
\(:reviewed N :done N :migrated N :rescheduled N :trashed N :skipped N).")

;;;; Keymaps

(defvar org-gtd-reflect-missed-calendar-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "d") #'org-gtd-reflect-missed-calendar-review-done)
    (define-key map (kbd "m") #'org-gtd-reflect-missed-calendar-review-migrate)
    (define-key map (kbd "r") #'org-gtd-reflect-missed-calendar-review-reschedule)
    (define-key map (kbd "t") #'org-gtd-reflect-missed-calendar-review-trash)
    (define-key map (kbd "c") #'org-gtd-reflect-missed-calendar-review-clarify)
    (define-key map (kbd "s") #'org-gtd-reflect-missed-calendar-review-skip)
    (define-key map (kbd "q") #'org-gtd-reflect-missed-calendar-review-quit)
    map)
  "Keymap for `org-gtd-reflect-missed-calendar-review-mode'.")

;;;; Detection

(defun org-gtd-reflect-missed-calendar-review--find-items ()
  "Return the org-ids of every overdue Calendar item across `org-agenda-files'.

Composes `org-gtd-skip.el' predicates matching the design's definition of
overdue calendar: ORG_GTD = Calendar, not done, ORG_GTD_TIMESTAMP strictly
before today, and not an org-gtd habit.  The not-habit clause is redundant
given the Calendar/Habit type invariant (an entry cannot be both) but is
kept for parity with that stated definition.  The predicate factories are
captured once in the outer `let' and `funcall'ed per heading."
  (let ((calendar-p (org-gtd-pred--property-equals
                     "ORG_GTD" (org-gtd-type-org-gtd-value 'calendar)))
        (not-done-p (org-gtd-pred--not-done))
        (overdue-p (org-gtd-pred--property-ts<
                    (org-gtd-type-property 'calendar :when) "today"))
        (not-habit-p (org-gtd-pred--property-not-equals
                      "ORG_GTD" (org-gtd-type-org-gtd-value 'habit)))
        (items '()))
    (dolist (file (org-agenda-files))
      (when (file-exists-p file)
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward "^\\*+ " nil t)
             (when (and (funcall calendar-p)
                        (funcall not-done-p)
                        (funcall overdue-p)
                        (funcall not-habit-p))
               (push (org-id-get-create) items)))))))
    (nreverse items)))

(defun org-gtd-reflect-missed-calendar-review--resolve (id)
  "Return non-nil when ID still resolves to a live heading marker."
  (org-id-find id 'marker))

;;;; Render

(defun org-gtd-reflect-missed-calendar-review--humanize-lapse (ts-string)
  "Return a humanized description of the lapsed date TS-STRING.
E.g. \"was: 2026-06-12 (37 days ago)\".  Returns \"date unknown\" when
TS-STRING cannot be parsed."
  (let ((ts (org-gtd--parse-timestamp ts-string)))
    (if (null ts)
        "date unknown"
      (let ((days (- (org-today) (time-to-days ts))))
        (format "was: %s (%d day%s ago)"
                (format-time-string "%F" ts)
                days
                (if (= days 1) "" "s"))))))

(defun org-gtd-reflect-missed-calendar-review--render (id surface)
  "Render the overdue Calendar item ID into SURFACE (the walk :render contract).
Resolves ID to a marker, refills SURFACE read-only with the teaching
framing, the humanized lapse, an optional area-of-focus line, and the
subtree body, then sets review mode, the header-line action bar, and
displays the buffer."
  (let ((marker (org-id-find id 'marker)))
    (when marker
      (let ((ts (org-with-point-at marker
                  (org-entry-get (point) (org-gtd-type-property 'calendar :when))))
            (aof (org-with-point-at marker
                   (org-entry-get (point) org-gtd-prop-area-of-focus))))
        (with-current-buffer surface
          ;; Bind `org-id-track-globally' to nil so `org-paste-subtree' does
          ;; not re-register the pasted :ID: (via `org-id-paste-tracker') into
          ;; this disposable surface's temp file, which would corrupt the
          ;; global `org-id-locations' map.  This binding is load-bearing, not
          ;; dead code -- see `org-gtd-someday-review--render'.
          (let ((inhibit-read-only t)
                (org-id-track-globally nil))
            (erase-buffer)
            (insert "# This date has passed -- decide what it is now.\n")
            (insert (format "# %s\n"
                            (org-gtd-reflect-missed-calendar-review--humanize-lapse ts)))
            (when (and aof (not (string-empty-p aof)))
              (insert (format "# Area of focus: %s\n" aof)))
            (insert "\n")
            (org-gtd--without-kill-merge
              (org-with-point-at marker (org-copy-subtree)))
            (org-paste-subtree)
            (goto-char (point-min)))
          (unless (eq major-mode 'org-gtd-reflect-missed-calendar-review-mode)
            (org-gtd-reflect-missed-calendar-review-mode))
          (setq buffer-read-only t)
          (let* ((model (plist-get org-gtd-walk--active :model))
                 (pos (1+ (plist-get model :cursor)))
                 (total (length (plist-get model :entries))))
            (setq header-line-format
                  (format (concat "[d] Done  [m] Migrate  [r] Reschedule  "
                                  "[t] Trash  [c] Clarify  [s] Skip  [q] Quit  (%d/%d)")
                          pos total)))
          (pop-to-buffer surface))))))

;;;; Walk Surface

(defun org-gtd-reflect-missed-calendar-review--surface ()
  "Return the fresh WIP surface buffer for a missed-calendar-review walk.
Activates `org-gtd-reflect-missed-calendar-review-mode' before setting the
buffer-local counters: (re-)running a major mode calls
`kill-all-local-variables', which would silently wipe them.  Doing this
here means the mode is already active by the time `org-gtd-walk-start'
calls :render, so :render's own mode-activation guard never fires and the
counters survive the whole walk (mirrors `org-gtd-someday-review--surface')."
  (let ((buf (org-gtd-wip--get-buffer
              org-gtd-reflect-missed-calendar-review--surface-key)))
    (with-current-buffer buf
      (org-gtd-reflect-missed-calendar-review-mode)
      (setq-local org-gtd-reflect-missed-calendar-review--counters
                  (list :reviewed 0 :done 0 :migrated 0
                        :rescheduled 0 :trashed 0 :skipped 0)))
    buf))

(defun org-gtd-reflect-missed-calendar-review--bump (key)
  "Increment counter KEY on the surface buffer's counters plist."
  (setq org-gtd-reflect-missed-calendar-review--counters
        (plist-put org-gtd-reflect-missed-calendar-review--counters key
                   (1+ (plist-get
                        org-gtd-reflect-missed-calendar-review--counters key)))))

(defun org-gtd-reflect-missed-calendar-review--summary ()
  "Return the human-readable tally string for the active surface."
  (let ((c org-gtd-reflect-missed-calendar-review--counters))
    (format "reviewed %d - done %d - migrated %d - rescheduled %d - trashed %d - skipped %d"
            (or (plist-get c :reviewed) 0)
            (or (plist-get c :done) 0)
            (or (plist-get c :migrated) 0)
            (or (plist-get c :rescheduled) 0)
            (or (plist-get c :trashed) 0)
            (or (plist-get c :skipped) 0))))

(defun org-gtd-reflect-missed-calendar-review--on-finish ()
  "End-of-walk: report the tally and clean up the surface buffer.
Runs in the surface buffer after the engine has cleared its session."
  (let ((summary (org-gtd-reflect-missed-calendar-review--summary)))
    (org-gtd-wip--cleanup-temp-file
     org-gtd-reflect-missed-calendar-review--surface-key)
    (message "Missed-calendar review complete. %s" summary)))

(defun org-gtd-reflect-missed-calendar-review--spec ()
  "Return the missed-calendar-review walk spec template.
The default :find covers all overdue calendar items."
  (list :name 'missed-calendar-review
        :find #'org-gtd-reflect-missed-calendar-review--find-items
        :render #'org-gtd-reflect-missed-calendar-review--render
        :actions org-gtd-reflect-missed-calendar-review-mode-map
        :on-finish #'org-gtd-reflect-missed-calendar-review--on-finish
        :resumable nil
        :resolve #'org-gtd-reflect-missed-calendar-review--resolve
        :scope (org-agenda-files)))

(org-gtd-walk-register 'missed-calendar-review
                       (org-gtd-reflect-missed-calendar-review--spec))

;;;; Modes

;;;###autoload
(define-derived-mode org-gtd-reflect-missed-calendar-review-mode org-mode "GTD-MissedCal"
  "Major mode for reviewing overdue calendar items one at a time.
Derived from `org-mode'; the buffer is read-only (set in the render
function) and offers disposition keys.

\\{org-gtd-reflect-missed-calendar-review-mode-map}"
  :group 'org-gtd)

;;;; Evil-mode Integration

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-gtd-reflect-missed-calendar-review-mode 'emacs)
  (add-hook 'org-gtd-reflect-missed-calendar-review-mode-hook #'evil-emacs-state))

;;;; Entry Point

;;;###autoload
(defun org-gtd-reflect-missed-calendar-review ()
  "Review overdue calendar items one at a time.
The actionable counterpart of the read-only `org-gtd-reflect-missed-calendar'
view: walks each open Calendar item whose date has passed and lets you
decide -- with consent -- what it becomes now.  Opens nothing when your
hard landscape is clean."
  (interactive)
  (let ((items (org-gtd-reflect-missed-calendar-review--find-items)))
    (if (null items)
        (message "No overdue calendar items -- your hard landscape is clean.")
      (let ((spec (org-gtd-reflect-missed-calendar-review--spec)))
        (setq spec (plist-put spec :find (lambda () items)))
        (setq spec (plist-put spec :scope (org-agenda-files)))
        (org-gtd-walk-start spec (org-gtd-reflect-missed-calendar-review--surface))))))

;;;; Commands

(defun org-gtd-reflect-missed-calendar-review-done ()
  "Mark the current item done and archive it (it happened), then advance."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (org-with-point-at marker
           ;; Suppress the state-change note prompt: this is a programmatic
           ;; "it already happened" mark, and an unfinished note buffer would
           ;; leave the following archive yanking the subtree out from under it.
           (let ((org-inhibit-logging 'note))
             (org-todo (org-gtd-keywords--done)))
           (org-gtd-archive-item-at-point)))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-reflect-missed-calendar-review--bump :done)
       (org-gtd-walk-advance)))))

(defun org-gtd-reflect-missed-calendar-review-migrate ()
  "Migrate the current item to a Next Action (it still needs doing), then advance.
Runs the headless organize pipeline with the classic decoration hooks
bound off -- the item is already clarified, so it must not be re-prompted
for tags/effort/etc.  The pipeline auto-drops the Calendar-only
ORG_GTD_TIMESTAMP because next-action declares no properties."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (let ((org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'next-action)))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-reflect-missed-calendar-review--bump :migrated)
       (org-gtd-walk-advance)))))

(defun org-gtd-reflect-missed-calendar-review-trash ()
  "Trash the current item (irrelevant now: cancel + archive), then advance.
Reuses the `trash' type's cancel-and-archive disposition through the
headless pipeline, with decoration hooks bound off."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker)))
       (when marker
         (let ((org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'trash)))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-reflect-missed-calendar-review--bump :trashed)
       (org-gtd-walk-advance)))))

(defun org-gtd-reflect-missed-calendar-review--read-future-date ()
  "Prompt for a date via `org-read-date', re-prompting until it is today or later.
Returns the chosen date as a \"YYYY-MM-DD\" string.  A past reschedule
is rejected, not silently accepted."
  (let ((today (org-today))
        date)
    (while (progn
             (setq date (org-read-date))
             (< (time-to-days (org-time-string-to-time date)) today))
      (message "That date is also in the past -- pick today or later.")
      (sit-for 1))
    date))

(defun org-gtd-reflect-missed-calendar-review-reschedule ()
  "Reschedule the current item to a new (today-or-later) date, then advance.
Stays a Calendar item; reuses the headless organize pipeline with the
decoration hooks bound off.  The :when config value is bracketed so it is
written verbatim as a valid org timestamp."
  (interactive)
  (org-gtd-walk-call-action
   (lambda ()
     (let* ((id (org-gtd-walk-model-current
                 (plist-get org-gtd-walk--active :model)))
            (marker (org-id-find id 'marker))
            (date (org-gtd-reflect-missed-calendar-review--read-future-date)))
       (when marker
         (let ((org-gtd-organize-hooks nil))
           (org-gtd-process-heading marker 'calendar
                                    (list (cons :when (format "<%s>" date))))))
       (org-gtd-reflect-missed-calendar-review--bump :reviewed)
       (org-gtd-reflect-missed-calendar-review--bump :rescheduled)
       (org-gtd-walk-advance)))))

(defun org-gtd-reflect-missed-calendar-review-quit ()
  "Abandon the review: report the tally, clean up, tear down the walk."
  (interactive)
  (let ((summary (org-gtd-reflect-missed-calendar-review--summary)))
    (org-gtd-walk-quit)
    (org-gtd-wip--cleanup-temp-file
     org-gtd-reflect-missed-calendar-review--surface-key)
    (message "Missed-calendar review complete. %s" summary)))

;;;; Footer

(provide 'org-gtd-reflect-missed-calendar-review)

;;; org-gtd-reflect-missed-calendar-review.el ends here
