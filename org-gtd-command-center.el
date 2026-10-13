;;; org-gtd-command-center.el --- Command center transient menu -*- lexical-binding: t; coding: utf-8 -*-
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
;; Global command center transient menu for org-gtd.
;; Provides a single entry point to all major GTD operations,
;; organized for both discovery (new users) and quick access (experienced users).
;;
;;; Code:

;;;; Requirements

(require 'transient)
(require 'org-gtd-archive)
(require 'org-gtd-capture)
(require 'org-gtd-clarify)
(require 'org-gtd-engage)
(require 'org-gtd-process)
(require 'org-gtd-reflect)
(require 'org-gtd-review-counts)

;;;; Counted descriptions

(defvar org-gtd-command-center--counts nil
  "The count job of the open review menus, or nil.
The main menu and its sub-menus share this job, so a sub-menu reuses the
ID index and the counts that its parent computed.")

(defconst org-gtd-command-center--counted-prefixes
  '(org-gtd-command-center
    org-gtd-command-center--stuck
    org-gtd-command-center--missed)
  "Menus that show review counts.")

(defclass org-gtd-command-center--counted-suffix (transient-suffix)
  ((count-key :initarg :count-key))
  "A review suffix whose count comes from the menu's count job.")

(cl-defmethod transient-format-description
  ((suffix org-gtd-command-center--counted-suffix))
  "Format SUFFIX using the current count job.
Show nothing for zero, (…) while pending and (?) after an error."
  (let ((label (cl-call-next-method)))
    (if org-gtd-review-show-counts
        (let ((count (org-gtd-review-counts--total
                      org-gtd-command-center--counts
                      (org-gtd-command-center--count-specs
                       (oref suffix count-key)))))
          (pcase count
            (0 label)
            ('pending (format "%s (…)" label))
            ((pred integerp) (format "%s (%d)" label count))
            (_ (format "%s (?)" label))))
      label)))

(defun org-gtd-command-center--count-specs (key)
  "Return the review view specifications represented by KEY."
  (pcase key
    ('stuck org-gtd-reflect-stuck-items-view-specs)
    ('missed org-gtd-reflect-missed-engagements-view-specs)
    ('missed-calendar (list (cadr org-gtd-reflect-missed-engagements-view-specs)))
    ('missed-delegated (list (car org-gtd-reflect-missed-engagements-view-specs)))
    ('missed-projects (nthcdr 2 org-gtd-reflect-missed-engagements-view-specs))
    ('someday (list org-gtd-reflect-someday-maybe-view-spec))
    ('upcoming (list org-gtd-reflect-upcoming-delegated-view-spec))
    ('completed (list org-gtd-reflect-completed-projects-view-spec))
    (_ (list (org-gtd-reflect--stuck-view-spec key)))))

(defun org-gtd-command-center--counts-updated (job)
  "Show the new counts of JOB in the open menu.
Warn once if JOB failed."
  (when (eq job org-gtd-command-center--counts)
    (when-let* ((err (org-gtd-review-counts--job-error job)))
      (display-warning 'org-gtd
                       (format "Review counts unavailable: %s"
                               (error-message-string err))
                       :warning))
    (when (and transient--prefix
               (memq (oref transient--prefix command)
                     org-gtd-command-center--counted-prefixes)
               (window-live-p transient--window))
      (transient--redisplay))))

(defun org-gtd-command-center--stop-counts ()
  "Stop the count job when no review menu stays open."
  (unless (or (and transient--prefix
                   (memq (oref transient--prefix command)
                         org-gtd-command-center--counted-prefixes))
              (cl-some (lambda (entry)
                         (memq (car entry)
                               org-gtd-command-center--counted-prefixes))
                       transient--stack))
    (org-gtd-review-counts--cancel org-gtd-command-center--counts)
    (setq org-gtd-command-center--counts nil)
    (remove-hook 'transient-exit-hook #'org-gtd-command-center--stop-counts)))

(defun org-gtd-command-center--setup (prefix keys)
  "Open PREFIX now, and count the views of KEYS in short timer steps.
A sub-menu opened from an open review menu reuses the job of that menu."
  (unless org-gtd-review-show-counts
    (org-gtd-review-counts--cancel org-gtd-command-center--counts)
    (setq org-gtd-command-center--counts nil)
    (remove-hook 'transient-exit-hook #'org-gtd-command-center--stop-counts))
  (when org-gtd-review-show-counts
    (let ((specs (mapcan (lambda (key)
                           (copy-sequence (org-gtd-command-center--count-specs key)))
                         keys)))
      (if (and org-gtd-command-center--counts
               (memq transient-current-command
                     org-gtd-command-center--counted-prefixes))
          (org-gtd-review-counts--request org-gtd-command-center--counts specs)
        (org-gtd-review-counts--cancel org-gtd-command-center--counts)
        (setq org-gtd-command-center--counts
              (org-gtd-review-counts--start
               specs #'org-gtd-command-center--counts-updated))
        (add-hook 'transient-exit-hook #'org-gtd-command-center--stop-counts)
        (org-gtd-review-counts--schedule org-gtd-command-center--counts))))
  (transient-setup prefix))

;;;; Main Transient

;;;###autoload (autoload 'org-gtd-command-center "org-gtd-command-center" nil t)
(transient-define-prefix org-gtd-command-center ()
  "GTD command center - entry point to all org-gtd operations."
  [["Engage"
    ("e" "Daily view" org-gtd-engage)
    ("@" "By context" org-gtd-engage-grouped-by-context)
    ("n" "All next actions" org-gtd-show-all-next)]
   ["Capture & Process"
    ("c" "Capture to inbox" org-gtd-capture)
    ("p" "Process inbox" org-gtd-process-inbox)
    ("k" "Clarify at point" org-gtd-clarify-item)]]
  [["Reflect"
    ("a" "Area of focus" org-gtd-reflect-area-of-focus)
    ("y" "Someday/maybe" org-gtd-reflect-someday-maybe
     :class org-gtd-command-center--counted-suffix :count-key someday)
    ("d" "Upcoming delegated" org-gtd-reflect-upcoming-delegated
     :class org-gtd-command-center--counted-suffix :count-key upcoming)
    ("r" "Completed items" org-gtd-reflect-completed-items)
    ("R" "Completed projects" org-gtd-reflect-completed-projects
     :class org-gtd-command-center--counted-suffix :count-key completed)]
   ["Archive"
    ("A" "Archive completed" org-gtd-archive-completed-items)]]
  ["Review System"
   ("S" "Stuck items..." org-gtd-command-center--stuck
    :class org-gtd-command-center--counted-suffix :count-key stuck)
   ("M" "Missed items..." org-gtd-command-center--missed
    :class org-gtd-command-center--counted-suffix :count-key missed)]
  [("q" "Quit" transient-quit-one)]
  (interactive)
  (org-gtd-command-center--setup
   'org-gtd-command-center '(someday upcoming completed stuck missed)))

;;;; Sub-menus

(transient-define-prefix org-gtd-command-center--stuck ()
  "Review stuck items by category."
  ["Stuck Items"
   ("p" "Projects" org-gtd-reflect-stuck-projects
    :class org-gtd-command-center--counted-suffix :count-key stuck-project)
   ("c" "Calendar" org-gtd-reflect-stuck-calendar-items
    :class org-gtd-command-center--counted-suffix :count-key stuck-calendar)
   ("d" "Delegated" org-gtd-reflect-stuck-delegated-items
    :class org-gtd-command-center--counted-suffix :count-key stuck-delegated)
   ("h" "Habits" org-gtd-reflect-stuck-habit-items
    :class org-gtd-command-center--counted-suffix :count-key stuck-habit)
   ("t" "Tickler" org-gtd-reflect-stuck-tickler-items
    :class org-gtd-command-center--counted-suffix :count-key stuck-tickler)
   ("s" "Single actions" org-gtd-reflect-stuck-next-action-items
    :class org-gtd-command-center--counted-suffix :count-key stuck-next-action)]
  [("q" "Back" transient-quit-one)]
  (interactive)
  (org-gtd-command-center--setup
   'org-gtd-command-center--stuck
   '(stuck-project stuck-calendar stuck-delegated stuck-habit
     stuck-tickler stuck-next-action)))

(transient-define-prefix org-gtd-command-center--missed ()
  "Review missed items by category."
  ["Missed Items"
   ("a" "All missed" org-gtd-reflect-missed-engagements
    :class org-gtd-command-center--counted-suffix :count-key missed)
   ("c" "Calendar only" org-gtd-reflect-missed-calendar
    :class org-gtd-command-center--counted-suffix :count-key missed-calendar)
   ("d" "Delegated only" org-gtd-reflect-missed-delegated
    :class org-gtd-command-center--counted-suffix :count-key missed-delegated)
   ("p" "Projects only" org-gtd-reflect-missed-projects
    :class org-gtd-command-center--counted-suffix :count-key missed-projects)]
  [("q" "Back" transient-quit-one)]
  (interactive)
  (org-gtd-command-center--setup
   'org-gtd-command-center--missed
   '(missed missed-calendar missed-delegated missed-projects)))

;;;; Footer

(provide 'org-gtd-command-center)

;;; org-gtd-command-center.el ends here
