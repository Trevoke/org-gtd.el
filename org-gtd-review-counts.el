;;; org-gtd-review-counts.el --- Counts for review menus -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Read-only counts of the native tags views used by review menus.
;; A count job runs in short timer steps.  Each step indexes the IDs of
;; one file, or runs the native Org scanner for one view in one file.
;; Rendering a transient description never scans agenda files.

;;; Code:

(require 'org-id)
(require 'org-gtd-view-language)

(defcustom org-gtd-review-show-counts t
  "Whether review menus show counts of matching entries.
Counts are computed in short steps after a menu opens, not on redisplay.
Set this to nil to avoid scanning agenda files and project-ID sources for
menu counts."
  :group 'org-gtd
  :type 'boolean)

(defvar org-gtd-review-counts--new-buffers nil
  "File buffers opened during the current count snapshot.")

(defun org-gtd-review-counts--buffer (file)
  "Return an Org buffer for FILE without running user hooks or local eval.
Remember newly opened buffers for cleanup after counting."
  (or (org-find-base-buffer-visiting file)
      (progn
        (unless (file-readable-p file)
          (error "Cannot read review count source: %s" file))
        (let ((org-inhibit-startup t)
              (org-agenda-file-menu-enabled nil)
              (non-essential t)
              (large-file-warning-threshold nil)
              (enable-local-eval nil)
              (enable-local-variables :safe)
              (find-file-hook nil)
              (org-mode-hook nil)
              (change-major-mode-hook nil)
              (after-change-major-mode-hook nil))
          (let ((buffer (find-file-noselect file)))
            (push buffer org-gtd-review-counts--new-buffers)
            buffer)))))

(defun org-gtd-review-counts--id-sources ()
  "Return the sources Org searches when resolving project IDs.
Use the same source set as `org-id-update-id-locations'."
  (delete-dups
   (cl-remove-if-not
     #'stringp
     (append (org-agenda-files t org-id-search-archives)
             (if (symbolp org-id-extra-files)
                 (symbol-value org-id-extra-files)
               org-id-extra-files)
             org-id-files
             (mapcar #'buffer-file-name (org-buffer-list 'files t))))))

(defun org-gtd-review-counts--index-file (file locations)
  "Add the IDs in FILE to the hash table LOCATIONS.
Use unsaved buffer contents when FILE is already open."
  (setq file (file-truename file))
  ;; Org's ID discovery also ignores missing historical ID sources.
  (when (or (org-find-base-buffer-visiting file) (file-exists-p file))
    (with-current-buffer (org-gtd-review-counts--buffer file)
      (unless (derived-mode-p 'org-mode)
        (error "Review count source is not in Org mode: %s" file))
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward org-heading-regexp nil t)
         (when-let* ((id (org-entry-get nil "ID")))
           (puthash id file locations)))))))

(defun org-gtd-review-counts--prepare (spec)
  "Return the native tags matcher state for the review SPEC."
  (let ((block (org-gtd-view-lang--create-agenda-block
                (org-gtd-view-lang--normalize-view-spec spec))))
    (unless (memq (car block) '(tags tags-todo))
      (error "Review counts require a tags view: %S" spec))
    (list :todo-only (eq (car block) 'tags-todo)
          :skip (eval (cadr (assq 'org-agenda-skip-function (nth 2 block))) t)
          :matcher (cdr (org-make-tags-matcher (nth 1 block))))))

(defun org-gtd-review-counts--count-file (prepared file)
  "Count the entries in FILE that match the PREPARED view.
Use both the native tags matcher and the translated skip function."
  (let ((org--matcher-tags-todo-only (plist-get prepared :todo-only))
        (org-tags-match-list-sublevels org-tags-match-list-sublevels)
        (org-agenda-skip-function (plist-get prepared :skip))
        (count 0))
    (with-current-buffer (org-gtd-review-counts--buffer file)
      (unless (derived-mode-p 'org-mode)
        (error "Review count source is not in Org mode: %s" file))
      (org-with-wide-buffer
       (when (eq (current-buffer) org-agenda-restrict)
         (narrow-to-region org-agenda-restrict-begin org-agenda-restrict-end))
       (org-scan-tags (lambda () (cl-incf count))
                      (plist-get prepared :matcher)
                      org--matcher-tags-todo-only)))
    count))

;;;; Incremental count jobs

(cl-defstruct (org-gtd-review-counts--job
               (:constructor org-gtd-review-counts--job-create)
               (:copier nil))
  "Resumable state for the counts of one review menu session.
Each step does one unit of work: it loads ID state, indexes one source
file, or runs the native scanner for one view in one file."
  (phase 'start) id-files sources files ids queue spec prepared
  pending partial results error timer cancelled on-update)

(defvar org-gtd-review-counts--step-seconds 0.05
  "Time that a scheduled count job can use before it yields.
A job always completes at least one step.  One step scans one file for
one view, so a very large file can use more time than this.")

(defun org-gtd-review-counts--start (specs &optional on-update)
  "Return a new job that counts SPECS.
Call ON-UPDATE with the job after each completed view, error or finish."
  (org-gtd-review-counts--job-create
   :queue (delete-dups (copy-sequence specs))
   :id-files org-id-files
   :on-update on-update))

(defun org-gtd-review-counts--request (job specs)
  "Ask JOB to count SPECS next, and reuse counts that it has already."
  (let ((new (cl-remove-if (lambda (spec)
                             (or (assoc spec (org-gtd-review-counts--job-results job))
                                 (equal spec (org-gtd-review-counts--job-spec job))))
                           (delete-dups (copy-sequence specs)))))
    (setf (org-gtd-review-counts--job-queue job)
          (append new (cl-remove-if (lambda (spec) (member spec new))
                                    (org-gtd-review-counts--job-queue job))))
    (when (and new (not (org-gtd-review-counts--job-timer job)))
      (org-gtd-review-counts--schedule job))))

(defun org-gtd-review-counts--done-p (job)
  "Return non-nil when JOB has no more work."
  (or (org-gtd-review-counts--job-error job)
      (org-gtd-review-counts--job-cancelled job)
      (and (eq (org-gtd-review-counts--job-phase job) 'count)
           (null (org-gtd-review-counts--job-spec job))
           (null (org-gtd-review-counts--job-queue job)))))

(defun org-gtd-review-counts--total (job specs)
  "Return the sum of the counts of SPECS in JOB.
Return `pending' while a count is not ready, and nil after an error."
  (cond
   ((or (null job) (org-gtd-review-counts--job-error job)) nil)
   (t (catch 'pending
        (apply #'+ (mapcar (lambda (spec)
                             (or (cdr (assoc spec (org-gtd-review-counts--job-results job)))
                                 (throw 'pending 'pending)))
                           specs))))))

(defun org-gtd-review-counts--step-1 (job)
  "Do one unit of the work in JOB.
Return non-nil only when the unit completed the count of a view."
  (pcase (org-gtd-review-counts--job-phase job)
    ('start
     (unless org-id-locations
       (org-id-locations-load))
     (setf (org-gtd-review-counts--job-id-files job) org-id-files
           (org-gtd-review-counts--job-sources job) (org-gtd-review-counts--id-sources)
           (org-gtd-review-counts--job-files job) (org-agenda-files nil 'ifmode)
           (org-gtd-review-counts--job-ids job) (make-hash-table :test #'equal)
           (org-gtd-review-counts--job-phase job) 'index)
     nil)
    ('index
     (if-let* ((file (pop (org-gtd-review-counts--job-sources job))))
         (org-gtd-review-counts--index-file file (org-gtd-review-counts--job-ids job))
       (setf (org-gtd-review-counts--job-phase job) 'count))
     nil)
    ('count
     (cond
      ((org-gtd-review-counts--job-spec job)
       (if-let* ((file (pop (org-gtd-review-counts--job-pending job))))
           (progn
             (cl-incf (org-gtd-review-counts--job-partial job)
                      (org-gtd-review-counts--count-file
                       (org-gtd-review-counts--job-prepared job) file))
             nil)
         (push (cons (org-gtd-review-counts--job-spec job)
                     (org-gtd-review-counts--job-partial job))
               (org-gtd-review-counts--job-results job))
         (setf (org-gtd-review-counts--job-spec job) nil)
         t))
      ((org-gtd-review-counts--job-queue job)
       (let ((spec (pop (org-gtd-review-counts--job-queue job))))
         (setf (org-gtd-review-counts--job-spec job) spec
               (org-gtd-review-counts--job-prepared job)
               (org-gtd-review-counts--prepare spec)
               (org-gtd-review-counts--job-pending job)
               (org-gtd-review-counts--job-files job)
               (org-gtd-review-counts--job-partial job) 0))
       nil)))))

(defun org-gtd-review-counts--run (job seconds)
  "Do steps of JOB for up to SECONDS, and always do at least one step.
Return non-nil when a view count completed.  Bind Org state only for
these steps: no ID cache is saved or refreshed on disk, no agenda buffer
is created, and buffers that the steps open are killed before return."
  (let ((non-essential t)
        (org-element-use-cache nil)
        (org-gtd-review-counts--new-buffers nil)
        (org-id-locations (or (org-gtd-review-counts--job-ids job) org-id-locations))
        (org-id-files (org-gtd-review-counts--job-id-files job))
        (org-agenda-new-buffers nil)
        (deadline (time-add nil seconds))
        updated)
    (save-match-data
      (save-current-buffer
        (unwind-protect
            ;; A missing ID must not trigger Org's disk-writing refresh.
            ;; All of its discovery sources are indexed before counting.
            (cl-letf (((symbol-function 'org-id-update-id-locations)
                       (lambda (&optional _files _silent)
                         (unless org-id-track-globally
                           (error "Please turn on `org-id-track-globally' if you want to track IDs"))
                         org-id-locations)))
              (while (progn
                       (setq org-id-locations
                             (or (org-gtd-review-counts--job-ids job)
                                 org-id-locations))
                       (when (org-gtd-review-counts--step-1 job)
                         (setq updated t))
                       (and (not (org-gtd-review-counts--done-p job))
                            (time-less-p nil deadline)
                            (not (input-pending-p))))))
          (dolist (buffer org-gtd-review-counts--new-buffers)
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))
    updated))

(defun org-gtd-review-counts--tick (job)
  "Run a bounded part of JOB from a timer, then schedule the rest."
  (setf (org-gtd-review-counts--job-timer job) nil)
  (unless (org-gtd-review-counts--job-cancelled job)
    (let ((updated
           (if (input-pending-p)
               nil
             (condition-case err
                 (org-gtd-review-counts--run job org-gtd-review-counts--step-seconds)
               (error
                (setf (org-gtd-review-counts--job-error job) err)
                t)))))
      (unless (org-gtd-review-counts--done-p job)
        (org-gtd-review-counts--schedule job))
      (when (and updated (org-gtd-review-counts--job-on-update job))
        (funcall (org-gtd-review-counts--job-on-update job) job)))))

(defun org-gtd-review-counts--schedule (job)
  "Run JOB in bounded steps from timers, so that input is not blocked."
  (unless (or (org-gtd-review-counts--job-timer job)
              (org-gtd-review-counts--done-p job))
    (setf (org-gtd-review-counts--job-timer job)
          (run-with-timer 0.01 nil #'org-gtd-review-counts--tick job))))

(defun org-gtd-review-counts--cancel (job)
  "Stop JOB.  Its timer is cancelled and later callbacks do nothing."
  (when job
    (setf (org-gtd-review-counts--job-cancelled job) t)
    (when-let* ((timer (org-gtd-review-counts--job-timer job)))
      (cancel-timer timer)
      (setf (org-gtd-review-counts--job-timer job) nil))))

(defun org-gtd-review-counts--snapshot (specs)
  "Return an alist of unique SPECS and their counts, computed now.
This uses the same steps as a scheduled job, without yielding.
Errors propagate to the caller."
  (let ((job (org-gtd-review-counts--start specs)))
    (while (not (org-gtd-review-counts--done-p job))
      (org-gtd-review-counts--run job most-positive-fixnum))
    (mapcar (lambda (spec)
              (assoc spec (org-gtd-review-counts--job-results job)))
            (delete-dups (copy-sequence specs)))))

(provide 'org-gtd-review-counts)
;;; org-gtd-review-counts.el ends here
