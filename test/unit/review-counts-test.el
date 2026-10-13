;;; review-counts-test.el --- Review count snapshots -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Compare count snapshots with real review views using synthetic Org data.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-review-counts)
(require 'org-gtd-command-center)
(e-unit-initialize)

(defvar native-comp-enable-subr-trampolines)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (let ((org-gtd-review-show-counts t)
          (org-id-locations-file "/mock:/gtd/id-locations")
          (org-id-extra-files nil)
          (org-id-search-archives nil)
          (native-comp-enable-subr-trampolines nil)
          (org-agenda-sticky nil)
          (org-gtd-command-center--counts nil))
      (unwind-protect
          (funcall proceed context)
        (org-gtd-review-counts--cancel org-gtd-command-center--counts)
        (remove-hook 'transient-exit-hook #'org-gtd-command-center--stop-counts)))))

(defun ogt-count-test--drain (job)
  "Run the timer steps of JOB until it is done, and return the step count."
  (let ((ticks 0))
    (cl-letf (((symbol-function 'input-pending-p) (lambda () nil)))
      (while (not (org-gtd-review-counts--done-p job))
        (when-let* ((timer (org-gtd-review-counts--job-timer job)))
          (cancel-timer timer))
        (org-gtd-review-counts--tick job)
        (cl-incf ticks))
      ticks)))

(defun ogt-count-test--open (keys)
  "Open a review menu for KEYS without a UI, and return its count job."
  (cl-letf (((symbol-function 'transient-setup) #'ignore))
    (org-gtd-command-center--setup 'org-gtd-command-center keys))
  org-gtd-command-center--counts)

(defun ogt-count-test--source (text)
  "Put TEXT in an unsaved agenda source buffer."
  (with-current-buffer (find-file-noselect "/mock:/gtd/org-gtd-tasks.org")
    (erase-buffer)
    (insert text)
    (org-mode)
    (current-buffer)))

(defun ogt-count-test--count (spec)
  "Return the snapshot count for SPEC."
  (cdr (assoc spec (org-gtd-review-counts--snapshot (list spec)))))

(defun ogt-count-test--view-count (spec)
  "Count actual agenda entry lines in the view for SPEC."
  (org-gtd-view-show spec)
  (let ((count 0))
    (goto-char (point-min))
    (while (not (eobp))
      (when (get-text-property (point) 'org-hd-marker)
        (cl-incf count))
      (forward-line 1))
    count))

(deftest review-counts/native-todo-matcher ()
  "Delegated counts require the configured WAIT state, not just the skip filter."
  (let ((org-todo-keywords '((sequence "OPEN" "READY" "HOLD" "|" "FINISHED")))
        (org-gtd-keyword-mapping
         '((todo . "OPEN") (next . "READY") (wait . "HOLD")
           (done . "FINISHED") (canceled . "FINISHED"))))
    (ogt-count-test--source
     "* HOLD Correct state\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* OPEN Wrong state\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* FINISHED Finished\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n")
    (let ((spec (car org-gtd-reflect-missed-engagements-view-specs)))
      (assert-equal 1 (ogt-count-test--count spec))
      (assert-equal 1 (ogt-count-test--view-count spec)))))

(deftest review-counts/all-review-specs-match-views ()
  "Every counted review specification agrees with its actual agenda view."
  (ogt-count-test--source
   "* TODO Unready\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* NEXT Ready\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* DONE Finished action\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* TODO Missing date\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n* DONE Done missing date\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n* TODO Old calendar\n:PROPERTIES:\n:ORG_GTD: Calendar\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* WAIT Old delegation\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:DELEGATED_TO: Example\n:END:\n* WAIT Future delegation\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2099-01-01 Thu>\n:DELEGATED_TO: Example\n:END:\n* TODO Invalid habit\n:PROPERTIES:\n:ORG_GTD: Habit\n:END:\n* TODO Invalid tickler\n:PROPERTIES:\n:ORG_GTD: Tickler\n:END:\n* Maybe\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n* TODO Archived :ARCHIVE:\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n** TODO Archived child\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* COMMENT Hidden\n** TODO Comment child\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n")
  (dolist (spec (append org-gtd-reflect-stuck-items-view-specs
                       org-gtd-reflect-missed-engagements-view-specs
                       (list org-gtd-reflect-upcoming-delegated-view-spec
                             org-gtd-reflect-someday-maybe-view-spec
                             org-gtd-reflect-completed-projects-view-spec)))
    (let ((count (ogt-count-test--count spec)))
      (assert-equal (ogt-count-test--view-count spec) count))))

(deftest review-counts/projects-use-graph-not-child-count ()
  "One project with two blocked graph tasks counts once, even across files."
  (let ((source (ogt-count-test--source
                 "* TODO Project\nDEADLINE: <2000-01-01 Sat> SCHEDULED: <2000-01-01 Sat>\n:PROPERTIES:\n:ID: count-project\n:ORG_GTD: Projects\n:ORG_GTD_FIRST_TASKS: count-task-a\n:END:\n")))
    (with-current-buffer (find-file-noselect "/mock:/gtd/inbox.org")
      (insert "* TODO Task A\n:PROPERTIES:\n:ID: count-task-a\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: count-project\n:ORG_GTD_BLOCKS: count-task-b\n:END:\n* TODO Task B\n:PROPERTIES:\n:ID: count-task-b\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: count-project\n:END:\n")
      (org-mode))
    (org-id-add-location "count-project" (buffer-file-name source))
    (org-id-add-location "count-task-a" "/mock:/gtd/inbox.org")
    (org-id-add-location "count-task-b" "/mock:/gtd/inbox.org")
    (let ((spec (org-gtd-reflect--stuck-view-spec 'stuck-project)))
      (assert-equal 1 (ogt-count-test--count spec))
      (assert-equal 1 (ogt-count-test--view-count spec)))
    (assert-equal 0 (ogt-count-test--count org-gtd-reflect-completed-projects-view-spec))
    ;; An overdue and late-starting project occurs in two view blocks.
    (assert-equal 2
                  (apply #'+
                         (mapcar #'ogt-count-test--count
                                 (org-gtd-command-center--count-specs 'missed-projects))))))

(deftest review-counts/inheritance-and-skip-settings ()
  "Counts use native inheritance, archive/comment and global skip settings."
  (ogt-count-test--source
   "* Parent\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n** Child\n* Archived :ARCHIVE:\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n* COMMENT Hidden\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n")
  (dolist (inherit '(nil t))
    (dolist (archives '(nil t))
      (let ((org-use-property-inheritance inherit)
            (org-agenda-archives-mode archives)
            (spec org-gtd-reflect-someday-maybe-view-spec))
        (assert-equal (ogt-count-test--view-count spec)
                      (ogt-count-test--count spec)))))
  (let ((org-agenda-skip-function-global
         (lambda () (org-entry-end-position))))
    (assert-equal 0
                  (ogt-count-test--count org-gtd-reflect-someday-maybe-view-spec))))

(deftest review-counts/no-ui-data-or-id-cache-side-effects ()
  "Counting respects unsaved data and leaves point, narrowing and IDs unchanged."
  (let* ((source (ogt-count-test--source
                  "* TODO Project\n:PROPERTIES:\n:ID: unsaved-project\n:ORG_GTD: Projects\n:ORG_GTD_FIRST_TASKS: unsaved-task missing-task\n:END:\n* TODO Task\n:PROPERTIES:\n:ID: unsaved-task\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: unsaved-project\n:END:\n"))
         (ids org-id-locations)
         (buffers (buffer-list))
         (window (selected-window))
         (spec (org-gtd-reflect--stuck-view-spec 'stuck-project)))
    (with-current-buffer source
      (goto-char (point-max))
      (narrow-to-region (line-beginning-position 0) (point-max))
      (let ((position (point))
            (start (point-min))
            (end (point-max))
            (tick (buffer-chars-modified-tick)))
        (cl-letf (((symbol-function 'org-agenda)
                   (lambda (&rest _) (error "Opened agenda while counting")))
                  ((symbol-function 'write-region)
                   (lambda (&rest _) (error "Wrote a file while counting")))
                  ((symbol-function 'yes-or-no-p)
                   (lambda (&rest _) (error "Prompted while counting")))
                  ((symbol-function 'y-or-n-p)
                   (lambda (&rest _) (error "Prompted while counting"))))
          (assert-equal 1 (ogt-count-test--count spec)))
        (assert-equal position (point))
        (assert-equal start (point-min))
        (assert-equal end (point-max))
        (assert-equal tick (buffer-chars-modified-tick))
        (assert-true (buffer-modified-p))))
    (assert-true (eq ids org-id-locations))
    (assert-equal 0 (hash-table-count ids))
    (assert-true (eq window (selected-window)))
    (assert-equal (sort (mapcar #'buffer-name buffers) #'string<)
                  (sort (mapcar #'buffer-name (buffer-list)) #'string<))))

(deftest review-counts/snapshot-deduplicates-views ()
  "Overlapping menu entries compute each constituent view only once."
  (let ((calls 0)
        (spec org-gtd-reflect-someday-maybe-view-spec))
    (cl-letf (((symbol-function 'org-gtd-review-counts--prepare)
               (lambda (&rest _) (cl-incf calls) nil))
              ((symbol-function 'org-gtd-review-counts--count-file)
               (lambda (&rest _) 0)))
      (assert-equal (list (cons spec 0))
                    (org-gtd-review-counts--snapshot (list spec spec))))
    (assert-equal 1 calls)))

(deftest review-counts/reopen-refreshes-unsaved-changes ()
  "Reopening recomputes zero/nonzero counts without a TTL."
  (let ((source (ogt-count-test--source "* Candidate\n"))
        (specs (org-gtd-command-center--count-specs 'someday))
        totals)
    (let ((job (ogt-count-test--open '(someday))))
      (ogt-count-test--drain job)
      (push (org-gtd-review-counts--total job specs) totals))
    (with-current-buffer source
      (goto-char (point-min))
      (org-entry-put nil "ORG_GTD" "Someday"))
    (let ((job (ogt-count-test--open '(someday))))
      (ogt-count-test--drain job)
      (push (org-gtd-review-counts--total job specs) totals))
    (assert-equal '(1 0) totals)))

(deftest review-counts/menu-opens-before-any-scan ()
  "Opening a menu schedules the work; it reads no file before it shows."
  (let ((scans 0) shown)
    (cl-letf (((symbol-function 'org-gtd-review-counts--index-file)
               (lambda (&rest _) (cl-incf scans)))
              ((symbol-function 'org-gtd-review-counts--count-file)
               (lambda (&rest _) (cl-incf scans) 0))
              ((symbol-function 'transient-setup)
               (lambda (&rest _) (setq shown (= scans 0)))))
      (org-gtd-command-center--setup 'org-gtd-command-center '(someday stuck)))
    (assert-true shown)
    (assert-equal 0 scans)
    (assert-true (timerp (org-gtd-review-counts--job-timer
                          org-gtd-command-center--counts)))
    (assert-equal 'pending
                  (org-gtd-review-counts--total
                   org-gtd-command-center--counts
                   (org-gtd-command-center--count-specs 'someday)))))

(deftest review-counts/steps-yield-per-file-and-view ()
  "With no time budget, each timer step indexes or scans at most one file."
  (let ((org-gtd-review-counts--step-seconds 0)
        (org-agenda-files '("/mock:/gtd/org-gtd-tasks.org" "/mock:/gtd/inbox.org"))
        (work 0)
        (max-work 0)
        (updates 0))
    (ogt-count-test--source "* Maybe\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n")
    (let ((job (org-gtd-review-counts--start
                (list org-gtd-reflect-someday-maybe-view-spec
                      org-gtd-reflect-completed-projects-view-spec)
                (lambda (_job) (cl-incf updates))))
          (index (symbol-function 'org-gtd-review-counts--index-file))
          (count (symbol-function 'org-gtd-review-counts--count-file)))
      (cl-letf (((symbol-function 'org-gtd-review-counts--index-file)
                 (lambda (&rest args) (cl-incf work) (apply index args)))
                ((symbol-function 'org-gtd-review-counts--count-file)
                 (lambda (&rest args) (cl-incf work) (apply count args))))
        (while (not (org-gtd-review-counts--done-p job))
          (setq work 0)
          (when-let* ((timer (org-gtd-review-counts--job-timer job)))
            (cancel-timer timer))
          (org-gtd-review-counts--tick job)
          (setq max-work (max max-work work))))
      (assert-equal 1 max-work)
      (assert-equal 2 updates)
      (assert-equal 1 (org-gtd-review-counts--total
                       job (list org-gtd-reflect-someday-maybe-view-spec))))))

(deftest review-counts/pending-input-defers-work ()
  "A step does no work while input waits, and it schedules a retry."
  (let ((job (org-gtd-review-counts--start
              (list org-gtd-reflect-someday-maybe-view-spec))))
    (cl-letf (((symbol-function 'input-pending-p) (lambda (&rest _) t))
              ((symbol-function 'org-gtd-review-counts--step-1)
               (lambda (&rest _) (error "Worked during input"))))
      (org-gtd-review-counts--tick job))
    (assert-true (timerp (org-gtd-review-counts--job-timer job)))
    (org-gtd-review-counts--cancel job)))

(deftest review-counts/closing-menu-cancels-job ()
  "Closing the last menu cancels the timer and ignores late callbacks."
  (let ((job (ogt-count-test--open '(someday))))
    (let ((transient--prefix nil)
          (transient--stack nil))
      (run-hooks 'transient-exit-hook))
    (assert-nil org-gtd-command-center--counts)
    (assert-nil (org-gtd-review-counts--job-timer job))
    (assert-nil (memq #'org-gtd-command-center--stop-counts transient-exit-hook))
    (cl-letf (((symbol-function 'org-gtd-review-counts--step-1)
               (lambda (&rest _) (error "Stale callback ran"))))
      (org-gtd-review-counts--tick job))
    (assert-equal 'pending
                  (org-gtd-review-counts--total
                   job (org-gtd-command-center--count-specs 'someday)))))

(deftest review-counts/reopening-replaces-old-job ()
  "A new top-level opening cancels the old job instead of reusing it."
  (let* ((old (ogt-count-test--open '(someday)))
         (new (ogt-count-test--open '(someday))))
    (assert-true (org-gtd-review-counts--job-cancelled old))
    (assert-nil (org-gtd-review-counts--job-timer old))
    (assert-true (eq new org-gtd-command-center--counts))
    (assert-nil (org-gtd-review-counts--job-cancelled new))))

(deftest review-counts/unrelated-prefix-cancels-job ()
  "Replacing a review menu with an unrelated transient stops count work."
  (let ((job (ogt-count-test--open '(someday))))
    (let ((transient--prefix (transient-prefix :command 'ignore))
          (transient--stack nil))
      (run-hooks 'transient-exit-hook))
    (assert-nil org-gtd-command-center--counts)
    (assert-true (org-gtd-review-counts--job-cancelled job))))

(deftest review-counts/stale-results-do-not-redisplay ()
  "A callback from a previous opening cannot update the current menu."
  (let ((old (ogt-count-test--open '(someday))))
    (ogt-count-test--open '(someday))
    (setf (org-gtd-review-counts--job-error old) '(error "Obsolete"))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (error "Warned from stale job")))
              ((symbol-function 'transient--redisplay)
               (lambda (&rest _) (error "Redisplayed from stale job"))))
      (org-gtd-command-center--counts-updated old))))

(deftest review-counts/scheduled-project-ids-remain-private ()
  "Project predicates resolve unsaved cross-file IDs between timer steps."
  (let ((org-gtd-review-counts--step-seconds 0)
        (ids org-id-locations))
    (ogt-count-test--source
     "* TODO Project\n:PROPERTIES:\n:ID: step-project\n:ORG_GTD: Projects\n:ORG_GTD_FIRST_TASKS: step-task\n:END:\n")
    (with-current-buffer (find-file-noselect "/mock:/gtd/inbox.org")
      (insert "* NEXT Task\n:PROPERTIES:\n:ID: step-task\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: step-project\n:END:\n")
      (org-mode))
    (let ((job (ogt-count-test--open '(stuck))))
      (ogt-count-test--drain job)
      (assert-equal 0 (org-gtd-review-counts--total
                       job (org-gtd-command-center--count-specs 'stuck-project))))
    (assert-true (eq ids org-id-locations))
    (assert-equal 0 (hash-table-count ids))))

(deftest review-counts/sub-menu-reuses-parent-counts ()
  "A sub-menu reuses the ID index and counts of its parent menu."
  (let ((job (ogt-count-test--open '(stuck someday)))
        (prepared 0)
        (indexed 0))
    (ogt-count-test--drain job)
    (cl-letf (((symbol-function 'org-gtd-review-counts--prepare)
               (lambda (&rest _) (cl-incf prepared) nil))
              ((symbol-function 'org-gtd-review-counts--index-file)
               (lambda (&rest _) (cl-incf indexed)))
              ((symbol-function 'transient-setup) #'ignore))
      (let ((transient-current-command 'org-gtd-command-center))
        (org-gtd-command-center--setup
         'org-gtd-command-center--stuck
         '(stuck-project stuck-calendar stuck-delegated stuck-habit
           stuck-tickler stuck-next-action))))
    (assert-true (eq job org-gtd-command-center--counts))
    (assert-equal 0 prepared)
    (assert-equal 0 indexed)
    (assert-nil (org-gtd-review-counts--job-timer job))))

(deftest review-counts/large-file-is-scanned-after-opening ()
  "A large single file is not read when the menu opens."
  (ogt-count-test--source
   (mapconcat (lambda (n)
                (format "* Maybe %d\n:PROPERTIES:\n:ORG_GTD: Someday\n:ID: large-%d\n:END:\n"
                        n n))
              (number-sequence 1 2000) ""))
  (let ((start (float-time))
        job opened)
    (setq job (ogt-count-test--open '(someday stuck missed)))
    (setq opened (- (float-time) start))
    (assert-equal 'pending (org-gtd-review-counts--total
                            job (org-gtd-command-center--count-specs 'someday)))
    (assert-true (< opened 0.5))
    (ogt-count-test--drain job)
    (assert-equal 2000 (org-gtd-review-counts--total
                        job (org-gtd-command-center--count-specs 'someday)))))

(deftest review-counts/scheduled-steps-match-snapshot ()
  "Counts from timer steps equal the synchronous snapshot for every view."
  (ogt-count-test--source
   "* NEXT Ready\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n* TODO Missing date\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n* WAIT Old delegation\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* Maybe\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n")
  (let* ((keys '(someday upcoming completed stuck missed))
         (org-gtd-review-counts--step-seconds 0)
         (job (ogt-count-test--open keys)))
    (ogt-count-test--drain job)
    (dolist (key keys)
      (let ((specs (org-gtd-command-center--count-specs key)))
        (assert-equal (apply #'+ (mapcar #'cdr (org-gtd-review-counts--snapshot specs)))
                      (org-gtd-review-counts--total job specs))))))

(deftest review-counts/disabled-does-not-scan ()
  "The opt-out keeps the menu available without any file scans."
  (let ((org-gtd-review-show-counts nil)
        called)
    (cl-letf (((symbol-function 'org-gtd-review-counts--start)
               (lambda (&rest _) (error "Unexpected count")))
              ((symbol-function 'transient-setup)
               (lambda (&rest _) (setq called t))))
      (org-gtd-command-center--setup 'org-gtd-command-center '(someday)))
    (assert-true called)
    (assert-nil org-gtd-command-center--counts)))

(deftest review-counts/disabling-stops-existing-job ()
  "Reopening with counts disabled also cancels previously scheduled work."
  (let ((job (ogt-count-test--open '(someday))))
    (let ((org-gtd-review-show-counts nil))
      (ogt-count-test--open '(someday)))
    (assert-true (org-gtd-review-counts--job-cancelled job))
    (assert-nil (org-gtd-review-counts--job-timer job))
    (assert-nil org-gtd-command-center--counts)))

(deftest review-counts/errors-are-not-zero ()
  "A failed job warns once and leaves unknown counts, not false zeroes."
  (let (warnings)
    (cl-letf (((symbol-function 'org-gtd-review-counts--prepare)
               (lambda (&rest _) (error "Unreadable source")))
              ((symbol-function 'display-warning)
               (lambda (_type message &rest _) (push message warnings))))
      (let ((job (ogt-count-test--open '(someday))))
        (ogt-count-test--drain job)
        (assert-nil (org-gtd-review-counts--total
                     job (org-gtd-command-center--count-specs 'someday)))
        (assert-nil (org-gtd-review-counts--job-timer job))))
    (assert-equal 1 (length warnings))
    (assert-match "Unreadable source" (car warnings))))

(deftest review-counts/label-formats-zero-positive-pending-and-unavailable ()
  "Zero hides the count; positive, pending and unknown counts stay distinct."
  (let ((suffix (org-gtd-command-center--counted-suffix
                 :description "Review" :command 'ignore :count-key 'someday))
        (spec org-gtd-reflect-someday-maybe-view-spec)
        (org-gtd-command-center--counts (org-gtd-review-counts--start nil)))
    (assert-equal "Review (…)" (transient-format-description suffix))
    (setf (org-gtd-review-counts--job-results org-gtd-command-center--counts)
          (list (cons spec 0)))
    (assert-equal "Review" (transient-format-description suffix))
    (setf (org-gtd-review-counts--job-results org-gtd-command-center--counts)
          (list (cons spec 3)))
    (assert-equal "Review (3)" (transient-format-description suffix))
    (setf (org-gtd-review-counts--job-error org-gtd-command-center--counts)
          '(error "Broken"))
    (assert-equal "Review (?)" (transient-format-description suffix))
    (let ((org-gtd-review-show-counts nil))
      (assert-equal "Review" (transient-format-description suffix)))))

(deftest review-counts/unopened-files-do-not-run-user-hooks ()
  "A snapshot reads unopened sources without user hooks and releases its buffers."
  (with-temp-file "/mock:/gtd/org-gtd-tasks.org"
    (insert "* Maybe\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n"))
  (let ((org-mode-hook (list (lambda () (error "Ran Org mode hook"))))
        (find-file-hook (list (lambda () (error "Ran find-file hook")))))
    (assert-equal 1 (ogt-count-test--count org-gtd-reflect-someday-maybe-view-spec)))
  (assert-nil (org-find-base-buffer-visiting "/mock:/gtd/org-gtd-tasks.org")))

(deftest review-counts/error-cleans-up-and-restores-id-lookup ()
  "A failed scan releases its buffers and restores native ID refresh."
  (let ((lookup (symbol-function 'org-id-update-id-locations))
        (buffers (buffer-list))
        caught)
    (cl-letf (((symbol-function 'org-gtd-review-counts--count-file)
               (lambda (&rest _) (error "Broken skip function"))))
      (condition-case err
          (ogt-count-test--count org-gtd-reflect-someday-maybe-view-spec)
        (error (setq caught (error-message-string err)))))
    (assert-equal "Broken skip function" caught)
    (assert-true (eq lookup (symbol-function 'org-id-update-id-locations)))
    (assert-equal (sort (mapcar #'buffer-name buffers) #'string<)
                  (sort (mapcar #'buffer-name (buffer-list)) #'string<))))

(deftest review-counts/inactive-and-unresolved-projects ()
  "Native predicates exclude inactive projects and keep unresolved references."
  (ogt-count-test--source
   "* CNCL Canceled project\n:PROPERTIES:\n:ID: canceled-project\n:ORG_GTD: Projects\n:END:\n* TODO Inactive task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: canceled-project\n:END:\n* TODO Unresolved task\n:PROPERTIES:\n:ORG_GTD: Actions\n:ORG_GTD_PROJECT_IDS: unknown-project\n:END:\n")
  (assert-equal 1 (ogt-count-test--count
                   (org-gtd-reflect--stuck-view-spec 'stuck-next-action)))
  (assert-equal 0 (ogt-count-test--count
                   (org-gtd-reflect--stuck-view-spec 'stuck-project)))
  (assert-equal 1 (ogt-count-test--count org-gtd-reflect-completed-projects-view-spec)))

(deftest review-counts/habits-use-scheduled ()
  "A habit's scheduled timestamp, not ORG_GTD_TIMESTAMP, determines stuckness."
  (ogt-count-test--source
   "* TODO Valid\nSCHEDULED: <2099-01-01 Thu +1d>\n:PROPERTIES:\n:ORG_GTD: Habit\n:END:\n* TODO Wrong property\n:PROPERTIES:\n:ORG_GTD: Habit\n:ORG_GTD_TIMESTAMP: <2099-01-01 Thu +1d>\n:END:\n")
  (let ((spec (org-gtd-reflect--stuck-view-spec 'stuck-habit)))
    (assert-equal 1 (ogt-count-test--count spec))
    (assert-equal 1 (ogt-count-test--view-count spec))))

(deftest review-counts/delegated-day-boundaries ()
  "Future means after today's midnight, as in the underlying delegated view."
  (ogt-count-test--source
   (mapconcat
    (lambda (timestamp)
      (format "* WAIT Follow up\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: %s\n:END:\n"
              timestamp))
    (list (format-time-string "<%Y-%m-%d %a>"
                              (time-subtract (current-time) (days-to-time 1)))
          (format-time-string "<%Y-%m-%d %a>")
          (format-time-string "<%Y-%m-%d %a 12:00>")
          (format-time-string "<%Y-%m-%d %a>"
                              (time-add (current-time) (days-to-time 1))))
    ""))
  (assert-equal 1 (ogt-count-test--count
                   (car org-gtd-reflect-missed-engagements-view-specs)))
  (assert-equal 2 (ogt-count-test--count org-gtd-reflect-upcoming-delegated-view-spec))
  (assert-equal 2 (ogt-count-test--view-count org-gtd-reflect-upcoming-delegated-view-spec)))

(deftest review-counts/native-sublevel-option ()
  "Native sublevel suppression applies to counts as well as review lists."
  (ogt-count-test--source
   "* Parent\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n** Child\n:PROPERTIES:\n:ORG_GTD: Someday\n:END:\n")
  (dolist (sublevels '(nil t))
    (let ((org-tags-match-list-sublevels sublevels)
          (spec org-gtd-reflect-someday-maybe-view-spec))
      (assert-equal (if sublevels 2 1) (ogt-count-test--count spec))
      (assert-equal (ogt-count-test--view-count spec)
                    (ogt-count-test--count spec)))))

(deftest review-counts/native-todo-ignore-options ()
  "The native tags-todo scanner honors configured scheduling exclusions."
  (ogt-count-test--source
   "* WAIT Included\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n* WAIT Scheduled\nSCHEDULED: <2099-01-01 Thu>\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2000-01-01 Sat>\n:END:\n")
  (let ((org-agenda-tags-todo-honor-ignore-options t)
        (org-agenda-todo-ignore-scheduled 'future)
        (spec (car org-gtd-reflect-missed-engagements-view-specs)))
    (assert-equal 1 (ogt-count-test--count spec))
    (assert-equal 1 (ogt-count-test--view-count spec))))

;;; review-counts-test.el ends here
