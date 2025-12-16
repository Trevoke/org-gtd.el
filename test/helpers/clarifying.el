;; Load guard to prevent redundant loading
(unless (featurep 'org-gtd-test-helper-clarifying)

(defun organize-as-single-action ()
  "Organize the item at point as a single-action task.

In GTD, single actions are standalone tasks that can be completed
independently without dependencies on other tasks. They appear in your
next actions list and can be worked on whenever you have time and the
appropriate context."
  (let ((inhibit-message t))
    (org-gtd-single-action)))

(defun organize-as-quick-action ()
  "Organize the item at point as a quick action.

Quick actions are tasks that take less than 2 minutes to complete.
In GTD methodology, these should be done immediately rather than
tracked in your system. This function is used in tests to verify
that quick actions are properly categorized."
  (let ((inhibit-message t))
    (org-gtd-quick-action)))

(defun organize-as-project ()
  "Organize the item at point as a multi-step project.

In GTD, a project is any outcome requiring more than one action step.
Projects have tasks with dependencies, and the system tracks which
tasks can be worked on next based on the dependency graph."
  (let ((inhibit-message t))
    (org-gtd-project-new)))

(defun defer-item (&optional date)
  "Defer the item at point for later review.

In GTD, tickler items are things you want to be reminded about
at a specific future date. They're reviewed periodically and surface
when their reminder date arrives.

DATE is optional and specifies when to review this item. If provided,
it should be in calendar-current-date format (MM DD YYYY). If omitted,
the item is deferred without a specific review date."
  (let ((inhibit-message t)
        (reminder-date (when date
                         (let* ((year (nth 2 date))
                                (month (nth 0 date))
                                (day (nth 1 date)))
                           (format "%04d-%02d-%02d" year month day)))))
    (org-gtd-tickler reminder-date)))

(defun delegate-item (&optional to-whom date)
  "Delegate the item at point to someone else.

In GTD, delegated items are tasks you're waiting for others to complete.
You need to track who you delegated to and when to follow up, ensuring
nothing falls through the cracks.

TO-WHOM is the person you're delegating to. Defaults to 'Someone' if
not provided.

DATE is the check-in date when you'll follow up on the delegated task.
Should be in calendar-current-date format (MM DD YYYY). If omitted,
defaults to no specific follow-up date."
  (let ((inhibit-message t)
        (person (or to-whom "Someone"))
        (checkin-date (when date
                        (let* ((year (nth 2 date))
                               (month (nth 0 date))
                               (day (nth 1 date)))
                          (format "%04d-%02d-%02d" year month day)))))
    (org-gtd-delegate person checkin-date)))

(defun schedule-item (&optional date)
  "Schedule the item at point for a specific date and time.

In GTD, calendar items are things that must happen at a specific time
or on a specific date. Unlike next actions which you do when you can,
calendar items are time-sensitive commitments.

DATE is when this item should happen. Should be in calendar-current-date
format (MM DD YYYY). If omitted, uses current date."
  (let ((inhibit-message t)
        (appointment-date (when date
                            (let* ((year (nth 2 date))
                                   (month (nth 0 date))
                                   (day (nth 1 date)))
                              (format "%04d-%02d-%02d" year month day)))))
    (org-gtd-calendar appointment-date)))

(defun organize-as-habit (repeater)
  "Organize the item at point as a recurring habit with REPEATER pattern.

In GTD, habits are recurring tasks that you want to do regularly.
They use org-mode's habit tracking to show consistency and streaks.

REPEATER is an org-mode date repeater pattern like '.+1d' for daily,
'++1w' for weekly, or '.+1m' for monthly. The prefix determines the
behavior:
  .+ means restart from completion date
  ++ means restart from scheduled date regardless of completion
  +  means restart from scheduled date if late, otherwise from completion"
  (let ((inhibit-message t))
    (org-gtd-habit repeater)))

(defun archive-as-reference ()
  "Archive the item at point as reference material.

In GTD, reference material is information you might need later but
doesn't require action. It's stored in a searchable system separate
from your action lists to keep them focused on what needs doing."
  (let ((inhibit-message t))
    (org-gtd-knowledge)))

(defun discard-item ()
  "Discard the item at point by moving it to trash.

During GTD processing, some inbox items turn out to be not actionable
and not worth keeping. This function moves them to trash rather than
cluttering your system with items that don't matter."
  (let ((inhibit-message t))
    (org-gtd-trash)))

(defun organize-as-someday ()
  "Organize the item at point as someday/maybe.

In GTD, someday/maybe items are things you might want to do eventually,
but with no specific timeframe. Unlike tickler items which have a review
date, someday items have no timestamps and are simply stored for
periodic review."
  (let ((inhibit-message t))
    (org-gtd-someday)))

;; End load guard and provide feature
(provide 'org-gtd-test-helper-clarifying))
