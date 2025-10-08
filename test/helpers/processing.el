;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)


;; Load guard to prevent redundant loading
(unless (featurep 'org-gtd-test-helper-processing)

;; Load dependencies
(require 'org-gtd-test-helper-clarifying (file-name-concat default-directory "test/helpers/clarifying.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))

(defun capture-inbox-item (&optional description)
  "Capture an item to the GTD inbox.

DESCRIPTION is the text of the inbox item. If omitted, defaults to
'single action'. This simulates what happens when a user captures
something during their day - it goes into the inbox for later
processing."
  (let ((inhibit-message t))
    (org-gtd-capture nil "i")
    (insert (or description "single action"))
    (org-capture-finalize)))

(defun create-project (label)
  "Create a new multi-step project through the full GTD workflow.

LABEL is the project name/description.

This function simulates the complete user workflow:
1. Capture the project idea to inbox
2. Process the inbox item
3. Add project tasks
4. Organize as a project

In GTD, projects are outcomes requiring multiple action steps. The
system will create a dependency graph to track which tasks can be
worked on next."
  (let ((inhibit-message t))
    (capture-inbox-item label)
    (org-gtd-process-inbox)
    (goto-char (point-max))
    (newline)
    ;; Create three simple tasks using builder
    (make-task "Task 1" :level 2)
    (make-task "Task 2" :level 2)
    (make-task "Task 3" :level 2)
    (organize-as-project)))

(defun create-calendar-item (description &optional date)
  "Create a calendar item through the full GTD workflow.

DESCRIPTION is what the calendar item is about.

DATE is when this item should happen. Should be in calendar-current-date
format (MM DD YYYY). If omitted, defaults to today.

In GTD, calendar items are time-specific commitments that must happen
on a particular date, unlike next actions which can be done whenever
you have time."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)
    (schedule-item (or date (calendar-current-date)))))

(defun create-habit (description repeater)
  "Create a recurring habit through the full GTD workflow.

DESCRIPTION is what the habit is about.

REPEATER is an org-mode date repeater pattern like '.+1d' for daily,
'++1w' for weekly, or '.+1m' for monthly.

In GTD, habits are recurring tasks you want to do regularly. The system
tracks your consistency and shows streaks to help build the habit."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)
    (organize-as-habit repeater)))

(defun create-delegated-item (description &optional to-whom date)
  "Create a delegated task through the full GTD workflow.

DESCRIPTION is what you're delegating.

TO-WHOM is the person you're delegating to. Defaults to 'Someone' if
not provided.

DATE is the check-in date when you'll follow up. Should be in
calendar-current-date format (MM DD YYYY). Defaults to today if omitted.

In GTD, delegated tasks go on your 'Waiting For' list with a follow-up
date to ensure they don't fall through the cracks."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)
    (delegate-item (or to-whom "Someone") (or date (calendar-current-date)))))

(defun create-deferred-item (description &optional date)
  "Create a deferred item (someday/maybe) through the full GTD workflow.

DESCRIPTION is what the item is about.

DATE is when to review this item. Should be in calendar-current-date
format (MM DD YYYY). Defaults to today if omitted.

In GTD, deferred items are things you might want to do later but not
right now. They're reviewed periodically during your weekly review to
see if they should become active."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)
    (defer-item (or date (calendar-current-date)))))

(defun create-single-action (description)
  "Create a single-action task through the full GTD workflow.

DESCRIPTION is what the task is about.

In GTD, single actions are standalone tasks that can be completed
independently. They appear in your next actions list and can be worked
on whenever you have time and the appropriate context."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)
    (organize-as-single-action)))

(defun create-quick-action (description)
  "Create a quick action through the full GTD workflow.

DESCRIPTION is what the task is about.

Quick actions are tasks that take less than 2 minutes. In GTD, these
should be done immediately, but this function is used in tests to verify
the categorization works correctly."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)
    (organize-as-quick-action)))

(defun create-reference-item (description)
  "Create a reference/knowledge item through the full GTD workflow.

DESCRIPTION is what the reference material is about.

In GTD, reference material is information you might need later but
doesn't require action. It's stored separately from action lists to
keep them focused on what needs doing."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)
    (archive-as-reference)))

(defun add-task-to-existing-project (description project-heading-simulated-input)
  "Add a task to an existing project through the GTD workflow.

DESCRIPTION is the task description.

PROJECT-HEADING-SIMULATED-INPUT is the simulated user input to select
which project to add the task to.

This simulates what happens when processing an inbox item that you
realize is actually part of an existing project - you add it as a new
task in that project rather than creating a new standalone task."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)

    (with-simulated-input project-heading-simulated-input
                          (org-gtd-organize--call
                           org-gtd-add-to-project-func))))

(defun discard-inbox-item (description)
  "Discard an inbox item through the GTD workflow.

DESCRIPTION is what the item is about.

During GTD processing, some inbox items turn out to be not actionable
and not worth keeping. This function moves them to trash rather than
cluttering your system."
  (let ((inhibit-message t))
    (capture-inbox-item description)
    (org-gtd-process-inbox)
    (discard-item)))

;; End load guard and provide feature
(provide 'org-gtd-test-helper-processing))
