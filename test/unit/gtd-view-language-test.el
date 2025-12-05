;;; gtd-view-language-test.el --- Tests for GTD view language -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Pure unit tests for org-gtd view language translation functions.
;; These tests verify the translation from GTD view specs to org-ql queries.
;;
;; Migrated from test/gtd-view-language-test.el (buttercup).
;; Integration tests that require file system are in a separate file.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd)
(require 'org-gtd-view-language)

;;; GTD View Language Specification

(deftest view-lang/simple-delegated-past ()
  "Translates delegated type with past timestamp."
  (let ((gtd-view-spec
         '((name . "Missed Delegated Check-ins")
           (type . delegated)
           (when . past))))
    (assert-equal
     `(and (property "ORG_GTD" "Delegated")
           (todo ,(org-gtd-keywords--wait))
           (property-ts< "ORG_GTD_TIMESTAMP" "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/calendar-past-with-level ()
  "Translates calendar type with level and past timestamp."
  (let ((gtd-view-spec
         '((name . "Missed Appointments")
           (type . calendar)
           (level . 2)
           (when . past))))
    (assert-equal
     '(and (property "ORG_GTD" "Calendar")
           (level 2)
           (property-ts< "ORG_GTD_TIMESTAMP" "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/simple-next-action-type ()
  "Translates next-action type filter."
  (let ((gtd-view-spec
         '((name . "All Next Actions")
           (type . next-action))))
    (assert-equal
     `(and (property "ORG_GTD" "Actions")
           (todo ,(org-gtd-keywords--next)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/when-today-filter ()
  "Translates when=today filter."
  (let ((gtd-view-spec
         '((name . "Today Items")
           (type . calendar)
           (when . today))))
    (assert-equal
     `(and (property "ORG_GTD" "Calendar")
           (property-ts= "ORG_GTD_TIMESTAMP" ,(format-time-string "%Y-%m-%d")))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/overdue-deadlines ()
  "Translates overdue deadlines filter."
  (let ((gtd-view-spec
         '((name . "Overdue Deadlines")
           (filters . ((deadline . past))))))
    (assert-equal
     '(and (deadline :to "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/overdue-scheduled-not-habit ()
  "Translates overdue scheduled excluding habits."
  (let ((gtd-view-spec
         '((name . "Overdue Scheduled Items")
           (filters . ((scheduled . past)
                       (not-habit . t))))))
    (assert-equal
     '(and (scheduled :to "today")
           (not (property "STYLE" "habit"))
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/scheduled-today-filter ()
  "Translates scheduled=today filter."
  (let ((gtd-view-spec
         '((name . "Scheduled Today")
           (filters . ((scheduled . today))))))
    (assert-equal
     '(and (scheduled :on "today"))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/not-done-filter ()
  "Translates not-done=t filter."
  (let ((gtd-view-spec
         '((name . "Incomplete Items")
           (filters . ((not-done . t))))))
    (assert-equal
     '(and (not (done)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

;;; Complex Scenarios

(deftest view-lang/multiple-time-type-filters ()
  "Combines multiple time-based and type filters."
  (let ((gtd-view-spec
         '((name . "Complex View")
           (filters . ((type . project)
                       (level . 2)
                       (deadline . past)
                       (scheduled . future))))))
    (assert-equal
     '(and (property "ORG_GTD" "Projects")
           (level 2)
           (deadline :to "today")
           (scheduled :from "today"))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/area-of-focus-filtering ()
  "Translates area-of-focus with todo filtering."
  (let ((gtd-view-spec
         '((name . "Area Focus View")
           (filters . ((area-of-focus . "Work")
                       (todo . ("TODO" "NEXT")))))))
    (assert-equal
     '(and (property "CATEGORY" "Work")
           (todo "TODO" "NEXT"))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

;;; Error Handling

(deftest view-lang/rejects-invalid-filters ()
  "Rejects invalid filter specifications."
  (let ((invalid-gtd-view-spec
         '((name . "Invalid View")
           (filters . ((invalid-filter . "bad-value"))))))
    (assert-raises 'error
      (org-gtd-view-lang--translate-to-org-ql invalid-gtd-view-spec))))

;;; Integration with Existing System

(deftest view-lang/delegated-oops-equivalent ()
  "Generates equivalent query for existing oops view patterns."
  (let ((delegated-oops-spec
         '((name . "Missed check-ins on delegated items")
           (type . delegated)
           (when . past))))
    (assert-equal
     `(and (property "ORG_GTD" "Delegated")
           (todo ,(org-gtd-keywords--wait))
           (property-ts< "ORG_GTD_TIMESTAMP" "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql delegated-oops-spec))))

;;; Tickler Type Filters

(deftest view-lang/tickler-projects ()
  "Translates tickler-project type."
  (let ((tickler-projects-spec
         '((name . "Tickler Projects")
           (type . tickler-project))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Tickler")
                (property "PREVIOUS_ORG_GTD" "Projects")))
     (org-gtd-view-lang--translate-to-org-ql tickler-projects-spec))))

(deftest view-lang/all-tickler ()
  "Translates all tickler items."
  (let ((tickler-spec
         '((name . "All Tickler")
           (type . tickler))))
    (assert-equal
     '(and (property "ORG_GTD" "Tickler"))
     (org-gtd-view-lang--translate-to-org-ql tickler-spec))))

;;; Agenda-Specific View Features

(deftest view-lang/agenda-block-creation ()
  "Creates agenda block from view-type agenda."
  (let ((agenda-view-spec
         '((name . "Simple Agenda View")
           (view-type . agenda))))
    (assert-equal
     '(agenda ""
              ((org-agenda-span 1)
               (org-agenda-start-day nil)
               (org-agenda-skip-additional-timestamps-same-entry t)
               (org-agenda-skip-function 'org-gtd-skip-unless-in-progress)))
     (org-gtd-view-lang--create-agenda-block agenda-view-spec))))

(deftest view-lang/daily-agenda-with-next ()
  "Creates daily agenda view with span and NEXT actions."
  (let ((engage-view-spec
         '((name . "Today's GTD Engage View")
           (view-type . agenda)
           (agenda-span . 1)
           (show-habits . nil)
           (additional-blocks . ((todo . "NEXT"))))))
    (assert-equal
     '(agenda ""
              ((org-agenda-include-all-todo nil)
               (org-agenda-span 1)
               (org-agenda-start-day nil)
               (org-agenda-skip-additional-timestamps-same-entry t)
               (org-agenda-skip-function 'org-gtd-skip-unless-in-progress)))
     (org-gtd-view-lang--create-agenda-block engage-view-spec))
    (assert-equal
     '((todo "NEXT"
             ((org-agenda-overriding-header "All actions ready to be executed."))))
     (org-gtd-view-lang--create-additional-blocks engage-view-spec))))

;;; Tag Filtering Features

(deftest view-lang/tags-filter ()
  "Filters items by specific tags."
  (let ((tag-view-spec
         '((name . "Context View")
           (filters . ((tags . ("@work" "@computer"))
                       (todo . ("NEXT")))))))
    (assert-equal
     '(and (tags "@work" "@computer")
           (todo "NEXT"))
     (org-gtd-view-lang--translate-to-org-ql tag-view-spec))))

(deftest view-lang/tags-match-pattern ()
  "Filters items by tag patterns."
  (let ((tag-match-spec
         '((name . "Context Pattern View")
           (filters . ((tags-match . "{^@}")
                       (todo . ("NEXT")))))))
    (assert-equal
     '(and (tags "{^@}")
           (todo "NEXT"))
     (org-gtd-view-lang--translate-to-org-ql tag-match-spec))))

(deftest view-lang/grouped-views-by-context ()
  "Creates simple grouped views by pre-defined contexts."
  (let ((grouped-spec
         '((name . "Actions by Context")
           (view-type . tags-grouped)
           (group-contexts . ("@work" "@home"))
           (filters . ((todo . ("NEXT")))))))
    (assert-equal
     '((tags "+@work+TODO=\"NEXT\""
             ((org-agenda-overriding-header "@work")))
       (tags "+@home+TODO=\"NEXT\""
             ((org-agenda-overriding-header "@home"))))
     (org-gtd-view-lang--create-grouped-views grouped-spec))))

;;; Active Projects View

(deftest view-lang/active-projects-filter ()
  "Translates active-project type."
  (let ((active-projects-spec
         '((name . "Active Projects")
           (type . active-project))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Projects")
                (project-has-active-tasks)))
     (org-gtd-view-lang--translate-to-org-ql active-projects-spec))))

;;; Completion and Closed Item Views

(deftest view-lang/done-items ()
  "Translates done items view."
  (let ((done-spec
         '((name . "Completed Items")
           (filters . ((done . t))))))
    (assert-equal
     '(and (done))
     (org-gtd-view-lang--translate-to-org-ql done-spec))))

(deftest view-lang/recently-done ()
  "Translates recently done items using done filter."
  (let ((done-spec
         '((name . "Recently Completed")
           (done . recent))))
    (assert-equal
     '(and (closed :from -7))
     (org-gtd-view-lang--translate-to-org-ql done-spec))))

(deftest view-lang/done-on-today ()
  "Translates items done today using done filter."
  (let ((done-today-spec
         '((name . "Completed Today")
           (done . today))))
    (assert-equal
     '(and (closed :on "today"))
     (org-gtd-view-lang--translate-to-org-ql done-today-spec))))

(deftest view-lang/completed-projects ()
  "Translates completed-project type."
  (let ((completed-projects-spec
         '((name . "Completed Projects")
           (type . completed-project))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Projects")
                (not (project-has-active-tasks))))
     (org-gtd-view-lang--translate-to-org-ql completed-projects-spec))))

(deftest view-lang/done-with-time-spec ()
  "Done filter with time spec uses closed predicate."
  (let ((recent-completed-spec
         '((name . "Recently Completed Items")
           (done . past-week))))
    (assert-equal
     '(and (closed :from -7))
     (org-gtd-view-lang--translate-to-org-ql recent-completed-spec))))

;;; Project Type Filters Without Level Constraints

(deftest view-lang/stuck-project-type ()
  "Translates stuck-project type without level constraint."
  (let ((gtd-view-spec
         '((name . "Stuck Projects")
           (filters . ((type . stuck-project))))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Projects")
                (project-is-stuck)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/completed-project-type ()
  "Translates completed-project type without level constraint."
  (let ((gtd-view-spec
         '((name . "Completed Projects")
           (filters . ((type . completed-project))))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Projects")
                (not (project-has-active-tasks))))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

;;; Flat Filter Structure Support

(deftest view-lang/flat-stuck-project ()
  "Supports flat filter structure without filters wrapper."
  (let ((gtd-view-spec
         '((name . "Stuck Projects")
           (type . stuck-project))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Projects")
                (project-is-stuck)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/flat-multiple-filters ()
  "Supports flat filter structure with multiple filters."
  (let ((gtd-view-spec
         '((name . "Next Actions in Work")
           (type . next-action)
           (area-of-focus . "Work"))))
    (assert-equal
     `(and (property "ORG_GTD" "Actions")
           (todo ,(org-gtd-keywords--next))
           (property "CATEGORY" "Work"))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/legacy-filters-wrapper ()
  "Still supports legacy filters wrapper format."
  (let ((gtd-view-spec
         '((name . "Stuck Projects")
           (filters . ((type . stuck-project))))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Projects")
                (project-is-stuck)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

;;; Stuck Item Detection via Computed Types

(deftest view-lang/stuck-calendar-type ()
  "Translates stuck-calendar computed type."
  (let ((stuck-calendar-spec
         '((name . "Stuck Calendar Items")
           (type . stuck-calendar))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Calendar")
                (property-invalid-timestamp "ORG_GTD_TIMESTAMP")))
     (org-gtd-view-lang--translate-to-org-ql stuck-calendar-spec))))

(deftest view-lang/stuck-delegated-type ()
  "Translates stuck-delegated computed type."
  (let ((stuck-delegated-spec
         '((name . "Stuck Delegated Items")
           (type . stuck-delegated))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Delegated")
                (or (property-empty-or-missing "DELEGATED_TO")
                    (property-invalid-timestamp "ORG_GTD_TIMESTAMP"))))
     (org-gtd-view-lang--translate-to-org-ql stuck-delegated-spec))))

(deftest view-lang/stuck-tickler-type ()
  "Translates stuck-tickler computed type."
  (let ((stuck-tickler-spec
         '((name . "Stuck Tickler Items")
           (type . stuck-tickler))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Tickler")
                (property-invalid-timestamp "ORG_GTD_TIMESTAMP")))
     (org-gtd-view-lang--translate-to-org-ql stuck-tickler-spec))))

(deftest view-lang/stuck-habit-type ()
  "Translates stuck-habit computed type."
  (let ((stuck-habit-spec
         '((name . "Stuck Habit Items")
           (type . stuck-habit))))
    (assert-equal
     '(and (and (property "ORG_GTD" "Habit")
                (property-invalid-timestamp "SCHEDULED")))
     (org-gtd-view-lang--translate-to-org-ql stuck-habit-spec))))

;;; Who Filter Tests

(deftest view-lang/who-specific-person ()
  "Translates who filter for specific person."
  (let ((gtd-view-spec
         '((name . "Delegated to Alice")
           (type . delegated)
           (who . "Alice"))))
    (assert-equal
     `(and (property "ORG_GTD" "Delegated")
           (todo ,(org-gtd-keywords--wait))
           (property "DELEGATED_TO" "Alice"))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/who-nil-missing ()
  "Translates who=nil to find missing delegatee."
  (let ((gtd-view-spec
         '((name . "Missing Delegatee")
           (type . delegated)
           (who . nil))))
    (assert-equal
     `(and (property "ORG_GTD" "Delegated")
           (todo ,(org-gtd-keywords--wait))
           (property-empty-or-missing "DELEGATED_TO"))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/who-requires-type ()
  "Signals error when who filter is used without type."
  (let ((gtd-view-spec
         '((name . "Invalid Who")
           (who . "Alice"))))
    (assert-raises 'user-error
      (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

;;; Type-Based Filters Using org-gtd-types

(deftest view-lang/type-next-action ()
  "Translates type=next-action to include NEXT keyword."
  (let ((view-spec
         '((name . "Next Actions")
           (filters . ((type . next-action))))))
    (assert-equal
     `(and (property "ORG_GTD" "Actions")
           (todo ,(org-gtd-keywords--next)))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-delegated ()
  "Translates type=delegated to include WAIT keyword."
  (let ((view-spec
         '((name . "Delegated Items")
           (filters . ((type . delegated))))))
    (assert-equal
     `(and (property "ORG_GTD" "Delegated")
           (todo ,(org-gtd-keywords--wait)))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-calendar ()
  "Translates type=calendar."
  (let ((view-spec
         '((name . "Calendar Items")
           (filters . ((type . calendar))))))
    (assert-equal
     '(and (property "ORG_GTD" "Calendar"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-tickler ()
  "Translates type=tickler."
  (let ((view-spec
         '((name . "Tickler Items")
           (filters . ((type . tickler))))))
    (assert-equal
     '(and (property "ORG_GTD" "Tickler"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-project ()
  "Translates type=project."
  (let ((view-spec
         '((name . "Projects")
           (filters . ((type . project))))))
    (assert-equal
     '(and (property "ORG_GTD" "Projects"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-reference ()
  "Translates type=reference."
  (let ((view-spec
         '((name . "Reference Items")
           (filters . ((type . reference))))))
    (assert-equal
     '(and (property "ORG_GTD" "Reference"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-trash ()
  "Translates type=trash."
  (let ((view-spec
         '((name . "Trash")
           (filters . ((type . trash))))))
    (assert-equal
     '(and (property "ORG_GTD" "Trash"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-quick-action ()
  "Translates type=quick-action."
  (let ((view-spec
         '((name . "Quick Actions")
           (filters . ((type . quick-action))))))
    (assert-equal
     '(and (property "ORG_GTD" "Quick"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-habit ()
  "Translates type=habit."
  (let ((view-spec
         '((name . "Habits")
           (filters . ((type . habit))))))
    (assert-equal
     '(and (property "ORG_GTD" "Habit"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-someday ()
  "Translates type=someday."
  (let ((view-spec
         '((name . "Someday Items")
           (filters . ((type . someday))))))
    (assert-equal
     '(and (property "ORG_GTD" "Someday"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/type-unknown-errors ()
  "Signals error for unknown type."
  (let ((view-spec
         '((name . "Unknown")
           (filters . ((type . nonexistent-type))))))
    (assert-raises 'user-error
      (org-gtd-view-lang--translate-to-org-ql view-spec))))

;;; Semantic :when Property Filter

(deftest view-lang/when-past-delegated ()
  "Translates :when=past with delegated type."
  (let ((view-spec
         '((name . "Missed Delegated")
           (filters . ((type . delegated)
                       (:when . past))))))
    (assert-equal
     `(and (property "ORG_GTD" "Delegated")
           (todo ,(org-gtd-keywords--wait))
           (property-ts< "ORG_GTD_TIMESTAMP" "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/when-past-calendar ()
  "Translates :when=past with calendar type."
  (let ((view-spec
         '((name . "Missed Calendar")
           (filters . ((type . calendar)
                       (:when . past))))))
    (assert-equal
     '(and (property "ORG_GTD" "Calendar")
           (property-ts< "ORG_GTD_TIMESTAMP" "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/when-past-tickler ()
  "Translates :when=past with tickler type."
  (let ((view-spec
         '((name . "Past Tickler")
           (filters . ((type . tickler)
                       (:when . past))))))
    (assert-equal
     '(and (property "ORG_GTD" "Tickler")
           (property-ts< "ORG_GTD_TIMESTAMP" "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/when-past-habit ()
  "Translates :when=past with habit type to SCHEDULED."
  (let ((view-spec
         '((name . "Past Habits")
           (filters . ((type . habit)
                       (:when . past))))))
    (assert-equal
     '(and (property "ORG_GTD" "Habit")
           (property-ts< "SCHEDULED" "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/when-future-calendar ()
  "Translates :when=future with calendar type."
  (let ((view-spec
         '((name . "Future Calendar")
           (filters . ((type . calendar)
                       (:when . future))))))
    (assert-equal
     '(and (property "ORG_GTD" "Calendar")
           (property-ts> "ORG_GTD_TIMESTAMP" "today"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

;;; When Filter Using Semantic Property Lookup

(deftest view-lang/when-today-semantic ()
  "Translates when=today using semantic property lookup."
  (let ((gtd-view-spec
         '((name . "Delegated Due Today")
           (type . delegated)
           (when . today))))
    (assert-equal
     `(and (property "ORG_GTD" "Delegated")
           (todo ,(org-gtd-keywords--wait))
           (property-ts= "ORG_GTD_TIMESTAMP" ,(format-time-string "%Y-%m-%d")))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/when-past-semantic ()
  "Translates when=past with delegated type semantic."
  (let ((gtd-view-spec
         '((name . "Missed Delegated")
           (type . delegated)
           (when . past))))
    (assert-equal
     `(and (property "ORG_GTD" "Delegated")
           (todo ,(org-gtd-keywords--wait))
           (property-ts< "ORG_GTD_TIMESTAMP" "today")
           (not (done)))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/when-future-tickler ()
  "Translates when=future with tickler type."
  (let ((gtd-view-spec
         '((name . "Future Tickler")
           (type . tickler)
           (when . future))))
    (assert-equal
     '(and (property "ORG_GTD" "Tickler")
           (property-ts> "ORG_GTD_TIMESTAMP" "today"))
     (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/when-requires-type ()
  "Requires type filter to be present for when."
  (let ((gtd-view-spec
         '((name . "Invalid When")
           (when . today))))
    (assert-raises 'user-error
      (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

(deftest view-lang/when-type-must-support ()
  "Errors when type doesn't have :when property."
  (let ((gtd-view-spec
         '((name . "Invalid Type")
           (type . next-action)
           (when . today))))
    (assert-raises 'user-error
      (org-gtd-view-lang--translate-to-org-ql gtd-view-spec))))

;;; Previous-Type Filter for Tickler Items

(deftest view-lang/previous-type-delegated ()
  "Translates previous-type=delegated."
  (let ((view-spec
         '((name . "Tickler Delegated")
           (filters . ((type . tickler)
                       (previous-type . delegated))))))
    (assert-equal
     '(and (property "ORG_GTD" "Tickler")
           (property "PREVIOUS_ORG_GTD" "Delegated"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/previous-type-next-action ()
  "Translates previous-type=next-action."
  (let ((view-spec
         '((name . "Tickler Actions")
           (filters . ((type . tickler)
                       (previous-type . next-action))))))
    (assert-equal
     '(and (property "ORG_GTD" "Tickler")
           (property "PREVIOUS_ORG_GTD" "Actions"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/previous-type-project ()
  "Translates previous-type=project."
  (let ((view-spec
         '((name . "Tickler Projects")
           (filters . ((type . tickler)
                       (previous-type . project))))))
    (assert-equal
     '(and (property "ORG_GTD" "Tickler")
           (property "PREVIOUS_ORG_GTD" "Projects"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

;;; Multi-Block View Support

(deftest view-lang/multi-block-creates-blocks ()
  "Creates agenda blocks from multi-block view spec."
  (let ((multi-block-spec
         '((name . "Multi View")
           (blocks . (((name . "Block 1")
                       (type . next-action))
                      ((name . "Block 2")
                       (type . delegated)))))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list multi-block-spec))))
      (assert-equal 2 (length (caddr (car commands)))))))

(deftest view-lang/multi-block-separate-processing ()
  "Processes each block separately in multi-block spec."
  (let ((multi-block-spec
         '((name . "Multi View")
           (blocks . (((name . "Next Actions Block")
                       (type . next-action))
                      ((name . "Delegated Block")
                       (type . delegated)))))))
    (let* ((commands (org-gtd-view-lang--create-custom-commands (list multi-block-spec)))
           (blocks (caddr (car commands))))
      (assert-equal '((org-ql-block-header "Next Actions Block"))
                    (caddr (car blocks)))
      (assert-equal '((org-ql-block-header "Delegated Block"))
                    (caddr (cadr blocks))))))

(deftest view-lang/single-block-without-blocks-key ()
  "Still supports single-block specs without blocks key."
  (let ((single-block-spec
         '((name . "Single View")
           (type . next-action))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list single-block-spec))))
      (assert-equal 1 (length (caddr (car commands)))))))

;;; Calendar-Day Block Type

(deftest view-lang/calendar-day-creates-agenda-block ()
  "Creates a native agenda block with calendar-day block-type."
  (let ((calendar-day-spec
         '((name . "Today's Calendar")
           (block-type . calendar-day))))
    (let ((block (org-gtd-view-lang--create-agenda-block calendar-day-spec)))
      (assert-equal 'agenda (car block))
      (let ((settings (caddr block)))
        (assert-true (assoc 'org-agenda-skip-function settings))))))

(deftest view-lang/calendar-day-skip-function ()
  "Includes skip function for Calendar and Habit items."
  (let ((calendar-day-spec
         '((name . "Today's Calendar")
           (block-type . calendar-day))))
    (let* ((block (org-gtd-view-lang--create-agenda-block calendar-day-spec))
           (settings (caddr block))
           (skip-fn (cadr (assoc 'org-agenda-skip-function settings))))
      (assert-equal ''org-gtd-view-lang--skip-unless-calendar-or-habit skip-fn))))

(deftest view-lang/calendar-day-time-grid ()
  "Includes time grid settings in calendar-day block."
  (let ((calendar-day-spec
         '((name . "Today's Calendar")
           (block-type . calendar-day))))
    (let* ((block (org-gtd-view-lang--create-agenda-block calendar-day-spec))
           (settings (caddr block)))
      (assert-equal 1 (cadr (assoc 'org-agenda-span settings))))))

;;; Extended Done Filter with Time Specs

(deftest view-lang/done-recent ()
  "Translates done=recent to closed in last 7 days."
  (let ((view-spec
         '((name . "Recently Completed")
           (done . recent))))
    (assert-equal
     '(and (closed :from -7))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/done-today ()
  "Translates done=today to closed today."
  (let ((view-spec
         '((name . "Completed Today")
           (done . today))))
    (assert-equal
     '(and (closed :on "today"))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/done-past-day ()
  "Translates done=past-day to closed in last day."
  (let ((view-spec
         '((name . "Completed Yesterday")
           (done . past-day))))
    (assert-equal
     '(and (closed :from -1))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/done-past-week ()
  "Translates done=past-week to closed in last week."
  (let ((view-spec
         '((name . "Completed This Week")
           (done . past-week))))
    (assert-equal
     '(and (closed :from -7))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/done-past-month ()
  "Translates done=past-month to closed in last month."
  (let ((view-spec
         '((name . "Completed This Month")
           (done . past-month))))
    (assert-equal
     '(and (closed :from -30))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/done-past-year ()
  "Translates done=past-year to closed in last year."
  (let ((view-spec
         '((name . "Completed This Year")
           (done . past-year))))
    (assert-equal
     '(and (closed :from -365))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(deftest view-lang/done-t-unchanged ()
  "Translates done=t to any done item (unchanged behavior)."
  (let ((view-spec
         '((name . "All Done")
           (done . t))))
    (assert-equal
     '(and (done))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

;;; Incubated Project Type

(deftest view-lang/incubated-project-type ()
  "Translates incubated-project to projects in Tickler OR Someday."
  (let ((view-spec
         '((name . "Incubated Projects")
           (type . incubated-project))))
    (assert-equal
     '(and (and (or (property "ORG_GTD" "Tickler")
                    (property "ORG_GTD" "Someday"))
                (property "PREVIOUS_ORG_GTD" "Projects")))
     (org-gtd-view-lang--translate-to-org-ql view-spec))))

(provide 'gtd-view-language-test)

;;; gtd-view-language-test.el ends here
