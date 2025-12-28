;;; org-gtd-upgrades.el --- Define upgrade logic across org-gtd versions -*- lexical-binding: t; coding: utf-8 -*-
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
;; Major versions aren't backward compatible.  This code helps users move
;; their data forward.
;;
;;; Code:

;;;; Requirements

(require 'org-habit)

(require 'org-gtd-types)
(require 'org-gtd-habit)
(require 'org-gtd-projects)

;;;; Commands

(defun org-gtd-upgrade-v2-to-v3 ()
  "Use only when upgrading org-gtd from v2 to v3.

Changes state of org-gtd tasks to move away from incorrectly used SCHEDULED
planning keyword in `org-mode'."
  (interactive)
  (org-gtd-upgrades-calendar-items-to-v3)
  (org-gtd-upgrades-delegated-items-to-v3)
  (org-gtd-upgrades-incubated-items-to-v3)
  (org-gtd-upgrades-habits-to-v3))

;;;; Functions

;;;;; Public

(defun org-gtd-upgrades-calendar-items-to-v3 ()
  "Change calendar items away from SCHEDULED to using a custom property."
  ;; v4: Explicitly bind settings for upgrade - users may not have
  ;; configured them yet. Property inheritance needed for v3-style data where
  ;; ORG_GTD is on parent headings.
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (org-use-property-inheritance "ORG_GTD"))
    (org-map-entries
     (lambda ()
       (when (org-gtd-upgrades--scheduled-item-p)
         (let ((date (org-entry-get (point) "SCHEDULED")))
           (org-schedule '(4)) ;; pretend I am a universal argument
           (org-entry-put (point) org-gtd-timestamp date)
           (org-end-of-meta-data t)
           (open-line 1)
           (insert date))))
     "+ORG_GTD=\"Calendar\"+LEVEL=2"
     'agenda)))

(defun org-gtd-upgrades-delegated-items-to-v3 ()
  "Change delegated items away from SCHEDULED to using a custom property."
  ;; v4: Explicitly bind settings for upgrade. Property inheritance needed
  ;; for v3-style data where ORG_GTD is on parent headings.
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (org-use-property-inheritance "ORG_GTD"))
    (org-map-entries
     (lambda ()
       (when (org-gtd-upgrades--delegated-item-p)
         (let ((date (org-entry-get (point) "SCHEDULED")))
           (org-schedule '(4)) ;; pretend I am a universal argument
           (org-entry-put (point) org-gtd-timestamp date)
           (org-end-of-meta-data t)
           (open-line 1)
           (insert date))))
     "+ORG_GTD=\"Actions\"+LEVEL=2"
     'agenda)))

(defun org-gtd-upgrades-habits-to-v3 ()
  "Move habits from wherever they may be to their own subtree."
  ;; v4: Explicitly bind settings for upgrade. Property inheritance needed
  ;; for v3-style data where ORG_GTD is on parent headings.
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (org-use-property-inheritance "ORG_GTD"))
    (org-gtd-refile--add-target org-gtd-habit-template)

    (let ((org-gtd-refile-to-any-target t))
      (org-map-entries #'org-gtd-upgrades--organize-habits-v3
                       "+LEVEL=2&+ORG_GTD=\"Actions\""
                       'agenda)
      (org-map-entries #'org-gtd-upgrades--organize-habits-v3
                       "+LEVEL=2&+ORG_GTD=\"Incubated\""
                       'agenda)
      (org-map-entries #'org-gtd-upgrades--organize-habits-v3
                       "+LEVEL=2&+ORG_GTD=\"Calendar\""
                       'agenda))))

(defun org-gtd-upgrades-incubated-items-to-v3 ()
  "Change incubated items away from SCHEDULED to using a custom property."
  ;; v4: Explicitly bind settings for upgrade. Property inheritance needed
  ;; for v3-style data where ORG_GTD is on parent headings.
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (org-use-property-inheritance "ORG_GTD"))
    (org-map-entries
     (lambda ()
       ;; Rename Incubated to Tickler
       (org-entry-put (point) "ORG_GTD" org-gtd-tickler)
       (when (org-gtd-upgrades--scheduled-item-p)
         (let ((date (org-entry-get (point) "SCHEDULED")))
           (org-schedule '(4)) ;; pretend I am a universal argument
           (org-entry-put (point) org-gtd-timestamp date)
           (org-end-of-meta-data t)
           (open-line 1)
           (insert date))))
     "+ORG_GTD=\"Incubated\"+LEVEL=2"
     'agenda)))

;;;;; Private

(defun org-gtd-upgrades--delegated-item-p ()
  "Return t if item at point is delegated."
  (and (org-entry-get (point) (org-gtd-type-property 'delegated :who))
       (string-equal (org-entry-get (point) org-gtd-prop-todo) (org-gtd-keywords--wait))))

(defun org-gtd-upgrades--organize-habits-v3 ()
  "Move element at point to the habits home if it's a habit."
  (when (org-is-habit-p)
    (setq org-map-continue-from (- (org-element-property :begin
                                                         (org-element-at-point))
                                   1))
    (org-entry-put (point) "ORG_GTD" org-gtd-habit)
    (org-gtd-refile--do org-gtd-habit org-gtd-habit-template)))

(defun org-gtd-upgrades--scheduled-item-p ()
  "Return t if item at point is SCHEDULED and not a habit."
  (and (not (org-is-habit-p))
       (org-get-scheduled-time (point))))

;;;###autoload
(defun org-gtd-upgrade-v3-to-v4 ()
  "Migrate from org-gtd v3 to v4 property-based and dependency system.

This migration performs FIVE required steps:

STEP 1: Migrate Incubated items to Tickler/Someday
  - Items with ORG_GTD_TIMESTAMP: Sets ORG_GTD=\"Tickler\"
  - Items without timestamp: Sets ORG_GTD=\"Someday\"
  - Level 1 heading: Gets ORG_GTD_REFILE based on first child's type

STEP 2: Migrate ORG_GTD properties
  - Level 1 category headings: Renames ORG_GTD → ORG_GTD_REFILE
    (preserves them as refile targets for org-gtd's refile system)
  - Level 2+ items: Adds ORG_GTD property to mark item type
  - Project tasks: Adds ORG_GTD=\"Actions\" property

STEP 3: Migrate delegated items to new type
  - Items with DELEGATED_TO property: Changes ORG_GTD from
    \"Actions\" to \"Delegated\"
  - This separates delegated items from regular single actions

STEP 4: Migrate habits to have ORG_GTD property
  - Items with STYLE=\"habit\": Adds ORG_GTD=\"Habit\"
  - This ensures habits are discoverable via the unified type
    system

STEP 5: Add dependency properties to projects
  - Adds ORG_GTD_DEPENDS_ON and ORG_GTD_BLOCKS for sequential
    dependencies
  - Adds ORG_GTD_FIRST_TASKS to project headings
  - Sets correct NEXT/TODO states based on dependencies

AFTER UPGRADE:

Your level 1 category headings now use ORG_GTD_REFILE property and
can serve as refile targets. You can:
  - Move these headings anywhere in org-agenda-files (any level)
  - Create new refile targets by adding ORG_GTD_REFILE property
  - Configure org-refile-targets - org-gtd merges your targets with
    its own

Items can now exist anywhere in org-agenda-files - ORG_GTD_REFILE
markers are purely for organizational convenience. Your
org-refile-targets appear first in completion, followed by org-gtd's
property-based targets.

This is REQUIRED for org-gtd v4 to work correctly. The old
TRIGGER-based project system no longer works - v4 uses dependency
properties instead.

Make a backup before running! Safe to run multiple times."
  (interactive)
  (when (yes-or-no-p "This will modify your GTD files. Have you made a backup? ")
    (message "Migrating to org-gtd v4...")

    ;; Step 1: Migrate Incubated items to Tickler/Someday
    ;; Must run before Step 2 so that Incubated headings are processed specially
    (message "Step 1/5: Migrating incubated items...")
    (org-gtd-upgrade--migrate-incubated-items)

    ;; Step 2: Add ORG_GTD properties
    (message "Step 2/5: Adding ORG_GTD properties...")
    (org-gtd-upgrade--add-org-gtd-properties)

    ;; Step 3: Migrate delegated items to new ORG_GTD value
    (message "Step 3/5: Migrating delegated items...")
    (org-gtd-upgrade--migrate-delegated-items)

    ;; Step 4: Migrate habits to have ORG_GTD property
    (message "Step 4/5: Migrating habits...")
    (org-gtd-upgrade--migrate-habits)

    ;; Step 5: Add dependency properties
    (message "Step 5/5: Adding project dependencies...")
    (org-gtd-upgrade--add-project-dependencies)

    (message "Migration complete! Your projects now use the dependency system.")))

(defun org-gtd-upgrade--migrate-delegated-items ()
  "Migrate delegated items to use ORG_GTD=Delegated (Step 2 of migration).

In v3, delegated items had ORG_GTD=\"Actions\" with a DELEGATED_TO property.
In v4, delegated items have ORG_GTD=\"Delegated\" with DELEGATED_TO property.

This function finds all items with DELEGATED_TO property and changes their
ORG_GTD value from \"Actions\" to \"Delegated\".

Safe to run multiple times - only updates items still marked as \"Actions\"."
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (migrated-count 0))
    (org-map-entries
     (lambda ()
       (let ((org-gtd-value (org-entry-get (point) "ORG_GTD"))
             (delegated-to (org-entry-get (point) "DELEGATED_TO")))
         ;; Only migrate if: has DELEGATED_TO AND ORG_GTD is "Actions"
         (when (and delegated-to
                    (string= org-gtd-value "Actions"))
           (org-entry-put (point) "ORG_GTD" "Delegated")
           (setq migrated-count (1+ migrated-count))
           (message "  Migrated: %s" (org-get-heading t t t t)))))
     "+DELEGATED_TO={.+}"
     'agenda)
    (message "Migrated %d delegated items to ORG_GTD=\"Delegated\"" migrated-count)))

(defun org-gtd-upgrade--migrate-habits ()
  "Set ORG_GTD=Habit for all items with STYLE=habit (Step 3 of migration).

In v3, habits were identified solely by STYLE=\"habit\" property.
In v4, habits have ORG_GTD=\"Habit\" for unified type discovery.

This function finds all items with STYLE=\"habit\" and sets ORG_GTD=\"Habit\".
Also fixes items from v2->v3 migration that incorrectly have ORG_GTD=\"Habits\"
\(plural) instead of the correct v4 value \"Habit\" (singular).

Safe to run multiple times - only updates items needing correction."
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (migrated-count 0))
    (org-map-entries
     (lambda ()
       (let ((org-gtd-value (org-entry-get (point) "ORG_GTD"))
             (style (org-entry-get (point) "STYLE")))
         ;; Migrate if: has STYLE="habit" AND ORG_GTD is missing or wrong
         (when (and (string= style "habit")
                    (not (string= org-gtd-value "Habit")))
           (org-entry-put (point) "ORG_GTD" "Habit")
           (setq migrated-count (1+ migrated-count))
           (message "  Migrated habit: %s" (org-get-heading t t t t)))))
     "+STYLE=\"habit\""
     'agenda)
    (message "Migrated %d habits to ORG_GTD=\"Habit\"" migrated-count)))

(defun org-gtd-upgrade--migrate-incubated-items ()
  "Migrate Incubated items to Tickler or Someday based on timestamp.

In v3, Incubated was used for both:
- Someday/maybe items (no specific date)
- Tickler items (remind me on a specific date)

In v4, these are separate types:
- Tickler: Items with ORG_GTD_TIMESTAMP property
- Someday: Items without a timestamp

This function:
1. Finds level 1 headings with ORG_GTD=\"Incubated\"
2. Sets ORG_GTD on each level 2 child based on timestamp presence
3. Sets ORG_GTD_REFILE on level 1 based on first child's type
4. Removes ORG_GTD from level 1 heading

Safe to run multiple times."
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (org-use-property-inheritance "ORG_GTD")
        (migrated-count 0))
    (org-map-entries
     (lambda ()
       (let ((category-level (org-current-level))
             (first-child-type nil))
         ;; Process all level 2 children
         (save-excursion
           (outline-next-heading)
           (while (and (not (eobp))
                       (> (org-current-level) category-level))
             (when (= (org-current-level) (1+ category-level))
               ;; Determine type based on timestamp
               (let ((has-timestamp (org-entry-get (point) org-gtd-timestamp))
                     (item-type nil))
                 (setq item-type (if has-timestamp "Tickler" "Someday"))
                 ;; Track first child's type for ORG_GTD_REFILE
                 (unless first-child-type
                   (setq first-child-type item-type))
                 ;; Set ORG_GTD on the child
                 (org-entry-put (point) "ORG_GTD" item-type)
                 (setq migrated-count (1+ migrated-count))
                 (message "  Migrated incubated item to %s: %s"
                          item-type (org-get-heading t t t t))))
             (outline-next-heading)))
         ;; Set ORG_GTD_REFILE on level 1 heading based on first child
         (when first-child-type
           (org-entry-put (point) "ORG_GTD_REFILE" first-child-type))
         ;; Remove ORG_GTD from level 1 heading
         (org-entry-delete (point) "ORG_GTD")))
     "+LEVEL=1+ORG_GTD=\"Incubated\""
     'agenda)
    (message "Migrated %d incubated items to Tickler/Someday" migrated-count)))

(defun org-gtd-upgrade--add-org-gtd-properties ()
  "Add ORG_GTD properties to existing items (Step 1 of migration)."
  ;; v4: Explicitly bind settings for upgrade. Property inheritance needed
  ;; for v3-style data where ORG_GTD is on parent headings.
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (org-use-property-inheritance "ORG_GTD"))
    ;; Find ALL level 1 category headings with ORG_GTD property
    ;; For each: save category type, remove from level 1, add to level 2 children
    (org-map-entries
     (lambda ()
       (let ((category-type (org-entry-get (point) "ORG_GTD"))
             (category-level (org-current-level)))
         (when category-type
           ;; Rename ORG_GTD to ORG_GTD_REFILE on level 1 category heading
           ;; This preserves the heading as a refile target in v4
           (org-entry-put (point) "ORG_GTD_REFILE" category-type)
           (org-entry-delete (point) "ORG_GTD")

           ;; Add category type to all level 2 children
           (outline-next-heading)
           (while (and (not (eobp))
                       (> (org-current-level) category-level))
             (when (= (org-current-level) (1+ category-level))
               ;; This is a level 2 item under category - add the category type
               (unless (org-entry-get (point) "ORG_GTD")
                 (org-entry-put (point) "ORG_GTD" category-type)))
             (outline-next-heading)))))
     "LEVEL=1"
     'agenda)

    ;; Find all project headings and process their children as project tasks
    (org-map-entries
     (lambda ()
       (let ((project-level (org-current-level)))
         (outline-next-heading)
         (while (and (not (eobp))
                     (> (org-current-level) project-level))
           (when (> (org-current-level) project-level)
             ;; This is any descendant (level 3+) under project heading - it's a project task
             (unless (org-entry-get (point) "ORG_GTD")
               (org-entry-put (point) "ORG_GTD" "Actions")))
           (outline-next-heading))))
     "+ORG_GTD=\"Projects\"+LEVEL=2"
     'agenda)))

(defun org-gtd-upgrade--set-project-ids-on-tasks (project-marker)
  "Set ORG_GTD_PROJECT_IDS and TRIGGER on tasks under PROJECT-MARKER.
Safe to run multiple times - only adds ID if not already present.
Sets TRIGGER to org-gtd-update-project-after-task-done! on all."
  (org-with-point-at project-marker
    (let ((project-id (or (org-entry-get (point) "ID")
                          (org-gtd-id-get-create))))
      ;; Process all descendants under the project heading
      (org-map-entries
       (lambda ()
         (when (string= (org-entry-get (point) "ORG_GTD") "Actions")
           ;; Add project ID to ORG_GTD_PROJECT_IDS (multivalued property)
           (let ((existing-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
             (unless (member project-id existing-ids)
               (org-entry-add-to-multivalued-property (point) "ORG_GTD_PROJECT_IDS" project-id)))
           ;; Add TRIGGER property to task (self finder + action for org-edna)
           (org-entry-put (point) "TRIGGER" "self org-gtd-update-project-after-task-done!")))
       nil
       'tree))))

(defun org-gtd-upgrade--add-project-dependencies ()
  "Add dependency properties to existing projects (Step 2 of migration)."
  (require 'org-gtd-projects)
  ;; v4: Explicitly bind settings for upgrade. Property inheritance needed
  ;; for v3-style data where ORG_GTD is on parent headings.
  (let ((org-agenda-files (org-gtd-core--agenda-files))
        (org-use-property-inheritance "ORG_GTD"))
    ;; Find all project headings and add dependencies
    (org-map-entries
     (lambda ()
       (let ((project-marker (point-marker)))
         (message "Processing project: %s" (org-get-heading t t t t))
         ;; Add ORG_GTD_PROJECT_IDS to all tasks under this project
         (org-gtd-upgrade--set-project-ids-on-tasks project-marker)
         ;; Setup sequential dependencies for this project
         (org-gtd-project--setup-dependencies project-marker)
         ;; Recalculate task states based on new dependencies
         (org-gtd-projects-fix-todo-keywords project-marker)))
     "+ORG_GTD=\"Projects\""
     'agenda)))

;;;; Footer

(provide 'org-gtd-upgrades)

;;; org-gtd-upgrades.el ends here
