;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



;; Load test helpers via setup.el (which now uses require internally)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Upgrading org-gtd"

; :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "To v3"

  (it "moves calendar items away from using SCHEDULED"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Calendared
:PROPERTIES:
:ORG_GTD:  Calendar
:END:
** Twitch Affiliate anniversary
:PROPERTIES:
:LAST_REPEAT: [2023-01-03 Tue 21:59]
:END:
<2023-12-11 Mon +1y>

** Workout                                                        :@workout:
SCHEDULED: <2023-01-04 Wed .+1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2023-01-03 Tue 21:58]
:CATEGORY: Health
:Effort:   30min
:END:

I think the text goes here

** record meaningful memories
SCHEDULED: <2023-04-03>

Do that thing.
""")
        (basic-save-buffer))
      (org-gtd-upgrades-calendar-items-to-v3)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Twitch")
        (expect (org-entry-get (point) org-gtd-timestamp)
                :to-be
                nil)
        (search-forward "Workout")
        (expect (org-entry-get (point) org-gtd-timestamp)
                :to-be nil)
        (search-forward "memories")
        (expect (org-entry-get (point) org-gtd-timestamp)
                :to-equal "<2023-04-03>")))

  (it "moves delegated items away from using SCHEDULED"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Incubated
:PROPERTIES:
:ORG_GTD:  Actions
:END:

** NEXT take a nice nap

** WAIT record meaningful memories
SCHEDULED: <2023-04-03>
:PROPERTIES:
:DELEGATED_TO: Someone
:END:

Do that thing.
""")
        (basic-save-buffer))
      (org-gtd-upgrades-delegated-items-to-v3)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "nice nap")
        (expect (org-entry-get (point) org-gtd-timestamp)
                :to-be nil)
        (search-forward "memories")
        (expect (org-entry-get (point) org-gtd-timestamp)
                :to-equal "<2023-04-03>")
        ))

  (it "moves incubate items away from using SCHEDULED"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Incubated
:PROPERTIES:
:ORG_GTD:  Incubated
:END:

** take a nice nap

** record meaningful memories
SCHEDULED: <2023-04-03>

Do that thing.
""")
        (basic-save-buffer))
      (org-gtd-upgrades-incubated-items-to-v3)
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "nice nap")
        (expect (org-entry-get (point) org-gtd-timestamp)
                :to-be nil)
        (expect (org-entry-get (point) "ORG_GTD" t)
                :to-equal org-gtd-incubate)
        (search-forward "memories")
        (expect (org-entry-get (point) "ORG_GTD" t)
                :to-equal org-gtd-incubate)
        (expect (org-entry-get (point) org-gtd-timestamp)
                :to-equal "<2023-04-03>")))


  (it "moves habits to their own tree"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Calendared
:PROPERTIES:
:ORG_GTD:  Calendar
:END:
** Twitch Affiliate anniversary
:PROPERTIES:
:LAST_REPEAT: [2023-01-03 Tue 21:59]
:END:
<2023-12-11 Mon +1y>

** Workout                                                        :@workout:
SCHEDULED: <2023-01-04 Wed .+1d>
:PROPERTIES:
:STYLE:    habit
:LAST_REPEAT: [2023-01-03 Tue 21:58]
:CATEGORY: Health
:Effort:   30min
:END:

I think the text goes here

** record meaningful memories
SCHEDULED: <2023-04-03>

Do that thing.
""")
        (basic-save-buffer))
      (org-gtd-upgrades-habits-to-v3)

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Workout")
        (expect (org-entry-get nil "ORG_GTD" t)
                :to-equal org-gtd-habit)

        (goto-char (point-min))
        (search-forward "Twitch Affiliate")
        (expect (org-entry-get nil "ORG_GTD" t)
                :to-equal org-gtd-calendar)

        (goto-char (point-min))
        (search-forward "record meaningful")
        (expect (org-entry-get nil "ORG_GTD" t)
                :to-equal org-gtd-calendar))))

 (describe
  "Property-based project system migration"

  (it "migrates level-based projects to property-based system where project headings can exist at any level"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
*** Set up development environment
*** Design the user interface
*** Implement backend API
**** Create user authentication
**** Set up database
** Write documentation
*** Create user manual
*** Document API endpoints
""")
        (basic-save-buffer))

      ;; Run the migration function
      (org-gtd-upgrade--migrate-level-to-property-based)

      (with-current-buffer (org-gtd--default-file)
        ;; Verify project headings have correct ORG_GTD property
        (goto-char (point-min))
        (search-forward "Build a webapp")
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Projects")

        (goto-char (point-min))
        (search-forward "Write documentation")
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Projects")

        ;; Verify project tasks have ORG_GTD=Actions property
        (goto-char (point-min))
        (search-forward "Set up development environment")
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Actions")

        (goto-char (point-min))
        (search-forward "Create user authentication")
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Actions")

        (goto-char (point-min))
        (search-forward "Set up database")
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Actions")))

  (it "allows project tasks to be collected via property queries regardless of their level in hierarchy"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
:PROPERTIES:
:ORG_GTD: Projects
:ID: proj-webapp
:ORG_GTD_FIRST_TASKS: task-setup task-design
:END:
*** Set up development environment
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-setup
:ORG_GTD_PROJECT_IDS: proj-webapp
:END:
*** Design the user interface
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-design
:ORG_GTD_BLOCKS: task-wireframes task-colors
:ORG_GTD_PROJECT_IDS: proj-webapp
:END:
**** Create wireframes
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-wireframes
:ORG_GTD_DEPENDS_ON: task-design
:ORG_GTD_PROJECT_IDS: proj-webapp
:END:
**** Choose color scheme
:PROPERTIES:
:ORG_GTD: Actions
:ID: task-colors
:ORG_GTD_DEPENDS_ON: task-design
:ORG_GTD_PROJECT_IDS: proj-webapp
:END:
""")
        (basic-save-buffer))

      ;; Test that we can find all project tasks via property query
      (with-current-buffer (org-gtd--default-file)
        ;; ORG_GTD_FIRST_TASKS property already set in the inserted content
        (goto-char (point-min))
        (search-forward "Build a webapp")
        (org-back-to-heading t)

        (goto-char (point-min))
        (search-forward "Build a webapp")
        (org-back-to-heading t)
        (let ((tasks (org-gtd-projects--collect-tasks-by-graph (point-marker))))
          ;; Should find all 4 tasks regardless of their level
          (expect (length tasks) :to-equal 4)
          ;; Verify we can access all tasks by their properties
          (let ((task-ids '()))
            (dolist (task tasks)
              (save-excursion
                (goto-char task)
                (let ((id (org-entry-get (point) "ID"))
                      (org-gtd (org-entry-get (point) "ORG_GTD")))
                  (expect org-gtd :to-equal "Actions")
                  (push id task-ids))))
            ;; All task IDs should be present
            (expect (member "task-setup" task-ids) :to-be-truthy)
            (expect (member "task-design" task-ids) :to-be-truthy)
            (expect (member "task-wireframes" task-ids) :to-be-truthy)
            (expect (member "task-colors" task-ids) :to-be-truthy)))))))
