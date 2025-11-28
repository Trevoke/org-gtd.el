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
                :to-equal org-gtd-tickler)
        (search-forward "memories")
        (expect (org-entry-get (point) "ORG_GTD" t)
                :to-equal org-gtd-tickler)
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

  (it "removes ORG_GTD from level 1 category headings while adding it to level 2 children"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
*** Design database
""")
        (basic-save-buffer))

      ;; Run property addition (Step 1 only)
      (org-gtd-upgrade--add-org-gtd-properties)

      (with-current-buffer (org-gtd--default-file)
        ;; Verify level 1 heading does NOT have ORG_GTD
        (goto-char (point-min))
        (re-search-forward "^\\* Projects$")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-be nil)

        ;; Verify level 2 heading HAS ORG_GTD="Projects"
        (goto-char (point-min))
        (search-forward "Build a webapp")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Projects")

        ;; Verify level 3 heading HAS ORG_GTD="Actions"
        (goto-char (point-min))
        (search-forward "Design database")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Actions")))

  (it "handles non-Projects categories correctly (e.g., Actions)"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Actions
:PROPERTIES:
:ORG_GTD: Actions
:END:
** Call mom
""")
        (basic-save-buffer))

      ;; Run property addition (Step 1 only)
      (org-gtd-upgrade--add-org-gtd-properties)

      (with-current-buffer (org-gtd--default-file)
        ;; Verify level 1 Actions heading does NOT have ORG_GTD
        (goto-char (point-min))
        (re-search-forward "^\\* Actions$")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-be nil)

        ;; Verify level 2 item HAS ORG_GTD="Actions"
        (goto-char (point-min))
        (search-forward "Call mom")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Actions")))

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

      ;; Run the migration function (Step 1 only - property addition)
      (org-gtd-upgrade--add-org-gtd-properties)

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

 (describe
  "Complete v3 to v4 user upgrade path"

  (it "removes ORG_GTD property from level 1 category headings"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
*** Design database
:PROPERTIES:
:ID: task-1
:END:

* Actions
:PROPERTIES:
:ORG_GTD: Actions
:END:
** Call mom
:PROPERTIES:
:ID: action-1
:END:
""")
        (basic-save-buffer))

      ;; Run full migration
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (org-gtd-upgrade-v3-to-v4))

      ;; Verify level 1 category headings do NOT have ORG_GTD property
      (with-current-buffer (org-gtd--default-file)
        ;; Check Projects category heading
        (goto-char (point-min))
        (re-search-forward "^\\* Projects$")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-be nil)

        ;; Check Actions category heading
        (goto-char (point-min))
        (re-search-forward "^\\* Actions$")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-be nil)

        ;; Verify level 2 items DO have ORG_GTD property
        (goto-char (point-min))
        (search-forward "Build a webapp")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Projects")

        (goto-char (point-min))
        (search-forward "Call mom")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Actions")

        ;; Verify level 3 under Projects has ORG_GTD="Actions"
        (goto-char (point-min))
        (search-forward "Design database")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD")
                :to-equal "Actions")))

  (it "full migration: converts v3 sequential projects to v4 dependency-based with correct task states"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
*** TODO Design database
:PROPERTIES:
:ID: task-1
:END:
*** TODO Implement API
:PROPERTIES:
:ID: task-2
:END:
""")
        (basic-save-buffer))

      ;; Verify v3 state
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Design database")
        (org-back-to-heading t)
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_DEPENDS_ON")
                :to-be nil))

      ;; Run full migration
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (org-gtd-upgrade-v3-to-v4))

      ;; Verify v4 state
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Design database")
        (org-back-to-heading t)
        (let ((blocks (org-entry-get-multivalued-property (point) "ORG_GTD_BLOCKS")))
          (expect blocks :to-be-truthy)
          (expect (member "task-2" blocks) :to-be-truthy))
        (expect (org-entry-get (point) "TODO")
                :to-equal (org-gtd-keywords--next))))

  (it "preserves user data during migration"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Project
*** TODO Task                                             :urgent:
:PROPERTIES:
:ID: t1
:CUSTOM: value
:END:
Content here.
""")
        (basic-save-buffer))

      (let (orig-tags orig-custom)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task")
          (org-back-to-heading t)
          (setq orig-tags (org-get-tags)
                orig-custom (org-entry-get (point) "CUSTOM")))

        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
          (org-gtd-upgrade-v3-to-v4))

        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Task")
          (org-back-to-heading t)
          (expect (org-get-tags) :to-equal orig-tags)
          (expect (org-entry-get (point) "CUSTOM") :to-equal orig-custom))))

  (it "respects backup confirmation"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Proj
*** TODO Task
:PROPERTIES:
:ID: t1
:END:
""")
        (basic-save-buffer))

      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
        (org-gtd-upgrade-v3-to-v4))

      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Proj$")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "ORG_GTD") :to-be nil)))

  (it "adds ORG_GTD_PROJECT_IDS property to all project tasks"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
:PROPERTIES:
:ID: proj-webapp-123
:END:
*** TODO Design database
:PROPERTIES:
:ID: task-1
:END:
*** TODO Implement API
:PROPERTIES:
:ID: task-2
:END:
**** TODO Create endpoint
:PROPERTIES:
:ID: task-3
:END:
** Write documentation
:PROPERTIES:
:ID: proj-docs-456
:END:
*** TODO Create user manual
:PROPERTIES:
:ID: task-4
:END:
""")
        (basic-save-buffer))

      ;; Run full migration
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (org-gtd-upgrade-v3-to-v4))

      ;; Verify all tasks have ORG_GTD_PROJECT_IDS set to their project's ID
      (with-current-buffer (org-gtd--default-file)
        ;; Check first project's tasks
        (goto-char (point-min))
        (search-forward "Design database")
        (org-back-to-heading t)
        (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (expect project-ids :to-be-truthy)
          (expect (member "proj-webapp-123" project-ids) :to-be-truthy))

        (goto-char (point-min))
        (search-forward "Implement API")
        (org-back-to-heading t)
        (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (expect project-ids :to-be-truthy)
          (expect (member "proj-webapp-123" project-ids) :to-be-truthy))

        ;; Check nested task
        (goto-char (point-min))
        (search-forward "Create endpoint")
        (org-back-to-heading t)
        (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (expect project-ids :to-be-truthy)
          (expect (member "proj-webapp-123" project-ids) :to-be-truthy))

        ;; Check second project's task
        (goto-char (point-min))
        (search-forward "Create user manual")
        (org-back-to-heading t)
        (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          (expect project-ids :to-be-truthy)
          (expect (member "proj-docs-456" project-ids) :to-be-truthy))))

  (it "adds TRIGGER property to all project tasks"
      (with-current-buffer (org-gtd--default-file)
        (insert """
* Projects
:PROPERTIES:
:ORG_GTD: Projects
:END:
** Build a webapp
:PROPERTIES:
:ID: proj-webapp-123
:END:
*** TODO Design database
:PROPERTIES:
:ID: task-1
:END:
*** TODO Implement API
:PROPERTIES:
:ID: task-2
:END:
**** TODO Create endpoint
:PROPERTIES:
:ID: task-3
:END:
** Write documentation
:PROPERTIES:
:ID: proj-docs-456
:END:
*** TODO Create user manual
:PROPERTIES:
:ID: task-4
:END:
""")
        (basic-save-buffer))

      ;; Run full migration
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
        (org-gtd-upgrade-v3-to-v4))

      ;; Verify all tasks have TRIGGER property set
      (with-current-buffer (org-gtd--default-file)
        ;; Check first project's tasks
        (goto-char (point-min))
        (search-forward "Design database")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TRIGGER")
                :to-equal "org-gtd-update-project-after-task-done!")

        (goto-char (point-min))
        (search-forward "Implement API")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TRIGGER")
                :to-equal "org-gtd-update-project-after-task-done!")

        ;; Check nested task
        (goto-char (point-min))
        (search-forward "Create endpoint")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TRIGGER")
                :to-equal "org-gtd-update-project-after-task-done!")

        ;; Check second project's task
        (goto-char (point-min))
        (search-forward "Create user manual")
        (org-back-to-heading t)
        (expect (org-entry-get (point) "TRIGGER")
                :to-equal "org-gtd-update-project-after-task-done!"))))
