;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Upgrading org-gtd"

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
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
                 :to-be
                 nil)
         (search-forward "Workout")
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
                 :to-be nil)
         (search-forward "memories")
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
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
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
                 :to-be nil)
         (search-forward "memories")
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
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
         (expect (org-entry-get (point) "ORG_GTD_INCUBATE")
                 :to-be nil)
         (search-forward "memories")
         (expect (org-entry-get (point) "ORG_GTD_INCUBATE")
                 :to-equal "<2023-04-03>")
         ))))
