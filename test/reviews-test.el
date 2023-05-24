;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Reviews"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs)
              (add-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
              (setq org-gtd-areas-of-focus '("Health" "Home" "Career")))
 (after-each (ogt--close-and-delete-files)
             (remove-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
             (setq org-gtd-areas-of-focus nil))

 (describe
  "Areas of focus"

  (it "throws an error if called programmatically with an area not in the list"
      (expect
       (org-gtd-reflect-area-of-focus "Playing")
       :to-throw
       'org-gtd-invalid-area-of-focus))

  (it "shows projects, next actions, habits, incubated items in agenda for a specific area of focus"
      (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                          "foo"
                          (org-file-contents
                           "test/fixtures/areas-of-focus.org"))))

        (org-gtd-reflect-area-of-focus "Home" "2021-11-20")

        (with-current-buffer org-agenda-buffer
          (let ((active-projects "Active projects[[:space:]].*?Fix the roof")
                (next-actions "Next actions[[:space:]].*?Clean gutters")
                (reminders "Reminders[[:space:]].*?20 November 2021[[:space:]].*?Meet plumber")
                (routines "Routines[[:space:]].*?20 November 2021[[:space:]].*?Sweep the")
                (incubated-items "Incubated items[[:space:]].*?For later"))
            (expect (buffer-name) :to-equal "*Org Agenda: Home*")
            (expect (ogt--current-buffer-raw-text) :to-match active-projects)
            (expect (ogt--current-buffer-raw-text) :to-match next-actions)
            (expect (ogt--current-buffer-raw-text) :to-match reminders)
            (expect (ogt--current-buffer-raw-text) :to-match routines)
            (expect (ogt--current-buffer-raw-text) :to-match incubated-items)))))))
