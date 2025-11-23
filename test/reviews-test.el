;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Reviews"


 (before-each (setq inhibit-message t) (ogt--configure-emacs)
              (add-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
              (setq org-gtd-areas-of-focus '("Health" "Home" "Career")))
 (after-each (ogt--close-and-delete-files)
             (remove-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
             (setq org-gtd-areas-of-focus nil))

 (describe
  "Areas of focus"

  (it "throws an error if called programmatically with an area not in the list"
      (expect
       (org-gtd-review-area-of-focus "Playing")
       :to-throw
       'org-gtd-invalid-area-of-focus))

  (it "shows projects, next actions, habits, incubated items in agenda for a specific area of focus"
      (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                          "foo"
                          (org-file-contents
                           "test/fixtures/areas-of-focus.org"))))

        (org-gtd-review-area-of-focus "Home" "2021-11-20")

        (with-current-buffer org-agenda-buffer
          (let ((active-projects "Active projects[[:space:]].*?Fix the roof")
                (next-actions "Next actions[[:space:]].*?Clean gutters")
                (reminders "Reminders[[:space:]].*?Meet plumber")
                (routines "Routines[[:space:]].*?Sweep the")
                (incubated-items "Incubated items[[:space:]].*?For later"))
            (expect (buffer-name) :to-equal "*Org Agenda: Home*")
            (expect (current-buffer-raw-text) :to-match active-projects)
            (expect (current-buffer-raw-text) :to-match next-actions)
            (expect (current-buffer-raw-text) :to-match reminders)
            (expect (current-buffer-raw-text) :to-match routines)
            (expect (current-buffer-raw-text) :to-match incubated-items))))))
 (describe
  "Missed events"

  (it "shows unfinished items that have a timestamp in the past"
      (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                          "foo"
                          (org-file-contents
                           "test/fixtures/gtd-file.org"))))
        (org-gtd-review-missed-items "2021-11-20")
        (let ((agenda-contents (agenda-raw-text)))
          ;; these are *DONE OR CANCELED*
          (expect agenda-contents :not :to-match "not worth thinking about")
          (expect agenda-contents :not :to-match "write a nice test")
          ;; these are *IN THE FUTURE*
          (expect agenda-contents :not :to-match "repeating item")
          (expect agenda-contents :not :to-match "For later")
          ;; these are *UNDONE IN THE PAST*
          (expect agenda-contents :to-match "probably overdue by now")
          (expect agenda-contents :to-match "Time to review this one")
          ;; this is *OVERDUE DELEGATED*
          (expect agenda-contents :to-match "Overdue delegated")
          )))))

 (describe
  "Area of focus review with incubated projects"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs)
               (setq org-gtd-areas-of-focus '("Work" "Personal")))
  (after-each (ogt--close-and-delete-files))

  (it "shows incubated projects in area review"
      ;; Create active and incubated projects in Work area
      (create-project "Active work project")
      (create-project "Incubated work project")

      (with-current-buffer (org-gtd--default-file)
        ;; Set CATEGORY property for both projects to Work area
        (goto-char (point-min))
        (search-forward "Active work project")
        (org-back-to-heading t)
        (org-entry-put (point) "CATEGORY" "Work")

        (goto-char (point-min))
        (search-forward "Incubated work project")
        (org-back-to-heading t)
        (org-entry-put (point) "CATEGORY" "Work")

        ;; Incubate the second project
        (org-gtd-incubate "2025-12-01"))

      ;; Run area of focus review for Work
      (org-gtd-review-area-of-focus "Work")

      (with-current-buffer org-agenda-buffer
        ;; Should show active project in Active projects section
        (expect (buffer-string) :to-match "Active projects")
        (expect (buffer-string) :to-match "Active work project")
        ;; Should show incubated project in Incubated projects section
        (expect (buffer-string) :to-match "Incubated projects")
        (expect (buffer-string) :to-match "Incubated work project"))))


;; (let* ((yesterday (format-org-date -1))
;;        (tomorrow (format-org-date 1))
;;        (mock-org-content (concat
;;                           "* Incubate
;; :PROPERTIES:
;; :ORG_GTD: Incubated
;; :END:
;; ** DONE write a test
;; :PROPERTIES:
;; :ORG_GTD_TIMESTAMP: " yesterday "
;; :END:

;; * Actions
;; :PROPERTIES:
;; :ORG_GTD: Actions
;; :END:
;; ** WAIT Wait for someone
;; :PROPERTIES:
;; :ORG_GTD_TIMESTAMP: " tomorrow "
;; :DELEGATED_TO: future me
;; :END:"
;;  )))

;;   (with-temp-buffer
;;     (org-mode)
;;     (insert mock-org-content)
;;     (setq temp-org-buffer (current-buffer))))
