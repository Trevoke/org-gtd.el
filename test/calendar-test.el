;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "A calendar item"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "can be added programmatically"
     (org-gtd-calendar-create "Dentist appointment"
                              (format-time-string "%Y-%m-%d"))
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (expect (ogt--current-buffer-raw-text)
               :to-match
               "Dentist appointment")))

 (it "has a specific property with the active timestamp"
     (let* ((date (calendar-current-date))
            (year (nth 2 date))
            (month (nth 0 date))
            (day (nth 1 date)))
       (ogt-capture-and-process-calendar-item "Yowza" date)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (org-entry-get (point) org-gtd-timestamp)
                 :to-match (format "%s-%#02d-%#02d" year month day)))))

 (it "cleans up conflicting GTD state when clarify-item is called during creation"
     ;; This test verifies that org-gtd-clarify-item (called by org-gtd-calendar-create)
     ;; properly cleans up any existing conflicting GTD properties via
     ;; org-gtd-wip--maybe-initialize-buffer-contents
     (let ((topic "Item with conflicting state")
           (date (format-time-string "%Y-%m-%d")))

       ;; Create an item with conflicting GTD state, then use calendar-create to clean it up
       (with-temp-buffer
         (org-mode)
         (insert (format "* %s\n:PROPERTIES:\n:ORG_GTD_TIMESTAMP: <2020-01-01>\n:DELEGATED_TO: Someone Else\n:STYLE: habit\n:ORG_GTD: Actions\n:END:\n" topic))
         (goto-char (point-min))
         (search-forward topic)
         (org-back-to-heading)

         ;; Verify conflicting properties are present before clarification
         (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-equal "<2020-01-01>")
         (expect (org-entry-get (point) "DELEGATED_TO") :to-equal "Someone Else")
         (expect (org-entry-get (point) "STYLE") :to-equal "habit")
         (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")

         ;; Test that a calendar item created programmatically is clean
         (org-gtd-calendar-create topic date)

         ;; Verify that a properly created calendar item appears in agenda
         (org-gtd-engage)
         (with-current-buffer org-agenda-buffer
           ;; The calendar creation should have worked despite conflicting source state
           (expect (ogt--current-buffer-raw-text) :to-match topic)))))

 (describe
  "compatibility with orgzly"
  (it "has a copy of the active timestamp in the body"
      (let* ((date (calendar-current-date))
             (year (nth 2 date))
             (month (nth 0 date))
             (day (nth 1 date)))
        (ogt-capture-and-process-calendar-item "Yowza" date)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Yowza")
          (org-end-of-meta-data t)
          (expect (ogt--current-buffer-raw-text)
                  :to-match
                  (format "<%s-%#02d-%#02d>" year month day)))))))
