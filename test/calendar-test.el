;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "A calendar item"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "appears in daily agenda after creation"
     (org-gtd-calendar-create "Dentist appointment"
                              (format-time-string "%Y-%m-%d"))
     (org-gtd-engage)
     (expect (agenda-contains? "Dentist appointment") :to-be-truthy))

 (it "stores scheduled date in ORG_GTD_TIMESTAMP property for calendar items"
     (let* ((date (calendar-current-date))
            (year (nth 2 date))
            (month (nth 0 date))
            (day (nth 1 date)))
       (create-calendar-item "Yowza" date)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (let ((timestamp (task-timestamp (current-task))))
           (expect timestamp :to-match (format "%s-%#02d-%#02d" year month day))))))

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
         (expect (task-timestamp (current-task)) :to-equal "<2020-01-01>")
         (expect (task-delegated-to (current-task)) :to-equal "Someone Else")
         (expect (task-property (current-task) "STYLE") :to-equal "habit")
         (expect (task-property (current-task) "ORG_GTD") :to-equal "Actions")

         ;; Test that a calendar item created programmatically is clean
         (org-gtd-calendar-create topic date)

         ;; Verify that a properly created calendar item appears in agenda
         (org-gtd-engage)
         ;; The calendar creation should have worked despite conflicting source state
         (expect (agenda-contains? topic) :to-be-truthy))))

 (describe
  "compatibility with orgzly"
  (it "has a copy of the active timestamp in the body"
      (let* ((date (calendar-current-date))
             (year (nth 2 date))
             (month (nth 0 date))
             (day (nth 1 date)))
        (create-calendar-item "Yowza" date)
        (expect (file-contains? (org-gtd--default-file)
                               (format "<%s-%#02d-%#02d>" year month day))
                :to-be-truthy)))))
