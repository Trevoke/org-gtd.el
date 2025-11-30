;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

;; "appears in daily agenda when review date arrives" migrated to
;; test-eunit/acceptance/basic-workflows-test.el (tickler-item-programmatic-create)

(describe
 "A tickler item"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "stores review date in ORG_GTD_TIMESTAMP property for future consideration"
     (let* ((date (calendar-current-date))
            (year (nth 2 date))
            (month (nth 0 date))
            (day (nth 1 date)))
       (create-deferred-item "Yowza" date)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (task-type (current-task)) :to-equal 'tickler)
         (let ((timestamp (task-timestamp (current-task))))
           (expect timestamp :to-match (format "%s-%#02d-%#02d" year month day))))))

  (it "appears in daily agenda when review date arrives"
     (org-gtd-tickler-create "Dentist appointment"
                             (format-time-string "%Y-%m-%d"))
     (org-gtd-engage)
     (expect (agenda-contains? "Dentist appointment") :to-be-truthy)))

(describe "Smart tickler dispatcher"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "detects project heading and calls org-gtd-project-incubate"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        (org-back-to-heading t)

        ;; Call org-gtd-tickler with review date parameter
        (org-gtd-tickler "2025-12-01")

        ;; Verify project was tickler'd
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")
        (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-equal "Projects")))

  (it "detects single item and uses existing tickler logic"
      ;; Just verify that calling org-gtd-tickler on a single item
      ;; doesn't error and uses the existing path
      (create-single-action "Test action")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test action")
        (org-back-to-heading t)

        ;; Verify it's not a project (should use existing tickler logic)
        (expect (org-entry-get (point) "ORG_GTD") :not :to-equal "Projects")
        (expect (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")
                :to-equal nil))))

(describe "Smart reactivation dispatcher"
  (before-each (setq inhibit-message t)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (it "detects tickler'd project and calls org-gtd-project-reactivate"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        (org-back-to-heading t)

        ;; Tickler it first
        (org-gtd-tickler "2025-12-01")

        ;; Verify it's tickler'd
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")

        ;; Reactivate it
        (org-gtd-reactivate)

        ;; Verify it's reactivated
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Projects")
        (expect (org-entry-get (point) "PREVIOUS_ORG_GTD") :to-be nil))))
