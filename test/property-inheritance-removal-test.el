;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd-test-helper-processing (file-name-concat default-directory "test/helpers/processing.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

;;; Commentary:
;;
;; These tests verify that v4 items have direct ORG_GTD properties
;; and do NOT need property inheritance to function correctly.
;;
;; The `t` argument in `org-entry-get` calls should be removable.

(describe "Property inheritance removal"

  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "ORG_GTD property is directly on items (not inherited)"

    (it "single actions have direct ORG_GTD property"
      (create-single-action "Test action")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test action")
        ;; WITHOUT inheritance (no t argument)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")))

    (it "calendar items have direct ORG_GTD property"
      (create-calendar-item "Test calendar" (calendar-current-date))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test calendar")
        ;; WITHOUT inheritance (no t argument)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Calendar")))

    (it "project headings have direct ORG_GTD property"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        ;; WITHOUT inheritance (no t argument)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Projects")))

    (it "project tasks have direct ORG_GTD property"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Task 1")
        ;; WITHOUT inheritance (no t argument)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Actions")))

    (it "tickler items have direct ORG_GTD property"
      (create-deferred-item "Test tickler" (calendar-current-date))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test tickler")
        ;; WITHOUT inheritance (no t argument)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Tickler")))

    (it "delegated items have direct ORG_GTD property"
      (create-delegated-item "Test delegate" "John Doe" (calendar-current-date))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test delegate")
        ;; WITHOUT inheritance (no t argument)
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Delegated")))

    (it "habits have direct ORG_GTD property"
      (create-habit "Test habit" ".+1d")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test habit")
        ;; WITHOUT inheritance (no t argument)
        ;; Note: Config sets "Habit" (singular), not "Habits"
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Habit"))))

  (describe "areas-of-focus validation works without inheritance"

    (it "can validate item has ORG_GTD without t argument"
      (create-single-action "Test action")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test action")
        ;; This is the check in org-gtd-areas-of-focus.el:71
        ;; WITHOUT inheritance should still work
        (expect (org-entry-get nil "ORG_GTD") :not :to-be nil))))

  (describe "skip functions work without inheritance"

    (it "calendar skip works without inheritance"
      (create-calendar-item "Test calendar" (calendar-current-date))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test calendar")
        ;; org-gtd-skip.el:101 - Check ORG_GTD directly without inheritance
        (expect (org-entry-get (point) "ORG_GTD") :to-equal "Calendar"))))

  (describe "refile target verification works without inheritance"

    (it "project heading verification works without inheritance"
      (create-project "Test project")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "Test project")
        ;; org-gtd-refile.el:103 - Check ORG_GTD directly without inheritance
        (expect (org-entry-get nil "ORG_GTD") :to-equal "Projects"))))

  (describe "agenda CATEGORY lookup for project tasks"

    (it "project task can look up CATEGORY from project heading via ID"
      ;; Create a project and set CATEGORY on it
      (create-project "Test project with category")
      (with-current-buffer (org-gtd--default-file)
        ;; Set CATEGORY on project heading
        (goto-char (point-min))
        (search-forward "Test project with category")
        (org-entry-put (point) "CATEGORY" "Work")
        (basic-save-buffer)

        ;; Now check that a project task can get the CATEGORY via lookup
        (goto-char (point-min))
        (search-forward "Task 1")
        (let ((project-ids (org-entry-get-multivalued-property (point) "ORG_GTD_PROJECT_IDS")))
          ;; Verify the task has project IDs
          (expect project-ids :not :to-be nil)
          ;; Look up project's CATEGORY via the new helper function
          (let ((category (org-gtd-agenda-get-category-for-task)))
            (expect category :to-equal "Work")))))))

;;; property-inheritance-removal-test.el ends here
