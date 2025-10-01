;; -*- lexical-binding: t; coding: utf-8 -*-

;; Load test helpers
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "GTD View Language Translation"

 (before-each
  (setq inhibit-message t)
  (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "GTD View Language Specification"

  (it "can define a simple GTD view for delegated items with past timestamps"
      (let ((gtd-view-spec
             '((name . "Missed Delegated Check-ins")
               (filters . ((category . delegated)
                           (timestamp . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "DELEGATED_TO")
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done))))))

  (it "can define a GTD view for calendar items with past timestamps"
      (let ((gtd-view-spec
             '((name . "Missed Appointments")
               (filters . ((category . calendar)
                           (level . 2)
                           (timestamp . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Calendar")
                      (level 2)
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done))))))

  (it "can define a GTD view for overdue deadlines"
      (let ((gtd-view-spec
             '((name . "Overdue Deadlines")
               (filters . ((deadline . past))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (deadline :to "today")
                      (not (done))))))

  (it "can define a GTD view for overdue scheduled items excluding habits"
      (let ((gtd-view-spec
             '((name . "Overdue Scheduled Items")
               (filters . ((scheduled . past)
                           (not-habit . t))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (scheduled :to "today")
                      (not (property "STYLE" "habit"))
                      (not (done)))))))

 (describe
  "GTD View Language Complex Scenarios"

  (it "can combine multiple time-based filters with category filters"
      (let ((gtd-view-spec
             '((name . "Complex View")
               (filters . ((category . projects)
                           (level . 2)
                           (deadline . past)
                           (scheduled . future))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "ORG_GTD" "Projects")
                      (level 2)
                      (deadline :to "today")
                      (scheduled :from "today")))))

  (it "can handle area-of-focus filtering"
      (let ((gtd-view-spec
             '((name . "Area Focus View")
               (filters . ((area-of-focus . "Work")
                           (todo . ("TODO" "NEXT")))))))
        (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                :to-equal
                '(and (property "CATEGORY" "Work")
                      (todo "TODO" "NEXT"))))))

 (describe
  "Error Handling and Validation"

  (it "rejects invalid filter specifications"
      (let ((invalid-gtd-view-spec
             '((name . "Invalid View")
               (filters . ((invalid-filter . "bad-value"))))))
        (expect (org-gtd-view-lang--translate-to-org-ql invalid-gtd-view-spec)
                :to-throw
                'error))))

 (describe
  "Integration with Existing System"

  (it "can generate equivalent org-ql query for current oops view patterns"
      ;; This tests that our new system can replicate existing functionality
      (let ((delegated-oops-spec
             '((name . "Missed check-ins on delegated items")
               (filters . ((category . delegated)
                           (timestamp . past))))))
        ;; The translated query should be functionally equivalent to:
        ;; tags "+DELEGATED_TO={.+}" with skip function for past timestamps
        (expect (org-gtd-view-lang--translate-to-org-ql delegated-oops-spec)
                :to-equal
                '(and (property "DELEGATED_TO")
                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                      (not (done)))))))

 (describe
  "Agenda-Specific View Features"

  (it "can detect agenda view type and create agenda block instead of org-ql block"
      ;; Unit test for view-type handling
      (let ((agenda-view-spec
             '((name . "Simple Agenda View")
               (view-type . agenda))))
        (expect (org-gtd-view-lang--create-agenda-block agenda-view-spec)
                :to-equal
                '(agenda ""
                         ((org-agenda-span 1)
                          (org-agenda-start-day nil)
                          (org-agenda-skip-additional-timestamps-same-entry t))))))

  (it "can define a daily agenda view with span and NEXT actions"
      ;; This tests the org-gtd-engage equivalent functionality
      (let ((engage-view-spec
             '((name . "Today's GTD Engage View")
               (view-type . agenda)
               (agenda-span . 1)
               (show-habits . nil)
               (additional-blocks . ((todo . "NEXT"))))))
        (expect (org-gtd-view-lang--create-agenda-block engage-view-spec)
                :to-equal
                '(agenda ""
                         ((org-agenda-include-all-todo nil)
                          (org-agenda-span 1)
                          (org-agenda-start-day nil)
                          (org-agenda-skip-additional-timestamps-same-entry t))))

        ;; Should also generate the NEXT block
        (expect (org-gtd-view-lang--create-additional-blocks engage-view-spec)
                :to-equal
                '((todo "NEXT"
                        ((org-agenda-overriding-header "All actions ready to be executed.")))))))

  (describe
   "Tag Filtering Features"

   (it "can filter items by specific tags"
       ;; Unit test for tag filtering
       (let ((tag-view-spec
              '((name . "Context View")
                (filters . ((tags . ("@work" "@computer"))
                            (todo . ("NEXT")))))))
         (expect (org-gtd-view-lang--translate-to-org-ql tag-view-spec)
                 :to-equal
                 '(and (tags "@work" "@computer")
                       (todo "NEXT")))))

   (it "can filter items by tag patterns using tags-match"
       ;; Unit test for tag pattern matching
       (let ((tag-match-spec
              '((name . "Context Pattern View")
                (filters . ((tags-match . "{^@}")
                            (todo . ("NEXT")))))))
         (expect (org-gtd-view-lang--translate-to-org-ql tag-match-spec)
                 :to-equal
                 '(and (tags "{^@}")
                       (todo "NEXT")))))

   (it "can create simple grouped views by pre-defined contexts"
       ;; Unit test for basic grouping functionality
       (let ((grouped-spec
              '((name . "Actions by Context")
                (view-type . tags-grouped)
                (group-contexts . ("@work" "@home"))
                (filters . ((todo . ("NEXT")))))))
         (expect (org-gtd-view-lang--create-grouped-views grouped-spec)
                 :to-equal
                 '((tags "+@work+TODO=\"NEXT\""
                         ((org-agenda-overriding-header "@work")))
                   (tags "+@home+TODO=\"NEXT\""
                         ((org-agenda-overriding-header "@home")))))))

   (it "can create grouped context views for org-gtd-engage-grouped-by-context"
       ;; Acceptance test for grouped context functionality
       ;; Set up test agenda files with context tags
       (let ((test-file (make-temp-file "test-agenda" nil ".org")))
         (with-temp-file test-file
           (insert "* NEXT Task at work :@work:\n")
           (insert "* NEXT Task at home :@home:\n"))
         (setq org-agenda-files (list test-file))

         (let ((grouped-context-spec
                '((name . "Actions by Context")
                  (view-type . tags-grouped)
                  (group-by . context)
                  (filters . ((tags-match . "{^@}")
                              (todo . ("NEXT")))))))
           (expect (org-gtd-view-lang--create-grouped-views grouped-context-spec)
                   :to-equal
                   '(("@work" . ((tags "+@work+TODO=\"NEXT\""
                                       ((org-agenda-overriding-header "@work")))))
                     ("@home" . ((tags "+@home+TODO=\"NEXT\""
                                       ((org-agenda-overriding-header "@home"))))))))

         ;; Clean up
         (delete-file test-file)
         (setq org-agenda-files nil))))))
