;; -*- lexical-binding: t; coding: utf-8 -*-

;; Load test helpers
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe "GTD View Language Translation"

  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  (describe "GTD View Language Specification"

    (it "can define a simple GTD view for delegated items with past timestamps"
        (let ((gtd-view-spec
               '((name . "Missed Delegated Check-ins")
                 (filters . ((category . delegated)
                            (timestamp . past))))))
          (expect (org-gtd-view-lang--translate-to-org-ql gtd-view-spec)
                  :to-equal
                  '(and (property "DELEGATED_TO" ".+")
                        (property-ts< "ORG_GTD_TIMESTAMP" "today")))))

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
                        (property-ts< "ORG_GTD_TIMESTAMP" "today")))))

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

  (describe "GTD View Language Complex Scenarios"

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

  (describe "Error Handling and Validation"

    (it "rejects invalid filter specifications"
        (let ((invalid-gtd-view-spec
               '((name . "Invalid View")
                 (filters . ((invalid-filter . "bad-value"))))))
          (expect (org-gtd-view-lang--translate-to-org-ql invalid-gtd-view-spec)
                  :to-throw
                  'error))))

  (describe "Integration with Existing System"

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
                  '(and (property "DELEGATED_TO" ".+")
                        (property-ts< "ORG_GTD_TIMESTAMP" "today")))))))