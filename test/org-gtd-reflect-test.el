;;; org-gtd-reflect-test.el --- Tests for the GTD view language-based reflect implementation -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'org-gtd-reflect)
(require 'org-gtd-view-language)

(describe "org-gtd-reflect Missed Engagements Implementation"

  (describe "GTD View Specifications"

    (it "defines the correct view specifications"
      (expect org-gtd-reflect-missed-engagements-view-specs)
      (expect (length org-gtd-reflect-missed-engagements-view-specs) :to-equal 4))

    (it "can translate delegated view specification to org-ql"
      (let* ((delegated-spec (car org-gtd-reflect-missed-engagements-view-specs))
             (query (org-gtd-view-lang--translate-to-org-ql delegated-spec)))
        (expect query :to-equal '(and (property "DELEGATED_TO")
                                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                                      (not (done))))))

    (it "can translate calendar view specification to org-ql"
      (let* ((calendar-spec (cadr org-gtd-reflect-missed-engagements-view-specs))
             (query (org-gtd-view-lang--translate-to-org-ql calendar-spec)))
        (expect query :to-equal '(and (property "ORG_GTD" "Calendar")
                                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                                      (not (done))))))

    (it "can translate project deadline view specification to org-ql"
      (let* ((deadline-spec (caddr org-gtd-reflect-missed-engagements-view-specs))
             (query (org-gtd-view-lang--translate-to-org-ql deadline-spec)))
        (expect query :to-equal '(and (property "ORG_GTD" "Projects")
                                      (deadline :to "today")
                                      (not (done))))))

    (it "can translate project scheduled view specification to org-ql"
      (let* ((scheduled-spec (cadddr org-gtd-reflect-missed-engagements-view-specs))
             (query (org-gtd-view-lang--translate-to-org-ql scheduled-spec)))
        (expect query :to-equal '(and (property "ORG_GTD" "Projects")
                                      (scheduled :to "today")
                                      (not (property "STYLE" "habit"))
                                      (not (done)))))))

  (describe "Agenda Block Generation"

    (it "can create org-ql agenda blocks"
      (let* ((delegated-spec (car org-gtd-reflect-missed-engagements-view-specs))
             (block (org-gtd-view-lang--create-agenda-block delegated-spec)))
        (expect (car block) :to-equal 'org-ql-block)
        (expect (car (caadr (cdr block))) :to-equal 'org-ql-block-header)
        (expect (cadr (caadr (cdr block))) :to-equal "Missed check-ins on delegated items")))

    (it "can create custom commands structure"
      (let ((commands (org-gtd-view-lang--create-custom-commands
                       org-gtd-reflect-missed-engagements-view-specs
                       "o"
                       "GTD Missed Engagements Reflection")))
        (expect (car (car commands)) :to-equal "o")
        (expect (cadr (car commands)) :to-equal "GTD Missed Engagements Reflection")
        (expect (length (caddr (car commands))) :to-equal 4))))

  (describe "Function Availability"

    (it "provides the main org-gtd-reflect-missed-engagements function"
      (expect (fboundp 'org-gtd-reflect-missed-engagements) :to-be t))

    (it "provides specialized reflect functions"
      (expect (fboundp 'org-gtd-reflect-missed-delegated) :to-be t)
      (expect (fboundp 'org-gtd-reflect-missed-calendar) :to-be t)
      (expect (fboundp 'org-gtd-reflect-missed-projects) :to-be t)
      (expect (fboundp 'org-gtd-reflect-missed-with-custom) :to-be t)))

  (describe "Backward Compatibility"

    (it "provides org-gtd-oops as an alias"
      (expect (fboundp 'org-gtd-oops) :to-be t))

    (it "provides org-gtd-oops-delegated as an alias"
      (expect (fboundp 'org-gtd-oops-delegated) :to-be t))

    (it "provides org-gtd-oops-calendar as an alias"
      (expect (fboundp 'org-gtd-oops-calendar) :to-be t))

    (it "provides org-gtd-oops-projects as an alias"
      (expect (fboundp 'org-gtd-oops-projects) :to-be t))

    (it "provides org-gtd-oops-with-custom as an alias"
      (expect (fboundp 'org-gtd-oops-with-custom) :to-be t))

    (it "provides org-gtd-oops-custom-views as a variable alias"
      (expect (boundp 'org-gtd-oops-custom-views) :to-be t))

    (it "org-gtd-oops-view-specs aliases to org-gtd-reflect-missed-view-specs"
      (expect (boundp 'org-gtd-oops-view-specs) :to-be t))

    (it "provides org-gtd-review-* as obsolete aliases"
      (expect (fboundp 'org-gtd-review-missed-engagements) :to-be t)
      (expect (fboundp 'org-gtd-review-missed-delegated) :to-be t)
      (expect (fboundp 'org-gtd-review-stuck-projects) :to-be t))))

(describe "org-gtd-reflect Upcoming Delegated Implementation"

  (describe "GTD View Specification"

    (it "defines the upcoming delegated view specification"
      (expect org-gtd-reflect-upcoming-delegated-view-spec)
      (expect (alist-get 'name org-gtd-reflect-upcoming-delegated-view-spec)
              :to-equal "Upcoming check-ins on delegated items"))

    (it "can translate upcoming delegated view specification to org-ql"
      (let* ((upcoming-spec org-gtd-reflect-upcoming-delegated-view-spec)
             (query (org-gtd-view-lang--translate-to-org-ql upcoming-spec)))
        (expect query :to-equal '(and (property "DELEGATED_TO")
                                      (property-ts> "ORG_GTD_TIMESTAMP" "today")
                                      (not (done)))))))

  (describe "Function Availability"

    (it "provides the org-gtd-reflect-upcoming-delegated function"
      (expect (fboundp 'org-gtd-reflect-upcoming-delegated) :to-be t))))

;;; org-gtd-reflect-test.el ends here
