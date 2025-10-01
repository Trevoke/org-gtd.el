;;; org-gtd-oops-new-test.el --- Tests for the new GTD view language-based oops implementation -*- lexical-binding: t; -*-

(require 'buttercup)
(require 'org-gtd-oops)
(require 'org-gtd-view-language)

(describe "New org-gtd-oops Implementation"

  (describe "GTD View Specifications"

    (it "defines the correct view specifications"
      (expect org-gtd-oops-view-specs)
      (expect (length org-gtd-oops-view-specs) :to-equal 4))

    (it "can translate delegated view specification to org-ql"
      (let* ((delegated-spec (car org-gtd-oops-view-specs))
             (query (org-gtd-view-lang--translate-to-org-ql delegated-spec)))
        (expect query :to-equal '(and (property "DELEGATED_TO")
                                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                                      (not (done))))))

    (it "can translate calendar view specification to org-ql"
      (let* ((calendar-spec (cadr org-gtd-oops-view-specs))
             (query (org-gtd-view-lang--translate-to-org-ql calendar-spec)))
        (expect query :to-equal '(and (property "ORG_GTD" "Calendar")
                                      (level 2)
                                      (property-ts< "ORG_GTD_TIMESTAMP" "today")
                                      (not (done))))))

    (it "can translate project deadline view specification to org-ql"
      (let* ((deadline-spec (caddr org-gtd-oops-view-specs))
             (query (org-gtd-view-lang--translate-to-org-ql deadline-spec)))
        (expect query :to-equal '(and (property "ORG_GTD" "Projects")
                                      (level 2)
                                      (deadline :to "today")
                                      (not (done))))))

    (it "can translate project scheduled view specification to org-ql"
      (let* ((scheduled-spec (cadddr org-gtd-oops-view-specs))
             (query (org-gtd-view-lang--translate-to-org-ql scheduled-spec)))
        (expect query :to-equal '(and (property "ORG_GTD" "Projects")
                                      (level 2)
                                      (scheduled :to "today")
                                      (not (property "STYLE" "habit"))
                                      (not (done)))))))

  (describe "Agenda Block Generation"

    (it "can create org-ql agenda blocks"
      (let* ((delegated-spec (car org-gtd-oops-view-specs))
             (block (org-gtd-view-lang--create-agenda-block delegated-spec)))
        (expect (car block) :to-equal 'org-ql-block)
        (expect (car (caadr (cdr block))) :to-equal 'org-ql-block-header)
        (expect (cadr (caadr (cdr block))) :to-equal "Missed check-ins on delegated items")))

    (it "can create custom commands structure"
      (let ((commands (org-gtd-view-lang--create-custom-commands
                       org-gtd-oops-view-specs
                       "o"
                       "GTD Oops Views")))
        (expect (car (car commands)) :to-equal "o")
        (expect (cadr (car commands)) :to-equal "GTD Oops Views")
        (expect (length (caddr (car commands))) :to-equal 4))))

  (describe "Function Availability"

    (it "provides the main org-gtd-oops function"
      (expect (fboundp 'org-gtd-oops) :to-be t))

    (it "provides specialized oops functions"
      (expect (fboundp 'org-gtd-oops-delegated) :to-be t)
      (expect (fboundp 'org-gtd-oops-calendar) :to-be t)
      (expect (fboundp 'org-gtd-oops-projects) :to-be t)
      (expect (fboundp 'org-gtd-oops-with-custom) :to-be t))))

;;; org-gtd-oops-new-test.el ends here