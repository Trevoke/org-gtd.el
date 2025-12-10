;;; reflect-test.el --- Unit tests for GTD reflect implementation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-reflect implementation.
;; These are pure unit tests that don't require filesystem setup.
;;
;; Migrated from test/org-gtd-reflect-test.el (buttercup).
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-reflect)
(require 'org-gtd-view-language)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; GTD View Specifications

(deftest reflect/missed-engagements-view-specs-defined ()
  "Defines the correct number of view specifications."
  (assert-true org-gtd-reflect-missed-engagements-view-specs)
  (assert-equal 4 (length org-gtd-reflect-missed-engagements-view-specs)))

(deftest reflect/translate-delegated-spec-to-org-ql ()
  "Can translate delegated view specification to org-ql."
  (let* ((delegated-spec (car org-gtd-reflect-missed-engagements-view-specs))
         (query (org-gtd-view-lang--translate-to-org-ql delegated-spec)))
    (assert-equal `(and (property "ORG_GTD" "Delegated")
                        (todo ,(org-gtd-keywords--wait))
                        (property-ts< "ORG_GTD_TIMESTAMP" "today")
                        (not (done)))
                  query)))

(deftest reflect/translate-calendar-spec-to-org-ql ()
  "Can translate calendar view specification to org-ql."
  (let* ((calendar-spec (cadr org-gtd-reflect-missed-engagements-view-specs))
         (query (org-gtd-view-lang--translate-to-org-ql calendar-spec)))
    (assert-equal '(and (property "ORG_GTD" "Calendar")
                        (property-ts< "ORG_GTD_TIMESTAMP" "today")
                        (not (done)))
                  query)))

(deftest reflect/translate-project-deadline-spec-to-org-ql ()
  "Can translate project deadline view specification to org-ql."
  (let* ((deadline-spec (caddr org-gtd-reflect-missed-engagements-view-specs))
         (query (org-gtd-view-lang--translate-to-org-ql deadline-spec)))
    (assert-equal '(and (property "ORG_GTD" "Projects")
                        (deadline :to "today")
                        (not (done)))
                  query)))

(deftest reflect/translate-project-scheduled-spec-to-org-ql ()
  "Can translate project scheduled view specification to org-ql."
  (let* ((scheduled-spec (cadddr org-gtd-reflect-missed-engagements-view-specs))
         (query (org-gtd-view-lang--translate-to-org-ql scheduled-spec)))
    (assert-equal '(and (property "ORG_GTD" "Projects")
                        (scheduled :to "today")
                        (not (property "STYLE" "habit"))
                        (not (done)))
                  query)))

;;; Agenda Block Generation

(deftest reflect/creates-native-agenda-blocks ()
  "Can create native agenda blocks from type filters."
  (let* ((delegated-spec (car org-gtd-reflect-missed-engagements-view-specs))
         (block (org-gtd-view-lang--create-agenda-block delegated-spec))
         (settings (caddr block)))
    ;; Delegated uses tags-todo (has specific TODO=WAIT keyword)
    (assert-equal 'tags-todo (car block))
    (assert-equal "Missed check-ins on delegated items"
                  (cadr (assoc 'org-agenda-overriding-header settings)))))

(deftest reflect/creates-custom-commands-structure ()
  "Can create custom commands structure."
  (let ((commands (org-gtd-view-lang--create-custom-commands
                   org-gtd-reflect-missed-engagements-view-specs
                   "o"
                   "GTD Missed Engagements Reflection")))
    (assert-equal "o" (car (car commands)))
    (assert-equal "GTD Missed Engagements Reflection" (cadr (car commands)))
    (assert-equal 4 (length (caddr (car commands))))))

;;; Function Availability

(deftest reflect/main-function-exists ()
  "Provides the main org-gtd-reflect-missed-engagements function."
  (assert-true (fboundp 'org-gtd-reflect-missed-engagements)))

(deftest reflect/specialized-functions-exist ()
  "Provides specialized reflect functions."
  (assert-true (fboundp 'org-gtd-reflect-missed-delegated))
  (assert-true (fboundp 'org-gtd-reflect-missed-calendar))
  (assert-true (fboundp 'org-gtd-reflect-missed-projects))
  (assert-true (fboundp 'org-gtd-reflect-missed-with-custom)))

;;; Backward Compatibility

(deftest reflect/oops-alias-exists ()
  "Provides org-gtd-oops as an alias."
  (assert-true (fboundp 'org-gtd-oops)))

(deftest reflect/oops-delegated-alias-exists ()
  "Provides org-gtd-oops-delegated as an alias."
  (assert-true (fboundp 'org-gtd-oops-delegated)))

(deftest reflect/oops-calendar-alias-exists ()
  "Provides org-gtd-oops-calendar as an alias."
  (assert-true (fboundp 'org-gtd-oops-calendar)))

(deftest reflect/oops-projects-alias-exists ()
  "Provides org-gtd-oops-projects as an alias."
  (assert-true (fboundp 'org-gtd-oops-projects)))

(deftest reflect/oops-with-custom-alias-exists ()
  "Provides org-gtd-oops-with-custom as an alias."
  (assert-true (fboundp 'org-gtd-oops-with-custom)))

(deftest reflect/oops-custom-views-variable-exists ()
  "Provides org-gtd-oops-custom-views as a variable alias."
  (assert-true (boundp 'org-gtd-oops-custom-views)))

(deftest reflect/oops-view-specs-variable-exists ()
  "org-gtd-oops-view-specs aliases to org-gtd-reflect-missed-view-specs."
  (assert-true (boundp 'org-gtd-oops-view-specs)))

(deftest reflect/review-obsolete-aliases-exist ()
  "Provides org-gtd-review-* as obsolete aliases."
  (assert-true (fboundp 'org-gtd-review-missed-engagements))
  (assert-true (fboundp 'org-gtd-review-missed-delegated))
  (assert-true (fboundp 'org-gtd-review-stuck-projects)))

;;; Upcoming Delegated

(deftest reflect/upcoming-delegated-view-spec-defined ()
  "Defines the upcoming delegated view specification."
  (assert-true org-gtd-reflect-upcoming-delegated-view-spec)
  (assert-equal "Upcoming check-ins on delegated items"
                (alist-get 'name org-gtd-reflect-upcoming-delegated-view-spec)))

(deftest reflect/translate-upcoming-delegated-spec-to-org-ql ()
  "Can translate upcoming delegated view specification to org-ql."
  (let* ((upcoming-spec org-gtd-reflect-upcoming-delegated-view-spec)
         (query (org-gtd-view-lang--translate-to-org-ql upcoming-spec)))
    (assert-equal `(and (property "ORG_GTD" "Delegated")
                        (todo ,(org-gtd-keywords--wait))
                        (property-ts> "ORG_GTD_TIMESTAMP" "today"))
                  query)))

(deftest reflect/upcoming-delegated-function-exists ()
  "Provides the org-gtd-reflect-upcoming-delegated function."
  (assert-true (fboundp 'org-gtd-reflect-upcoming-delegated)))

;;; Stuck Single Actions

(deftest reflect/stuck-single-action-view-spec ()
  "Can translate stuck-single-action view specification to org-ql.
Stuck single actions are undone items with ORG_GTD=Actions but not in NEXT state."
  (let* ((stuck-spec '((name . "Stuck Single Actions")
                       (type . stuck-single-action)))
         (query (org-gtd-view-lang--translate-to-org-ql stuck-spec)))
    ;; Should match Actions that are not in NEXT state and not done
    (assert-equal `(and (property "ORG_GTD" "Actions")
                        (not (todo ,(org-gtd-keywords--next)))
                        (not (done)))
                  query)))

(deftest reflect/stuck-single-action-function-exists ()
  "Provides the org-gtd-reflect-stuck-single-action-items function."
  (assert-true (fboundp 'org-gtd-reflect-stuck-single-action-items)))

(provide 'reflect-test)

;;; reflect-test.el ends here
