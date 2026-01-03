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

(deftest reflect/upcoming-delegated-function-exists ()
  "Provides the org-gtd-reflect-upcoming-delegated function."
  (assert-true (fboundp 'org-gtd-reflect-upcoming-delegated)))

;;; Stuck Single Actions

(deftest reflect/stuck-single-action-function-exists ()
  "Provides the org-gtd-reflect-stuck-single-action-items function."
  (assert-true (fboundp 'org-gtd-reflect-stuck-single-action-items)))

(provide 'reflect-test)

;;; reflect-test.el ends here
