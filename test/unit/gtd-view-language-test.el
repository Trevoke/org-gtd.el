;;; gtd-view-language-test.el --- Tests for GTD view language -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Pure unit tests for org-gtd view language translation functions.
;; These tests verify the translation from GTD view specs to org-ql queries.
;;
;; Migrated from test/gtd-view-language-test.el (buttercup).
;; Integration tests that require file system are in a separate file.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd)
(require 'org-gtd-view-language)

;;; GTD View Language Specification - Native Tests Only

;;; Agenda-Specific View Features

(deftest view-lang/agenda-block-creation ()
  "Creates agenda block from view-type agenda."
  (let ((agenda-view-spec
         '((name . "Simple Agenda View")
           (view-type . agenda))))
    (assert-equal
     '(agenda ""
              ((org-agenda-span 1)
               (org-agenda-start-day nil)
               (org-agenda-skip-additional-timestamps-same-entry t)
               (org-agenda-skip-function 'org-gtd-skip-unless-in-progress)))
     (org-gtd-view-lang--create-agenda-block agenda-view-spec))))

(deftest view-lang/daily-agenda-with-next ()
  "Creates daily agenda view with span and NEXT actions."
  (let ((engage-view-spec
         '((name . "Today's GTD Engage View")
           (view-type . agenda)
           (agenda-span . 1)
           (show-habits . nil)
           (additional-blocks . ((todo . "NEXT"))))))
    (assert-equal
     '(agenda ""
              ((org-agenda-include-all-todo nil)
               (org-agenda-span 1)
               (org-agenda-start-day nil)
               (org-agenda-skip-additional-timestamps-same-entry t)
               (org-agenda-skip-function 'org-gtd-skip-unless-in-progress)))
     (org-gtd-view-lang--create-agenda-block engage-view-spec))
    (assert-equal
     '((todo "NEXT"
             ((org-agenda-overriding-header "All actions ready to be executed."))))
     (org-gtd-view-lang--create-additional-blocks engage-view-spec))))

;;; Tag Filtering Features

(deftest view-lang/grouped-views-by-context ()
  "Creates simple grouped views by pre-defined contexts."
  (let ((grouped-spec
         '((name . "Actions by Context")
           (view-type . tags-grouped)
           (group-contexts . ("@work" "@home"))
           (filters . ((todo . ("NEXT")))))))
    (assert-equal
     '((tags "+@work+TODO=\"NEXT\""
             ((org-agenda-overriding-header "@work")))
       (tags "+@home+TODO=\"NEXT\""
             ((org-agenda-overriding-header "@home"))))
     (org-gtd-view-lang--create-grouped-views grouped-spec))))

;;; Multi-Block View Support

(deftest view-lang/multi-block-creates-blocks ()
  "Creates agenda blocks from multi-block view spec."
  (let ((multi-block-spec
         '((name . "Multi View")
           (blocks . (((name . "Block 1")
                       (type . next-action))
                      ((name . "Block 2")
                       (type . delegated)))))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list multi-block-spec))))
      (assert-equal 2 (length (caddr (car commands)))))))

(deftest view-lang/multi-block-separate-processing ()
  "Processes each block separately in multi-block spec."
  (let ((multi-block-spec
         '((name . "Multi View")
           (blocks . (((name . "Next Actions Block")
                       (type . next-action))
                      ((name . "Delegated Block")
                       (type . delegated)))))))
    (let* ((commands (org-gtd-view-lang--create-custom-commands (list multi-block-spec)))
           (blocks (caddr (car commands)))
           (first-block-settings (caddr (car blocks)))
           (second-block-settings (caddr (cadr blocks))))
      ;; Native blocks use org-agenda-overriding-header
      (assert-equal "Next Actions Block"
                    (cadr (assoc 'org-agenda-overriding-header first-block-settings)))
      (assert-equal "Delegated Block"
                    (cadr (assoc 'org-agenda-overriding-header second-block-settings))))))

(deftest view-lang/single-block-without-blocks-key ()
  "Still supports single-block specs without blocks key."
  (let ((single-block-spec
         '((name . "Single View")
           (type . next-action))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list single-block-spec))))
      (assert-equal 1 (length (caddr (car commands)))))))

;;; Calendar-Day Block Type

(deftest view-lang/calendar-day-creates-agenda-block ()
  "Creates a native agenda block with calendar-day block-type."
  (let ((calendar-day-spec
         '((name . "Today's Calendar")
           (block-type . calendar-day))))
    (let ((block (org-gtd-view-lang--create-agenda-block calendar-day-spec)))
      (assert-equal 'agenda (car block))
      (let ((settings (caddr block)))
        (assert-true (assoc 'org-agenda-skip-function settings))))))

(deftest view-lang/calendar-day-skip-function ()
  "Includes skip function for Calendar and Habit items."
  (let ((calendar-day-spec
         '((name . "Today's Calendar")
           (block-type . calendar-day))))
    (let* ((block (org-gtd-view-lang--create-agenda-block calendar-day-spec))
           (settings (caddr block))
           (skip-fn (cadr (assoc 'org-agenda-skip-function settings))))
      (assert-equal ''org-gtd-view-lang--skip-unless-calendar-or-habit skip-fn))))

(deftest view-lang/calendar-day-time-grid ()
  "Includes time grid settings in calendar-day block."
  (let ((calendar-day-spec
         '((name . "Today's Calendar")
           (block-type . calendar-day))))
    (let* ((block (org-gtd-view-lang--create-agenda-block calendar-day-spec))
           (settings (caddr block)))
      (assert-equal 1 (cadr (assoc 'org-agenda-span settings))))))

;;; Native Block Translation Tests
;;
;; Tests for the native org-agenda block translation layer that replaces org-ql.

(deftest view-lang/build-match-string-next-action ()
  "Builds match string for next-action type with TODO filter."
  (let ((view-spec '((type . next-action))))
    (assert-equal
     (format "LEVEL>0+ORG_GTD=\"Actions\"/%s" (org-gtd-keywords--next))
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-delegated ()
  "Builds match string for delegated type with TODO filter."
  (let ((view-spec '((type . delegated))))
    (assert-equal
     (format "LEVEL>0+ORG_GTD=\"Delegated\"/%s" (org-gtd-keywords--wait))
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-calendar ()
  "Builds match string for calendar type (property only, no TODO filter)."
  (let ((view-spec '((type . calendar))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Calendar\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-tickler ()
  "Builds match string for tickler type (property only, no TODO filter)."
  (let ((view-spec '((type . tickler))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Tickler\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-project ()
  "Builds match string for project type (property only, no TODO filter)."
  (let ((view-spec '((type . project))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Projects\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-someday ()
  "Builds match string for someday type (property only, no TODO filter)."
  (let ((view-spec '((type . someday))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Someday\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-habit ()
  "Builds match string for habit type (property only, no TODO filter)."
  (let ((view-spec '((type . habit))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Habit\""
     (org-gtd-view-lang--build-match-string view-spec))))

;;; Skip Function Builder Tests

(deftest view-lang/build-skip-function-returns-function ()
  "Returns a function that can be used as skip function."
  (let ((view-spec '((type . delegated)
                     (when . past))))
    (let ((skip-fn (org-gtd-view-lang--build-skip-function view-spec)))
      (assert-true (functionp skip-fn)))))

(deftest view-lang/build-skip-function-skips-wrong-type ()
  "Skip function skips items with wrong ORG_GTD type."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Should return a number (end of subtree) to skip
      (assert-true (numberp result)))))

(deftest view-lang/build-skip-function-includes-matching-type ()
  "Skip function includes items with matching ORG_GTD type."
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Should return nil to include
      (assert-nil result))))

(deftest view-lang/build-skip-function-handles-when-past ()
  "Skip function handles when=past filter."
  (with-temp-buffer
    (org-mode)
    ;; Future timestamp should be skipped for when=past
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2099-01-01 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated)
                        (when . past)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Future timestamp should be skipped
      (assert-true (numberp result)))))

(deftest view-lang/build-skip-function-includes-past-timestamp ()
  "Skip function includes items with past timestamps for when=past."
  (with-temp-buffer
    (org-mode)
    ;; Past timestamp should be included for when=past
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2020-01-01 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated)
                        (when . past)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Past timestamp should be included
      (assert-nil result))))

(deftest view-lang/skip-function-when-without-type-errors ()
  "Skip function errors when 'when' is used without 'type'."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function '((when . past)))))

(deftest view-lang/skip-function-when-unsupported-type-errors ()
  "Skip function errors when 'when' is used with unsupported type."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function '((type . next-action) (when . past)))))

(deftest view-lang/skip-function-when-today-includes-match ()
  "Skip function includes items with today timestamp for when=today."
  (with-temp-buffer
    (org-mode)
    ;; Today's timestamp should be included for when=today
    (insert (format "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <%s>\n:END:\n"
                    (format-time-string "%Y-%m-%d %a")))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated)
                        (when . today)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Today's timestamp should be included
      (assert-nil result))))

(deftest view-lang/skip-function-when-future-includes-match ()
  "Skip function includes items with future timestamp for when=future."
  (with-temp-buffer
    (org-mode)
    ;; Future timestamp should be included for when=future
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2099-01-01 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . delegated)
                        (when . future)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Future timestamp should be included
      (assert-nil result))))

(deftest view-lang/skip-function-includes-matching-area-of-focus ()
  "Skip function includes items matching area-of-focus filter."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:CATEGORY: Work\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (area-of-focus . "Work")))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Matching area-of-focus should be included
      (assert-nil result))))

(deftest view-lang/skip-function-excludes-non-matching-area-of-focus ()
  "Skip function excludes items with non-matching area-of-focus."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:CATEGORY: Personal\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (area-of-focus . "Work")))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Non-matching area-of-focus should be skipped
      (assert-true (numberp result)))))

(deftest view-lang/skip-function-excludes-missing-area-of-focus ()
  "Skip function excludes items without CATEGORY property."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (area-of-focus . "Work")))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Missing CATEGORY should be skipped
      (assert-true (numberp result)))))

;;; Native Block Translator Tests

(deftest view-lang/translate-to-native-block-uses-tags ()
  "Native block translator uses tags for types without specific TODO keywords."
  (let* ((view-spec '((name . "Test View")
                      (type . calendar)))
         (block (org-gtd-view-lang--translate-to-native-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/translate-to-native-block-includes-match-string ()
  "Native block includes match string."
  (let* ((view-spec '((name . "Test View")
                      (type . calendar)))
         (block (org-gtd-view-lang--translate-to-native-block view-spec)))
    (assert-equal "LEVEL>0+ORG_GTD=\"Calendar\""
                  (cadr block))))

(deftest view-lang/translate-to-native-block-includes-header ()
  "Native block includes header setting."
  (let* ((view-spec '((name . "My Test View")
                      (type . next-action)))
         (block (org-gtd-view-lang--translate-to-native-block view-spec))
         (settings (caddr block)))
    (assert-equal "My Test View"
                  (cadr (assoc 'org-agenda-overriding-header settings)))))

(deftest view-lang/translate-to-native-block-includes-skip-function ()
  "Native block includes skip function when filters need it."
  (let* ((view-spec '((name . "Delegated Past")
                      (type . delegated)
                      (when . past)))
         (block (org-gtd-view-lang--translate-to-native-block view-spec))
         (settings (caddr block)))
    (assert-true (assoc 'org-agenda-skip-function settings))))

(deftest view-lang/translate-to-native-block-with-prefix-format ()
  "Native block includes prefix format when provided."
  (let* ((view-spec '((name . "Test View")
                      (type . next-action)))
         (prefix-format " %i %-12:(org-gtd-agenda--prefix-format 12) ")
         (block (org-gtd-view-lang--translate-to-native-block view-spec prefix-format))
         (settings (caddr block))
         (prefix-setting (assoc 'org-agenda-prefix-format settings)))
    ;; The setting is: (org-agenda-prefix-format '((tags . FORMAT) (todo . FORMAT)))
    ;; So cadr is the quoted alist, and we need to eval or get the value from that quote
    (assert-true prefix-setting)
    (let ((format-alist (cadr (cadr prefix-setting))))  ; Get inside the quote
      (assert-equal prefix-format (cdr (assoc 'tags format-alist))))))

;;; Routing Tests - Verify types use native blocks

(deftest view-lang/create-agenda-block-routes-next-action-to-native ()
  "Routes type=next-action to native tags-todo block (has specific TODO keyword)."
  (let* ((view-spec '((name . "Next Actions")
                      (type . next-action)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags-todo (car block))))

(deftest view-lang/create-agenda-block-routes-delegated-to-native ()
  "Routes type=delegated to native tags-todo block (has specific TODO keyword)."
  (let* ((view-spec '((name . "Delegated")
                      (type . delegated)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags-todo (car block))))

(deftest view-lang/create-agenda-block-routes-calendar-to-native ()
  "Routes type=calendar to native tags block."
  (let* ((view-spec '((name . "Calendar")
                      (type . calendar)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-tickler-to-native ()
  "Routes type=tickler to native tags block."
  (let* ((view-spec '((name . "Tickler")
                      (type . tickler)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-native-includes-prefix-format ()
  "Native block includes inherited prefix format."
  (let* ((view-spec '((name . "Test")
                      (type . next-action)))
         (prefix-format " %i %-12:(test) ")
         (block (org-gtd-view-lang--create-agenda-block view-spec prefix-format))
         (settings (caddr block)))
    (assert-true (assoc 'org-agenda-prefix-format settings))))

;;; Complex Type Routing Tests

(deftest view-lang/create-agenda-block-routes-stuck-project-to-native ()
  "Routes type=stuck-project to native tags block."
  (let* ((view-spec '((name . "Stuck Projects")
                      (type . stuck-project)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-active-project-to-native ()
  "Routes type=active-project to native tags block."
  (let* ((view-spec '((name . "Active Projects")
                      (type . active-project)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-completed-project-to-native ()
  "Routes type=completed-project to native tags block."
  (let* ((view-spec '((name . "Completed Projects")
                      (type . completed-project)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-stuck-calendar-to-native ()
  "Routes type=stuck-calendar to native tags block."
  (let* ((view-spec '((name . "Stuck Calendar")
                      (type . stuck-calendar)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-stuck-delegated-to-native ()
  "Routes type=stuck-delegated to native tags block."
  (let* ((view-spec '((name . "Stuck Delegated")
                      (type . stuck-delegated)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-stuck-tickler-to-native ()
  "Routes type=stuck-tickler to native tags block."
  (let* ((view-spec '((name . "Stuck Tickler")
                      (type . stuck-tickler)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-stuck-habit-to-native ()
  "Routes type=stuck-habit to native tags block."
  (let* ((view-spec '((name . "Stuck Habit")
                      (type . stuck-habit)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-tickler-project-to-native ()
  "Routes type=tickler-project to native tags block."
  (let* ((view-spec '((name . "Tickler Projects")
                      (type . tickler-project)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

(deftest view-lang/create-agenda-block-routes-incubated-project-to-native ()
  "Routes type=incubated-project to native tags block."
  (let* ((view-spec '((name . "Incubated Projects")
                      (type . incubated-project)))
         (block (org-gtd-view-lang--create-agenda-block view-spec)))
    (assert-equal 'tags (car block))))

;;; Complex Type Match String Tests

(deftest view-lang/build-match-string-stuck-project ()
  "Builds match string for stuck-project type."
  (let ((view-spec '((type . stuck-project))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Projects\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-active-project ()
  "Builds match string for active-project type."
  (let ((view-spec '((type . active-project))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Projects\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-stuck-calendar ()
  "Builds match string for stuck-calendar type."
  (let ((view-spec '((type . stuck-calendar))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Calendar\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-stuck-delegated ()
  "Builds match string for stuck-delegated type."
  (let ((view-spec '((type . stuck-delegated))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Delegated\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-tickler-project ()
  "Builds match string for tickler-project type."
  (let ((view-spec '((type . tickler-project))))
    (assert-equal
     "LEVEL>0+ORG_GTD=\"Tickler\""
     (org-gtd-view-lang--build-match-string view-spec))))

(deftest view-lang/build-match-string-incubated-project ()
  "Builds match string for incubated-project type (requires OR in skip fn)."
  (let ((view-spec '((type . incubated-project))))
    ;; Match string can only do one property, skip function handles OR
    (assert-equal
     "LEVEL>0+ORG_GTD<>\"\""
     (org-gtd-view-lang--build-match-string view-spec))))

;;; Complex Type Skip Function Tests

(deftest view-lang/build-skip-function-stuck-project ()
  "Skip function for stuck-project uses project-is-stuck predicate."
  (let* ((view-spec '((type . stuck-project)))
         (skip-fn (org-gtd-view-lang--build-skip-function view-spec)))
    (assert-true (functionp skip-fn))))

(deftest view-lang/build-skip-function-active-project ()
  "Skip function for active-project uses project-has-active-tasks predicate."
  (let* ((view-spec '((type . active-project)))
         (skip-fn (org-gtd-view-lang--build-skip-function view-spec)))
    (assert-true (functionp skip-fn))))

(deftest view-lang/build-skip-function-stuck-calendar-invalid-ts ()
  "Skip function for stuck-calendar checks for invalid timestamp."
  (with-temp-buffer
    (org-mode)
    ;; Calendar item with valid timestamp should be skipped (not stuck)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Calendar\n:ORG_GTD_TIMESTAMP: <2025-01-15 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . stuck-calendar)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Valid timestamp = not stuck, should be skipped
      (assert-true (numberp result)))))

(deftest view-lang/build-skip-function-stuck-calendar-missing-ts ()
  "Skip function for stuck-calendar includes items with missing timestamp."
  (with-temp-buffer
    (org-mode)
    ;; Calendar item with NO timestamp should be included (is stuck)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . stuck-calendar)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Missing timestamp = stuck, should be included
      (assert-nil result))))

(deftest view-lang/build-skip-function-stuck-delegated-missing-who ()
  "Skip function for stuck-delegated includes items with missing DELEGATED_TO."
  (with-temp-buffer
    (org-mode)
    ;; Delegated item with timestamp but NO who should be included (is stuck)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2025-01-15 Wed>\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . stuck-delegated)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Missing who = stuck, should be included
      (assert-nil result))))

(deftest view-lang/build-skip-function-stuck-delegated-complete ()
  "Skip function for stuck-delegated skips items with both who and timestamp."
  (with-temp-buffer
    (org-mode)
    ;; Delegated item with BOTH timestamp and who should be skipped (not stuck)
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Delegated\n:ORG_GTD_TIMESTAMP: <2025-01-15 Wed>\n:DELEGATED_TO: Alice\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . stuck-delegated)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Has both = not stuck, should be skipped
      (assert-true (numberp result)))))

(deftest view-lang/build-skip-function-tickler-project ()
  "Skip function for tickler-project checks PREVIOUS_ORG_GTD property."
  (with-temp-buffer
    (org-mode)
    ;; Tickler item that was a project should be included
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Tickler\n:PREVIOUS_ORG_GTD: Projects\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . tickler-project)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Was a project, should be included
      (assert-nil result))))

(deftest view-lang/build-skip-function-tickler-project-wrong-previous ()
  "Skip function for tickler-project skips items that weren't projects."
  (with-temp-buffer
    (org-mode)
    ;; Tickler item that was NOT a project should be skipped
    (insert "* Test\n:PROPERTIES:\n:ORG_GTD: Tickler\n:PREVIOUS_ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . tickler-project)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Was not a project, should be skipped
      (assert-true (numberp result)))))

;;; Prefix DSL Tests

(deftest view-lang/expand-prefix-generates-format-string ()
  "The prefix key generates an org-agenda-prefix-format string."
  (let* ((view-spec '((name . "Test")
                      (type . next-action)
                      (prefix . (project area-of-focus))
                      (prefix-width . 12)))
         (block (org-gtd-view-lang--create-agenda-block view-spec))
         (settings (caddr block))
         (prefix-setting (assoc 'org-agenda-prefix-format settings)))
    ;; Should have a prefix format setting
    (assert-true prefix-setting)))

(deftest view-lang/expand-prefix-uses-default-width ()
  "The prefix key uses default width when prefix-width not specified."
  (let* ((view-spec '((name . "Test")
                      (type . next-action)
                      (prefix . (project area-of-focus))))
         (block (org-gtd-view-lang--create-agenda-block view-spec))
         (settings (caddr block)))
    ;; Should still generate prefix format with default width
    (assert-true (assoc 'org-agenda-prefix-format settings))))

(deftest view-lang/expand-prefix-inherits-to-blocks ()
  "The prefix key at top level inherits to child blocks."
  (let* ((view-spec '((name . "Multi Block View")
                      (prefix . (project "default"))
                      (prefix-width . 10)
                      (blocks . (((name . "Actions")
                                  (type . next-action))))))
         (commands (org-gtd-view-lang--create-custom-commands (list view-spec)))
         (first-command (car commands))
         (blocks (caddr first-command))
         (first-block (car blocks))
         (settings (caddr first-block)))
    ;; Child block should have inherited prefix format
    (assert-true (assoc 'org-agenda-prefix-format settings))))

(deftest view-lang/prefix-generates-resolve-chain-call ()
  "The prefix format string calls org-gtd-agenda--resolve-prefix-chain."
  (let* ((format-str (org-gtd-view-lang--expand-prefix '(project area-of-focus) 12)))
    ;; Format string should contain the resolver function call
    (assert-match "org-gtd-agenda--resolve-prefix-chain" format-str)))

(deftest view-lang/prefix-includes-width-in-format ()
  "The prefix format string includes the width parameter."
  (let* ((format-str (org-gtd-view-lang--expand-prefix '(project area-of-focus) 15)))
    ;; Format string should reference the width
    (assert-match "15" format-str)))

(deftest view-lang/prefix-quoted-elements-in-format ()
  "Literal strings in prefix are properly quoted in format."
  (let* ((format-str (org-gtd-view-lang--expand-prefix '(project "Incubated") 12)))
    ;; Format string should contain the quoted literal
    (assert-match "Incubated" format-str)))

;;; Implicit Blocks with Type Defaults Tests

(deftest view-lang/type-defaults-constant-exists ()
  "Type defaults constant exists and is an alist."
  (assert-true (boundp 'org-gtd-view-lang--type-defaults))
  (assert-true (listp org-gtd-view-lang--type-defaults)))

(deftest view-lang/type-defaults-has-calendar ()
  "Type defaults has calendar with when=today."
  (let ((calendar-defaults (alist-get 'calendar org-gtd-view-lang--type-defaults)))
    (assert-equal 'today (alist-get 'when calendar-defaults))
    (assert-equal "Calendar" (alist-get 'name calendar-defaults))))

(deftest view-lang/type-defaults-has-delegated ()
  "Type defaults has delegated with when=today."
  (let ((delegated-defaults (alist-get 'delegated org-gtd-view-lang--type-defaults)))
    (assert-equal 'today (alist-get 'when delegated-defaults))
    (assert-equal "Delegated" (alist-get 'name delegated-defaults))))

(deftest view-lang/type-defaults-has-tickler ()
  "Type defaults has tickler with when=today."
  (let ((tickler-defaults (alist-get 'tickler org-gtd-view-lang--type-defaults)))
    (assert-equal 'today (alist-get 'when tickler-defaults))
    (assert-equal "Tickler" (alist-get 'name tickler-defaults))))

(deftest view-lang/type-defaults-has-next-action ()
  "Type defaults has next-action with name only."
  (let ((next-action-defaults (alist-get 'next-action org-gtd-view-lang--type-defaults)))
    (assert-nil (alist-get 'when next-action-defaults))
    (assert-equal "Next Actions" (alist-get 'name next-action-defaults))))

(deftest view-lang/type-defaults-has-stuck-types ()
  "Type defaults has stuck types with (Needs Attention) suffix."
  (let ((stuck-calendar (alist-get 'stuck-calendar org-gtd-view-lang--type-defaults))
        (stuck-delegated (alist-get 'stuck-delegated org-gtd-view-lang--type-defaults))
        (stuck-project (alist-get 'stuck-project org-gtd-view-lang--type-defaults)))
    (assert-equal "Calendar (Needs Attention)" (alist-get 'name stuck-calendar))
    (assert-equal "Delegated (Needs Attention)" (alist-get 'name stuck-delegated))
    (assert-equal "Projects (Needs Attention)" (alist-get 'name stuck-project))))

(deftest view-lang/default-prefix-constant-exists ()
  "Default prefix constant exists."
  (assert-true (boundp 'org-gtd-view-lang--default-prefix))
  (assert-equal '(project area-of-focus file-name) org-gtd-view-lang--default-prefix))

;;; Extract Type Keys Tests

(deftest view-lang/extract-type-keys-single ()
  "Extracts single type key from spec."
  (let ((spec '((name . "Test") (type . calendar))))
    (assert-equal '(calendar) (org-gtd-view-lang--extract-type-keys spec))))

(deftest view-lang/extract-type-keys-multiple ()
  "Extracts multiple type keys preserving order."
  (let ((spec '((name . "Test") (type . calendar) (area-of-focus . "Health") (type . next-action))))
    (assert-equal '(calendar next-action) (org-gtd-view-lang--extract-type-keys spec))))

(deftest view-lang/extract-type-keys-none ()
  "Returns nil when no type keys present."
  (let ((spec '((name . "Test") (area-of-focus . "Health"))))
    (assert-nil (org-gtd-view-lang--extract-type-keys spec))))

;;; Extract Top-Level Keys Tests

(deftest view-lang/extract-top-level-keys-filters-reserved ()
  "Filters out reserved keys from top-level."
  (let ((spec '((name . "Test") (area-of-focus . "Health") (type . calendar) (blocks . ()))))
    (let ((result (org-gtd-view-lang--extract-top-level-keys spec)))
      (assert-nil (alist-get 'name result))
      (assert-nil (alist-get 'type result))
      (assert-nil (alist-get 'blocks result))
      (assert-equal "Health" (alist-get 'area-of-focus result)))))

(deftest view-lang/extract-top-level-keys-preserves-filters ()
  "Preserves filter keys like area-of-focus and when."
  (let ((spec '((name . "Test") (area-of-focus . "Work") (when . past))))
    (let ((result (org-gtd-view-lang--extract-top-level-keys spec)))
      (assert-equal "Work" (alist-get 'area-of-focus result))
      (assert-equal 'past (alist-get 'when result)))))

;;; Apply Defaults with Four-Tier Precedence Tests

(deftest view-lang/apply-defaults-block-explicit-wins ()
  "Block-explicit key takes highest precedence."
  (let ((block-spec '((type . calendar) (when . future)))
        (top-level '((when . past)))
        (type 'calendar))
    (let ((result (org-gtd-view-lang--apply-defaults block-spec type top-level)))
      ;; Block-explicit 'future should win over top-level 'past and type default 'today
      (assert-equal 'future (alist-get 'when result)))))

(deftest view-lang/apply-defaults-top-level-over-type-default ()
  "Top-level explicit beats type smart default."
  (let ((block-spec '((type . calendar)))  ; no when in block
        (top-level '((when . past)))
        (type 'calendar))
    (let ((result (org-gtd-view-lang--apply-defaults block-spec type top-level)))
      ;; Top-level 'past should win over type default 'today
      (assert-equal 'past (alist-get 'when result)))))

(deftest view-lang/apply-defaults-type-default-applied ()
  "Type smart default applied when no explicit override."
  (let ((block-spec '((type . calendar)))  ; no when
        (top-level '())                    ; no when
        (type 'calendar))
    (let ((result (org-gtd-view-lang--apply-defaults block-spec type top-level)))
      ;; Type default 'today should be applied
      (assert-equal 'today (alist-get 'when result)))))

(deftest view-lang/apply-defaults-adds-name-from-type ()
  "Adds default name from type when not specified."
  (let ((block-spec '((type . calendar)))
        (top-level '())
        (type 'calendar))
    (let ((result (org-gtd-view-lang--apply-defaults block-spec type top-level)))
      (assert-equal "Calendar" (alist-get 'name result)))))

(deftest view-lang/apply-defaults-preserves-area-of-focus ()
  "Inherits area-of-focus from top-level."
  (let ((block-spec '((type . calendar)))
        (top-level '((area-of-focus . "Health")))
        (type 'calendar))
    (let ((result (org-gtd-view-lang--apply-defaults block-spec type top-level)))
      (assert-equal "Health" (alist-get 'area-of-focus result)))))

;;; Expand Implicit Blocks Tests

(deftest view-lang/expand-implicit-blocks-multiple-types ()
  "Expands multiple type keys into blocks."
  (let ((spec '((name . "Test View")
                (type . calendar)
                (type . next-action))))
    (let ((result (org-gtd-view-lang--expand-implicit-blocks spec)))
      ;; Should have blocks key
      (assert-true (alist-get 'blocks result))
      ;; Should have two blocks
      (assert-equal 2 (length (alist-get 'blocks result))))))

(deftest view-lang/expand-implicit-blocks-inherits-area-of-focus ()
  "Top-level area-of-focus inherits to all blocks."
  (let ((spec '((name . "Health Review")
                (area-of-focus . "Health")
                (type . calendar)
                (type . next-action))))
    (let* ((result (org-gtd-view-lang--expand-implicit-blocks spec))
           (blocks (alist-get 'blocks result)))
      (assert-equal "Health" (alist-get 'area-of-focus (car blocks)))
      (assert-equal "Health" (alist-get 'area-of-focus (cadr blocks))))))

(deftest view-lang/expand-implicit-blocks-applies-type-defaults ()
  "Applies type-specific defaults to each block."
  (let ((spec '((name . "Test")
                (type . calendar)
                (type . next-action))))
    (let* ((result (org-gtd-view-lang--expand-implicit-blocks spec))
           (blocks (alist-get 'blocks result))
           (calendar-block (car blocks))
           (next-action-block (cadr blocks)))
      ;; Calendar should get when=today default
      (assert-equal 'today (alist-get 'when calendar-block))
      ;; Next-action should NOT have when
      (assert-nil (alist-get 'when next-action-block)))))

(deftest view-lang/expand-implicit-blocks-skips-when-blocks-present ()
  "Does not expand when explicit blocks key is present."
  (let ((spec '((name . "Test")
                (type . calendar)
                (blocks . (((name . "Explicit") (type . next-action)))))))
    (let ((result (org-gtd-view-lang--expand-implicit-blocks spec)))
      ;; Should keep explicit blocks unchanged
      (assert-equal 1 (length (alist-get 'blocks result)))
      (assert-equal "Explicit" (alist-get 'name (car (alist-get 'blocks result)))))))

(deftest view-lang/expand-implicit-blocks-single-type-no-expansion ()
  "Single type key does not create blocks."
  (let ((spec '((name . "Test") (type . calendar))))
    (let ((result (org-gtd-view-lang--expand-implicit-blocks spec)))
      ;; Should NOT have blocks key for single type
      (assert-nil (alist-get 'blocks result))
      ;; Should keep type in place
      (assert-equal 'calendar (alist-get 'type result)))))

(deftest view-lang/expand-implicit-blocks-top-level-when-overrides-default ()
  "Top-level when overrides type smart default."
  (let ((spec '((name . "Test")
                (when . past)
                (type . calendar)      ; default when=today
                (type . delegated))))  ; default when=today
    (let* ((result (org-gtd-view-lang--expand-implicit-blocks spec))
           (blocks (alist-get 'blocks result)))
      ;; Both blocks should have when=past (from top-level)
      (assert-equal 'past (alist-get 'when (car blocks)))
      (assert-equal 'past (alist-get 'when (cadr blocks))))))

(deftest view-lang/expand-implicit-blocks-adds-default-prefix ()
  "Adds default prefix when none specified."
  (let ((spec '((name . "Test")
                (type . calendar)
                (type . next-action))))
    (let ((result (org-gtd-view-lang--expand-implicit-blocks spec)))
      ;; Should add default prefix at top level
      (assert-equal '(project area-of-focus file-name) (alist-get 'prefix result)))))

(deftest view-lang/expand-implicit-blocks-preserves-explicit-prefix ()
  "Preserves explicitly specified prefix."
  (let ((spec '((name . "Test")
                (prefix . (project "Custom"))
                (type . calendar)
                (type . next-action))))
    (let ((result (org-gtd-view-lang--expand-implicit-blocks spec)))
      ;; Should keep explicit prefix
      (assert-equal '(project "Custom") (alist-get 'prefix result)))))

;;; Priority Skip Predicate Tests

(deftest view-lang/skip-priority-matches-single ()
  "Skip predicate includes items with matching priority."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-priority ?A)
    (let* ((view-spec '((type . next-action)
                        (priority . A)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Should include (return nil)
      (assert-nil result))))

(deftest view-lang/skip-priority-skips-non-matching ()
  "Skip predicate skips items with non-matching priority."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-priority ?C)
    (let* ((view-spec '((type . next-action)
                        (priority . A)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Should skip (return number)
      (assert-true (numberp result)))))

(deftest view-lang/skip-priority-nil-matches-missing ()
  "Skip predicate with priority=nil matches items without priority."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (priority . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; Should include (return nil)
      (assert-nil result))))

(deftest view-lang/skip-priority-comparison-gte ()
  "Skip predicate handles >= comparison correctly."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-priority ?A)
    (let* ((view-spec '((type . next-action)
                        (priority . (>= B))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; A >= B should match (A is higher priority than B)
      (assert-nil result))))

(deftest view-lang/skip-priority-list ()
  "Skip predicate handles list of priorities."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-priority ?B)
    (let* ((view-spec '((type . next-action)
                        (priority . (A B))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; B in (A B) should match
      (assert-nil result))))

;;; Effort Skip Predicate Tests

(deftest view-lang/skip-effort-less-than-includes ()
  "Skip predicate includes items with effort under threshold."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:Effort: 0:15\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (effort . (< "0:30"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-effort-less-than-skips ()
  "Skip predicate skips items with effort over threshold."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:Effort: 2:00\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (effort . (< "0:30"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-true (numberp result)))))

(deftest view-lang/skip-effort-nil-matches-missing ()
  "Skip predicate with effort=nil matches items without effort."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (effort . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      (assert-nil result))))

(deftest view-lang/skip-effort-parses-duration-formats ()
  "Skip predicate handles various duration formats."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:Effort: 1d2h30min\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (effort . (> "1:00"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; 1d2h30min > 1:00, should include
      (assert-nil result))))

;;; Clocked Skip Predicate Tests

(deftest view-lang/skip-clocked-nil-matches-zero ()
  "Skip predicate with clocked=nil matches items with zero clock time."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (clocked . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; No clock entries = zero time, should match
      (assert-nil result))))

(deftest view-lang/skip-clocked-greater-skips-zero ()
  "Skip predicate with clocked=(> \"0:30\") skips items with zero time."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Test\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((view-spec '((type . next-action)
                        (clocked . (> "0:30"))))
           (skip-fn (org-gtd-view-lang--build-skip-function view-spec))
           (result (funcall skip-fn)))
      ;; No clock = 0 time, should skip
      (assert-true (numberp result)))))

;;; Unknown Filter Key Error Handling Tests

(deftest view-lang/skip-function-errors-on-unknown-filter ()
  "Skip function errors on unknown filter keys."
  (assert-raises 'user-error
    (org-gtd-view-lang--build-skip-function
     '((type . next-action) (invalid-filter . "bad")))))

(provide 'gtd-view-language-test)

;;; gtd-view-language-test.el ends here
