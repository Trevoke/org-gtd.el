;;; view-filters-test.el --- Integration tests for view filters -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Integration tests for priority, effort, and clocked view filters.
;; These tests verify the complete view creation flow end-to-end.
;;
;; Tests verify:
;; 1. Priority filter works in view creation
;; 2. Effort filter works in view creation
;; 3. Clocked filter works in view creation
;; 4. Combined filters work together
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Setup

(around-each (proceed context)
  "Wrap all tests in mock GTD context.
Disable native compilation trampolines to avoid mock-fs conflicts with /tmp/."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Priority Integration Tests

(deftest view-filters/priority-single-value-end-to-end ()
  "Priority filter with single value works in full view creation flow."
  (let ((view-spec '((name . "High Priority Actions")
                     (type . next-action)
                     (priority . A))))
    ;; Should not error when creating the view
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      ;; Should create a valid command structure
      (assert-equal 1 (length commands))
      ;; First element is key, second is title, third is blocks list
      (let* ((command (car commands))
             (blocks (caddr command)))
        (assert-true (listp blocks))
        (assert-true (> (length blocks) 0))))))

(deftest view-filters/priority-list-end-to-end ()
  "Priority filter with multiple values works in view creation."
  (let ((view-spec '((name . "High/Medium Priority")
                     (type . next-action)
                     (priority . (A B)))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/priority-comparison-end-to-end ()
  "Priority filter with comparison works in view creation."
  (let ((view-spec '((name . "B or Higher")
                     (type . next-action)
                     (priority . (>= B)))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/priority-nil-end-to-end ()
  "Priority filter with nil (missing priority) works in view creation."
  (let ((view-spec '((name . "No Priority Set")
                     (type . next-action)
                     (priority . nil))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

;;; Effort Integration Tests

(deftest view-filters/effort-less-than-end-to-end ()
  "Effort filter with < comparison works in view creation."
  (let ((view-spec '((name . "Quick Wins")
                     (type . next-action)
                     (effort . (< "0:30")))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/effort-greater-than-end-to-end ()
  "Effort filter with > comparison works in view creation."
  (let ((view-spec '((name . "Deep Work Tasks")
                     (type . next-action)
                     (effort . (> "1:00")))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/effort-between-end-to-end ()
  "Effort filter with between range works in view creation."
  (let ((view-spec '((name . "Medium Tasks")
                     (type . next-action)
                     (effort . (between "0:15" "1:00")))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/effort-nil-end-to-end ()
  "Effort filter with nil (missing effort) works in view creation."
  (let ((view-spec '((name . "No Estimate")
                     (type . next-action)
                     (effort . nil))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

;;; Clocked Integration Tests

(deftest view-filters/clocked-less-than-end-to-end ()
  "Clocked filter with < comparison works in view creation."
  (let ((view-spec '((name . "Low Investment")
                     (type . next-action)
                     (clocked . (< "0:30")))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/clocked-greater-than-end-to-end ()
  "Clocked filter with > comparison works in view creation."
  (let ((view-spec '((name . "Invested Tasks")
                     (type . next-action)
                     (clocked . (> "0:30")))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/clocked-between-end-to-end ()
  "Clocked filter with between range works in view creation."
  (let ((view-spec '((name . "Moderate Investment")
                     (type . next-action)
                     (clocked . (between "0:30" "2:00")))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/clocked-nil-end-to-end ()
  "Clocked filter with nil (zero time) works in view creation."
  (let ((view-spec '((name . "Not Started")
                     (type . next-action)
                     (clocked . nil))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

;;; Combined Filters

(deftest view-filters/combined-priority-effort ()
  "Priority and effort filters combine correctly in view creation."
  (let ((view-spec '((name . "High Priority Quick Wins")
                     (type . next-action)
                     (priority . (A B))
                     (effort . (< "0:30")))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/combined-priority-clocked ()
  "Priority and clocked filters combine correctly in view creation."
  (let ((view-spec '((name . "High Priority New Tasks")
                     (type . next-action)
                     (priority . A)
                     (clocked . nil))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/combined-effort-clocked ()
  "Effort and clocked filters combine correctly in view creation."
  (let ((view-spec '((name . "Quick Tasks Not Started")
                     (type . next-action)
                     (effort . (< "0:30"))
                     (clocked . nil))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

(deftest view-filters/combined-all-three ()
  "Priority, effort, and clocked filters all work together."
  (let ((view-spec '((name . "High Priority Quick Wins Not Started")
                     (type . next-action)
                     (priority . A)
                     (effort . (< "0:30"))
                     (clocked . nil))))
    (let ((commands (org-gtd-view-lang--create-custom-commands (list view-spec))))
      (assert-true commands)
      (assert-equal 1 (length commands)))))

;;; View Show Integration

(deftest view-filters/priority-view-show-integration ()
  "Priority filter works with org-gtd-view-show."
  ;; Create a test item with priority
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT High Priority Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Actions\n")
    (insert ":PRIORITY: A\n")
    (insert ":END:\n")
    (basic-save-buffer))

  ;; Should be able to display view without error
  (org-gtd-view-show
   '((name . "High Priority")
     (type . next-action)
     (priority . A)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (assert-match "High Priority" (buffer-string)))))

(deftest view-filters/effort-view-show-integration ()
  "Effort filter works with org-gtd-view-show."
  ;; Create a test item with effort
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Quick Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Actions\n")
    (insert ":Effort: 0:15\n")
    (insert ":END:\n")
    (basic-save-buffer))

  ;; Should be able to display view without error
  (org-gtd-view-show
   '((name . "Quick Tasks")
     (type . next-action)
     (effort . (< "0:30"))))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (assert-match "Quick Task" (buffer-string)))))

(deftest view-filters/clocked-view-show-integration ()
  "Clocked filter works with org-gtd-view-show."
  ;; Create a test item without clock entries (zero time)
  (with-current-buffer (org-gtd--default-file)
    (insert "* NEXT Unstarted Task\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Actions\n")
    (insert ":END:\n")
    (basic-save-buffer))

  ;; Should be able to display view without error
  (org-gtd-view-show
   '((name . "Not Started")
     (type . next-action)
     (clocked . nil)))

  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    (with-current-buffer agenda-buffer
      (assert-match "Unstarted Task" (buffer-string)))))

;;; Multiple View Show Tests

(deftest view-filters/two-view-show-calls-create-two-buffers ()
  "Calling org-gtd-view-show twice should create two separate agenda buffers.
This test verifies that users can display multiple independent views
in a single function, each in its own buffer."
  ;; Call org-gtd-view-show with first view
  (org-gtd-view-show
   '((name . "First View")
     (type . next-action)))

  (let ((first-buffer (current-buffer)))

    ;; Call org-gtd-view-show with second view
    (org-gtd-view-show
     '((name . "Second View")
       (type . calendar)))

    (let ((second-buffer (current-buffer)))

      ;; We should have two distinct agenda buffers
      ;; (Expected to FAIL: org-agenda reuses the same buffer)
      (assert-not-equal first-buffer second-buffer)

      ;; Verify both buffers still exist
      (assert-true (buffer-live-p first-buffer))
      (assert-true (buffer-live-p second-buffer)))))

(deftest view-filters/view-show-uses-default-key-when-not-specified ()
  "org-gtd-view-show uses 'g' as default key when KEYS not provided."
  (org-gtd-view-show '((name . "Default Key View") (type . next-action)))
  (let ((agenda-buffer (get-buffer org-agenda-buffer-name)))
    (assert-true agenda-buffer)
    ;; Buffer name should be *Org Agenda* (not *Org Agenda(g)*) when not sticky
    (assert-equal "*Org Agenda*" (buffer-name agenda-buffer))))

(deftest view-filters/same-key-reuses-buffer-with-sticky ()
  "Using same KEYS value reuses the same buffer even with org-agenda-sticky."
  (let ((org-agenda-sticky t))
    (org-gtd-view-show '((name . "View 1") (type . next-action)) "x")
    (let ((first-buffer (current-buffer)))
      (org-gtd-view-show '((name . "View 2") (type . calendar)) "x")
      (let ((second-buffer (current-buffer)))
        ;; Same key = same buffer (content replaced)
        (assert-equal first-buffer second-buffer)))))

(provide 'view-filters-integration-test)

;;; view-filters-test.el ends here
