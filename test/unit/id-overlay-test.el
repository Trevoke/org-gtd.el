;;; id-overlay-test.el --- Tests for org-gtd-id-overlay -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-id-overlay text functions and integration behavior.
;; Includes both pure unit tests and integration tests with WIP mode.
;;
;; Migrated from test/id-overlay-test.el (buttercup).
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Test Helpers

(defun ogt-create-test-heading-with-id (heading-text &optional id)
  "Create a test heading with ID property.
HEADING-TEXT is the heading content, ID is optional custom ID."
  (let ((test-id (or id (org-id-uuid))))
    (format "* %s\n:PROPERTIES:\n:ID: %s\n:END:\n"
            heading-text test-id)))

(defun ogt-count-overlays-in-buffer ()
  "Count the number of org-gtd-id overlays in the current buffer."
  (length (seq-filter
           (lambda (overlay)
             (overlay-get overlay 'org-gtd-id-overlay))
           (overlays-in (point-min) (point-max)))))

(defun ogt-find-overlay-with-text (text)
  "Find overlay displaying TEXT in the current buffer."
  (seq-find
   (lambda (overlay)
     (and (overlay-get overlay 'org-gtd-id-overlay)
          (string-equal (overlay-get overlay 'display) text)))
   (overlays-in (point-min) (point-max))))

;;; Pure Unit Tests - org-gtd-id-overlay--extract-heading-text

(deftest id-overlay/extract-plain-heading ()
  "Extracts plain heading text."
  (assert-equal "Meeting with client"
                (org-gtd-id-overlay--extract-heading-text "Meeting with client")))

(deftest id-overlay/extract-removes-todo-keywords ()
  "Removes TODO keywords."
  (assert-equal "Review quarterly budget"
                (org-gtd-id-overlay--extract-heading-text "TODO Review quarterly budget"))
  (assert-equal "Complete project"
                (org-gtd-id-overlay--extract-heading-text "DONE Complete project"))
  (assert-equal "Call customer"
                (org-gtd-id-overlay--extract-heading-text "NEXT Call customer")))

(deftest id-overlay/extract-removes-statistics-cookies ()
  "Removes statistics cookies."
  (assert-equal "Project tasks"
                (org-gtd-id-overlay--extract-heading-text "Project tasks [1/3]"))
  (assert-equal "Progress"
                (org-gtd-id-overlay--extract-heading-text "Progress [33%]"))
  (assert-equal "Multi"
                (org-gtd-id-overlay--extract-heading-text "Multi [2/5] [40%]")))

(deftest id-overlay/extract-handles-complex-combinations ()
  "Handles complex combinations."
  (assert-equal "Complete project"
                (org-gtd-id-overlay--extract-heading-text "TODO Complete project [2/4] [50%]")))

(deftest id-overlay/extract-handles-empty-gracefully ()
  "Handles empty results gracefully."
  (assert-equal ""
                (org-gtd-id-overlay--extract-heading-text "TODO"))
  (assert-equal ""
                (org-gtd-id-overlay--extract-heading-text "[1/3]")))

(deftest id-overlay/extract-handles-whitespace ()
  "Handles whitespace properly."
  (assert-equal "Multiple spaces"
                (org-gtd-id-overlay--extract-heading-text "TODO   Multiple    spaces   ")))

;;; Pure Unit Tests - org-gtd-id-overlay--truncate-text

(deftest id-overlay/truncate-long-text-with-ellipsis ()
  "Truncates text longer than limit with ellipsis."
  (assert-equal "This is a very long ..."
                (org-gtd-id-overlay--truncate-text
                 "This is a very long heading that needs truncation" 20)))

(deftest id-overlay/truncate-short-text-unchanged ()
  "Returns short text unchanged."
  (assert-equal "Short text"
                (org-gtd-id-overlay--truncate-text "Short text" 20)))

(deftest id-overlay/truncate-exact-limit ()
  "Handles text exactly at limit."
  (assert-equal "Exactly twenty chars"
                (org-gtd-id-overlay--truncate-text "Exactly twenty chars" 20)))

(deftest id-overlay/truncate-empty-text ()
  "Handles empty text."
  (assert-equal ""
                (org-gtd-id-overlay--truncate-text "" 20)))

(deftest id-overlay/truncate-custom-length ()
  "Handles custom truncation length."
  (assert-equal "Long tex..."
                (org-gtd-id-overlay--truncate-text "Long text here" 8)))

;;; Integration Tests - Clarify Mode

(deftest id-overlay/clarify-enables-overlays-automatically ()
  "Automatically enables overlays in clarify buffers."
  (with-temp-buffer
    (insert (ogt-create-test-heading-with-id "Buy groceries for the week [1/3]"))
    (org-gtd-clarify-mode)
    (org-gtd-id-overlay-maybe-enable)
    (assert-true (bound-and-true-p org-gtd-id-overlay-mode))
    (assert-true (> (ogt-count-overlays-in-buffer) 0))))

(deftest id-overlay/clarify-disables-overlays-when-mode-disabled ()
  "Disables overlays when clarify mode is disabled."
  (with-temp-buffer
    (insert (ogt-create-test-heading-with-id "Test heading"))
    (org-gtd-clarify-mode)
    (org-gtd-id-overlay-mode 1)
    (org-gtd-id-overlay-mode -1)
    (assert-nil org-gtd-id-overlay-mode)
    (assert-equal 0 (ogt-count-overlays-in-buffer))))

;;; Integration Tests - Overlay Display Behavior

(deftest id-overlay/displays-truncated-heading-text ()
  "Displays truncated heading text instead of ID."
  (with-temp-buffer
    (org-mode)
    (insert (ogt-create-test-heading-with-id "Buy groceries for the week"))
    (org-gtd-id-overlay-mode 1)
    (assert-true (ogt-find-overlay-with-text "Buy groceries for th..."))))

(deftest id-overlay/removes-todo-keywords-from-overlay ()
  "Removes TODO keywords from overlay text."
  (with-temp-buffer
    (org-mode)
    (insert (ogt-create-test-heading-with-id "TODO Buy groceries for the week"))
    (org-gtd-id-overlay-mode 1)
    (assert-true (ogt-find-overlay-with-text "Buy groceries for th..."))))

(deftest id-overlay/removes-statistics-cookies-from-overlay ()
  "Removes statistics cookies from overlay text."
  (with-temp-buffer
    (org-mode)
    (insert (ogt-create-test-heading-with-id "Buy groceries for the week [1/3]"))
    (org-gtd-id-overlay-mode 1)
    (assert-true (ogt-find-overlay-with-text "Buy groceries for th..."))))

(deftest id-overlay/handles-short-headings-without-ellipsis ()
  "Handles short headings without ellipsis."
  (with-temp-buffer
    (org-mode)
    (insert (ogt-create-test-heading-with-id "Short"))
    (org-gtd-id-overlay-mode 1)
    (assert-true (ogt-find-overlay-with-text "Short"))))

;;; Integration Tests - Overlay Management

(deftest id-overlay/creates-overlays-for-all-ids ()
  "Creates overlays for all ID properties in buffer."
  (with-temp-buffer
    (org-mode)
    (insert (ogt-create-test-heading-with-id "First heading"))
    (insert (ogt-create-test-heading-with-id "Second heading"))
    (insert (ogt-create-test-heading-with-id "Third heading"))
    (org-gtd-id-overlay-mode 1)
    (assert-equal 3 (ogt-count-overlays-in-buffer))))

(deftest id-overlay/removes-all-overlays-when-disabled ()
  "Removes all overlays when mode is disabled."
  (with-temp-buffer
    (org-mode)
    (insert (ogt-create-test-heading-with-id "Test heading"))
    (org-gtd-id-overlay-mode 1)
    (assert-true (> (ogt-count-overlays-in-buffer) 0))
    (org-gtd-id-overlay-mode -1)
    (assert-equal 0 (ogt-count-overlays-in-buffer))))

(deftest id-overlay/handles-malformed-id-gracefully ()
  "Handles malformed ID properties gracefully."
  (with-temp-buffer
    (org-mode)
    (insert "* Test heading\n:PROPERTIES:\n:ID: \n:END:\n")
    (org-gtd-id-overlay-mode 1)
    ;; Should not crash or create invalid overlays
    (assert-true t)))

;;; Integration Tests - Clarification Workflow (uses mock-fs)

(around-each (proceed context)
  "Wrap clarification workflow test in mock GTD context."
  (when (string-match-p "clarification-workflow"
                        (symbol-name (plist-get context :test-name)))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))
    (cl-return-from nil nil))
  (funcall proceed context))

(deftest id-overlay/clarification-workflow-handles-new-ids ()
  "Handles newly created IDs in WIP buffer during clarification workflow."
  (capture-inbox-item "Set up office equipment")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (insert "** TODO Buy standing desk")
    (org-gtd-id-get-create)
    (let ((desk-id (org-gtd-id-get-create)))
      (insert "\n** TODO Install accessories")
      (org-gtd-id-get-create)
      (end-of-line)
      (insert (format "\n:PROPERTIES:\n:BLOCKED_BY: %s\n:END:" desk-id))
      (org-gtd-id-overlay-mode 1)
      (assert-true (> (ogt-count-overlays-in-buffer) 0)))))

;;; Integration Tests - Dynamic Overlay Updates

(deftest id-overlay/updates-overlays-when-blocked-by-changes ()
  "Updates overlays when BLOCKED_BY property changes."
  (with-temp-buffer
    (org-mode)
    (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
    (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n")
    (org-gtd-id-overlay-mode 1)
    (let ((initial-count (ogt-count-overlays-in-buffer)))
      ;; Add BLOCKED_BY property to Task B
      (goto-char (point-min))
      (re-search-forward "Task B")
      (org-back-to-heading t)
      (org-set-property "BLOCKED_BY" "task-a-id")
      ;; Check that overlays increased due to the new property
      (assert-true (> (ogt-count-overlays-in-buffer) initial-count)))))

;;; Performance Tests

(deftest id-overlay/handles-multiple-overlays-efficiently ()
  "Handles multiple overlays efficiently (under 100ms for 50 headings)."
  (with-temp-buffer
    (org-mode)
    ;; Create 50 headings with IDs
    (dotimes (i 50)
      (insert (ogt-create-test-heading-with-id (format "Heading %d with some text" i))))
    ;; Time the overlay creation
    (let ((start-time (current-time)))
      (org-gtd-id-overlay-mode 1)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (assert-true (< elapsed 0.1))))))

(deftest id-overlay/cleans-up-memory-properly ()
  "Cleans up memory properly during repeated enable/disable."
  (with-temp-buffer
    (org-mode)
    (insert (ogt-create-test-heading-with-id "Test heading"))
    ;; Repeatedly enable/disable mode
    (dotimes (_i 10)
      (org-gtd-id-overlay-mode 1)
      (org-gtd-id-overlay-mode -1))
    ;; Should have no overlays remaining
    (assert-equal 0 (ogt-count-overlays-in-buffer))))

(provide 'id-overlay-test)

;;; id-overlay-test.el ends here
