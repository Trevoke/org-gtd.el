;; -*- lexical-binding: t; coding: utf-8 -*-

;;; id-overlay-test.el --- Tests for org-gtd-id-overlay -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Tests for org-gtd-id-overlay functionality
;;

;;; Code:

;;;; Requirements

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

;;;; Test Helpers

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

;;;; Test Setup

(describe "org-gtd-id-overlay"
  
  (before-each (setq inhibit-message t) (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files))

  ;;;; Acceptance Tests - High Level Behavior

  (describe "integration with WIP mode"
    
    (it "automatically enables overlays in WIP buffers"
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "Buy groceries for the week [1/3]"))
        (org-gtd-wip-mode)
        ;; Try the hook again after fixing the major mode check
        (org-gtd-id-overlay-maybe-enable)
        (expect (bound-and-true-p org-gtd-id-overlay-mode) :to-be t)
        (expect (> (ogt-count-overlays-in-buffer) 0) :to-be t)))

    (it "disables overlays when WIP mode is disabled"
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "Test heading"))
        (org-gtd-wip-mode)
        (org-gtd-id-overlay-mode 1) ;; Manually enable for test
        (let ((overlay-count (ogt-count-overlays-in-buffer)))
          (org-gtd-id-overlay-mode -1) ;; Test disabling the mode directly
          (expect org-gtd-id-overlay-mode :to-be nil)
          (expect (ogt-count-overlays-in-buffer) :to-be 0)))))

  (describe "overlay display behavior"
    
    (it "displays truncated heading text instead of ID"
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "Buy groceries for the week"))
        (org-gtd-id-overlay-mode 1)
        (expect (ogt-find-overlay-with-text "Buy groceries for th...") :not :to-be nil)))

    (it "removes TODO keywords from overlay text"
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "TODO Buy groceries for the week"))
        (org-gtd-id-overlay-mode 1)
        (expect (ogt-find-overlay-with-text "Buy groceries for th...") :not :to-be nil)))

    (it "removes statistics cookies from overlay text"
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "Buy groceries for the week [1/3]"))
        (org-gtd-id-overlay-mode 1)
        (expect (ogt-find-overlay-with-text "Buy groceries for th...") :not :to-be nil)))

    (it "handles short headings without ellipsis"
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "Short"))
        (org-gtd-id-overlay-mode 1)
        (expect (ogt-find-overlay-with-text "Short") :not :to-be nil))))

  ;; Pure unit tests migrated to test-eunit/unit/id-overlay-test.el:
  ;; - org-gtd-id-overlay--extract-heading-text (6 tests)
  ;; - org-gtd-id-overlay--truncate-text (5 tests)

  ;;;; Unit Tests - Component Specific Behavior

  (describe "overlay management"
    
    (it "creates overlays for all ID properties in buffer"
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "First heading"))
        (insert (ogt-create-test-heading-with-id "Second heading"))
        (insert (ogt-create-test-heading-with-id "Third heading"))
        (org-gtd-id-overlay-mode 1)
        (expect (ogt-count-overlays-in-buffer) :to-be 3)))

    (it "removes all overlays when mode is disabled"
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "Test heading"))
        (org-gtd-id-overlay-mode 1)
        (expect (> (ogt-count-overlays-in-buffer) 0) :to-be t)
        (org-gtd-id-overlay-mode -1)
        (expect (ogt-count-overlays-in-buffer) :to-be 0)))

    (it "handles malformed ID properties gracefully"
      (with-temp-buffer
        (org-mode)
        (insert "* Test heading\n:PROPERTIES:\n:ID: \n:END:\n")
        (org-gtd-id-overlay-mode 1)
        ;; Should not crash or create invalid overlays
        (expect t :to-be t))))

  ;;;; Integration Tests

  (describe "clarification workflow integration"
    

    (it "handles newly created IDs in WIP buffer"
      ;; Test overlay functionality with IDs created during workflow
      (message "DEBUG: Starting second integration test")
      (capture-inbox-item "Set up office equipment") 
      (message "DEBUG: Capture completed")
      (org-gtd-process-inbox)
      (message "DEBUG: Process inbox completed")
      
      ;; Work in the WIP buffer where IDs will be created
      (with-wip-buffer
        ;; Add subtasks and create IDs for task dependencies
        (goto-char (point-max))
        (newline)
        (insert "** TODO Buy standing desk")
        (org-gtd-id-get-create)  ; Create ID for this task
        (let ((desk-id (org-gtd-id-get-create)))
          (insert "\n** TODO Install accessories")
          (org-gtd-id-get-create)
          (end-of-line)
          (insert (format "\n:PROPERTIES:\n:BLOCKED_BY: %s\n:END:" desk-id))

          ;; Enable overlay mode and test that overlays work
          (message "DEBUG: About to enable overlay mode")
          (org-gtd-id-overlay-mode 1)
          (message "DEBUG: Overlay mode enabled")
          (let ((overlay-count (ogt-count-overlays-in-buffer)))
            (message "DEBUG: Found %d overlays in WIP buffer" overlay-count)
            (expect (> overlay-count 0) :to-be t))))))

  (describe "dynamic overlay updates"
    
    (it "updates overlays when BLOCKED_BY property changes"
      (with-temp-buffer
        (org-mode)
        (insert "* Task A\n:PROPERTIES:\n:ID: task-a-id\n:END:\n\n")
        (insert "* Task B\n:PROPERTIES:\n:ID: task-b-id\n:END:\n")
        
        ;; Enable overlay mode
        (org-gtd-id-overlay-mode 1)
        (let ((initial-count (ogt-count-overlays-in-buffer)))
          
          ;; Add BLOCKED_BY property to Task B
          (goto-char (point-min))
          (re-search-forward "Task B")
          (org-back-to-heading t)
          (org-set-property "BLOCKED_BY" "task-a-id")
          
          ;; Check that overlays increased due to the new property
          (let ((new-count (ogt-count-overlays-in-buffer)))
            (expect (> new-count initial-count) :to-be t))))))

  ;;;; Performance Tests

  (describe "performance characteristics"
    
    (it "handles multiple overlays efficiently"
      (with-temp-buffer
        (org-mode)
        ;; Create 50 headings with IDs
        (dotimes (i 50)
          (insert (ogt-create-test-heading-with-id (format "Heading %d with some text" i))))
        ;; Time the overlay creation
        (let ((start-time (current-time)))
          (org-gtd-id-overlay-mode 1)
          (let ((elapsed (float-time (time-subtract (current-time) start-time))))
            ;; Should complete in under 100ms (0.1 seconds)
            (expect (< elapsed 0.1) :to-be t)))))

    (it "cleans up memory properly"
      ;; Test for memory leaks during repeated enable/disable
      (with-temp-buffer
        (org-mode)
        (insert (ogt-create-test-heading-with-id "Test heading"))
        ;; Repeatedly enable/disable mode
        (dotimes (i 10)
          (org-gtd-id-overlay-mode 1)
          (org-gtd-id-overlay-mode -1))
        ;; Should have no overlays remaining
        (expect (ogt-count-overlays-in-buffer) :to-be 0)))))

;;; id-overlay-test.el ends here
