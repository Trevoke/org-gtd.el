;;; agenda-property-loop-test.el --- Tests for infinite loop fix in org-gtd-agenda-property -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Test for the infinite loop fix in org-gtd-agenda-property
;; when using 'next-line position.
;;
;; The original bug: When properties are inserted on a new line, the code
;; copied ALL text properties (including org-marker) to the new line. On the
;; next iteration, the function found org-marker on this new line and tried
;; to process it again, creating an infinite loop.
;;
;; The fix: Property lines now have BOTH:
;; - org-marker: so org-agenda commands work when cursor is on property line
;; - org-gtd-property-line: so the processing loop skips these lines
;;
;; This makes property lines interactive while preventing infinite loops.
;;
;; GitHub issue: https://github.com/Malabarba/org-agenda-property/issues/6
;;
;; Migrated from test/agenda-property-loop-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-agenda-property)
(require 'org-gtd-delegate)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    ;; Save original hook state to restore later
    (let ((original-finalize-hook (copy-sequence org-agenda-finalize-hook))
          (original-old-finalize-hook (and (boundp 'org-finalize-agenda-hook)
                                           (copy-sequence org-finalize-agenda-hook))))
      ;; Remove function from hooks to isolate the test
      (remove-hook 'org-agenda-finalize-hook 'org-gtd-agenda-property-add-properties)
      (remove-hook 'org-finalize-agenda-hook 'org-gtd-agenda-property-add-properties)
      ;; Configure org-gtd-agenda-property to use next-line position
      (setq org-gtd-agenda-property-list '("DELEGATED_TO"))
      (setq org-gtd-agenda-property-position 'next-line)
      (unwind-protect
          (funcall proceed context)
        ;; Restore hooks to their original state
        (setq org-agenda-finalize-hook original-finalize-hook)
        (when (boundp 'org-finalize-agenda-hook)
          (setq org-finalize-agenda-hook original-old-finalize-hook))))))

(deftest agenda-property/property-lines-are-interactive-but-skipped ()
  "Property lines are interactive but skipped during processing.
Verifies that property lines have:
- org-marker: so org-agenda commands work (interactive)
- org-gtd-property-line: so the while loop skips them (no infinite loop)"
  ;; Create a delegated item
  (capture-inbox-item "Test delegated item")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-delegate "Test Person" "2025-12-01"))

  ;; Now generate an agenda to trigger org-gtd-agenda-property-add-properties
  (let ((org-agenda-files (list (expand-file-name "org-gtd-tasks.org" org-gtd-directory)))
        (iteration-count 0)
        (max-iterations 100))

    ;; Create a mock agenda buffer to test the property addition
    (with-current-buffer (get-buffer-create "*Test Agenda*")
      (erase-buffer)
      ;; Simulate an agenda line with org-marker
      (let* ((marker (with-current-buffer (org-gtd--default-file)
                       (goto-char (point-min))
                       (search-forward "Test delegated item")
                       (org-back-to-heading t)
                       (point-marker)))
             (line "  waiting:  WAIT Test delegated item"))
        (insert line)
        (put-text-property (point-min) (point-max) 'org-marker marker)

        ;; Now run the property addition with loop detection
        ;; IMPORTANT: Use symbol-function to capture the actual function object,
        ;; not the symbol. Using #' would return the symbol, which would be
        ;; looked up at call time (after cl-letf rebinds it), causing infinite recursion.
        (let ((original-add-properties (symbol-function 'org-gtd-agenda-property-add-properties)))
          (cl-letf (((symbol-function 'org-gtd-agenda-property-add-properties)
                     (lambda ()
                       (setq iteration-count (1+ iteration-count))
                       (when (> iteration-count max-iterations)
                         (error "Infinite loop detected! Iterations: %d" iteration-count))
                       (funcall original-add-properties))))

            ;; This should complete without infinite loop
            (org-gtd-agenda-property-add-properties)))

        ;; Verify the buffer was modified
        (assert-match "DELEGATED_TO\\|Test Person" (buffer-string))

        ;; Verify property line has correct text properties
        (goto-char (point-min))
        (forward-line 1) ; Move to inserted property line
        (when (not (eobp))
          ;; Property line SHOULD have org-marker (for interactivity)
          (assert-true (org-get-at-bol 'org-marker))
          ;; Property line SHOULD have org-gtd-property-line (to skip processing)
          (assert-true (org-get-at-bol 'org-gtd-property-line))))

      (kill-buffer))))

(deftest agenda-property/handles-delegated-item-without-infinite-loop ()
  "Handles agenda with delegated item without infinite loop."
  ;; Create a delegated item
  (capture-inbox-item "Delegated task to check on")
  (org-gtd-process-inbox)
  (with-wip-buffer
    (org-gtd-delegate "Alice" "2025-12-15"))

  ;; Generate agenda - this should not hang
  ;; We use a timeout approach: if the function takes too long, it's looping
  (let* ((org-agenda-files (list (expand-file-name "org-gtd-tasks.org" org-gtd-directory)))
         (start-time (float-time))
         (timeout 5.0)  ; 5 seconds should be more than enough
         (completed nil))

    ;; Run agenda in a way that we can detect hangs
    (condition-case err
        (progn
          (org-agenda-list nil nil 'day)
          (setq completed t))
      (error
       (message "Agenda error: %s" err)))

    (let ((elapsed (- (float-time) start-time)))
      ;; Should complete quickly, not hang
      (assert-true completed)
      (assert-true (< elapsed timeout)))

    ;; Clean up agenda buffer
    (when (get-buffer "*Org Agenda*")
      (kill-buffer "*Org Agenda*"))))

(provide 'agenda-property-loop-test)

;;; agenda-property-loop-test.el ends here
