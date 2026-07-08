;;; checklist-reset-test.el --- Tests for checkbox reset on repeater re-arm -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for resetting checkboxes when a repeating heading is
;; completed and re-armed by org.
;;
;; Test Coverage:
;; - Repeating heading resets its checkboxes and cookie on completion (1 test)
;; - Non-repeating heading keeps its checkboxes on completion (1 test)
;; - Cancelled (zero-interval) repeater keeps its checkboxes (1 test)
;; - Done-to-done transition on a repeating heading keeps boxes (1 test)
;; - org-gtd-mode installs and removes the reset hook (1 test)
;;
;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-checklist)
(require 'org-gtd-mode)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defmacro checklist-reset--with-heading (text &rest body)
  "Run BODY in an org buffer containing TEXT, point on first heading,
with the reset hook installed."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,text)
     (goto-char (point-min))
     (add-hook 'org-after-todo-state-change-hook
               #'org-gtd-checklist--maybe-reset-checkboxes)
     (unwind-protect (progn ,@body)
       (remove-hook 'org-after-todo-state-change-hook
                    #'org-gtd-checklist--maybe-reset-checkboxes))))

;;; Reset Behavior Tests

(deftest checklist-reset/repeating-heading-resets-boxes-on-done ()
  "Completing a repeating heading clears its checkboxes and re-arms."
  (checklist-reset--with-heading
      "* TODO Weekly triggers [1/2]\nSCHEDULED: <2026-07-10 Fri .+1w>\n- [X] Boss?\n- [ ] Car?\n"
    (let ((org-inhibit-logging t) (org-log-repeat nil))
      (org-todo "DONE"))
    (assert-nil (string-match-p "\\[X\\]" (buffer-string)))
    (assert-match "\\* TODO Weekly triggers \\[0/2\\]" (buffer-string))))

(deftest checklist-reset/cancelled-repeater-keeps-boxes ()
  "Finishing a repeating task for good keeps its checkboxes.
`org-todo' with -1 cancels the repeater first (interval becomes zero,
the documented C-- 1 C-c C-t flow); org does not re-arm, so the
completion record must stay intact."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE"))))
    (checklist-reset--with-heading
        "* TODO Weekly triggers\nSCHEDULED: <2026-07-10 Fri .+1w>\n- [X] Boss?\n- [ ] Car?\n"
      (let ((org-inhibit-logging t) (org-log-repeat nil))
        (org-todo -1))
      (assert-match "\\[X\\] Boss\\?" (buffer-string))
      (assert-match "\\* DONE Weekly triggers" (buffer-string)))))

(deftest checklist-reset/done-to-done-keeps-boxes ()
  "A done-to-done transition on a repeating heading does not reset.
Org only re-arms when a done state is entered from a non-done one;
the reset guard must match."
  (let ((org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELED"))))
    (checklist-reset--with-heading
        "* DONE Weekly triggers\nSCHEDULED: <2026-07-10 Fri .+1w>\n- [X] Boss?\n- [ ] Car?\n"
      (let ((org-inhibit-logging t) (org-log-repeat nil))
        (org-todo "CANCELED"))
      (assert-match "\\[X\\] Boss\\?" (buffer-string))
      (assert-match "\\* CANCELED Weekly triggers" (buffer-string)))))

(deftest checklist-reset/plain-done-keeps-boxes ()
  "Completing a non-repeating heading leaves checkboxes alone."
  (checklist-reset--with-heading
      "* TODO Beach packing\n- [X] Sunscreen\n- [ ] Towel\n"
    (let ((org-inhibit-logging t))
      (org-todo "DONE"))
    (assert-match "\\[X\\] Sunscreen" (buffer-string))))

;;; Mode Wiring Tests

(deftest checklist-reset/org-gtd-mode-installs-hook ()
  "org-gtd-mode adds and removes the reset hook."
  (org-gtd-mode 1)
  (unwind-protect
      (assert-true (memq #'org-gtd-checklist--maybe-reset-checkboxes
                         org-after-todo-state-change-hook))
    (org-gtd-mode -1))
  (assert-nil (memq #'org-gtd-checklist--maybe-reset-checkboxes
                    org-after-todo-state-change-hook)))

(provide 'checklist-reset-test)

;;; checklist-reset-test.el ends here
