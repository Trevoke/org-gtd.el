;;; setup.el --- E-unit test setup with mock-fs integration -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Test infrastructure for org-gtd e-unit tests using mock-fs for filesystem
;; virtualization. Each test runs in an isolated virtual filesystem.
;;
;; Usage in test files:
;;
;;   (require 'ogt-eunit-prelude "test/helpers/prelude.el")
;;
;;   ;; Wrap each test in mock filesystem
;;   (around-each (proceed context)
;;     (ogt-eunit-with-mock-gtd
;;       (funcall proceed context)))
;;
;;   (deftest my-test ()
;;     "Test description."
;;     (capture-inbox-item "Test item")
;;     ...)
;;

;;; Code:

(require 'e-unit)
(require 'mock-fs)
(require 'org-gtd)

;;; Custom Assertions

(defun assert-same-items (expected actual)
  "Assert EXPECTED and ACTUAL contain same items (order-independent).
Useful for migrating buttercup tests that use :to-have-same-items-as matcher."
  (let ((exp-sorted (sort (copy-sequence expected) #'string<))
        (act-sorted (sort (copy-sequence actual) #'string<)))
    (assert-equal exp-sorted act-sorted)))

;;; Mock-FS Configuration

;; Paths in the mock-fs spec should NOT include the /mock: prefix.
;; The prefix is added when accessing files through Emacs functions.
(defconst ogt-eunit--mock-fs-gtd-path "/gtd/"
  "Virtual path for GTD directory in mock-fs spec (without /mock: prefix).")

(defconst ogt-eunit--mock-gtd-path "/mock:/gtd/"
  "Path to the virtual GTD directory when accessing via Emacs (with /mock: prefix).")

(defun ogt-eunit--mock-fs-spec ()
  "Return the mock filesystem spec for GTD tests.
Creates a minimal GTD directory structure with empty org files.

Spec paths use bare paths like /gtd/, but test code accesses them
via /mock:/gtd/ - the prefix triggers the file-name-handler.

Includes /tmp/ for tests that need files outside GTD directory."
  `(("/gtd/" . directory)
    ("/gtd/inbox.org" . "")
    ("/gtd/org-gtd-tasks.org" . "")
    ("/gtd/org-gtd-calendar.org" . "")
    ("/gtd/org-gtd-incubate.org" . "")
    ;; /tmp/ for tests that create non-GTD files (e.g., smart-save tests)
    ("/tmp/" . directory)))

;;; Emacs Configuration for Tests

(defconst ogt-eunit--mock-tmp-path "/mock:/tmp/"
  "Path to the virtual temp directory when accessing via Emacs (with /mock: prefix).")

(defun ogt-eunit--configure-emacs ()
  "Configure Emacs for GTD testing with mock-fs paths.
Sets up org-gtd to use the virtual filesystem."

  ;; Temp file directory for mock-fs (used by make-temp-file)
  (setq temporary-file-directory ogt-eunit--mock-tmp-path)

  ;; GTD configuration pointing to mock filesystem
  ;; IMPORTANT: org-gtd-areas-of-focus and org-gtd-organize-hooks must be
  ;; reset here to prevent cross-test contamination (e.g., unwanted prompts
  ;; for "Which area of focus?" when a previous test set these values)
  (setq org-gtd-directory ogt-eunit--mock-gtd-path
        org-gtd-areas-of-focus nil
        org-gtd-organize-hooks '()
        org-gtd-refile-to-any-target t
        org-gtd-update-ack "3.0.0"
        ;; Show DELEGATED_TO property in agenda views (for delegation tests)
        org-gtd-agenda-property-list '("DELEGATED_TO"))

  ;; Org-mode configuration
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (done . "DONE")
                                  (canceled . "CNCL")))

  ;; v4: Users must configure org-agenda-files to include GTD directory
  (setq org-agenda-files (list org-gtd-directory))

  ;; Enable org-edna for dependency management
  (org-edna-mode 1)

  ;; Set up clarify keybinding
  (define-key org-gtd-clarify-map (kbd "C-c c") #'org-gtd-organize))

;;; Cleanup Functions

(defun ogt-eunit--cleanup ()
  "Clean up Emacs state after a test.
Kills GTD-related buffers and clears org-mode internal state."

  ;; Kill GTD-related buffers
  (let ((buffers-to-kill '()))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (not (member (buffer-name buffer) '("*scratch*" "*Messages*")))
                 (or (string-match-p "org-gtd" (buffer-name buffer))
                     (string-match-p "\\.org" (buffer-name buffer))
                     (string-match-p "Agenda" (buffer-name buffer))
                     (string-match-p "gtd_archive" (buffer-name buffer))
                     (string-match-p "Calendar" (buffer-name buffer))
                     (string-search org-gtd-wip--prefix (buffer-name buffer))
                     (string-match-p "\\*Help\\*" (buffer-name buffer))
                     (string-match-p "\\*Warnings\\*" (buffer-name buffer))
                     (and (buffer-file-name buffer)
                          (or (string-match-p "org-gtd" (buffer-file-name buffer))
                              (string-match-p "/mock:" (buffer-file-name buffer))))))
        (push buffer buffers-to-kill)))

    (when buffers-to-kill
      (set-buffer (get-buffer-create "*scratch*"))
      (dolist (buffer buffers-to-kill)
        (when (buffer-live-p buffer)
          (when (buffer-file-name buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil)))
          (kill-buffer buffer)))))

  ;; Clear org-mode internal state
  (ogt-eunit--clear-org-state))

(defun ogt-eunit--clear-org-state ()
  "Clear org-mode internal state that might contaminate between tests."
  (setq org-agenda-files nil
        org-agenda-buffer nil
        org-todo-keywords-1 nil
        org-todo-keywords-for-agenda nil
        org-done-keywords-for-agenda nil
        org-agenda-markers nil
        org-agenda-contributing-files nil
        org-agenda-last-search-view-search-was-boolean nil
        org-id-files nil
        org-id-extra-files nil
        file-name-history nil)

  ;; Create a fresh empty hash table for org-id-locations
  ;; CRITICAL: Setting to nil causes org-id to reload from disk on next use,
  ;; which brings back /tmp paths from buttercup tests. An empty hash table
  ;; prevents this lazy-loading behavior.
  (setq org-id-locations (make-hash-table :test 'equal))

  ;; Clear org-id internal state that could cause cross-test contamination:
  ;; - org-id--locations-checksum: If stale, org-id-update-id-locations may
  ;;   skip rescanning when files have actually changed
  ;; - org-id-overriding-file-name: Used for temp buffers, could leak paths
  (setq org-id--locations-checksum nil
        org-id-overriding-file-name nil)

  ;; Clear transient state
  (when (fboundp 'transient--emergency-exit)
    (ignore-errors (transient--emergency-exit)))
  (when (and (boundp 'transient--prefix) transient--prefix)
    (setq transient--prefix nil))
  (when (boundp 'transient-history)
    (setq transient-history nil))

  ;; Clear input state
  (discard-input)
  (setq buffer-save-without-query nil
        current-prefix-arg nil)

  ;; Reset window configuration
  (delete-other-windows)

  ;; End in clean state
  (set-buffer (get-buffer-create "*scratch*"))
  (with-current-buffer "*scratch*"
    (emacs-lisp-mode)))

;;; Archive Helpers

(defun ogt-eunit--archive-buffer ()
  "Return the buffer visiting the archive file."
  (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
    (let ((org-archive-location (funcall org-gtd-archive-location)))
      (find-file-noselect
       (car (org-archive--compute-location org-archive-location))))))

(defun archive-string ()
  "Return string of items archived from actionable file."
  (with-current-buffer (ogt-eunit--archive-buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

;;; Test Wrapper Macro

(defmacro ogt-eunit-with-mock-gtd (&rest body)
  "Execute BODY within a mock GTD filesystem context.
Sets up the mock filesystem, configures Emacs for testing,
executes BODY, and cleans up afterward."
  (declare (indent 0) (debug t))
  `(progn
     ;; Clear state BEFORE test to ensure clean start
     (ogt-eunit--clear-org-state)
     (with-mock-fs (ogt-eunit--mock-fs-spec)
       (ogt-eunit--configure-emacs)
       (unwind-protect
           (progn ,@body)
         (ogt-eunit--cleanup)))))

(setup-suite
 (setq inhibit-message t)
 ;; Suppress noisy org-mode state change messages during tests
 ;; e.g., "TODO state changed to NEXT", "TODO state was already TODO"
 (setq inhibit-message-regexps '("^.*? state was already.*?$"
                                 "^.*? state changed to.*?$"
                                 "^.*? state changed to^"
                                 "^Copied: Subtree(s) with .*? characters$"
                                 ;"^Save file .*?inbox\.org.*?$" ;;; seems to not be a message
                                 "^Graph exported to.*?$"
                                 "^Refile to .*?: done$"
                                 "^.*?files scanned.*?$"
                                 "^Finding ID locations.*?$"
                                 ))
 )

(provide 'ogt-eunit-setup)

;;; setup.el ends here
