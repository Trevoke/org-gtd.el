;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)


;; Load guard to prevent redundant loading
(unless (featurep 'org-gtd-test-setup)

(setq org-gtd-update-ack "3.0.0")

;; Load helpers using require for idempotent loading
(require 'org-gtd-test-helper-clarifying (file-name-concat default-directory "test/helpers/clarifying.el"))
(require 'org-gtd-test-helper-project-fixtures (file-name-concat default-directory "test/helpers/project-fixtures.el"))
(require 'org-gtd-test-helper-processing (file-name-concat default-directory "test/helpers/processing.el"))
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))
(require 'org-gtd-test-helper-wip (file-name-concat default-directory "test/helpers/wip.el"))

(defun ogt--configure-emacs ()
  (setq last-command nil
        org-gtd-directory (make-temp-file "org-gtd" t)
        org-gtd-areas-of-focus nil
        org-gtd-organize-hooks '()
        org-gtd-refile-to-any-target t
        org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL"))
        org-gtd-keyword-mapping '((todo . "TODO")
                                  (next . "NEXT")
                                  (wait . "WAIT")
                                  (done . "DONE")
                                  (canceled . "CNCL")))
  ;; v4: Users must configure org-agenda-files to include GTD directory
  ;; This mirrors the required user configuration
  (setq org-agenda-files (list org-gtd-directory))
  (org-edna-mode 1)
  (define-key org-gtd-clarify-map (kbd "C-c c") #'org-gtd-organize))

(defun ogt--reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(defun ogt--close-and-delete-files ()
  "Run after every test to clear open buffers state"

  ;; Kill GTD-related buffers but preserve essential system buffers
  (let ((buffers-to-kill '()))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 ;; Don't kill essential system buffers
                 (not (member (buffer-name buffer) '("*scratch*" "*Messages*")))
                 (or (string-match-p "org-gtd" (buffer-name buffer))
                     (string-match-p "\\.org" (buffer-name buffer))
                     (string-match-p "Agenda" (buffer-name buffer))
                     (string-match-p "gtd_archive" (buffer-name buffer))
                     (string-match-p "Calendar" (buffer-name buffer))
                     (string-search org-gtd-wip--prefix (buffer-name buffer))
                     ;; Kill some transient-related buffers but not all
                     (string-match-p "\\*Help\\*" (buffer-name buffer))
                     (string-match-p "\\*Warnings\\*" (buffer-name buffer))
                     ;; Also kill any buffer with file name containing GTD paths
                     (and (buffer-file-name buffer)
                          (string-match-p "org-gtd" (buffer-file-name buffer)))))
        (push buffer buffers-to-kill)))

    ;; Kill buffers but ensure we don't kill the current buffer without setting a new one
    (when buffers-to-kill
      ;; Switch to scratch buffer first to avoid killing current buffer issues
      (set-buffer (get-buffer-create "*scratch*"))
      (dolist (buffer buffers-to-kill)
        (when (buffer-live-p buffer)
          (when (buffer-file-name buffer)
            (with-current-buffer buffer
              (set-buffer-modified-p nil)))
          (kill-buffer buffer)))))

  ;; Clear all org-mode internal state
  (ogt--clear-org-mode-state)
  )

(defun ogt--clear-file-and-buffer (buffer)
  (if (bufferp buffer)
      (let ((filename (buffer-file-name buffer)))
        (with-current-buffer buffer (basic-save-buffer))
        (kill-buffer buffer)
        (delete-file filename))))

(defun ogt--get-buffers (regexp)
  (seq-filter (lambda (buf)
                (string-match-p regexp (buffer-name buf)))
              (buffer-list)))

(defun ogt--kill-buffer (buffer)
  (when (buffer-file-name buffer)
    (with-current-buffer buffer
      (revert-buffer t t)))
  (kill-buffer buffer))

(defun ogt--clear-org-mode-state ()
  "Clear org-mode internal state that might contaminate between tests"
  (setq org-agenda-files nil
        org-agenda-buffer nil
        org-todo-keywords-1 nil
        org-todo-keywords-for-agenda nil
        org-done-keywords-for-agenda nil
        org-agenda-markers nil
        org-agenda-contributing-files nil
        org-agenda-last-search-view-search-was-boolean nil
        ;; Clear org-id cache - CRITICAL for test isolation
        org-id-locations nil
        org-id-files nil
        org-id-extra-files nil
        ;; Clear file-name to buffer mapping to prevent find-file-noselect from
        ;; returning stale buffers from previous test directories
        file-name-history nil)

  ;; CRITICAL: Save the cleared org-id state to disk
  ;; Without this, the .org-id-locations file still contains stale IDs
  ;; from previous tests, which get reloaded and cause test pollution
  (org-id-locations-save)

  ;; Clear transient state that might be left over from previous tests
  (when (fboundp 'transient--emergency-exit)
    (transient--emergency-exit))

  ;; Clear any active transient
  (when (and (boundp 'transient--prefix) transient--prefix)
    (setq transient--prefix nil))

  ;; Reset transient history
  (when (boundp 'transient-history)
    (setq transient-history nil))

  ;; Clear input event queue and keyboard state
  (discard-input)

  ;; Clear any save-related state
  (setq buffer-save-without-query nil)

  ;; Clear any agenda-related current buffer confusion
  (setq current-prefix-arg nil)

  ;; Reset window configuration to clean state
  (delete-other-windows)

  ;; Ensure we end up in a clean buffer state
  (set-buffer (get-buffer-create "*scratch*"))
  (with-current-buffer "*scratch*"
    (emacs-lisp-mode))

  ;; Let org-mode handle its own cache state
  )

(defun ogt--recursive-eldev-test (file)
  (unless (file-readable-p (file-name-concat "test" file))
    (error "Cannot find or read file %s" file))
  (with-temp-buffer
    (prog1 (call-process
            eldev-shell-command
            nil
            t
            nil
            "test"
            "-f"
            file)
      (princ (buffer-substring 1 (point-max))))))

;; End load guard and provide feature
(provide 'org-gtd-test-setup))
