;; -*- lexical-binding: t; coding: utf-8 -*-

;; Keyboard-aware helper functions for end-to-end testing
;; These functions use org-gtd-process-inbox (critical!) while verifying keyboard integration

(require 'org-gtd)

(defun ogt-capture-and-process-with-keyboard-verification (item-text)
  "Capture ITEM-TEXT, process inbox, and verify keyboard integration points.
This uses the critical org-gtd-process-inbox command while checking keyboard accessibility."
  (let ((inhibit-message t))
    ;; 1. CAPTURE (using proven pattern)
    (org-gtd-capture nil "i")
    (insert item-text)
    (org-capture-finalize)

    ;; 2. PROCESS (the critical command!)
    (org-gtd-process-inbox)

    ;; 3. VERIFY keyboard integration points
    (with-wip-buffer
      ;; Verify WIP buffer is in correct mode
      (unless org-gtd-clarify-mode
        (error "WIP buffer not in org-gtd-clarify-mode"))

      ;; Verify keyboard binding exists
      (unless (eq (lookup-key org-gtd-clarify-map (kbd "C-c c")) #'org-gtd-organize)
        (error "C-c c not bound to org-gtd-organize in WIP buffer"))

      ;; Verify org-mode integration
      (unless (derived-mode-p 'org-mode)
        (error "WIP buffer not derived from org-mode"))

      ;; Navigate to the item (critical for organization)
      (goto-char (point-min))
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1)))))

(defun ogt-verify-keyboard-and-organize-as-single-action (item-text)
  "Full end-to-end: capture ITEM-TEXT, process, verify keyboard, organize as single action."
  (ogt-capture-and-process-with-keyboard-verification item-text)
  ;; Use the proven organization pattern (no keyboard simulation!)
  (organize-as-single-action))

(defun ogt-verify-keyboard-and-organize-as-project (item-text project-tasks)
  "Full end-to-end: capture ITEM-TEXT, process, verify keyboard, organize as project.
PROJECT-TASKS should be a string with the project structure to add."
  (ogt-capture-and-process-with-keyboard-verification item-text)

  ;; Add project structure (this is what users do in WIP buffer)
  (with-wip-buffer
    (goto-char (point-max))
    (newline)
    (insert project-tasks)
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1)))

  ;; Use the proven organization pattern
  (organize-as-project))

(defun ogt-verify-keyboard-and-organize-as-calendar (item-text &optional date)
  "Full end-to-end: capture ITEM-TEXT, process, verify keyboard, organize as calendar."
  (ogt-capture-and-process-with-keyboard-verification item-text)
  ;; Use the proven organization pattern
  (schedule-item (or date (calendar-current-date))))

(defun ogt-verify-keyboard-and-organize-as-delegate (item-text &optional person date)
  "Full end-to-end: capture ITEM-TEXT, process, verify keyboard, organize as delegate."
  (ogt-capture-and-process-with-keyboard-verification item-text)
  ;; Use the proven organization pattern
  (delegate-item person date))

(defun ogt-verify-keyboard-and-organize-as-tickler (item-text &optional date)
  "Full end-to-end: capture ITEM-TEXT, process, verify keyboard, organize as tickler."
  (ogt-capture-and-process-with-keyboard-verification item-text)
  ;; Use the proven organization pattern
  (defer-item date))

(defun ogt-verify-keyboard-and-organize-as-knowledge (item-text)
  "Full end-to-end: capture ITEM-TEXT, process, verify keyboard, organize as knowledge."
  (ogt-capture-and-process-with-keyboard-verification item-text)
  ;; Knowledge requires WIP buffer interaction
  (with-wip-buffer
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    ;; Use direct function call, not keyboard simulation
    (org-gtd-knowledge)))

(defun ogt-multiple-items-with-keyboard-verification (items-and-types)
  "Process multiple items with keyboard verification.
ITEMS-AND-TYPES should be a list of (text . type) pairs where type is a symbol."
  (let ((inhibit-message t))
    ;; 1. CAPTURE multiple items
    (dolist (item items-and-types)
      (org-gtd-capture nil "i")
      (insert (car item))
      (org-capture-finalize))

    ;; 2. PROCESS all at once (this is realistic user behavior!)
    (org-gtd-process-inbox)

    ;; 3. ORGANIZE each item with keyboard verification
    (dolist (item items-and-types)
      (let ((text (car item))
            (type (cdr item)))
        ;; Verify keyboard integration for each item
        (with-wip-buffer
          ;; Verify keyboard binding exists for each organization step
          (unless (eq (lookup-key org-gtd-clarify-map (kbd "C-c c")) #'org-gtd-organize)
            (error "Keyboard binding missing during multi-item processing"))

          ;; Navigate to current item
          (goto-char (point-min))
          (when (org-before-first-heading-p)
            (org-next-visible-heading 1))

          ;; Organize based on type using proven patterns
          (cond
           ((eq type 'single-action) (organize-as-single-action))
           ((eq type 'project)
            ;; Add project structure for projects
            (goto-char (point-max))
            (newline)
            (insert "** First task\n** Second task")
            (goto-char (point-min))
            (when (org-before-first-heading-p)
              (org-next-visible-heading 1))
            (organize-as-project))
           ((eq type 'calendar) (schedule-item (calendar-current-date)))
           ((eq type 'delegate) (delegate-item "Someone" (calendar-current-date)))
           ((eq type 'tickler) (defer-item (calendar-current-date)))
           ((eq type 'knowledge) (org-gtd-knowledge))
           (t (error "Unknown organization type: %s" type))))))))

(provide 'keyboard-integration)
