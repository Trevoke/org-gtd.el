;;; project-cancel-adversarial-test.el --- Adversarial tests for project cancellation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Adversarial tests for project cancel hook feature.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))
(require 'with-simulated-input)

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

;;; Test 1: cancel-from-context with confirmation via graph view

(deftest adversarial/cancel-from-context-confirms-and-cancels ()
  "Calling org-gtd-project-cancel-from-context from graph view cancels project."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Context Cancel Project"
                          :tasks '((:description "Task 1" :status next)
                                   (:description "Task 2" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    ;; Simulate being in graph view
    (with-temp-buffer
      (setq major-mode 'org-gtd-graph-view-mode)
      (setq-local org-gtd-graph-view--project-marker project-marker)
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel-from-context)))

    ;; Project heading should be CNCL
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 1 was NEXT (incomplete), should be CNCL
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 2 was TODO (incomplete), should be CNCL
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))))

;;; Test 2: Double cancel -- calling org-gtd-project-cancel twice

(deftest adversarial/double-cancel-is-safe ()
  "Calling cancel twice on same project does not error."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Double Cancel Project"
                          :tasks '((:description "Task 1" :status next)
                                   (:description "Task 2" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    ;; First cancel
    (org-with-point-at project-marker
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Second cancel -- should not error
    (org-with-point-at project-marker
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Project heading should still be CNCL
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; All children still CNCL
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    (org-with-point-at (nth 1 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))))

;;; Test 3: Cancel project with WAIT tasks

(deftest adversarial/cancel-project-with-wait-tasks ()
  "Cancelling a project with WAIT tasks sets WAIT tasks to CNCL."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Wait Cancel Project"
                          :tasks '((:description "Task 1" :status next)
                                   (:description "Task 2" :status wait)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    (org-with-point-at project-marker
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Project heading should be CNCL
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 1 was NEXT (incomplete), should be CNCL
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 2 was WAIT (incomplete), should be CNCL
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))))

;;; Test 4: Cancel project with mixed states

(deftest adversarial/cancel-project-with-mixed-states ()
  "Cancelling a project preserves DONE and CNCL, cancels TODO/NEXT/WAIT."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Mixed States Project"
                          :tasks '((:description "Task TODO" :status todo)
                                   (:description "Task NEXT" :status next)
                                   (:description "Task WAIT" :status wait)
                                   (:description "Task DONE" :status done)
                                   (:description "Task CNCL" :status cncl)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    (org-with-point-at project-marker
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Project heading should be CNCL
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; TODO -> CNCL
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; NEXT -> CNCL
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; WAIT -> CNCL
    (org-with-point-at (nth 2 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; DONE stays DONE
    (org-with-point-at (nth 3 task-markers)
      (assert-equal "DONE" (org-get-todo-state)))

    ;; CNCL stays CNCL
    (org-with-point-at (nth 4 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))))

;;; Test 5: Hook does not fire for headings without ORG_GTD property

(deftest adversarial/hook-no-org-gtd-property-no-prompt ()
  "Setting a plain heading (no ORG_GTD) to CNCL does not trigger hook."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    ;; Insert a plain heading manually -- no ORG_GTD property at all
    (insert "* TODO Plain Heading\n")
    (forward-line -1)
    (org-back-to-heading t)
    (let ((heading-marker (point-marker)))

      (org-gtd-mode 1)
      (unwind-protect
          (org-with-point-at heading-marker
            ;; No simulated input -- if hook fires and prompts, it will error
            (let ((org-inhibit-logging 'note))
              (org-todo "CNCL")))
        (org-gtd-mode -1))

      ;; Heading should be CNCL (org-todo set it, hook did not interfere)
      (org-with-point-at heading-marker
        (assert-equal "CNCL" (org-get-todo-state))))))

;;; Test 6: Guard is nil after confirmed cancel via org-gtd-project-cancel

(deftest adversarial/guard-nil-after-confirmed-cancel ()
  "Guard variable is nil after org-gtd-project-cancel completes with confirmation."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Guard Check Project"
                          :tasks '((:description "Task 1" :status next)))))
         (project-marker (plist-get project-info :marker)))

    (org-with-point-at project-marker
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Guard should be nil after cancel completes
    (assert-nil org-gtd-project--cancel-in-progress)))

;;; Test 7: Hook confirmed cancel on project with DONE children preserves DONE

(deftest adversarial/hook-confirmed-cancel-preserves-done ()
  "Confirmed cancel via hook (org-todo CNCL under org-gtd-mode) preserves DONE children."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Hook Preserve DONE Project"
                          :tasks '((:description "Task 1" :status done)
                                   (:description "Task 2" :status next)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    (org-gtd-mode 1)
    (unwind-protect
        (org-with-point-at project-marker
          (with-simulated-input "yes RET"
            (let ((org-inhibit-logging 'note))
              (org-todo "CNCL"))))
      (org-gtd-mode -1))

    ;; Project heading should be CNCL
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 1 was DONE, should stay DONE
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "DONE" (org-get-todo-state)))

    ;; Task 2 was NEXT (incomplete), should be CNCL
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))))

(provide 'project-cancel-adversarial-test)
;;; project-cancel-adversarial-test.el ends here
