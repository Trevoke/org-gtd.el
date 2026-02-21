;;; project-cancel-hook-test.el --- Tests for project cancel hook detection -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-project--maybe-cancel-from-hook.
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

;;; Tests

(deftest hook/manual-cncl-confirmed ()
  "Manually setting project to CNCL and confirming cancels child tasks."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "My Important Project"
                          :tasks '((:description "Task 1" :status next)
                                   (:description "Task 2" :status todo)))))
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

    ;; Task 1 was NEXT (incomplete), should be CNCL
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 2 was TODO (incomplete), should be CNCL
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))))

(deftest hook/manual-cncl-declined ()
  "Declining cancellation reverts the project heading and leaves children unchanged."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Keep This Project"
                          :tasks '((:description "Task 1" :status next)
                                   (:description "Task 2" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    (org-gtd-mode 1)
    (unwind-protect
        (org-with-point-at project-marker
          (with-simulated-input "no RET"
            (let ((org-inhibit-logging 'note))
              (org-todo "CNCL"))))
      (org-gtd-mode -1))

    ;; Project heading should NOT be CNCL (reverted)
    (org-with-point-at project-marker
      (assert-not-equal "CNCL" (org-get-todo-state)))

    ;; Task 1 should remain NEXT
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "NEXT" (org-get-todo-state)))

    ;; Task 2 should remain TODO
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "TODO" (org-get-todo-state)))))

(deftest hook/guard-suppresses-hook ()
  "Guard variable prevents hook from prompting or canceling children."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Guarded Project"
                          :tasks '((:description "Task 1" :status next)
                                   (:description "Task 2" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    (org-gtd-mode 1)
    (unwind-protect
        (let ((org-gtd-project--cancel-in-progress t))
          (org-with-point-at project-marker
            (let ((org-inhibit-logging 'note))
              (org-todo "CNCL"))))
      (org-gtd-mode -1))

    ;; Project heading should be CNCL (set directly by org-todo)
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Children should be unchanged (hook was suppressed)
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "NEXT" (org-get-todo-state)))

    (org-with-point-at (nth 1 task-markers)
      (assert-equal "TODO" (org-get-todo-state)))))

(deftest hook/non-project-heading-ignored ()
  "Hook does not fire for non-project headings (e.g., single actions)."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (let ((task-marker (make-task "Single Action Task"
                                  :status 'next
                                  :level 1)))
      (org-gtd-mode 1)
      (unwind-protect
          (org-with-point-at task-marker
            ;; No simulated input -- if hook prompts, it will error
            (let ((org-inhibit-logging 'note))
              (org-todo "CNCL")))
        (org-gtd-mode -1))

      ;; Heading should be CNCL (org-todo set it, hook did nothing)
      (org-with-point-at task-marker
        (assert-equal "CNCL" (org-get-todo-state))))))

(deftest hook/guard-cleared-after-revert ()
  "Guard variable is cleared after declining cancellation."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Revert Guard Project"
                          :tasks '((:description "Task 1" :status next)))))
         (project-marker (plist-get project-info :marker)))

    (org-gtd-mode 1)
    (unwind-protect
        (progn
          (org-with-point-at project-marker
            (with-simulated-input "no RET"
              (let ((org-inhibit-logging 'note))
                (org-todo "CNCL"))))
          ;; Guard should be nil after the revert completes
          (assert-nil org-gtd-project--cancel-in-progress))
      (org-gtd-mode -1))))

(provide 'project-cancel-hook-test)
;;; project-cancel-hook-test.el ends here
