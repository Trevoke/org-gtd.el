;;; project-cancel-confirm-test.el --- Tests for project cancel confirmation -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the confirmation prompt and heading cancellation behavior
;; of org-gtd-project-cancel.
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

(deftest cancel-confirm/confirm-and-cancel ()
  "Confirming cancellation sets heading and incomplete children to CNCL."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "My Important Project"
                          :tasks '((:description "Task 1" :status done)
                                   (:description "Task 2" :status next)
                                   (:description "Task 3" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    (org-with-point-at project-marker
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Project heading should be CNCL
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 1 was DONE, should stay DONE
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "DONE" (org-get-todo-state)))

    ;; Task 2 was NEXT (incomplete), should be CNCL
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 3 was TODO (incomplete), should be CNCL
    (org-with-point-at (nth 2 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))))

(deftest cancel-confirm/decline-and-no-change ()
  "Declining cancellation leaves heading and children unchanged."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Keep This Project"
                          :tasks '((:description "Task 1" :status next)
                                   (:description "Task 2" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    (org-with-point-at project-marker
      (with-simulated-input "no RET"
        (org-gtd-project-cancel)))

    ;; Project heading should NOT be CNCL
    (org-with-point-at project-marker
      (assert-nil (org-get-todo-state)))

    ;; Task 1 should remain NEXT
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "NEXT" (org-get-todo-state)))

    ;; Task 2 should remain TODO
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "TODO" (org-get-todo-state)))))

(deftest cancel-confirm/already-cncl-heading ()
  "Cancelling a project whose heading is already CNCL still cancels children."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Already Canceled"
                          :tasks '((:description "Task 1" :status next)
                                   (:description "Task 2" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    ;; Manually set project heading to CNCL first
    (org-with-point-at project-marker
      (let ((org-inhibit-logging 'note))
        (org-todo "CNCL")))

    (org-with-point-at project-marker
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Project heading should still be CNCL
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 1 was NEXT, should be CNCL
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Task 2 was TODO, should be CNCL
    (org-with-point-at (nth 1 task-markers)
      (assert-equal "CNCL" (org-get-todo-state)))))

(deftest cancel-confirm/no-incomplete-children ()
  "Cancelling a project with all DONE tasks sets heading to CNCL without errors."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "All Done Project"
                          :tasks '((:description "Task 1" :status done)
                                   (:description "Task 2" :status done)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))

    (org-with-point-at project-marker
      (with-simulated-input "yes RET"
        (org-gtd-project-cancel)))

    ;; Project heading should be CNCL
    (org-with-point-at project-marker
      (assert-equal "CNCL" (org-get-todo-state)))

    ;; Tasks should remain DONE
    (org-with-point-at (nth 0 task-markers)
      (assert-equal "DONE" (org-get-todo-state)))

    (org-with-point-at (nth 1 task-markers)
      (assert-equal "DONE" (org-get-todo-state)))))

(provide 'project-cancel-confirm-test)
;;; project-cancel-confirm-test.el ends here
