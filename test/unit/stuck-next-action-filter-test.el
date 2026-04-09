;;; stuck-next-action-filter-test.el --- Tests for stuck-next-action inactive project filtering -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests that the stuck-next-action skip function correctly excludes
;; tasks from cancelled/done projects.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))
(require 'org-gtd-view-language)

(e-unit-initialize)

(around-each (proceed context)
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (funcall proceed context))))

(deftest stuck-na-filter/cancelled-project-task-skipped ()
  "Task from a cancelled project is skipped by stuck-next-action filter."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Cancelled Project"
                          :tasks '((:description "Task 1" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))
    ;; Set project heading to CNCL
    (org-with-point-at project-marker
      (let ((org-inhibit-logging 'note))
        (org-todo "CNCL")))
    ;; Now test the skip function on the task
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-stuck-next-action)))
      (org-with-point-at (nth 0 task-markers)
        (assert-true (funcall skip-fn))))))

(deftest stuck-na-filter/active-project-stuck-task-included ()
  "Task from an active project in TODO state (stuck) is included."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Active Project"
                          :tasks '((:description "Task 1" :status todo)))))
         (task-markers (plist-get project-info :task-markers)))
    ;; Project is active (default state, no TODO keyword on heading)
    ;; Task is in TODO state (not NEXT), so it is stuck
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-stuck-next-action)))
      (org-with-point-at (nth 0 task-markers)
        (assert-nil (funcall skip-fn))))))

(deftest stuck-na-filter/active-project-next-task-skipped ()
  "Task from an active project in NEXT state (not stuck) is skipped."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Active Project"
                          :tasks '((:description "Task 1" :status next)))))
         (task-markers (plist-get project-info :task-markers)))
    ;; Project is active, task is in NEXT state (not stuck)
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-stuck-next-action)))
      (org-with-point-at (nth 0 task-markers)
        (assert-true (funcall skip-fn))))))

(deftest stuck-na-filter/done-project-task-skipped ()
  "Task from a DONE project is skipped by stuck-next-action filter."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Done Project"
                          :tasks '((:description "Task 1" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))
    ;; Set project heading to DONE
    (org-with-point-at project-marker
      (let ((org-inhibit-logging 'note))
        (org-todo "DONE")))
    ;; Now test the skip function on the task
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-stuck-next-action)))
      (org-with-point-at (nth 0 task-markers)
        (assert-true (funcall skip-fn))))))

(deftest stuck-na-filter/standalone-stuck-task-included ()
  "A stuck single action with no project is included."
  (let ((task-marker
         (with-current-buffer (org-gtd--default-file)
           (goto-char (point-max))
           (make-task "Orphan stuck task" :status 'todo :level 1))))
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-stuck-next-action)))
      (org-with-point-at task-marker
        (assert-nil (funcall skip-fn))))))

(provide 'stuck-next-action-filter-test)
;;; stuck-next-action-filter-test.el ends here
