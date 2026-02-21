;;; stuck-type-filter-test.el --- Tests for stuck-type and generic path inactive project filtering -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests that the stuck-type skip function and generic predicate-composition
;; path correctly exclude tasks from cancelled/done projects.
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

(deftest stuck-type/cancelled-project-delegated-task-skipped ()
  "Stuck delegated task from a cancelled project is skipped."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Cancelled Project"
                          :tasks '((:description "Delegated Task" :status todo)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))
    ;; Make the task look like a delegated item (ORG_GTD=Delegated, no DELEGATED_TO = stuck)
    (org-with-point-at (nth 0 task-markers)
      (org-entry-put (point) "ORG_GTD" "Delegated"))
    ;; Set project heading to CNCL
    (org-with-point-at project-marker
      (let ((org-inhibit-logging 'note))
        (org-todo "CNCL")))
    ;; Now test the skip function on the task
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-stuck-type 'delegated)))
      (org-with-point-at (nth 0 task-markers)
        (assert-true (funcall skip-fn))))))

(deftest stuck-type/active-project-stuck-delegated-included ()
  "Stuck delegated task from an active project is included."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Active Project"
                          :tasks '((:description "Delegated Task" :status todo)))))
         (task-markers (plist-get project-info :task-markers)))
    ;; Make the task look like a delegated item (ORG_GTD=Delegated, no DELEGATED_TO = stuck)
    (org-with-point-at (nth 0 task-markers)
      (org-entry-put (point) "ORG_GTD" "Delegated"))
    ;; Project is active (default state, no TODO keyword on heading)
    ;; Task is stuck (missing DELEGATED_TO)
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-stuck-type 'delegated)))
      (org-with-point-at (nth 0 task-markers)
        (assert-nil (funcall skip-fn))))))

(deftest generic-path/cancelled-project-task-skipped ()
  "Task from a cancelled project is skipped by the generic predicate path."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Cancelled Project"
                          :tasks '((:description "Task 1" :status next)))))
         (project-marker (plist-get project-info :marker))
         (task-markers (plist-get project-info :task-markers)))
    ;; Set project heading to CNCL
    (org-with-point-at project-marker
      (let ((org-inhibit-logging 'note))
        (org-todo "CNCL")))
    ;; Build skip function via the generic path
    (let ((skip-fn (org-gtd-view-lang--build-skip-function '((type . next-action)))))
      (org-with-point-at (nth 0 task-markers)
        (assert-true (funcall skip-fn))))))

(deftest generic-path/active-project-task-included ()
  "Task from an active project is included by the generic predicate path."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Active Project"
                          :tasks '((:description "Task 1" :status next)))))
         (task-markers (plist-get project-info :task-markers)))
    ;; Project is active (default state, no TODO keyword on heading)
    ;; Build skip function via the generic path
    (let ((skip-fn (org-gtd-view-lang--build-skip-function '((type . next-action)))))
      (org-with-point-at (nth 0 task-markers)
        (assert-nil (funcall skip-fn))))))

(provide 'stuck-type-filter-test)
;;; stuck-type-filter-test.el ends here
