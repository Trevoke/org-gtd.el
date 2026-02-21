;;; project-type-filter-test.el --- Tests for project-type skip function done/cancelled filtering -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests that the project-type skip function correctly excludes
;; done/cancelled projects from stuck-project and active-project views.
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

(deftest project-type/cncl-project-skipped-from-stuck-view ()
  "CNCL project is skipped by stuck-project skip function."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Cancelled Project"
                          :tasks '((:description "Task 1" :status todo)))))
         (project-marker (plist-get project-info :marker)))
    ;; Set project heading to CNCL
    (org-with-point-at project-marker
      (let ((org-inhibit-logging 'note))
        (org-todo "CNCL")))
    ;; Now test the skip function on the project heading
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-project-type 'stuck-project)))
      (org-with-point-at project-marker
        (assert-true (funcall skip-fn))))))

(deftest project-type/done-project-skipped-from-stuck-view ()
  "DONE project is skipped by stuck-project skip function."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Done Project"
                          :tasks '((:description "Task 1" :status todo)))))
         (project-marker (plist-get project-info :marker)))
    ;; Set project heading to DONE
    (org-with-point-at project-marker
      (let ((org-inhibit-logging 'note))
        (org-todo "DONE")))
    ;; Now test the skip function on the project heading
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-project-type 'stuck-project)))
      (org-with-point-at project-marker
        (assert-true (funcall skip-fn))))))

(deftest project-type/cncl-project-skipped-from-active-view ()
  "CNCL project is skipped by active-project skip function."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Cancelled Project"
                          :tasks '((:description "Task 1" :status next)))))
         (project-marker (plist-get project-info :marker)))
    ;; Set project heading to CNCL
    (org-with-point-at project-marker
      (let ((org-inhibit-logging 'note))
        (org-todo "CNCL")))
    ;; Now test the skip function on the project heading
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-project-type 'active-project)))
      (org-with-point-at project-marker
        (assert-true (funcall skip-fn))))))

(deftest project-type/active-project-with-tasks-included-in-stuck-view ()
  "Active project with TODO tasks (stuck) is included by stuck-project skip function."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Active Project"
                          :tasks '((:description "Task 1" :status todo)))))
         (project-marker (plist-get project-info :marker)))
    ;; Project is active (default state, no TODO keyword on heading)
    ;; Task is in TODO state (not NEXT), so the project is stuck
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-project-type 'stuck-project)))
      (org-with-point-at project-marker
        (assert-nil (funcall skip-fn))))))

(deftest project-type/active-project-included-in-active-view ()
  "Active project with active tasks is included by active-project skip function."
  (let* ((project-info
          (with-current-buffer (org-gtd--default-file)
            (goto-char (point-max))
            (make-project "Active Project"
                          :tasks '((:description "Task 1" :status next)))))
         (project-marker (plist-get project-info :marker)))
    ;; Project is active (default state, no TODO keyword on heading)
    ;; Task is in NEXT state (active)
    (let ((skip-fn (org-gtd-view-lang--build-skip-function-for-project-type 'active-project)))
      (org-with-point-at project-marker
        (assert-nil (funcall skip-fn))))))

(provide 'project-type-filter-test)
;;; project-type-filter-test.el ends here
