;;; area-of-focus-filter-test.el --- Tests for area-of-focus filter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for area-of-focus predicate with project inheritance.

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))
(require 'org-gtd-skip)

(e-unit-initialize)

;;;; Direct matching

(deftest aof-pred/matches-direct-category ()
  "Predicate matches when item has CATEGORY set directly."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:CATEGORY: Work\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
      (assert-true (funcall pred)))))

(deftest aof-pred/no-match-different-category ()
  "Predicate returns nil when CATEGORY differs."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:CATEGORY: Health\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
      (assert-nil (funcall pred)))))

;;;; Project inheritance

(deftest aof-pred/inherits-category-from-project ()
  "Predicate matches when parent project has CATEGORY but task does not."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (let* ((project-info
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-max))
                (make-project "Build app"
                              :properties '(("CATEGORY" . "Work"))
                              :tasks '("Write code"))))
             (task-marker (car (plist-get project-info :task-markers))))
        (org-with-point-at task-marker
          (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
            (assert-true (funcall pred))))))))

(deftest aof-pred/task-category-wins-over-project ()
  "Task's own CATEGORY takes precedence over project's."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-max))
        (let* ((project-info
                (make-project "Build app"
                              :properties '(("CATEGORY" . "Work"))
                              :tasks '("Write code")))
               (task-marker (car (plist-get project-info :task-markers))))
          ;; Set CATEGORY directly on the task
          (org-with-point-at task-marker
            (org-entry-put (point) "CATEGORY" "Health")
            (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
              (assert-nil (funcall pred)))))))))

(deftest aof-pred/standalone-task-no-inheritance ()
  "Standalone task without project has no inheritance."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--area-of-focus-matches "Work")))
      (assert-nil (funcall pred)))))

(provide 'area-of-focus-filter-test)
;;; area-of-focus-filter-test.el ends here
