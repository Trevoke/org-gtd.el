;;; tags-filter-test.el --- Tests for tags filter predicate -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for tags predicate used in skip functions.
;; These tests verify that the tags matching predicate works correctly
;; for filtering org entries by tags.
;;

;;; Code:

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))
(require 'org-gtd-skip)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Predicate Unit Tests

(deftest tags-pred/matches-single-tag ()
  "Predicate matches when entry has the specified tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@work:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work"))))
      (assert-true (funcall pred)))))

(deftest tags-pred/matches-any-of-multiple-tags ()
  "Predicate matches when entry has any of the specified tags."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@home:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work" "@home"))))
      (assert-true (funcall pred)))))

(deftest tags-pred/no-match-when-different-tag ()
  "Predicate returns nil when entry has different tag."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@errands:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work" "@home"))))
      (assert-nil (funcall pred)))))

(deftest tags-pred/no-match-when-no-tags ()
  "Predicate returns nil when entry has no tags."
  (with-temp-buffer
    (org-mode)
    (insert "* Task\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work"))))
      (assert-nil (funcall pred)))))

(deftest tags-pred/matches-among-multiple-entry-tags ()
  "Predicate matches when entry has multiple tags including a match."
  (with-temp-buffer
    (org-mode)
    (insert "* Task :@work:urgent:important:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("urgent"))))
      (assert-true (funcall pred)))))

;;; Skip Function Integration

(deftest tags-filter/skip-function-includes-tags ()
  "Skip function builder includes tags filter predicate."
  (with-temp-buffer
    (org-mode)
    (insert "* NEXT Next Action :@work:\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (let* ((spec '((type . next-action)
                   (tags . ("@work" "@home"))))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      ;; Skip function should be a lambda (closure)
      (assert-true (functionp skip-fn))
      ;; Should NOT skip entry with matching tag
      (assert-nil (funcall skip-fn)))))

;;;; Project Tag Inheritance

(deftest tags-pred/inherits-tag-from-project-heading ()
  "Predicate matches when parent project has the tag but task does not."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (let* ((project-info
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-max))
                (make-project "Build app"
                              :tags '("@work")
                              :tasks '("Write code"))))
             (task-marker (car (plist-get project-info :task-markers))))
        (org-with-point-at task-marker
          (let ((pred (org-gtd-pred--tags-matches '("@work"))))
            (assert-true (funcall pred))))))))

(deftest tags-pred/task-own-tag-takes-precedence ()
  "Task with its own tag matches without needing project lookup."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (let* ((project-info
              (with-current-buffer (org-gtd--default-file)
                (goto-char (point-max))
                (make-project "Build app"
                              :tags '("@home")
                              :tasks '((:description "Write code" :tags ("@work"))))))
             (task-marker (car (plist-get project-info :task-markers))))
        (org-with-point-at task-marker
          (let ((pred (org-gtd-pred--tags-matches '("@work"))))
            (assert-true (funcall pred))))))))

(deftest tags-pred/no-inheritance-for-standalone-task ()
  "Standalone task without project does not inherit anything."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Task\n:PROPERTIES:\n:ORG_GTD: Actions\n:END:\n")
    (goto-char (point-min))
    (let ((pred (org-gtd-pred--tags-matches '("@work"))))
      (assert-nil (funcall pred)))))

(deftest tags-pred/inherits-from-any-project-in-multi-project ()
  "Task matches if ANY parent project has the tag."
  (let ((native-comp-enable-subr-trampolines nil))
    (ogt-eunit-with-mock-gtd
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-max))
        (let* ((proj-a (make-project "Project A" :tags '("@work")))
               (proj-b (make-project "Project B" :tags '("@home")))
               (proj-a-id (plist-get proj-a :id))
               (proj-b-id (plist-get proj-b :id)))
          (make-task "Shared task" :project-ids (list proj-a-id proj-b-id))
          (search-backward "Shared task")
          (org-back-to-heading t)
          (let ((pred (org-gtd-pred--tags-matches '("@home"))))
            (assert-true (funcall pred))))))))

(provide 'tags-filter-test)
;;; tags-filter-test.el ends here
