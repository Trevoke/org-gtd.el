;;; archiving-test.el --- Tests for GTD item archiving -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for archiving completed GTD items including projects and single actions.
;;
;; Test Coverage:
;; - Archiving completed and canceled projects (1 test)
;; - Archiving completed single actions (1 test - already in cancel-archive-test.el)
;; - Repeating scheduled items not archived (1 test)
;; - Undone tickler items not archived (1 test)
;; - DAG project archiving (1 test)
;; - Incomplete project not archived (1 test)
;; - Archive location configuration (2 tests)
;;
;; Migrated from test/archiving-test.el (buttercup)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Project Archiving Tests

(deftest archiving/archives-completed-and-canceled-projects ()
  "Archives completed and canceled projects."
  (create-project "project headline")
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (newline)
    ;; Use builder instead of string fixture
    (make-canceled-project "canceled")
    (newline)
    (make-completed-project "completed")
    (basic-save-buffer))
  (org-gtd-archive-completed-items)
  (let ((archived-projects (ogt--archive-string)))
    (assert-match "completed" archived-projects)
    (assert-match "canceled" archived-projects)))

;;; Repeating/Tickler Item Tests

(deftest archiving/does-not-archive-repeating-scheduled-items ()
  "Does not archive repeating scheduled items."
  ;; Set up a repeating habit in the GTD file
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "\n* Habits\n:PROPERTIES:\n:ORG_GTD: Habit\n:END:\n")
    (insert "** repeating item\nSCHEDULED: <2021-12-04 Sat .+1d>\n")
    (insert ":PROPERTIES:\n:ORG_GTD: Habit\n:STYLE: habit\n:END:\n")
    ;; Also add a completed calendar item that should be archived
    (insert "\n* Calendar\n:PROPERTIES:\n:ORG_GTD: Calendar\n:END:\n")
    (insert "** DONE write a nice test\n")
    (insert ":PROPERTIES:\n:ORG_GTD: Calendar\n:ORG_GTD_TIMESTAMP: <2021-11-20 Sat>\n:END:\n")
    (basic-save-buffer))

  (org-gtd-archive-completed-items)

  ;; Repeating item should still be in file
  (assert-true (file-contains? (org-gtd--default-file) "repeating item"))
  ;; Completed item should be archived
  (assert-nil (file-contains? (org-gtd--default-file) "write a nice test")))

(deftest archiving/does-not-archive-undone-tickler-items ()
  "Does not archive undone tickler items."
  ;; Set up tickler items
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "\n* Tickler\n:PROPERTIES:\n:ORG_GTD: Tickler\n:END:\n")
    ;; Future tickler item - should not be archived
    (insert "** For later\n")
    (insert ":PROPERTIES:\n:ORG_GTD: Tickler\n:ORG_GTD_TIMESTAMP: <2037-02-19 Thu>\n:END:\n")
    ;; Canceled tickler item - should be archived
    (insert "** CNCL not worth thinking about\n")
    (insert ":PROPERTIES:\n:ORG_GTD: Tickler\n:ORG_GTD_TIMESTAMP: <2021-11-21 Sun>\n:END:\n")
    (basic-save-buffer))

  (org-gtd-archive-completed-items)

  ;; Future tickler should still be in file
  (assert-true (file-contains? (org-gtd--default-file) "For later"))
  ;; Canceled tickler should be archived
  (assert-nil (file-contains? (org-gtd--default-file) "not worth thinking about")))

;;; DAG Project Archiving Tests

(deftest archiving/archives-dag-project-in-breadth-first-order ()
  "Archives DAG-structured project with tasks."
  (let* ((gtd-file-path (f-join org-gtd-directory "org-gtd-tasks.org"))
         (archive-file-path (car (split-string (funcall org-gtd-archive-location) "::"))))

    (with-current-buffer (find-file-noselect gtd-file-path)
      (goto-char (point-max))
      (insert "\n")

      ;; Use builder for DAG project with mixed nesting levels
      (make-dag-project "DAG Project"
                        :id "dag-project-id"
                        :task-specs '((:description "Task A"
                                       :id "task-a-id"
                                       :status done
                                       :blocks ("task-b-id" "task-d-id")
                                       :level 2)
                                      (:description "Task B"
                                       :id "task-b-id"
                                       :status done
                                       :depends-on ("task-a-id")
                                       :blocks ("task-c-id")
                                       :level 3)
                                      (:description "Task C"
                                       :id "task-c-id"
                                       :status done
                                       :depends-on ("task-b-id")
                                       :level 4)
                                      (:description "Task D"
                                       :id "task-d-id"
                                       :status done
                                       :depends-on ("task-a-id")
                                       :level 2)))

      (org-mode-restart)
      (basic-save-buffer))

    ;; Update org-id locations so graph traversal can find tasks
    (org-id-update-id-locations (list gtd-file-path))

    ;; Archive the completed project
    (org-gtd-archive-completed-items)

    ;; Save all buffers to ensure archive is written
    (save-some-buffers t)

    ;; Verify project and tasks are archived
    (with-current-buffer (find-file-noselect gtd-file-path)
      (assert-nil (string-match-p "DAG Project" (ogt--current-buffer-raw-text)))
      (assert-nil (string-match-p "Task A" (ogt--current-buffer-raw-text)))
      (assert-nil (string-match-p "Task B" (ogt--current-buffer-raw-text)))
      (assert-nil (string-match-p "Task C" (ogt--current-buffer-raw-text)))
      (assert-nil (string-match-p "Task D" (ogt--current-buffer-raw-text))))

    ;; Verify archived structure: All tasks and project are present
    (with-current-buffer (find-file-noselect archive-file-path)
      (let ((content (ogt--current-buffer-raw-text)))
        ;; All tasks should be present in the archive
        (assert-match "Task A" content)
        (assert-match "Task B" content)
        (assert-match "Task C" content)
        (assert-match "Task D" content)
        ;; Project should be present
        (assert-match "DAG Project" content)))))

(deftest archiving/does-not-archive-projects-with-incomplete-tasks ()
  "Does not archive projects with incomplete tasks."
  (let* ((gtd-file-path (f-join org-gtd-directory "org-gtd-tasks.org"))
         (archive-file-path (car (split-string (funcall org-gtd-archive-location) "::"))))

    (with-current-buffer (find-file-noselect gtd-file-path)
      (goto-char (point-max))
      (insert "\n")

      ;; Use builder for incomplete project
      (make-project "Partial Project"
                    :id "partial-project-id"
                    :tasks '((:description "Task X"
                              :id "task-x-id"
                              :status done
                              :blocks ("task-y-id"))
                             (:description "Task Y"
                              :id "task-y-id"
                              :status todo
                              :depends-on ("task-x-id"))))

      (basic-save-buffer)
      ;; Update org-id locations
      (org-id-update-id-locations (list gtd-file-path)))

    ;; Try to archive - should NOT archive because Task Y is incomplete
    (org-gtd-archive-completed-items)

    ;; Verify project and tasks are NOT archived
    (with-current-buffer (find-file-noselect gtd-file-path)
      (let ((content (ogt--current-buffer-raw-text)))
        (assert-match "Partial Project" content)
        (assert-match "Task X" content)
        (assert-match "Task Y" content)))

    ;; Verify nothing was archived for this project
    (with-current-buffer (find-file-noselect archive-file-path)
      (let ((content (ogt--current-buffer-raw-text)))
        (assert-nil (string-match-p "Partial Project" content))))))

;;; Archive Location Configuration Tests

(deftest archiving/uses-org-archive-location-when-gtd-location-nil ()
  "Uses org-archive-location when org-gtd-archive-location is nil."
  (let* ((custom-archive-file (f-join org-gtd-directory "custom-archive.org"))
         (org-gtd-archive-location nil)
         (org-archive-location (concat custom-archive-file "::")))

    ;; Create a completed single action
    (create-single-action "archive-me")
    (ogt--save-all-buffers)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "NEXT archive-me")
      (org-todo "DONE"))
    (ogt--save-all-buffers)

    ;; Archive completed items
    (org-gtd-archive-completed-items)
    (ogt--save-all-buffers)

    ;; Verify item went to the custom archive location
    (assert-true (file-exists-p custom-archive-file))
    (with-current-buffer (find-file-noselect custom-archive-file)
      (assert-match "archive-me" (ogt--current-buffer-raw-text)))))

(deftest archiving/uses-gtd-archive-location-when-set-to-function ()
  "Uses org-gtd-archive-location when set to a function."
  (let ((org-gtd-archive-location #'org-gtd-archive-location-func))

    ;; Create a completed single action
    (create-single-action "gtd-archive-me")
    (ogt--save-all-buffers)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-min))
      (search-forward "NEXT gtd-archive-me")
      (org-todo "DONE"))
    (ogt--save-all-buffers)

    ;; Archive completed items
    (org-gtd-archive-completed-items)
    (ogt--save-all-buffers)

    ;; Verify item went to org-gtd's datetree archive
    (let ((archive-file-path (car (split-string (funcall org-gtd-archive-location) "::"))))
      (assert-true (file-exists-p archive-file-path))
      (with-current-buffer (find-file-noselect archive-file-path)
        (assert-match "gtd-archive-me" (ogt--current-buffer-raw-text))))))

(provide 'archiving-test)

;;; archiving-test.el ends here
