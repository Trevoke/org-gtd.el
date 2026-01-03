;;; project-map-test.el --- Tests for project mapping API functions -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for public API functions that map over projects and tasks:
;; - org-gtd-project-map-tasks: Apply function to tasks in a project
;; - org-gtd-projects-map: Apply function to all projects
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-projects)

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    ;; Clear org-id locations to prevent pollution from previous tests
    (setq org-id-locations nil)
    (setq org-id-files nil)
    (unwind-protect
        (funcall proceed context)
      ;; Clear org-id locations after tests
      (setq org-id-locations nil)
      (setq org-id-files nil)
      ;; Save cleared state to disk to prevent test pollution
      (org-id-locations-save))))

;;; org-gtd-project-map-tasks Tests

(deftest project-map/map-tasks-returns-collected-results ()
  "org-gtd-project-map-tasks collects results from function applied to each task."
  (let* ((temp-file (make-temp-file "project-map-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with 3 tasks in dependency chain
          (make-project "Test Project"
                        :id "proj-id"
                        :first-tasks '("task-1"))

          (make-task "Task 1"
                     :id "task-1"
                     :level 2
                     :project-ids '("proj-id")
                     :blocks '("task-2"))

          (make-task "Task 2"
                     :id "task-2"
                     :level 2
                     :project-ids '("proj-id")
                     :depends-on '("task-1")
                     :blocks '("task-3"))

          (make-task "Task 3"
                     :id "task-3"
                     :level 2
                     :project-ids '("proj-id")
                     :depends-on '("task-2"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          ;; Get project marker
          (goto-char (point-min))
          (search-forward "Test Project")
          (org-back-to-heading t)
          (let* ((project-marker (point-marker))
                 ;; Collect task headings
                 (results (org-gtd-project-map-tasks
                           (lambda () (org-get-heading t t t t))
                           project-marker)))
            ;; Should return list with 3 task headings
            (assert-equal 3 (length results))
            (assert-true (member "Task 1" results))
            (assert-true (member "Task 2" results))
            (assert-true (member "Task 3" results))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest project-map/map-tasks-excludes-nil-results ()
  "org-gtd-project-map-tasks only includes non-nil results."
  (let* ((temp-file (make-temp-file "project-map-nil-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with 3 tasks
          (make-project "Filtered Project"
                        :id "proj-id"
                        :first-tasks '("task-1"))

          (make-task "Task with Effort"
                     :id "task-1"
                     :level 2
                     :project-ids '("proj-id")
                     :properties '(("Effort" . "2:00"))
                     :blocks '("task-2"))

          (make-task "Task without Effort"
                     :id "task-2"
                     :level 2
                     :project-ids '("proj-id")
                     :depends-on '("task-1")
                     :blocks '("task-3"))

          (make-task "Another Task with Effort"
                     :id "task-3"
                     :level 2
                     :project-ids '("proj-id")
                     :properties '(("Effort" . "1:00"))
                     :depends-on '("task-2"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          ;; Get project marker
          (goto-char (point-min))
          (search-forward "Filtered Project")
          (org-back-to-heading t)
          (let* ((project-marker (point-marker))
                 ;; Collect only tasks WITH effort estimates
                 (results (org-gtd-project-map-tasks
                           (lambda ()
                             (when-let ((effort (org-entry-get nil "Effort")))
                               (cons (org-get-heading t t t t) effort)))
                           project-marker)))
            ;; Should return only 2 results (tasks with Effort property)
            (assert-equal 2 (length results))
            ;; Check that we got the right tasks
            (assert-true (assoc-string "Task with Effort" results))
            (assert-true (assoc-string "Another Task with Effort" results))
            ;; Should NOT include task without Effort
            (assert-nil (assoc-string "Task without Effort" results))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; org-gtd-projects-map Tests

(deftest project-map/projects-map-returns-alist ()
  "org-gtd-projects-map returns alist of (marker . result)."
  (let* ((temp-file (make-temp-file "projects-map-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Create 3 projects
          (make-project "Project Alpha" :id "proj-alpha")
          (make-project "Project Beta" :id "proj-beta")
          (make-project "Project Gamma" :id "proj-gamma")

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          ;; Add to agenda files for org-map-entries
          (let ((org-agenda-files (list temp-file)))
            ;; Collect project names
            (let ((results (org-gtd-projects-map
                            (lambda () (org-get-heading t t t t)))))
              ;; Should return alist with 3 entries
              (assert-equal 3 (length results))
              ;; Each entry should be (marker . heading)
              (dolist (entry results)
                (assert-true (markerp (car entry)))
                (assert-true (stringp (cdr entry))))
              ;; Check we got all projects
              (let ((project-names (mapcar #'cdr results)))
                (assert-true (member "Project Alpha" project-names))
                (assert-true (member "Project Beta" project-names))
                (assert-true (member "Project Gamma" project-names))))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest project-map/projects-map-excludes-nil-results ()
  "org-gtd-projects-map only includes non-nil results."
  (let* ((temp-file (make-temp-file "projects-map-nil-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Create 3 projects - 2 with Effort, 1 without
          (make-project "Project with Effort"
                        :id "proj-1"
                        :properties '(("Effort" . "10:00")))
          (make-project "Project without Effort"
                        :id "proj-2")
          (make-project "Another Project with Effort"
                        :id "proj-3"
                        :properties '(("Effort" . "5:00")))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          ;; Add to agenda files for org-map-entries
          (let ((org-agenda-files (list temp-file)))
            ;; Collect only projects with Effort
            (let ((results (org-gtd-projects-map
                            (lambda ()
                              (when-let ((effort (org-entry-get nil "Effort")))
                                (org-get-heading t t t t))))))
              ;; Should return only 2 results
              (assert-equal 2 (length results))
              ;; Check we got the right projects
              (let ((project-names (mapcar #'cdr results)))
                (assert-true (member "Project with Effort" project-names))
                (assert-true (member "Another Project with Effort" project-names))
                ;; Should NOT include project without Effort
                (assert-nil (member "Project without Effort" project-names))))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; org-gtd-project-last-clock-out-time Tests

(deftest project-map/last-clock-out-time-returns-nil-when-no-clocks ()
  "org-gtd-project-last-clock-out-time returns nil when no tasks have clocks."
  (let* ((temp-file (make-temp-file "clock-test-no-clocks" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with 2 tasks but no clock entries
          (make-project "Test Project"
                        :id "proj-id"
                        :first-tasks '("task-1"))

          (make-task "Task 1"
                     :id "task-1"
                     :level 2
                     :project-ids '("proj-id")
                     :blocks '("task-2"))

          (make-task "Task 2"
                     :id "task-2"
                     :level 2
                     :project-ids '("proj-id")
                     :depends-on '("task-1"))

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          ;; Get project marker
          (goto-char (point-min))
          (search-forward "Test Project")
          (org-back-to-heading t)
          (let* ((project-marker (point-marker))
                 (result (org-gtd-project-last-clock-out-time project-marker)))
            ;; Should return nil since no tasks have clocks
            (assert-nil result)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest project-map/last-clock-out-time-returns-most-recent ()
  "org-gtd-project-last-clock-out-time returns most recent clock-out time."
  (let* ((temp-file (make-temp-file "clock-test-with-clocks" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with 3 tasks, each with different clock times
          (make-project "Test Project"
                        :id "proj-id"
                        :first-tasks '("task-1"))

          (make-task "Task 1"
                     :id "task-1"
                     :level 2
                     :project-ids '("proj-id")
                     :blocks '("task-2"))

          ;; Add LOGBOOK with old clock entry to Task 1
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-20 Fri 14:30]--[2025-12-20 Fri 15:45] =>  1:15\n")
          (insert ":END:\n")

          (make-task "Task 2"
                     :id "task-2"
                     :level 2
                     :project-ids '("proj-id")
                     :depends-on '("task-1")
                     :blocks '("task-3"))

          ;; Add LOGBOOK with most recent clock entry to Task 2
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-22 Sun 10:00]--[2025-12-22 Sun 11:30] =>  1:30\n")
          (insert ":END:\n")

          (make-task "Task 3"
                     :id "task-3"
                     :level 2
                     :project-ids '("proj-id")
                     :depends-on '("task-2"))

          ;; Add LOGBOOK with middle clock entry to Task 3
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-21 Sat 16:00]--[2025-12-21 Sat 17:00] =>  1:00\n")
          (insert ":END:\n")

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          ;; Get project marker
          (goto-char (point-min))
          (search-forward "Test Project")
          (org-back-to-heading t)
          (let* ((project-marker (point-marker))
                 (result (org-gtd-project-last-clock-out-time project-marker))
                 ;; Expected result: most recent time from Task 2
                 (expected-time (encode-time 0 30 11 22 12 2025)))
            ;; Should return the most recent clock-out time
            (assert-true result)
            ;; Compare times (allowing for timezone/DST differences)
            (assert-equal (format-time-string "%Y-%m-%d %H:%M" expected-time)
                         (format-time-string "%Y-%m-%d %H:%M" result))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'project-map-test)

;;; project-map-test.el ends here
