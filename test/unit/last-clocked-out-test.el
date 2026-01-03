;;; last-clocked-out-test.el --- E-unit tests for last-clocked-out predicates -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2025 Aldric Giacomoni

;;; Commentary:
;;
;; E-unit tests for org-gtd--parse-relative-time and
;; org-gtd-pred--last-clocked-out-matches predicate.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-skip)
(require 'org-clock)

;; Note: e-unit-initialize is called by prelude

;;;; org-gtd--parse-relative-time tests

(deftest last-clocked-out/parse-relative-time-days ()
  "Parses days correctly (e.g., '2d' = 172800 seconds)."
  (assert-equal 172800 (org-gtd--parse-relative-time "2d")))

(deftest last-clocked-out/parse-relative-time-weeks ()
  "Parses weeks correctly (e.g., '1w' = 604800 seconds)."
  (assert-equal 604800 (org-gtd--parse-relative-time "1w")))

(deftest last-clocked-out/parse-relative-time-hours ()
  "Parses hours correctly (e.g., '3h' = 10800 seconds)."
  (assert-equal 10800 (org-gtd--parse-relative-time "3h")))

(deftest last-clocked-out/parse-relative-time-minutes ()
  "Parses minutes correctly (e.g., '30m' = 1800 seconds)."
  (assert-equal 1800 (org-gtd--parse-relative-time "30m")))

(deftest last-clocked-out/parse-relative-time-unknown-unit-errors ()
  "Throws error for unknown time unit."
  (assert-raises 'error
    (org-gtd--parse-relative-time "5x")))

;;;; org-gtd-pred--last-clocked-out-matches tests

(deftest last-clocked-out/predicate-handles-invalid-time-format ()
  "Predicate returns nil (doesn't crash) when given invalid time format like '2x'."
  (let* ((temp-file (make-temp-file "clock-pred-invalid" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Task with clock entry
          (make-task "Clocked Task"
                     :id "task-1"
                     :level 1)

          ;; Add LOGBOOK with old clock entry
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-20 Fri 14:30]--[2025-12-20 Fri 15:45] =>  1:15\n")
          (insert ":END:\n")

          (org-mode-restart)
          (basic-save-buffer)

          ;; Test the predicate with invalid time format
          (goto-char (point-min))
          (search-forward "Clocked Task")
          (org-back-to-heading t)

          ;; Should return nil instead of throwing error
          (let ((predicate (org-gtd-pred--last-clocked-out-matches '(> "2x"))))
            (assert-nil (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;; Initialize e-unit short syntax
(e-unit-initialize)

;; Wrap each test in mock GTD filesystem
(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest last-clocked-out/predicate-nil-matches-never-clocked ()
  "Predicate with nil matches tasks that have never been clocked."
  (let* ((temp-file (make-temp-file "clock-pred-test" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Task with no clock entries
          (make-task "Never Clocked Task"
                     :id "task-1"
                     :level 1)

          (org-mode-restart)
          (basic-save-buffer)

          ;; Test the predicate
          (goto-char (point-min))
          (search-forward "Never Clocked Task")
          (org-back-to-heading t)

          (let ((predicate (org-gtd-pred--last-clocked-out-matches nil)))
            (assert-true (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest last-clocked-out/predicate-nil-does-not-match-clocked ()
  "Predicate with nil does not match tasks that have been clocked."
  (let* ((temp-file (make-temp-file "clock-pred-test2" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Task with clock entry
          (make-task "Clocked Task"
                     :id "task-1"
                     :level 1)

          ;; Add LOGBOOK with clock entry
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-22 Sun 10:00]--[2025-12-22 Sun 11:30] =>  1:30\n")
          (insert ":END:\n")

          (org-mode-restart)
          (basic-save-buffer)

          ;; Test the predicate
          (goto-char (point-min))
          (search-forward "Clocked Task")
          (org-back-to-heading t)

          (let ((predicate (org-gtd-pred--last-clocked-out-matches nil)))
            (assert-nil (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest last-clocked-out/predicate-greater-matches-old-clock ()
  "Predicate with (> \"1d\") matches tasks clocked out more than 1 day ago."
  (let* ((temp-file (make-temp-file "clock-pred-test3" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Task with old clock entry (2 days ago)
          (make-task "Old Clocked Task"
                     :id "task-1"
                     :level 1)

          ;; Add LOGBOOK with old clock entry
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-20 Fri 14:30]--[2025-12-20 Fri 15:45] =>  1:15\n")
          (insert ":END:\n")

          (org-mode-restart)
          (basic-save-buffer)

          ;; Test the predicate
          (goto-char (point-min))
          (search-forward "Old Clocked Task")
          (org-back-to-heading t)

          (let ((predicate (org-gtd-pred--last-clocked-out-matches '(> "1d"))))
            (assert-true (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest last-clocked-out/predicate-greater-does-not-match-recent ()
  "Predicate with (> \"1d\") does not match tasks clocked out recently."
  (let* ((temp-file (make-temp-file "clock-pred-test4" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Task with recent clock entry (just now)
          (make-task "Recent Clocked Task"
                     :id "task-1"
                     :level 1)

          ;; Add LOGBOOK with very recent clock entry
          (let ((now (current-time)))
            (insert ":LOGBOOK:\n")
            (insert (format "CLOCK: [%s]--[%s] =>  0:30\n"
                           (format-time-string "%Y-%m-%d %a %H:%M" (time-subtract now 1800))
                           (format-time-string "%Y-%m-%d %a %H:%M" now)))
            (insert ":END:\n"))

          (org-mode-restart)
          (basic-save-buffer)

          ;; Test the predicate
          (goto-char (point-min))
          (search-forward "Recent Clocked Task")
          (org-back-to-heading t)

          (let ((predicate (org-gtd-pred--last-clocked-out-matches '(> "1d"))))
            (assert-nil (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest last-clocked-out/predicate-less-matches-recent ()
  "Predicate with (< \"1w\") matches tasks clocked out within the past week."
  (let* ((temp-file (make-temp-file "clock-pred-test5" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Task with recent clock entry
          (make-task "Recent Clocked Task"
                     :id "task-1"
                     :level 1)

          ;; Add LOGBOOK with clock entry from yesterday (dynamically calculated)
          (let ((yesterday (time-subtract (current-time) 86400))) ; 1 day ago
            (insert ":LOGBOOK:\n")
            (insert (format "CLOCK: [%s]--[%s] =>  1:30\n"
                           (format-time-string "%Y-%m-%d %a 10:00" yesterday)
                           (format-time-string "%Y-%m-%d %a 11:30" yesterday)))
            (insert ":END:\n"))

          (org-mode-restart)
          (basic-save-buffer)

          ;; Test the predicate
          (goto-char (point-min))
          (search-forward "Recent Clocked Task")
          (org-back-to-heading t)

          (let ((predicate (org-gtd-pred--last-clocked-out-matches '(< "1w"))))
            (assert-true (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest last-clocked-out/predicate-less-does-not-match-old ()
  "Predicate with (< \"1d\") does not match tasks clocked out more than 1 day ago."
  (let* ((temp-file (make-temp-file "clock-pred-test6" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Task with old clock entry
          (make-task "Old Clocked Task"
                     :id "task-1"
                     :level 1)

          ;; Add LOGBOOK with old clock entry (2 days ago)
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-20 Fri 14:30]--[2025-12-20 Fri 15:45] =>  1:15\n")
          (insert ":END:\n")

          (org-mode-restart)
          (basic-save-buffer)

          ;; Test the predicate
          (goto-char (point-min))
          (search-forward "Old Clocked Task")
          (org-back-to-heading t)

          (let ((predicate (org-gtd-pred--last-clocked-out-matches '(< "1d"))))
            (assert-nil (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest last-clocked-out/predicate-project-nil-matches-never-clocked ()
  "Predicate with nil matches projects with no clocked tasks."
  (let* ((temp-file (make-temp-file "clock-pred-proj1" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with tasks but no clock entries
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

          ;; Test the predicate on the project heading
          (goto-char (point-min))
          (search-forward "Test Project")
          (org-back-to-heading t)

          (let ((predicate (org-gtd-pred--last-clocked-out-matches nil)))
            (assert-true (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(deftest last-clocked-out/predicate-project-greater-matches-old ()
  "Predicate with (> \"1d\") matches projects with tasks clocked out more than 1 day ago."
  (let* ((temp-file (make-temp-file "clock-pred-proj2" nil ".org"))
         (buf (find-file-noselect temp-file)))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          ;; Project with tasks with clock entries
          (make-project "Test Project"
                        :id "proj-id"
                        :first-tasks '("task-1"))

          (make-task "Task 1"
                     :id "task-1"
                     :level 2
                     :project-ids '("proj-id")
                     :blocks '("task-2"))

          ;; Add old clock entry to Task 1
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-20 Fri 14:30]--[2025-12-20 Fri 15:45] =>  1:15\n")
          (insert ":END:\n")

          (make-task "Task 2"
                     :id "task-2"
                     :level 2
                     :project-ids '("proj-id")
                     :depends-on '("task-1"))

          ;; Add even older clock entry to Task 2
          (insert ":LOGBOOK:\n")
          (insert "CLOCK: [2025-12-19 Thu 10:00]--[2025-12-19 Thu 11:00] =>  1:00\n")
          (insert ":END:\n")

          (org-mode-restart)
          (basic-save-buffer)
          (org-id-update-id-locations (list temp-file))

          ;; Test the predicate on the project heading
          (goto-char (point-min))
          (search-forward "Test Project")
          (org-back-to-heading t)

          (let ((predicate (org-gtd-pred--last-clocked-out-matches '(> "1d"))))
            ;; Should match because most recent clock-out (Task 1) is > 1d ago
            (assert-true (funcall predicate))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'last-clocked-out-test)

;;; last-clocked-out-test.el ends here
