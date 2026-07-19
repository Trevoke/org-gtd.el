;;; missed-calendar-review-test.el --- Tests for overdue calendar review -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;;; Commentary:
;;
;; Tests for the actionable overdue-calendar review walk (REC-UI-04).

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-reflect-missed-calendar-review)
(require 'org-gtd-walk)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(defun mcr-test--make-calendar (title timestamp)
  "Insert a Calendar item TITLE with ORG_GTD_TIMESTAMP TIMESTAMP into the GTD file.
Returns the id."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (let ((id (org-id-uuid)))
      (insert (format "* %s\n:PROPERTIES:\n:ID: %s\n:ORG_GTD: Calendar\n:ORG_GTD_TIMESTAMP: %s\n:END:\n"
                      title id timestamp))
      (save-buffer)
      id)))

;;; Detection

(deftest mcr/find-includes-overdue-calendar ()
  "An open Calendar item dated before today is detected."
  (mcr-test--make-calendar "Dentist" "<2020-01-01>")
  (assert-equal 1 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-excludes-today-and-future ()
  "A Calendar item dated today or in the future is not overdue."
  (mcr-test--make-calendar "Today thing" (format-time-string "<%Y-%m-%d>"))
  (mcr-test--make-calendar "Future thing" "<2099-01-01>")
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-excludes-done ()
  "A done Calendar item is not detected."
  (let ((id (mcr-test--make-calendar "Happened" "<2020-01-01>")))
    (let ((m (org-id-find id 'marker)))
      (org-with-point-at m
        (let ((org-inhibit-logging t)) (org-todo (org-gtd-keywords--done)))
        (save-buffer))))
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-excludes-non-calendar ()
  "A non-Calendar heading with a past timestamp is not detected."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* A next action\n:PROPERTIES:\n:ID: mcr-na-1\n:ORG_GTD: Actions\n:ORG_GTD_TIMESTAMP: <2020-01-01>\n:END:\n")
    (save-buffer))
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-excludes-habit ()
  "A Habit heading with a past timestamp is not detected."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* A routine\n:PROPERTIES:\n:ID: mcr-hab-1\n:ORG_GTD: Habit\n:ORG_GTD_TIMESTAMP: <2020-01-01>\n:STYLE: habit\n:END:\n")
    (save-buffer))
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/find-includes-repeating-calendar ()
  "A repeating Calendar item with a past base date is included (view parity)."
  (mcr-test--make-calendar "Weekly sync" "<2020-01-01 +1w>")
  (assert-equal 1 (length (org-gtd-reflect-missed-calendar-review--find-items))))

(deftest mcr/resolve-rejects-missing-id ()
  "The :resolve predicate is nil for an unknown id, non-nil for a real one."
  (let ((id (mcr-test--make-calendar "Real" "<2020-01-01>")))
    (assert-true (org-gtd-reflect-missed-calendar-review--resolve id))
    (assert-nil (org-gtd-reflect-missed-calendar-review--resolve "no-such-id-xyz"))))

(provide 'missed-calendar-review-test)

;;; missed-calendar-review-test.el ends here
