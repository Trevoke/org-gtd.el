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

;;; Mode + keymap

(deftest mcr/mode-is-derived-from-org-mode ()
  "Review mode is derived from org-mode."
  (with-temp-buffer
    (org-gtd-reflect-missed-calendar-review-mode)
    (assert-true (derived-mode-p 'org-mode))))

(deftest mcr/mode-has-disposition-keybindings ()
  "The mode keymap binds every disposition key to its command."
  (let ((map org-gtd-reflect-missed-calendar-review-mode-map))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-done
                  (lookup-key map (kbd "d")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-migrate
                  (lookup-key map (kbd "m")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-reschedule
                  (lookup-key map (kbd "r")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-trash
                  (lookup-key map (kbd "t")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-clarify
                  (lookup-key map (kbd "c")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-skip
                  (lookup-key map (kbd "s")))
    (assert-equal 'org-gtd-reflect-missed-calendar-review-quit
                  (lookup-key map (kbd "q")))))

;;; Render

(deftest mcr/render-fills-surface-read-only ()
  "Render draws the item, activates review mode read-only, humanizes the
lapse, and advertises the disposition keys."
  (let* ((id (mcr-test--make-calendar "Overdue thing" "<2020-01-01>"))
         (surface (org-gtd-wip--get-buffer
                   org-gtd-reflect-missed-calendar-review--surface-key)))
    (with-current-buffer surface
      (setq-local org-gtd-walk--active
                  (list :model (org-gtd-walk-model-create (list id))))
      (org-gtd-reflect-missed-calendar-review--render id surface)
      (assert-true (eq major-mode 'org-gtd-reflect-missed-calendar-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Overdue thing" (buffer-string))
      (assert-match "days ago" (buffer-string))
      (assert-match "\\[d\\] Done" header-line-format)
      (assert-match "\\[r\\] Reschedule" header-line-format)
      (assert-match "(1/1)" header-line-format))
    (org-gtd-wip--cleanup-temp-file
     org-gtd-reflect-missed-calendar-review--surface-key)))

;;; Spec registration + entry + empty state

(deftest mcr/registers-a-walk-consumer ()
  "Loading the module registers a `missed-calendar-review' walk."
  (let ((spec (org-gtd-walk-get 'missed-calendar-review)))
    (assert-true spec)
    (assert-same 'missed-calendar-review (plist-get spec :name))
    (assert-true (org-gtd-walk--callable-p (plist-get spec :render)))
    (assert-true (org-gtd-walk--callable-p (plist-get spec :find)))))

(deftest mcr/entry-opens-console-when-items-exist ()
  "The entry command opens a read-only review surface for the item."
  (mcr-test--make-calendar "Review me" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((bufs (org-gtd-wip--get-buffers)))
    (assert-true (> (length bufs) 0))
    (with-current-buffer (car bufs)
      (assert-true (eq major-mode 'org-gtd-reflect-missed-calendar-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Review me" (buffer-string))
      (org-gtd-reflect-missed-calendar-review-quit))))

(deftest mcr/entry-empty-state-opens-no-console ()
  "With no overdue calendar items, the console never opens."
  (org-gtd-reflect-missed-calendar-review)
  (assert-equal 0 (length (org-gtd-wip--get-buffers))))

(deftest mcr/quit-cleans-up-surface ()
  "Quit tears down the walk and cleans up the surface buffer."
  (mcr-test--make-calendar "Item" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (assert-true (> (length (org-gtd-wip--get-buffers)) 0))
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-reflect-missed-calendar-review-quit))
  (assert-equal 0 (length (org-gtd-wip--get-buffers))))

;;; Disposition: done

(deftest mcr/done-archives-and-advances ()
  "`d' marks the item done, archives it, and ends the walk on the last item."
  (let ((id (mcr-test--make-calendar "It happened" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-done))
    ;; Only item -> walk finished -> surface cleaned up.
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))
    ;; The item is no longer detected as overdue (it was archived away).
    (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items)))
    (ignore id)))

(provide 'missed-calendar-review-test)

;;; missed-calendar-review-test.el ends here
