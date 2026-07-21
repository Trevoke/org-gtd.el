;;; missed-calendar-review-test.el --- Tests for overdue calendar review -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;;; Commentary:
;;
;; Tests for the actionable overdue-calendar review walk (REC-UI-04).

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'cl-lib)
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
    ;; It was archived with the DONE keyword, not just removed from view.
    (assert-match (format "\\* %s It happened" (org-gtd-keywords--done))
                  (ogt--archive-string))
    (ignore id)))

;;; Disposition: migrate

(deftest mcr/migrate-retypes-to-next-action ()
  "`m' migrates the item to a next action: ORG_GTD=Actions, NEXT state,
ORG_GTD_TIMESTAMP dropped."
  (let ((id (mcr-test--make-calendar "Still need to do this" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-migrate))
    (let ((marker (org-id-find id 'marker)))
      (assert-true marker)
      (org-with-point-at marker
        (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
        (assert-equal (org-gtd-keywords--next) (org-get-todo-state))
        (assert-nil (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))))

(deftest mcr/migrate-suppresses-organize-hooks ()
  "Migrate binds `org-gtd-organize-hooks' off, so decoration hooks never fire."
  (mcr-test--make-calendar "No re-prompt" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let* ((fired nil)
         (org-gtd-organize-hooks (list (lambda () (setq fired t)))))
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-migrate))
    (assert-nil fired)))

(deftest mcr/trash-suppresses-organize-hooks ()
  "Trash binds `org-gtd-organize-hooks' off, so decoration hooks never fire."
  (mcr-test--make-calendar "No re-prompt on trash" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let* ((fired nil)
         (org-gtd-organize-hooks (list (lambda () (setq fired t)))))
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-trash))
    (assert-nil fired)))

;;; Disposition: reschedule

(deftest mcr/read-future-date-rejects-past ()
  "The date reader re-prompts until the chosen date is today-or-later."
  (let ((answers (list "2000-01-01" "2999-01-01")))
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&rest _) (pop answers)))
              ((symbol-function 'sit-for) (lambda (&rest _) t)))
      (assert-equal "2999-01-01"
                    (org-gtd-reflect-missed-calendar-review--read-future-date))
      ;; both answers consumed => it looped past the in-the-past first answer.
      (assert-nil answers))))

(deftest mcr/reschedule-sets-new-future-timestamp ()
  "`r' keeps the item a Calendar item and writes the new (bracketed) timestamp."
  (let ((id (mcr-test--make-calendar "Needs new date" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&rest _) "2999-01-01")))
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        (org-gtd-reflect-missed-calendar-review-reschedule)))
    (let ((marker (org-id-find id 'marker)))
      (assert-true marker)
      (org-with-point-at marker
        (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))
        (assert-equal "<2999-01-01>"
                      (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))))

(deftest mcr/reschedule-suppresses-organize-hooks ()
  "Reschedule binds `org-gtd-organize-hooks' off, so decoration hooks never fire."
  (mcr-test--make-calendar "No re-prompt on reschedule" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let* ((fired nil)
         (org-gtd-organize-hooks (list (lambda () (setq fired t)))))
    (cl-letf (((symbol-function 'org-read-date)
               (lambda (&rest _) "2999-01-01")))
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        (org-gtd-reflect-missed-calendar-review-reschedule)))
    (assert-nil fired)))

;;; Disposition: trash

(deftest mcr/trash-cancels-and-archives ()
  "`t' cancels + archives the item (irrelevant now) and ends the walk."
  (mcr-test--make-calendar "No longer relevant" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-reflect-missed-calendar-review-trash))
  (assert-equal 0 (length (org-gtd-wip--get-buffers)))
  (assert-equal 0 (length (org-gtd-reflect-missed-calendar-review--find-items)))
  ;; It was archived with the CANCELED keyword, not the DONE keyword --
  ;; distinguishes trash from done (a copy-paste keyword swap would fail this).
  (assert-match (format "\\* %s No longer relevant" (org-gtd-keywords--canceled))
                (ogt--archive-string)))

;;; Disposition: skip

(deftest mcr/skip-advances-without-changing-item ()
  "`s' advances without mutating the item; the item is still overdue."
  (let ((id (mcr-test--make-calendar "Decide later" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-skip))
    ;; Only item -> walk finished -> surface cleaned up, but item unchanged.
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))
    (let ((marker (org-id-find id 'marker)))
      (org-with-point-at marker
        (assert-equal "Calendar" (org-entry-get (point) "ORG_GTD"))
        (assert-equal "<2020-01-01>"
                      (org-entry-get (point) "ORG_GTD_TIMESTAMP"))))
    ;; Still detected on a fresh run (skip is "not now", not "never").
    (assert-equal 1 (length (org-gtd-reflect-missed-calendar-review--find-items)))))

(deftest mcr/skip-counts-a-skip ()
  "`s' increments the skipped counter (checked mid-walk, two items)."
  (mcr-test--make-calendar "First" "<2020-01-01>")
  (mcr-test--make-calendar "Second" "<2020-01-02>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((surface (car (org-gtd-wip--get-buffers))))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-skip)
      (assert-same 1 (plist-get org-gtd-reflect-missed-calendar-review--counters :skipped))
      (org-gtd-reflect-missed-calendar-review-quit))))

;;; Disposition: clarify

(deftest mcr/clarify-transforms-surface-in-place-without-advancing ()
  "`c' turns the review surface itself into an editable clarify buffer for
the current item -- it does not advance the walk (advancing happens later,
when the user finishes organizing)."
  (mcr-test--make-calendar "Rethink me" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((surface (car (org-gtd-wip--get-buffers))))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-clarify)
      (assert-true (derived-mode-p 'org-gtd-clarify-mode))
      (assert-nil buffer-read-only)
      (assert-match "Rethink me" (buffer-string))
      ;; Walk is STILL parked on the item: the walk session is still active
      ;; and the cursor has not moved, proving `c' did not advance.
      (assert-true org-gtd-walk--active)
      (assert-equal 0 (plist-get (plist-get org-gtd-walk--active :model) :cursor))
      ;; Cancel back to the console, then quit cleanly (no leaked temp file).
      (org-gtd-reflect-missed-calendar-review--cancel-clarify)
      (org-gtd-reflect-missed-calendar-review-quit))))

(deftest mcr/clarify-then-organize-advances-and-retypes-item ()
  "Finishing organize on an in-place-clarified item advances the review to
the next item, through the REAL organize -> walk-advance seam (not a
direct call to `org-gtd-walk-advance').

Drives `org-gtd-next-action', which is `org-gtd--dispatch' for the
`next-action' type: because the surface's `org-gtd-clarify--clarify-id'
is set (by `--clarify-in-place'), dispatch routes through
`org-gtd-organize--call', whose `walk-active' branch is the exact code
path `org-gtd-organize--call' uses for the inbox walk's auto-advance.
`org-gtd-organize-hooks' is bound to nil to suppress the default
tag-prompt hook, which would otherwise block on interactive input."
  (mcr-test--make-calendar "First" "<2020-01-01>")
  (mcr-test--make-calendar "Second" "<2020-01-02>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((surface (car (org-gtd-wip--get-buffers)))
        (org-gtd-organize-hooks nil))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-clarify)
      (assert-true (derived-mode-p 'org-gtd-clarify-mode))
      (org-gtd-next-action))
    ;; The walk advanced: the surface is back to the console, on item 2.
    (with-current-buffer surface
      (assert-true (eq major-mode 'org-gtd-reflect-missed-calendar-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Second" (buffer-string))
      (refute-match "First" (buffer-string)))
    ;; Item 1 is now a next action, no longer an overdue calendar item.
    (assert-equal 1 (length (org-gtd-reflect-missed-calendar-review--find-items)))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-quit))))

(deftest mcr/cancel-clarify-returns-to-console-for-same-item ()
  "Canceling (`C-c C-k') an in-place clarify redraws the console for the
SAME item, without advancing or quitting: the walk stays active, the
cursor is unchanged."
  (mcr-test--make-calendar "Item" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((surface (car (org-gtd-wip--get-buffers))))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-clarify)
      ;; The buffer-local override shadows the default `org-gtd-clarify-stop'
      ;; (which would abandon the whole review) with our per-item cancel.
      (assert-equal 'org-gtd-reflect-missed-calendar-review--cancel-clarify
                    (lookup-key (current-local-map) (kbd "C-c C-k")))
      (call-interactively (lookup-key (current-local-map) (kbd "C-c C-k"))))
    (with-current-buffer surface
      (assert-true (eq major-mode 'org-gtd-reflect-missed-calendar-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Item" (buffer-string))
      (assert-true org-gtd-walk--active)
      (assert-equal 0 (plist-get (plist-get org-gtd-walk--active :model) :cursor))
      (org-gtd-reflect-missed-calendar-review-quit))))

(deftest mcr/clarify-then-organize-finish-leaves-no-wip-surface ()
  "After `c' + organize on the only overdue item, the walk finishes and no
WIP surface buffer is left behind.  This is the tricky case: the walk
finishes directly from the advance (no next item to render), so
`--render' never runs to rekey the surface back to the fixed surface
key -- `--on-finish' itself must clean up under whichever key the
surface currently carries."
  (mcr-test--make-calendar "Only item" "<2020-01-01>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((surface (car (org-gtd-wip--get-buffers)))
        (org-gtd-organize-hooks nil))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-clarify)
      (org-gtd-next-action))
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))))

;;; Landmine guard: mutating dispositions after out-of-band changes

(deftest mcr/mutating-disposition-skips-item-clarified-away ()
  "A mutating key refuses to act on the current item once it is no longer an
overdue Calendar item (e.g. retyped away), advancing instead of corrupting it."
  (let ((id (mcr-test--make-calendar "Was calendar" "<2020-01-01>")))
    (org-gtd-reflect-missed-calendar-review)
    ;; Simulate the item being handled out of band: retype it away from Calendar.
    (let ((m (org-id-find id 'marker)))
      (org-with-point-at m (org-entry-put (point) "ORG_GTD" "Actions")))
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-done))
    ;; `done' did NOT touch it: still in the default GTD file (not archived
    ;; away) with no TODO state added (org-gtd-archive-item-at-point leaves
    ;; ORG_GTD alone, so that property alone can't prove the guard fired --
    ;; location + TODO state are the properties that actually distinguish
    ;; "mutated" from "left alone").  The single-item walk still advanced.
    (let ((m (org-id-find id 'marker)))
      (assert-true m)
      (assert-equal (org-gtd--default-file) (marker-buffer m))
      (org-with-point-at m
        (assert-equal "Actions" (org-entry-get (point) "ORG_GTD"))
        (assert-nil (org-get-todo-state))))
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))))

;;; Counters + finish

(deftest mcr/counters-tally-across-dispositions ()
  "Mixed dispositions across several items tally correctly on the surface."
  (mcr-test--make-calendar "One"   "<2020-01-01>")
  (mcr-test--make-calendar "Two"   "<2020-01-02>")
  (mcr-test--make-calendar "Three" "<2020-01-03>")
  (org-gtd-reflect-missed-calendar-review)
  (let ((surface (car (org-gtd-wip--get-buffers))))
    ;; item 1 -> skip, item 2 -> migrate, item 3 -> done (finishes the walk)
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-skip))
    (with-current-buffer surface
      (org-gtd-reflect-missed-calendar-review-migrate))
    ;; Read counters BEFORE the last disposition finishes+cleans up the surface.
    (with-current-buffer surface
      (assert-same 2 (plist-get org-gtd-reflect-missed-calendar-review--counters :reviewed))
      (assert-same 1 (plist-get org-gtd-reflect-missed-calendar-review--counters :skipped))
      (assert-same 1 (plist-get org-gtd-reflect-missed-calendar-review--counters :migrated))
      (org-gtd-reflect-missed-calendar-review-done))
    ;; Last disposition ran :on-finish, which cleaned up the surface buffer.
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))))

;;; Persistence: on-finish / quit save buffers when opted in

(deftest mcr/on-finish-saves-when-opted-in ()
  "`--on-finish' saves org-gtd buffers via `org-gtd-save-buffers' when the
user opted in via `org-gtd-save-after-organize'.  Migrating the only item
finishes the walk (runs :on-finish) and modifies the default GTD file
(refile-in-place); afterward that buffer must not be left modified."
  (let ((org-gtd-save-after-organize t))
    (mcr-test--make-calendar "Save me" "<2020-01-01>")
    (org-gtd-reflect-missed-calendar-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-migrate))
    (with-current-buffer (org-gtd--default-file)
      (assert-nil (buffer-modified-p)))))

(deftest mcr/quit-saves-when-opted-in ()
  "`-quit' also saves org-gtd buffers via `org-gtd-save-buffers' when opted in.
Migrates one item (mutating the default GTD file) while the walk is still
active (a second item remains), then quits; the buffer must be saved."
  (let ((org-gtd-save-after-organize t))
    (mcr-test--make-calendar "First" "<2020-01-01>")
    (mcr-test--make-calendar "Second" "<2020-01-02>")
    (org-gtd-reflect-missed-calendar-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-reflect-missed-calendar-review-migrate)
      (org-gtd-reflect-missed-calendar-review-quit))
    (with-current-buffer (org-gtd--default-file)
      (assert-nil (buffer-modified-p)))))

(provide 'missed-calendar-review-test)

;;; missed-calendar-review-test.el ends here
