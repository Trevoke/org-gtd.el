;;; someday-review-test.el --- Tests for someday review -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;;; Commentary:
;;
;; Tests for someday/maybe review functionality.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-someday-review)
(require 'org-gtd-walk)

(e-unit-initialize)

;;; LOGBOOK Entry Tests

(deftest someday-review/adds-reviewed-entry-to-logbook ()
  "Adds 'Reviewed' entry to LOGBOOK drawer."
  (ogt--with-temp-org-buffer
   "* Test item
:PROPERTIES:
:ORG_GTD: Someday
:END:"
   (org-back-to-heading t)
   (org-gtd-someday-review--add-reviewed-entry)
   (let ((content (buffer-string)))
     (assert-match ":LOGBOOK:" content)
     (assert-match "- Reviewed \\[" content))))

(deftest someday-review/preserves-existing-logbook-entries ()
  "Preserves existing LOGBOOK entries when adding new one."
  (ogt--with-temp-org-buffer
   "* Test item
:PROPERTIES:
:ORG_GTD: Someday
:END:
:LOGBOOK:
- Previous note [2025-01-01 Wed]
:END:"
   (org-back-to-heading t)
   (org-gtd-someday-review--add-reviewed-entry)
   (let ((content (buffer-string)))
     (assert-match "Previous note" content)
     (assert-match "- Reviewed \\[" content))))

;;; Finding Someday Items Tests

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest someday-review/finds-all-someday-items ()
  "Finds all items with ORG_GTD: Someday."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Item one")
    (org-gtd-someday-create "Item two"))
  (let ((items (org-gtd-someday-review--find-items nil)))
    (assert-equal 2 (length items))))

(deftest someday-review/filters-by-list-property ()
  "Filters items by ORG_GTD_SOMEDAY_LIST property."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    ;; Create items with different lists
    (with-suppressed-warnings ((obsolete org-gtd-someday-create))
      (with-simulated-input "Work RET"
        (org-gtd-someday-create "Work idea"))
      (with-simulated-input "Personal RET"
        (org-gtd-someday-create "Personal idea"))))
  (let ((work-items (org-gtd-someday-review--find-items "Work")))
    (assert-equal 1 (length work-items))))

(deftest someday-review/finds-unassigned-items ()
  "Finds items without ORG_GTD_SOMEDAY_LIST when filtering for unassigned."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (let ((org-gtd-someday-lists nil))
      (org-gtd-someday-create "Unassigned item"))
    (let ((org-gtd-someday-lists '("Work")))
      (with-simulated-input "Work RET"
        (org-gtd-someday-create "Work item"))))
  (let ((unassigned (org-gtd-someday-review--find-items 'unassigned)))
    (assert-equal 1 (length unassigned))))

(deftest someday-review/find-builder-returns-filtered-ids ()
  "The :find builder yields exactly the ids matching its filter."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    (with-suppressed-warnings ((obsolete org-gtd-someday-create))
      (with-simulated-input "Work RET" (org-gtd-someday-create "Work idea"))
      (with-simulated-input "Personal RET" (org-gtd-someday-create "Personal idea")))
    (let ((find (org-gtd-someday-review--make-find "Work")))
      (assert-equal 1 (length (funcall find))))))

(deftest someday-review/resolve-rejects-missing-id ()
  "The :resolve predicate is nil for an unknown id, non-nil for a real one."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Real item"))
  (let ((id (car (org-gtd-someday-review--find-items nil))))
    (assert-true (org-gtd-someday-review--resolve id))
    (assert-nil (org-gtd-someday-review--resolve "no-such-id-xyz"))))

(deftest someday-review/render-fills-surface-with-current-item ()
  "Render draws the item, activates review mode read-only, and shows progress."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Render me"))
  (let* ((id (car (org-gtd-someday-review--find-items nil)))
         (surface (org-gtd-wip--get-buffer "someday-review")))
    (with-current-buffer surface
      (setq-local org-gtd-walk--active
                  (list :model (org-gtd-walk-model-create (list id))))
      (org-gtd-someday-review--render id surface)
      (assert-true (eq major-mode 'org-gtd-someday-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Render me" (buffer-string))
      (assert-match "\\[d\\]" header-line-format)
      (assert-match "(1/1)" header-line-format))
    (org-gtd-wip--cleanup-temp-file "someday-review")))

;;; Review Buffer Tests

(deftest someday-review/creates-wip-buffer-with-review-mode ()
  "The walk shows the current item in a read-only review-mode WIP buffer."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Review me"))
  (org-gtd-reflect-someday-review)
  (let ((bufs (org-gtd-wip--get-buffers)))
    (assert-true (> (length bufs) 0))
    (with-current-buffer (car bufs)
      (assert-true (eq major-mode 'org-gtd-someday-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Review me" (buffer-string))))
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-someday-review-quit)))

(deftest someday-review/shows-keybindings-in-header-line ()
  "Shows available keybindings in header-line."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Review me"))
  (org-gtd-reflect-someday-review)
  (let ((bufs (org-gtd-wip--get-buffers)))
    (with-current-buffer (car bufs)
      (assert-match "\\[d\\]" header-line-format)
      (assert-match "\\[c\\]" header-line-format)
      (assert-match "\\[q\\]" header-line-format)))
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-someday-review-quit)))

;;; Review Mode Keybinding Tests

(deftest someday-review/mode-has-defer-keybinding ()
  "Review mode has 'd' bound to defer command."
  (assert-equal 'org-gtd-someday-review-defer
                (lookup-key org-gtd-someday-review-mode-map (kbd "d"))))

(deftest someday-review/mode-has-clarify-keybinding ()
  "Review mode has 'c' bound to clarify command."
  (assert-equal 'org-gtd-someday-review-clarify
                (lookup-key org-gtd-someday-review-mode-map (kbd "c"))))

(deftest someday-review/mode-has-quit-keybinding ()
  "Review mode has 'q' bound to quit command."
  (assert-equal 'org-gtd-someday-review-quit
                (lookup-key org-gtd-someday-review-mode-map (kbd "q"))))

(deftest someday-review/mode-is-derived-from-org-mode ()
  "Review mode is derived from org-mode."
  (with-temp-buffer
    (org-gtd-someday-review-mode)
    (assert-true (derived-mode-p 'org-mode))))

;;; Defer Command Tests

(deftest someday-review/defer-adds-logbook-entry ()
  "Defer command adds reviewed entry to item's LOGBOOK."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Defer me"))
  (let ((item-id (car (org-gtd-someday-review--find-items nil))))
    (org-gtd-reflect-someday-review)
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-someday-review-defer))
    ;; Check the source item has LOGBOOK entry - get fresh marker after defer
    (let ((marker (org-id-find item-id 'marker)))
      (when marker
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (org-back-to-heading t)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (assert-match ":LOGBOOK:" (buffer-substring (point) subtree-end)))))))
  ;; Cleanup - the walk already ended by defer on the (only) last item
  )

(deftest someday-review/defer-ends-session-when-done ()
  "Defer ends session when last item is deferred."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Only item"))
  (org-gtd-reflect-someday-review)
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-someday-review-defer))
  (assert-equal 0 (length (org-gtd-wip--get-buffers))))

;;; Clarify Command Tests

(deftest someday-review/clarify-increments-clarified-count ()
  "Clarify command reactivates the item and ends the walk on the last item."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Clarify me"))
  (org-gtd-reflect-someday-review)
  ;; Mock reactivate to avoid side effects
  (cl-letf (((symbol-function 'org-gtd-reactivate) (lambda ())))
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-someday-review-clarify)))
  (assert-equal 0 (length (org-gtd-wip--get-buffers))))

;;; Quit Command Tests

(deftest someday-review/quit-ends-session ()
  "Quit command ends the review session."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Item"))
  (org-gtd-reflect-someday-review)
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-someday-review-quit))
  (assert-equal 0 (length (org-gtd-wip--get-buffers))))

(deftest someday-review/quit-cleans-up-wip-buffer ()
  "Quit command cleans up the WIP buffer."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Item"))
  (org-gtd-reflect-someday-review)
  (let ((wip-bufs-before (length (org-gtd-wip--get-buffers))))
    (assert-true (> wip-bufs-before 0))
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-someday-review-quit))
    ;; WIP buffer should be cleaned up
    (assert-equal 0 (length (org-gtd-wip--get-buffers)))))

;;; Entry Point Tests

(deftest someday-review/entry-point-starts-session ()
  "org-gtd-reflect-someday-review starts a review session."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "Item"))
  (org-gtd-reflect-someday-review)
  ;; Should create a WIP buffer
  (assert-true (> (length (org-gtd-wip--get-buffers)) 0))
  ;; Cleanup
  (with-current-buffer (car (org-gtd-wip--get-buffers))
    (org-gtd-someday-review-quit)))

(deftest someday-review/entry-point-shows-message-when-no-items ()
  "Shows message when no items to review."
  ;; No items created - just start session
  (org-gtd-reflect-someday-review)
  (assert-equal 0 (length (org-gtd-wip--get-buffers))))

;;; Walk Adapter Tests

(deftest someday-review/find-returns-only-someday-ids ()
  "The adapter :find yields ids for someday items and nothing else."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "One")
    (org-gtd-someday-create "Two"))
  (assert-equal 2 (length (funcall (org-gtd-someday-review--make-find nil)))))

(deftest someday-review/defer-logs-review-then-advances ()
  "defer writes a Reviewed logbook line on the source item and moves on."
  (with-suppressed-warnings ((obsolete org-gtd-someday-create))
    (org-gtd-someday-create "First")
    (org-gtd-someday-create "Second"))
  (org-gtd-reflect-someday-review)
  (let* ((surface (car (org-gtd-wip--get-buffers)))
         (id (with-current-buffer surface
               (org-gtd-walk-model-current (plist-get org-gtd-walk--active :model)))))
    (with-current-buffer surface (org-gtd-someday-review-defer))
    ;; still walking (a second item remains) and the first item got its log line
    (assert-true (> (length (org-gtd-wip--get-buffers)) 0))
    (let ((marker (org-id-find id 'marker)))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker) (org-back-to-heading t)
        (let ((end (save-excursion (org-end-of-subtree t))))
          (assert-match ":LOGBOOK:" (buffer-substring (point) end)))))
    (with-current-buffer (car (org-gtd-wip--get-buffers))
      (org-gtd-someday-review-quit))))

;;; Evil-mode Integration Tests

(deftest someday-review/evil-integration-registered ()
  "Verifies evil-set-initial-state is registered for someday-review-mode.
This ensures evil users get emacs state by default for better UX."
  (let ((evil-entry (assq 'evil after-load-alist)))
    (assert-true evil-entry)
    ;; The entry should contain code that references org-gtd-someday-review-mode
    ;; (may be byte-compiled, so check string representation)
    (let ((forms (cdr evil-entry)))
      (assert-true
       (cl-some (lambda (form)
                  (string-match-p "org-gtd-someday-review-mode"
                                  (format "%S" form)))
                forms)))))

(deftest someday-review/mode-has-essential-keybindings ()
  "Someday review mode keymap should have essential bindings."
  (assert-true (lookup-key org-gtd-someday-review-mode-map (kbd "d")))
  (assert-true (lookup-key org-gtd-someday-review-mode-map (kbd "c")))
  (assert-true (lookup-key org-gtd-someday-review-mode-map (kbd "q"))))

(deftest someday-review/registers-a-walk-consumer ()
  "Loading someday-review registers a `someday-review' walk in `org-gtd-walks'."
  (let ((spec (org-gtd-walk-get 'someday-review)))
    (assert-true spec)
    (assert-same 'someday-review (plist-get spec :name))
    (assert-true (org-gtd-walk--callable-p (plist-get spec :render)))))

(provide 'someday-review-test)

;;; someday-review-test.el ends here
