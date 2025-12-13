;;; someday-review-test.el --- Tests for someday review -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;;; Commentary:
;;
;; Tests for someday/maybe review functionality.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-someday-review)

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
  (org-gtd-someday-create "Item one")
  (org-gtd-someday-create "Item two")
  (let ((items (org-gtd-someday-review--find-items nil)))
    (assert-equal 2 (length items))))

(deftest someday-review/filters-by-list-property ()
  "Filters items by ORG_GTD_SOMEDAY_LIST property."
  (let ((org-gtd-someday-lists '("Work" "Personal")))
    ;; Create items with different lists
    (with-simulated-input "Work RET"
      (org-gtd-someday-create "Work idea"))
    (with-simulated-input "Personal RET"
      (org-gtd-someday-create "Personal idea")))
  (let ((work-items (org-gtd-someday-review--find-items "Work")))
    (assert-equal 1 (length work-items))))

(deftest someday-review/finds-unassigned-items ()
  "Finds items without ORG_GTD_SOMEDAY_LIST when filtering for unassigned."
  (let ((org-gtd-someday-lists nil))
    (org-gtd-someday-create "Unassigned item"))
  (let ((org-gtd-someday-lists '("Work")))
    (with-simulated-input "Work RET"
      (org-gtd-someday-create "Work item")))
  (let ((unassigned (org-gtd-someday-review--find-items 'unassigned)))
    (assert-equal 1 (length unassigned))))

;;; Review Session State Tests

(deftest someday-review/initializes-session-state ()
  "Initializes review session state with item queue."
  (org-gtd-someday-create "Item one")
  (org-gtd-someday-create "Item two")
  (org-gtd-someday-review--start-session nil)
  (assert-true org-gtd-someday-review--session-active)
  (assert-equal 2 (length (plist-get org-gtd-someday-review--state :queue)))
  (assert-equal 0 (plist-get org-gtd-someday-review--state :position))
  ;; Cleanup
  (org-gtd-someday-review--end-session))

(deftest someday-review/tracks-statistics ()
  "Tracks review statistics (reviewed count, clarified count)."
  (org-gtd-someday-create "Item one")
  (org-gtd-someday-review--start-session nil)
  (assert-equal 0 (plist-get org-gtd-someday-review--state :reviewed))
  (assert-equal 0 (plist-get org-gtd-someday-review--state :clarified))
  ;; Cleanup
  (org-gtd-someday-review--end-session))

;;; Review Buffer Tests

(deftest someday-review/creates-wip-buffer-with-review-mode ()
  "Creates WIP buffer with review mode active."
  (org-gtd-someday-create "Review me")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  ;; Should use WIP buffer infrastructure
  (let ((bufs (org-gtd-wip--get-buffers)))
    (assert-true (> (length bufs) 0))
    (with-current-buffer (car bufs)
      (assert-true (eq major-mode 'org-gtd-someday-review-mode))
      (assert-true buffer-read-only)
      (assert-match "Review me" (buffer-string))))
  ;; Cleanup
  (org-gtd-someday-review--cleanup-current-buffer)
  (org-gtd-someday-review--end-session))

(deftest someday-review/shows-keybindings-in-header-line ()
  "Shows available keybindings in header-line."
  (org-gtd-someday-create "Review me")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  (let ((bufs (org-gtd-wip--get-buffers)))
    (with-current-buffer (car bufs)
      (assert-match "\\[d\\]" header-line-format)
      (assert-match "\\[c\\]" header-line-format)
      (assert-match "\\[q\\]" header-line-format)))
  ;; Cleanup
  (org-gtd-someday-review--cleanup-current-buffer)
  (org-gtd-someday-review--end-session))

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
  (org-gtd-someday-create "Defer me")
  (org-gtd-someday-review--start-session nil)
  (let ((item-id (car (plist-get org-gtd-someday-review--state :queue))))
    (org-gtd-someday-review--display-current-item)
    (org-gtd-someday-review-defer)
    ;; Check the source item has LOGBOOK entry - get fresh marker after defer
    (let ((marker (org-id-find item-id 'marker)))
      (when marker
        (with-current-buffer (marker-buffer marker)
          (goto-char marker)
          (org-back-to-heading t)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (assert-match ":LOGBOOK:" (buffer-substring (point) subtree-end)))))))
  ;; Cleanup - session already ended by defer on last item
  )

(deftest someday-review/defer-advances-to-next-item ()
  "Defer command advances to the next item."
  (org-gtd-someday-create "First item")
  (org-gtd-someday-create "Second item")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  (org-gtd-someday-review-defer)
  (assert-equal 1 (plist-get org-gtd-someday-review--state :position))
  (assert-equal 1 (plist-get org-gtd-someday-review--state :reviewed))
  ;; Cleanup
  (org-gtd-someday-review--cleanup-current-buffer)
  (org-gtd-someday-review--end-session))

(deftest someday-review/defer-ends-session-when-done ()
  "Defer ends session when last item is deferred."
  (org-gtd-someday-create "Only item")
  (org-gtd-someday-review--start-session nil)
  (org-gtd-someday-review--display-current-item)
  (org-gtd-someday-review-defer)
  (assert-nil org-gtd-someday-review--session-active))

(provide 'someday-review-test)

;;; someday-review-test.el ends here
