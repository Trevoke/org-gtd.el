;;; inbox-walk-test.el --- Tests for the inbox walk adapter -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tier-3 tests for the inbox walk adapter (org-gtd-inbox-walk.el).
;; This is additive: none of these tests exercise `org-gtd-process-inbox'
;; or `org-gtd-walk-start' -- the adapter's `:find'/`:render' are driven
;; directly.  See docs/plans/2026-07-17-walk-engine-phase-4-plan.md
;; Tasks 1-3.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-inbox-walk)
(require 'org-gtd-walk)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;; Scan Tests (Task 1)

(deftest inbox-walk/scan-returns-tokens-for-all-inbox-headings ()
  "Scan returns one token per inbox heading, main inbox then additional."
  (capture-inbox-item "Main item one")
  (capture-inbox-item "Main item two")
  (let* ((additional-inbox-file (f-join org-gtd-directory "additional-inbox.org")))
    (with-current-buffer (find-file-noselect additional-inbox-file)
      (insert "* Additional item\n")
      (basic-save-buffer))
    (let* ((org-gtd-additional-inbox-files (list additional-inbox-file))
           (scanned (org-gtd-inbox-walk--scan))
           (tokens (car scanned))
           (meta (cdr scanned)))
      (assert-equal 3 (length tokens))
      (dolist (token tokens)
        (assert-true (stringp token))
        (assert-true (org-gtd-walk-model--handle-serializable-p token)))
      ;; Main inbox items come first, in document order, then additional.
      (let ((marker-1 (cdr (assoc (nth 0 tokens) meta)))
            (marker-2 (cdr (assoc (nth 1 tokens) meta)))
            (marker-3 (cdr (assoc (nth 2 tokens) meta))))
        (assert-true (markerp marker-1))
        (assert-true (markerp marker-2))
        (assert-true (markerp marker-3))
        (assert-match "Main item one" (org-with-point-at marker-1 (org-get-heading t t t t)))
        (assert-match "Main item two" (org-with-point-at marker-2 (org-get-heading t t t t)))
        (assert-match "Additional item" (org-with-point-at marker-3 (org-get-heading t t t t))))
      ;; The constructed model stays valid; meta-with-markers doesn't
      ;; break validity (validity only inspects entries + cursor).
      (let ((model (org-gtd-walk-model-create tokens meta)))
        (assert-true (org-gtd-walk-model-valid-p model))))))

(deftest inbox-walk/scan-skips-missing-additional-inbox-file ()
  "Scan skips an additional inbox file that does not exist."
  (capture-inbox-item "Only main item")
  (let* ((missing-file (f-join org-gtd-directory "does-not-exist.org"))
         (org-gtd-additional-inbox-files (list missing-file))
         (tokens (car (org-gtd-inbox-walk--scan))))
    (assert-equal 1 (length tokens))))

(deftest inbox-walk/scan-skips-empty-additional-inbox-file ()
  "Scan skips an additional inbox file with no headings."
  (capture-inbox-item "Only main item")
  (let* ((empty-file (f-join org-gtd-directory "empty-inbox.org")))
    (with-current-buffer (find-file-noselect empty-file)
      (basic-save-buffer))
    (let* ((org-gtd-additional-inbox-files (list empty-file))
           (tokens (car (org-gtd-inbox-walk--scan))))
      (assert-equal 1 (length tokens)))))

(deftest inbox-walk/scan-orders-multiple-additional-inboxes ()
  "Scan visits additional inbox files in the order listed."
  (let* ((inbox1-file (f-join org-gtd-directory "inbox1.org"))
         (inbox2-file (f-join org-gtd-directory "inbox2.org")))
    (with-current-buffer (find-file-noselect inbox1-file)
      (insert "* Item from inbox 1\n")
      (basic-save-buffer))
    (with-current-buffer (find-file-noselect inbox2-file)
      (insert "* Item from inbox 2\n")
      (basic-save-buffer))
    (let* ((org-gtd-additional-inbox-files (list inbox1-file inbox2-file))
           (scanned (org-gtd-inbox-walk--scan))
           (tokens (car scanned))
           (meta (cdr scanned)))
      (assert-equal 2 (length tokens))
      (assert-match "Item from inbox 1"
                    (org-with-point-at (cdr (assoc (nth 0 tokens) meta))
                      (org-get-heading t t t t)))
      (assert-match "Item from inbox 2"
                    (org-with-point-at (cdr (assoc (nth 1 tokens) meta))
                      (org-get-heading t t t t))))))

(deftest inbox-walk/marker-survives-cut-of-earlier-heading ()
  "The second item's marker still resolves after the first is cut.
This is the property (D2) that makes markers correct where captured
positions would be broken by in-session edits."
  (capture-inbox-item "First item")
  (capture-inbox-item "Second item")
  (let* ((scanned (org-gtd-inbox-walk--scan))
         (tokens (car scanned))
         (meta (cdr scanned))
         (marker-1 (cdr (assoc (nth 0 tokens) meta)))
         (marker-2 (cdr (assoc (nth 1 tokens) meta))))
    ;; Cut the first item's subtree out of its buffer.
    (with-current-buffer (marker-buffer marker-1)
      (org-with-point-at marker-1
        (org-cut-subtree)))
    ;; The second token's marker still resolves to the correct heading.
    (assert-true (marker-buffer marker-2))
    (assert-match "Second item" (org-with-point-at marker-2 (org-get-heading t t t t)))))

(deftest inbox-walk/build-model-seeds-meta-from-scan ()
  "The model builder combines scan tokens and meta into one valid model."
  (capture-inbox-item "Solo item")
  (let ((model (org-gtd-inbox-walk--build-model)))
    (assert-true (org-gtd-walk-model-valid-p model))
    (assert-equal 1 (length (plist-get model :entries)))
    (let ((marker (cdr (assoc (car (plist-get model :entries)) (plist-get model :meta)))))
      (assert-true (markerp marker))
      (assert-match "Solo item" (org-with-point-at marker (org-get-heading t t t t))))))

;;; Meta Accessor Tests (Task 2)

(deftest inbox-walk/token-returns-fresh-unique-strings ()
  "Token minting returns distinct strings each call."
  (let ((token-1 (org-gtd-inbox-walk--token))
        (token-2 (org-gtd-inbox-walk--token)))
    (assert-true (stringp token-1))
    (assert-true (stringp token-2))
    (assert-nil (equal token-1 token-2))))

(deftest inbox-walk/meta-put-marker-then-get-returns-marker ()
  "meta-put-marker stores a marker under a token; meta-get retrieves it."
  (ogt--with-temp-org-buffer
   "* Some heading"
   (let* ((marker (point-marker))
          (token (org-gtd-inbox-walk--token))
          (model (org-gtd-walk-model-create nil)))
     (setq model (org-gtd-inbox-walk--meta-put-marker model token marker))
     (assert-same marker (org-gtd-inbox-walk--meta-get model token)))))

(deftest inbox-walk/meta-put-dup-then-get-returns-plist ()
  "meta-put-dup stores a (:title :content) plist under a token."
  (let* ((token (org-gtd-inbox-walk--token))
         (model (org-gtd-walk-model-create nil)))
    (setq model (org-gtd-inbox-walk--meta-put-dup model token "A title" "* A title\ncontent"))
    (let ((value (org-gtd-inbox-walk--meta-get model token)))
      (assert-equal "A title" (plist-get value :title))
      (assert-equal "* A title\ncontent" (plist-get value :content)))))

(deftest inbox-walk/meta-dup-p-distinguishes-marker-from-duplicate ()
  "meta-dup-p is nil for a marker value, non-nil for a duplicate plist."
  (ogt--with-temp-org-buffer
   "* Some heading"
   (let ((marker (point-marker))
         (dup-value (list :title "T" :content "C")))
     (assert-nil (org-gtd-inbox-walk--meta-dup-p marker))
     (assert-true (org-gtd-inbox-walk--meta-dup-p dup-value)))))

(deftest inbox-walk/model-with-mixed-meta-stays-valid ()
  "A model carrying both marker and duplicate meta entries stays valid.
Validity only inspects entries + cursor; meta shape never affects it,
and this meta -- live markers -- is intentionally never serialized
(resume is deferred, D5b)."
  (ogt--with-temp-org-buffer
   "* Some heading"
   (let* ((marker (point-marker))
          (marker-token (org-gtd-inbox-walk--token))
          (dup-token (org-gtd-inbox-walk--token))
          (model (org-gtd-walk-model-create (list marker-token dup-token))))
     (setq model (org-gtd-inbox-walk--meta-put-marker model marker-token marker))
     (setq model (org-gtd-inbox-walk--meta-put-dup model dup-token "T" "* T\n"))
     (assert-true (org-gtd-walk-model-valid-p model)))))

;;; Render Tests (Task 3)

(deftest inbox-walk/render-marker-token-fills-surface-with-source-item ()
  "Render on a marker token draws the item, activates clarify mode,
sets the source marker + clarify id, and strips state properties."
  (capture-inbox-item "Render me")
  (let* ((model (org-gtd-inbox-walk--build-model))
         (token (car (plist-get model :entries)))
         (surface (org-gtd-inbox-walk--surface))
         clarify-id)
    (with-current-buffer surface
      (setq-local org-gtd-walk--active (list :model model)))
    (org-gtd-inbox-walk--render token surface)
    (with-current-buffer surface
      (assert-true (derived-mode-p 'org-gtd-clarify-mode))
      (assert-match "Render me" (buffer-string))
      (assert-true (markerp org-gtd-clarify--source-heading-marker))
      (assert-match "Render me"
                    (org-with-point-at org-gtd-clarify--source-heading-marker
                      (org-get-heading t t t t)))
      (assert-true org-gtd-clarify--clarify-id)
      (goto-char (point-min))
      (assert-nil (org-entry-get (point) org-gtd-timestamp))
      (assert-nil (org-entry-get (point) org-gtd-prop-project))
      (setq clarify-id org-gtd-clarify--clarify-id))
    (org-gtd-wip--cleanup-temp-file clarify-id)))

(deftest inbox-walk/render-duplicate-token-inserts-content-with-fresh-id ()
  "Render on a duplicate token inserts its content fresh, assigns a
new readable id (not the stale one carried in :content), and sets no
source marker (D4a)."
  (let* ((model (org-gtd-walk-model-create nil))
         (token (org-gtd-inbox-walk--token))
         (content "* Buy groceries\n:PROPERTIES:\n:ID: stale-leftover-id\n:END:\n")
         (surface (org-gtd-inbox-walk--surface))
         clarify-id)
    (setq model (org-gtd-inbox-walk--meta-put-dup model token "Buy groceries" content))
    (with-current-buffer surface
      (setq-local org-gtd-walk--active (list :model model)))
    (org-gtd-inbox-walk--render token surface)
    (with-current-buffer surface
      (assert-true (derived-mode-p 'org-gtd-clarify-mode))
      (assert-match "Buy groceries" (buffer-string))
      (assert-nil org-gtd-clarify--source-heading-marker)
      (assert-true org-gtd-clarify--clarify-id)
      (goto-char (point-min))
      (let ((id (org-entry-get (point) "ID")))
        (assert-true id)
        (assert-match "buy-groceries" (downcase id))
        (refute-match "stale-leftover-id" id))
      (setq clarify-id org-gtd-clarify--clarify-id))
    (org-gtd-wip--cleanup-temp-file clarify-id)))

(deftest inbox-walk/render-auto-skips-dead-marker-to-next-entry ()
  "A dead marker (source buffer killed) auto-skips to the next entry
via org-gtd-walk-advance instead of erroring (D2 durability guard)."
  (capture-inbox-item "First item")
  (let* ((additional-file (f-join org-gtd-directory "second-inbox.org")))
    (with-current-buffer (find-file-noselect additional-file)
      (insert "* Second item\n")
      (basic-save-buffer))
    (let* ((org-gtd-additional-inbox-files (list additional-file))
           (model (org-gtd-inbox-walk--build-model))
           (tokens (plist-get model :entries))
           (token-1 (nth 0 tokens))
           (marker-1 (org-gtd-inbox-walk--meta-get model token-1))
           (surface (org-gtd-inbox-walk--surface))
           (spec (list :name 'inbox :render #'org-gtd-inbox-walk--render
                       :scope "inbox-walk-test-scope"))
           clarify-id)
      (assert-equal 2 (length tokens))
      ;; Kill the first item's source buffer so its marker dies.
      (kill-buffer (marker-buffer marker-1))
      (with-current-buffer surface
        (setq-local org-gtd-walk--active
                    (list :model model :spec spec :surface surface :skipped 0)))
      (org-gtd-inbox-walk--render token-1 surface)
      (with-current-buffer surface
        (assert-match "Second item" (buffer-string))
        (refute-match "First item" (buffer-string))
        (assert-equal 1 (plist-get (plist-get org-gtd-walk--active :model) :cursor))
        (setq clarify-id org-gtd-clarify--clarify-id))
      (org-gtd-wip--cleanup-temp-file clarify-id))))

;;; Save-on-quit safety net (in-progress edits)

(deftest inbox-walk/render-leaves-surface-unmodified ()
  "After rendering an inbox item, the surface reports unmodified so a
later quit can distinguish an edited item (save its edits back) from an
untouched one (discard, no churn)."
  (capture-inbox-item "Glance and quit")
  (let* ((model (org-gtd-inbox-walk--build-model))
         (token (car (plist-get model :entries)))
         (surface (org-gtd-inbox-walk--surface))
         clarify-id)
    (with-current-buffer surface
      (setq-local org-gtd-walk--active (list :model model)))
    (org-gtd-inbox-walk--render token surface)
    (with-current-buffer surface
      (assert-nil (buffer-modified-p))
      (setq clarify-id org-gtd-clarify--clarify-id))
    (org-gtd-wip--cleanup-temp-file clarify-id)))

(deftest inbox-walk/quit-after-edit-saves-changes-back-to-inbox ()
  "Quitting inbox processing while mid-clarify on an edited item writes
the in-progress edits back over the source inbox heading, so re-running
picks the item up in its edited form (the save-on-quit safety net)."
  (capture-inbox-item "Buy milk")
  (let* ((model (org-gtd-inbox-walk--build-model))
         (surface (org-gtd-inbox-walk--surface)))
    (org-gtd-walk-start (org-gtd-inbox-walk--spec) surface model)
    (with-current-buffer surface
      ;; Simulate the user editing the item before deciding on it.
      (goto-char (point-min))
      (search-forward "Buy milk")
      (replace-match "Buy oat milk")
      (assert-true (buffer-modified-p))
      (org-gtd-clarify--stop-walk))
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (assert-match "Buy oat milk" (buffer-string)))))

(deftest inbox-walk/quit-without-edit-leaves-inbox-heading-intact ()
  "Quitting without touching the item leaves the source inbox heading
exactly as captured (the gate: no needless rewrite of a glanced item)."
  (capture-inbox-item "Untouched item")
  (let* ((model (org-gtd-inbox-walk--build-model))
         (surface (org-gtd-inbox-walk--surface)))
    (org-gtd-walk-start (org-gtd-inbox-walk--spec) surface model)
    (with-current-buffer surface
      (org-gtd-clarify--stop-walk))
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (goto-char (point-min))
      (assert-equal 1 (length (org-map-entries #'point-marker "LEVEL=1")))
      (assert-match "Untouched item" (buffer-string)))))

(deftest inbox-walk/kill-buffer-after-edit-saves-changes-back-to-inbox ()
  "Killing the surface buffer directly (C-x k) is also an abandonment:
in-progress edits are written back over the source inbox heading and the
walk's scope lock is released."
  (capture-inbox-item "Kill me")
  (let* ((model (org-gtd-inbox-walk--build-model))
         (surface (org-gtd-inbox-walk--surface))
         (spec (org-gtd-inbox-walk--spec)))
    (org-gtd-walk-start spec surface model)
    (with-current-buffer surface
      (goto-char (point-min))
      (search-forward "Kill me")
      (replace-match "Kill me EDITED"))
    ;; Directly killing the surface fires the buffer-local kill-buffer
    ;; hooks (query then cleanup), the non-`q' abandonment route.
    (kill-buffer surface)
    (assert-false (org-gtd-walk--scope-locked-p (plist-get spec :scope)))
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (assert-match "Kill me EDITED" (buffer-string)))))

(deftest inbox-walk/skip-to-duplicate-does-not-save-current-edits ()
  "Stopping while a duplicate is pending SKIPS the current item to the
duplicate (an explicit discard of the current item), so edits to the
current item are NOT written back to the inbox."
  (capture-inbox-item "Parent item")
  (let* ((base (org-gtd-inbox-walk--build-model))
         (dup-token (org-gtd-inbox-walk--token))
         (with-dup (org-gtd-inbox-walk--meta-put-dup
                    base dup-token "Dup" "* Dup\n"))
         (model (org-gtd-walk-model-enqueue with-dup dup-token 'top))
         (surface (org-gtd-inbox-walk--surface)))
    (org-gtd-walk-start (org-gtd-inbox-walk--spec) surface model)
    (with-current-buffer surface
      (goto-char (point-min))
      (search-forward "Parent item")
      (replace-match "Parent item EDITED")
      ;; A duplicate is pending after the cursor, so stop takes the
      ;; skip-to-duplicate branch, not the quit-and-save branch.
      (assert-true (org-gtd-clarify--walk-pending-duplicates))
      (org-gtd-clarify--stop-walk)
      ;; Tear down the now-active-on-the-duplicate walk.
      (let ((org-gtd-clarify--source-heading-marker nil))
        (org-gtd-walk-quit)))
    (with-current-buffer (find-file-noselect (org-gtd-inbox-path))
      (refute-match "EDITED" (buffer-string))
      (assert-match "Parent item" (buffer-string)))))

(provide 'inbox-walk-test)

;;; inbox-walk-test.el ends here
