;;; view-manager-sections-test.el --- Tests for the section state model -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni
;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;; Pure unit tests for the multi-section builder state transitions.
;;; Code:

(require 'e-unit)
(require 'org-gtd-view-manager)

(e-unit-initialize)

(defun view-manager-sections-test--seed (sections active)
  "Seed the builder section state for a test."
  (setq org-gtd-view-manager--build-name "V")
  (setq org-gtd-view-manager--build-sections sections)
  (setq org-gtd-view-manager--build-active active)
  (setq org-gtd-view-manager--build-state (nth active sections)))

(deftest view-manager-sections/sync-writes-state-back ()
  "Sync copies the live --build-state into its section slot."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'next-action)) (list (cons 'type 'delegated))) 0)
  ;; Simulate --set-value reassigning --build-state to a new list.
  (setq org-gtd-view-manager--build-state
        (list (cons 'type 'next-action) (cons 'area-of-focus "Work")))
  (org-gtd-view-manager--build-sync-active)
  (assert-equal "Work"
                (cdr (assq 'area-of-focus
                           (nth 0 org-gtd-view-manager--build-sections)))))

(deftest view-manager-sections/add-appends-and-activates ()
  "Add appends a default next-action section and makes it active."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))) 0)
  (org-gtd-view-manager--build-add-section)
  (assert-equal 2 (length org-gtd-view-manager--build-sections))
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action (cdr (assq 'type org-gtd-view-manager--build-state))))

(deftest view-manager-sections/next-prev-switch-active ()
  "Next/prev move the active index and reload --build-state; clamped at ends."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))
         (list (cons 'type 'delegated))) 0)
  (org-gtd-view-manager--build-next-section)
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action (cdr (assq 'type org-gtd-view-manager--build-state)))
  (org-gtd-view-manager--build-prev-section)
  (assert-equal 0 org-gtd-view-manager--build-active)
  ;; prev at 0 is a no-op (clamped).
  (org-gtd-view-manager--build-prev-section)
  (assert-equal 0 org-gtd-view-manager--build-active)
  ;; next past the end is a no-op (clamped).
  (org-gtd-view-manager--build-next-section)
  (org-gtd-view-manager--build-next-section)
  (org-gtd-view-manager--build-next-section)
  (assert-equal 2 org-gtd-view-manager--build-active))

(deftest view-manager-sections/delete-refuses-last ()
  "Deleting the only section is refused; state is unchanged."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'next-action))) 0)
  (assert-nil (org-gtd-view-manager--build-delete-section))
  (assert-equal 1 (length org-gtd-view-manager--build-sections)))

(deftest view-manager-sections/delete-active-moves-to-neighbor ()
  "Deleting the active section drops it and clamps active to a neighbor."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))
         (list (cons 'type 'delegated))) 2)
  (assert-true (org-gtd-view-manager--build-delete-section))
  (assert-equal 2 (length org-gtd-view-manager--build-sections))
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action (cdr (assq 'type org-gtd-view-manager--build-state))))

(deftest view-manager-sections/move-up-swaps-and-follows ()
  "Move-up swaps with the previous section; active follows the moved one."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))) 1)
  (org-gtd-view-manager--build-move-section-up)
  (assert-equal 0 org-gtd-view-manager--build-active)
  (assert-equal 'next-action
                (cdr (assq 'type (nth 0 org-gtd-view-manager--build-sections))))
  (assert-equal 'calendar
                (cdr (assq 'type (nth 1 org-gtd-view-manager--build-sections)))))

(deftest view-manager-sections/move-up-at-top-is-noop ()
  "Move-up at index 0 changes nothing."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))) 0)
  (org-gtd-view-manager--build-move-section-up)
  (assert-equal 0 org-gtd-view-manager--build-active)
  (assert-equal 'calendar
                (cdr (assq 'type (nth 0 org-gtd-view-manager--build-sections)))))

(deftest view-manager-sections/move-down-swaps-and-follows ()
  "Move-down swaps with the next section; active follows the moved one."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))) 0)
  (org-gtd-view-manager--build-move-section-down)
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action
                (cdr (assq 'type (nth 0 org-gtd-view-manager--build-sections))))
  (assert-equal 'calendar
                (cdr (assq 'type (nth 1 org-gtd-view-manager--build-sections))))
  (assert-equal 'calendar (cdr (assq 'type org-gtd-view-manager--build-state))))

(deftest view-manager-sections/move-down-at-bottom-is-noop ()
  "Move-down at the last index changes nothing."
  (view-manager-sections-test--seed
   (list (list (cons 'type 'calendar))
         (list (cons 'type 'next-action))) 1)
  (org-gtd-view-manager--build-move-section-down)
  (assert-equal 1 org-gtd-view-manager--build-active)
  (assert-equal 'next-action
                (cdr (assq 'type (nth 1 org-gtd-view-manager--build-sections)))))

(deftest view-manager-load/fresh-is-one-default-section ()
  "A nil spec seeds one Untitled next-action section."
  (org-gtd-view-manager--build-load nil)
  (assert-equal "Untitled" org-gtd-view-manager--build-name)
  (assert-equal 1 (length org-gtd-view-manager--build-sections))
  (assert-equal 0 org-gtd-view-manager--build-active)
  (assert-equal 'next-action (cdr (assq 'type org-gtd-view-manager--build-state)))
  (assert-nil (assq 'name org-gtd-view-manager--build-state)))

(deftest view-manager-load/flat-spec-is-one-section ()
  "A flat spec loads name + one section (spec minus name)."
  (org-gtd-view-manager--build-load
   '((name . "Saved") (type . delegated) (who . "Sam")))
  (assert-equal "Saved" org-gtd-view-manager--build-name)
  (assert-equal 1 (length org-gtd-view-manager--build-sections))
  (assert-nil (assq 'name (nth 0 org-gtd-view-manager--build-sections)))
  (assert-equal 'delegated (cdr (assq 'type org-gtd-view-manager--build-state)))
  (assert-equal "Sam" (cdr (assq 'who org-gtd-view-manager--build-state))))

(deftest view-manager-load/blocks-spec-loads-section-list ()
  "A blocks spec loads name + the section list, active at 0."
  (org-gtd-view-manager--build-load
   '((name . "Engage")
     (blocks . (((type . calendar))
                ((type . next-action) (area-of-focus . "Work"))))))
  (assert-equal "Engage" org-gtd-view-manager--build-name)
  (assert-equal 2 (length org-gtd-view-manager--build-sections))
  (assert-equal 0 org-gtd-view-manager--build-active)
  (assert-equal 'calendar (cdr (assq 'type org-gtd-view-manager--build-state)))
  (assert-equal "Work"
                (cdr (assq 'area-of-focus
                           (nth 1 org-gtd-view-manager--build-sections)))))

(deftest view-manager-load/blocks-spec-strips-synthesized-names ()
  "Loaded blocks carry a synthesized `name' header, but sections are canonical:
`--build-sections' entries must have NO name (it is re-synthesized on compile)."
  (org-gtd-view-manager--build-load
   '((name . "Engage")
     (blocks . (((name . "calendar") (type . calendar))
                ((name . "next-action · Work")
                 (type . next-action) (area-of-focus . "Work"))))))
  (assert-nil (assq 'name (nth 0 org-gtd-view-manager--build-sections)))
  (assert-nil (assq 'name (nth 1 org-gtd-view-manager--build-sections)))
  (assert-equal 'calendar
                (cdr (assq 'type (nth 0 org-gtd-view-manager--build-sections))))
  (assert-equal "Work"
                (cdr (assq 'area-of-focus
                           (nth 1 org-gtd-view-manager--build-sections)))))

(provide 'view-manager-sections-test)
;;; view-manager-sections-test.el ends here
