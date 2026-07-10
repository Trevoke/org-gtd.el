;;; view-manager-build-test.el --- Tests for the builder transient -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2026 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Thin integration tests for the view-manager builder transient.  The
;; interactive read/preview loop is verified manually (see the commit body);
;; here we only assert the transient exists and its keys are wired.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

(deftest view-manager-build/is-a-transient-prefix ()
  "The builder is defined as a transient prefix command."
  (assert-true (fboundp 'org-gtd-view-manager--build)))

(deftest view-manager-build/has-save-suffix ()
  "The builder binds `s' to the save action."
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager--build "s")))
    (assert-equal "s" (plist-get plist :key))))

(deftest view-manager-build/has-type-infix ()
  "The builder binds `t' to the generated type infix."
  (let ((plist (ogt--transient-suffix-plist 'org-gtd-view-manager--build "t")))
    (assert-equal "t" (plist-get plist :key))))

(deftest view-manager-build/every-infix-and-action-key-is-bound ()
  "Every generated infix key and every action key stays bound.
Regression guard for the multi-column layout: rearranging the five
infix groups into side-by-side columns must not drop any infix.  Keys
are read from `org-gtd-view-manager--filter-specs' so this can never
drift from the single source of truth the builder is generated from."
  (dolist (entry org-gtd-view-manager--filter-specs)
    (let* ((key (plist-get (cdr entry) :key))
           (plist (ogt--transient-suffix-plist
                   'org-gtd-view-manager--build key)))
      (assert-equal key (plist-get plist :key))))
  (dolist (key '("RET" "s" "C-c C-k"))
    (let ((plist (ogt--transient-suffix-plist
                  'org-gtd-view-manager--build key)))
      (assert-equal key (plist-get plist :key)))))

(deftest view-manager-build/section-keys-are-bound ()
  "The Sections group binds all six section-management keys."
  (dolist (key '("M-a" "M-n" "M-p" "M-k" "M-<up>" "M-<down>"))
    (let ((plist (ogt--transient-suffix-plist
                  'org-gtd-view-manager--build key)))
      (assert-equal key (plist-get plist :key)))))

(deftest view-manager-build/preview-on-open-compiles-whole-view ()
  "Opening a builder on a blocks spec previews the composite (blocks) spec."
  (let ((captured 'unset)
        (org-gtd-view-manager--preview-last nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore)
              ((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (spec) (setq captured spec))))
      (org-gtd-view-manager--build
       '((name . "Engage")
         (blocks . (((type . calendar)) ((type . next-action)))))))
    (assert-equal 2 (length (cdr (assq 'blocks captured))))
    (assert-equal "Engage" (cdr (assq 'name captured)))))

(deftest view-manager-build/save-rejects-blank-name ()
  "A blank name errors, writes nothing, and leaves the builder dirty.
Guards against silently persisting a nameless `(name . \"\")' entry
that would surface as a blank candidate in `org-gtd-view-run'."
  (org-gtd-view-manager--build-load '((name . "Untitled") (type . next-action)))
  (setq org-gtd-view-manager--build-dirty t)
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) ""))
            ;; Keep the test hermetic: don't register a resume timer.
            ((symbol-function 'org-gtd-view-manager--build-resume) #'ignore))
    (assert-raises 'user-error (org-gtd-view-manager--save)))
  (assert-equal nil (org-gtd-view-manager--store-read))
  (assert-true org-gtd-view-manager--build-dirty))

(deftest view-manager-build/flag-infix-toggles-off ()
  "Re-selecting a flag infix unsets it.
Regression: a flag such as `not-done' could be set but never cleared,
so a builder session could not remove it once added."
  (setq org-gtd-view-manager--build-state (list (cons 'name "x")))
  (org-gtd-view-manager--set-value 'not-done)
  (assert-equal t (cdr (assq 'not-done org-gtd-view-manager--build-state)))
  (org-gtd-view-manager--set-value 'not-done)
  (assert-nil (assq 'not-done org-gtd-view-manager--build-state))
  (when (timerp org-gtd-view-manager--preview-timer)
    (cancel-timer org-gtd-view-manager--preview-timer)))

(deftest view-manager-build/renders-preview-on-open-fresh ()
  "Opening a fresh builder renders the preview once for the default spec.
Regression: the builder used to show nothing until the first RET/infix,
so a stale agenda from a prior action looked like the builder's preview."
  (let ((count 0)
        (captured 'unset)
        (org-gtd-view-manager--preview-last nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore)
              ((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (spec) (cl-incf count) (setq captured spec))))
      (org-gtd-view-manager--build))
    (assert-equal 1 count)
    (assert-equal 'next-action (alist-get 'type captured))
    (assert-equal "Untitled" (alist-get 'name captured))))

(deftest view-manager-build/renders-preview-on-open-edit ()
  "Editing an existing view renders that view's stored spec once on open."
  (let ((count 0)
        (captured 'unset)
        (org-gtd-view-manager--preview-last nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore)
              ((symbol-function 'org-gtd-view-manager--render-preview)
               (lambda (spec) (cl-incf count) (setq captured spec))))
      (org-gtd-view-manager--build '((name . "Saved") (type . delegated))))
    (assert-equal 1 count)
    (assert-equal 'delegated (alist-get 'type captured))
    (assert-equal "Saved" (alist-get 'name captured))))

(deftest view-manager-build/render-on-open-populates-cache ()
  "The on-open render seeds `--preview-last', so an identical debounce no-ops.
RET still force-renders (covered in view-manager-preview-test); this only
guards that the open render is not itself skipped and does update the cache."
  (let ((org-gtd-view-manager--preview-last nil))
    (cl-letf (((symbol-function 'transient-setup) #'ignore)
              ((symbol-function 'org-gtd-view-manager--render-preview) #'ignore))
      (org-gtd-view-manager--build '((name . "Saved") (type . delegated))))
    (assert-equal 'delegated (alist-get 'type org-gtd-view-manager--preview-last))))

(deftest view-manager-build/summary-label-is-a-heading ()
  "The summary's `View:' label carries `transient-heading' and shows the
view name plus a section marker."
  (org-gtd-view-manager--build-load '((name . "Untitled") (type . next-action)))
  (let ((summary (org-gtd-view-manager--build-summary)))
    (assert-equal 'transient-heading (get-text-property 0 'face summary))
    (assert-true (string-prefix-p "View: " (substring-no-properties summary)))
    (assert-true (string-match-p "Untitled" (substring-no-properties summary)))
    (assert-true (string-match-p "Section 1/1"
                                 (substring-no-properties summary)))))

(deftest view-manager-build/section-rename-key-is-bound ()
  "The Sections group binds `M-r' to the rename command."
  (let ((plist (ogt--transient-suffix-plist
                'org-gtd-view-manager--build "M-r")))
    (assert-equal "M-r" (plist-get plist :key))))

(deftest view-manager-build/section-rename-sets-title ()
  "Rename stores the entered string as the active section's title."
  (org-gtd-view-manager--build-load '((name . "V") (type . next-action)))
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "My focus"))
            ((symbol-function 'org-gtd-view-manager--preview-schedule) #'ignore))
    (org-gtd-view-manager--section-rename))
  (assert-equal "My focus"
                (cdr (assq 'title (nth 0 org-gtd-view-manager--build-sections))))
  (assert-true org-gtd-view-manager--build-dirty))

(deftest view-manager-build/section-rename-blank-clears-title ()
  "A blank rename input removes the title (falls back to the badge)."
  (org-gtd-view-manager--build-load
   '((name . "V") (blocks . (((name . "Kept") (type . next-action))
                             ((name . "Other") (type . delegated))))))
  ;; Active is section 0, which loaded with title "Kept".
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   "))
            ((symbol-function 'org-gtd-view-manager--preview-schedule) #'ignore))
    (org-gtd-view-manager--section-rename))
  (assert-nil (assq 'title (nth 0 org-gtd-view-manager--build-sections))))

;;; view-manager-build-test.el ends here
