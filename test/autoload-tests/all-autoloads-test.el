;; -*- lexical-binding: t; -*-
;;
;; Consolidated autoload tests - verifies all autoloaded entry points work.
;; This file runs in a fresh Emacs via ogt--recursive-eldev-test to ensure
;; only org-gtd-autoloads is loaded initially (not the full package).

(require 'buttercup)
(setq org-gtd-update-ack "3.0.0")
(load "org-gtd-autoloads")
(load "test/helpers/utils.el")
(load "test/helpers/autoload-setup.el")

(describe "autoload management"

  ;; Functions that don't need any setup
  (describe "basic autoloads (no setup required)"
    (it "org-gtd-mode"
      (org-gtd-mode))

    (it "org-gtd-clarify-mode"
      (org-gtd-clarify-mode)))

  ;; Functions that need the GTD directory to exist
  (describe "autoloads requiring GTD directory"
    (before-each (ogt--prepare-gtd-directory))
    (after-each (ogt--clear-gtd-directory))

    (it "org-gtd-archive-completed-items"
      (org-gtd-archive-completed-items))

    (it "org-gtd-capture"
      (org-gtd-capture nil "i"))

    (it "org-gtd-engage"
      (org-gtd-engage))

    (it "org-gtd-inbox-path"
      (org-gtd-inbox-path))

    (it "org-gtd-process-inbox"
      (org-gtd-process-inbox))

    (it "org-gtd-review-stuck-projects"
      (org-gtd-review-stuck-projects))

    (it "org-gtd-show-all-next"
      (org-gtd-show-all-next))

    (it "org-gtd-clarify-item"
      (ogt--with-temp-org-buffer
       "* This is the heading to clarify"
       (org-gtd-clarify-item)))))
