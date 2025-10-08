;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



;; Load test helpers via setup.el (which now uses require internally)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Higher horizons"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "when org-gtd should show the horizons"

  (before-each (setq org-gtd-clarify-show-horizons 'right))
  (after-each (setq org-gtd-clarify-show-horizons nil))

  (describe
   "when we clarify an item"

   (it "creates a templated file when there isn't one"
       (capture-inbox-item "Add a configuration option")
       (org-gtd-process-inbox)
       (expect (get-buffer-window "horizons.org")
               :not :to-be
               nil)
       (expect (get-buffer-window (car (org-gtd-wip--get-buffers)))
               :not :to-be
               nil))

   (it "shows the existing file if there is one"
       (ogt--create-org-file-in-org-gtd-dir
        "horizons"
        "We are the champions")
       (capture-inbox-item "Add a configuration option")
       (org-gtd-process-inbox)
       (expect (get-buffer-window "horizons.org")
               :not :to-be
               nil)
       (expect (get-buffer-window (car (org-gtd-wip--get-buffers)))
               :not :to-be
               nil)
       (expect (file-raw-text "horizons.org")
               :to-match
               "We are the champions"))

   (it "when we return to a WIP buffer"
       (capture-inbox-item "Add a configuration option")
       (org-gtd-process-inbox)
       (set-buffer "*scratch*")
       (delete-other-windows)
       (with-simulated-input
        "TAB RET"
        (org-gtd-clarify-switch-to-buffer))
       (expect (get-buffer-window "horizons.org")
               :not :to-be
               nil))))


 (describe
  "when org-gtd should not show the horizons"
  (before-each (setq org-gtd-clarify-show-horizons nil))
  (after-each (setq org-gtd-clarify-show-horizons nil))

  (it "does not show the window"
             (capture-inbox-item "Add a configuration option")
       (org-gtd-process-inbox)
       (expect (get-buffer-window "horizons.org")
               :to-be
               nil))))
