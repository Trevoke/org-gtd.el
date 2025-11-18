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
       (expect (get-buffer-window "*Org GTD Horizons View*")
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
       (expect (get-buffer-window "*Org GTD Horizons View*")
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
       (expect (get-buffer-window "*Org GTD Horizons View*")
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
               nil)))

 (describe
  "Read-only horizons view"

  (before-each (setq inhibit-message t
                     org-gtd-clarify-show-horizons 'right)
               (ogt--configure-emacs))
  (after-each (ogt--close-and-delete-files)
              (setq org-gtd-clarify-show-horizons nil))

  (it "creates a read-only indirect buffer for horizons view"
      (ogt--create-org-file-in-org-gtd-dir
       "horizons"
       "* Purpose and principles")
      (capture-inbox-item "Test item")
      (org-gtd-process-inbox)
      (let ((view-buffer (get-buffer "*Org GTD Horizons View*")))
        (expect view-buffer :not :to-be nil)
        (with-current-buffer view-buffer
          (expect buffer-read-only :to-be t)
          (expect (buffer-base-buffer) :not :to-be nil))))

  (it "view buffer reflects changes to horizons file"
      (ogt--create-org-file-in-org-gtd-dir
       "horizons"
       "* Purpose")
      (capture-inbox-item "Test item")
      (org-gtd-process-inbox)
      (let ((view-buffer (get-buffer "*Org GTD Horizons View*"))
            (horizons-buffer (get-buffer "horizons.org")))
        ;; Modify the horizons file buffer
        (with-current-buffer horizons-buffer
          (goto-char (point-max))
          (insert "\n* Vision"))
        ;; Check that view buffer sees the change
        (with-current-buffer view-buffer
          (expect (buffer-string) :to-match "Vision"))))

  (it "cleanup function kills the view buffer"
      (ogt--create-org-file-in-org-gtd-dir
       "horizons"
       "* Purpose")
      ;; Create the view buffer
      (org-gtd-clarify--get-or-create-horizons-view)
      (expect (get-buffer "*Org GTD Horizons View*") :not :to-be nil)
      ;; Clean it up
      (org-gtd-clarify--cleanup-horizons-view)
      (expect (get-buffer "*Org GTD Horizons View*") :to-be nil))

  (it "cleans up view buffer after one-off clarification completes"
      (ogt--create-org-file-in-org-gtd-dir
       "horizons"
       "* Purpose")
      (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                          "tasks"
                          "* TODO Test task")))
        (with-current-buffer task-buffer
          (goto-char (point-min))
          (org-next-visible-heading 1)
          ;; Start one-off clarification
          (org-gtd-clarify-item))
        ;; View buffer should exist during clarification
        (expect (get-buffer "*Org GTD Horizons View*") :not :to-be nil)
        ;; Switch to WIP buffer and organize as trash
        (with-current-buffer (car (org-gtd-wip--get-buffers))
          (org-gtd-trash))
        ;; View buffer should be cleaned up after organize
        (expect (get-buffer "*Org GTD Horizons View*") :to-be nil)))

  (it "cleans up view buffer after inbox processing completes"
      (ogt--create-org-file-in-org-gtd-dir
       "horizons"
       "* Purpose")
      (capture-inbox-item "Test inbox item")
      (org-gtd-process-inbox)
      ;; View buffer should exist during inbox processing
      (expect (get-buffer "*Org GTD Horizons View*") :not :to-be nil)
      ;; Organize as trash
      (with-current-buffer (car (org-gtd-wip--get-buffers))
        (org-gtd-trash))
      ;; View buffer should be cleaned up after inbox completes
      (expect (get-buffer "*Org GTD Horizons View*") :to-be nil))

  (it "stop clarifying command cleans up and restores state"
      (ogt--create-org-file-in-org-gtd-dir
       "horizons"
       "* Purpose")
      (let* ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                           "tasks"
                           "* TODO Test task"))
             (orig-window-config (current-window-configuration))
             wip-buffer)
        (with-current-buffer task-buffer
          (goto-char (point-min))
          (org-next-visible-heading 1)
          (org-gtd-clarify-item))
        (setq wip-buffer (car (org-gtd-wip--get-buffers)))
        ;; View buffer and WIP buffer should exist
        (expect (get-buffer "*Org GTD Horizons View*") :not :to-be nil)
        (expect wip-buffer :not :to-be nil)
        ;; Stop clarifying
        (with-current-buffer wip-buffer
          (org-gtd-clarify-stop))
        ;; WIP buffer should be killed
        (expect (buffer-live-p wip-buffer) :to-be nil)
        ;; View buffer should be cleaned up
        (expect (get-buffer "*Org GTD Horizons View*") :to-be nil)))

  (it "C-c C-k keybinding is bound to stop clarifying"
      (expect (lookup-key org-gtd-clarify-map (kbd "C-c C-k"))
              :to-equal #'org-gtd-clarify-stop))

  (it "header line shows both organize and stop keybindings"
      (ogt--create-org-file-in-org-gtd-dir
       "horizons"
       "* Purpose")
      (let ((task-buffer (ogt--create-org-file-in-org-gtd-dir
                          "tasks"
                          "* TODO Test task")))
        (with-current-buffer task-buffer
          (goto-char (point-min))
          (org-next-visible-heading 1)
          (org-gtd-clarify-item))
        (with-current-buffer (car (org-gtd-wip--get-buffers))
          (expect header-line-format :to-match "C-c c")
          (expect header-line-format :to-match "C-c C-k")
          (expect header-line-format :to-match "cancel"))))))
