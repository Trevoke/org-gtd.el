;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "Additional inboxes"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "processes items from additional inbox after main inbox is empty"
     ;; Setup: Create item in main inbox
     (capture-inbox-item "Main inbox item")

     ;; Setup: Create additional inbox file with an item
     (let* ((additional-inbox-file (f-join org-gtd-directory "additional-inbox.org"))
            (additional-buffer (find-file-noselect additional-inbox-file)))
       (with-current-buffer additional-buffer
         (insert "* Additional inbox item\n")
         (basic-save-buffer))

       ;; Configure additional inboxes
       (let ((org-gtd-additional-inbox-files (list additional-inbox-file)))
         ;; Process inbox - should start with main inbox
         (org-gtd-process-inbox)

         ;; Organize the main inbox item
         (organize-as-single-action)

         ;; After main inbox empty, should continue to additional inbox
         ;; We should now be clarifying the additional inbox item
         (expect (ogt-get-wip-buffer) :not :to-be nil)
         (with-wip-buffer
           (expect (buffer-string) :to-match "Additional inbox item"))

         ;; Organize the additional inbox item
         (organize-as-single-action)

         ;; Both inboxes should now be empty
         (expect (file-contains? (org-gtd-inbox-path) "Main inbox item") :to-be nil)
         (expect (file-contains? additional-inbox-file "Additional inbox item") :to-be nil))))

 (it "skips empty additional inbox files"
     ;; Setup: Create item in main inbox only
     (capture-inbox-item "Only main item")

     ;; Setup: Create empty additional inbox file
     (let* ((empty-inbox-file (f-join org-gtd-directory "empty-inbox.org"))
            (empty-buffer (find-file-noselect empty-inbox-file)))
       (with-current-buffer empty-buffer
         (basic-save-buffer))

       ;; Configure additional inboxes
       (let ((org-gtd-additional-inbox-files (list empty-inbox-file)))
         ;; Process inbox
         (org-gtd-process-inbox)

         ;; Organize the only item
         (organize-as-single-action)

         ;; Should have completed without error
         (expect (file-contains? (org-gtd-inbox-path) "Only main item") :to-be nil))))

 (it "processes multiple additional inbox files in order"
     ;; Setup: Main inbox empty, two additional inboxes with items
     (let* ((inbox1-file (f-join org-gtd-directory "inbox1.org"))
            (inbox2-file (f-join org-gtd-directory "inbox2.org"))
            (inbox1-buffer (find-file-noselect inbox1-file))
            (inbox2-buffer (find-file-noselect inbox2-file)))

       (with-current-buffer inbox1-buffer
         (insert "* Item from inbox 1\n")
         (basic-save-buffer))

       (with-current-buffer inbox2-buffer
         (insert "* Item from inbox 2\n")
         (basic-save-buffer))

       ;; Configure additional inboxes
       (let ((org-gtd-additional-inbox-files (list inbox1-file inbox2-file)))
         ;; Process inbox (main inbox is empty)
         (org-gtd-process-inbox)

         ;; Should be processing item from inbox1
         (expect (ogt-get-wip-buffer) :not :to-be nil)
         (with-wip-buffer
           (expect (buffer-string) :to-match "Item from inbox 1"))

         ;; Organize first item
         (organize-as-single-action)

         ;; Should now be processing item from inbox2
         (expect (ogt-get-wip-buffer) :not :to-be nil)
         (with-wip-buffer
           (expect (buffer-string) :to-match "Item from inbox 2"))

         ;; Organize second item
         (organize-as-single-action)

         ;; All inboxes should be empty
         (expect (file-contains? inbox1-file "Item from inbox 1") :to-be nil)
         (expect (file-contains? inbox2-file "Item from inbox 2") :to-be nil))))

 (it "clears session state when user cancels with C-c C-k"
     ;; Setup: Create items in main inbox and additional inbox
     (capture-inbox-item "Main item")

     (let* ((additional-inbox-file (f-join org-gtd-directory "cancel-test-inbox.org"))
            (additional-buffer (find-file-noselect additional-inbox-file)))
       (with-current-buffer additional-buffer
         (insert "* Additional item\n")
         (basic-save-buffer))

       (let ((org-gtd-additional-inbox-files (list additional-inbox-file)))
         ;; Start processing
         (org-gtd-process-inbox)

         ;; Session should be active
         (expect org-gtd-process--session-active :to-be t)

         ;; Cancel clarification
         (org-gtd-clarify-stop)

         ;; Session state should be cleared
         (expect org-gtd-process--session-active :to-be nil)
         (expect org-gtd-process--pending-inboxes :to-be nil)

         ;; Starting again should re-initialize properly
         (org-gtd-process-inbox)
         (expect org-gtd-process--session-active :to-be t)
         (expect org-gtd-process--pending-inboxes :to-equal (list additional-inbox-file))

         ;; Clean up by canceling again
         (org-gtd-clarify-stop)))))
