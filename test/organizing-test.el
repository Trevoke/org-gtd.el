;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

(describe
 "Organizing (in 3.0)"


 (before-each (setq inhibit-message t)
              (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "cleanup"


  (before-each (setq inhibit-message t)
               (defun hook1 ()
                 (if (org-gtd-organize-type-member-p '(quick-action))
                     (org-entry-put (point) "HOOK1" "YES")))
               (defun hook2 ()
                 (if (org-gtd-organize-type-member-p '(single-action))
                     (org-entry-put (point) "HOOK2" "YES"))))
  (after-each (fmakunbound 'hook1)
              (fmakunbound 'hook2))

  (it "returns to previous window layout after organizing item"
      (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
            (window-config nil)
            (org-gtd-refile-to-any-target t))
        (set-buffer source-buffer)
        (org-gtd-clarify-item)
        (setq window-config org-gtd-clarify--window-config)
        (org-gtd-single-action)

        (expect (compare-window-configurations (current-window-configuration) window-config)
                :to-be t)))

  (it "cleans up temporary WIP buffer after organizing item"
      (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
            (org-gtd-refile-to-any-target t))
        (set-buffer source-buffer)
        (org-gtd-clarify-item)
        (org-gtd-single-action)
        (expect (org-gtd-wip--get-buffers) :to-be nil)))

  (it "deletes the source heading"
      (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
            (org-gtd-refile-to-any-target t))
        (set-buffer source-buffer)
        (org-gtd-clarify-item)
        (org-gtd-single-action)
        (expect (current-buffer-empty?) :to-be t)))

  (it "triggers only the relevant hooks"
      (let* ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
             (org-gtd-refile-to-any-target t)
             (org-gtd-organize-hooks '(hook1 hook2)))
        (set-buffer source-buffer)
        (org-gtd-clarify-item)
        (organize-as-single-action)
        (expect (file-contains? (org-gtd--default-file) "HOOK2") :to-be-truthy))))

 (describe
  "hook filter helper"

  (it "treats a single argument properly as a list"
      (expect (org-gtd-organize-type-member-p 'everything)
              :to-be-truthy))

  (it "is truthy as long as 'everything is in the list"
      (expect (org-gtd-organize-type-member-p '(tickler trash everything project-task))
              :to-be-truthy))

  (it "signals an error if any element in the list is not one of the expected members"
      (expect (org-gtd-organize-type-member-p '(foobar))
              :to-throw 'org-gtd-invalid-organize-action-type-error))

  (it "is truthy if the buffer-local variable is in the list"
      (with-temp-buffer
        (setq-local org-gtd--organize-type 'quick-action)
        (expect (org-gtd-organize-type-member-p '(tickler quick-action delegated))
                :to-be-truthy)))

  (it "is falsey if the buffer-local variable is not the list"
      (with-temp-buffer
        (setq-local org-gtd--organize-type 'trash)
        (expect (org-gtd-organize-type-member-p '(tickler quick-action delegated))
                :not :to-be-truthy)))))

(describe
 "Saving buffers after organizing"
 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "saves buffers if org-gtd-save-after-organize is t"
     (let ((org-gtd-save-after-organize t)
           (test-buffer (get-buffer-create "*org-gtd-test*")))
       (spy-on 'save-some-buffers :and-call-through)
       (with-current-buffer test-buffer
         (org-mode)
         (insert "* Test heading\n")
         (goto-char (point-min))
         (org-gtd-organize--call (lambda () (insert "Test"))))
       (expect 'save-some-buffers :to-have-been-called-with t #'org-gtd-buffer-p)
       (kill-buffer test-buffer)))

 (it "does not save buffers if org-gtd-save-after-organize is nil"
     (let ((org-gtd-save-after-organize nil)
           (test-buffer (get-buffer-create "*org-gtd-test*")))
       (spy-on 'save-some-buffers)
       (with-current-buffer test-buffer
         (org-mode)
         (insert "* Test heading\n")
         (goto-char (point-min))
         (org-gtd-organize--call (lambda () (insert "Test"))))
       (expect 'save-some-buffers :not :to-have-been-called)
       (kill-buffer test-buffer))))

(describe
 "org-gtd-organize--update-in-place"
 (before-each
   (setq inhibit-message t)
   (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "replaces original heading with WIP buffer content"
     (create-single-action "Original title")
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Original title")
       (org-back-to-heading t)
       ;; Clarify the item (this sets org-gtd-clarify--source-heading-marker)
       (org-gtd-clarify-item)
       ;; Get the WIP buffer and modify content
       (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
         (with-current-buffer wip-buffer
           (goto-char (point-min))
           (search-forward "Original title")
           (replace-match "Modified title")
           ;; Call update-in-place
           (org-gtd-organize--update-in-place)))
       ;; Verify original location has new content
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (expect (search-forward "Modified title" nil t) :to-be-truthy)
         (goto-char (point-min))
         (expect (search-forward "Original title" nil t) :to-be nil)))))

(describe
 "org-gtd-organize transient"
 (before-each
   (setq inhibit-message t)
   (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "has skip-refile toggle infix command defined"
     ;; Simply verify the infix command is defined
     (expect (fboundp 'org-gtd-organize--skip-refile-infix) :to-be-truthy)))

(describe
 "skip-refile behavior"
 (before-each
   (setq inhibit-message t)
   (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "updates single-action in place when skip-refile is set"
     (create-single-action "Update me")
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Update me")
       (org-back-to-heading t)
       ;; Clarify with skip-refile (C-u prefix)
       (let ((current-prefix-arg '(4)))
         (org-gtd-clarify-item))
       ;; Spy on org-refile to ensure it's NOT called when skip-refile is set
       (spy-on 'org-refile)
       ;; Modify in WIP buffer and re-organize
       (with-current-buffer (car (org-gtd-wip--get-buffers))
         (goto-char (point-min))
         (search-forward "Update me")
         (replace-match "Updated item")
         ;; With skip-refile, this should call update-in-place, not refile
         (org-gtd-single-action))
       ;; Verify org-refile was NOT called (skip-refile should use update-in-place)
       (expect 'org-refile :not :to-have-been-called)
       ;; Verify item was updated in place in the default file
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         ;; Should find "Updated item" exactly once (updated in place)
         (expect (how-many "Updated item" (point-min) (point-max)) :to-equal 1)
         ;; Original should be gone
         (goto-char (point-min))
         (expect (search-forward "Update me" nil t) :to-be nil)))))
