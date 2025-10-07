;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)
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

  (it "restores the window configuration"
      (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
            (window-config nil)
            (org-gtd-refile-to-any-target t))
        (set-buffer source-buffer)
        (org-gtd-clarify-item)
        (setq window-config org-gtd-clarify--window-config)
        (org-gtd-single-action)

        (expect (compare-window-configurations (current-window-configuration) window-config)
                :to-be t)))

  (it "kills the temp buffer"
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
        (expect (buffer-size) :to-equal 0)))

  (it "triggers only the relevant hooks"
      (let* ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify"))
             (org-gtd-refile-to-any-target t)
             (org-gtd-organize-hooks '(hook1 hook2)))
        (set-buffer source-buffer)
        (org-gtd-clarify-item)
        (organize-as-single-action)
        (with-current-buffer (org-gtd--default-file)
          (expect (ogt--current-buffer-raw-text)
                  :to-match "HOOK2"))
        )))

 (describe
  "hook filter helper"

  (it "treats a single argument properly as a list"
      (expect (org-gtd-organize-type-member-p 'everything)
              :to-be-truthy))

  (it "is truthy as long as 'everything is in the list"
      (expect (org-gtd-organize-type-member-p '(incubated trash everything project-task))
              :to-be-truthy))

  (it "signals an error if any element in the list is not one of the expected members"
      (expect (org-gtd-organize-type-member-p '(foobar))
              :to-throw 'org-gtd-invalid-organize-action-type-error))

  (it "is truthy if the buffer-local variable is in the list"
      (with-temp-buffer
        (setq-local org-gtd--organize-type 'quick-action)
        (expect (org-gtd-organize-type-member-p '(incubated quick-action delegated))
                :to-be-truthy)))

  (it "is falsey if the buffer-local variable is not the list"
      (with-temp-buffer
        (setq-local org-gtd--organize-type 'trash)
        (expect (org-gtd-organize-type-member-p '(incubated quick-action delegated))
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
       (expect 'save-some-buffers :to-have-been-called-with t)
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
