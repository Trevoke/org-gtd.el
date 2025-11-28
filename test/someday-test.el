;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "A someday/maybe item"

 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "has ORG_GTD property set to Someday"
     (create-someday-item "Learn Spanish")
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Learn Spanish")
       (expect (org-entry-get (point) "ORG_GTD") :to-equal "Someday")))

 (it "has no timestamp properties (no ORG_GTD_TIMESTAMP, SCHEDULED, or DEADLINE)"
     (create-someday-item "Build a treehouse")
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Build a treehouse")
       (expect (org-entry-get (point) "ORG_GTD_TIMESTAMP") :to-be nil)
       (expect (org-entry-get (point) "SCHEDULED") :to-be nil)
       (expect (org-entry-get (point) "DEADLINE") :to-be nil)))

 (it "is refiled to a heading with ORG_GTD_REFILE: Someday property"
     (create-someday-item "Visit Japan")
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "Visit Japan")
       ;; Navigate to parent heading
       (org-up-heading-safe)
       (expect (org-entry-get (point) "ORG_GTD_REFILE") :to-equal "Someday"))))

(defun create-someday-item (topic)
  "Test helper to create a someday item with TOPIC."
  (let ((buffer (generate-new-buffer "Org GTD test buffer"))
        (org-id-overriding-file-name "org-gtd"))
    (with-current-buffer buffer
      (org-mode)
      (insert (format "* %s" topic))
      (org-gtd-clarify-item)
      (org-gtd-someday))
    (kill-buffer buffer)))

;;; someday-test.el ends here
