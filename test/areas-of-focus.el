;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Areas of focus"


 (before-each (setq inhibit-message t)
              (ogt--configure-emacs)
              (setq org-gtd-areas-of-focus '("Health" "Home" "Career")))
 (after-each (ogt--close-and-delete-files)
             (setq org-gtd-areas-of-focus nil))

 (describe
  "org-mode CATEGORY"

 (before-each
  (add-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus))
 (after-each
  (remove-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus))

  (it "is set on clarified item from a customizable list"
      (capture-inbox-item "Medical Appointment")
      (org-gtd-process-inbox)
      (execute-kbd-macro (kbd "C-c c s H e a l t h RET"))
      (org-gtd-engage)
      (expect (ogt--buffer-string org-agenda-buffer)
              :to-match
              "Health.*Medical"))

  (it "is set on item at point from the areas of focus decoration"
      (with-current-buffer (get-buffer-create "temp.org")
        (org-mode)
        (insert "* A heading")
        (with-simulated-input "Health RET"
                              (org-gtd-area-of-focus-set-on-item-at-point))
        (expect (org-entry-get (point) "CATEGORY")
                :to-equal
                "Health")
        (kill-buffer))))

 (describe
  "through the agenda"
  (it "sets area of focus on single action task from agenda view"
      (create-single-action "foobar")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "foobar")
        (with-simulated-input
         "Home RET"
         (org-gtd-area-of-focus-set-on-agenda-item)))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "foobar")
        (expect (org-entry-get (point) "CATEGORY")
                :to-equal
                "Home")))

  (it "sets category on project heading if on project task"
      (create-project "my project")
      (org-gtd-engage)
      (with-current-buffer org-agenda-buffer
        (goto-char (point-min))
        (search-forward "Task 1")
        (with-simulated-input
         "Home RET"
         (org-gtd-area-of-focus-set-on-agenda-item)))
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-min))
        (search-forward "my project")
        (expect (org-entry-get (point) "CATEGORY")
                :to-equal
                "Home")))))
