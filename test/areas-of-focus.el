;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Areas of focus"

 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs)
              (add-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
              (setq org-gtd-areas-of-focus '("Health" "Home" "Career")))
 (after-each (ogt--close-and-delete-files)
             (remove-hook 'org-gtd-organize-hooks #'org-gtd-set-area-of-focus)
             (setq org-gtd-areas-of-focus nil))

 (describe
  "org-mode CATEGORY"

  (it "is set on clarified item from a customizable list"
      (ogt-capture-single-item "Medical Appointment")
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
                              (org-gtd-set-area-of-focus))
        (expect (org-entry-get (point) "CATEGORY")
                :to-equal
                "Health")
        (kill-buffer)))))
