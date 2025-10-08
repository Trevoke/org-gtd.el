;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "org-gtd-items configuration"

 (before-each (setq inhibit-message t)
              (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "project-task configuration"

  (it "includes ORG_GTD property set to Actions value"
      ;; Test that :project-task configuration includes ORG_GTD property
      (let ((project-task-config (alist-get :project-task (org-gtd-items))))
        (expect project-task-config :not :to-be nil)
        (expect (alist-get 'ORG_GTD project-task-config) :not :to-be nil)
        (expect (alist-get 'ORG_GTD project-task-config) :to-equal "Actions")))))
