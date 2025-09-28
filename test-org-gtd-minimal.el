;;; test-org-gtd-minimal.el --- Minimal org-gtd configuration for testing -*- lexical-binding: t; -*-

;; This file provides a minimal configuration for testing org-gtd
;; Use with: emacs -Q -l test-org-gtd-minimal.el

;;; Configuration:

;; Suppress version warning
(setq org-gtd-update-ack "3.0.0")

;; Set up package archives and quelpa for package installation
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Install and setup quelpa
(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

(require 'quelpa)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package org-gtd
      :after org
      :quelpa ((org-gtd :fetcher github :repo "trevoke/org-gtd.el")
               :upgrade t)
      :demand t
      :init
      (setq org-gtd-update-ack "3.0.0")
      :custom
      (org-gtd-directory "~/tmp-gtd-test/")
      (org-agenda-property-position 'next-line)
      (org-edna-use-inheritance t)
      :config
      (org-edna-mode 1)
      :bind
      (("C-c d c" . org-gtd-capture)
       ("C-c d e" . org-gtd-engage)
       ("C-c d p" . org-gtd-process-inbox)
       ("C-c d n" . org-gtd-show-all-next)
       ("C-c d s" . org-gtd-show-stuck-projects)
       :map org-gtd-clarify-map
       ("C-c c" . org-gtd-choose)))

;; Basic org-mode settings
(setq org-log-done t
      org-log-into-drawer t)

;; Configure org-edna for project state management

;; Set up some basic GTD areas of focus for testing
(setq org-gtd-areas-of-focus '("Work" "Personal" "Health"))

;; Enable org-gtd mode
(org-gtd-mode 1)

;;; Helper functions for testing:


;;; test-org-gtd-minimal.el ends here
