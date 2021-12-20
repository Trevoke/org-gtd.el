(setq package-enable-at-startup nil)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'bind-key)

;; ;; This lets use-package install packages that come built-in with emacs, like org-mode
(defun package-from-archive (f &rest args)
  (and (apply f args)
       (assq (car args) package-alist)))

(show-paren-mode)

(use-package f)
(use-package org-edna)
(use-package org-agenda-property)
(use-package transient)

(advice-add 'package-installed-p :around 'package-from-archive)
(use-package org
  :pin "gnu"
                                        ;:ensure org-plus-contrib
  :init
  (setq org-directory ".")
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/local-files/.org-id-locations"))
(advice-remove 'package-installed-p 'package-from-archive)

(use-package org-gtd
  ;:after (org f org-edna org-agenda-property transient)
  :demand t
  :load-path "/home/stag/src/projects/org-gtd.el/"
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d p" . org-gtd-process-inbox)
   (:map org-gtd-process-map
         ("C-c c" . org-gtd-choose))))

(use-package org-agenda
  :after org-gtd
  :ensure nil
  :demand t
  :bind
  (("C-c d a" . org-gtd-daily-agenda)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(camcorder transient org f org-agenda-property org-edna org-plus-contrib use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
