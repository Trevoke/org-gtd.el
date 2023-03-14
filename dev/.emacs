(setq package-enable-at-startup nil)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)

(require 'package)
(package-initialize)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa 'use-package)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(setq use-package-always-ensure t)

;;  have to do this because org comes preinstalled with emacs
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

(straight-use-package '(org-gtd :type git :host github :repo "trevoke/org-gtd.el"))
(require 'org-gtd)

;; (use-package org
;;   :quelpa ((org) :upgrade t)
;;   :demand t
;;   :ensure t
;;   :init
;;   (setq org-directory "~/orgnotes/")
;;   (setq org-id-track-globally t)
;;   (setq org-id-locations-file "~/.emacs.d/.org-id-locations"))

;; (use-package org-gtd
;;   :after org
;;   :quelpa ((org-gtd :fetcher github :repo "trevoke/org-gtd.el")
;; 	   :upgrade t)
;;   :demand t
;;   :custom
;;   (org-agenda-property-position 'next-line)
;;   (org-edna-use-inheritance t)
;;   :config
;;   (org-edna-mode)
;;   :bind
;;   (("C-c d c" . org-gtd-capture)
;;    ("C-c d e" . org-gtd-engage)
;;    ("C-c d p" . org-gtd-process-inbox)
;;    ("C-c d n" . org-gtd-show-all-next)
;;    ("C-c d s" . org-gtd-show-stuck-projects)
;;    :map org-gtd-process-map
;;    ("C-c c" . org-gtd-choose)))

;; (use-package org-agenda
;;   :ensure nil
;;   :after org-gtd
;;   :custom
;;   (org-agenda-window-setup 'only-window))

(use-package camcorder :quelpa t)

(use-package command-log-mode :quelpa t)

(show-paren-mode)

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
(defun my-minibuffer-setup ()
       (set (make-local-variable 'face-remapping-alist)
          '((default :height 2.0))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(camcorder-window-id-offset 2)
 '(command-log-mode-auto-show t)
 '(command-log-mode-is-global t)
 '(command-log-mode-open-log-turns-on-mode t)
 '(command-log-mode-window-size 40)
 '(package-selected-packages '(camcorder org-gtd quelpa-use-package)))
