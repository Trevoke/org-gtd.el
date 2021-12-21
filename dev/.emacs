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

(straight-use-package '(org-gtd :type git :host github :repo "trevoke/org-gtd.el" :branch "2.0.0"))

(show-paren-mode)

;; (straight-use-package 'use-package)
;; (setq use-package-always-demand t
;;       straight-use-package-by-default t)



;; (use-package org-gtd
;;   :after 'org
;;   :straight (:type git :host github :repo "trevoke/org-gtd.el" :branch "2.0.0"))


;; :bind
  ;; (("C-c d c" . org-gtd-capture)
  ;;  ("C-c d p" . org-gtd-process-inbox)
  ;;  ("C-c d a" . org-gtd-agenda-daily)
  ;;  :map org-gtd-process-map
  ;;        ("C-c c" . org-gtd-choose)))
