(use-package org-edna
  :config
  (org-edna-load)
  (setq org-edna-use-inheritance t))

(or org-gtd-directory
    (customize-save-variable 'org-gtd-directory
                             (file-name-as-directory (read-directory-name
                                                      "What is the root GTD directory? "
                                                      "~/"))))
(setq org-gtd-agenda-directory (concat org-directory "agenda"))
(setq org-agenda-files ('(org-gtd-agenda-directory)))
(setq diary-file (concat org-agenda-directory "diary-file.org"))

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(setq calendar-week-start-day 1) ;; Monday

(setq org-todo-keywords '( "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))

(setq org-default-notes-file (concat org-directory "capture-fallback.org"))


(setq org-capture-templates
      '(
        ("i" "Inbox"
         entry (file "Inbox.org")
         "* TODO %?\n  %i"
         :kill-buffer t)
        ("t" "Todo with link"
         entry (file "Inbox.org")
         "* TODO %?\n  %i\n  %a"
         :kill-buffer t)
        ("s" "Someday"
         entry (file "Someday.org")
         "* %i%? \n %U"
         :kill-buffer t)
        ("r" "Remind me"
         entry (file "Tickler.org")
         "* TODO %?\nSCHEDULED: %^{Remind me on:}t"
         :kill-buffer t)))


(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-log-refile 'time)
(setq org-refile-targets '(("Projects.org" :maxlevel . 1)
                           ("Someday.org" :maxlevel . 1)
                           ("Tickler.org" :maxlevel . 1)))

(defun org-gtd-refile ()
  (interactive)
  (org-set-tags-command)
  (org-refile))

(defun org-gtd-create-single-task-project ()
  (interactive)
  (org-refile nil nil `("Projects.org" ,stag-gtd-projects))
  (find-file stag-gtd-projects)
  (end-of-buffer)
  (previous-line)
  (kill-ring-save (point-at-bol) (point-at-eol))
  (org-end-of-line)
  (insert " [/]")
  (next-line)
  (org-end-of-line)
  (org-return)
  (org-yank)
  (org-beginning-of-line)
  (forward-word)
  (backward-word)
  (insert "TODO ")
  (org-metaright)
  (org-update-statistics-cookies t))

(setq org-agenda-diary-file 'diary-file)
(setq org-agenda-include-diary t)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-sticky t)
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)

(setq org-agenda-custom-commands
      (quote
       (("n" "Agenda and all TODOs"
         ((agenda "" nil)
          (alltodo "" nil))
         nil)
        ("N" "All NEXT actions" todo "NEXT"
         ((org-agenda-overriding-header ""))))))

(setq org-stuck-projects '("+LEVEL=1/-DONE"
                           ("TODO" "NEXT" "NEXTACTION")
                           nil ""))

(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-switchb)
(org-defkey org-mode-map "\C-cr" 'org-refile)
(org-defkey org-mode-map "\C-cs" 'org-gtd-refile)
(org-defkey org-mode-map "\C-co" 'org-gtd-create-single-task-project)
