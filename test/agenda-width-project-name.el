(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)

;; the the implementation of org-gtd-agenda-width-project-name

(describe
    "Engage view"

  :var ((inhibit-message t))

  (before-each (ogt--configure-emacs)
               (setq org-gtd-agenda-width-project-name 17))
  (after-each (ogt--close-and-delete-files)
              (setq org-gtd-agenda-width-project-name 12))

  (it "can have a specific agenda width"
    (ogt-capture-and-process-project "My long project name which needs shortening") ;; see test/helpers/processing.el
    (org-gtd-engage)
    (expect (message (ogt--buffer-string org-agenda-buffer))
            :to-match
            "My long project …")
    ))

(describe
    "Engage view"

  :var ((inhibit-message t))
  
  (before-each (ogt--configure-emacs)
               (setq org-gtd-agenda-width-project-name 5))
  (after-each (ogt--close-and-delete-files)
              (setq org-gtd-agenda-width-project-name 12))
  
  (it "can have a specific agenda width"
    (ogt-capture-and-process-project "P234567890")
    (org-gtd-engage)
    (expect (message (ogt--buffer-string org-agenda-buffer))
            :to-match
            "P234…")
    ))

(describe
    "Engage view"

  :var ((inhibit-message t))
  
  (before-each (ogt--configure-emacs)
               (setq org-gtd-agenda-width-project-name 1))
  (after-each (ogt--close-and-delete-files)
              (setq org-gtd-agenda-width-project-name 12))
  
  (it "can have a specific agenda width"
    (ogt-capture-and-process-project "P234567890")
    (org-gtd-engage)
    (expect (message (ogt--buffer-string org-agenda-buffer))
            :to-match
            "…")
    ))

(describe
    "Engage view"

  :var ((inhibit-message t))
  
  (before-each (ogt--configure-emacs)
               (setq org-gtd-agenda-width-project-name 10))
  (after-each (ogt--close-and-delete-files)
              (setq org-gtd-agenda-width-project-name 12))
  
  (it "can have a specific agenda width"
    (ogt-capture-and-process-project "P234567890")
    (org-gtd-engage)
    (expect (message (ogt--buffer-string org-agenda-buffer))
            :to-match
            "P234567890")
    ))

(describe
    "Engage view"

  :var ((inhibit-message t))
  
  (before-each (ogt--configure-emacs)
               (setq org-gtd-agenda-width-project-name 12))
  (after-each (ogt--close-and-delete-files)
              (setq org-gtd-agenda-width-project-name 12))
  
  (it "can have a specific agenda width"
    (ogt-capture-and-process-project "P234567890")
    (org-gtd-engage)
    (expect (message (ogt--buffer-string org-agenda-buffer))
            :to-match
            "P234567890  ")
    ))

(describe
    "Engage view"

  :var ((inhibit-message t))
  
  (before-each (ogt--configure-emacs)
               (setq org-gtd-agenda-width-project-name 50))
  (after-each (ogt--close-and-delete-files)
              (setq org-gtd-agenda-width-project-name 12))
  
  (it "can have a specific agenda width"
    (ogt-capture-and-process-project "P234567890")
    (org-gtd-engage)
    (expect (message (ogt--buffer-string org-agenda-buffer))
            :to-match
            "P234567890                                        ")
    ))
