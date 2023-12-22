(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)



(describe
 "Engage agenda prefix"
 :var ((inhibit-message t))

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe
  "link prettification"

  (it "replaces a link with its description"
      (ogt-capture-and-process-project "[[https://workera.ai][Classify Workera]] exam problems")
      (org-gtd-engage)
      (expect (ogt--buffer-string org-agenda-buffer)
              :to-match "Classify Wo…")))

 (describe
  "prefix width"

  (it "adds ellipses if name is too long"
      (ogt-capture-and-process-project "My long project name which needs shortening")
      (let ((org-gtd-engage-prefix-width 17))
        (org-gtd-engage))
      (expect (ogt--buffer-string org-agenda-buffer)
              :to-match
              "My long project …"))

  (it "shortens prefix words if necessary"
      (ogt-capture-and-process-project "P234567890")
      (let ((org-gtd-engage-prefix-width 5))
        (org-gtd-engage))
      (expect (ogt--buffer-string org-agenda-buffer)
              :to-match
              "P234…"))

  (it "only shows ellipses if width is 1"
      (ogt-capture-and-process-project "P234567890")
      (let ((org-gtd-engage-prefix-width 1))
        (org-gtd-engage))
      (expect (ogt--buffer-string org-agenda-buffer)
              :to-match
              "^  …"))

  (it "shows the full project name without ellipses if it can"
      (ogt-capture-and-process-project "P234567890")
      (let ((org-gtd-engage-prefix-width 10))
        (org-gtd-engage))
      (org-gtd-engage)
      (expect (ogt--buffer-string org-agenda-buffer)
              :to-match
              "P234567890"))

  (it "adds as many spaces as needed"
      (ogt-capture-and-process-project "P234567890")
      (let ((org-gtd-engage-prefix-width 50))
        (org-gtd-engage))
      (expect (ogt--buffer-string org-agenda-buffer)
              :to-match
              "P234567890                                        "))))
