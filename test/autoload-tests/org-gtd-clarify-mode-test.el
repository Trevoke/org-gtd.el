(require 'buttercup)
(setq org-gtd-update-ack "3.0.0")
(load "org-gtd-autoloads")

(describe
 "autoload management (recursive)"

 (it "org-gtd-clarify-mode"
     (org-gtd-clarify-mode)))
