;; -*- lexical-binding: t; -*-

(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)
(require 'f)

(defun ogt--clean-target-directory (dir)
  (delete-directory dir t nil)
  (make-directory dir))

(describe
 "Processing items"
 :var ((ogt-target-dir (f-join (f-dirname (f-this-file)) "runtime-file-path")))

 (before-each
  (setq org-gtd-directory ogt-target-dir)
  (ogt--clean-target-directory org-gtd-directory)
  (define-key org-gtd-command-map (kbd "C-c c") #'org-gtd-clarify-finalize)
  (setq org-capture-templates `(("i" "GTD item"
                                 entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                                 "* %?\n%U\n\n  %i"
                                 :kill-buffer t))))

 (it "uses a keybinding to finish clarifying an item when point is on headline"
     (with-simulated-input '("i" "foobar" "C-c C-c" "i")
                           (org-gtd-capture))
     (with-simulated-input '("t" "C-c c" "RET")
                           (org-gtd-process-inbox))))
