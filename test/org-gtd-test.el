;; -*- lexical-binding: t; -*-

(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)
(require 'f)

(defun ogt--clean-target-directory (dir)
  (delete-directory dir t nil)
  (make-directory dir))

(describe "Processing items"
          :var ((ogt-target-dir (f-join (f-dirname (f-this-file)) "runtime-file-path")))

          (before-each
           (setq org-gtd-directory ogt-target-dir)
           (ogt--clean-target-directory org-gtd-directory)
           (define-key org-gtd-command-map (kbd "C-c c") #'org-gtd-clarify-finalize)
           (setq org-capture-templates `(("i" "GTD item"
                                          entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                                          "* %?\n%U\n\n  %i"
                                          :kill-buffer t))))

          (it "can use a keybinding to finish clarifying an item"
              (with-simulated-input
                  '("C-c C-c")
                (org-gtd-capture 4 "i")
                (insert "foobar"))
              (org-gtd-process-inbox)
              (with-simulated-input "t" ())
              ;; (self-insert-command "t")
              ;; (execute-kbd-macro (kbd "T"))
              (execute-kbd-macro "C-c c")
              ))
