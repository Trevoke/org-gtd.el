;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(defun ogt-manage-active-minor-modes ()
  "Get a list of which minor modes are enabled in the current buffer. Taken from
https://emacs.stackexchange.com/a/62414/61
because I need to run this on older emacsen than 28.1 which has
`local-minor-modes'."
  (let ($list)
    (mapc (lambda ($mode)
            (condition-case nil
                (if (and (symbolp $mode) (symbol-value $mode))
                    (setq $list (cons $mode $list)))
              (error nil)))
          minor-mode-list)
    (sort $list 'string<)))

(describe
 "WIP state for tasks"


 (before-each (setq inhibit-message t)
  (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "holds the subtree for the task we want to clarify"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
       (with-current-buffer source-buffer
         (org-gtd-clarify-item))

       (expect (ogt--buffer-string (car (org-gtd-wip--get-buffers)))
               :to-match
               "This is the heading to clarify")))

 (it "has the org-gtd-processing mode"
     (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* This is the heading to clarify")))
       (with-current-buffer source-buffer
         (org-gtd-clarify-item))

       (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
         (with-current-buffer wip-buffer
           (expect (ogt-manage-active-minor-modes) :to-contain 'org-gtd-clarify-mode))))))
