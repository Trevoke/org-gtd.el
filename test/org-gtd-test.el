;; -*- lexical-binding: t; -*-

(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)
(require 'f)

(defun ogt--clean-target-directory (dir)
  (delete-directory dir t nil)
  (make-directory dir))

(defun ogt--get-string-from-buffer (buffer)
  "Return buffer's content."
  (with-current-buffer buffer
    (buffer-string)))

(describe "Org GTD"
  :var ((ogt-target-dir (f-join (f-dirname (f-this-file)) "runtime-file-path")))
  (before-each
    (setq org-gtd-directory ogt-target-dir)
    (ogt--clean-target-directory org-gtd-directory)
    (setq org-agenda-files `(,ogt-target-dir)))

  (after-each
    (mapcar (lambda (buffer) (with-current-buffer buffer (save-buffer) (kill-buffer)))
            (org-gtd--find-or-create-and-save-files))
    (setq org-agenda-files '()))

 (describe
     "Processing items"

   (before-each
     (define-key org-gtd-command-map (kbd "C-c c") #'org-gtd-clarify-finalize)
     (setq org-capture-templates `(("i" "GTD item"
                                    entry (file (lambda () (org-gtd--path org-gtd-inbox-file-basename)))
                                    "* %?\n%U\n\n  %i"
                                    :kill-buffer t))))

   (it "shows item in agenda when done"
     (org-gtd-capture nil "i")
     (insert "single-action")
     (org-capture-finalize)

     (with-simulated-input '("s" "C-c c" "RET")
                           (org-gtd-process-inbox))

     (expect (with-current-buffer (org-gtd--actionable-file) (buffer-modified-p)) :to-equal nil)

     (org-gtd-show-all-next)

     (setq ogt-agenda-string (with-current-buffer "*Org Agenda*" (buffer-string)))


     (expect (string-match "single-action" ogt-agenda-string) :to-be-truthy))

   (it "uses a keybinding to finish clarifying an item when point is on headline"
     (org-gtd-capture nil "i")
     (insert "foobar")
     (org-capture-finalize)

     (with-simulated-input '("s" "C-c c" "RET")
                           (org-gtd-process-inbox))
     (mapcar
      (lambda (buffer) (with-current-buffer buffer (save-buffer)))
      `(,(org-gtd--actionable-file) ,(org-gtd--incubate-file) ,(org-gtd--inbox-file)))

     (expect (ogt--get-string-from-buffer (org-gtd--actionable-file)) :to-match "foobar")))


 (describe "Finding a refile target"
   (before-each
     (org-gtd--find-or-create-and-save-files))

   (it "finds the Actions heading in the actionable file"
     (expect (car (org-gtd--refile-target org-gtd-actions)) :to-equal "Actions"))

   (it "finds the Incubate headings in the incubate file"
     (with-current-buffer (org-gtd--incubate-file)
       (goto-char (point-max))
       (insert "* To Read\n* To Eat\n")
       (save-buffer))
     (setq org-refile-targets (org-gtd--refile-incubate-targets))
     (setq target-names (mapcar 'car (org-refile-get-targets)))
     (expect target-names :to-have-same-items-as '("To Eat" "To Read")))))
