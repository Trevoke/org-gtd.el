;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
    "Processing items"

  (before-all (ogt--configure-emacs))

  (before-each (ogt--prepare-filesystem)
               (ogt--add-single-item))
  (after-each (ogt--close-and-delete-files))

  (it "processes all the elements"
    (dotimes (x 8)
      (ogt--add-single-item (format "single action %s" x)))

    (org-gtd-process-inbox)

    (execute-kbd-macro (kbd "M-> RET"))
    (insert ogt--project-text)
    (execute-kbd-macro (kbd "C-c c p TAB RET"))

    (execute-kbd-macro (kbd "C-c c c RET TAB RET"))

    (execute-kbd-macro (kbd "C-c c d RET Someone RET TAB RET"))

    (execute-kbd-macro (kbd "C-c c i RET TAB RET"))

    (execute-kbd-macro (kbd "C-c c s TAB RET"))

    (execute-kbd-macro (kbd "C-c c s TAB RET"))

    (execute-kbd-macro (kbd "M-> RET"))
    (insert ogt--project-text)
    (execute-kbd-macro (kbd "C-c c p TAB RET"))

    (execute-kbd-macro (kbd "C-c c c RET TAB RET"))

    (dotimes (x 8)
      (ogt--add-single-item (format "single action %s" x)))

    (org-gtd-process-inbox)

    (execute-kbd-macro (kbd "C-c c i RET TAB RET"))

    (execute-kbd-macro (kbd "M-> RET"))
    (insert ogt--project-text)
    (execute-kbd-macro (kbd "C-c c p TAB RET"))

    (execute-kbd-macro (kbd "C-c c s TAB RET"))

    (execute-kbd-macro (kbd "C-c c s TAB RET"))

    (execute-kbd-macro (kbd "C-c c c RET TAB RET"))

    (execute-kbd-macro (kbd "C-c c d RET Someone RET TAB RET"))

    (execute-kbd-macro (kbd "C-c c i RET TAB RET"))

    (execute-kbd-macro (kbd "C-c c s TAB RET"))

    (execute-kbd-macro (kbd "C-c c s TAB RET"))

    (with-current-buffer (org-gtd--inbox-file)
      (expect (buffer-string)
              :not :to-match
              "single action")))

  (it "uses configurable decorations on the processed items"
    (let ((org-gtd-process-item-hooks '(org-set-tags-command org-priority)))
      (org-gtd-process-inbox)
      (execute-kbd-macro (kbd "C-c c s RET A TAB RET")))

    (org-gtd-show-all-next)
    (let ((ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer)))
      (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string)
              :to-be-truthy)))

  (it "shows item in agenda when done"
    (org-gtd-process-inbox)
    (execute-kbd-macro (kbd "C-c c s TAB RET"))
    (expect (buffer-modified-p (org-gtd--default-action-file)) :to-equal t)

    (org-gtd-show-all-next)

    (let ((ogt-agenda-string (ogt--get-string-from-buffer ogt--agenda-buffer)))
      (expect (string-match "single action" ogt-agenda-string)
              :to-be-truthy))))
