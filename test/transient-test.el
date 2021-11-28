;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'transient)
(require 'buttercup)
(require 'with-simulated-input)

(defun ogt-print-a () (interactive) (insert "a"))
(defun ogt-print-b () (interactive) (insert "b"))

(transient-define-prefix ogt-transient ()
  ["X"
   ("f" "test f" ogt-print-a)
   ("g" "test g" ogt-print-b)])

(describe
 "Transient"
 (it "takes keys"
     (let ((buffer (find-file-noselect (make-temp-file "ogt"))))
       (set-buffer buffer)
       (ogt-transient)
       (setq unread-command-events (listify-key-sequence "f"))
       ;(self-insert-command ?f)
       (expect (buffer-string)
               :to-match "a"))
     ;; (with-temp-buffer
     ;;   ;(ogt-transient)
     ;;   (with-simulated-input "f"
     ;;     (ogt-transient)
     ;;     (expect (buffer-string)
     ;;             :to-match "a")))
     ))
