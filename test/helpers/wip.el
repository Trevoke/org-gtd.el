;;; wip.el --- WIP buffer test helpers -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Helper functions for accessing WIP (Work In Progress) buffers in tests.
;; These replace the verbose pattern that was duplicated 71+ times:
;;
;;   (let ((wip-buffers (seq-filter (lambda (buf)
;;                                    (string-search org-gtd-wip--prefix (buffer-name buf)))
;;                                  (buffer-list))))
;;     (when wip-buffers
;;       (with-current-buffer (car wip-buffers)
;;         ...)))
;;
;; With the cleaner:
;;
;;   (with-wip-buffer
;;     ...)
;;

;;; Code:

(require 'org-gtd-wip)

(defun ogt-get-wip-buffer ()
  "Return the current WIP buffer, or nil if none exists."
  (car (seq-filter (lambda (buf)
                     (string-search org-gtd-wip--prefix (buffer-name buf)))
                   (buffer-list))))

(defmacro with-wip-buffer (&rest body)
  "Execute BODY in the current WIP buffer if one exists.
Returns the value of the last form in BODY, or nil if no WIP buffer exists."
  (declare (indent 0) (debug t))
  `(when-let ((wip-buf (ogt-get-wip-buffer)))
     (with-current-buffer wip-buf
       ,@body)))

(provide 'org-gtd-test-helper-wip)

;;; wip.el ends here
