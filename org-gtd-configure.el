;;; org-gtd-configure.el --- Configure org headings -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package tries to replicate as closely as possible the GTD workflow.
;; This package assumes familiarity with GTD.
;;
;; This package provides a system that allows you to capture incoming things
;; into an inbox, then process the inbox and categorize each item based on the
;; GTD categories.  It leverages org-agenda to show today's items as well as the
;; NEXT items.  It also has a simple project management system, which currently
;; assumes all tasks in a project are sequential.
;;
;; For a comprehensive instruction manual, see the documentation.
;; Either the info file or in the doc/ directory of the repository.
;; Upgrade information is also available therein.
;;
;;; Code:
(require 'org-gtd-id)
(require 'org-gtd-ql)

;;;; Forward declarations
(defvar org-gtd-user-item-config)
(defvar org-gtd-default-input-config)

(declare-function org-gtd-items "org-gtd")

(defun org-gtd-prompt-for-active-date (prompt)
  "Prompt the user for a date and return it formatted as an active timestamp."
  (interactive)
  (let ((date (org-read-date nil nil nil (format "%s > " prompt))))
    (format "<%s>" date)))

(defun org-gtd-prompt-for-active-date-with-repeater (prompt)
  (interactive)
  (let ((input (read-from-minibuffer (format "%s > " prompt))))
    ;; Check if input contains a repeater pattern (starts with + or .)
    (if (string-match "^[.+]" input)
        ;; Input is just a repeater, use today's date
        (format "<%s %s>" (format-time-string "%Y-%m-%d") input)
      ;; Input might be a date specification, use org-read-date
      (let ((date (org-read-date nil nil input)))
        (if (string-match "\\([.+][+.][0-9]+[hdwmy]\\)" input)
            ;; Date with repeater in input
            (format "<%s %s>" date (match-string 1 input))
          ;; Just date, ask for repeater
          (let ((repeater (read-from-minibuffer "How do you want this to repeat? ")))
            (format "<%s %s>" date repeater)))))))

(defun org-gtd-configure-item (pos item-type &optional user-config input-config)
  "Configure item at POS according to ITEM-TYPE."
  (let* ((items (org-gtd-items))
         (item-data (alist-get item-type items))
         (item-config item-data)
         (input-config (or input-config '()))
         (user-config (or user-config org-gtd-user-item-config))
         (user-item-config (when user-config (alist-get item-type user-config)))
         (config (if user-item-config
                     (map-merge 'alist item-config user-item-config)
                   item-config)))
    (when config
      (dolist (definition config)
        (pcase-let ((`(,key . ,value) definition))
          (let* ((key (symbol-name key))
                 (value (if (and (consp value)
                                 (not (functionp value)))
                            (org-gtd--build-prompt-form value input-config)
                          value))
                 (computed-value (pcase value
                                   ((pred functionp) (funcall value))
                                   ((and (pred symbolp)
                                         (pred fboundp))
                                    (funcall value))
                                   ((and (pred symbolp)
                                         (pred boundp))
                                    (symbol-value value))
                                   ((pred listp) (eval value))
                                   (_ value))))

            ;; Special handling for TODO keyword vs properties
            (if (string-equal key org-gtd-prop-todo)
                (org-with-point-at pos (org-todo computed-value))
              (org-entry-put pos (upcase key) (format "%s" computed-value)))))))))

(defun org-gtd--merge-inputs (&optional user-config)
  (let ((user-config (or user-config '())))
    (map-merge 'alist
               org-gtd-default-input-config
               user-config
               '((_ . (lambda (x) (read-string (format "%s: " x))))))))

(defun org-gtd--pcase-inputs (&optional user-config)
  (mapcar (lambda (x) `(,(car x) ,(cdr x)))
          (org-gtd--merge-inputs user-config)))

(defconst org-gtd-default-input-config
  '(('active-timestamp . (lambda (x) (org-gtd-prompt-for-active-date x)))
    ('active-timestamp-with-repeater . (lambda (x) (org-gtd-prompt-for-active-date-with-repeater x)))
    ('text . (lambda (x) (read-string (format "%s: " x))))))

(defun org-gtd--build-prompt-form (value user-entry-config)
  "Build input prompt function from VALUE config and USER-ENTRY-CONFIG overrides.

This function avoids using `eval' for code clarity and robustness.
Instead of returning a quoted form with a closure embedded, we return an
actual function that will do the lookup and call at runtime."
  (let-alist value
    (let ((prompt .prompt)
          (type .type))
      ;; Return a lambda that will look up and call the input function
      ;; This works on all Emacs versions because the closure is called directly,
      ;; not embedded in a form to be evaluated later
      (lambda ()
        (let* ((input-funcs (org-gtd--merge-inputs user-entry-config))
               (input-func (or (cdr (assoc type input-funcs))
                              (cdr (assoc '_ input-funcs)))))
          (funcall input-func prompt))))))

(provide 'org-gtd-configure)
;;; org-gtd-configure.el ends here
