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
(require 'org-gtd-types)
(require 'org-gtd-core)

(defun org-gtd-prompt-for-active-date (prompt)
  "Prompt the user for a date and return it formatted as an active timestamp."
  (interactive)
  (let ((date (org-read-date nil nil nil (format "%s > " prompt))))
    (format "<%s>" date)))

(defun org-gtd-prompt-for-active-date-with-repeater (prompt)
  "Prompt for an active date with optional repeater using PROMPT."
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

;;;; Type-based configuration

(defun org-gtd-configure-as-type (type-name &optional values)
  "Configure item at point as TYPE-NAME using the GTD type system.

TYPE-NAME must be a symbol from `org-gtd-types' (e.g., \\='next-action,
\\='delegated, \\='calendar, \\='project).  This function:

1. Sets the ORG_GTD property to the type's defined value
2. Sets the TODO state based on the type's :state semantic
3. Sets all required semantic properties (prompted or from VALUES)

VALUES is an optional alist mapping semantic property keywords to values,
for non-interactive use.  Example: \\='((:who . \"John\") (:when . \"<2025-01-15>\"))

When VALUES is provided, properties are set directly without prompting.
When VALUES is nil, required properties are prompted interactively.
Properties with :default values are set automatically without prompting."
  (let ((type-def (org-gtd-type-get type-name)))
    (unless type-def
      (user-error "Unknown GTD type: %s" type-name))
    (let ((org-gtd-val (plist-get (cdr type-def) :org-gtd))
          (state (plist-get (cdr type-def) :state))
          (props (plist-get (cdr type-def) :properties)))
      ;; Set ORG_GTD property
      (org-entry-put nil "ORG_GTD" org-gtd-val)
      ;; Set TODO state if defined
      (when state
        (org-todo (org-gtd--state-to-keyword state)))
      ;; Set each required property (from VALUES, default, input-fn, or by prompting)
      (dolist (prop props)
        (when (plist-get (cdr prop) :required)
          (let* ((semantic-name (car prop))
                 (prompt (plist-get (cdr prop) :prompt))
                 (prop-type (plist-get (cdr prop) :type))
                 (org-prop (plist-get (cdr prop) :org-property))
                 (default-val (plist-get (cdr prop) :default))
                 (input-fn (plist-get (cdr prop) :input-fn))
                 ;; Look up value: VALUES > default > input-fn > default prompt
                 (value (or (alist-get semantic-name values)
                            default-val
                            (if input-fn
                                (funcall input-fn prompt)
                              (org-gtd--prompt-for-property-type prop-type prompt)))))
            (if (string-equal org-prop "SCHEDULED")
                (org-schedule nil value)
              (org-entry-put nil org-prop value)))))
      ;; Ensure ID exists
      (org-gtd-id-get-create))))

(defun org-gtd--state-to-keyword (state)
  "Convert STATE semantic keyword to actual org TODO keyword.
STATE is one of :next, :wait, :done, or :canceled."
  (pcase state
    (:next (org-gtd-keywords--next))
    (:wait (org-gtd-keywords--wait))
    (:done (org-gtd-keywords--done))
    (:canceled (org-gtd-keywords--canceled))
    (_ nil)))

(defun org-gtd--prompt-for-property-type (prop-type prompt)
  "Prompt for a value of PROP-TYPE using PROMPT.
PROP-TYPE is one of: text, timestamp, repeating-timestamp."
  (pcase prop-type
    ('text (read-string (format "%s " prompt)))
    ('timestamp (org-gtd-prompt-for-active-date prompt))
    ('repeating-timestamp (org-gtd-prompt-for-active-date-with-repeater prompt))
    (_ (read-string (format "%s " prompt)))))

(provide 'org-gtd-configure)
;;; org-gtd-configure.el ends here
