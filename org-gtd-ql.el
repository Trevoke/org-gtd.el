;;; org-gtd-ql.el --- Adapter layer for org-ql -*- lexical-binding: t; coding: utf-8 -*-

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

;; Org-ql is a fantastic DSL on top of org-mode, which simplifies
;; tremendously the querying of various items.
;; Org-GTD uses org-ql to facilitate customization of views.
;;
;;; Code:
(require 'org-ql)
(require 'ts)

(org-ql-defpred property-ts< (property greater-ts)
  "Checks whether a timestamp is earlier than a given date."
  :body (when-let ((ts-value (org-entry-get nil property)))
          (let ((target-ts (if (string-equal greater-ts "today")
                               (ts-now)
                             (ts-parse greater-ts))))
            (ts< (ts-parse-org ts-value) target-ts))))

(org-ql-defpred property-ts> (property lesser-ts)
  "Checks whether a timestamp is later than a given date."
  :body (when-let ((ts-value (org-entry-get nil property)))
          (let ((target-ts (if (string-equal lesser-ts "today")
                               (ts-now)
                             (ts-parse lesser-ts))))
            (ts> (ts-parse-org ts-value) target-ts))))

(org-ql-defpred property-ts= (property other-ts)
  "Checks whether a timestamp is earlier than a given date."
  :body (when-let ((ts-value (org-entry-get nil property))
                   (other-ts-start (ts-parse-fill 'begin other-ts))
                   (other-ts-end (ts-parse-fill 'end other-ts)))
          (ts-in other-ts-start other-ts-end (ts-parse-org ts-value))))

(org-ql-defpred property-invalid-timestamp (property)
  "True if PROPERTY doesn't exist, is empty, or isn't a valid org timestamp.

A property is considered invalid if:
- It doesn't exist on the entry
- Its value is empty or whitespace-only
- Its value doesn't match org-mode's timestamp format

This is useful for finding \\='stuck\\=' items in GTD workflows where timestamps
are required but missing or malformed."
  :body (let ((prop-value (org-entry-get nil property)))
          (or (not prop-value)
              (string-empty-p (string-trim prop-value))
              (not (org-string-match-p org-ts-regexp-both prop-value)))))

(org-ql-defpred property-empty-or-missing (property)
  "True if PROPERTY doesn't exist or is empty/whitespace-only.

This is useful for finding items missing required metadata, such as
delegated items without a \\='who\\=' field."
  :body (let ((prop-value (org-entry-get nil property)))
          (or (not prop-value)
              (string-empty-p (string-trim prop-value)))))

(defun org-gtd-generate-org-ql-block (view-type alist)
  "Generate an org-ql block based on VIEW-TYPE and ALIST."
  (let* ((view (cdr (assoc view-type (alist-get 'views (cdr alist)))))
         (filters (mapcan (lambda (prop)
                            (let ((key (car prop))
                                  (value (cdr prop)))
                              (cond
                               ((eq key 'todo) (list `(todo ,value)))
                               ((eq value :today) (list `(property-ts= ,(symbol-name key) ,(format-time-string "%Y-%m-%d"))))
                               (t (list `(property ,(symbol-name key) ,value))))))
                          view)))
    `(org-ql-block '(and ,@filters))))


(provide 'org-gtd-ql)
;;; org-gtd.el ends here
