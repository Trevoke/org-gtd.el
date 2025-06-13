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

(org-ql-defpred property-ts< (property greater-ts)
  "Checks whether a timestamp is earlier than a given date."
  :body (when-let ((ts-value (org-entry-get nil property)))
          (ts< (ts-parse-org ts-value) (ts-parse greater-ts))))

(org-ql-defpred property-ts= (property other-ts)
  "Checks whether a timestamp is earlier than a given date."
  :body (when-let ((ts-value (org-entry-get nil property))
                   (other-ts-start (ts-parse-fill 'begin other-ts))
                   (other-ts-end (ts-parse-fill 'end other-ts)))
          (ts-in other-ts-start other-ts-end (ts-parse-org ts-value))))

(defun org-gtd-generate-org-ql-block (view-type alist)
  "Generate an org-ql block based on VIEW-TYPE and ALIST."
  (let* ((view (cdr (assoc view-type (alist-get 'views (cdr alist)))))
         (filters (mapcan (lambda (prop)
                            (let ((key (car prop))
                                  (value (cdr prop)))
                              (cond
                               ((eq key 'todo) (list `(todo ,value)))
                               ((eq value :today) (list `(property-ts= ,(symbol-name key) ,(format-time-string "%Y-%m-%d"))))
                               (t (list `(property ,(symbol-name key) ,value))))
                              ))
                          view)))
    `(org-ql-block '(and ,@filters))))

(defun org-gtd-generate-org-ql-block (section config)
  "Generate an org-ql block based on VIEW-TYPE and ALIST."
  (let* ((conses (alist-get section config))
         (filters (mapcan
                   (lambda (prop)
                     (pcase-let ((`(,key . ,value) prop))
                       (cond
                        ((eq key 'todo) (list `(todo ,value)))
                        ((eq value :today) (list `(property-ts= ,(symbol-name key) ,(format-time-string "%Y-%m-%d"))))
                        (t (list `(property ,(symbol-name key) ,value))))))
                   conses)))
    `(org-ql-block '(and ,@filters))))

(provide 'org-gtd-ql)
;;; org-gtd.el ends here
