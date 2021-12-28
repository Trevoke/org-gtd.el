;;; org-gtd-capture.el --- capturing items to the inbox -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2021 Aldric Giacomoni

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
;;
;; capturing items to the inbox for org-gtd.
;;
;;; Code:

(require 'org-capture)
(require 'org-gtd-core)

;; move this here to make a clear load path to make straight.el happy
;; it was originally in org-gtd-capture.el
(defun org-gtd--capture-templates ()
  "Private function.

Return valid `org-capture' templates based on `org-gtd-capture-templates'."
  (mapcar #'org-gtd--gen-capture-templates
          org-gtd-capture-templates))

;; move this here to make a clear load path to make straight.el happy
;; it was originally in org-gtd-capture.el
(defun org-gtd--gen-capture-templates (template)
  "Private function.

Given an `org-capture-template' TEMPLATE string, generate a valid
org-gtd-capture item."
  (cl-destructuring-bind (key description template-string) template
    `(,key ,description entry
           (file (lambda () (org-gtd-inbox-path)))
                 ,template-string :kill-buffer t)))

(provide 'org-gtd-capture)
;;; org-gtd-capture.el ends here
