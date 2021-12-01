;;; org-gtd-refile.el --- refiling logic for org gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Refiling logic for org-gtd.
;;
;;; Code:

(defconst org-gtd-refile--prompt
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions "Refile single action to: " myhash)
    (puthash org-gtd-incubated "Refile incubated item to: " myhash)
    (puthash org-gtd-delegated "Refile delegated item to: " myhash)
    (puthash org-gtd-projects "Refile project to: " myhash)
    (puthash org-gtd-calendar "Refile calendar item to: " myhash)
    myhash))

(defun org-gtd-refile--do (type)
  "Refile an item to the single action file."
  (with-org-gtd-refile type
    (unless (org-refile-get-targets) (org-gtd--gtd-file-buffer type))
    (if org-gtd-refile-to-any-target
        (org-refile nil nil (car (org-refile-get-targets)))
      (org-refile nil nil nil (org-gtd-refile--prompt type)))))

(defmacro with-org-gtd-refile (type &rest body)
  (declare (debug t) (indent 1))
  `(let ((org-refile-target-verify-function (lambda () (org-gtd-refile--group-p ,type)))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (with-org-gtd-context (progn ,@body)))))

(defun org-gtd-refile--group-p (type)
  (string-equal (org-gtd-refile--group type)
                (org-element-property :ORG_GTD (org-element-at-point))))

(defun org-gtd-refile--group (type)
  (gethash type org-gtd--properties))

(defun org-gtd-refile--prompt (type)
  (gethash type org-gtd-refile--prompt))

(provide 'org-gtd-refile)
;;; org-gtd-refile.el ends here
