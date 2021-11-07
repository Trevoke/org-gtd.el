;; -*- lexical-binding: t; -*-
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

(defconst org-gtd--refile-prompt
  (let ((myhash (make-hash-table :test 'equal)))
    (puthash org-gtd-actions "Refile single action to: " myhash)
    (puthash org-gtd-incubated "Refile incubated item to: " myhash)
    (puthash org-gtd-delegated "Refile delegated item to: " myhash)
    (puthash org-gtd-projects "Refile project to: " myhash)
    (puthash org-gtd-scheduled "Refile scheduled item to: " myhash)
    myhash))

(defmacro with-org-gtd-refile (type &rest body)
  (declare (debug t))
  `(let ((org-refile-target-verify-function (lambda () (org-gtd--group-p ,type)))
         (org-refile-targets '((org-agenda-files :level . 1))))
     (unwind-protect
         (progn ,@body))))

(defun org-gtd--refile (type)
  "Refile an item to the single action file."
  (with-org-gtd-context
   (with-org-gtd-refile
    type
    (unless (org-refile-get-targets) (org-gtd--gtd-file-buffer type))
    (org-refile nil nil nil (org-gtd--refile-prompt type)))))

(defun org-gtd--group-p (type)
  (string-equal (org-gtd--group type)
                (org-element-property :ORG_GTD (org-element-at-point))))

(defun org-gtd--group (type)
  (gethash type org-gtd--refile-properties))

(defun org-gtd--refile-prompt (type)
  (gethash type org-gtd--refile-prompt))

(provide 'org-gtd-refile)
