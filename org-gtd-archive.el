;;; org-gtd-archive --- Logic to archive tasks -*- lexical-binding: t; coding: utf-8 -*-
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
;; Archiving logic for org-gtd
;;
;;; Code:

(require 'org-archive)

(defconst org-gtd-item-match "+LEVEL=1&+ORG_GTD=\"%s\"")

;;;###autoload
(defun org-gtd-archive-completed-items ()
  (interactive)
  (with-org-gtd-context
   (mapcar 'org-gtd--archive-done `(,org-gtd-actions
                                    ,org-gtd-incubated
                                    ,org-gtd-calendar
                                    ,org-gtd-projects))))

(defun org-gtd--archive-done (subset)
  (org-map-entries
   (lambda () (org-gtd--archive-all-done))
   (format org-gtd-item-match (gethash subset org-gtd--properties))
   'agenda))

(defun org-gtd--archive-all-done (&optional tag)
  "Archive sublevels of the current tree without open TODO items.
If the cursor is not on a headline, try all level 1 trees.  If
it is on a headline, try all direct children.
When TAG is non-nil, don't move trees, but mark them with the ARCHIVE tag."
  (org-gtd--archive-all-matches-no-confirm
   (lambda (_beg end)
     (let ((case-fold-search nil))
       (unless (re-search-forward org-not-done-heading-regexp end t)
         "no open TODO items")))
   tag))

(defun org-gtd--archive-all-matches-no-confirm (predicate &optional tag)
  "Archive sublevels of the current tree that match PREDICATE.

PREDICATE is a function of two arguments, BEG and END, which
specify the beginning and end of the headline being considered.
It is called with point positioned at BEG.  The headline will be
archived if PREDICATE returns non-nil.  If the return value of
PREDICATE is a string, it should describe the reason for
archiving the heading.

If the cursor is not on a headline, try all level 1 trees.  If it
is on a headline, try all direct children.  When TAG is non-nil,
don't move trees, but mark them with the ARCHIVE tag.

This function was shamelessly copied-and-modified from org-archive.
"
  (let ((rea (concat ".*:" org-archive-tag ":")) re1
        (begm (make-marker))
        (endm (make-marker))
        reason beg end (cntarch 0))
    (if (org-at-heading-p)
        (progn
          (setq re1 (concat "^" (regexp-quote
                                 (make-string
                                  (+ (- (match-end 0) (match-beginning 0) 1)
                                     (if org-odd-levels-only 2 1))
                                  ?*))
                            " "))
          (move-marker begm (point))
          (move-marker endm (org-end-of-subtree t)))
      (setq re1 "^* ")
      (move-marker begm (point-min))
      (move-marker endm (point-max)))
    (save-excursion
      (goto-char begm)
      (while (re-search-forward re1 endm t)
        (setq beg (match-beginning 0)
              end (save-excursion (org-end-of-subtree t) (point)))
        (goto-char beg)
        (if (not (setq reason (funcall predicate beg end)))
            (goto-char end)
          (goto-char beg)
          (if (or (not tag) (not (looking-at rea)))
              (progn
                (if tag
                    (org-toggle-tag org-archive-tag 'on)
                  (org-archive-subtree))
                (setq cntarch (1+ cntarch)))
            (goto-char end)))))
    (message "%d trees archived" cntarch)))

(provide 'org-gtd-archive)
;;; org-gtd-archive ends here
