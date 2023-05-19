;;; org-gtd-backward-compatibility.el --- Functions added in later versions of emacs -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

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
;; Functions that don't exist in older vanilla emacsen
;;
;;; Code:

;;;; Requirements

(require 'subr-x)

;;;; Functions

;;;;; Public

;; this was added in emacs 28.1
(unless (fboundp 'ensure-list)
  (defalias 'ensure-list 'org-gtd--ensure-list))

;; this was added in emacs 28.1
(unless (fboundp 'string-pad)
  (defalias 'string-pad 'org-gtd--string-pad))

;;;;; Private

(defun org-gtd--ensure-list (object)
  "Return OBJECT as a list.
If OBJECT is already a list, return OBJECT itself.  If it's
not a list, return a one-element list containing OBJECT."
  (if (listp object)
      object
    (list object)))

(defun org-gtd--string-pad (string length &optional padding start)
  "Pad STRING to LENGTH using PADDING.
If PADDING is nil, the space character is used.  If not nil, it
should be a character.

If STRING is longer than the absolute value of LENGTH, no padding
is done.

If START is nil (or not present), the padding is done to the end
of the string, and if non-nil, padding is done to the start of
the string."
  (unless (natnump length)
    (signal 'wrong-type-argument (list 'natnump length)))
  (let ((pad-length (- length (length string))))
    (cond ((<= pad-length 0) string)
          (start (concat (make-string pad-length (or padding ?\s)) string))
          (t (concat string (make-string pad-length (or padding ?\s)))))))

;;;; Footer

(provide 'org-gtd-backward-compatibility)

;;; org-gtd-backward-compatibility.el ends here
