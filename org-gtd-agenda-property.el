;;; org-gtd-agenda-property.el --- Display org properties in the agenda buffer.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/org-gtd-agenda-property
;; Version: 1.3.2
;; Package-Requires: ((emacs "24.2"))
;; Keywords: calendar
;; Separator: -
;; ShortName: org-gtd-agenda-property

;;; Commentary:

;; org-gtd-agenda-property is a package which displays the properties of
;; an org item beside (or below) the item's title in the agenda
;; buffer. Customize the variable `org-gtd-agenda-property-list' to add
;; which properties you which to show.

;;; Instructions:

;; Variables
;;
;;      All variables can be edited by running
;;      `org-gtd-agenda-property-customize' (seriously, chech it out, they
;;      are just a few :-)). The documentations are mostly self
;;      explanatory, I list here only the most important two.

;;	`org-gtd-agenda-property-list'
;;              This should be a list of all properties you want
;;              displayed in the buffer. Default is "LOCATION".

;;      `org-gtd-agenda-property-position'
;;              This is where you want the properties to be displayed
;;              (besides the title or below the title?).

;;; License:

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Change Log:
;; 1.3.1 - 20130707 - Fixed some mentions to the wrong package.
;; 1.3 - 20130522 - Fixed bug.
;; 1.2 - 20130521 - Renamed function. More robust hook.
;; 1.1 - 20130521 - Fixed some Warnings.
;; 1.1 - 20130521 - Added requirements.
;; 1 - 20130521 - Released.

;;; Code:

(require 'org-agenda)

(defconst org-gtd-agenda-property-version "1.3.2"
  "Version string of the `org-gtd-agenda-property' package.")
(defconst org-gtd-agenda-property-version-int 6
  "Integer version number of the `org-gtd-agenda-property' package (for comparing versions).")

(defun org-gtd-agenda-property-bug-report ()
  "Opens github issues page in a web browser.
Please send me any bugs you find, and please inclue your emacs and your package versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/org-gtd-agenda-property/issues/new")
  (message "Your org-gtd-agenda-property-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           org-gtd-agenda-property-version emacs-version))

(defun org-gtd-agenda-property-customize ()
  "Open the customization menu the `org-gtd-agenda-property' group."
  (interactive)
  (customize-group 'org-gtd-agenda-property t))

(defcustom org-gtd-agenda-property-list '("LOCATION")
  "List of properties to be displayed in the agenda buffer."
  :type '(list string)
   :group 'org-gtd-agenda-property)

(defcustom org-gtd-agenda-property-separator "|"
  "The separator used when several properties are found."
  :type 'string
   :group 'org-gtd-agenda-property)

(defcustom org-gtd-agenda-property-column 60
  "Minimum column in which to insert in-line locations in agenda view."
  :type 'integer
   :group 'org-mode-property)

(defcustom org-gtd-agenda-property-position 'where-it-fits
  "Where the properties will be placed in the agenda buffer.

'same-line means in the same line as the item it belongs to,
starting at `org-gtd-agenda-property-column'. 'next-line means on the
next-line. 'where-it-fits means 'same-line if it fits in the
window, otherwise 'next-line."
  :type 'symbol
   :group 'org-gtd-agenda-property)

(defface org-gtd-agenda-property-face
  '((t :inherit font-lock-comment-face ))
  "Face used for the properties string."
  :group 'org-gtd-agenda-property)

;;;###autoload
(defun org-gtd-agenda-property-add-properties ()
  "Append locations to agenda view.
Uses `org-agenda-locations-column'."
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line 1)
    ;; Only process lines with org-marker that are NOT property-display lines
    (when (and (org-get-at-bol 'org-marker)
               (not (org-get-at-bol 'org-gtd-property-line)))
      ;; Move past the file name.
      (search-forward-regexp " +" (line-end-position) t 2)
      ;; Move to the title.
      (if (looking-at "\\(.[0-9]:[0-9][0-9][^ ][^ ][^ ][^ ][^ ][^ ]\\|Sched\.[0-9]+x:\\)")
          (search-forward-regexp " +" (line-end-position) t 1)
        (if (looking-at "In *[0-9]+ *[a-z]\.:")
            (search-forward-regexp " +" (line-end-position) t 3)))
      ;; Get properties and insert.
      (let* ((this-marker (org-get-at-bol 'org-marker))
             (loc (org-gtd-agenda-property-create-string this-marker))
             (col (+ (current-column) (if (looking-at "Scheduled:") 11 -1)))
             (prop (org-gtd-agenda-property--prepare-props (text-properties-at (point)) this-marker))
             indentedLocation)
        ;; If this item doesn't containi any of the properties, loc will be nil.
        (when loc
          (end-of-line)
          ;; Decide where to put the properties string.
          (if (or (eq org-gtd-agenda-property-position 'next-line)
                  (and (eq org-gtd-agenda-property-position 'where-it-fits)
                       (> (+ 3 (max org-gtd-agenda-property-column (current-column)) (length loc)) (window-width))))
              (progn
                (setq indentedLocation (concat "\n" (make-string col ?\ ) loc))
                (set-text-properties 0 (length indentedLocation) prop indentedLocation)
                (add-text-properties 0 (length indentedLocation) '(face font-lock-comment-face) indentedLocation)
                (insert indentedLocation))
            (setq loc (concat (make-string (max 0 (- org-gtd-agenda-property-column (current-column))) ?\ ) loc))
            (set-text-properties 0 (length loc) prop loc)
            (add-text-properties 0 (length loc) '(face font-lock-comment-face) loc)
            (insert loc)))))))

(defun org-gtd-agenda-property--prepare-props (props marker)
  "Prepare PROPS for property display lines, adding MARKER for interactivity.

Adds:
- `org-marker' MARKER so org-agenda commands work on property lines
- `org-gtd-property-line' so the processing loop skips these lines

This allows users to interact with agenda items even when their cursor
is on the property display line below the main item.

See: https://github.com/Malabarba/org-agenda-property/issues/6"
  (append props (list 'org-marker marker 'org-gtd-property-line t)))

(defun org-gtd-agenda-property-create-string (marker)
  "Creates a string of properties to be inserted in the agenda buffer."
  (let ((out " [")
        (first t))
    (dolist (cur org-gtd-agenda-property-list)
      (let ((prop (org-entry-get marker cur 'selective)))
        (when prop
          (setq out (if first (concat out prop)
                      (concat out org-gtd-agenda-property-separator prop)))
          (setq first nil))))
    (if first nil
      (concat out "]"))))

;;;###autoload
(eval-after-load 'org-agenda
  '(if (boundp 'org-agenda-finalize-hook)
       (add-hook 'org-agenda-finalize-hook 'org-gtd-agenda-property-add-properties)
     (add-hook 'org-finalize-agenda-hook 'org-gtd-agenda-property-add-properties)))

;;;###autoload
(if (boundp 'org-agenda-finalize-hook)
    (add-hook 'org-agenda-finalize-hook 'org-gtd-agenda-property-add-properties)
  (when (boundp 'org-finalize-agenda-hook)
    (add-hook 'org-finalize-agenda-hook 'org-gtd-agenda-property-add-properties)))

(provide 'org-gtd-agenda-property)
;;; org-gtd-agenda-property.el ends here
