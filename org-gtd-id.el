;;; org-gtd-id.el --- generating ids for tasks -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023, 2025 Aldric Giacomoni

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
;; Generating ids from tasks.
;; Most of this code is stolen and adapted from Karl Voit's code and demo at
;; https://gitlab.com/publicvoit/orgmode-link-demo/-/raw/main/link_demo.org
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'org-id)
(require 'org-macs)
(require 'ffap)

;;;; Commands

(defun org-gtd-id-get-create (&optional pom)
  "Get the ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.
If the entry does not have an ID, create an ID prefixed for org-gtd.
In any case, the ID of the entry is returned.

This function is a modified copy of `org-id-get'."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "ID")))
      (if (and id (stringp id) (string-match "\\S-" id))
          id
        (setq id (org-gtd-id--generate))
        (org-entry-put pom "ID" id)
        (org-id-add-location id "Org GTD WIP buffer")
        id))))

;;;; Functions

;;;;; Private

(defconst org-gtd-id--max-heading-length 50
  "Maximum length for the heading portion of generated IDs.
The timestamp suffix is added after this limit.")

(defun org-gtd-id--generate ()
  "Generate and return a new id.
The generated ID is stripped off potential progress indicator cookies and
sanitized to get a slug.  Furthermore, it is suffixed with an ISO date-stamp.
The heading portion is truncated to `org-gtd-id--max-heading-length' characters."
  (let* ((my-heading-text (or (nth 4 (org-heading-components))
                              "org-gtd-makeshift-id"))
         (clean-text (org-gtd-id--remove-week-time-from-inactive-timestamps
                      (org-gtd-id--remove-day-time-from-active-timestamps
                       (org-gtd-id--remove-links
                        (org-gtd-id--remove-priority-indicators
                         (org-gtd-id--remove-tally-progress-indicators
                          (org-gtd-id--remove-percent-progress-indicators
                           my-heading-text)))))))
         (raw-id (org-gtd-id--generate-sanitized-alnum-dash-string clean-text))
         (truncated-id (org-gtd-id--truncate-to-limit raw-id))
         (timestamp (format-time-string "%F-%H-%M-%S")))
    (concat truncated-id "-" timestamp)))

(defun org-gtd-id--truncate-to-limit (str)
  "Truncate STR to `org-gtd-id--max-heading-length' chars, ending cleanly.
Truncates at word boundary (dash) when possible, removes trailing dashes."
  (if (<= (length str) org-gtd-id--max-heading-length)
      str
    (let ((truncated (substring str 0 org-gtd-id--max-heading-length)))
      ;; Try to end at a word boundary (last dash before limit)
      (if-let ((last-dash (string-match-p "-[^-]*$" truncated)))
          (substring truncated 0 last-dash)
        ;; No dash found, just use the truncated string and remove trailing dash
        (replace-regexp-in-string "-+$" "" truncated)))))

(defun org-gtd-id--remove-percent-progress-indicators (heading)
  "Remove [N%] progress indicators from HEADING."
  (replace-regexp-in-string "\\(\\[[0-9]+%\\]\\)" "" heading))

(defun org-gtd-id--remove-tally-progress-indicators (heading)
  "Remove [N/M] tally progress indicators from HEADING."
  (replace-regexp-in-string "\\(\\[[0-9]+/[0-9]+\\]\\)" "" heading))

(defun org-gtd-id--remove-priority-indicators (heading)
  "Remove [#A], [#B], [#C] priority indicators from HEADING."
  (replace-regexp-in-string "\\(\\[#[ABC]\\]\\)" "" heading))

(defun org-gtd-id--remove-links (heading)
  "Remove org link markup from HEADING, keeping link text."
  (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[" "" heading t))

(defun org-gtd-id--remove-day-time-from-active-timestamps (heading)
  "Remove day and time from active timestamps in HEADING."
  (replace-regexp-in-string "<[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)>" "" heading t))

(defun org-gtd-id--remove-week-time-from-inactive-timestamps (heading)
  "Remove week and time from inactive timestamps in HEADING."
  (replace-regexp-in-string "\\[[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)\\]" "" heading t))

(defun org-gtd-id--generate-sanitized-alnum-dash-string (str)
  "Clean up STR and make it fit to be used as an org id.

Returns a string which contains only a-zA-Z0-9 with single dashes replacing
all other characters in-between them.

Some parts were copied and adapted from org-hugo-slug from
https://github.com/kaushalmodi/ox-hugo (GPLv3).

Taken from
https://gitlab.com/publicvoit/orgmode-link-demo/-/raw/main/link_demo.org ."
  (let* (;; Remove "<FOO>..</FOO>" HTML tags if present.
         (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
         ;; Remove URLs if present in the string.  The ")" in the
         ;; below regexp is the closing parenthesis of a Markdown
         ;; link: [Desc](Link).
         (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
         ;; Replace "&" with " and ", "." with " dot ", "+" with
         ;; " plus ".
         (str (replace-regexp-in-string
               "&" " and "
               (replace-regexp-in-string
                "\\." " dot "
                (replace-regexp-in-string
                 "\\+" " plus " str))))
         ;; Replace German Umlauts with 7-bit ASCII.
         (str (replace-regexp-in-string "ä" "ae" str nil))
         (str (replace-regexp-in-string "ü" "ue" str nil))
         (str (replace-regexp-in-string "ö" "oe" str nil))
         (str (replace-regexp-in-string "ß" "ss" str nil))
         ;; Replace all characters except alphabets, numbers and
         ;; parentheses with spaces.
         (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
         ;; Multibyte punctuation characters are handled correctly in Emacs 25+
         (str str)
         ;; Remove leading and trailing whitespace.
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         ;; Replace 2 or more spaces with a single space.
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         ;; Replace parentheses with double-hyphens.
         (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
         ;; Remove any remaining parentheses character.
         (str (replace-regexp-in-string "[()]" "" str))
         ;; Replace spaces with hyphens.
         (str (replace-regexp-in-string " " "-" str))
         ;; Remove leading and trailing hyphens.
         (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
    str))

;;;; Footer

(provide 'org-gtd-id)

;;; org-gtd-id.el ends here
