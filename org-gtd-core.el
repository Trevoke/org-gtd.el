;;; org-gtd-core.el --- Core code for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Core logic for org-gtd
;; Creating this file because straight.el seems unhappy.
;;
;;; Code:

;;;; Requirements

(require 'org-agenda-property)

(require 'org-gtd-backward-compatibility)

;;;; Customization

(defgroup org-gtd nil
  "Customize the org-gtd package."
  :group 'org
  :link '(url-link "https://github.com/Trevoke/org-gtd.el")
  :package-version '(org-gtd . "0.1"))

(defcustom org-gtd-canceled "CNCL"
  "The `org-mode' keyword for a canceled task.

 See `org-todo-keywords' for customization options."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-canceled-suffix "(c@)"
  "Additional `org-mode' tools for this keyword.  Example: \"(w@/!)\".

You can define:
- a key to be used with `org-use-fast-todo-selection'
- behavior (optional note/timestamp) for entering state
- behavior (optional note/timestamp) for leaving state.

See `org-todo-keywords' for definition."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-directory "~/gtd/"
  "Directory for org-gtd.

The package will use this directory for all its functionality, whether it is
building the agenda or refiling items.  This is the directory where you will
find the default org-gtd file, and it is the directory where you should place
your own files if you want multiple refile targets (projects, etc.)."
  :group 'org-gtd
  :package-version '(org-gtd . "0.1")
  :type 'directory)

(defcustom org-gtd-done "DONE"
  "The `org-mode' keyword for a finished task.

 See `org-todo-keywords' for customization options."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-done-suffix "(d)"
  "Additional `org-mode' tools for this keyword.  Example: \"(w@/!)\".

You can define:
- a key to be used with `org-use-fast-todo-selection'
- behavior (optional note/timestamp) for entering state
- behavior (optional note/timestamp) for leaving state.

See `org-todo-keywords' for definition."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-next "NEXT"
  "The `org-mode' keyword for an action ready to be done.  Just the word."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-next-suffix "(n)"
  "Additional `org-mode' tools for this keyword.  Example: \"(w@/!)\".

You can define:
- a key to be used with `org-use-fast-todo-selection'
- behavior (optional note/timestamp) for entering state
- behavior (optional note/timestamp) for leaving state.

See `org-todo-keywords' for definition."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-todo "TODO"
  "The `org-mode' keyword for an upcoming action (not yet ready, not blocked).

See `org-todo-keywords' for customization options."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-todo-suffix "(t)"
  "Additional `org-mode' tools for this keyword.  Example: \"(w@/!)\".

You can define:
- a key to be used with `org-use-fast-todo-selection'
- behavior (optional note/timestamp) for entering state
- behavior (optional note/timestamp) for leaving state.

See `org-todo-keywords' for definition."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-wait "WAIT"
  "The `org-mode' keyword when an action is blocked/delegated.

See `org-todo-keywords' for customization options."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

(defcustom org-gtd-wait-suffix "(w@)"
  "Additional `org-mode' tools for this keyword.  Example: \"(w@/!)\".

You can define:
- a key to be used with `org-use-fast-todo-selection'
- behavior (optional note/timestamp) for entering state
- behavior (optional note/timestamp) for leaving state.

See `org-todo-keywords' for definition."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0")
  :type 'string)

;;;; Constants

(defconst org-gtd-timestamp "ORG_GTD_TIMESTAMP"
  "Org property storing timestamps for `org-gtd' logic.")

;;;; Variables

(defvar org-gtd-archive-location)
(defvar org-gtd-delegate-property)
(defvar org-gtd-project-headings)
(defvar org-gtd-stuck-projects)

(defvar-local org-gtd--loading-p nil
  "`Org-gtd' sets this variable after it has changed the state in this buffer.")

;;;; Macros

;;;###autoload
(defmacro with-org-gtd-context (&rest body)
  "Wrap BODY... in this macro to inherit the org-gtd settings for your logic."
  (declare (debug t) (indent 2))
  `(let* ((org-use-property-inheritance "ORG_GTD")
          (org-todo-keywords `((sequence ,(string-join `(,org-gtd-next ,org-gtd-next-suffix))
                                         ,(string-join `(,org-gtd-todo ,org-gtd-todo-suffix))
                                         ,(string-join `(,org-gtd-wait ,org-gtd-wait-suffix))
                                         "|"
                                         ,(string-join `(,org-gtd-done ,org-gtd-done-suffix))
                                         ,(string-join `(,org-gtd-canceled ,org-gtd-canceled-suffix)))))
          ;; (org-log-done 'time)
          ;; (org-log-done-with-time t)
          ;; (org-log-refile 'time)
          (org-archive-location (funcall org-gtd-archive-location))
          ;(org-refile-use-outline-path nil)
          (org-stuck-projects org-gtd-stuck-projects)
          (org-odd-levels-only nil)
          (org-agenda-files (org-gtd-core--agenda-files))
          (org-agenda-property-list `(,org-gtd-delegate-property)))
     (unwind-protect
         (progn
           (advice-add 'org-agenda-files :filter-return #'org-gtd-core--uniq)
           ,@body)
       (progn
         (advice-remove 'org-agenda-files #'org-gtd-core--uniq)))))

;;;; Functions

;;;;; Public

(defun org-gtd-core-prepare-agenda-buffers ()
  "Ensure `org-mode' has the desired settings in the agenda buffers."
  (mapc
   (lambda (file) (org-gtd-core-prepare-buffer (find-file-noselect file)))
   (-flatten
    (mapcar
     (lambda (org-agenda-entry) (if (f-directory-p org-agenda-entry)
                                    (directory-files org-agenda-entry t org-agenda-file-regexp t)
                                  org-agenda-entry))
     (with-org-gtd-context (org-gtd-core--agenda-files))))))

(defun org-gtd-core-prepare-buffer (&optional buffer)
  "Make sure BUFFER is prepared to handle Org GTD operations.

If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (unless (bound-and-true-p org-gtd--loading-p)
      (setq-local org-gtd--loading-p t)
      (with-org-gtd-context
          (with-temp-message ""
            (org-mode-restart)))
      (setq-local org-gtd--loading-p t))))

;;;;; Private

(define-error
  'org-gtd-error
  "Something went wrong with `org-gtd'"
  'user-error)

(defun org-gtd-core--agenda-files ()
  "Concatenate `org-agenda-files' variable with `org-gtd-directory' contents."
  (seq-uniq (if (stringp org-agenda-files)
                (append (org-read-agenda-file-list)
                        (ensure-list org-gtd-directory))
              (append (ensure-list org-agenda-files)
                      (ensure-list org-gtd-directory)))))

(defun org-gtd-core--uniq (list)
  (seq-uniq list))

;;;; Footer

(provide 'org-gtd-core)

;;; org-gtd-core.el ends here
