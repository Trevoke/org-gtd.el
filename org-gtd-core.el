;;; org-gtd-core.el --- Core code for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
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
;; Core logic for org-gtd
;; Creating this file because straight.el seems unhappy.
;;
;;; Code:

;;;; Requirements

(require 'f)
(require 'org-agenda-property)

(require 'org-gtd-backward-compatibility)

;;;; Forward declarations
(defvar org-gtd-delegate-property)
(defvar org-gtd-archive-location)
(defvar org-gtd-projects)
(declare-function org-gtd-stuck-projects "org-gtd-projects")

;;;; Essential variables for autoload compatibility
;; These provide fallback values when the full modules aren't loaded
(unless (boundp 'org-gtd-archive-location)
  (defvar org-gtd-archive-location
    (lambda ()
      (let* ((year (number-to-string (caddr (calendar-current-date))))
             (full-org-gtd-path (expand-file-name org-gtd-directory))
             (filename (format "gtd_archive_%s" year))
             (filepath (f-join full-org-gtd-path filename)))
        (string-join `(,filepath "::" "datetree/"))))))

(unless (boundp 'org-gtd-stuck-projects)
  (defvar org-gtd-stuck-projects
    '("ORG_GTD=\"Projects\""
      ("TODO" "NEXT")
      ("@computer" "@phone" "@travel" "@agenda")
      "")))

(unless (boundp 'org-gtd-delegate-property)
  (defvar org-gtd-delegate-property "DELEGATED_TO"))

;;;; Customization

(defgroup org-gtd nil
  "Customize the org-gtd package."
  :group 'org
  :link '(url-link "https://github.com/Trevoke/org-gtd.el")
  :package-version '(org-gtd . "0.1"))


(defcustom org-gtd-directory "~/gtd/"
  "Directory for org-gtd.

The package will use this directory for all its functionality, whether it is
building the agenda or refiling items.  This is the directory where you will
find the default org-gtd file, and it is the directory where you should place
your own files if you want multiple refile targets (projects, etc.)."
  :group 'org-gtd
  :package-version '(org-gtd . "0.1")
  :type 'directory)





;; New user option to control buffer saving behavior after organizing
(defcustom org-gtd-save-after-organize nil
  "If non-nil, save all modified buffers after each organize step."
  :group 'org-gtd
  :type 'boolean
  :package-version '(org-gtd . "3.1"))

;;;; GTD Semantic Keyword Mapping

(defcustom org-gtd-todo-keyword "TODO"
  "Keyword for GTD \\='todo\\=' state (not ready to act).
This should be a keyword from your `org-todo-keywords' that represents
tasks that are not yet ready to be acted upon."
  :type 'string
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))

(defcustom org-gtd-next-keyword "NEXT"  
  "Keyword for GTD \\='next\\=' state (ready to act).
This should be a keyword from your `org-todo-keywords' that represents
tasks that are ready to be acted upon immediately."
  :type 'string
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))

(defcustom org-gtd-wait-keyword "WAIT"
  "Keyword for GTD \\='wait\\=' state (blocked/delegated).
This should be a keyword from your `org-todo-keywords' that represents
tasks that are waiting for someone else or are blocked."
  :type 'string
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))

(defcustom org-gtd-canceled-keyword "CNCL"
  "Keyword for GTD \\='canceled\\=' state (terminated, not done).
This should be a keyword from your `org-todo-keywords' that represents
tasks that have been canceled and will not be completed."
  :type 'string
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))

;;;; Constants

(defconst org-gtd-timestamp "ORG_GTD_TIMESTAMP"
  "Org property storing timestamps for `org-gtd' logic.")

;;;; Variables

(defvar-local org-gtd--loading-p nil
  "`Org-gtd' sets this variable after it has changed the state in this buffer.")

;;;; Commands

(defun org-gtd-set-event-date-on-heading-at-point ()
  (interactive)
  (let ((old-timestamp (org-entry-get nil "ORG_GTD_TIMESTAMP"))
        (new-timestamp (org-read-date nil t)))
    ;; Replace the ORG_GTD_TIMESTAMP property
    (org-entry-put nil "ORG_GTD_TIMESTAMP" new-timestamp)

    ;; Move to the end of the current heading
    (save-excursion
      (org-end-of-subtree)
      (if (search-backward old-timestamp nil t)
          (replace-match new-timestamp)
        ;; If the timestamp is not found, insert it at the end of the body
        (insert "\n" new-timestamp)))))

;;;; Macros

;;;###autoload
(defmacro with-org-gtd-context (&rest body)
  "Wrap BODY... in this macro to inherit the org-gtd settings for your logic."
  (declare (debug t) (indent 2))
  `(let* ((org-use-property-inheritance "ORG_GTD")
          ;; (org-log-done 'time)
          ;; (org-log-done-with-time t)
          ;; (org-log-refile 'time)
          (org-archive-location (funcall org-gtd-archive-location))
                                        ;(org-refile-use-outline-path nil)
          (org-stuck-projects (org-gtd-stuck-projects))
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

;;;;; GTD Keyword Semantic Functions

(defun org-gtd-keywords--todo ()
  "Get keyword for GTD \\='todo\\=' semantic state."
  org-gtd-todo-keyword)

(defun org-gtd-keywords--next ()
  "Get keyword for GTD \\='next\\=' semantic state."
  org-gtd-next-keyword)

(defun org-gtd-keywords--wait ()
  "Get keyword for GTD \\='wait\\=' semantic state."
  org-gtd-wait-keyword)

(defun org-gtd-keywords--canceled ()
  "Get keyword for GTD \\='canceled\\=' semantic state."
  org-gtd-canceled-keyword)

(defun org-gtd-keywords--is-done-p (keyword)
  "Check if KEYWORD represents a completed state."
  (member keyword org-done-keywords))

(defun org-gtd-keywords--validate-configuration ()
  "Validate that all GTD semantic keywords are properly configured."
  ;; Skip validation if org-todo-keywords-1 is not yet initialized
  ;; This happens during autoload or early initialization
  (when org-todo-keywords-1
    (let ((all-todo-keywords org-todo-keywords-1)
          (errors nil))
      
      (unless (member org-gtd-todo-keyword all-todo-keywords)
        (push (format "org-gtd-todo-keyword '%s' not found in org-todo-keywords" 
                      org-gtd-todo-keyword) errors))
      
      (unless (member org-gtd-next-keyword all-todo-keywords)
        (push (format "org-gtd-next-keyword '%s' not found in org-todo-keywords" 
                      org-gtd-next-keyword) errors))
      
      (unless (member org-gtd-wait-keyword all-todo-keywords)
        (push (format "org-gtd-wait-keyword '%s' not found in org-todo-keywords" 
                      org-gtd-wait-keyword) errors))
      
      (unless (member org-gtd-canceled-keyword all-todo-keywords)
        (push (format "org-gtd-canceled-keyword '%s' not found in org-todo-keywords" 
                      org-gtd-canceled-keyword) errors))
      
      (when errors
        (user-error "org-gtd keyword configuration errors:\n%s" 
                    (string-join errors "\n"))))
    
    t))

(defun org-gtd-core--ensure-keyword-configuration ()
  "Ensure GTD keyword configuration is valid."
  (condition-case err
      (org-gtd-keywords--validate-configuration)
    (user-error 
     (display-warning 'org-gtd 
                      (format "GTD keyword configuration error: %s" 
                              (error-message-string err))
                      :error))))

;;;###autoload
(defun org-gtd-migrate-keyword-configuration ()
  "Migrate from old hardcoded keywords to new semantic mapping."
  (interactive)
  (let ((migrated nil))
    (when (and (boundp 'org-gtd-next) org-gtd-next)
      (customize-save-variable 'org-gtd-next-keyword org-gtd-next)
      (setq migrated t))
    (when (and (boundp 'org-gtd-todo) org-gtd-todo)
      (customize-save-variable 'org-gtd-todo-keyword org-gtd-todo)
      (setq migrated t))
    (when (and (boundp 'org-gtd-wait) org-gtd-wait)
      (customize-save-variable 'org-gtd-wait-keyword org-gtd-wait)
      (setq migrated t))
    (when (and (boundp 'org-gtd-canceled) org-gtd-canceled)
      (customize-save-variable 'org-gtd-canceled-keyword org-gtd-canceled)
      (setq migrated t))
    (when migrated
      (message "Migrated org-gtd keyword configuration. Please restart Emacs.")
      (org-gtd-keywords--validate-configuration))))

;;;###autoload
(defun org-gtd-setup-keywords-wizard ()
  "Interactive wizard to configure GTD keyword mapping."
  (interactive)
  (let ((available-keywords org-todo-keywords-1))
    (unless available-keywords
      (user-error "No TODO keywords found. Please configure `org-todo-keywords' first"))
    
    (message "Setting up org-gtd keyword mapping...")
    (message "Available TODO keywords: %s" (string-join available-keywords " "))
    
    (customize-save-variable 
     'org-gtd-todo-keyword
     (completing-read "Keyword for 'TODO' (not ready to act): " 
                      available-keywords nil t "TODO"))
    
    (customize-save-variable 
     'org-gtd-next-keyword  
     (completing-read "Keyword for 'NEXT' (ready to act): " 
                      available-keywords nil t "NEXT"))
    
    (customize-save-variable 
     'org-gtd-wait-keyword
     (completing-read "Keyword for 'WAIT' (blocked/delegated): " 
                      available-keywords nil t "WAIT"))
    
    (customize-save-variable 
     'org-gtd-canceled-keyword
     (completing-read "Keyword for 'CANCELED': " 
                      available-keywords nil t "CNCL"))
    
    (org-gtd-keywords--validate-configuration)
    (message "org-gtd keyword configuration complete!")))

;;;;; Core Functions

(defun org-gtd-core-prepare-agenda-buffers ()
  "Ensure `org-mode' uses org-gtd settings in the relevant agenda buffers."
  (org-gtd-core--agenda-files)
  ;; (mapc
  ;;  (lambda (file) (org-gtd-core-prepare-buffer (find-file-noselect file)))
  ;;  (-flatten
  ;;   (mapcar
  ;;    (lambda (org-agenda-entry) (if (f-directory-p org-agenda-entry)
  ;;                                   (directory-files org-agenda-entry t org-agenda-file-regexp t)
  ;;                                 org-agenda-entry))
  ;;    (with-org-gtd-context (org-gtd-core--agenda-files)))))
  )

(defun org-gtd-core-prepare-buffer (&optional buffer)
  "Make sure BUFFER is prepared to handle Org GTD operations.

If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (unless (bound-and-true-p org-gtd--loading-p)
      (setq-local org-gtd--loading-p t)
      (org-gtd-core--ensure-keyword-configuration)
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
