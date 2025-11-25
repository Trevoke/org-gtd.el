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

(require 'cl-lib)
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


;; Note: org-gtd-directory must be defined above for the defvars below

(unless (boundp 'org-gtd-archive-location)
  (defvar org-gtd-archive-location
    (lambda ()
      (let* ((year (number-to-string (caddr (calendar-current-date))))
             (full-org-gtd-path (expand-file-name org-gtd-directory))
             (filename (format "gtd_archive_%s" year))
             (filepath (f-join full-org-gtd-path filename)))
        (string-join `(,filepath "::" "datetree/"))))))


;; New user option to control buffer saving behavior after organizing
(defcustom org-gtd-save-after-organize nil
  "If non-nil, save all modified buffers after each organize step."
  :group 'org-gtd
  :type 'boolean
  :package-version '(org-gtd . "3.1"))

;;;; GTD Semantic Keyword Mapping

(defun org-gtd--validate-and-set-keyword-mapping (symbol value)
  "Validate and set the org-gtd keyword mapping.
SYMBOL should be `org-gtd-keyword-mapping' and VALUE should be the new mapping.

Validates that:
- All required mappings exist (todo, next, wait, done, canceled)
- All mapped keywords exist in `org-todo-keywords'
- All GTD keywords are in the same sequence within `org-todo-keywords'

Only sets the value if validation passes."
  ;; Skip validation during byte compilation or when loading - org-todo-keywords may not be set up yet
  (if (or byte-compile-current-file
          (bound-and-true-p byte-compile-current-buffer)
          (bound-and-true-p load-in-progress)
          )
      (set-default symbol value)
    ;; Normal runtime validation
    (org-gtd--validate-and-set-keyword-mapping-runtime symbol value)))

(defun org-gtd--validate-and-set-keyword-mapping-runtime (symbol value)
  "Runtime validation for org-gtd keyword mapping."
  (let ((todo-kw (alist-get 'todo value))
        (next-kw (alist-get 'next value))
        (wait-kw (alist-get 'wait value))
        (done-kw (alist-get 'done value))
        (canceled-kw (alist-get 'canceled value))
        (all-sequences (if (listp (car org-todo-keywords))
                           org-todo-keywords
                         (list org-todo-keywords)))
        (errors nil))

    ;; Check that all required mappings exist
    (unless todo-kw
      (push "Missing 'todo' mapping in org-gtd-keyword-mapping" errors))
    (unless next-kw
      (push "Missing 'next' mapping in org-gtd-keyword-mapping" errors))
    (unless wait-kw
      (push "Missing 'wait' mapping in org-gtd-keyword-mapping" errors))
    (unless done-kw
      (push "Missing 'done' mapping in org-gtd-keyword-mapping" errors))
    (unless canceled-kw
      (push "Missing 'canceled' mapping in org-gtd-keyword-mapping" errors))

    (when (and todo-kw next-kw wait-kw done-kw canceled-kw)
      (let ((gtd-keywords (list todo-kw next-kw wait-kw done-kw canceled-kw))
            (found-sequence nil))

        ;; Check that all keywords exist somewhere in org-todo-keywords
        (let ((all-keywords (if (listp (car org-todo-keywords))
                                ;; Extract keywords from sequences, removing "|" separators
                                (cl-remove-if (lambda (kw) (string-match-p "^|" kw))
                                              (apply #'append (mapcar #'cdr org-todo-keywords)))
                              ;; Simple list format
                              org-todo-keywords)))
          (dolist (keyword gtd-keywords)
            (unless (member keyword all-keywords)
              (push (format "GTD keyword '%s' not found in org-todo-keywords" keyword) errors))))

        ;; Check that all GTD keywords are in the same sequence
        (when (not errors)  ; Only check sequences if all keywords exist
          (catch 'found
            (dolist (sequence all-sequences)
              (let ((seq-keywords (cl-remove-if (lambda (kw) (string-match-p "^|" kw))
                                                (if (listp sequence) (cdr sequence) sequence))))
                (when (cl-every (lambda (kw) (member kw seq-keywords)) gtd-keywords)
                  (setq found-sequence sequence)
                  (throw 'found t))))

            ;; If we get here, keywords are not all in the same sequence
            (unless found-sequence
              (push (format "All GTD keywords (%s) must be in the same sequence within org-todo-keywords"
                            (string-join gtd-keywords ", "))
                    errors))))))

    ;; Only set the value if validation passed
    (if errors
        (user-error "org-gtd keyword configuration errors:\n%s\n\nExample valid configuration:\n(setq org-todo-keywords '((sequence \"TODO\" \"NEXT\" \"WAIT\" \"|\" \"DONE\" \"CNCL\")))\n(setopt org-gtd-keyword-mapping\n        '((todo . \"TODO\") (next . \"NEXT\") (wait . \"WAIT\") (done . \"DONE\") (canceled . \"CNCL\")))"
                    (string-join (reverse errors) "\n"))
      (set-default symbol value))))




;;;###autoload
(defun org-gtd-setup-keywords-wizard ()
  "Configure GTD keyword mapping interactively.
Walks you through mapping your org TODO keywords to GTD concepts
like TODO, NEXT, WAIT, and CANCELED."
  (interactive)
  (let ((available-keywords org-todo-keywords-1))
    (unless available-keywords
      (user-error "No TODO keywords found. Please configure `org-todo-keywords' first"))

    (message "Setting up org-gtd keyword mapping...")
    (message "Available TODO keywords: %s" (string-join available-keywords " "))

    (let ((todo-kw (completing-read "Keyword for 'TODO' (not ready to act): "
                                    available-keywords nil t "TODO"))
          (next-kw (completing-read "Keyword for 'NEXT' (ready to act): "
                                    available-keywords nil t "NEXT"))
          (wait-kw (completing-read "Keyword for 'WAIT' (blocked/delegated): "
                                    available-keywords nil t "WAIT"))
          (canceled-kw (completing-read "Keyword for 'CANCELED': "
                                        available-keywords nil t "CNCL")))
      (customize-save-variable
       'org-gtd-keyword-mapping
       `((todo . ,todo-kw)
         (next . ,next-kw)
         (wait . ,wait-kw)
         (canceled . ,canceled-kw)))

      (message "org-gtd keyword configuration complete!"))))

(defcustom org-gtd-keyword-mapping
  '((todo . "TODO")
    (next . "NEXT")
    (wait . "WAIT")
    (done . "DONE")
    (canceled . "CNCL"))
  "Mapping of GTD semantic states to org-todo-keywords.

Each entry maps a GTD semantic state to a keyword from your `org-todo-keywords':
- \\='todo\\=' - tasks not ready to be acted upon
- \\='next\\=' - tasks ready to be acted upon immediately
- \\='wait\\=' - tasks waiting for someone else or blocked
- \\='done\\=' - tasks successfully completed
- \\='canceled\\=' - tasks terminated and will not be completed

This variable validates that:
- All required mappings exist (todo, next, wait, done, canceled)
- All mapped keywords exist in `org-todo-keywords'
- All GTD keywords are in the same sequence within `org-todo-keywords'

When setting programmatically, use `setopt' (Emacs 29+) or
`customize-set-variable' to ensure validation runs. To set manually
without validation, use `setq' but ensure the mapping is valid."
  :type '(alist :key-type (choice (const todo)
                                  (const next)
                                  (const wait)
                                  (const done)
                                  (const canceled))
                :value-type string)
  :set #'org-gtd--validate-and-set-keyword-mapping
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))

;; Backward compatibility - mark old variables as obsolete
(defcustom org-gtd-todo-keyword nil
  "OBSOLETE: Use `org-gtd-keyword-mapping' instead."
  :type '(choice (const nil) string)
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))
(make-obsolete-variable 'org-gtd-todo-keyword 'org-gtd-keyword-mapping "4.0")

(defcustom org-gtd-next-keyword nil
  "OBSOLETE: Use `org-gtd-keyword-mapping' instead."
  :type '(choice (const nil) string)
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))
(make-obsolete-variable 'org-gtd-next-keyword 'org-gtd-keyword-mapping "4.0")

(defcustom org-gtd-wait-keyword nil
  "OBSOLETE: Use `org-gtd-keyword-mapping' instead."
  :type '(choice (const nil) string)
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))
(make-obsolete-variable 'org-gtd-wait-keyword 'org-gtd-keyword-mapping "4.0")

(defcustom org-gtd-canceled-keyword nil
  "OBSOLETE: Use `org-gtd-keyword-mapping' instead."
  :type '(choice (const nil) string)
  :group 'org-gtd
  :package-version '(org-gtd . "4.0"))
(make-obsolete-variable 'org-gtd-canceled-keyword 'org-gtd-keyword-mapping "4.0")

;;;; Constants

(defconst org-gtd-timestamp "ORG_GTD_TIMESTAMP"
  "Org property storing timestamps for `org-gtd' logic.")

;;;;; GTD Category Constants

(defconst org-gtd-action "Actions"
  "GTD category for single action tasks.")

(defconst org-gtd-projects "Projects"
  "GTD category for multi-step projects.")

(defconst org-gtd-calendar "Calendar"
  "GTD category for calendar/time-specific items.")

(defconst org-gtd-habit "Habits"
  "GTD category for recurring habits.")

(defconst org-gtd-incubate "Incubated"
  "GTD category for someday/maybe items.")

(defconst org-gtd-knowledge "Reference"
  "GTD category for reference materials/knowledge.")

(defconst org-gtd-trash "Trash"
  "GTD category for discarded items.")

;;;;; Org-mode Special Property Names

(defconst org-gtd-prop-todo "TODO"
  "Org-mode property name that stores the TODO keyword state.")

(defconst org-gtd-prop-style "STYLE"
  "Org-mode STYLE property name, used for habits and other styling.")

(defconst org-gtd-prop-style-value-habit "habit"
  "Value for STYLE property to mark an item as a habit.")

(defconst org-gtd-prop-area-of-focus "CATEGORY"
  "Org-mode CATEGORY property, used in org-gtd for Areas of Focus (GTD Horizons).
This property also controls the prefix displayed in agenda views.")

;;;;; Org-gtd Property Names

(defconst org-gtd-prop-depends-on "ORG_GTD_DEPENDS_ON"
  "Property storing task IDs this task depends on.")

(defconst org-gtd-prop-blocks "ORG_GTD_BLOCKS"
  "Property storing task IDs this task blocks.")

(defconst org-gtd-prop-first-tasks "ORG_GTD_FIRST_TASKS"
  "Property storing root task IDs for a project.")

(defconst org-gtd-prop-project-ids "ORG_GTD_PROJECT_IDS"
  "Property storing project IDs this task belongs to.")

(defconst org-gtd-prop-category "ORG_GTD"
  "Property storing org-gtd category (Actions, Projects, etc.).")

(defconst org-gtd-prop-project "ORG_GTD_PROJECT"
  "Property storing the primary project name for a task.")

(defconst org-gtd-prop-refile "ORG_GTD_REFILE"
  "Property storing the refile target category.")

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

(defun org-gtd-keywords--get-effective-mapping ()
  "Get the effective keyword mapping, handling backward compatibility."
  (cond
   ;; If new mapping is configured, use it
   ((and org-gtd-keyword-mapping
         (alist-get 'todo org-gtd-keyword-mapping)
         (alist-get 'next org-gtd-keyword-mapping)
         (alist-get 'wait org-gtd-keyword-mapping)
         (alist-get 'done org-gtd-keyword-mapping)
         (alist-get 'canceled org-gtd-keyword-mapping))
    org-gtd-keyword-mapping)
   ;; If old variables are set, convert them (with warning)
   ((or org-gtd-todo-keyword org-gtd-next-keyword
        org-gtd-wait-keyword org-gtd-canceled-keyword)
    (display-warning 'org-gtd
                     "Old individual keyword variables are deprecated. Please use `org-gtd-keyword-mapping' instead."
                     :warning)
    `((todo . ,(or org-gtd-todo-keyword "TODO"))
      (next . ,(or org-gtd-next-keyword "NEXT"))
      (wait . ,(or org-gtd-wait-keyword "WAIT"))
      (done . "DONE")
      (canceled . ,(or org-gtd-canceled-keyword "CNCL"))))
   ;; Default fallback
   (t org-gtd-keyword-mapping)))

(defun org-gtd-keywords--todo ()
  "Get keyword for GTD \\='todo\\=' semantic state."
  (alist-get 'todo (org-gtd-keywords--get-effective-mapping)))

(defun org-gtd-keywords--next ()
  "Get keyword for GTD \\='next\\=' semantic state."
  (alist-get 'next (org-gtd-keywords--get-effective-mapping)))

(defun org-gtd-keywords--wait ()
  "Get keyword for GTD \\='wait\\=' semantic state."
  (alist-get 'wait (org-gtd-keywords--get-effective-mapping)))

(defun org-gtd-keywords--done ()
  "Get keyword for GTD \\='done\\=' semantic state."
  (alist-get 'done (org-gtd-keywords--get-effective-mapping)))

(defun org-gtd-keywords--canceled ()
  "Get keyword for GTD \\='canceled\\=' semantic state."
  (alist-get 'canceled (org-gtd-keywords--get-effective-mapping)))

(defun org-gtd-keywords--is-done-p (keyword)
  "Check if KEYWORD represents a completed state."
  (member keyword org-done-keywords))

;;;;; Core Functions

(defun org-gtd-buffer-p (&optional buffer)
  "Return t if BUFFER (or current buffer if nil) is an org-gtd managed buffer.
A buffer is considered org-gtd managed if its file is within `org-gtd-directory'."
  (let ((file (buffer-file-name (or buffer (current-buffer)))))
    (and file
         (string-prefix-p (expand-file-name org-gtd-directory)
                          (expand-file-name file)))))

(defun org-gtd-save-buffers ()
  "Save all modified org-gtd buffers.
Only saves buffers that are in `org-gtd-directory'."
  (when org-gtd-save-after-organize
    (save-some-buffers t #'org-gtd-buffer-p)))

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

;;;;; Note: Using native org-mode multivalued property functions
;; org-entry-get-multivalued-property
;; org-entry-add-to-multivalued-property
;; org-entry-remove-from-multivalued-property
;; These are part of org-mode's Property API

;;;; Footer

(provide 'org-gtd-core)

;;; org-gtd-core.el ends here
