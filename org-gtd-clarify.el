;;; org-gtd-clarify.el --- Handle clarifying tasks -*- lexical-binding: t; coding: utf-8 -*-
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
;; Set up Emacs to helpfully clarify tasks so they can then be organized.
;;
;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'org-agenda)

(require 'org-gtd-core)
(require 'org-gtd-wip)
(require 'org-gtd-horizons)

;;;; Customization

(defgroup org-gtd-clarify nil
  "Customize the behavior when clarifying an item."
  :group 'org-gtd
  :package-version '(org-gtd . "3.0"))

(defcustom org-gtd-clarify-project-templates nil
  "This is an alist of (\"template title\" . \"template\").

Used by `org-gtd-clarify-projects-insert-template', when clarifying an item
which turns out to be a project."
  :group 'org-gtd-clarify
  :type '(alist :key-type string :value-type string)
  :package-version '(org-gtd . "3.0.0"))

(defcustom org-gtd-clarify-show-horizons nil
  "If non-nil, show a side buffer with the horizons during item clarification.
The values can be: nil, top, right, left, bottom.

The file shown can be configured in `org-gtd-horizons-file'."
  :group 'org-gtd-clarify
  :options '('right 'top 'left 'bottom 'nil)
  :package-version '(org-gtd . "3.0")
  :type 'symbol)

(defcustom org-gtd-clarify-display-helper-buffer nil
  "If non-nil, display project dependencies helper window in WIP buffers.
When enabled, shows a live view of task relationships in a side window when
there are multiple tasks in the WIP buffer."
  :group 'org-gtd-clarify
  :package-version '(org-gtd . "4.0")
  :type 'boolean)


;;;; Variables

(defvar-local org-gtd-clarify--clarify-id nil
  "Reference to the org id of the heading currently in the WIP buffer.")

(defvar-local org-gtd-clarify--inbox-p nil
  "Used to separate a one-off clarify from the inbox clarification.")

(defvar-local org-gtd-clarify--source-heading-marker nil
  "Store marker to item that is being clarified.")

(defvar-local org-gtd-clarify--window-config nil
  "Store window configuration prior to clarifying task.")

(defvar-local org-gtd-clarify--skip-refile nil
  "When non-nil, update item in place instead of refiling.
Set via C-u prefix to clarify commands or transient toggle.")

;;;;; External variables (defined in org-gtd-process.el)

(defvar org-gtd-process--session-active)
(defvar org-gtd-process--pending-inboxes)

;;;;; Keymaps

(defvar org-gtd-clarify-map (make-sparse-keymap))

(define-key org-gtd-clarify-map (kbd "C-c C-k") #'org-gtd-clarify-stop)

;; code to make windows atomic, from emacs manual
;; (let ((window (split-window-right)))
;;   (window-make-atom (window-parent window))
;;   (display-buffer-in-atom-window
;;    (get-buffer-create "*Messages*")
;;    `((window . ,(window-parent window)) (window-height . 5))))

;; code to make windows non-atomic
;; (walk-window-subtree (lambda (window) (set-window-parameter window 'window-atom nil)) (window-parent (get-buffer-window (current-buffer))) t)

;; dedicated side window
;; (display-buffer-in-side-window (get-buffer "horizons.org") '((side . right) (dedicated . t)))

;;;; Macros

;;;###autoload
(define-minor-mode org-gtd-clarify-mode
  "Enable GTD clarification features in the current buffer.
Provides keybindings and interface for clarifying GTD items before
organizing them."
  :lighter " GPM"
  :keymap org-gtd-clarify-map
  :group 'org-gtd-clarify
  (if org-gtd-clarify-mode
      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<org-gtd-clarify-map>Clarify item.  Use `\\[org-gtd-organize]' to file it when finished, or `\\[org-gtd-clarify-stop]' to cancel."))
    (setq-local header-line-format nil)))

;;;; Commands

(defun org-gtd-clarify-agenda-item ()
  "Process item at point on agenda view.
With prefix argument (C-u), mark item for in-place update instead of refile."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (let ((heading-marker (or (org-get-at-bol 'org-marker)
                            (org-agenda-error)))
        (prefix-arg current-prefix-arg))
    ;; Rebind current-prefix-arg so org-gtd-clarify-item sees it
    (let ((current-prefix-arg prefix-arg))
      (org-gtd-clarify-item heading-marker
                            (current-window-configuration)))))

;;;###autoload
(defun org-gtd-clarify-item (&optional marker window-config)
  "Clarify the GTD item at point for decision-making.
Opens a dedicated clarification buffer where you can refine the item's
details before organizing it.

With prefix argument (C-u), mark item for in-place update instead of refile.

MARKER must be a marker pointing to an org heading.
WINDOW-CONFIG is the window config to set after clarification finishes."
  (interactive)
  (let* ((skip-refile current-prefix-arg)
         (window-config (or window-config (current-window-configuration)))
         (source-heading-marker (or marker (point-marker)))
         (clarify-id (org-gtd-id-get-create source-heading-marker))
         (processing-buffer (org-gtd-wip--get-buffer clarify-id)))
    (org-gtd-wip--maybe-initialize-buffer-contents source-heading-marker processing-buffer)
    (with-current-buffer processing-buffer
      (org-gtd-clarify-mode 1)
      (setq-local org-gtd-clarify--window-config window-config
                  org-gtd-clarify--source-heading-marker source-heading-marker
                  org-gtd-clarify--clarify-id clarify-id
                  org-gtd-clarify--skip-refile (when skip-refile t)))
    (org-gtd-clarify-setup-windows processing-buffer)))

(defun org-gtd-clarify-switch-to-buffer ()
  "Prompt the user to choose one of the existing WIP buffers."
  (interactive)
  (let ((buf-names (mapcar #'buffer-name (org-gtd-wip--get-buffers))))
    (if buf-names
        (let ((chosen-buf-name (completing-read "Choose a buffer: " buf-names nil t)))
          (org-gtd-clarify-setup-windows chosen-buf-name))
      (message "There are no Org-GTD WIP buffers."))))

(defun org-gtd-clarify-toggle-horizons-window ()
  "Toggle the window with the horizons buffer."
  (interactive)
  (let* ((buffer (org-gtd--horizons-file))
         (window (get-buffer-window buffer)))
    (if window
        (quit-window nil window)
      (org-gtd-clarify--display-horizons-window))))

(defun org-gtd-clarify-stop ()
  "Stop clarifying the current item and restore previous state.
Closes the horizons view, restores the window configuration,
cleans up temp file, and kills the WIP buffer without organizing the item."
  (interactive)
  (let ((window-config org-gtd-clarify--window-config)
        (task-id org-gtd-clarify--clarify-id)
        (inbox-p org-gtd-clarify--inbox-p))
    ;; Clean up horizons view
    (org-gtd-clarify--cleanup-horizons-view)
    ;; Clean up temp file and kill buffer
    (when task-id
      (org-gtd-wip--cleanup-temp-file task-id))
    ;; Clear inbox processing session state if we were processing inbox
    (when inbox-p
      (setq org-gtd-process--session-active nil
            org-gtd-process--pending-inboxes nil))
    ;; Restore window configuration
    (when window-config
      (set-window-configuration window-config))
    (message "Stopped clarifying")))

;;;; Functions

;;;;; Public

(defun org-gtd-clarify-inbox-item (marker window-config)
  "Process item at point through org-gtd.
This function is called through the inbox clarification process.

MARKER must be a marker pointing to an org heading.
WINDOW-CONFIG is the window config to set after clarification finishes."
  (org-gtd-clarify-item marker window-config)
  (setq-local org-gtd-clarify--inbox-p t))

(defun org-gtd-clarify-project-insert-template ()
  "Insert user-provided template under item at point."
  (interactive)
  (let* ((choice (completing-read
                  "Choose a project template to insert: "
                  org-gtd-clarify-project-templates nil t))
         (chosen-template (alist-get
                           choice
                           org-gtd-clarify-project-templates nil nil 'equal)))
    (save-excursion
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1))
      (when (equal (point-min) (point))
        (goto-char 2))
      (org-paste-subtree 2 chosen-template))))

(defun org-gtd-clarify-setup-windows (buffer-or-name)
  "Setup clarifying windows around BUFFER-OR-NAME."
  (let ((buffer (get-buffer buffer-or-name)))
    (set-buffer buffer)
    (display-buffer buffer)
    (delete-other-windows (get-buffer-window buffer))
    (if org-gtd-clarify-show-horizons
        (org-gtd-clarify--display-horizons-window))))

(defun org-gtd-clarify-display-dependency-helper ()
  "Display project dependencies in a helper window for current WIP buffer.
Shows task relationships in format: (depends_on, ...) -> task -> (blocks, ...)
Only displays when `org-gtd-clarify-display-helper-buffer' is non-nil and
the buffer contains more than one task heading."
  (interactive)
  (when (and org-gtd-clarify-display-helper-buffer
             (derived-mode-p 'org-gtd-wip-mode))
    (let ((project-name (org-gtd-clarify--extract-project-name))
          (task-info (org-gtd-clarify--collect-task-information)))
      
      ;; Show helper if we have tasks to display
      (when task-info
        (org-gtd-clarify--create-dependency-helper-window 
         project-name task-info)))))

;;;;; Private

(defun org-gtd-clarify--get-or-create-horizons-view ()
  "Get or create read-only indirect buffer for horizons file."
  (let* ((horizons-buffer (org-gtd--horizons-file))
         (view-buffer-name "*Org GTD Horizons View*")
         (existing-view (get-buffer view-buffer-name)))
    (if (and existing-view (buffer-live-p existing-view))
        existing-view
      (with-current-buffer horizons-buffer
        (let ((view-buffer (make-indirect-buffer
                            horizons-buffer
                            view-buffer-name
                            t)))
          (with-current-buffer view-buffer
            (read-only-mode 1))
          view-buffer)))))

(defun org-gtd-clarify--display-horizons-window ()
  "Display horizons window."
  (let ((horizons-side (or org-gtd-clarify-show-horizons 'right))
        (view-buffer (org-gtd-clarify--get-or-create-horizons-view)))
    (display-buffer view-buffer
                    `(display-buffer-in-side-window . ((side . ,horizons-side))))))

(defun org-gtd-clarify--cleanup-horizons-view ()
  "Kill the horizons view buffer if it exists."
  (let ((view-buffer (get-buffer "*Org GTD Horizons View*")))
    (when (and view-buffer (buffer-live-p view-buffer))
      (kill-buffer view-buffer))))

(defun org-gtd-clarify--extract-project-name ()
  "Extract project name from the first level heading in current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*[[:space:]]+" nil t)
      (org-get-heading t t t t))))

(defun org-gtd-clarify--collect-task-information ()
  "Collect task information from all level-2 headings in current buffer.
Returns a list of (heading id depends-on blocks) for each task."
  (let ((task-info '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\*\\*[[:space:]]" nil t)
        (when (= (org-outline-level) 2)
          (let* ((heading (org-get-heading t t t t))
                 (id (org-entry-get nil "ID"))
                 (depends-on (org-entry-get-multivalued-property nil "ORG_GTD_DEPENDS_ON"))
                 (blocks (org-entry-get-multivalued-property nil "ORG_GTD_BLOCKS")))
            (push (list heading id depends-on blocks) task-info)))))
    (nreverse task-info)))

(defun org-gtd-clarify--create-dependency-helper-window (project-name task-info)
  "Create and display dependency helper window.
PROJECT-NAME is the name of the project.
TASK-INFO is a list of (heading id depends-on blocks) for each task."
  (let ((helper-buffer (get-buffer-create "*Org GTD Project Dependencies*")))
    (with-current-buffer helper-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      
      ;; Build and insert content
      (insert (org-gtd-clarify--format-helper-content project-name task-info))
      
      (setq buffer-read-only t)
      (goto-char (point-min)))
    
    ;; Display the helper buffer in a side window
    (display-buffer helper-buffer
                    '(display-buffer-in-side-window . ((side . right))))))

(defun org-gtd-clarify--format-helper-content (project-name task-info)
  "Format the helper window content with project name and task relationships.
PROJECT-NAME is the name of the project.
TASK-INFO is a list of (heading id depends-on blocks) for each task."
  (let ((content (format "Project name: %s\n\n" (or project-name "Unknown Project")))
        (orphaned '()))
    
    ;; Process each task and build relationship strings
    (dolist (task-data task-info)
      (let* ((heading (nth 0 task-data))
             (_id (nth 1 task-data))
             (depends-on (nth 2 task-data))
             (blocks (nth 3 task-data)))
        (if (or depends-on blocks)
            ;; Task with relationships - format as: (deps) -> task -> (blocks)
            (setq content
                  (concat content
                          (format "(%s) -> %s -> (%s)\n"
                                 (org-gtd-clarify--format-task-list depends-on task-info)
                                 heading
                                 (org-gtd-clarify--format-task-list blocks task-info))))
          ;; Task without relationships (orphaned)
          (push heading orphaned))))
    
    ;; Add orphaned tasks section if any exist
    (when orphaned
      (setq content (concat content "\nOrphaned tasks:\n"))
      (dolist (task orphaned)
        (setq content (concat content (format "%s\n" task)))))
    
    content))

(defun org-gtd-clarify--format-task-list (task-ids task-info)
  "Format a list of TASK-IDS as comma-separated task names using TASK-INFO."
  (if task-ids
      (mapconcat (lambda (id) (org-gtd-clarify--resolve-task-name id task-info))
                 task-ids ", ")
    ""))

(defun org-gtd-clarify--resolve-task-name (task-id task-info)
  "Resolve TASK-ID to task name using TASK-INFO."
  (let ((task-data (cl-find-if (lambda (task) (string= (nth 1 task) task-id)) task-info)))
    (if task-data
        (nth 0 task-data)
      task-id)))

;;;; Footer

(provide 'org-gtd-clarify)

;;; org-gtd-clarify.el ends here
