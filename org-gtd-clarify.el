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
(require 'org-gtd-id)
(require 'org-gtd-wip)
(require 'org-gtd-horizons)
(require 'org-gtd-walk-model)
(require 'org-gtd-walk)

;; Defined in org-gtd-capture (loaded at the org-gtd.el level); declared
;; here to avoid a require cycle while silencing the compiler.
(declare-function org-gtd-inbox-path "org-gtd-capture")

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

(defcustom org-gtd-clarify-show-organize-help nil
  "If non-nil, position to display the organize types help buffer.
The values can be: nil, top, right, left, bottom."
  :group 'org-gtd-clarify
  :options '('right 'top 'left 'bottom 'nil)
  :package-version '(org-gtd . "4.0")
  :type 'symbol)

(defcustom org-gtd-clarify-duplicate-queue-position 'bottom
  "Position for the duplicate queue window during clarification.
When duplicates are created, they appear in a side window at this position.
Values can be: top, right, left, bottom."
  :group 'org-gtd-clarify
  :package-version '(org-gtd . "4.4.0")
  :type '(choice (const :tag "Top" top)
                 (const :tag "Right" right)
                 (const :tag "Left" left)
                 (const :tag "Bottom" bottom)))

;;;; Constants

(defconst org-gtd-clarify-organize-help-content
  "* Quick Action [q]
Do it now (< 2 minutes). Marks DONE and archives immediately.

* Single Action [s]
One-off task to do when ready.
- Marks as NEXT
- Shows in engage view

* Project [p]
Multi-step outcome requiring several tasks.
- Creates task dependencies
- First task(s) marked NEXT, rest TODO
- Auto-advances on completion

* Add to Project [a]
Attach this task to an existing project.

* Calendar [c]
Must happen at a specific date/time.
- Prompts for date
- Shows in agenda at that time

* Delegate [d]
Someone else will do it; you follow up.
- Prompts for person and follow-up date
- Marks as WAIT

* Habit [h]
Recurring action with org-habit tracking.

* Tickler [i]
Remind me to reconsider on a specific date.
- Prompts for review date
- Reappears in agenda then

* Someday/Maybe [y]
Might do eventually, no commitment or date.

* Knowledge [k]
Reference material to file away.

* Trash [t]
Not needed. Deletes the item.
"
  "Content for the organize help buffer showing GTD item types.")

(defconst org-gtd-clarify-organize-help-buffer-name "*Org GTD Organize Help*"
  "Buffer name for the organize types help window.")

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

(defvar-local org-gtd-clarify--continuation nil
  "Function to call after organizing completes.
Set by `org-gtd-clarify-inbox-item' to continue inbox processing.
Called by `org-gtd-organize--call' when non-nil.")

(defvar-local org-gtd-clarify--duplicate-queue nil
  "List of pending duplicate items to clarify after current item.
Each element is a plist with :title and :content keys.")

;;;;; External variables (defined in org-gtd-process.el)

(defvar org-gtd-process--session-active)
(defvar org-gtd-process--pending-inboxes)

;;;;; Keymaps

(defvar org-gtd-clarify-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'org-gtd-clarify-stop)
    (define-key map (kbd "C-c d") #'org-gtd-clarify-duplicate)
    (define-key map (kbd "C-c D") #'org-gtd-clarify-duplicate-exact)
    map)
  "Keymap for `org-gtd-clarify-mode'.")

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

;;;; Modes

;;;###autoload
(define-derived-mode org-gtd-clarify-mode org-mode "GTD-Clarify"
  "Major mode for GTD item clarification.
Derived from `org-mode' and provides keybindings and interface for
clarifying GTD items before organizing them.

\\{org-gtd-clarify-mode-map}"
  :group 'org-gtd-clarify
  (setq-local org-gtd--loading-p t)
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<org-gtd-clarify-mode-map>Clarify item.  \\[org-gtd-organize] to file, \\[org-gtd-clarify-duplicate] to duplicate, \\[org-gtd-clarify-stop] to cancel."))
  ;; Enable auto-save for crash protection (absorbed from wip-mode)
  (auto-save-mode 1)
  ;; Kill buffer hooks for cleanup
  (add-hook 'kill-buffer-query-functions
            #'org-gtd-clarify--kill-buffer-query nil t)
  (add-hook 'kill-buffer-hook
            #'org-gtd-clarify--kill-buffer-cleanup nil t))

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
    (org-gtd-clarify--initialize-buffer-contents source-heading-marker processing-buffer)
    (with-current-buffer processing-buffer
      (unless (derived-mode-p 'org-gtd-clarify-mode)
        (org-gtd-clarify-mode))
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
      (message "There are no Org GTD WIP buffers."))))

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
On the walk-driven inbox path, delegates to
`org-gtd-clarify--stop-walk'; on the one-off clarify path, to
`org-gtd-clarify--stop-one-off'.  In both cases: if the next thing to
handle is a queued duplicate, discard only the current item and move to
it; otherwise tear down and restore the window configuration."
  (interactive)
  (if org-gtd-walk--active
      (org-gtd-clarify--stop-walk)
    (org-gtd-clarify--stop-one-off)))

(defun org-gtd-clarify--stop-walk ()
  "Stop clarifying on the walk-driven inbox path (Task 7).
If the entry immediately after the current one is a queued duplicate,
`org-gtd-walk-advance' (skip the current item, render the duplicate) --
reproducing the old \"discard current, process next queued duplicate.\"
Otherwise abandon the whole walk: `org-gtd-walk-quit' (which releases
the scope lock), tear down the surface and side windows, restore the
window configuration, and report -- reproducing the old \"stop with no
pending duplicates aborts the session.\"

Quits BEFORE killing the surface so the surface's kill-buffer query
sees no active walk and no pending duplicates, silently allowing
teardown -- matching the old stop, which cleared the duplicate queue
before cleanup so the save/discard prompt never fired."
  (let* ((model (plist-get org-gtd-walk--active :model))
         (entries (plist-get model :entries))
         (cursor (plist-get model :cursor))
         (next-token (nth (1+ cursor) entries))
         (next-value (and next-token
                          (cdr (assoc next-token (plist-get model :meta)))))
         ;; A duplicate's meta value is a (:title :content) plist; an
         ;; inbox item's is a live marker (D2/D4a).
         (next-is-duplicate (and next-value (not (markerp next-value)))))
    (if next-is-duplicate
        ;; The refresh of the side window is handled by :render, which
        ;; `org-gtd-walk-advance' runs.
        (org-gtd-walk-advance)
      (let ((window-config org-gtd-clarify--window-config)
            (task-id org-gtd-clarify--clarify-id))
        (org-gtd-walk-quit)
        (org-gtd-clarify--queue-cleanup)
        (org-gtd-clarify--cleanup-horizons-view)
        (when task-id
          (org-gtd-wip--cleanup-temp-file task-id))
        (when window-config
          (set-window-configuration window-config))
        (message "Stopped clarifying")))))

(defun org-gtd-clarify--stop-one-off ()
  "Stop clarifying on the one-off (non-walk) clarify path.
Preserves the pre-walk-engine behavior: if the buffer-local duplicate
queue has items, discard the current item and process the next queued
duplicate; otherwise clean up and restore the window configuration."
  (let ((queue (copy-sequence org-gtd-clarify--duplicate-queue))
        (window-config org-gtd-clarify--window-config)
        (task-id org-gtd-clarify--clarify-id))
    ;; Clear queue on current buffer so kill hooks don't prompt
    (setq org-gtd-clarify--duplicate-queue nil)
    (if queue
        ;; Queue has items — discard current, process next
        (org-gtd-clarify--process-next-queued-item
         queue window-config nil task-id)
      ;; No queue — full cleanup
      (org-gtd-clarify--queue-cleanup)
      (org-gtd-clarify--cleanup-horizons-view)
      (when task-id
        (org-gtd-wip--cleanup-temp-file task-id))
      (when window-config
        (set-window-configuration window-config))
      (message "Stopped clarifying"))))

(defun org-gtd-clarify-duplicate ()
  "Duplicate current item with a new title.
Prompts for a new title, then enqueues the duplicate."
  (interactive)
  (unless (derived-mode-p 'org-gtd-clarify-mode)
    (user-error "Not in a clarify buffer"))
  (let ((content-plist (org-gtd-clarify--get-wip-content)))
    (unless content-plist
      (user-error "Nothing to duplicate"))
    (let* ((default-title (plist-get content-plist :title))
           (new-title (read-string "Duplicate title: " default-title))
           (content (plist-get content-plist :content))
           (updated-content
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (org-mode)
              (org-edit-headline new-title)
              (buffer-string))))
      (org-gtd-clarify--enqueue-duplicate new-title updated-content)
      (message "Duplicated: %s" new-title))))

(defun org-gtd-clarify-duplicate-exact ()
  "Duplicate current item exactly as-is.
Enqueues an exact copy without prompting for changes."
  (interactive)
  (unless (derived-mode-p 'org-gtd-clarify-mode)
    (user-error "Not in a clarify buffer"))
  (let ((content-plist (org-gtd-clarify--get-wip-content)))
    (unless content-plist
      (user-error "Nothing to duplicate"))
    (let ((title (plist-get content-plist :title))
          (content (plist-get content-plist :content)))
      (org-gtd-clarify--enqueue-duplicate title content)
      (message "Duplicated: %s" title))))

;;;; Functions

;;;;; Public

(defun org-gtd-clarify-inbox-item (marker window-config &optional continuation)
  "Process item at point through org-gtd.
This function is called through the inbox clarification process.

MARKER must be a marker pointing to an org heading.
WINDOW-CONFIG is the window config to set after clarification finishes.
CONTINUATION is a function to call after organizing completes (e.g., to
process the next inbox item)."
  (org-gtd-clarify-item marker window-config)
  (setq-local org-gtd-clarify--inbox-p t)
  (setq-local org-gtd-clarify--continuation continuation))

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
             (derived-mode-p 'org-gtd-clarify-mode))
    (let ((proj-name (org-gtd-clarify--extract-project-name))
          (task-info (org-gtd-clarify--collect-task-information)))

      ;; Show helper if we have tasks to display
      (when task-info
        (org-gtd-clarify--create-dependency-helper-window
         proj-name task-info)))))

;;;;; Private

(defun org-gtd-clarify--initialize-buffer-contents (marker buffer)
  "Prepare BUFFER with org heading at MARKER if possible.
If BUFFER is empty, copy org heading at MARKER and paste inside BUFFER,
then remove org-gtd state properties to prepare for fresh organization."
  (with-temp-message ""
    (when (= (buffer-size buffer) 0)
      (let ((last-command nil))
        (org-with-point-at marker
          (org-copy-subtree)))
      (with-current-buffer buffer
        (org-paste-subtree)
        (org-entry-delete (point) org-gtd-timestamp)
        (org-entry-delete (point) (org-gtd-type-property 'delegated :who))
        (org-entry-delete (point) org-gtd-prop-style)
        (org-entry-delete (point) org-gtd-prop-project)))))

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

(defun org-gtd-clarify--get-or-create-organize-help-buffer ()
  "Get or create the organize help buffer with GTD type descriptions."
  (let ((buffer (get-buffer org-gtd-clarify-organize-help-buffer-name)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (with-current-buffer (get-buffer-create org-gtd-clarify-organize-help-buffer-name)
        (erase-buffer)
        (insert org-gtd-clarify-organize-help-content)
        (org-mode)
        (read-only-mode 1)
        (goto-char (point-min))
        (current-buffer)))))

(defun org-gtd-clarify--display-organize-help-window ()
  "Display organize help in a side window."
  (let ((side (or org-gtd-clarify-show-organize-help 'right))
        (buffer (org-gtd-clarify--get-or-create-organize-help-buffer)))
    (display-buffer buffer
                    `(display-buffer-in-side-window . ((side . ,side))))))

(defun org-gtd-clarify-toggle-organize-help ()
  "Toggle the organize types help window."
  (interactive)
  (let ((window (get-buffer-window org-gtd-clarify-organize-help-buffer-name)))
    (if window
        (quit-window nil window)
      (org-gtd-clarify--display-organize-help-window))))

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

(defun org-gtd-clarify--create-dependency-helper-window (proj-name task-info)
  "Create and display dependency helper window.
PROJ-NAME is the name of the project.
TASK-INFO is a list of (heading id depends-on blocks) for each task."
  (let ((helper-buffer (get-buffer-create "*Org GTD Project Dependencies*")))
    (with-current-buffer helper-buffer
      (setq buffer-read-only nil)
      (erase-buffer)

      ;; Build and insert content
      (insert (org-gtd-clarify--format-helper-content proj-name task-info))
      
      (setq buffer-read-only t)
      (goto-char (point-min)))
    
    ;; Display the helper buffer in a side window
    (display-buffer helper-buffer
                    '(display-buffer-in-side-window . ((side . right))))))

(defun org-gtd-clarify--format-helper-content (proj-name task-info)
  "Format the helper window content with project name and task relationships.
PROJ-NAME is the name of the project.
TASK-INFO is a list of (heading id depends-on blocks) for each task."
  (let ((content (format "Project name: %s\n\n" (or proj-name "Unknown Project")))
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

;;;; Duplicate Enqueue (D4a + D4c)

(defun org-gtd-clarify--enqueue-duplicate (title content)
  "Enqueue a duplicate with TITLE and CONTENT for later clarification.

On the walk-driven path (`org-gtd-walk--active' non-nil in the current
buffer -- i.e. duplicating while processing the inbox), stores
\(:title :content) in the active walk model's `:meta' under a fresh
synthetic token and inserts that token at
`org-gtd-clarify-duplicate-queue-position' (D4a + D4c): a direct model
mutation, no render.  The subsequent organize's advance (see
`org-gtd-organize--call') renders it in turn via the inbox walk's
generic `:render' (`org-gtd-inbox-walk--render', which dispatches on
the meta shape) -- so this function does not need to know how to
render a duplicate itself.

On the one-off path (no active walk -- duplicating during a one-off
`org-gtd-clarify-item'), falls back to the legacy buffer-local queue
and its side-window display, exactly as before the walk engine.

Both paths refresh the `*Org GTD Duplicate Queue*' side window via
`org-gtd-clarify--queue-display', which now reads whichever store is
active (D6a)."
  (if org-gtd-walk--active
      (let* ((token (format "inbox-dup-%s" (org-id-uuid)))
             (model (plist-get org-gtd-walk--active :model))
             (seeded (list :entries (plist-get model :entries)
                          :cursor (plist-get model :cursor)
                          :meta (cons (cons token (list :title title :content content))
                                     (plist-get model :meta)))))
        (setf (plist-get org-gtd-walk--active :model)
              (org-gtd-walk-model-enqueue
               seeded token (org-gtd-clarify--duplicate-enqueue-position)))
        (org-gtd-clarify--queue-display))
    (org-gtd-clarify--queue-add title content)
    (org-gtd-clarify--queue-display)))

(defun org-gtd-clarify--duplicate-enqueue-position ()
  "Return the `org-gtd-walk-model-enqueue' position for a new duplicate.
`org-gtd-clarify-duplicate-queue-position' historically names the side
window's SIDE (top/right/left/bottom); only `top' and `bottom' are
meaningful as walk-model enqueue positions (`org-gtd-walk-model-enqueue'
rejects the rest).  Map `top' to `top' (handled next) and every other
value -- the default `bottom' and the legacy `left'/`right' window
sides -- to `bottom' (handled after the remaining items), so a user
with a left/right side window never trips the enqueue validator."
  (if (eq org-gtd-clarify-duplicate-queue-position 'top) 'top 'bottom))

;;;;; Pending-duplicate accessors (D6a)

(defun org-gtd-clarify--walk-pending-duplicates ()
  "Return the remaining duplicate plists from the active walk model.
Each is a (:title :content) plist, in queue order, for the duplicate
tokens strictly after the model's cursor (the current item, whatever it
is, is being clarified now and is not \"pending\").  nil when no walk is
active in the current buffer."
  (when org-gtd-walk--active
    (let* ((model (plist-get org-gtd-walk--active :model))
           (entries (plist-get model :entries))
           (cursor (plist-get model :cursor))
           (meta (plist-get model :meta))
           (remaining (nthcdr (1+ cursor) entries))
           dups)
      (dolist (token remaining)
        (let ((value (cdr (assoc token meta))))
          ;; A duplicate's meta value is a (:title :content) plist; an
          ;; inbox item's is a live marker (D2/D4a).
          (when (and value (not (markerp value)))
            (push value dups))))
      (nreverse dups))))

(defun org-gtd-clarify--pending-duplicates ()
  "Return the pending duplicate plists for the current clarify context.
Walk-driven inbox clarify reads them from the active walk model
(`org-gtd-clarify--walk-pending-duplicates', D6a); one-off clarify
reads the legacy buffer-local `org-gtd-clarify--duplicate-queue'.  Both
shapes are (:title :content) plists, so all downstream display/save/
kill-safety code is store-agnostic."
  (if org-gtd-walk--active
      (org-gtd-clarify--walk-pending-duplicates)
    org-gtd-clarify--duplicate-queue))

;;;; Duplicate Queue Functions

;;;;; Queue Predicates

(defun org-gtd-clarify--queue-empty-p ()
  "Return t when there are no pending duplicates in the current context.
Reads whichever store is active (walk model or legacy queue) via
`org-gtd-clarify--pending-duplicates'."
  (null (org-gtd-clarify--pending-duplicates)))

;;;;; Queue Operations

(defun org-gtd-clarify--queue-add (title content)
  "Add item with TITLE and CONTENT to end of duplicate queue."
  (setq org-gtd-clarify--duplicate-queue
        (append org-gtd-clarify--duplicate-queue
                (list (list :title title :content content)))))

(defun org-gtd-clarify--queue-pop ()
  "Remove and return first item from duplicate queue.
Returns nil if queue is empty."
  (when org-gtd-clarify--duplicate-queue
    (pop org-gtd-clarify--duplicate-queue)))

;;;;; Queue Display

(defconst org-gtd-clarify--queue-buffer-name "*Org GTD Duplicate Queue*"
  "Buffer name for the duplicate queue window.")

(defun org-gtd-clarify--queue-display ()
  "Refresh the pending-duplicates side window (D6a).
Reads the pending duplicates for the current context (walk model or
legacy queue) via `org-gtd-clarify--pending-duplicates'.  When none
remain, tears the side window down (`org-gtd-clarify--queue-cleanup')
rather than showing an empty pane -- so on a plain inbox item with no
duplicates the window simply is not shown, matching the pre-engine
\"window appears only when duplicates are pending\" behavior."
  (let ((queue (org-gtd-clarify--pending-duplicates)))
    (if (null queue)
        (org-gtd-clarify--queue-cleanup)
      (let ((buffer (get-buffer-create org-gtd-clarify--queue-buffer-name)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Pending (%d):\n" (length queue)))
            (let ((idx 1))
              (dolist (item queue)
                (insert (format "  %d. %s\n" idx (plist-get item :title)))
                (setq idx (1+ idx))))
            (goto-char (point-min)))
          (setq buffer-read-only t))
        (display-buffer buffer
                        `(display-buffer-in-side-window
                          . ((side . ,org-gtd-clarify-duplicate-queue-position))))))))

(defun org-gtd-clarify--queue-cleanup ()
  "Close the queue window and kill the queue buffer."
  (when-let ((buffer (get-buffer org-gtd-clarify--queue-buffer-name)))
    (when-let ((window (get-buffer-window buffer)))
      (quit-window nil window))
    (kill-buffer buffer))
  (setq org-gtd-clarify--duplicate-queue nil))

;;;;; Queue Processing

(defun org-gtd-clarify--process-next-queued-item (queue window-config continuation old-task-id)
  "Process the next item from the one-off duplicate QUEUE.
WINDOW-CONFIG is restored after all items are processed.
CONTINUATION is called after the queue is empty.
OLD-TASK-ID is the clarify-id of the buffer being reused.

This drives the pre-walk-engine one-off clarify duplicate path only;
the walk-driven inbox path advances through the walk model instead (see
`org-gtd-organize--call' and `org-gtd-clarify--stop-walk')."
  (let ((item (pop queue)))
    (if item
        (let* ((content (plist-get item :content))
               (temp-file (gethash old-task-id org-gtd-wip--temp-files))
               (processing-buffer (if temp-file
                                      (find-buffer-visiting temp-file)
                                    (org-gtd-wip--get-buffer (org-id-new)))))
          (with-current-buffer processing-buffer
            ;; Clear buffer and insert new content
            (let ((inhibit-read-only t))
              (erase-buffer))
            (insert content)
            (goto-char (point-min))
            ;; Delete stale ID from original, generate fresh one
            (org-entry-delete nil "ID")
            (let ((new-id (org-gtd-id-get-create)))
              (org-gtd-wip--rekey old-task-id new-id)
              ;; Ensure mode is active
              (unless (derived-mode-p 'org-gtd-clarify-mode)
                (org-gtd-clarify-mode))
              ;; Update buffer-local state
              (setq-local org-gtd-clarify--window-config window-config
                          org-gtd-clarify--clarify-id new-id
                          org-gtd-clarify--continuation continuation
                          org-gtd-clarify--source-heading-marker nil
                          org-gtd-clarify--skip-refile nil
                          org-gtd-clarify--duplicate-queue queue))
            ;; Update queue display or cleanup if empty
            (if (org-gtd-clarify--queue-empty-p)
                (org-gtd-clarify--queue-cleanup)
              (org-gtd-clarify--queue-display))))
      ;; No more items - cleanup and continue
      (when old-task-id
        (org-gtd-wip--cleanup-temp-file old-task-id))
      (org-gtd-clarify--queue-cleanup)
      (message "All duplicates processed")
      (when window-config
        (set-window-configuration window-config))
      (when continuation
        (funcall continuation)))))

;;;;; Content Extraction

(defun org-gtd-clarify--get-wip-content ()
  "Extract title and full content from current WIP buffer.
Returns plist with :title and :content keys, or nil if buffer is empty."
  (save-excursion
    (goto-char (point-min))
    (when (org-before-first-heading-p)
      (org-next-visible-heading 1))
    (when (org-at-heading-p)
      (let ((title (org-get-heading t t t t))
            (content (buffer-substring-no-properties
                      (point-min) (point-max))))
        (when (and title (not (string-empty-p (string-trim title))))
          (list :title title :content content))))))

;;;;; Queue Persistence

(defun org-gtd-clarify--queue-save-to-inbox ()
  "Save the current context's pending duplicates to the inbox.
Reads them via `org-gtd-clarify--pending-duplicates' (walk model or
legacy queue, D6a)."
  (let ((inbox-file (org-gtd-inbox-path))
        (queue (org-gtd-clarify--pending-duplicates)))
    (with-current-buffer (find-file-noselect inbox-file)
      (goto-char (point-max))
      (dolist (item queue)
        (insert "\n" (plist-get item :content)))
      (save-buffer))))

(defun org-gtd-clarify--prompt-queue-action ()
  "Prompt user for action on pending duplicates.
Returns \\='discard, \\='save, or \\='cancel."
  (let* ((dups (org-gtd-clarify--pending-duplicates))
         (count (length dups))
         (titles (mapcar (lambda (item) (plist-get item :title)) dups))
         (prompt (format "%d pending duplicate%s:\n  - %s\n[d]iscard all  [s]ave to inbox  [c]ancel: "
                         count
                         (if (= count 1) "" "s")
                         (string-join titles "\n  - "))))
    (pcase (read-char-choice prompt '(?d ?s ?c))
      (?d 'discard)
      (?s 'save)
      (?c 'cancel))))

;;;;; Kill-Emacs Safety

(defun org-gtd-clarify--buffer-has-pending-duplicates-p ()
  "Return non-nil when the current buffer holds pending duplicates.
Covers both the walk-driven surface (active walk with remaining
duplicate entries) and a one-off clarify buffer with a legacy queue."
  (or (and org-gtd-walk--active
           (org-gtd-clarify--walk-pending-duplicates))
      (and (derived-mode-p 'org-gtd-clarify-mode)
           (bound-and-true-p org-gtd-clarify--duplicate-queue))))

(defun org-gtd-clarify--pending-duplicates-all-buffers ()
  "Return all pending duplicates across every clarify context (D6a).
Collects the active walk model's remaining duplicates (inbox clarify)
and any legacy buffer-local queues (one-off clarify)."
  (let (all-duplicates)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (org-gtd-clarify--buffer-has-pending-duplicates-p)
          (setq all-duplicates
                (append all-duplicates (org-gtd-clarify--pending-duplicates))))))
    all-duplicates))

(defun org-gtd-clarify--save-all-pending-duplicates ()
  "Save pending duplicates from every clarify context to the inbox (D6a)."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (org-gtd-clarify--buffer-has-pending-duplicates-p)
        (org-gtd-clarify--queue-save-to-inbox)))))

(defun org-gtd-clarify--kill-emacs-query ()
  "Prompt about pending duplicates before Emacs exits.
Added to `kill-emacs-query-functions'."
  (let ((duplicates (org-gtd-clarify--pending-duplicates-all-buffers)))
    (if (null duplicates)
        t  ; No duplicates, allow exit
      ;; Prompt user
      (let* ((count (length duplicates))
             (titles (mapcar (lambda (item) (plist-get item :title)) duplicates))
             (prompt (format "%d pending duplicate%s will be lost:\n  - %s\n[d]iscard  [s]ave to inbox  [c]ancel exit: "
                             count
                             (if (= count 1) "" "s")
                             (string-join titles "\n  - "))))
        (pcase (read-char-choice prompt '(?d ?s ?c))
          (?d t)  ; Discard, allow exit
          (?s (org-gtd-clarify--save-all-pending-duplicates) t)  ; Save, allow exit
          (?c nil))))))  ; Cancel, abort exit

(add-hook 'kill-emacs-query-functions #'org-gtd-clarify--kill-emacs-query)

;;;;; Kill Buffer Safety

(defun org-gtd-clarify--kill-buffer-query ()
  "Query before killing clarify buffer if duplicates are pending.
Returns t to allow kill, nil to abort.
Added to `kill-buffer-query-functions' buffer-locally."
  (if (org-gtd-clarify--queue-empty-p)
      t  ; No duplicates, allow kill
    ;; Prompt user - reuse existing prompt logic
    (pcase (org-gtd-clarify--prompt-queue-action)
      ('save (org-gtd-clarify--queue-save-to-inbox) t)
      ('discard t)
      ('cancel nil))))

(defun org-gtd-clarify--other-clarify-buffers-exist-p ()
  "Return t if other clarify buffers exist besides current one."
  (let ((current (current-buffer)))
    (cl-some (lambda (buf)
               (and (not (eq buf current))
                    (buffer-live-p buf)
                    (with-current-buffer buf
                      (derived-mode-p 'org-gtd-clarify-mode))))
             (buffer-list))))

(defun org-gtd-clarify--kill-side-window (buffer-name)
  "Kill side window buffer BUFFER-NAME if it exists."
  (when-let ((buffer (get-buffer buffer-name)))
    (when-let ((window (get-buffer-window buffer)))
      (quit-window nil window))
    (kill-buffer buffer)))

(defun org-gtd-clarify--kill-buffer-cleanup ()
  "Clean up side windows when clarify buffer is killed.
Only cleans up global side windows if no other clarify buffers exist.
Added to `kill-buffer-hook' buffer-locally."
  (unless (org-gtd-clarify--other-clarify-buffers-exist-p)
    ;; Clean up all side windows
    (org-gtd-clarify--kill-side-window org-gtd-clarify--queue-buffer-name)
    (org-gtd-clarify--kill-side-window org-gtd-clarify-organize-help-buffer-name)
    (org-gtd-clarify--kill-side-window "*Org GTD Horizons View*")
    (org-gtd-clarify--kill-side-window "*Org GTD Project Dependencies*"))
  ;; Delete WIP temp file directly (don't call org-gtd-wip--cleanup-temp-file
  ;; which would try to kill-buffer again, causing infinite recursion)
  (when org-gtd-clarify--clarify-id
    (when-let ((temp-file (gethash org-gtd-clarify--clarify-id org-gtd-wip--temp-files)))
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (remhash org-gtd-clarify--clarify-id org-gtd-wip--temp-files))))

;;;; Footer

(provide 'org-gtd-clarify)

;;; org-gtd-clarify.el ends here
