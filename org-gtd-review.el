;;; org-gtd-review.el --- Guided review sessions for org-gtd -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2026 Aldric Giacomoni

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
;; Guided, profile-driven review sessions — Weekly Review by default.
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'f)
(require 'seq)
(require 'org-gtd-core)
(require 'org-gtd-files)
(require 'org-gtd-checklist)
(require 'org-gtd-create)
(require 'org-gtd-capture)

;;;; External Function Declarations

;; Evil functions (only called inside with-eval-after-load 'evil)
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-emacs-state "evil-states")

;;;; Customization

(defcustom org-gtd-review-profiles
  '(("Weekly Review"
     ("Get Clear"
      (:title "Gather loose materials"
       :type prompt
       :instruction "Collect loose papers, receipts, and notes.  Capture each one into the inbox.")
      (:title "Mind sweep"
       :type checklist
       :checklist "Weekly Review triggers"
       :instruction "Walk each trigger.  Press c to capture whatever it shakes loose.")
      (:title "Inbox to zero"
       :type command
       :command org-gtd-process-inbox
       :instruction "Process every inbox item.  Come back here and press n when the inbox is empty."))
     ("Get Current"
      (:title "Review missed items"
       :type view
       :view org-gtd-reflect-missed-items
       :instruction "Reschedule, complete, or cancel anything that slipped.")
      (:title "Review Waiting-For"
       :type view
       :view org-gtd-reflect-upcoming-delegated
       :instruction "Nudge, close, or re-delegate each delegated item.")
      (:title "Review next actions"
       :type view
       :view org-gtd-show-all-next
       :instruction "Mark done what is done; check these still feel current.")
      (:title "Review stuck projects"
       :type view
       :view org-gtd-reflect-stuck-projects
       :instruction "Give every active project a next action."))
     ("Get Creative"
      (:title "Review Someday/Maybe"
       :type view
       :view org-gtd-reflect-someday-maybe
       :instruction "Reactivate anything whose time has come.")
      (:title "Capture new ideas"
       :type prompt
       :instruction "Any creative, risky, or fun ideas?  Capture them."))))
  "Alist of guided review profiles.
Each entry is (PROFILE-NAME . PHASES); each phase is
\(PHASE-NAME . STEPS); each step is a plist with :title, :type
\(one of `prompt', `command', `view', `checklist'), an optional
:instruction, and the type-specific key :command, :view, or
:checklist (a template name in the checklists file)."
  :group 'org-gtd
  :type 'sexp)

;;;; Variables

(defvar org-gtd-review--state nil
  "State plist of the active review session.
Keys: :profile (name string), :phase (index), :step (index),
:acted (step-local flag), :walk-items, :walk-pos, :done, :skipped.")

(defvar org-gtd-review--window-config nil
  "Window configuration to restore when the session ends.")

(defconst org-gtd-review--buffer-name "*GTD Review*")

;;;; Accessors

(defun org-gtd-review--phases ()
  "Return the phases of the active profile."
  (cdr (assoc (plist-get org-gtd-review--state :profile)
              org-gtd-review-profiles)))

(defun org-gtd-review--current-phase ()
  "Return the (NAME . STEPS) phase the session is in."
  (nth (plist-get org-gtd-review--state :phase) (org-gtd-review--phases)))

(defun org-gtd-review--current-step ()
  "Return the step plist the session is on."
  (nth (plist-get org-gtd-review--state :step)
       (cdr (org-gtd-review--current-phase))))

;;;; Profile Validation

(defun org-gtd-review--check-profile (profile)
  "Signal a `user-error' when PROFILE is malformed.
PROFILE is an entry of `org-gtd-review-profiles', that is
\(PROFILE-NAME . PHASES).  Explain what a well-formed phase or
step looks like so the error teaches the fix."
  (let ((name (car profile))
        (phases (cdr profile)))
    (unless phases
      (user-error
       "Review profile '%s' has no phases — give it at least one (PHASE-NAME STEP...) list"
       name))
    (dolist (phase phases)
      (unless (and (proper-list-p phase) (stringp (car phase)))
        (user-error
         "Review profile '%s': phase %S should be a list starting with a name string, like (\"Get Clear\" STEP...)"
         name phase))
      (unless (cdr phase)
        (user-error
         "Review profile '%s': phase '%s' has no steps — give it at least one step plist"
         name (car phase)))
      (dolist (step (cdr phase))
        (unless (and (listp step) (keywordp (car-safe step)))
          (user-error
           "Review profile '%s', phase '%s': step %S should be a plist like (:title \"...\" :type prompt)"
           name (car phase) step))
        (unless (plist-get step :title)
          (user-error
           "Review profile '%s', phase '%s': a step is missing :title — every step needs one"
           name (car phase)))
        (unless (plist-get step :type)
          (user-error
           "Review profile '%s', phase '%s': step \"%s\" is missing :type — one of prompt, command, view, or checklist"
           name (car phase) (plist-get step :title)))
        (when (and (eq (plist-get step :type) 'command)
                   (not (plist-get step :command)))
          (user-error
           "Review profile '%s', phase '%s': command step \"%s\" is missing :command — name the command to run, like :command org-gtd-process-inbox"
           name (car phase) (plist-get step :title)))
        (when (and (eq (plist-get step :type) 'view)
                   (not (plist-get step :view)))
          (user-error
           "Review profile '%s', phase '%s': view step \"%s\" is missing :view — name the command that shows the view, like :view org-gtd-engage"
           name (car phase) (plist-get step :title)))
        (when (and (eq (plist-get step :type) 'checklist)
                   (not (plist-get step :checklist)))
          (user-error
           "Review profile '%s', phase '%s': checklist step \"%s\" is missing :checklist — name a template from your checklists file, like :checklist \"Weekly Review triggers\""
           name (car phase) (plist-get step :title)))))))

;;;; Checkpoint and Resume Persistence

(defun org-gtd-review--state-file ()
  "Return the path of the session checkpoint file.
A single slot with no locking: concurrent Emacs sessions writing
checkpoints race, and the last write wins."
  (f-join org-gtd-directory "review-state.eld"))

(defun org-gtd-review--save-state ()
  "Serialize the session state to `org-gtd-review--state-file'."
  (with-temp-file (org-gtd-review--state-file)
    (let ((print-length nil) (print-level nil))
      (prin1 org-gtd-review--state (current-buffer)))))

(defun org-gtd-review--load-state ()
  "Read a saved session state, or nil."
  (let ((file (org-gtd-review--state-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (ignore-errors (read (current-buffer)))))))

(defun org-gtd-review--delete-state-file ()
  "Remove the saved session state, if any."
  (let ((file (org-gtd-review--state-file)))
    (when (file-exists-p file) (delete-file file))))

(defun org-gtd-review--state-valid-p (state)
  "Non-nil when STATE still fits `org-gtd-review-profiles'.
Also rejects internally incoherent states — negative indices,
non-integer tallies, a walk position outside the walk items — so a
corrupted checkpoint falls back to a fresh session instead of
crashing mid-render."
  (when-let* ((profile (assoc (plist-get state :profile)
                              org-gtd-review-profiles))
              (phases (cdr profile))
              (p (plist-get state :phase))
              (s (plist-get state :step)))
    (let ((walk-items (plist-get state :walk-items))
          (walk-pos (plist-get state :walk-pos)))
      (and (integerp p) (>= p 0) (< p (length phases))
           (integerp s) (>= s 0) (< s (length (cdr (nth p phases))))
           (integerp (plist-get state :done))
           (integerp (plist-get state :skipped))
           (listp walk-items)
           (or (null walk-items)
               (and (integerp walk-pos)
                    (>= walk-pos 0)
                    (< walk-pos (length walk-items))))))))

;;;; Keymap and Mode

(defvar org-gtd-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'org-gtd-review-next)
    (define-key map (kbd "s") #'org-gtd-review-skip)
    (define-key map (kbd "c") #'org-gtd-review-capture)
    (define-key map (kbd "p") #'org-gtd-review-pause)
    (define-key map (kbd "q") #'org-gtd-review-quit)
    map)
  "Keymap for `org-gtd-review-mode'.")

(define-derived-mode org-gtd-review-mode special-mode "GTD-Review"
  "Major mode for the guided review session console.

\\{org-gtd-review-mode-map}"
  :group 'org-gtd
  (add-hook 'kill-buffer-hook #'org-gtd-review--on-buffer-kill nil t))

;; When evil-mode is loaded, start review-mode in emacs state.
;; We use both evil-set-initial-state AND a mode hook for robustness:
;; - evil-set-initial-state: handles new buffers entering this mode
;; - mode hook: forces emacs state even if evil-collection or user config
;;   has set a different state
(with-eval-after-load 'evil
  (evil-set-initial-state 'org-gtd-review-mode 'emacs)
  (add-hook 'org-gtd-review-mode-hook #'evil-emacs-state))

;;;; Rendering

(defun org-gtd-review--header-line ()
  "Compute the header line advertising the session keys.
Capture is always available: any step can shake something loose."
  "[n] Do/advance  [s] Skip  [c] Capture  [p] Pause  [q] Quit")

(defun org-gtd-review--phase-tracker ()
  "Render the phase tracker line."
  (let ((current (plist-get org-gtd-review--state :phase)))
    (mapconcat
     (lambda (pair)
       (let ((i (car pair)) (name (car (cdr pair))))
         (cond ((< i current) (format "[✓ %s]" name))
               ((= i current) (format "▸ %s ◂" name))
               (t (format "[ %s ]" name)))))
     (seq-map-indexed (lambda (ph i) (cons i ph)) (org-gtd-review--phases))
     "  ")))

(defun org-gtd-review--render ()
  "Render the session buffer from `org-gtd-review--state'."
  (let* ((state org-gtd-review--state)
         (phase (org-gtd-review--current-phase))
         (steps (cdr phase))
         (step (org-gtd-review--current-step)))
    (with-current-buffer (get-buffer-create org-gtd-review--buffer-name)
      (unless (derived-mode-p 'org-gtd-review-mode) (org-gtd-review-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n" (plist-get state :profile)))
        (insert (org-gtd-review--phase-tracker) "\n\n")
        (insert (format "%s — step %d/%d\n\n"
                        (car phase)
                        (1+ (plist-get state :step))
                        (length steps)))
        (insert (format "  %s\n" (plist-get step :title)))
        (when-let ((instr (plist-get step :instruction)))
          (insert (format "\n  %s\n" instr)))
        (when (and (eq (plist-get step :type) 'checklist)
                   (plist-get state :walk-items))
          (let ((items (plist-get state :walk-items))
                (pos (plist-get state :walk-pos)))
            (insert (format "\n    → %s   (%d/%d)\n"
                            (nth pos items) (1+ pos) (length items)))))
        (goto-char (point-min)))
      (setq header-line-format (org-gtd-review--header-line))
      (pop-to-buffer (current-buffer)))))

;;;; Step Advancement

(defun org-gtd-review--complete-step (&optional skipped)
  "Advance past the current step, tallying SKIPPED or done.
Checkpoints the new position to disk at every step boundary, so a
crash or killed buffer resumes where the user left off."
  (let ((state org-gtd-review--state)
        (counter (if skipped :skipped :done)))
    (plist-put state counter (1+ (plist-get state counter)))
    (plist-put state :acted nil)
    (plist-put state :walk-items nil)
    (plist-put state :walk-pos 0)
    (let ((steps (cdr (org-gtd-review--current-phase)))
          (next-step (1+ (plist-get state :step))))
      (if (< next-step (length steps))
          (progn (plist-put state :step next-step)
                 (org-gtd-review--save-state)
                 (org-gtd-review--render))
        (let ((phases (org-gtd-review--phases))
              (next-phase (1+ (plist-get state :phase))))
          (if (< next-phase (length phases))
              (progn
                (message "%s complete — on to %s."
                         (car (nth (plist-get state :phase) phases))
                         (car (nth next-phase phases)))
                (plist-put state :phase next-phase)
                (plist-put state :step 0)
                (org-gtd-review--save-state)
                (org-gtd-review--render))
            (org-gtd-review--finish)))))))

(defun org-gtd-review--reset-session ()
  "Clear the session state and restore the saved window configuration."
  (setq org-gtd-review--state nil)
  (when org-gtd-review--window-config
    (set-window-configuration org-gtd-review--window-config)
    (setq org-gtd-review--window-config nil)))

(defun org-gtd-review--on-buffer-kill ()
  "End the session when its buffer is killed out from under it.
Buffer-local `kill-buffer-hook' for `org-gtd-review-mode'.  No-op
when the session already ended, so `org-gtd-review--teardown' does
not recurse through the kill it performs itself.

Deliberately leaves the checkpoint file alone: the session saves at
every step boundary and walk advance, so the file already holds the
latest position.  A stray \\[kill-buffer] — or a crash — therefore
loses nothing: the next `org-gtd-review' offers to resume right
where the user was."
  (when org-gtd-review--state
    (org-gtd-review--reset-session)))

(defun org-gtd-review--teardown ()
  "Kill the session buffer, clear state, restore windows."
  (org-gtd-review--reset-session)
  (when (get-buffer org-gtd-review--buffer-name)
    (kill-buffer org-gtd-review--buffer-name)))

(defun org-gtd-review--finish ()
  "Complete the session: report, clean up.
Every completion path funnels through here, so this is where the
checkpoint file is removed."
  (org-gtd-review--delete-state-file)
  (let ((done (plist-get org-gtd-review--state :done))
        (skipped (plist-get org-gtd-review--state :skipped)))
    (org-gtd-review--teardown)
    (message (concat "Review complete: %d steps done, %d skipped.  "
                     "Tip: M-x org-gtd-review-schedule puts this on your calendar.")
             done skipped)))

;;;; Commands

(defun org-gtd-review--walk-next (step)
  "Advance the checklist walk for STEP, loading it on first call.
Every walk-position change is checkpointed to disk, so resuming
lands on the item the user was looking at."
  (let ((state org-gtd-review--state))
    (if (not (plist-get state :acted))
        (let ((items (org-gtd-checklist--items (plist-get step :checklist))))
          (if (null items)
              (progn
                (message "Nothing in checklist '%s' — moving on.  (Edit %s to add items.)"
                         (plist-get step :checklist)
                         (org-gtd-checklist--file-path))
                (org-gtd-review--complete-step))
            (plist-put state :acted t)
            (plist-put state :walk-items items)
            (plist-put state :walk-pos 0)
            (org-gtd-review--save-state)
            (org-gtd-review--render)))
      (let ((next (1+ (plist-get state :walk-pos))))
        (if (< next (length (plist-get state :walk-items)))
            (progn (plist-put state :walk-pos next)
                   (org-gtd-review--save-state)
                   (org-gtd-review--render))
          (org-gtd-review--complete-step))))))

(defun org-gtd-review-next ()
  "Do the current step, or advance past it."
  (interactive)
  (let* ((step (org-gtd-review--current-step))
         (type (plist-get step :type)))
    (pcase type
      ('prompt (org-gtd-review--complete-step))
      ;; Command/view steps set :acted only AFTER the call returns, so a
      ;; signaling command leaves the step un-acted and n retries it.
      ;; Deliberate: s after the first n tallies :skipped — the user
      ;; declined to confirm completion.
      ('command
       (if (plist-get org-gtd-review--state :acted)
           (org-gtd-review--complete-step)
         (call-interactively (plist-get step :command))
         (plist-put org-gtd-review--state :acted t)))
      ('view
       (if (plist-get org-gtd-review--state :acted)
           (org-gtd-review--complete-step)
         (save-selected-window
           (call-interactively (plist-get step :view)))
         (plist-put org-gtd-review--state :acted t)))
      ('checklist (org-gtd-review--walk-next step))
      (_
       (message "Step type '%s' is unknown — check org-gtd-review-profiles; skipping this step" type)
       (org-gtd-review--complete-step t)))))

(defun org-gtd-review-skip ()
  "Skip the current step for this run only."
  (interactive)
  (org-gtd-review--complete-step t))

(defun org-gtd-review-capture ()
  "Capture something to the inbox mid-review."
  (interactive)
  (call-interactively #'org-gtd-capture))

(defun org-gtd-review-pause ()
  "Pause the session; `org-gtd-review' resumes it later.
Guarded against running with no active session: saving then would
write an empty state over an existing checkpoint."
  (interactive)
  (unless org-gtd-review--state
    (user-error "No review session is active"))
  (org-gtd-review--save-state)
  (org-gtd-review--teardown)
  (message "Review paused — run M-x org-gtd-review to resume."))

(defun org-gtd-review-quit ()
  "Quit the session, offering to keep or abandon progress.
Guarded against running with no active session: abandoning then
would delete an existing checkpoint."
  (interactive)
  (unless org-gtd-review--state
    (user-error "No review session is active"))
  (if (y-or-n-p "Keep progress to resume later? ")
      (org-gtd-review-pause)
    (org-gtd-review--delete-state-file)
    (org-gtd-review--teardown)
    (message "Review abandoned.")))

;;;; Entry Point

;;;###autoload
(defun org-gtd-review (&optional profile-name)
  "Run a guided review session, resuming a checkpointed one when offered.
With more than one profile in `org-gtd-review-profiles', prompt;
PROFILE-NAME selects one non-interactively.  An explicit
PROFILE-NAME that differs from the checkpointed profile skips the
resume offer and starts that profile fresh."
  (interactive)
  (when org-gtd-review--state
    (user-error
     "A review session is already active — finish it or press q in %s first"
     org-gtd-review--buffer-name))
  (unless org-gtd-review-profiles
    (user-error
     "No review profiles configured — see `org-gtd-review-profiles'"))
  (let ((saved (org-gtd-review--load-state)))
    (cond
     ((null saved)
      (org-gtd-review--start-fresh profile-name))
     ((not (org-gtd-review--state-valid-p saved))
      (org-gtd-review--delete-state-file)
      (message "Saved review no longer matches your profiles — starting over.")
      (org-gtd-review--start-fresh profile-name))
     ((and profile-name
           (not (equal profile-name (plist-get saved :profile))))
      ;; Asked for a different profile by name: no resume offer.  No
      ;; eager delete either — the new session's first checkpoint
      ;; claims the slot, so aborting the start keeps the old save.
      (org-gtd-review--start-fresh profile-name))
     ((y-or-n-p (format "Resume paused '%s' review? "
                        (plist-get saved :profile)))
      (org-gtd-review--resume saved profile-name))
     (t
      ;; Declined.  Deleting here would destroy the save if the user
      ;; then quits the profile picker; the fresh session's first
      ;; checkpoint overwrites it instead, and nothing between the
      ;; decline and that overwrite reads the file.
      (org-gtd-review--start-fresh profile-name)))))

(defun org-gtd-review--resume (saved profile-name)
  "Resume the SAVED session after re-validating its profile.
The profile may have been edited into an invalid shape while the
session was paused; in that case surface the teaching error, drop
the checkpoint, and start over — PROFILE-NAME seeds the fresh
start, as in the invalid-state branch of `org-gtd-review'."
  (let ((problem
         (condition-case err
             (prog1 nil
               (org-gtd-review--check-profile
                (assoc (plist-get saved :profile) org-gtd-review-profiles)))
           (user-error err))))
    (if (null problem)
        (org-gtd-review--begin-session saved)
      (org-gtd-review--delete-state-file)
      (message "Saved review's profile is no longer valid — starting over.")
      (org-gtd-review--start-fresh profile-name))))

(defun org-gtd-review--start-fresh (profile-name)
  "Start a new session for PROFILE-NAME (prompting when nil)."
  (let* ((names (mapcar #'car org-gtd-review-profiles))
         (name (or profile-name
                   (if (cdr names)
                       (completing-read "Review profile: " names nil t)
                     (car names)))))
    (unless (assoc name org-gtd-review-profiles)
      (user-error "No review profile named '%s'" name))
    (org-gtd-review--check-profile (assoc name org-gtd-review-profiles))
    (org-gtd-review--begin-session
     (list :profile name :phase 0 :step 0 :acted nil
           :walk-items nil :walk-pos 0 :done 0 :skipped 0))))

(defun org-gtd-review--begin-session (state)
  "Install STATE as the live session, render, and checkpoint it.
Snapshots the current window configuration for restore at session
end.  A resumed session re-snapshots here rather than restoring a
saved one: a live window configuration cannot be serialized to the
state file.  The immediate checkpoint means a fresh session claims
the single save slot as soon as it successfully boots."
  (setq org-gtd-review--window-config (current-window-configuration))
  (setq org-gtd-review--state state)
  (condition-case err
      (org-gtd-review--render)
    (error
     (org-gtd-review--teardown)
     (signal (car err) (cdr err))))
  (org-gtd-review--save-state))

;;;; Footer

(provide 'org-gtd-review)

;;; org-gtd-review.el ends here
