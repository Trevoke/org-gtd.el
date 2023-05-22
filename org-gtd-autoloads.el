;;; org-gtd-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-gtd" "org-gtd.el" (0 0 0 0))
;;; Generated autoloads from org-gtd.el

(register-definition-prefixes "org-gtd" '("org-gtd-"))

;;;***

;;;### (autoloads nil "org-gtd-archive" "org-gtd-archive.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-gtd-archive.el

(autoload 'org-gtd-archive-completed-items "org-gtd-archive" "\
Archive everything that needs to be archived in your org-gtd." t nil)

(register-definition-prefixes "org-gtd-archive" '("org-gtd-"))

;;;***

;;;### (autoloads nil "org-gtd-areas-of-focus" "org-gtd-areas-of-focus.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-gtd-areas-of-focus.el

(register-definition-prefixes "org-gtd-areas-of-focus" '("org-gtd-"))

;;;***

;;;### (autoloads nil "org-gtd-backward-compatibility" "org-gtd-backward-compatibility.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-gtd-backward-compatibility.el

(register-definition-prefixes "org-gtd-backward-compatibility" '("org-gtd--"))

;;;***

;;;### (autoloads nil "org-gtd-calendar" "org-gtd-calendar.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-gtd-calendar.el

(register-definition-prefixes "org-gtd-calendar" '("org-gtd-calendar"))

;;;***

;;;### (autoloads nil "org-gtd-capture" "org-gtd-capture.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-gtd-capture.el

(autoload 'org-gtd-capture "org-gtd-capture" "\
Capture something into the GTD inbox.

Wraps the function `org-capture' to ensure the inbox exists.
For GOTO and KEYS, see `org-capture' documentation for the variables of the
same name.

\(fn &optional GOTO KEYS)" t nil)

(autoload 'org-gtd-inbox-path "org-gtd-capture" "\
Return the full path to the inbox file." nil nil)

(register-definition-prefixes "org-gtd-capture" '("org-gtd-" "with-org-gtd-capture"))

;;;***

;;;### (autoloads nil "org-gtd-clarify" "org-gtd-clarify.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-gtd-clarify.el

(defvar org-gtd-clarify-map (make-sparse-keymap) "\
Keymap for command `org-gtd-clarify-mode', a minor mode.")

(autoload 'org-gtd-clarify-mode "org-gtd-clarify" "\
Minor mode for org-gtd.

This is a minor mode.  If called interactively, toggle the
`Org-Gtd-Clarify mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `org-gtd-clarify-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'org-gtd-clarify-item "org-gtd-clarify" "\
Process item at point through org-gtd." t nil)

(function-put 'org-gtd-clarify-item 'command-modes '(org-mode))

(register-definition-prefixes "org-gtd-clarify" '("org-gtd-clarify-"))

;;;***

;;;### (autoloads nil "org-gtd-core" "org-gtd-core.el" (0 0 0 0))
;;; Generated autoloads from org-gtd-core.el

(autoload 'with-org-gtd-context "org-gtd-core" "\
Wrap BODY... in this macro to inherit the org-gtd settings for your logic.

\(fn &rest BODY)" nil t)

(function-put 'with-org-gtd-context 'lisp-indent-function '2)

(register-definition-prefixes "org-gtd-core" '("org-gtd-"))

;;;***

;;;### (autoloads nil "org-gtd-delegate" "org-gtd-delegate.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-gtd-delegate.el

(autoload 'org-gtd-delegate-item-at-point "org-gtd-delegate" "\
Delegate item at point.  Use this if you do not want to refile the item.

You can pass DELEGATED-TO as the name of the person to whom this was delegated
and CHECKIN-DATE as the YYYY-MM-DD string of when you want `org-gtd' to remind
you if you want to call this non-interactively.
If you call this interactively, the function will ask for the name of the
person to whom to delegate by using `org-gtd-delegate-read-func'.

\(fn &optional DELEGATED-TO CHECKIN-DATE)" t nil)

(function-put 'org-gtd-delegate-item-at-point 'command-modes '(org-mode))

(autoload 'org-gtd-delegate-agenda-item "org-gtd-delegate" "\
Delegate item at point on agenda view." t nil)

(function-put 'org-gtd-delegate-agenda-item 'command-modes '(org-agenda-mode))

(register-definition-prefixes "org-gtd-delegate" '("org-gtd-delegate"))

;;;***

;;;### (autoloads nil "org-gtd-engage" "org-gtd-engage.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-gtd-engage.el

(autoload 'org-gtd-engage "org-gtd-engage" "\
Display `org-agenda' customized by org-gtd." t nil)

(autoload 'org-gtd-engage-grouped-by-context "org-gtd-engage" "\
Show all `org-gtd-next' actions grouped by context (tag prefixed with @)." t nil)

(autoload 'org-gtd-engage-next "org-gtd-engage" "\
Show all next actions from all agenda files in a single list.
This assumes all GTD files are also agenda files." t nil)

(defalias 'org-gtd-show-all-next 'org-gtd-engage-next)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-gtd-engage" '("org-gtd-agenda--prefix-format")))

;;;***

;;;### (autoloads nil "org-gtd-files" "org-gtd-files.el" (0 0 0 0))
;;; Generated autoloads from org-gtd-files.el

(register-definition-prefixes "org-gtd-files" '("org-gtd-"))

;;;***

;;;### (autoloads nil "org-gtd-habit" "org-gtd-habit.el" (0 0 0 0))
;;; Generated autoloads from org-gtd-habit.el

(register-definition-prefixes "org-gtd-habit" '("org-gtd-habit"))

;;;***

;;;### (autoloads nil "org-gtd-horizons" "org-gtd-horizons.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-gtd-horizons.el

(register-definition-prefixes "org-gtd-horizons" '("org-gtd-"))

;;;***

;;;### (autoloads nil "org-gtd-id" "org-gtd-id.el" (0 0 0 0))
;;; Generated autoloads from org-gtd-id.el

(register-definition-prefixes "org-gtd-id" '("org-gtd-id-"))

;;;***

;;;### (autoloads nil "org-gtd-incubate" "org-gtd-incubate.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-gtd-incubate.el

(register-definition-prefixes "org-gtd-incubate" '("org-gtd-incubate"))

;;;***

;;;### (autoloads nil "org-gtd-knowledge" "org-gtd-knowledge.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-gtd-knowledge.el

(register-definition-prefixes "org-gtd-knowledge" '("org-gtd-knowledge"))

;;;***

;;;### (autoloads nil "org-gtd-mode" "org-gtd-mode.el" (0 0 0 0))
;;; Generated autoloads from org-gtd-mode.el

(defvar org-gtd-mode nil "\
Non-nil if Org-Gtd mode is enabled.
See the `org-gtd-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-gtd-mode'.")

(custom-autoload 'org-gtd-mode "org-gtd-mode" nil)

(autoload 'org-gtd-mode "org-gtd-mode" "\
Global minor mode to bound `org-agenda' to the org-gtd settings.

If called interactively, enable Org-Gtd mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-gtd-mode" '("org-gtd-")))

;;;***

;;;### (autoloads nil "org-gtd-oops" "org-gtd-oops.el" (0 0 0 0))
;;; Generated autoloads from org-gtd-oops.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-gtd-oops" '("org-gtd-oops")))

;;;***

;;;### (autoloads nil "org-gtd-organize" "org-gtd-organize.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-gtd-organize.el

(register-definition-prefixes "org-gtd-organize" '("org-gtd-"))

;;;***

;;;### (autoloads nil "org-gtd-process" "org-gtd-process.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-gtd-process.el

(autoload 'org-gtd-process-inbox "org-gtd-process" "\
Start the inbox processing item, one heading at a time." t nil)

(register-definition-prefixes "org-gtd-process" '("org-gtd-process--stop"))

;;;***

;;;### (autoloads nil "org-gtd-projects" "org-gtd-projects.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-gtd-projects.el

(autoload 'org-gtd-project-cancel "org-gtd-projects" "\
With point on topmost project heading, mark all undone tasks canceled." t nil)

(autoload 'org-gtd-project-cancel-from-agenda "org-gtd-projects" "\
Cancel the project that has the highlighted task." t nil)

(function-put 'org-gtd-project-cancel-from-agenda 'command-modes '(org-agenda-mode))

(register-definition-prefixes "org-gtd-projects" '("org-"))

;;;***

;;;### (autoloads nil "org-gtd-quick-action" "org-gtd-quick-action.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-gtd-quick-action.el

(register-definition-prefixes "org-gtd-quick-action" '("org-gtd-quick-action"))

;;;***

;;;### (autoloads nil "org-gtd-refile" "org-gtd-refile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-gtd-refile.el

(autoload 'with-org-gtd-refile "org-gtd-refile" "\
Macro to refile specifically within org-gtd context.

TYPE is the org-gtd action type.  BODY is the rest of the code.

\(fn TYPE &rest BODY)" nil t)

(function-put 'with-org-gtd-refile 'lisp-indent-function '1)

(register-definition-prefixes "org-gtd-refile" '("org-gtd-refile-"))

;;;***

;;;### (autoloads nil "org-gtd-review" "org-gtd-review.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-gtd-review.el

(autoload 'org-gtd-review-area-of-focus "org-gtd-review" "\
Generate an overview agenda for a given area of focus.

You can pass an optional AREA (must be a member of `org-gtd-areas-of-focus') to
skip the menu to choose one.
START-DATE tells the code what to use as the first day for the agenda.  It is
mostly of value for testing purposes.

\(fn &optional AREA START-DATE)" t nil)

(autoload 'org-gtd-review-stuck-projects "org-gtd-review" "\
Show all projects that do not have a next action." t nil)

(register-definition-prefixes "org-gtd-review" '("org-gtd-review-stuck-"))

;;;***

;;;### (autoloads nil "org-gtd-single-action" "org-gtd-single-action.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-gtd-single-action.el

(register-definition-prefixes "org-gtd-single-action" '("org-gtd-"))

;;;***

;;;### (autoloads nil "org-gtd-skip" "org-gtd-skip.el" (0 0 0 0))
;;; Generated autoloads from org-gtd-skip.el

(register-definition-prefixes "org-gtd-skip" '("org-gtd-skip-"))

;;;***

;;;### (autoloads nil "org-gtd-trash" "org-gtd-trash.el" (0 0 0 0))
;;; Generated autoloads from org-gtd-trash.el

(register-definition-prefixes "org-gtd-trash" '("org-gtd-trash"))

;;;***

;;;### (autoloads nil "org-gtd-upgrades" "org-gtd-upgrades.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-gtd-upgrades.el

(register-definition-prefixes "org-gtd-upgrades" '("org-gtd-upgrade"))

;;;***

;;;### (autoloads nil nil ("org-gtd-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-gtd-autoloads.el ends here
