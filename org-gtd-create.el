;;; org-gtd-create.el --- Programmatic item creation -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2025 Aldric Giacomoni

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
;; Unified programmatic entry point for creating GTD items of any type.
;; This replaces the per-type `*-create' helpers with a single function
;; that dispatches through the type registry.
;;
;;; Code:

(require 'org)
(require 'org-gtd-organize-core)

;;;###autoload
(defun org-gtd-create-item (type topic &optional config)
  "Programmatically create a GTD item of TYPE with heading TOPIC.

TYPE is a symbol naming a registered org-gtd type (for example
`next-action', `calendar', `delegated', `tickler', `someday',
`habit', `reference', `quick-action', or `trash').  TOPIC is the
heading text.  CONFIG is an optional alist forwarded to the type's
`:organize-fn' for non-interactive use (e.g. `((:when . \"<2026-05-01>\")
(:who . \"Alice\"))')."
  (let ((buffer (generate-new-buffer "*org-gtd-create*"))
        (org-id-overriding-file-name "org-gtd"))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert (format "* %s" topic))
          (goto-char (point-min))
          (org-gtd-process-heading (point-marker) type config))
      (kill-buffer buffer))))

(provide 'org-gtd-create)

;;; org-gtd-create.el ends here
