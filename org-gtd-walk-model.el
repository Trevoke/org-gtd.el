;;; org-gtd-walk-model.el --- Pure headless walk model -*- lexical-binding: t; coding: utf-8 -*-
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
;; The pure, headless walk model: a serializable plist and pure functions
;; over it.  No buffers, no org, no I/O.  This is the Tier 1 core of the
;; walk engine (see docs/plans/2026-07-17-walk-engine-design.md §4).
;;
;; A model is a plist: (:entries LIST :cursor INT :meta LIST).
;; The cursor is the index of the current item; entries before it are
;; handled, entries after it are pending, cursor == (length entries) is done.
;;
;;; Code:

(require 'seq)

;;;; Construction

(defun org-gtd-walk-model-create (entries &optional meta)
  "Return a fresh walk model over ENTRIES with optional META plist.
ENTRIES is copied; the cursor starts at 0."
  (list :entries (copy-sequence entries)
        :cursor 0
        :meta meta))

;;;; Footer

(provide 'org-gtd-walk-model)

;;; org-gtd-walk-model.el ends here
