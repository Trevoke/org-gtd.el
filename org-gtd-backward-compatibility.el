;;; org-gtd-backward-compatibility.el --- Functions added in later versions of emacs -*- lexical-binding: t; coding: utf-8 -*-
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
;; Backwards compatibility for org-gtd with older Emacs versions.
;;
;; This module uses the `compat' library to provide compatibility shims
;; for functions that were added in later Emacs versions:
;;
;; - ensure-list (Emacs 27.1)
;; - file-name-concat (Emacs 28.1)
;; - string-pad (Emacs 28.1)
;;
;; The compat library handles all compatibility concerns, so we just
;; need to require it.
;;
;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'compat)

;;;; Footer

(provide 'org-gtd-backward-compatibility)

;;; org-gtd-backward-compatibility.el ends here
