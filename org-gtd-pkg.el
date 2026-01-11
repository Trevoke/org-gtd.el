;;; org-gtd-pkg.el --- An implementation of GTD -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Homepage: https://github.com/Trevoke/org-gtd.el

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Package metadata for org-gtd.

;;; Code:

(define-package "org-gtd" "4.2.3"
  "An implementation of GTD."
  '((emacs "28.1")
    (compat "30.0.0.0")
    (org-edna "1.1.2")
    (f "0.20.0")
    (org "9.6")
    (transient "0.11.0")
    (dag-draw "1.0.4")))

;;; org-gtd-pkg.el ends here
