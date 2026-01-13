;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Homepage: https://github.com/Trevoke/org-gtd.el
;; Package-Requires: ((emacs "28.1") (compat "30.0.0.0") (org-edna "1.1.2") (f "0.20.0") (org "9.6") (transient "0.11.0") (dag-draw "1.0.4"))
;; Package-Version: 4.3.0

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

;; This package tries to replicate as closely as possible
;; the GTD workflow.
;; This package assumes familiarity with GTD.
;;
;; This package provides a system that allows you to capture incoming
;; things into an inbox, then process the inbox and categorize each
;; item based on the GTD categories.  It leverages org-agenda to show
;; today's items as well as the NEXT items.  It also has a simple
;; project management system, which currently assumes all tasks in a
;; project are sequential.
;;
;; For a comprehensive instruction manual, see the documentation.
;; Either the info file or in the doc/ directory of the repository.
;; Upgrade information is also available therein.
;;
;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'cl-lib)
(require 'f)
(require 'transient)
(require 'org)
(require 'org-capture)
(require 'org-element)
(require 'org-edna)

(require 'org-gtd-core)
(require 'org-gtd-context)
(require 'org-gtd-id)
(require 'org-gtd-files)
(require 'org-gtd-horizons)
(require 'org-gtd-areas-of-focus)
(require 'org-gtd-clarify)
(require 'org-gtd-delegate)
(require 'org-gtd-archive)
(require 'org-gtd-reactivate)
(require 'org-gtd-capture)
(require 'org-gtd-refile)
(require 'org-gtd-accessors)
(require 'org-gtd-value-objects)
(require 'org-gtd-dependencies)
(require 'org-gtd-projects)
;; Graph features are autoloaded - only loaded when user invokes graph commands
;; (org-gtd-show-project-graph, org-gtd-graph-view-mode, etc.)
(require 'org-gtd-agenda)
(require 'org-gtd-agenda-transient)
(require 'org-gtd-engage)
(require 'org-gtd-agenda-property)
(require 'org-gtd-organize)
(require 'org-gtd-process)
(require 'org-gtd-mode)
(require 'org-gtd-reflect)
(require 'org-gtd-someday-review)
(require 'org-gtd-view-language)
(require 'org-gtd-upgrades)
(require 'org-gtd-command-center)

;;;; Constants

(defconst org-gtd-version "4.0.7")

;;;; Variables

(defvar org-gtd-update-ack "1.0.0"
  "Set this to the latest version you have upgraded to.

You will only see warnings relevant to upgrade steps you must take to
go up from your version to the one installed.  Use a version string.
For instance:

If org-gtd is 2.0.0, use \"2.0.0\".
If org-gtd is 2.3.5, use \"2.3.5\".")

(if (version< org-gtd-update-ack "4.0.0")
    (lwarn 'org-gtd :warning "

|---------------------------------------|
| WARNING: ORG GTD MAJOR VERSION CHANGE |
|---------------------------------------|

See the changelog for a full set of changes.

See the documentation for complete upgrade information:

=> `C-h i m org gtd RET' or `M-x info-display-manual RET org-gtd'

Important notices involve:
- required (but simple) changes to the configuration
- running `org-gtd-upgrade-v3-to-v4'

So do review this, and join the discord in the readme if you want to
meet more users!

To make this warning go away on further loads, add the following setting
to your config file (BEFORE ORG-GTD LOADS):

(setq org-gtd-update-ack \"4.0.0\")
"))

;;;; Footer

(provide 'org-gtd)

;;; org-gtd.el ends here
