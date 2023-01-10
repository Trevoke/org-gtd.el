;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2023 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Homepage: https://github.com/Trevoke/org-gtd.el
;; Package-Requires: ((emacs "27.1") (org-edna "1.1.2") (f "0.20.0") (org "9.5") (org-agenda-property "1.3.1") (transient "0.3.7"))
;; Package-Version: 2.3.0

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

;; This package tries to replicate as closely as possible the GTD workflow.
;; This package assumes familiarity with GTD.
;;
;; This package provides a system that allows you to capture incoming things
;; into an inbox, then process the inbox and categorize each item based on the
;; GTD categories.  It leverages org-agenda to show today's items as well as the
;; NEXT items.  It also has a simple project management system, which currently
;; assumes all tasks in a project are sequential.
;;
;; For a comprehensive instruction manual, see the documentation.
;; Either the info file or in the doc/ directory of the repository.
;; Upgrade information is also available therein.
;;
;;; Code:
(defconst org-gtd-version "2.3.0")

(require 'subr-x)
(require 'cl-lib)
(require 'f)
(require 'transient)
(require 'org)
(require 'org-capture)
(require 'org-element)
(require 'org-agenda-property)
(require 'org-edna)
(require 'org-gtd-customize)

(require 'org-gtd-core)
(require 'org-gtd-delegate)
(require 'org-gtd-archive)
(require 'org-gtd-capture)
(require 'org-gtd-files)
(require 'org-gtd-refile)
(require 'org-gtd-projects)
(require 'org-gtd-agenda)
(require 'org-gtd-inbox-processing)
(require 'org-gtd-mode)

(defvar org-gtd-update-ack "1.0.0"
  "Set this to the latest version you have upgraded to.

You will only see warnings relevant to upgrade steps you must take to go up from
your version to the one installed. Use a version string. For instance:

If org-gtd is 2.0.0, use \"2.0.0\".
If org-gtd is 2.3.5, use \"2.3.5\".")

(if (version< org-gtd-update-ack "2.1.0")
    (lwarn 'org-gtd :warning "

|--------------------------|
| WARNING: action required |
|--------------------------|

Upgrading to 2.1.0 requires changing the org-edna triggers for the project
categories. Failure to do so means your projects will end up in inconsistent
states.

See the documentation for instructions to upgrade (C-h i, then find org-gtd),
then add the following setting to your config file (BEFORE ORG-GTD LOADS)
to disable this warning.

(setq org-gtd-update-ack \"2.1.0\")

"))


(provide 'org-gtd)
;;; org-gtd.el ends here
