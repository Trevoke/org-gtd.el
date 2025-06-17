;;; org-gtd.el --- An implementation of GTD -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2019-2023, 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; Homepage: https://github.com/Trevoke/org-gtd.el
;; Package-Requires: ((emacs "27.2") (org-edna "1.1.2") (f "0.20.0") (org "9.6") (org-agenda-property "1.3.1") (transient "0.3.7") (org-ql "0.8.10"))
;; Package-Version: 3.0.0

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
(require 'org-agenda-property)
(require 'org-edna)

(require 'org-gtd-core)
(require 'org-gtd-id)
(require 'org-gtd-files)
(require 'org-gtd-horizons)
(require 'org-gtd-areas-of-focus)
(require 'org-gtd-clarify)
(require 'org-gtd-delegate)
(require 'org-gtd-archive)
(require 'org-gtd-capture)
(require 'org-gtd-refile)
(require 'org-gtd-projects)
(require 'org-gtd-agenda)
(require 'org-gtd-organize)
(require 'org-gtd-process)
(require 'org-gtd-mode)
(require 'org-gtd-review)
(require 'org-gtd-oops)
(require 'org-gtd-upgrades)

;;;; Constants

(defconst org-gtd-version "3.0.0")

(defcustom org-gtd-user-item-config '()
  "Overrides for `org-gtd-items' with your usual config.
Note that if `org-gtd' finds an item type in here, it will use this
information instead of the built-in one."
  :group 'org-gtd
  :type 'alist
  :package-version '(org-gtd . "3.1"))

(defun org-gtd-items ()
  "Return the base configuration for org-gtd item types."
  `((:calendar
     .
     ((ORG_GTD . "Calendar")
      (ID . org-gtd-id-get-create)
      (ORG_GTD_TIMESTAMP
       .
       ((type . 'active-timestamp)
        (prompt .
                "When is this going to happen?: ")))))
    (:next . ((ORG_GTD . "Actions")
              (TODO . ,(org-gtd-keywords--next))
              (ID . org-gtd-id-get-create)))
    (:habit
     .
     ((ORG_GTD . "Habit")
      (SCHEDULED
       .
       ((type . 'active-timestamp-with-repeater)
        (prompt . "When and how often do you want this to repeat?")))
      (STYLE . "habit")
      (ID . org-gtd-id-get-create)))
    (:incubate
     .
     ((ORG_GTD . "Incubated")
      (ID . org-gtd-id-get-create)
      (ORG_GTD_TIMESTAMP
       .
       ((type . 'active-timestamp)
        (prompt . "When do you want to be reminded of this?: ")))))
    (:delegated
     .
     ((ORG_GTD . "Actions")
      (TODO . ,(org-gtd-keywords--wait))
      (ID . org-gtd-id-get-create)
      (DELEGATED_TO . ((type . 'text)
                       (prompt . "Who will do this?: ")))
      (ORG_GTD_TIMESTAMP
       .
       ((type . 'active-timestamp)
        (prompt . "When do you want to check on this?: ")))))
    (:project-heading . ((ORG_GTD . "Projects")
                         (ID . org-gtd-id-get-create)))
    (:project-task . ((ID . org-gtd-id-get-create)
                      (TODO . ,(org-gtd-keywords--todo))))
    (:knowledge . ((ORG_GTD . "Reference")
                   (ID . org-gtd-id-get-create)
                   (TODO . ,(car org-done-keywords))))
    (:trash . ((ORG_GTD . "Trash")
               (ID . org-gtd-id-get-create)
               (TODO . ,(org-gtd-keywords--canceled))))
    (:quick-action . ((ORG_GTD . "Actions")
                      (ID . org-gtd-id-get-create)
                      (TODO . ,(car org-done-keywords))))))

;;;; Variables

(defvar org-gtd-update-ack "1.0.0"
  "Set this to the latest version you have upgraded to.

You will only see warnings relevant to upgrade steps you must take to
go up from your version to the one installed.  Use a version string.
For instance:

If org-gtd is 2.0.0, use \"2.0.0\".
If org-gtd is 2.3.5, use \"2.3.5\".")

(if (version< org-gtd-update-ack "3.0.0")
    (lwarn 'org-gtd :warning "

|-------------------------|
| WARNING: MAJOR VERSION  |
|-------------------------|

See the changelog for a full set of changes.

See the documentation for complete upgrade information:

=> `C-h i m org gtd RET'

Important notices involve:
- run `org-gtd-upgrade-v2-to-v3'
- this moves all your habits to a new sub-heading in the default
  org-gtd file
- it will create this file if you don't have it
- as long you respect the Habits structure, move them where you want

- some of the key commands have changed, e.g.
- `org-gtd-choose' is now `org-gtd-organize'
- `org-gtd-process-item-hooks' is now `org-gtd-organize-hooks'

So do review this, and join the discord in the readme if you want to
meet more users!

To make this warning go away, add the following setting to your config
file (BEFORE ORG-GTD LOADS)

(setq org-gtd-update-ack \"3.0.0\"

"))

(if (version< org-gtd-update-ack "2.1.0")
    (lwarn 'org-gtd :warning "

|--------------------------|
| WARNING: action required |
|--------------------------|

Upgrading to 2.1.0 requires changing the org-edna triggers for the
project categories. Failure to do so means your projects will end up
in inconsistent states.

See the documentation for instructions to upgrade (C-h i, then find
org-gtd), then add the following setting to your config file
(BEFORE ORG-GTD LOADS) to disable this warning.

(setq org-gtd-update-ack \"2.1.0\")

"))

;;;; Footer

(provide 'org-gtd)

;;; org-gtd.el ends here
