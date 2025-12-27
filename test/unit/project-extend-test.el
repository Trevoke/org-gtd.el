;;; project-extend-test.el --- Tests for add-to-project flow -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for the add-to-project (project-extend) flow.
;; Tests the new flow: select project first, set ORG_GTD_PROJECT_IDS explicitly.
;;

;;; Code:

(require 'e-unit)
(require 'org-gtd-projects)

;; Initialize e-unit short syntax
(e-unit-initialize)

(provide 'project-extend-test)
;;; project-extend-test.el ends here
