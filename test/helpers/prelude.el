;;; prelude.el --- Standard test requirements -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Standard prelude for org-gtd tests. Provides all common test dependencies
;; in a single require, reducing boilerplate and ensuring consistency.
;;
;; Usage:
;;   (require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
;;
;;   (describe "My test suite"
;;     (before-each (ogt--configure-emacs))
;;     (after-each (ogt--close-and-delete-files))
;;     ...)
;;
;; This prelude provides:
;;   - compat (backwards compatibility)
;;   - buttercup (test framework)
;;   - org-gtd (main package)
;;   - org-gtd-test-setup (test infrastructure, helpers, cleanup functions)
;;   - ogt-assertions (custom matchers like :to-have-same-items-as)
;;   - org-gtd-test-helper-builders (make-task, make-project, etc.)
;;
;; Tests that need additional modules (e.g., with-simulated-input) can require
;; them after the prelude.
;;

;;; Code:

(require 'compat)
(require 'buttercup)
(require 'org-gtd)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))

(provide 'org-gtd-test-prelude)

;;; prelude.el ends here
