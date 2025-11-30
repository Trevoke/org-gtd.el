;;; prelude.el --- Standard test requirements for e-unit tests -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Standard prelude for org-gtd e-unit tests. Provides all common test
;; dependencies in a single require.
;;
;; Usage:
;;   (require 'ogt-eunit-prelude "test-eunit/helpers/prelude.el")
;;
;; This prelude provides:
;;   - e-unit (test framework with short syntax)
;;   - mock-fs (virtual filesystem)
;;   - org-gtd (main package)
;;   - ogt-eunit-setup (test infrastructure with mock-fs integration)
;;   - Reusable helpers from buttercup test suite (clarifying, processing, etc.)
;;

;;; Code:

(require 'e-unit)
(require 'mock-fs)
(require 'org-gtd)

;; E-unit setup with mock-fs integration
(require 'ogt-eunit-setup (file-name-concat default-directory "test-eunit/helpers/setup.el"))

;; Reuse framework-agnostic helpers from buttercup test suite
(require 'org-gtd-test-helper-clarifying (file-name-concat default-directory "test/helpers/clarifying.el"))
(require 'org-gtd-test-helper-processing (file-name-concat default-directory "test/helpers/processing.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))
(require 'org-gtd-test-helper-wip (file-name-concat default-directory "test/helpers/wip.el"))
(require 'org-gtd-test-helper-utils (file-name-concat default-directory "test/helpers/utils.el"))

;; Domain assertions (query functions are framework-agnostic)
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))

(provide 'ogt-eunit-prelude)

;;; prelude.el ends here
