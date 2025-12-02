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
(require 'e-unit-mock)
(require 'mock-fs)
(require 'org-gtd)

;; Initialize e-unit-mock for test doubles (spy, stub, mock, fake, dummy)
(e-unit-mock-initialize)

;; Helper function to get path relative to this prelude file
;; This file is at test-eunit/helpers/prelude.el
(eval-and-compile
  (defun ogt-eunit--relative-path (relative-path)
    "Get absolute path RELATIVE-PATH from this file's directory."
    (let* ((file-loc (or load-file-name
                         byte-compile-current-file
                         buffer-file-name))
           (base-dir (when file-loc (file-name-directory file-loc))))
      (if base-dir
          (expand-file-name relative-path base-dir)
        (error "Cannot determine prelude.el location")))))

;; E-unit setup with mock-fs integration (same directory)
(require 'ogt-eunit-setup (ogt-eunit--relative-path "setup.el"))

;; Reuse framework-agnostic helpers from buttercup test suite
;; Navigate from test-eunit/helpers/ to test/helpers/ via ../../test/helpers/
;; NOTE: Load order matters! builders.el must be loaded BEFORE processing.el
;; because processing.el has a require for builders with default-directory,
;; which fails during Eldev compilation. By loading builders first, the
;; require in processing.el becomes a no-op (already provided).
(require 'org-gtd-test-helper-builders (ogt-eunit--relative-path "../../test/helpers/builders.el"))
(require 'org-gtd-test-helper-clarifying (ogt-eunit--relative-path "../../test/helpers/clarifying.el"))
(require 'org-gtd-test-helper-processing (ogt-eunit--relative-path "../../test/helpers/processing.el"))
(require 'org-gtd-test-helper-wip (ogt-eunit--relative-path "../../test/helpers/wip.el"))
(require 'org-gtd-test-helper-utils (ogt-eunit--relative-path "../../test/helpers/utils.el"))

;; Domain assertions (query functions are framework-agnostic)
(require 'ogt-assertions (ogt-eunit--relative-path "../../test/helpers/assertions.el"))

;; Keyboard integration helpers for end-to-end testing
(require 'keyboard-integration (ogt-eunit--relative-path "../../test/helpers/keyboard-integration.el"))

;;; Simple utility macros

(defmacro ogt--with-temp-org-buffer (contents &rest body)
  "Like `with-temp-buffer', but in Org mode.

CONTENTS is inserted and point is set to the buffer's beginning
before running BODY."
  (declare (debug t))
  `(with-temp-buffer
     (org-mode)
     (insert ,contents)
     (goto-char 1)
     ,@body))

(provide 'ogt-eunit-prelude)

;;; prelude.el ends here
