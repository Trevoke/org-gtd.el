;;; -*- lexical-binding: t; -*-
;;
;; Autoload entry point test - runs all autoload tests in a fresh subprocess.
;; This ensures org-gtd-autoloads is tested without the full package being loaded.

(require 'compat)

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'buttercup)

(describe "autoloaded entry points"
  (it "all autoloads work correctly"
    (expect
     (ogt--recursive-eldev-test "autoload-tests/all-autoloads-test.el")
     :to-equal 0)))
