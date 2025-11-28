;; -*- lexical-binding: t; coding: utf-8 -*-

;;; wip-test.el --- Tests for WIP buffer helper -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;;; Commentary:
;;
;; TDD tests for the with-wip-buffer macro and ogt-get-wip-buffer function.
;;

;;; Code:

(require 'compat)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

;; Load the module under test
(require 'org-gtd-test-helper-wip (file-name-concat default-directory "test/helpers/wip.el"))

(describe "WIP buffer helpers"
  (before-each
    (setq inhibit-message t)
    (ogt--configure-emacs))
  (after-each
    (ogt--close-and-delete-files))

  (describe "ogt-get-wip-buffer"
    (it "returns nil when no WIP buffer exists"
      (expect (ogt-get-wip-buffer) :to-be nil))

    (it "returns the WIP buffer when one exists"
      ;; Create a WIP buffer by starting to process an inbox item
      (org-gtd-capture nil "i")
      (insert "Test item")
      (org-capture-finalize)
      (org-gtd-process-inbox)

      ;; Now there should be a WIP buffer
      (let ((wip-buf (ogt-get-wip-buffer)))
        (expect wip-buf :not :to-be nil)
        (expect (buffer-live-p wip-buf) :to-be t)
        (expect (string-search org-gtd-wip--prefix (buffer-name wip-buf)) :not :to-be nil))))

  (describe "with-wip-buffer macro"
    (it "executes body in WIP buffer context"
      ;; Create a WIP buffer
      (org-gtd-capture nil "i")
      (insert "Test item for macro")
      (org-capture-finalize)
      (org-gtd-process-inbox)

      ;; Use the macro to insert text
      (with-wip-buffer
        (goto-char (point-max))
        (insert "\n** Added by macro"))

      ;; Verify the text was added
      (let ((wip-buf (ogt-get-wip-buffer)))
        (with-current-buffer wip-buf
          (expect (buffer-string) :to-match "Added by macro"))))

    (it "returns nil when no WIP buffer exists"
      (expect (with-wip-buffer (point)) :to-be nil))

    (it "returns the value of the last form"
      ;; Create a WIP buffer
      (org-gtd-capture nil "i")
      (insert "Test item")
      (org-capture-finalize)
      (org-gtd-process-inbox)

      ;; The macro should return the last form's value
      (expect (with-wip-buffer 42) :to-equal 42)
      (expect (with-wip-buffer (+ 1 2 3)) :to-equal 6))))
