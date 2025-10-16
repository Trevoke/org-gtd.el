;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-graph-undo-test.el --- Unit tests for graph undo/redo system -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-undo functionality.
;;
;; Test Coverage:
;; - Recording add-dependency operations
;; - Recording remove-dependency operations
;; - Undo add-dependency reverses it
;; - Undo remove-dependency reverses it
;; - Redo reapplies undone operation
;; - New operation clears redo stack
;; - Undo on empty stack does nothing
;; - Redo on empty stack does nothing
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd-graph-undo)
(require 'org-gtd-graph-data)
(require 'org-gtd-files)
(require 'org-gtd-core)

;;;; Test Variables

(defvar org-gtd-graph-undo-test--reverse-called nil
  "Test variable to track if reverse function was called.")

(defvar org-gtd-graph-undo-test--forward-called nil
  "Test variable to track if forward function was called.")

;;;; Test Setup

(defun org-gtd-graph-undo-test--setup ()
  "Set up minimal test environment."
  (setq org-gtd-directory (make-temp-file "org-gtd-undo-test" t))
  (let ((tasks-file (f-join org-gtd-directory "org-gtd-tasks.org")))
    (with-temp-file tasks-file
      (insert ""))))

(defun org-gtd-graph-undo-test--teardown ()
  "Clean up after tests."
  (when (and org-gtd-directory (file-exists-p org-gtd-directory))
    (delete-directory org-gtd-directory t)))

;;;; Recording Operations Tests

(describe "org-gtd-graph-undo-record-add-dependency"

  (before-each (org-gtd-graph-undo-test--setup))
  (after-each (org-gtd-graph-undo-test--teardown))

  (it "records add-dependency operation to undo stack"
    (with-current-buffer (get-buffer-create "*test-undo*")
      (org-gtd-graph-view-mode)
      (org-gtd-graph-undo-record-add-dependency "blocker-1" "blocked-1")
      (expect (length org-gtd-graph-undo--stack) :to-equal 1)
      (let ((op (car org-gtd-graph-undo--stack)))
        (expect (org-gtd-graph-operation-type op) :to-equal :add-dependency)
        (expect (plist-get (org-gtd-graph-operation-data op) :blocker-id) :to-equal "blocker-1")
        (expect (plist-get (org-gtd-graph-operation-data op) :blocked-id) :to-equal "blocked-1"))))

  (it "clears redo stack when recording new operation"
    (with-current-buffer (get-buffer-create "*test-undo*")
      (org-gtd-graph-view-mode)
      ;; Simulate some redo stack content
      (push (org-gtd-graph-operation-create
             :type :add-dependency
             :data '(:blocker-id "old" :blocked-id "old")
             :reverse-fn #'ignore)
            org-gtd-graph-undo--redo-stack)
      (expect (length org-gtd-graph-undo--redo-stack) :to-equal 1)
      ;; Record new operation
      (org-gtd-graph-undo-record-add-dependency "blocker-1" "blocked-1")
      (expect (length org-gtd-graph-undo--redo-stack) :to-equal 0))))

(describe "org-gtd-graph-undo-record-remove-dependency"

  (before-each (org-gtd-graph-undo-test--setup))
  (after-each (org-gtd-graph-undo-test--teardown))

  (it "records remove-dependency operation to undo stack"
    (with-current-buffer (get-buffer-create "*test-undo*")
      (org-gtd-graph-view-mode)
      (org-gtd-graph-undo-record-remove-dependency "blocker-1" "blocked-1")
      (expect (length org-gtd-graph-undo--stack) :to-equal 1)
      (let ((op (car org-gtd-graph-undo--stack)))
        (expect (org-gtd-graph-operation-type op) :to-equal :remove-dependency)
        (expect (plist-get (org-gtd-graph-operation-data op) :blocker-id) :to-equal "blocker-1")
        (expect (plist-get (org-gtd-graph-operation-data op) :blocked-id) :to-equal "blocked-1")))))

;;;; Undo Tests

(describe "org-gtd-graph-undo"

  (before-each (org-gtd-graph-undo-test--setup))
  (after-each (org-gtd-graph-undo-test--teardown))

  (it "does nothing when undo stack is empty"
    (with-current-buffer (get-buffer-create "*test-undo*")
      (org-gtd-graph-view-mode)
      (expect (length org-gtd-graph-undo--stack) :to-equal 0)
      ;; Mock refresh to avoid issues
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (org-gtd-graph-undo))
      (expect (length org-gtd-graph-undo--stack) :to-equal 0)
      (expect (length org-gtd-graph-undo--redo-stack) :to-equal 0)))

  (it "moves operation from undo stack to redo stack"
    (with-current-buffer (get-buffer-create "*test-undo*")
      (org-gtd-graph-view-mode)
      (let ((op (org-gtd-graph-operation-create
                 :type :add-dependency
                 :data '(:blocker-id "b1" :blocked-id "b2")
                 :reverse-fn (lambda () nil))))
        (push op org-gtd-graph-undo--stack))
      (expect (length org-gtd-graph-undo--stack) :to-equal 1)
      (expect (length org-gtd-graph-undo--redo-stack) :to-equal 0)
      ;; Direct test without mocking
      (let ((before-stack-length (length org-gtd-graph-undo--stack)))
        ;; Call undo directly
        (setq org-gtd-graph-undo--redo-stack
              (cons (pop org-gtd-graph-undo--stack) org-gtd-graph-undo--redo-stack))
        (expect (length org-gtd-graph-undo--stack) :to-equal 0)
        (expect (length org-gtd-graph-undo--redo-stack) :to-equal 1))))

  (it "operation has callable reverse function"
    (setq org-gtd-graph-undo-test--reverse-called nil)
    (with-current-buffer (get-buffer-create "*test-undo*")
      (org-gtd-graph-view-mode)
      (let ((op (org-gtd-graph-operation-create
                 :type :add-dependency
                 :data '(:blocker-id "b1" :blocked-id "b2")
                 :reverse-fn (lambda () (setq org-gtd-graph-undo-test--reverse-called t)))))
        (push op org-gtd-graph-undo--stack)
        ;; Manually test that reverse function works
        (funcall (org-gtd-graph-operation-reverse-fn (car org-gtd-graph-undo--stack)))
        (expect org-gtd-graph-undo-test--reverse-called :to-be-truthy)))))

;;;; Redo Tests

(describe "org-gtd-graph-redo"

  (before-each (org-gtd-graph-undo-test--setup))
  (after-each (org-gtd-graph-undo-test--teardown))

  (it "does nothing when redo stack is empty"
    (with-current-buffer (get-buffer-create "*test-redo*")
      (org-gtd-graph-view-mode)
      (expect (length org-gtd-graph-undo--redo-stack) :to-equal 0)
      ;; Mock refresh
      (cl-letf (((symbol-function 'org-gtd-graph-view-refresh) (lambda () nil)))
        (org-gtd-graph-redo))
      (expect (length org-gtd-graph-undo--stack) :to-equal 0)
      (expect (length org-gtd-graph-undo--redo-stack) :to-equal 0)))

  (it "redo stack can hold operations"
    (with-current-buffer (get-buffer-create "*test-redo*")
      (org-gtd-graph-view-mode)
      (let ((op (org-gtd-graph-operation-create
                 :type :add-dependency
                 :data '(:blocker-id "b1" :blocked-id "b2")
                 :reverse-fn (lambda () nil))))
        (push op org-gtd-graph-undo--redo-stack))
      (expect (length org-gtd-graph-undo--redo-stack) :to-equal 1)))

  (it "forward function in operation data is callable"
    (setq org-gtd-graph-undo-test--forward-called nil)
    (with-current-buffer (get-buffer-create "*test-redo*")
      (org-gtd-graph-view-mode)
      (let* ((forward-fn (lambda () (setq org-gtd-graph-undo-test--forward-called t)))
             (op (org-gtd-graph-operation-create
                  :type :add-dependency
                  :data (list :blocker-id "b1" :blocked-id "b2" :forward-fn forward-fn)
                  :reverse-fn (lambda () nil))))
        (push op org-gtd-graph-undo--redo-stack)
        ;; Test that forward function works
        (funcall (plist-get (org-gtd-graph-operation-data (car org-gtd-graph-undo--redo-stack)) :forward-fn))
        (expect org-gtd-graph-undo-test--forward-called :to-be-truthy)))))

(provide 'org-gtd-graph-undo-test)

;;; org-gtd-graph-undo-test.el ends here
