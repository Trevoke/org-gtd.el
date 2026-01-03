;;; prefix-element-test.el --- Unit tests for prefix element resolvers -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for the GTD prefix element resolver functions that support
;; the semantic prefix DSL.
;;
;; Test Coverage:
;; - org-gtd-agenda--resolve-project
;; - org-gtd-agenda--resolve-area-of-focus
;; - org-gtd-agenda--resolve-file-name
;; - org-gtd-agenda--resolve-prefix-element (dispatcher)
;; - org-gtd-agenda--resolve-prefix-chain (fallback chain)

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")

(e-unit-initialize)

;;; Project Resolver Tests

(deftest prefix-element/resolve-project-returns-project-heading ()
  "Resolves project element to parent project headline."
  (ogt--with-temp-org-buffer
   "* Project Alpha
** NEXT Task under project
"
   (search-forward "Task under project")
   (org-entry-put nil org-gtd-prop-project "Project Alpha")
   (assert-equal "Project Alpha" (org-gtd-agenda--resolve-project))))

(deftest prefix-element/resolve-project-returns-nil-when-no-project ()
  "Returns nil when item has no parent project."
  (ogt--with-temp-org-buffer
   "* NEXT Standalone task
"
   (search-forward "Standalone task")
   (assert-equal nil (org-gtd-agenda--resolve-project))))

;;; Area of Focus Resolver Tests

(deftest prefix-element/resolve-area-of-focus-returns-category ()
  "Resolves area-of-focus element to CATEGORY property."
  (ogt--with-temp-org-buffer
   "* NEXT Task with category
"
   (search-forward "Task with category")
   (org-entry-put nil "CATEGORY" "Health")
   (assert-equal "Health" (org-gtd-agenda--resolve-area-of-focus))))

(deftest prefix-element/resolve-area-of-focus-returns-nil-when-no-category ()
  "Returns nil when item has no CATEGORY property."
  (ogt--with-temp-org-buffer
   "* NEXT Task without category
"
   (search-forward "Task without category")
   (assert-equal nil (org-gtd-agenda--resolve-area-of-focus))))

;;; File Name Resolver Tests

(deftest prefix-element/resolve-file-name-returns-base-name ()
  "Resolves file-name element to buffer file base name."
  (ogt--with-temp-org-buffer
   "* NEXT Some task
"
   (setq buffer-file-name "/path/to/org-gtd-tasks.org")
   (search-forward "Some task")
   (assert-equal "org-gtd-tasks" (org-gtd-agenda--resolve-file-name))))

(deftest prefix-element/resolve-file-name-returns-nil-when-no-file ()
  "Returns nil when buffer has no file."
  (ogt--with-temp-org-buffer
   "* NEXT Some task
"
   (setq buffer-file-name nil)
   (search-forward "Some task")
   (assert-equal nil (org-gtd-agenda--resolve-file-name))))

;;; Generic Dispatcher Tests

(deftest prefix-element/resolve-element-dispatches-project ()
  "Dispatcher routes 'project to project resolver."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (search-forward "Task")
   (org-entry-put nil org-gtd-prop-project "My Project")
   (assert-equal "My Project" (org-gtd-agenda--resolve-prefix-element 'project))))

(deftest prefix-element/resolve-element-dispatches-area-of-focus ()
  "Dispatcher routes 'area-of-focus to area resolver."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (search-forward "Task")
   (org-entry-put nil "CATEGORY" "Finance")
   (assert-equal "Finance" (org-gtd-agenda--resolve-prefix-element 'area-of-focus))))

(deftest prefix-element/resolve-element-dispatches-file-name ()
  "Dispatcher routes 'file-name to file resolver."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (setq buffer-file-name "/some/inbox.org")
   (search-forward "Task")
   (assert-equal "inbox" (org-gtd-agenda--resolve-prefix-element 'file-name))))

(deftest prefix-element/resolve-element-returns-literal-string ()
  "Dispatcher returns string literals as-is."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (search-forward "Task")
   (assert-equal "Incubated" (org-gtd-agenda--resolve-prefix-element "Incubated"))))

;;; Fallback Chain Tests

(deftest prefix-element/resolve-chain-returns-first-match ()
  "Fallback chain returns first non-nil element."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (search-forward "Task")
   (org-entry-put nil org-gtd-prop-project "My Project")
   (org-entry-put nil "CATEGORY" "Health")
   ;; Project is first, so it wins even though CATEGORY is also set
   (assert-equal "My Project"
                 (string-trim-right
                  (org-gtd-agenda--resolve-prefix-chain '(project area-of-focus) 50)))))

(deftest prefix-element/resolve-chain-falls-back-to-second ()
  "Fallback chain tries second element when first is nil."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (search-forward "Task")
   ;; No project, but has CATEGORY
   (org-entry-put nil "CATEGORY" "Health")
   (assert-equal "Health"
                 (string-trim-right
                  (org-gtd-agenda--resolve-prefix-chain '(project area-of-focus) 50)))))

(deftest prefix-element/resolve-chain-uses-literal-string ()
  "Fallback chain returns literal string when symbols fail."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (search-forward "Task")
   ;; No project, no category - falls back to literal
   (assert-equal "Incubated"
                 (string-trim-right
                  (org-gtd-agenda--resolve-prefix-chain '(project area-of-focus "Incubated") 50)))))

(deftest prefix-element/resolve-chain-returns-padded-when-all-fail ()
  "Fallback chain returns padded empty string when all elements fail."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (setq buffer-file-name nil)
   (search-forward "Task")
   ;; No project, no category, no file - returns spaces for width
   (let ((result (org-gtd-agenda--resolve-prefix-chain '(project area-of-focus file-name) 10)))
     (assert-equal 10 (length result))
     (assert-equal "" (string-trim result)))))

(deftest prefix-element/resolve-chain-truncates-to-width ()
  "Fallback chain truncates result to specified width."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (search-forward "Task")
   (org-entry-put nil org-gtd-prop-project "Very Long Project Name")
   (assert-equal "Very Lon…"
                 (org-gtd-agenda--resolve-prefix-chain '(project) 9))))

(deftest prefix-element/resolve-chain-pads-to-width ()
  "Fallback chain pads result with spaces to specified width."
  (ogt--with-temp-org-buffer
   "* NEXT Task
"
   (search-forward "Task")
   (org-entry-put nil org-gtd-prop-project "Short")
   (assert-equal "Short     "
                 (org-gtd-agenda--resolve-prefix-chain '(project) 10))))

(provide 'prefix-element-test)

;;; prefix-element-test.el ends here
