;;; hooks-test.el --- Unit tests for org-gtd hook runner -*- lexical-binding: t; coding: utf-8 -*-

;;; Commentary:
;; Tests for the six-stage hook infrastructure introduced in
;; docs/plans/2026-04-07-unified-type-and-hook-model-design.md.

;;; Code:

(require 'cl-lib)
(require 'e-unit)
(require 'org-gtd)
(require 'org-gtd-types)
(require 'org-gtd-hooks)

(e-unit-initialize)

(deftest hooks-run-calls-global-then-local ()
  "Global hooks fire before local hooks within a single stage."
  (let ((log nil))
    (cl-letf (((symbol-value 'org-gtd-types) (copy-tree org-gtd-types))
              ((symbol-value 'org-gtd-before-organize-hook)
               (list (lambda (_pom) (push 'g log)))))
      (org-gtd-customize-type 'calendar
        :hooks (list :before-organize
                     (list (lambda (_pom) (push 'l log)))))
      (org-gtd-hooks-run :before-organize 'calendar (point-marker))
      (assert-equal '(g l) (reverse log)))))

(deftest hooks-run-swallows-errors-and-continues ()
  "An erroring hook must not abort the remaining hooks in the stage."
  (let ((log nil))
    (cl-letf (((symbol-value 'org-gtd-before-organize-hook)
               (list (lambda (_pom) (error "boom"))
                     (lambda (_pom) (push 'ran log)))))
      (org-gtd-hooks-run :before-organize 'calendar (point-marker))
      (assert-equal '(ran) log))))

(deftest hooks-run-fires-all-six-stages ()
  "Every documented stage maps to a distinct global defvar."
  (dolist (stage '(:before-clarify :after-clarify
                   :before-organize :after-organize
                   :before-file :after-file))
    (let ((called nil))
      (cl-letf (((symbol-value (org-gtd-hooks--global-var stage))
                 (list (lambda (_pom) (setq called t)))))
        (org-gtd-hooks-run stage 'calendar (point-marker))
        (assert-true called)))))

(deftest hooks-run-handles-unknown-type-gracefully ()
  "Running hooks for an unknown type still fires global hooks."
  (let ((called nil))
    (cl-letf (((symbol-value 'org-gtd-after-file-hook)
               (list (lambda (_pom) (setq called t)))))
      (org-gtd-hooks-run :after-file 'bogus-type (point-marker))
      (assert-true called))))

(provide 'hooks-test)
;;; hooks-test.el ends here
