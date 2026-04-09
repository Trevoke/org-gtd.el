;;; transient-registry-parity-test.el --- Transient menu matches registry -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Contract test: every built-in type that declares a `:transient-key'
;; in `org-gtd-types' must have a matching entry in the
;; `org-gtd-organize' transient.  Catches drift between the registry
;; and the hand-curated menu.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-organize)
(require 'org-gtd-types)
(require 'transient)

(e-unit-initialize)

(defun org-gtd-test--transient-keys (prefix)
  "Return the set of suffix keys declared on transient PREFIX.
Walks the stored prefix layout and collects every :key attribute as
a string.  Transient stores keys as symbols in the raw layout, so we
normalize to strings via `symbol-name'."
  (let ((layout (get prefix 'transient--layout))
        (keys nil))
    (cl-labels
        ((walk (node)
           (cond
            ((vectorp node) (mapc #'walk (append node nil)))
            ((and (consp node) (eq (car-safe node) 'transient-suffix))
             (let ((key (plist-get (cdr node) :key)))
               (when key
                 (push (if (symbolp key) (symbol-name key) key) keys))))
            ((consp node)
             (mapc #'walk node)))))
      (walk layout))
    keys))

(deftest transient/every-type-with-transient-key-appears-in-menu ()
  "Every built-in type declaring :transient-key has a matching suffix."
  (let ((transient-keys (org-gtd-test--transient-keys 'org-gtd-organize)))
    (dolist (entry org-gtd-types)
      (let* ((type (car entry))
             (key (plist-get (cdr entry) :transient-key)))
        (when key
          (assert-true (member key transient-keys)))))))

(provide 'transient-registry-parity-test)

;;; transient-registry-parity-test.el ends here
