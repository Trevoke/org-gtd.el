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

(defun org-gtd-test--transient-key-commands (prefix)
  "Return an alist of (KEY . COMMAND) declared on transient PREFIX.
Walks the stored prefix layout and collects every suffix's
:key (normalized to a string) and :command pair."
  (let ((layout (get prefix 'transient--layout))
        (pairs nil))
    (cl-labels
        ((walk (node)
           (cond
            ((vectorp node) (mapc #'walk (append node nil)))
            ((and (consp node) (eq (car-safe node) 'transient-suffix))
             (let ((key (plist-get (cdr node) :key))
                   (cmd (plist-get (cdr node) :command)))
               (when (and key cmd)
                 (push (cons (if (symbolp key) (symbol-name key) key)
                             cmd)
                       pairs))))
            ((consp node)
             (mapc #'walk node)))))
      (walk layout))
    pairs))

(defvar org-gtd-test--type-command-exceptions
  '((reference . org-gtd-knowledge)
    (delegated . org-gtd-delegate))
  "Registry types whose transient command name does NOT follow the
default `org-gtd-<type-name>' convention.  Phase F parity test
consults this map for each type.")

(defun org-gtd-test--expected-command-for-type (type-name)
  "Return the symbol the `org-gtd-organize' transient should
dispatch to for TYPE-NAME.  Defaults to `org-gtd-<type-name>',
with `org-gtd-test--type-command-exceptions' overriding."
  (or (alist-get type-name org-gtd-test--type-command-exceptions)
      (intern (format "org-gtd-%s" type-name))))

(deftest transient/every-type-with-transient-key-appears-in-menu ()
  "Every built-in type declaring :transient-key has a matching suffix."
  (let ((transient-keys
         (mapcar #'car
                 (org-gtd-test--transient-key-commands 'org-gtd-organize))))
    (dolist (entry org-gtd-types)
      (let* ((type (car entry))
             (key (plist-get (cdr entry) :transient-key)))
        (when key
          (assert-true (member key transient-keys)))))))

(deftest transient/every-type-with-transient-key-dispatches-to-expected-command ()
  "Every built-in type's :transient-key suffix dispatches to the
command named by the <type-name> convention (plus known
exceptions listed in `org-gtd-test--type-command-exceptions')."
  (let ((pairs (org-gtd-test--transient-key-commands 'org-gtd-organize)))
    (dolist (entry org-gtd-types)
      (let* ((type (car entry))
             (key (plist-get (cdr entry) :transient-key))
             (expected (org-gtd-test--expected-command-for-type type)))
        (when key
          (let ((actual (cdr (assoc key pairs))))
            (assert-equal expected actual)))))))

(provide 'transient-registry-parity-test)

;;; transient-registry-parity-test.el ends here
