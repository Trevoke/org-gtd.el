;;; graph-mode-test.el --- Unit tests for graph mode -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-graph-mode, including evil-mode integration.
;;

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-graph-mode)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;; Evil-mode integration tests

(deftest graph-mode/evil-integration-registered ()
  "Verifies evil-set-initial-state is registered for graph-view-mode.
This ensures evil users get emacs state by default, avoiding C-z conflicts."
  ;; Check that after-load-alist has an entry for 'evil
  (let ((evil-entry (assq 'evil after-load-alist)))
    (assert-true evil-entry)
    ;; The entry should contain code that references org-gtd-graph-view-mode
    ;; (may be byte-compiled, so check string representation)
    (let ((forms (cdr evil-entry)))
      (assert-true
       (cl-some (lambda (form)
                  (string-match-p "org-gtd-graph-view-mode"
                                  (format "%S" form)))
                forms)))))

(deftest graph-mode/derived-from-special-mode ()
  "Graph view mode should be derived from special-mode."
  (assert-equal 'special-mode (get 'org-gtd-graph-view-mode 'derived-mode-parent)))

(deftest graph-mode/keymap-has-essential-bindings ()
  "Graph view mode keymap should have essential navigation bindings."
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "?")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "q")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "n")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "p"))))

(provide 'graph-mode-test)

;;; graph-mode-test.el ends here
