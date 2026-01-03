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
  ;; Help and quit
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "?")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "q")))
  ;; Navigation
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "n")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "p")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "G")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "TAB"))))

(deftest graph-mode/keymap-has-tier1-add-bindings ()
  "Graph view mode keymap should have single-key add task bindings."
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "r")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "s")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "b"))))

(deftest graph-mode/keymap-has-tier1-modify-bindings ()
  "Graph view mode keymap should have single-key modify bindings."
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "B")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "S"))))

(deftest graph-mode/keymap-has-tier1-view-bindings ()
  "Graph view mode keymap should have view operation bindings."
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "v")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "g"))))

(deftest graph-mode/keymap-has-tier2-task-ops ()
  "Graph view mode keymap should have t-prefixed task operations."
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "t t")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "t r")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "t d")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "t e")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "t i"))))

(deftest graph-mode/keymap-has-tier2-project-and-view-ops ()
  "Graph view mode keymap should have project and view operations."
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "I")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "Q")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "x s")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "x d")))
  (assert-true (lookup-key org-gtd-graph-view-mode-map (kbd "x a"))))

(provide 'graph-mode-test)

;;; graph-mode-test.el ends here
