;;; graph-view-export-test.el --- Tests for graph view export and display -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for org-gtd-graph-view export functions and display features.
;;
;; Test Coverage:
;; - SVG export
;; - DOT export
;; - ASCII export
;; - Buffer naming (project name vs UUID)
;; - Project node positioning (bottom placement, leaf task connections)
;;
;; Migrated from test/org-gtd-graph-view-test.el (buttercup)
;; Note: Pending tests (xit) for add-dependency/add-blocker not migrated.

;;; Code:

(require 'ogt-eunit-prelude "test/helpers/prelude.el")
(require 'org-gtd-graph-view)
(require 'org-gtd-graph-data)
(require 'org-gtd-dependencies)
(require 'org-gtd-accessors)
(require 'org-gtd-files)
(require 'org-gtd-core)
(require 'org-gtd-dag-draw)
(require 'dag-draw)
(require 'dag-draw-core)

(e-unit-initialize)

(around-each (proceed context)
  (ogt-eunit-with-mock-gtd
    (funcall proceed context)))

;;;; Test helpers

(defun graph-view-test--create-project (title)
  "Create a test project with TITLE and return its marker.
Creates a minimal project with one task in the mock GTD file."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert (format "* %s\n" title))
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (insert (format ":ID: project-%s\n" (make-temp-name "id")))
    (insert ":END:\n")
    (insert "** TODO Task 1\n")
    (insert ":PROPERTIES:\n")
    (insert (format ":ID: task-%s\n" (make-temp-name "id")))
    (insert ":ORG_GTD: Actions\n")
    (insert (format ":ORG_GTD_PROJECT_IDS: project-%s\n" (make-temp-name "id")))
    (insert ":END:\n")
    (goto-char (point-min))
    (search-forward title)
    (org-back-to-heading t)
    (basic-save-buffer)
    (point-marker)))

;;;; Export tests

(deftest graph-view-export/exports-graph-to-svg-file ()
  "Exports graph to SVG file."
  (let* ((project-marker (graph-view-test--create-project "Test Project"))
         (export-file (expand-file-name "graph-export.svg" temporary-file-directory)))
    (unwind-protect
        (progn
          ;; Set up minimal graph view state (without full rendering)
          (with-current-buffer (get-buffer-create "*Test Graph Export*")
            (setq-local org-gtd-graph-view--graph
                        (org-gtd-graph-data--extract-from-project project-marker))
            (setq-local org-gtd-graph-ui--selected-node-id nil)

            ;; Export to SVG
            (org-gtd-graph-export-svg export-file))

          ;; Verify file exists and contains SVG
          (assert-true (file-exists-p export-file))
          (with-temp-buffer
            (insert-file-contents export-file)
            (assert-match "<svg" (buffer-string))
            (assert-match "Test Project" (buffer-string))))
      ;; Cleanup
      (when (file-exists-p export-file)
        (delete-file export-file))
      (when (get-buffer "*Test Graph Export*")
        (kill-buffer "*Test Graph Export*")))))

(deftest graph-view-export/exports-graph-to-dot-file ()
  "Exports graph to DOT file."
  (let* ((project-marker (graph-view-test--create-project "Test Project"))
         (export-file (expand-file-name "graph-export.dot" temporary-file-directory)))
    (unwind-protect
        (progn
          ;; Set up minimal graph view state (without full rendering)
          (with-current-buffer (get-buffer-create "*Test Graph Export*")
            (setq-local org-gtd-graph-view--graph
                        (org-gtd-graph-data--extract-from-project project-marker))
            (setq-local org-gtd-graph-ui--selected-node-id nil)

            ;; Export to DOT
            (org-gtd-graph-export-dot export-file))

          ;; Verify file exists and contains DOT format
          (assert-true (file-exists-p export-file))
          (with-temp-buffer
            (insert-file-contents export-file)
            (assert-match "digraph G" (buffer-string))
            (assert-match "Test Project" (buffer-string))))
      ;; Cleanup
      (when (file-exists-p export-file)
        (delete-file export-file))
      (when (get-buffer "*Test Graph Export*")
        (kill-buffer "*Test Graph Export*")))))

(deftest graph-view-export/exports-graph-to-ascii-file ()
  "Exports graph to ASCII file."
  (let* ((project-marker (graph-view-test--create-project "Test Project"))
         (export-file (expand-file-name "graph-export.txt" temporary-file-directory)))
    (unwind-protect
        (progn
          ;; Set up minimal graph view state (without full rendering)
          (with-current-buffer (get-buffer-create "*Test Graph Export*")
            (setq-local org-gtd-graph-view--graph
                        (org-gtd-graph-data--extract-from-project project-marker))
            (setq-local org-gtd-graph-ui--selected-node-id nil)

            ;; Export to ASCII
            (org-gtd-graph-export-ascii export-file))

          ;; Verify file exists and contains ASCII art
          (assert-true (file-exists-p export-file))
          (with-temp-buffer
            (insert-file-contents export-file)
            (assert-match "Test Project" (buffer-string))))
      ;; Cleanup
      (when (file-exists-p export-file)
        (delete-file export-file))
      (when (get-buffer "*Test Graph Export*")
        (kill-buffer "*Test Graph Export*")))))

;;;; Buffer naming test

(deftest graph-view-buffer/uses-project-name-instead-of-uuid ()
  "Uses project name in buffer name instead of UUID."
  (with-current-buffer (org-gtd--default-file)
    (goto-char (point-max))
    (insert "* Build the spaceship\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GTD: Projects\n")
    (insert ":END:\n")
    (org-back-to-heading)
    (let ((project-id (org-id-get-create)))
      (save-buffer)

      (let ((buffer-name (org-gtd-graph-view--buffer-name "Build the spaceship")))
        (assert-equal "*Org GTD Graph: Build the spaceship*" buffer-name)
        (refute-match project-id buffer-name)))))

;;;; Project node positioning tests

(deftest graph-view-positioning/places-project-node-at-bottom ()
  "Places project node at the bottom (receives edges, not a root)."
  (let (project-id task-a-id task-b-id)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Build the spaceship\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (org-back-to-heading)
      (setq project-id (org-id-get-create))
      (goto-char (point-max))
      (insert "** TODO Design hull\n")
      (org-back-to-heading)
      (setq task-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (goto-char (point-max))
      (insert "** TODO Build engine\n")
      (org-back-to-heading)
      (setq task-b-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Set first tasks and create dependency chain
      (goto-char (point-min))
      (org-back-to-heading)
      (org-entry-put (point) "ORG_GTD_FIRST_TASKS" task-a-id)
      (org-gtd-dependencies-create task-a-id task-b-id)
      (save-buffer))

    (let* ((project-marker (org-id-find project-id t))
           (graph (org-gtd-graph-data--extract-from-project project-marker))
           (root-ids (org-gtd-graph-root-ids graph))
           (edges (org-gtd-graph-edges graph))
           ;; Count edges TO project (project receives edges from leaf tasks)
           (incoming-to-project (seq-filter
                                 (lambda (e)
                                   (equal (org-gtd-graph-edge-to-id e) project-id))
                                 edges))
           ;; Count edges FROM project (should be none now)
           (outgoing-from-project (seq-filter
                                   (lambda (e)
                                     (equal (org-gtd-graph-edge-from-id e) project-id))
                                   edges)))
      ;; Project should NOT be a root (it's the sink/finish line)
      (assert-nil (member project-id root-ids))
      ;; First task should be a root
      (assert-true (member task-a-id root-ids))
      ;; Project should have incoming edges (from leaf tasks)
      (assert-true (> (length incoming-to-project) 0))
      ;; Project should NOT have outgoing edges
      (assert-equal 0 (length outgoing-from-project)))))

(deftest graph-view-positioning/connects-leaf-tasks-to-project-node ()
  "Connects leaf tasks to project node (tasks block outcome)."
  (let (project-id task-a-id task-b-id)
    (with-current-buffer (org-gtd--default-file)
      (goto-char (point-max))
      (insert "* Build the spaceship\n")
      (insert ":PROPERTIES:\n")
      (insert ":ORG_GTD: Projects\n")
      (insert ":END:\n")
      (org-back-to-heading)
      (setq project-id (org-id-get-create))
      (goto-char (point-max))
      (insert "** TODO Design hull\n")
      (org-back-to-heading)
      (setq task-a-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)
      (goto-char (point-max))
      (insert "** TODO Build engine\n")
      (org-back-to-heading)
      (setq task-b-id (org-id-get-create))
      (org-entry-put (point) "ORG_GTD_PROJECT_IDS" project-id)

      ;; Set first tasks and create dependency: A -> B
      (goto-char (point-min))
      (org-back-to-heading)
      (org-entry-put (point) "ORG_GTD_FIRST_TASKS" task-a-id)
      (org-gtd-dependencies-create task-a-id task-b-id)
      (save-buffer))

    (let* ((project-marker (org-id-find project-id t))
           (graph (org-gtd-graph-data--extract-from-project project-marker))
           (edges (org-gtd-graph-edges graph))
           ;; Find edge TO the project (leaf task -> project)
           (edges-to-project (seq-filter
                              (lambda (e)
                                (equal (org-gtd-graph-edge-to-id e) project-id))
                              edges)))
      ;; Task B is the leaf (no dependents), should point to project
      (assert-true (> (length edges-to-project) 0))
      (assert-true (seq-some (lambda (e)
                               (equal (org-gtd-graph-edge-from-id e) task-b-id))
                             edges-to-project)))))

(provide 'graph-view-export-test)

;;; graph-view-export-test.el ends here
