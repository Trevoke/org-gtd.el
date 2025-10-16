;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-svg-render-test.el --- Unit tests for SVG rendering -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Unit tests for org-gtd-svg-render visual polish and feedback features.
;;
;; Test Coverage:
;; - Completed tasks (DONE/CNCL) render with reduced opacity (0.5)
;; - Priority A tasks use red color (#e74c3c)
;; - Priority B tasks use orange color (#f39c12)
;; - Priority C tasks use blue color (#3498db)
;; - Tag-based color coding (:urgent:, :important:, :waiting:, :someday:)
;; - Priority takes precedence over tag for color
;; - SVG tooltips include task metadata
;;

;;; Code:

(require 'buttercup)
(require 'org-gtd-svg-render)
(require 'org-gtd-graph-data)
(require 'svg)
(require 'dom)

;;;; Test Helpers

(defun org-gtd-svg-render-test--create-test-node (&rest plist)
  "Create a test node with properties from PLIST.
Defaults: id='test-id', title='Test Task', state='TODO'."
  (org-gtd-graph-node-create
   :id (or (plist-get plist :id) "test-id")
   :title (or (plist-get plist :title) "Test Task")
   :state (or (plist-get plist :state) "TODO")
   :category (or (plist-get plist :category) "Actions")
   :x (or (plist-get plist :x) 0)
   :y (or (plist-get plist :y) 0)
   :width (or (plist-get plist :width) 120)
   :height (or (plist-get plist :height) 40)
   :priority (plist-get plist :priority)
   :tags (plist-get plist :tags)
   :scheduled (plist-get plist :scheduled)
   :deadline (plist-get plist :deadline)))

(defun org-gtd-svg-render-test--render-node (node)
  "Render NODE to SVG and return the SVG object."
  (let ((svg (svg-create 200 100)))
    (org-gtd-svg-draw-node svg node)
    svg))

(defun org-gtd-svg-render-test--get-rect-opacity (svg)
  "Extract opacity from first rect element in SVG."
  (let* ((rect-element (dom-by-tag svg 'rect)))
    (when rect-element
      (dom-attr (car rect-element) 'fill-opacity))))

(defun org-gtd-svg-render-test--get-rect-fill (svg)
  "Extract fill color from first rect element in SVG."
  (let* ((rect-element (dom-by-tag svg 'rect)))
    (when rect-element
      (dom-attr (car rect-element) 'fill))))

(defun org-gtd-svg-render-test--get-title-text (svg)
  "Extract text from first title element in SVG."
  (let* ((title-elements (dom-by-tag svg 'title)))
    (when title-elements
      (dom-texts (car title-elements)))))

;;;; Completed Task Opacity Tests

(describe "org-gtd-svg-draw-node opacity for completed tasks"

  (it "renders DONE tasks with 0.5 opacity"
    (let* ((node (org-gtd-svg-render-test--create-test-node :state "DONE"))
           (svg (org-gtd-svg-render-test--render-node node))
           (opacity (org-gtd-svg-render-test--get-rect-opacity svg)))
      (expect opacity :to-equal "0.5")))

  (it "renders CNCL tasks with 0.5 opacity"
    (let* ((node (org-gtd-svg-render-test--create-test-node :state "CNCL"))
           (svg (org-gtd-svg-render-test--render-node node))
           (opacity (org-gtd-svg-render-test--get-rect-opacity svg)))
      (expect opacity :to-equal "0.5")))

  (it "renders TODO tasks with full opacity (1.0)"
    (let* ((node (org-gtd-svg-render-test--create-test-node :state "TODO"))
           (svg (org-gtd-svg-render-test--render-node node))
           (opacity (org-gtd-svg-render-test--get-rect-opacity svg)))
      (expect (or opacity "1.0") :to-equal "1.0")))

  (it "renders NEXT tasks with full opacity (1.0)"
    (let* ((node (org-gtd-svg-render-test--create-test-node :state "NEXT"))
           (svg (org-gtd-svg-render-test--render-node node))
           (opacity (org-gtd-svg-render-test--get-rect-opacity svg)))
      (expect (or opacity "1.0") :to-equal "1.0"))))

;;;; Priority Color Tests

(describe "org-gtd-svg-draw-node priority-based colors"

  (it "renders priority A tasks with red color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :priority "A"))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#e74c3c")))

  (it "renders priority B tasks with orange color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :priority "B"))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#f39c12")))

  (it "renders priority C tasks with blue color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :priority "C"))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#3498db")))

  (it "renders tasks without priority with default gray color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :priority nil))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#ecf0f1"))))

;;;; Tag-based Color Tests

(describe "org-gtd-svg-draw-node tag-based colors"

  (it "renders :urgent: tag with red color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :tags '("urgent")))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#e74c3c")))

  (it "renders :important: tag with orange color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :tags '("important")))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#f39c12")))

  (it "renders :waiting: tag with yellow color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :tags '("waiting")))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#f1c40f")))

  (it "renders :someday: tag with blue color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :tags '("someday")))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#3498db")))

  (it "renders other tags with light gray color"
    (let* ((node (org-gtd-svg-render-test--create-test-node :tags '("custom")))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#bdc3c7")))

  (it "uses first tag when multiple tags are present"
    (let* ((node (org-gtd-svg-render-test--create-test-node :tags '("waiting" "urgent")))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      (expect fill :to-equal "#f1c40f"))))

;;;; Priority Precedence Tests

(describe "org-gtd-svg-draw-node priority precedence over tags"

  (it "uses priority color even when tag is present"
    (let* ((node (org-gtd-svg-render-test--create-test-node
                  :priority "A"
                  :tags '("waiting")))
           (svg (org-gtd-svg-render-test--render-node node))
           (fill (org-gtd-svg-render-test--get-rect-fill svg)))
      ;; Should be red (priority A) not yellow (waiting tag)
      (expect fill :to-equal "#e74c3c"))))

;;;; SVG Tooltip Tests

(describe "org-gtd-svg-draw-node SVG tooltips"

  (it "includes task heading in tooltip"
    (let* ((node (org-gtd-svg-render-test--create-test-node :title "My Task"))
           (svg (org-gtd-svg-render-test--render-node node))
           (tooltip (org-gtd-svg-render-test--get-title-text svg)))
      (expect tooltip :to-match "My Task")))

  (it "includes TODO state in tooltip"
    (let* ((node (org-gtd-svg-render-test--create-test-node
                  :state "NEXT"
                  :title "Next Task"))
           (svg (org-gtd-svg-render-test--render-node node))
           (tooltip (org-gtd-svg-render-test--get-title-text svg)))
      (expect tooltip :to-match "NEXT")))

  (it "includes priority in tooltip when set"
    (let* ((node (org-gtd-svg-render-test--create-test-node
                  :priority "A"
                  :title "Priority Task"))
           (svg (org-gtd-svg-render-test--render-node node))
           (tooltip (org-gtd-svg-render-test--get-title-text svg)))
      (expect tooltip :to-match "Priority.*A")))

  (it "includes tags in tooltip when present"
    (let* ((node (org-gtd-svg-render-test--create-test-node
                  :tags '("urgent" "work")
                  :title "Tagged Task"))
           (svg (org-gtd-svg-render-test--render-node node))
           (tooltip (org-gtd-svg-render-test--get-title-text svg)))
      (expect tooltip :to-match "urgent")
      (expect tooltip :to-match "work")))

  (it "includes scheduled date in tooltip when set"
    (let* ((node (org-gtd-svg-render-test--create-test-node
                  :scheduled "2025-10-15"
                  :title "Scheduled Task"))
           (svg (org-gtd-svg-render-test--render-node node))
           (tooltip (org-gtd-svg-render-test--get-title-text svg)))
      (expect tooltip :to-match "2025-10-15")))

  (it "includes deadline in tooltip when set"
    (let* ((node (org-gtd-svg-render-test--create-test-node
                  :deadline "2025-10-20"
                  :title "Deadline Task"))
           (svg (org-gtd-svg-render-test--render-node node))
           (tooltip (org-gtd-svg-render-test--get-title-text svg)))
      (expect tooltip :to-match "2025-10-20"))))

(provide 'org-gtd-svg-render-test)

;;; org-gtd-svg-render-test.el ends here
