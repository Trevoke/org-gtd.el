;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-layout-bounds-test.el --- Tests for graph bounds calculation -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for graph bounds calculation including edge waypoints.
;;
;;; Code:

(require 'buttercup)
(require 'org-gtd-graph-data)
(require 'org-gtd-layout)

;;;; Test Helpers

(defun org-gtd-layout-bounds-test--create-simple-graph ()
  "Create a simple graph for testing bounds calculation."
  (let ((graph (org-gtd-graph-create
                :project-id "project-1"
                :project-name "Test Project"
                :root-ids '("node-a"))))

    ;; Add two nodes in same column (x=100)
    (puthash "node-a"
             (org-gtd-graph-node-create
              :id "node-a"
              :title "Node A"
              :state "TODO"
              :category "Actions"
              :x 100
              :y 0
              :layer 0
              :width 120
              :height 40)
             (org-gtd-graph-nodes graph))

    (puthash "node-b"
             (org-gtd-graph-node-create
              :id "node-b"
              :title "Node B"
              :state "TODO"
              :category "Actions"
              :x 100
              :y 240
              :layer 3
              :width 120
              :height 40)
             (org-gtd-graph-nodes graph))

    ;; Add edge with waypoints that route outside node bounds
    (let ((edge (org-gtd-graph-edge-create
                 :from-id "node-a"
                 :to-id "node-b"
                 :type :blocks)))
      ;; Calculate waypoints (will include detour to x=250)
      (let* ((from-node (org-gtd-graph-data-get-node graph "node-a"))
             (to-node (org-gtd-graph-data-get-node graph "node-b"))
             (points (org-gtd-layout--calculate-edge-waypoints from-node to-node)))
        (setf (org-gtd-graph-edge-points edge) points))
      (push edge (org-gtd-graph-edges graph)))

    graph))

;;;; Bounds Calculation Tests

(describe "org-gtd-layout-get-graph-bounds"

  (describe "node-only bounds"

    (it "includes node positions in bounds"
      (let* ((graph (org-gtd-layout-bounds-test--create-simple-graph))
             (bounds (org-gtd-layout-get-graph-bounds graph))
             (min-x (nth 0 bounds))
             (max-x (nth 2 bounds)))

        ;; Nodes are at x=100 with width=120
        ;; min-x should be at node start
        (expect min-x :to-equal 100)
        ;; max-x should at least cover the nodes (220) but will extend for edge detour
        (expect (>= max-x 220) :to-be-truthy))))

  (describe "edge waypoint inclusion"

    (it "includes edge waypoints in bounds calculation"
      (let* ((graph (org-gtd-layout-bounds-test--create-simple-graph))
             (bounds (org-gtd-layout-get-graph-bounds graph))
             (min-x (nth 0 bounds))
             (max-x (nth 2 bounds)))

        ;; Edge routes to detour at x=250 (160 + 60 + 30)
        ;; With edges, max-x should extend to include detour
        (expect max-x :to-be-greater-than 220)))))

(provide 'org-gtd-layout-bounds-test)

;;; org-gtd-layout-bounds-test.el ends here
