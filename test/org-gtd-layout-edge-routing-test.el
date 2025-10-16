;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)

;;; org-gtd-layout-edge-routing-test.el --- Tests for edge routing with waypoints -*- lexical-binding: t; -*-

;; Copyright © 2025 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Tests for the edge routing phase of the Sugiyama layout algorithm.
;; Verifies that multi-layer edges are routed with waypoints to avoid
;; overlapping with intermediate nodes.
;;
;;; Code:

(require 'buttercup)
(require 'org-gtd-graph-data)
(require 'org-gtd-layout)

;;;; Test Helpers

(defun org-gtd-layout-test--create-node (id title x y layer &optional width height)
  "Create a test node with specified position and layer.
ID and TITLE identify the node.
X, Y are pixel coordinates.
LAYER is the layer number.
WIDTH and HEIGHT default to 120 and 40."
  (org-gtd-graph-node-create
   :id id
   :title title
   :state "TODO"
   :category "Actions"
   :x x
   :y y
   :layer layer
   :width (or width 120)
   :height (or height 40)))

(defun org-gtd-layout-test--create-test-graph (&rest nodes)
  "Create a minimal graph containing NODES for testing."
  (let ((graph (org-gtd-graph-create
                :project-id "test-project"
                :project-name "Test"
                :root-ids '("node-a"))))
    (dolist (node nodes)
      (puthash (org-gtd-graph-node-id node) node (org-gtd-graph-nodes graph)))
    graph))

;;;; Edge Waypoint Calculation Tests

(describe "org-gtd-layout--calculate-edge-waypoints"

  (describe "adjacent-layer edges"

    (it "returns 2 points for nodes in consecutive layers"
      ;; Node at layer 0, directly above node at layer 1
      (let* ((from-node (org-gtd-layout-test--create-node
                         "node-a" "Node A" 100 0 0 120 40))
             (to-node (org-gtd-layout-test--create-node
                       "node-b" "Node B" 100 80 1 120 40))
             (graph (org-gtd-layout-test--create-test-graph from-node to-node))
             (points (org-gtd-layout--calculate-edge-waypoints graph from-node to-node)))

        ;; Should be straight line (2 points)
        (expect (length points) :to-equal 2)

        ;; First point: bottom-center of source
        (expect (car (nth 0 points)) :to-equal 160)   ; x = 100 + 120/2
        (expect (cadr (nth 0 points)) :to-equal 40)   ; y = 0 + 40

        ;; Second point: top-center of target
        (expect (car (nth 1 points)) :to-equal 160)   ; x = 100 + 120/2
        (expect (cadr (nth 1 points)) :to-equal 80))) ; y = 80

    (it "returns 2 points even when horizontally offset"
      ;; Adjacent layers, but different x positions
      (let* ((from-node (org-gtd-layout-test--create-node
                         "node-a" "Node A" 100 0 0 120 40))
             (to-node (org-gtd-layout-test--create-node
                       "node-b" "Node B" 250 80 1 120 40))
             (graph (org-gtd-layout-test--create-test-graph from-node to-node))
             (points (org-gtd-layout--calculate-edge-waypoints graph from-node to-node)))

        ;; Still just 2 points for adjacent layers
        (expect (length points) :to-equal 2))))

  (describe "multi-layer edges"

    (it "returns more than 2 points for nodes spanning multiple layers"
      ;; Node at layer 0, connecting to node at layer 3 (spans 3 layers)
      (let* ((from-node (org-gtd-layout-test--create-node
                         "node-a" "Node A" 100 0 0 120 40))
             (to-node (org-gtd-layout-test--create-node
                       "node-d" "Node D" 100 240 3 120 40))
             (graph (org-gtd-layout-test--create-test-graph from-node to-node))
             (points (org-gtd-layout--calculate-edge-waypoints graph from-node to-node)))

        ;; Should have waypoints (more than 2 points)
        (expect (length points) :to-be-greater-than 2)))

    (it "includes waypoint below source node"
      ;; Verify first waypoint drops down from source
      (let* ((from-node (org-gtd-layout-test--create-node
                         "node-a" "Node A" 100 0 0 120 40))
             (to-node (org-gtd-layout-test--create-node
                       "node-d" "Node D" 100 240 3 120 40))
             (graph (org-gtd-layout-test--create-test-graph from-node to-node))
             (points (org-gtd-layout--calculate-edge-waypoints graph from-node to-node)))

        ;; Second point should be below source (waypoint-down)
        (expect (cadr (nth 1 points)) :to-be-greater-than 40)))

    (it "includes waypoint above target node"
      ;; Verify last waypoint before target is above it
      (let* ((from-node (org-gtd-layout-test--create-node
                         "node-a" "Node A" 100 0 0 120 40))
             (to-node (org-gtd-layout-test--create-node
                       "node-d" "Node D" 100 240 3 120 40))
             (graph (org-gtd-layout-test--create-test-graph from-node to-node))
             (points (org-gtd-layout--calculate-edge-waypoints graph from-node to-node))
             (penultimate-point (nth (- (length points) 2) points)))

        ;; Penultimate point should be above target
        (expect (cadr penultimate-point) :to-be-less-than 240))))

  (describe "horizontal routing for offset nodes"

    (it "routes horizontally when target is offset"
      ;; Nodes at different x positions, multiple layers apart
      (let* ((from-node (org-gtd-layout-test--create-node
                         "node-a" "Node A" 100 0 0 120 40))
             (to-node (org-gtd-layout-test--create-node
                       "node-d" "Node D" 400 240 3 120 40))
             (graph (org-gtd-layout-test--create-test-graph from-node to-node))
             (points (org-gtd-layout--calculate-edge-waypoints graph from-node to-node)))

        ;; Should have 5 points for multi-layer routing
        (expect (length points) :to-equal 5)

        ;; Some waypoint should have different x than source center (160)
        (let ((has-horizontal-route
               (cl-some (lambda (point)
                          (not (= (car point) 160))) ; source center-x
                        (cdr points)))) ; skip first point
          (expect has-horizontal-route :to-be-truthy)))))

  (describe "same-column routing (vertically aligned nodes)"

    (it "adds horizontal detour when nodes are in same column"
      ;; Nodes at same x position (vertically aligned), multiple layers apart
      (let* ((from-node (org-gtd-layout-test--create-node
                         "node-a" "Node A" 100 0 0 120 40))
             (to-node (org-gtd-layout-test--create-node
                       "node-d" "Node D" 100 240 3 120 40))
             (graph (org-gtd-layout-test--create-test-graph from-node to-node))
             (points (org-gtd-layout--calculate-edge-waypoints graph from-node to-node)))

        ;; Should have 6 points for same-column routing (includes detour)
        (expect (length points) :to-equal 6)))

    (it "routes to appropriate side based on surrounding nodes"
      ;; Verify detour direction depends on node placement
      ;; When no nodes to the right, routes right
      (let* ((from-node (org-gtd-layout-test--create-node
                         "node-a" "Node A" 100 0 0 120 40))
             (to-node (org-gtd-layout-test--create-node
                       "node-d" "Node D" 100 240 3 120 40))
             (graph (org-gtd-layout-test--create-test-graph from-node to-node))
             (points (org-gtd-layout--calculate-edge-waypoints graph from-node to-node))
             (from-center-x (+ 100 (/ 120 2))) ; 160
             ;; Waypoints 2 and 3 should be the detour points
             (detour-point-1 (nth 2 points))
             (detour-point-2 (nth 3 points)))

        ;; Detour should be to the right (no nodes on right side)
        (expect (car detour-point-1) :to-be-greater-than from-center-x)
        (expect (car detour-point-2) :to-be-greater-than from-center-x)

        ;; Both detour points should have same x (vertical detour line)
        (expect (car detour-point-1) :to-equal (car detour-point-2))))))

(provide 'org-gtd-layout-edge-routing-test)

;;; org-gtd-layout-edge-routing-test.el ends here
