;;; org-gtd-graph-debug.el --- Debug helpers for graph visualization -*- lexical-binding: t; -*-

(require 'org-gtd-graph-data)

(defun org-gtd-graph-debug-print-edges (graph)
  "Print all edges in GRAPH to the messages buffer."
  (let ((edges (org-gtd-graph-edges graph)))
    (message "=== GRAPH EDGES (Total: %d) ===" (length edges))
    (dolist (edge edges)
      (let* ((from-id (org-gtd-graph-edge-from-id edge))
             (to-id (org-gtd-graph-edge-to-id edge))
             (from-node (org-gtd-graph-data-get-node graph from-id))
             (to-node (org-gtd-graph-data-get-node graph to-id))
             (from-title (if from-node (org-gtd-graph-node-title from-node) "UNKNOWN"))
             (to-title (if to-node (org-gtd-graph-node-title to-node) "UNKNOWN")))
        (message "  %s -> %s" from-title to-title)
        (message "    (from: %s, to: %s)" from-id to-id)))))

(defun org-gtd-graph-debug-print-incoming-edges (graph node-id)
  "Print all incoming edges to NODE-ID in GRAPH."
  (let* ((node (org-gtd-graph-data-get-node graph node-id))
         (node-title (if node (org-gtd-graph-node-title node) "UNKNOWN"))
         (incoming (org-gtd-graph-data-get-incoming-edges graph node-id)))
    (message "=== INCOMING EDGES TO: %s ===" node-title)
    (message "  Node ID: %s" node-id)
    (message "  Total incoming edges: %d" (length incoming))
    (dolist (edge incoming)
      (let* ((from-id (org-gtd-graph-edge-from-id edge))
             (from-node (org-gtd-graph-data-get-node graph from-id))
             (from-title (if from-node (org-gtd-graph-node-title from-node) "UNKNOWN")))
        (message "    FROM: %s (id: %s)" from-title from-id)))))

(provide 'org-gtd-graph-debug)
;;; org-gtd-graph-debug.el ends here
