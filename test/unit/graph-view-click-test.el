;;; graph-view-click-test.el --- Tests for graph view click handling -*- lexical-binding: t -*-

(require 'e-unit)
(require 'org-gtd-graph-view)

;; Initialize e-unit short syntax
(e-unit-initialize)

;;;; org-gtd-graph-view--node-at-position tests

(deftest graph-view-click/returns-node-id-when-inside-bounds ()
  "Returns node-id when click is inside node bounds."
  (let ((org-gtd-graph-view--node-bounds
         '(("node-1" . (0 0 10 3))
           ("node-2" . (15 0 12 3)))))
    (assert-equal "node-1" (org-gtd-graph-view--node-at-position 5 1))))

(deftest graph-view-click/returns-correct-node-when-multiple-exist ()
  "Returns correct node when multiple nodes exist."
  (let ((org-gtd-graph-view--node-bounds
         '(("node-1" . (0 0 10 3))
           ("node-2" . (15 0 12 3)))))
    (assert-equal "node-2" (org-gtd-graph-view--node-at-position 20 2))))

(deftest graph-view-click/returns-nil-when-click-in-empty-space ()
  "Returns nil when click is in empty space."
  (let ((org-gtd-graph-view--node-bounds
         '(("node-1" . (0 0 10 3)))))
    (assert-nil (org-gtd-graph-view--node-at-position 50 50))))

(deftest graph-view-click/returns-nil-when-bounds-nil ()
  "Returns nil when node-bounds is nil."
  (let ((org-gtd-graph-view--node-bounds nil))
    (assert-nil (org-gtd-graph-view--node-at-position 5 5))))

;;;; org-gtd-graph-view-click-select tests

(deftest graph-view-click/select-calls-select-node-when-inside ()
  "Calls select-node when clicking inside a node."
  (let ((org-gtd-graph-view--node-bounds '(("node-1" . (0 0 10 3))))
        (org-gtd-graph-view--render-mode 'ascii)
        (selected-node nil))
    ;; Stub extract-click-coords to return fixed coordinates
    (cl-letf (((symbol-function 'org-gtd-graph-view--extract-click-coords)
               (lambda (_pos) '(5 . 1)))
              ((symbol-function 'org-gtd-graph-ui-select-node)
               (lambda (node-id) (setq selected-node node-id))))
      (org-gtd-graph-view-click-select 'ignored-event)
      (assert-equal "node-1" selected-node))))

(deftest graph-view-click/select-does-nothing-when-empty-space ()
  "Does nothing when clicking empty space."
  (let ((org-gtd-graph-view--node-bounds '(("node-1" . (0 0 10 3))))
        (org-gtd-graph-view--render-mode 'ascii)
        (selected-node nil))
    ;; Stub extract-click-coords to return coordinates outside any node
    (cl-letf (((symbol-function 'org-gtd-graph-view--extract-click-coords)
               (lambda (_pos) '(50 . 50)))
              ((symbol-function 'org-gtd-graph-ui-select-node)
               (lambda (node-id) (setq selected-node node-id))))
      (org-gtd-graph-view-click-select 'ignored-event)
      (assert-nil selected-node))))

(provide 'graph-view-click-test)
;;; graph-view-click-test.el ends here
