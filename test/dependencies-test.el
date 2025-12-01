;; -*- lexical-binding: t; coding: utf-8 -*-

;; All tests migrated to test-eunit/unit/dependencies-test.el:
;;
;; org-gtd-dependencies-has-path-p (4 tests):
;; - dependencies/has-path-nil-when-no-connection
;; - dependencies/has-path-t-when-direct-dependency
;; - dependencies/has-path-t-when-transitive-dependency
;; - dependencies/has-path-nil-for-same-task
;;
;; org-gtd-dependencies-validate-acyclic (3 tests):
;; - dependencies/validate-acyclic-accepts-acyclic
;; - dependencies/validate-acyclic-rejects-direct-cycle
;; - dependencies/validate-acyclic-rejects-transitive-cycle
;;
;; org-gtd-dependencies-create (2 tests):
;; - dependencies/create-bidirectional-dependency
;; - dependencies/create-prevents-cyclic
;;
;; This file is kept for documentation. The tests now run via e-unit.
