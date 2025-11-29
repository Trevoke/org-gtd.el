;;; example-test.el --- Example e-unit test for org-gtd -*- lexical-binding: t -*-

;; This is a sample e-unit test demonstrating the dual-framework setup.
;; Place e-unit tests in test-eunit/ directory.
;; Buttercup tests remain in test/ directory.

;; Note: (e-unit-initialize) is called via the Eunit config file

(deftest example-e-unit-test-passes ()
  "A simple passing test demonstrating e-unit works."
  (assert-equal 4 (+ 2 2)))

(deftest example-e-unit-string-test ()
  "Test string operations."
  (assert-equal "hello world" (concat "hello" " " "world")))

;;; example-test.el ends here
