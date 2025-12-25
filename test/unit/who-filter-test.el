;;; who-filter-test.el --- Tests for who filter -*- lexical-binding: t; coding: utf-8 -*-

(require 'ogt-eunit-prelude
         (concat (file-name-directory
                  (or load-file-name byte-compile-current-file buffer-file-name))
                 "../helpers/prelude.el"))

;;; Skip Function with Who Filter

(deftest who-filter/matches-delegated-to-person ()
  "Skip function keeps items delegated to specified person."
  (with-temp-buffer
    (org-mode)
    (insert "* WAIT Delegated task
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Alice
:END:
")
    (goto-char (point-min))
    (let* ((spec '((type . delegated)
                   (who . "Alice")))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      ;; Should NOT skip (nil means include)
      (assert-nil (funcall skip-fn)))))

(deftest who-filter/skips-delegated-to-other-person ()
  "Skip function excludes items delegated to different person."
  (with-temp-buffer
    (org-mode)
    (insert "* WAIT Delegated task
:PROPERTIES:
:ORG_GTD: Delegated
:DELEGATED_TO: Bob
:END:
")
    (goto-char (point-min))
    (let* ((spec '((type . delegated)
                   (who . "Alice")))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      ;; Should skip (returns end position)
      (assert-true (funcall skip-fn)))))

(deftest who-filter/nil-matches-missing-delegated-to ()
  "Who filter with nil matches items without DELEGATED_TO."
  (with-temp-buffer
    (org-mode)
    (insert "* WAIT Delegated task
:PROPERTIES:
:ORG_GTD: Delegated
:END:
")
    (goto-char (point-min))
    (let* ((spec '((type . delegated)
                   (who . nil)))
           (skip-fn (org-gtd-view-lang--build-skip-function spec)))
      ;; Should NOT skip - item is missing who
      (assert-nil (funcall skip-fn)))))

(provide 'who-filter-test)
;;; who-filter-test.el ends here
