;;; organize-core-test.el --- Tests for process-heading / process-project -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; Tests for the pipeline primitives in org-gtd-organize-core.
;;; Code:

(require 'cl-lib)
(require 'e-unit)
(require 'org-gtd)
(require 'org-gtd-organize-core)
(require 'org-gtd-hooks)
(require 'org-gtd-types)

(e-unit-initialize)

(deftest process-heading-runs-pipeline-in-order ()
  "process-heading runs before-organize, organize-fn, after-organize,
before-file, after-file in that order for a fake type."
  (let* ((log nil)
         (record (lambda (tag) (lambda (_pom) (push tag log))))
         (org-gtd-before-organize-hook (list (funcall record 'b-org)))
         (org-gtd-after-organize-hook  (list (funcall record 'a-org)))
         (org-gtd-before-file-hook     (list (funcall record 'b-file)))
         (org-gtd-after-file-hook      (list (funcall record 'a-file)))
         (org-gtd-types
          `((fake :org-gtd "Fake" :state nil :properties nil
                  :organize-fn ,(lambda (&rest _) (push 'org-fn log))))))
    (with-temp-buffer
      (org-mode)
      (insert "* Thing\n")
      (goto-char (point-min))
      (org-gtd-process-heading (point-marker) 'fake))
    (assert-equal '(b-org org-fn a-org b-file a-file) (reverse log))))

(deftest process-heading-respects-reactivate-support ()
  "process-heading calls org-gtd-save-state when the type declares
:supports reactivate."
  (let ((save-state-called nil))
    (cl-letf (((symbol-function 'org-gtd-save-state)
               (lambda () (setq save-state-called t))))
      (let ((org-gtd-types
             '((fake :org-gtd "Fake" :state nil :properties nil
                     :organize-fn ignore
                     :supports (reactivate)))))
        (with-temp-buffer
          (org-mode)
          (insert "* Thing\n")
          (goto-char (point-min))
          (org-gtd-process-heading (point-marker) 'fake))
        (assert-true save-state-called)))))

(deftest process-heading-skips-save-state-without-reactivate ()
  "process-heading does not call org-gtd-save-state when reactivate
is not declared."
  (let ((save-state-called nil))
    (cl-letf (((symbol-function 'org-gtd-save-state)
               (lambda () (setq save-state-called t))))
      (let ((org-gtd-types
             '((fake :org-gtd "Fake" :state nil :properties nil
                     :organize-fn ignore))))
        (with-temp-buffer
          (org-mode)
          (insert "* Thing\n")
          (goto-char (point-min))
          (org-gtd-process-heading (point-marker) 'fake))
        (assert-nil save-state-called)))))

(provide 'organize-core-test)
;;; organize-core-test.el ends here
