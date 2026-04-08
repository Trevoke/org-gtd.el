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
    (cl-letf (((symbol-function 'org-gtd-refile--do) (lambda (&rest _) nil)))
      (with-temp-buffer
        (org-mode)
        (insert "* Thing\n")
        (goto-char (point-min))
        (org-gtd-process-heading (point-marker) 'fake)))
    (assert-equal '(b-org org-fn a-org b-file a-file) (reverse log))))

(deftest process-heading-respects-reactivate-support ()
  "process-heading calls org-gtd-save-state when the type declares
:supports reactivate."
  (let ((save-state-called nil))
    (cl-letf (((symbol-function 'org-gtd-save-state)
               (lambda () (setq save-state-called t)))
              ((symbol-function 'org-gtd-refile--do) (lambda (&rest _) nil)))
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
               (lambda () (setq save-state-called t)))
              ((symbol-function 'org-gtd-refile--do) (lambda (&rest _) nil)))
      (let ((org-gtd-types
             '((fake :org-gtd "Fake" :state nil :properties nil
                     :organize-fn ignore))))
        (with-temp-buffer
          (org-mode)
          (insert "* Thing\n")
          (goto-char (point-min))
          (org-gtd-process-heading (point-marker) 'fake))
        (assert-nil save-state-called)))))

(deftest run-disposition-list-calls-refile ()
  "Disposition 'list dispatches to org-gtd-refile--do."
  (let ((called-with nil)
        (org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil
                 :organize-fn ignore :disposition list))))
    (cl-letf (((symbol-function 'org-gtd-refile--do)
               (lambda (type template) (setq called-with (list type template)))))
      (with-temp-buffer
        (org-mode)
        (insert "* Thing\n")
        (goto-char (point-min))
        (org-gtd-process-heading (point-marker) 'fake)))
    (assert-equal "Fake" (car called-with))
    (assert-true (stringp (cadr called-with)))))

(deftest run-disposition-respects-skip-refile ()
  "When skip-refile is set, dispatch calls update-in-place, not refile."
  (let ((refile-called nil)
        (update-called nil)
        (org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil
                 :organize-fn ignore :disposition list))))
    (cl-letf (((symbol-function 'org-gtd-refile--do)
               (lambda (&rest _) (setq refile-called t)))
              ((symbol-function 'org-gtd-organize--update-in-place)
               (lambda () (setq update-called t))))
      (with-temp-buffer
        (org-mode)
        (insert "* Thing\n")
        (goto-char (point-min))
        (let ((org-gtd-clarify--skip-refile t))
          (org-gtd-process-heading (point-marker) 'fake))))
    (assert-nil refile-called)
    (assert-true update-called)))

(deftest run-disposition-unknown-disposition-errors ()
  "An unrecognized :disposition signals a user-error."
  (let ((org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil
                 :organize-fn ignore :disposition weird-value))))
    (with-temp-buffer
      (org-mode)
      (insert "* Thing\n")
      (goto-char (point-min))
      (assert-raises 'error
        (org-gtd-process-heading (point-marker) 'fake)))))

(provide 'organize-core-test)
;;; organize-core-test.el ends here
