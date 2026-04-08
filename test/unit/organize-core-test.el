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
         (org-gtd-organize-hooks nil)
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

(deftest process-heading-runs-organize-apply-hooks ()
  "process-heading fires each function in `org-gtd-organize-hooks'
for every type, between :organize-fn and :after-organize."
  (let* ((log nil)
         (sentinel (lambda () (push 'apply-hook log)))
         (org-gtd-organize-hooks (list sentinel))
         (org-gtd-before-organize-hook nil)
         (org-gtd-after-organize-hook
          (list (lambda (_pom) (push 'after-organize log))))
         (org-gtd-before-file-hook nil)
         (org-gtd-after-file-hook nil)
         (org-gtd-types
          `((fake :org-gtd "Fake" :state nil :properties nil
                  :organize-fn ,(lambda (&rest _) (push 'org-fn log))))))
    (cl-letf (((symbol-function 'org-gtd-refile--do) (lambda (&rest _) nil)))
      (with-temp-buffer
        (org-mode)
        (insert "* Thing\n")
        (goto-char (point-min))
        (org-gtd-process-heading (point-marker) 'fake)))
    (assert-equal '(org-fn apply-hook after-organize) (reverse log))))

(deftest process-heading-respects-reactivate-support ()
  "process-heading calls org-gtd-save-state when the type declares
:supports reactivate."
  (let ((save-state-called nil)
        (org-gtd-organize-hooks nil))
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
  (let ((save-state-called nil)
        (org-gtd-organize-hooks nil))
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
        (org-gtd-organize-hooks nil)
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
        (org-gtd-organize-hooks nil)
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
  (let ((org-gtd-organize-hooks nil)
        (org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil
                 :organize-fn ignore :disposition weird-value))))
    (with-temp-buffer
      (org-mode)
      (insert "* Thing\n")
      (goto-char (point-min))
      (assert-raises 'error
        (org-gtd-process-heading (point-marker) 'fake)))))

(deftest clear-foreign-properties-removes-previous-type-only ()
  "Properties declared on the previous type but not the new type are cleared."
  (let ((org-gtd-types
         '((from-type :org-gtd "From" :state nil
                      :properties
                      ((:a :org-property "PROP_A" :type text)
                       (:b :org-property "PROP_B" :type text)))
           (to-type   :org-gtd "To" :state nil
                      :properties
                      ((:b :org-property "PROP_B" :type text)
                       (:c :org-property "PROP_C" :type text))))))
    (with-temp-buffer
      (org-mode)
      (insert "* Thing\n:PROPERTIES:\n:ORG_GTD: From\n:PROP_A: a-val\n:PROP_B: b-val\n:END:\n")
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (org-gtd--clear-foreign-properties 'to-type)
      (assert-nil (org-entry-get (point) "PROP_A"))
      (assert-equal "b-val" (org-entry-get (point) "PROP_B")))))

(deftest clear-foreign-properties-noop-when-no-previous-type ()
  "With no ORG_GTD set, the function touches nothing."
  (let ((org-gtd-types
         '((to-type :org-gtd "To" :state nil
                    :properties ((:a :org-property "PROP_A" :type text))))))
    (with-temp-buffer
      (org-mode)
      (insert "* Thing\n:PROPERTIES:\n:SOME_OTHER: val\n:END:\n")
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (org-gtd--clear-foreign-properties 'to-type)
      (assert-equal "val" (org-entry-get (point) "SOME_OTHER")))))

(deftest clear-foreign-properties-noop-when-previous-type-unknown ()
  "If ORG_GTD names a type not in the registry, do nothing."
  (let ((org-gtd-types
         '((to-type :org-gtd "To" :state nil
                    :properties ((:a :org-property "PROP_A" :type text))))))
    (with-temp-buffer
      (org-mode)
      (insert "* Thing\n:PROPERTIES:\n:ORG_GTD: Mystery\n:PROP_A: a-val\n:END:\n")
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (org-gtd--clear-foreign-properties 'to-type)
      (assert-equal "a-val" (org-entry-get (point) "PROP_A")))))

(deftest process-project-calls-project-fn-with-pom ()
  "process-project invokes the type's :project-fn with POM."
  (let* ((received-pom nil)
         (received-config nil)
         (org-gtd-types
          `((fake :org-gtd "Fake" :state nil :properties nil
                  :supports (project-handler)
                  :project-fn ,(lambda (pom config)
                                 (setq received-pom pom)
                                 (setq received-config config))))))
    (with-temp-buffer
      (org-mode)
      (insert "* Project\n")
      (goto-char (point-min))
      (let ((m (point-marker)))
        (org-gtd-process-project m 'fake '((:when . "2026-05-01")))
        (assert-same m received-pom)
        (assert-equal '((:when . "2026-05-01")) received-config)))))

(deftest process-project-errors-without-project-handler-support ()
  "Types that do not declare :supports project-handler raise user-error."
  (let ((org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil))))
    (with-temp-buffer
      (org-mode)
      (insert "* Project\n")
      (goto-char (point-min))
      (assert-raises 'user-error
        (org-gtd-process-project (point-marker) 'fake)))))

(deftest process-project-errors-when-project-fn-missing ()
  "A project-handler type without :project-fn raises user-error."
  (let ((org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil
                 :supports (project-handler)))))
    (with-temp-buffer
      (org-mode)
      (insert "* Project\n")
      (goto-char (point-min))
      (assert-raises 'user-error
        (org-gtd-process-project (point-marker) 'fake)))))

(deftest dispatch-plain-heading-calls-process-heading ()
  "A heading with no ORG_GTD and no project ids dispatches to process-heading."
  (let ((heading-called nil)
        (project-called nil)
        (org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil))))
    (cl-letf (((symbol-function 'org-gtd-process-heading)
               (lambda (&rest _) (setq heading-called t)))
              ((symbol-function 'org-gtd-process-project)
               (lambda (&rest _) (setq project-called t))))
      (with-temp-buffer
        (org-mode)
        (insert "* Thing\n")
        (goto-char (point-min))
        (org-gtd--dispatch 'fake)))
    (assert-true heading-called)
    (assert-nil project-called)))

(deftest dispatch-project-heading-with-project-handler-calls-process-project ()
  "ORG_GTD=Projects dispatches to process-project when type supports it."
  (let ((heading-called nil)
        (project-called nil)
        (org-gtd-types
         `((fake :org-gtd "Fake" :state nil :properties nil
                 :supports (project-handler)
                 :project-fn ,(lambda (&rest _) nil)))))
    (cl-letf (((symbol-function 'org-gtd-process-heading)
               (lambda (&rest _) (setq heading-called t)))
              ((symbol-function 'org-gtd-process-project)
               (lambda (&rest _) (setq project-called t))))
      (with-temp-buffer
        (org-mode)
        (insert "* Some project\n:PROPERTIES:\n:ORG_GTD: Projects\n:END:\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd--dispatch 'fake)))
    (assert-true project-called)
    (assert-nil heading-called)))

(deftest dispatch-project-task-with-project-handler-prompts-and-dispatches ()
  "A task with ORG_GTD_PROJECT_IDS routes through project marker selection."
  (let ((project-called nil)
        (project-marker (point-marker))
        (org-gtd-types
         `((fake :org-gtd "Fake" :state nil :properties nil
                 :supports (project-handler)
                 :project-fn ,(lambda (&rest _) nil)))))
    (cl-letf (((symbol-function 'org-gtd-project--get-marker-at-point)
               (lambda (&optional _prompt) project-marker))
              ((symbol-function 'org-gtd-process-project)
               (lambda (pom _type &optional _config)
                 (setq project-called pom))))
      (with-temp-buffer
        (org-mode)
        (insert "* Do the thing\n:PROPERTIES:\n:ORG_GTD_PROJECT_IDS: abc-123\n:END:\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd--dispatch 'fake)))
    (assert-same project-marker project-called)))

(deftest dispatch-project-task-without-project-handler-calls-process-heading ()
  "A task with ORG_GTD_PROJECT_IDS still reclassifies as a single heading
when the type does not support project-handler."
  (let ((heading-called nil)
        (project-called nil)
        (org-gtd-types
         '((fake :org-gtd "Fake" :state nil :properties nil))))
    (cl-letf (((symbol-function 'org-gtd-process-heading)
               (lambda (&rest _) (setq heading-called t)))
              ((symbol-function 'org-gtd-process-project)
               (lambda (&rest _) (setq project-called t))))
      (with-temp-buffer
        (org-mode)
        (insert "* A project task\n:PROPERTIES:\n:ORG_GTD_PROJECT_IDS: abc-123\n:END:\n")
        (goto-char (point-min))
        (org-next-visible-heading 1)
        (org-gtd--dispatch 'fake)))
    (assert-true heading-called)
    (assert-nil project-called)))

(provide 'organize-core-test)
;;; organize-core-test.el ends here
