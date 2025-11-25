;; -*- lexical-binding: t; coding: utf-8 -*-

;; TDD tests for smart buffer saving after organize

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "Smart buffer saving for GTD operations"

 (before-each (setq inhibit-message t)
  (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "org-gtd-buffer-p predicate"

   (it "returns t for buffers in org-gtd-directory"
       (let ((gtd-file (ogt--temp-org-file-in-gtd-dir "test-task" "* Task")))
         (with-current-buffer gtd-file
           (expect (org-gtd-buffer-p) :to-be t))))

   (it "returns nil for buffers outside org-gtd-directory"
       (let ((non-gtd-file (ogt--temp-org-file-buffer "other" "* Not GTD")))
         (with-current-buffer non-gtd-file
           (expect (org-gtd-buffer-p) :to-be nil))))

   (it "returns nil for buffers without files"
       (with-temp-buffer
         (org-mode)
         (expect (org-gtd-buffer-p) :to-be nil)))

   (it "can check other buffers"
       (let ((gtd-file (ogt--temp-org-file-in-gtd-dir "test" "* Task"))
             (other-file (ogt--temp-org-file-buffer "other" "* Not GTD")))
         (expect (org-gtd-buffer-p gtd-file) :to-be t)
         (expect (org-gtd-buffer-p other-file) :to-be nil))))

 (describe "Saving GTD buffers only"

   (it "saves GTD buffers when org-gtd-save-after-organize is t"
       (let ((org-gtd-save-after-organize t)
             (gtd-file (ogt--temp-org-file-in-gtd-dir "tasks" "* Original"))
             (non-gtd-file (ogt--temp-org-file-buffer "other" "* Other")))

         ;; Modify both buffers
         (with-current-buffer gtd-file
           (goto-char (point-max))
           (insert "\n* New GTD Task")
           (set-buffer-modified-p t))

         (with-current-buffer non-gtd-file
           (goto-char (point-max))
           (insert "\n* New Other Task")
           (set-buffer-modified-p t))

         ;; Save GTD buffers
         (org-gtd-save-buffers)

         ;; Check GTD buffer was saved
         (with-current-buffer gtd-file
           (expect (buffer-modified-p) :to-be nil))

         ;; Check non-GTD buffer was NOT saved
         (with-current-buffer non-gtd-file
           (expect (buffer-modified-p) :not :to-be nil))))

   (it "does not save when org-gtd-save-after-organize is nil"
       (let ((org-gtd-save-after-organize nil)
             (gtd-file (ogt--temp-org-file-in-gtd-dir "tasks" "* Task")))

         (with-current-buffer gtd-file
           (goto-char (point-max))
           (insert "\n* Modified")
           (set-buffer-modified-p t))

         ;; This should not save anything
         (when org-gtd-save-after-organize
           (org-gtd-save-buffers))

         ;; Buffer should still be modified
         (with-current-buffer gtd-file
           (expect (buffer-modified-p) :not :to-be nil)))))

 (describe "Integration with organize operations"

   (it "saves GTD buffers after organizing when enabled"
       (let ((org-gtd-save-after-organize t)
             (gtd-file (ogt--temp-org-file-in-gtd-dir "tasks" "* Original Task")))

         ;; Modify the GTD buffer
         (with-current-buffer gtd-file
           (goto-char (point-max))
           (insert "\n* Modified Task")
           (set-buffer-modified-p t))

         ;; Verify it's modified
         (with-current-buffer gtd-file
           (expect (buffer-modified-p) :not :to-be nil))

         ;; Call save function
         (org-gtd-save-buffers)

         ;; Check that it was saved
         (with-current-buffer gtd-file
           (expect (buffer-modified-p) :to-be nil))))

   (it "does not save when org-gtd-save-after-organize is disabled"
       (let ((org-gtd-save-after-organize nil)
             (gtd-file (ogt--temp-org-file-in-gtd-dir "tasks" "* Original Task")))

         ;; Modify the GTD buffer
         (with-current-buffer gtd-file
           (goto-char (point-max))
           (insert "\n* Modified Task")
           (set-buffer-modified-p t))

         ;; Call save function (should do nothing)
         (org-gtd-save-buffers)

         ;; Buffer should still be modified
         (with-current-buffer gtd-file
           (expect (buffer-modified-p) :not :to-be nil))))))

(defun ogt--temp-org-file-in-gtd-dir (basename content)
  "Create a temp org file in the GTD directory."
  (let ((file (expand-file-name (format "%s.org" basename) org-gtd-directory)))
    (with-temp-file file
      (insert content))
    (find-file-noselect file)))