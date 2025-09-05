;; -*- lexical-binding: t; coding: utf-8 -*-

;; Load test helpers via setup.el (which now uses require internally)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd)
(require 'buttercup)

(describe
 "org-gtd-agenda-files"

 (before-each 
   (setq inhibit-message t)
   (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "appends to the existing org-agenda-files"
     (let ((org-agenda-files '("/tmp/foo.org")))
       (with-org-gtd-context
           (expect org-agenda-files
                   :to-have-same-items-as
                   `(,org-gtd-directory "/tmp/foo.org")))))

 (it "expands and appends if org-agenda-files is a single file"
     (let* ((other-dir (make-temp-file "other-dir" t))
            ;(file1 (buffer-file-name (find-file-noselect (f-join other-dir "file1.org"))))
            (index (find-file-noselect (f-join other-dir "index"))))
       (write-region (f-join other-dir "file1.org") nil (buffer-file-name index))
       ;(with-current-buffer file1 (basic-save-buffer))
       (with-current-buffer index (basic-save-buffer))

       (let ((org-agenda-files (buffer-file-name index)))
         (with-org-gtd-context
             (expect org-agenda-files
                     :to-have-same-items-as
                     `(,org-gtd-directory ,(f-join other-dir "file1.org")))))
       (kill-buffer index)))

 (it "sets the variable if org-agenda-files is nil"
     (let ((org-agenda-files nil))
       (with-org-gtd-context
           (expect org-agenda-files
                   :to-have-same-items-as
                   `(,org-gtd-directory)))))
)
