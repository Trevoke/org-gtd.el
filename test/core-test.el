;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

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

(describe "org-gtd-project-progress-cookie-position"
  (it "defaults to 'end"
    (expect org-gtd-project-progress-cookie-position :to-equal 'end))

  (it "accepts nil, 'start, or 'end"
    (let ((org-gtd-project-progress-cookie-position nil))
      (expect org-gtd-project-progress-cookie-position :to-be nil))
    (let ((org-gtd-project-progress-cookie-position 'start))
      (expect org-gtd-project-progress-cookie-position :to-equal 'start))
    (let ((org-gtd-project-progress-cookie-position 'end))
      (expect org-gtd-project-progress-cookie-position :to-equal 'end))))

(describe "org-gtd--extract-keyword-name"
  (it "returns plain keyword unchanged"
    (expect (org-gtd--extract-keyword-name "NEXT") :to-equal "NEXT"))

  (it "strips single-char shortcut"
    (expect (org-gtd--extract-keyword-name "NEXT(n)") :to-equal "NEXT"))

  (it "strips shortcut with logging config"
    (expect (org-gtd--extract-keyword-name "NEXT(n/@)") :to-equal "NEXT")
    (expect (org-gtd--extract-keyword-name "DONE(d/!)") :to-equal "DONE"))

  (it "handles complex logging syntax"
    (expect (org-gtd--extract-keyword-name "WAIT(w@/!)") :to-equal "WAIT")))

(describe "org-gtd-keyword-mapping validation"
  (it "validates keywords with DSL syntax in org-todo-keywords"
    (let ((org-todo-keywords '((sequence "TODO(t)" "NEXT(n/@)" "WAIT(w@/!)" "|" "DONE(d/!)" "CNCL(c@)")))
          (org-gtd-keyword-mapping nil))
      ;; Should not error - the mapping uses plain keywords while org-todo-keywords uses DSL
      (expect (org-gtd--validate-and-set-keyword-mapping
               'org-gtd-keyword-mapping
               '((todo . "TODO") (next . "NEXT") (wait . "WAIT") (done . "DONE") (canceled . "CNCL")))
              :not :to-throw)))

  (it "still rejects keywords not in org-todo-keywords"
    (let ((org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))
          (org-gtd-keyword-mapping nil))
      ;; Should error - WAIT and CNCL don't exist
      (expect (org-gtd--validate-and-set-keyword-mapping
               'org-gtd-keyword-mapping
               '((todo . "TODO") (next . "NEXT") (wait . "WAIT") (done . "DONE") (canceled . "CNCL")))
              :to-throw 'user-error))))

(describe "ORG_GTD category constants"

  (it "defines org-gtd-delegated constant"
    (expect org-gtd-delegated :to-equal "Delegated"))

  (it "defines org-gtd-quick constant"
    (expect org-gtd-quick :to-equal "Quick"))

  (it "has all existing constants defined"
    (expect org-gtd-action :to-equal "Actions")
    (expect org-gtd-projects :to-equal "Projects")
    (expect org-gtd-calendar :to-equal "Calendar")
    (expect org-gtd-tickler :to-equal "Tickler")
    (expect org-gtd-knowledge :to-equal "Reference")
    (expect org-gtd-trash :to-equal "Trash")))

