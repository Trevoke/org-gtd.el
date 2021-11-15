;; -*- lexical-binding: t; -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "Create a default file"

 (before-all (ogt--configure-emacs))
 (before-each (ogt--clean-target-directory org-gtd-directory))
 (after-each (ogt--close-and-delete-files))

 (describe
  "with default content"
  (it "for the inbox"
      (with-current-buffer (org-gtd--inbox-file)
        (expect (buffer-string)
                :to-match
                "This is the inbox")
        (expect (buffer-string)
                :to-match
                "#\\+STARTUP: overview hidestars logrefile indent logdone")))

  (it "has a shared header for the processed files"
      (dolist (buffer `(,(org-gtd--default-projects-file)
                        ,(org-gtd--default-action-file)
                        ,(org-gtd--default-incubated-file)
                        ,(org-gtd--default-delegated-file)
                        ,(org-gtd--default-calendar-file)))
        (with-current-buffer buffer
          (expect (buffer-string)
                  :to-match
                  "#\\+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#\\+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CNCL(c@)"))))

  (it "for the default projects file"
      (with-current-buffer (org-gtd--default-projects-file)
        (expect (buffer-string)
                :to-match
                ":ORG_GTD: Projects")))

  (it "for the default calendar file"
      (with-current-buffer (org-gtd--default-calendar-file)
        (expect (buffer-string)
                :to-match
                ":ORG_GTD: Calendar")))

  (it "for the default delegated file"
      (with-current-buffer (org-gtd--default-delegated-file)
        (expect (buffer-string)
                :to-match
                ":ORG_GTD: Actions")))

  (it "for the default incubated file"
      (with-current-buffer (org-gtd--default-incubated-file)
        (expect (buffer-string)
                :to-match
                ":ORG_GTD: Incubated")))

  (it "for the default action file"
      (with-current-buffer (org-gtd--default-action-file)
        (expect (buffer-string)
                :to-match
                ":ORG_GTD: Actions")))

  )

 (describe
  "when there isn't a refile target"
  (it "for a project"
      (ogt--add-and-process-project "project headline")
      (ogt--save-all-buffers)
      (list-directory org-gtd-directory)
      (with-current-buffer "*Directory*"
        (expect (buffer-string)
                :to-match
                "projects\\.org"))
      (kill-buffer "*Directory*"))

  (it "for a calendar item"
      (ogt--add-and-process-calendar-item "calendar headline")
      (ogt--save-all-buffers)
      (list-directory org-gtd-directory)
      (with-current-buffer "*Directory*"
        (expect (buffer-string)
                :to-match
                "calendar\\.org"))
      (kill-buffer "*Directory*"))

  (it "for a delegated item"
      (ogt--add-and-process-delegated-item "delegated headline")
      (ogt--save-all-buffers)
      (list-directory org-gtd-directory)
      (with-current-buffer "*Directory*"
        (expect (buffer-string)
                :to-match
                "actions\\.org"))
      (kill-buffer "*Directory*"))

  (it "for a incubated item"
      (ogt--add-and-process-incubated-item "incubated headline")
      (ogt--save-all-buffers)
      (list-directory org-gtd-directory)
      (with-current-buffer "*Directory*"
        (expect (buffer-string)
                :to-match
                "incubated\\.org"))
      (kill-buffer "*Directory*"))

  (it "for a single action"
      (ogt--add-and-process-single-action "single action")
      (ogt--save-all-buffers)
      (list-directory org-gtd-directory)
      (with-current-buffer "*Directory*"
        (expect (buffer-string)
                :to-match
                "actions\\.org"))
      (kill-buffer "*Directory*"))))
