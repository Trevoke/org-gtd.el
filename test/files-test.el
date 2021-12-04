;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(load "test/helpers/utils.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe "Create a default file"

          (before-each
           (ogt--configure-emacs)
           (ogt--prepare-filesystem))
          (after-each (ogt--close-and-delete-files))

          (describe "with default content"
                    (it "for the inbox"
                        (with-current-buffer (org-gtd--inbox-file)
                          (expect (buffer-string)
                                  :to-match
                                  "This is the inbox")
                          (expect (buffer-string)
                                  :to-match
                                  "#\\+STARTUP: overview hidestars logrefile indent logdone")))

                    (it "has a header for the default file"
                        (with-current-buffer (org-gtd--default-file)
                          (expect (buffer-string)
                                  :to-match
                                  "#\\+STARTUP: overview indent align inlineimages hidestars logdone logrepeat logreschedule logredeadline
#\\+TODO: NEXT(n) TODO(t) WAIT(w@) | DONE(d) CNCL(c@)")))

                    (it "with project header"
                        (with-current-buffer (org-gtd--default-file)
                          (expect (buffer-string)
                                  :to-match
                                  ":ORG_GTD: Projects")))

                    (it "with calendar header"
                        (with-current-buffer (org-gtd--default-file)
                          (expect (buffer-string)
                                  :to-match
                                  ":ORG_GTD: Calendar")))

                    (it "with actionable header"
                        (with-current-buffer (org-gtd--default-file)
                          (expect (buffer-string)
                                  :to-match
                                  ":ORG_GTD: Actions")))

                    (it "with incubated header"
                        (with-current-buffer (org-gtd--default-file)
                          (expect (buffer-string)
                                  :to-match
                                  ":ORG_GTD: Incubated")))

                    (describe
                     "when there isn't a refile target"
                     (it "for a project"
                         (ogt--add-and-process-project "project headline")
                         (ogt--save-all-buffers)
                         (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org"))

                     (it "for a calendar item"
                         (ogt--add-and-process-calendar-item "calendar headline")
                         (ogt--save-all-buffers)
                         (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org"))

                     (it "for a delegated item"
                         (ogt--add-and-process-delegated-item "delegated headline")
                         (ogt--save-all-buffers)
                         (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org"))

                     (it "for a incubated item"
                         (ogt--add-and-process-incubated-item "incubated headline")
                         (ogt--save-all-buffers)
                         (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org"))

                     (it "for a single action"
                         (ogt--add-and-process-single-action "single action")
                         (ogt--save-all-buffers)
                         (expect (ogt--org-dir-buffer-string) :to-match "org-gtd-tasks\\.org")))))
