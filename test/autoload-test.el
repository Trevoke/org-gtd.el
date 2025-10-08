;;; -*- lexical-binding: t; -*-

(require 'compat)



(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'buttercup)
(describe
    "autoloaded entry point"

  (it "org-gtd-archive-completed-items"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-archive-completed-items-test.el")
     :to-equal 0))

  (it "org-gtd-capture"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-capture-test.el")
     :to-equal 0))

  (it "org-gtd-clarify-item"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-clarify-item-test.el")
     :to-equal 0))

  (it "org-gtd-clarify-mode"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-clarify-mode-test.el")
     :to-equal 0))

  (it "org-gtd-engage"
    (expect
     (ogt--recursive-eldev-test "autoload-tests/org-gtd-engage-test.el")
     :to-equal 0))

  (it "org-gtd-inbox-path"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-inbox-path-test.el")
     :to-equal 0))

  (it "org-gtd-mode"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-mode-test.el")
     :to-equal 0))

  (it "org-gtd-process-inbox"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-process-inbox-test.el")
     :to-equal 0))

  (it "org-gtd-review-stuck-projects"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-review-stuck-projects-test.el")
     :to-equal 0))

  (it "org-gtd-show-all-next"
    (expect
     (ogt--recursive-eldev-test
      "autoload-tests/org-gtd-show-all-next-test.el")
     :to-equal 0))

  )
