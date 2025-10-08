;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "An incubated item"


 (before-each (setq inhibit-message t) (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "has a specific property with the active timestamp"
     (let* ((date (calendar-current-date))
            (year (nth 2 date))
            (month (nth 0 date))
            (day (nth 1 date)))
       (create-deferred-item "Yowza" date)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (task-type (current-task)) :to-equal 'incubated)
         (let ((timestamp (task-timestamp (current-task))))
           (expect timestamp :to-match (format "%s-%#02d-%#02d" year month day))))))

  (it "can be added programmatically"
     (org-gtd-incubate-create "Dentist appointment"
                              (format-time-string "%Y-%m-%d"))
     (org-gtd-engage)
     (expect (agenda-contains? "Dentist appointment") :to-be-truthy))


 (describe
  "compatibility with orgzly"

  (it "has a copy of the active timestamp in the body"
      (let* ((date (calendar-current-date))
             (year (nth 2 date))
             (month (nth 0 date))
             (day (nth 1 date)))
        (create-deferred-item "Yowza" date)
        (expect (file-contains? (org-gtd--default-file)
                               (format "<%s-%#02d-%#02d>" year month day))
                :to-be-truthy)))))
