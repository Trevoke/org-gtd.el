;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
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
         (expect (org-entry-get (point) "ORG_GTD" t)
                 :to-equal org-gtd-incubate)
         (expect (org-entry-get (point) org-gtd-timestamp)
                 :to-match (format "%s-%#02d-%#02d" year month day)))))

  (it "can be added programmatically"
     (org-gtd-incubate-create "Dentist appointment"
                              (format-time-string "%Y-%m-%d"))
     (org-gtd-engage)
     (with-current-buffer org-agenda-buffer
       (expect (ogt--current-buffer-raw-text)
               :to-match
               "Dentist appointment")))


 (describe
  "compatibility with orgzly"

  (it "has a copy of the active timestamp in the body"
      (let* ((date (calendar-current-date))
             (year (nth 2 date))
             (month (nth 0 date))
             (day (nth 1 date)))
        (create-deferred-item "Yowza" date)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Yowza")
          (org-end-of-meta-data t)
          (expect (buffer-substring (point) (point-max))
                  :to-match
                  (format "<%s-%#02d-%#02d>" year month day)))))))
