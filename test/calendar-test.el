;; -*- lexical-binding: t; coding: utf-8 -*-

(load "test/helpers/setup.el")
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "A calendar item"

 (before-each (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (it "has a specific property with the active timestamp"
     (let* ((date (calendar-current-date))
            (year (nth 2 date))
            (month (nth 0 date))
            (day (nth 1 date)))
       (ogt-capture-and-process-calendar-item "Yowza" date)
       (with-current-buffer (org-gtd--default-file)
         (goto-char (point-min))
         (search-forward "Yowza")
         (expect (org-entry-get (point) "ORG_GTD_CALENDAR")
                 :to-match (format "%s-%#02d-%#02d" year month day)))))

 (describe
  "compatibility with orgzly"
  (it "has a copy of the active timestamp in the body"
      (let* ((date (calendar-current-date))
             (year (nth 2 date))
             (month (nth 0 date))
             (day (nth 1 date)))
        (ogt-capture-and-process-calendar-item "Yowza" date)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "Yowza")
          (org-end-of-meta-data t)
          (expect (buffer-substring (point) (point-max))
                  :to-match
                  (format "<%s-%#02d-%#02d>" year month day)))))))
