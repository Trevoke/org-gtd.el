;; ;; -*- lexical-binding: t; coding: utf-8 -*-

;; (require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
;; (load "test/helpers/utils.el")
;; (require 'org-gtd)
;; (require 'buttercup)
;; (require 'with-simulated-input)

;; (describe
;;  "Processing items"

;;  :var ((inhibit-message t))

;;  (before-each
;;   (ogt--configure-emacs)
;;   (capture-inbox-item))
;;  (after-each (ogt--close-and-delete-files))

;;  (it "processes all the elements"
;;      ;; Capture some additional items (note: before-each already captures one)
;;      (capture-inbox-item "test project")
;;      (capture-inbox-item "test calendar item")
;;      (capture-inbox-item "test delegated item")
;;      (capture-inbox-item "test incubated item")
;;      (capture-inbox-item "test single action")
;;      (capture-inbox-item "test knowledge item")

;;      ;; Process them all using helper functions (7 items total)
;;      (org-gtd-process-inbox)
;;      ;; First item from before-each (single action)
;;      (organize-as-single-action)

;;      ;; Items we captured in this test
;;      (execute-kbd-macro (kbd "M-> RET"))
;;      (insert ogt--project-text)
;;      (organize-as-project)

;;      (schedule-item (calendar-current-date))
;;      (delegate-item "Someone" (calendar-current-date))
;;      (defer-item (calendar-current-date))
;;      (organize-as-single-action)
;;      (archive-as-reference)

;;      ;; Check inbox is empty
;;      (with-current-buffer (ogt-inbox-buffer)
;;        (expect (ogt--current-buffer-raw-text)
;;                :not :to-match
;;                "test")))

;;  (it "uses configurable decorations on the processed items"
;;      ;; Define a simple test hook that adds a priority
;;      (defun test-hook-add-priority ()
;;        "Test hook that adds priority A without user input."
;;        (org-priority ?A))

;;      (let ((org-gtd-organize-hooks '(test-hook-add-priority)))
;;        (org-gtd-process-inbox)
;;        (organize-as-single-action))

;;      (org-gtd-engage)
;;      (let ((ogt-agenda-string (ogt--buffer-string org-agenda-buffer)))
;;        (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string)
;;                :to-be-truthy)))

;;  (it "shows item in agenda when done"
;;      (org-gtd-process-inbox)
;;      (organize-as-single-action)
;;      (expect (buffer-modified-p (org-gtd--default-file)) :to-equal t)

;;      (org-gtd-engage)
;;      (let ((ogt-agenda-string (ogt--buffer-string org-agenda-buffer)))
;;        (expect (string-match "single action" ogt-agenda-string)
;;                :to-be-truthy)))

;;  (describe
;;   "error management"
;;   (describe
;;    "when project has incorrect shape"
;;    (it "tells the user and returns to editing"
;;        (org-gtd-process-inbox)
;;        (organize-as-project)

;;        (expect (buffer-name) :to-match org-gtd-wip--prefix)
;;        (expect (ogt--buffer-string "*Message*") :to-match "** First task")))))
