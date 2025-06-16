;; ;; -*- lexical-binding: t; coding: utf-8 -*-

;; (load "test/helpers/setup.el")
;; (load "test/helpers/utils.el")
;; (require 'org-gtd)
;; (require 'buttercup)
;; (require 'with-simulated-input)

;; (describe
;;  "Processing items"

;;  :var ((inhibit-message t))

;;  (before-each
;;   (ogt--configure-emacs)
;;   (ogt-capture-single-item))
;;  (after-each (ogt--close-and-delete-files))

;;  (it "processes all the elements"
;;      ;; Capture some additional items (note: before-each already captures one)
;;      (ogt-capture-single-item "test project")
;;      (ogt-capture-single-item "test calendar item")
;;      (ogt-capture-single-item "test delegated item")
;;      (ogt-capture-single-item "test incubated item")
;;      (ogt-capture-single-item "test single action")
;;      (ogt-capture-single-item "test knowledge item")

;;      ;; Process them all using helper functions (7 items total)
;;      (org-gtd-process-inbox)
;;      ;; First item from before-each (single action)
;;      (ogt-clarify-as-single-action)

;;      ;; Items we captured in this test
;;      (execute-kbd-macro (kbd "M-> RET"))
;;      (insert ogt--project-text)
;;      (ogt-clarify-as-project)

;;      (ogt-clarify-as-calendar-item (calendar-current-date))
;;      (ogt-clarify-as-delegated-item "Someone" (calendar-current-date))
;;      (ogt-clarify-as-incubated-item (calendar-current-date))
;;      (ogt-clarify-as-single-action)
;;      (ogt-clarify-as-knowledge-item)

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
;;        (ogt-clarify-as-single-action))

;;      (org-gtd-engage)
;;      (let ((ogt-agenda-string (ogt--buffer-string org-agenda-buffer)))
;;        (expect (string-match "NEXT \\[#A\\] single action" ogt-agenda-string)
;;                :to-be-truthy)))

;;  (it "shows item in agenda when done"
;;      (org-gtd-process-inbox)
;;      (ogt-clarify-as-single-action)
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
;;        (ogt-clarify-as-project)

;;        (expect (buffer-name) :to-match org-gtd-wip--prefix)
;;        (expect (ogt--buffer-string "*Message*") :to-match "** First task")))))
