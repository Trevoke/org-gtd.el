;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))
(require 'with-simulated-input)

;;; TODO uncomment this test
;;; TODO stat cookie should support percent and tally versions

;; (it "safely adds the stats cookie"
;;     (setq org-gtd-organize-hooks '(org-set-tags-command org-priority))
;;     (capture-inbox-item "project headline")
;;     (org-gtd-process-inbox)
;;     (execute-kbd-macro (kbd "M-> RET"))
;;     (insert ogt--project-text)
;;     (execute-kbd-macro (kbd "C-c c p headline_tag RET A task_1_tag RET B task_2_tag RET C task_3_tag RET A"))
;;     (ogt--save-all-buffers)
;;     (with-current-buffer (org-gtd--default-file)
;;       (goto-char (point-min))
;;       (expect (current-buffer-raw-text)
;;               :to-match "[0/3]")
;;       (search-forward "project headline")
;;       (expect (member "headline_tag" (org-get-tags)))
;;       (expect (org-element-property :priority (org-element-at-point))
;;               :to-equal (org-priority-to-value "A"))
;;       (search-forward "Task 1")
;;       (expect (member "task_1_tag" (org-get-tags)))
;;       (expect (org-element-property :priority (org-element-at-point))
;;               :to-equal (org-priority-to-value "B"))
;;       (search-forward "Task 2")
;;       (expect (member "task_2_tag" (org-get-tags)))
;;       (expect (org-element-property :priority (org-element-at-point))
;;               :to-equal (org-priority-to-value "C"))
;;       (search-forward "Task 3")
;;       (expect (member "task_3_tag" (org-get-tags)))
;;       (expect (org-element-property :priority (org-element-at-point))
;;               :to-equal (org-priority-to-value "A"))))
;; )

;; Sequential dependency tests migrated to test-eunit/integration/project-dependencies-test.el

;; Fix TODO keywords tests migrated to test-eunit/integration/project-fix-keywords-test.el
;; (including previously pending AND semantics negative case test - now active)

;; State preservation and basic tickler tests migrated to test-eunit/unit/project-tickler-test.el
;; Advanced tickler tests migrated to test-eunit/integration/project-tickler-advanced-test.el

