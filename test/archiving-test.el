;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'compat)



;; Load test helpers via setup.el (which now uses require internally)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
(require 'org-gtd-test-helper-builders (file-name-concat default-directory "test/helpers/builders.el"))
(require 'ogt-assertions (file-name-concat default-directory "test/helpers/assertions.el"))
(require 'org-gtd)
(require 'buttercup)
(require 'with-simulated-input)

(describe
 "archiving"


 (before-each (setq inhibit-message t)
              (ogt--configure-emacs)
              ;; Clear org-id locations to prevent pollution
              (setq org-id-locations nil)
              (setq org-id-files nil))
 (after-each (ogt--close-and-delete-files)
             ;; Clear org-id locations after tests
             (setq org-id-locations nil)
             (setq org-id-files nil))

 (describe
  "finished work"

  (it "archives completed and canceled projects"
      (create-project "project headline")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-max))
        (newline)
        ;; Use builder instead of string fixture
        (make-canceled-project "canceled")
        (newline)
        (make-completed-project "completed")
        (basic-save-buffer))
      (org-gtd-archive-completed-items)
      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "completed")
        (expect archived-projects :to-match "canceled"))))

 (it "archives completed single action while preserving active ones"
     (create-single-action "one")
     (ogt--save-all-buffers)
     (create-single-action "two")
     (ogt--save-all-buffers)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "NEXT one")
       (org-todo "DONE"))
     (org-gtd-archive-completed-items)
     (ogt--save-all-buffers)
     (expect (file-contains? (org-gtd--default-file) "two") :to-be-truthy)
     (expect (file-contains? (org-gtd--default-file) " DONE one") :to-be nil))

 (it "does not archive repeating scheduled items"
     (let* ((temporary-file-directory org-gtd-directory)
            (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
       (org-gtd-archive-completed-items)
       (expect (file-contains? gtd-file "repeating item") :to-be-truthy)
       (expect (file-contains? gtd-file "write a nice test") :to-be nil)))

 (it "does not archive undone incubated items"
     (let* ((temporary-file-directory org-gtd-directory)
            (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
       (org-gtd-archive-completed-items)
       (expect (file-contains? gtd-file "For later") :to-be-truthy)
       (expect (file-contains? gtd-file "not worth thinking about") :to-be nil)))

 (it "archives DAG-structured project with tasks in breadth-first order"
     ;; Create a property-based project with tasks connected via BLOCKS/DEPENDS_ON
     ;; arranged as a DAG: Project -> Task A -> Task B -> Task C
     ;;                                      \-> Task D
     ;; All tasks are within the project tree but at different levels
     (let* ((gtd-file-path (f-join org-gtd-directory "org-gtd-tasks.org"))
            (archive-file-path (car (split-string (funcall org-gtd-archive-location) "::"))))

       (with-current-buffer (find-file-noselect gtd-file-path)
         (goto-char (point-max))
         (insert "\n")

         ;; Use builder for DAG project with mixed nesting levels
         (make-dag-project "DAG Project"
                          :id "dag-project-id"
                          :task-specs '((:description "Task A"
                                        :id "task-a-id"
                                        :status done
                                        :blocks ("task-b-id" "task-d-id")
                                        :level 2)
                                       (:description "Task B"
                                        :id "task-b-id"
                                        :status done
                                        :depends-on ("task-a-id")
                                        :blocks ("task-c-id")
                                        :level 3)
                                       (:description "Task C"
                                        :id "task-c-id"
                                        :status done
                                        :depends-on ("task-b-id")
                                        :level 4)
                                       (:description "Task D"
                                        :id "task-d-id"
                                        :status done
                                        :depends-on ("task-a-id")
                                        :level 2)))

         (org-mode-restart)
         (basic-save-buffer))

       ;; Update org-id locations so graph traversal can find tasks
       (org-id-update-id-locations (list gtd-file-path))

       ;; Archive the completed project
       (org-gtd-archive-completed-items)

       ;; Save all buffers to ensure archive is written
       (save-some-buffers t)

       ;; Verify project and tasks are archived
       (with-current-buffer (find-file-noselect gtd-file-path)
         (expect (current-buffer-raw-text) :not :to-match "DAG Project")
         (expect (current-buffer-raw-text) :not :to-match "Task A")
         (expect (current-buffer-raw-text) :not :to-match "Task B")
         (expect (current-buffer-raw-text) :not :to-match "Task C")
         (expect (current-buffer-raw-text) :not :to-match "Task D"))

       ;; Verify archived structure: All tasks and project are present
       (with-current-buffer (find-file-noselect archive-file-path)
         (let ((content (current-buffer-raw-text)))
           ;; All tasks should be present in the archive
           (expect content :to-match "Task A")
           (expect content :to-match "Task B")
           (expect content :to-match "Task C")
           (expect content :to-match "Task D")

           ;; Project should be present
           (expect content :to-match "DAG Project")))))

 (it "does not archive projects with incomplete tasks"
     ;; Create a project where only some tasks are done
     (let* ((gtd-file-path (f-join org-gtd-directory "org-gtd-tasks.org"))
            (archive-file-path (car (split-string (funcall org-gtd-archive-location) "::"))))

       (with-current-buffer (find-file-noselect gtd-file-path)
         (goto-char (point-max))
         (insert "\n")

         ;; Use builder for incomplete project
         (make-project "Partial Project"
                      :id "partial-project-id"
                      :tasks '((:description "Task X"
                               :id "task-x-id"
                               :status done
                               :blocks ("task-y-id"))
                              (:description "Task Y"
                               :id "task-y-id"
                               :status todo
                               :depends-on ("task-x-id"))))

         (basic-save-buffer)
         ;; Update org-id locations
         (org-id-update-id-locations (list gtd-file-path)))

       ;; Try to archive - should NOT archive because Task Y is incomplete
       (org-gtd-archive-completed-items)

       ;; Verify project and tasks are NOT archived
       (with-current-buffer (find-file-noselect gtd-file-path)
         (let ((content (current-buffer-raw-text)))
           (expect content :to-match "Partial Project")
           (expect content :to-match "Task X")
           (expect content :to-match "Task Y")))

       ;; Verify nothing was archived for this project
       (with-current-buffer (find-file-noselect archive-file-path)
         (let ((content (current-buffer-raw-text)))
           (expect content :not :to-match "Partial Project")))))

 (describe
  "org-gtd-archive-location nil behavior"

  (it "uses org-archive-location when org-gtd-archive-location is nil"
      ;; Setup: nil org-gtd-archive-location should use org-archive-location
      (let* ((custom-archive-file (f-join org-gtd-directory "custom-archive.org"))
             (org-gtd-archive-location nil)
             (org-archive-location (concat custom-archive-file "::")))

        ;; Create a completed single action
        (create-single-action "archive-me")
        (ogt--save-all-buffers)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "NEXT archive-me")
          (org-todo "DONE"))
        (ogt--save-all-buffers)

        ;; Archive completed items
        (org-gtd-archive-completed-items)
        (ogt--save-all-buffers)

        ;; Verify item went to the custom archive location
        (expect (file-exists-p custom-archive-file) :to-be-truthy)
        (with-current-buffer (find-file-noselect custom-archive-file)
          (expect (current-buffer-raw-text) :to-match "archive-me"))))

  (it "uses org-gtd-archive-location when set to a function"
      ;; This confirms existing behavior still works
      (let ((org-gtd-archive-location #'org-gtd-archive-location-func))

        ;; Create a completed single action
        (create-single-action "gtd-archive-me")
        (ogt--save-all-buffers)
        (with-current-buffer (org-gtd--default-file)
          (goto-char (point-min))
          (search-forward "NEXT gtd-archive-me")
          (org-todo "DONE"))
        (ogt--save-all-buffers)

        ;; Archive completed items
        (org-gtd-archive-completed-items)
        (ogt--save-all-buffers)

        ;; Verify item went to org-gtd's datetree archive
        (let ((archive-file-path (car (split-string (funcall org-gtd-archive-location) "::"))))
          (expect (file-exists-p archive-file-path) :to-be-truthy)
          (with-current-buffer (find-file-noselect archive-file-path)
            (expect (current-buffer-raw-text) :to-match "gtd-archive-me")))))))
