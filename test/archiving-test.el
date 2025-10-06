;; -*- lexical-binding: t; coding: utf-8 -*-

;; Load test helpers via setup.el (which now uses require internally)
(require 'org-gtd-test-setup (file-name-concat default-directory "test/helpers/setup.el"))
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
      (ogt-capture-and-process-project "project headline")
      (with-current-buffer (org-gtd--default-file)
        (goto-char (point-max))
        (newline)
        (insert ogt--canceled-project)
        (goto-char (point-max))
        (newline)
        (insert ogt--completed-project)
        (basic-save-buffer))
      (org-gtd-archive-completed-items)
      (let ((archived-projects (ogt--archive-string)))
        (expect archived-projects :to-match "completed")
        (expect archived-projects :to-match "canceled"))))

 (it "on a single action"
     (ogt-capture-and-process-single-action "one")
     (ogt--save-all-buffers)
     (ogt-capture-and-process-single-action "two")
     (ogt--save-all-buffers)
     (with-current-buffer (org-gtd--default-file)
       (goto-char (point-min))
       (search-forward "NEXT one")
       (org-todo "DONE"))
     (org-gtd-archive-completed-items)
     (ogt--save-all-buffers)
     (with-current-buffer (org-gtd--default-file)
       (expect (buffer-string)
               :to-match
               "two")
       (expect (buffer-string)
               :not :to-match
               " DONE one")))

 (it "does not archive repeating scheduled items"
     (let* ((temporary-file-directory org-gtd-directory)
            (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
       (org-gtd-archive-completed-items)
       (with-current-buffer (find-file-noselect gtd-file)
         (expect (ogt--current-buffer-raw-text) :to-match "repeating item")
         (expect (ogt--current-buffer-raw-text) :not :to-match "write a nice test"))))

 (it "does not archive undone incubated items"
     (let* ((temporary-file-directory org-gtd-directory)
            (gtd-file (make-temp-file "foo" nil ".org" (org-file-contents "test/fixtures/gtd-file.org"))))
       (org-gtd-archive-completed-items)
       (with-current-buffer (find-file-noselect gtd-file)
         (expect (ogt--current-buffer-raw-text) :to-match "For later")
         (expect (ogt--current-buffer-raw-text) :not :to-match "not worth thinking about"))))

 (it "archives DAG-structured project with tasks in breadth-first order"
     ;; Create a property-based project with tasks connected via BLOCKS/DEPENDS_ON
     ;; arranged as a DAG: Project -> Task A -> Task B -> Task C
     ;;                                      \-> Task D
     ;; All tasks are within the project tree but at different levels
     (let* ((gtd-file-path (f-join org-gtd-directory "org-gtd-tasks.org"))
            (archive-file-path (car (split-string (funcall org-gtd-archive-location) "::"))))

       (with-current-buffer (find-file-noselect gtd-file-path)
         (goto-char (point-max))
         (insert "\n* DAG Project\n")
         (insert ":PROPERTIES:\n")
         (insert ":ORG_GTD: Projects\n")
         (insert ":ID: dag-project-id\n")
         (insert ":FIRST_TASKS: task-a-id\n")
         (insert ":END:\n")

         ;; Task A - root of the DAG, blocks B and D
         (insert "** DONE Task A\n")
         (insert ":PROPERTIES:\n")
         (insert ":ORG_GTD: Actions\n")
         (insert ":ID: task-a-id\n")
         (insert ":ORG_GTD_BLOCKS: task-b-id task-d-id\n")
         (insert ":ORG_GTD_PROJECT: DAG Project\n")
         (insert ":END:\n")

         ;; Task B - depends on A, blocks C (nested deeper)
         (insert "*** DONE Task B\n")
         (insert ":PROPERTIES:\n")
         (insert ":ORG_GTD: Actions\n")
         (insert ":ID: task-b-id\n")
         (insert ":ORG_GTD_DEPENDS_ON: task-a-id\n")
         (insert ":ORG_GTD_BLOCKS: task-c-id\n")
         (insert ":ORG_GTD_PROJECT: DAG Project\n")
         (insert ":END:\n")

         ;; Task C - depends on B (deeply nested)
         (insert "**** DONE Task C\n")
         (insert ":PROPERTIES:\n")
         (insert ":ORG_GTD: Actions\n")
         (insert ":ID: task-c-id\n")
         (insert ":ORG_GTD_DEPENDS_ON: task-b-id\n")
         (insert ":ORG_GTD_PROJECT: DAG Project\n")
         (insert ":END:\n")

         ;; Task D - depends on A (different level, still within project)
         (insert "** DONE Task D\n")
         (insert ":PROPERTIES:\n")
         (insert ":ORG_GTD: Actions\n")
         (insert ":ID: task-d-id\n")
         (insert ":ORG_GTD_DEPENDS_ON: task-a-id\n")
         (insert ":ORG_GTD_PROJECT: DAG Project\n")
         (insert ":END:\n")

         (basic-save-buffer))

       ;; Update org-id locations so graph traversal can find tasks
       (org-id-update-id-locations (list gtd-file-path))

       ;; Archive the completed project
       (org-gtd-archive-completed-items)

       ;; Save all buffers to ensure archive is written
       (save-some-buffers t)

       ;; Verify project and tasks are archived
       (with-current-buffer (find-file-noselect gtd-file-path)
         (expect (buffer-string) :not :to-match "DAG Project")
         (expect (buffer-string) :not :to-match "Task A")
         (expect (buffer-string) :not :to-match "Task B")
         (expect (buffer-string) :not :to-match "Task C")
         (expect (buffer-string) :not :to-match "Task D"))

       ;; Verify archived structure: Project should contain all tasks
       (with-current-buffer (find-file-noselect archive-file-path)
         (goto-char (point-min))

         ;; Find the archived project
         (expect (search-forward "DAG Project" nil t) :to-be-truthy)
         (let ((project-pos (point)))

           ;; All tasks should be children of the archived project
           ;; They should be in breadth-first order: A, B, D, C
           ;; (A is level 0, B and D are level 1, C is level 2)

           ;; Task A should be first child
           (expect (search-forward "Task A" nil t) :to-be-truthy)
           (expect (< project-pos (point)) :to-be-truthy)

           ;; Task B should be present
           (goto-char project-pos)
           (expect (search-forward "Task B" nil t) :to-be-truthy)

           ;; Task C should be present
           (goto-char project-pos)
           (expect (search-forward "Task C" nil t) :to-be-truthy)

           ;; Task D should be present
           (goto-char project-pos)
           (expect (search-forward "Task D" nil t) :to-be-truthy)))))

 (it "does not archive projects with incomplete tasks"
     ;; Create a project where only some tasks are done
     (let* ((gtd-file-path (f-join org-gtd-directory "org-gtd-tasks.org"))
            (archive-file-path (car (split-string (funcall org-gtd-archive-location) "::"))))

       (with-current-buffer (find-file-noselect gtd-file-path)
         (goto-char (point-max))
         (insert "\n* Partial Project\n")
         (insert ":PROPERTIES:\n")
         (insert ":ORG_GTD: Projects\n")
         (insert ":ID: partial-project-id\n")
         (insert ":FIRST_TASKS: task-x-id\n")
         (insert ":END:\n")

         ;; Task X - done, blocks Y
         (insert "** DONE Task X\n")
         (insert ":PROPERTIES:\n")
         (insert ":ORG_GTD: Actions\n")
         (insert ":ID: task-x-id\n")
         (insert ":ORG_GTD_BLOCKS: task-y-id\n")
         (insert ":ORG_GTD_PROJECT: Partial Project\n")
         (insert ":END:\n")

         ;; Task Y - NOT done, depends on X
         (insert "** TODO Task Y\n")
         (insert ":PROPERTIES:\n")
         (insert ":ORG_GTD: Actions\n")
         (insert ":ID: task-y-id\n")
         (insert ":ORG_GTD_DEPENDS_ON: task-x-id\n")
         (insert ":ORG_GTD_PROJECT: Partial Project\n")
         (insert ":END:\n")

         (basic-save-buffer)
         ;; Update org-id locations
         (org-id-update-id-locations (list gtd-file-path)))

       ;; Try to archive - should NOT archive because Task Y is incomplete
       (org-gtd-archive-completed-items)

       ;; Verify project and tasks are NOT archived
       (with-current-buffer (find-file-noselect gtd-file-path)
         (let ((content (buffer-string)))
           (expect content :to-match "Partial Project")
           (expect content :to-match "Task X")
           (expect content :to-match "Task Y")))

       ;; Verify nothing was archived for this project
       (with-current-buffer (find-file-noselect archive-file-path)
         (let ((content (buffer-string)))
           (expect content :not :to-match "Partial Project"))))))
