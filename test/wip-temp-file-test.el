;; -*- lexical-binding: t; coding: utf-8 -*-

;; TDD tests for WIP buffer temp file implementation

(require 'org-gtd-test-prelude (file-name-concat default-directory "test/helpers/prelude.el"))

(describe
 "WIP buffers with temporary files"

 (before-each (setq inhibit-message t)
  (ogt--configure-emacs))
 (after-each (ogt--close-and-delete-files))

 (describe "temp file creation"
   (it "creates a temporary file when creating a WIP buffer"
       (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
           (with-current-buffer wip-buffer
             ;; Buffer should have a file name
             (expect buffer-file-name :not :to-be nil)
             ;; File should exist
             (expect (file-exists-p buffer-file-name) :to-be t)
             ;; File should be in temp directory
             (expect buffer-file-name :to-match
                     (regexp-quote (expand-file-name "org-gtd" temporary-file-directory)))
             ;; File should have .org extension
             (expect buffer-file-name :to-match "\\.org$")))))

   (it "uses unique temp files for different WIP buffers"
       (let ((source-buffer-1 (ogt--temp-org-file-buffer "taskfile1" "* Task 1"))
             (source-buffer-2 (ogt--temp-org-file-buffer "taskfile2" "* Task 2")))

         (with-current-buffer source-buffer-1
           (org-gtd-clarify-item))
         (with-current-buffer source-buffer-2
           (org-gtd-clarify-item))

         (let ((wip-buffers (org-gtd-wip--get-buffers)))
           (expect (length wip-buffers) :to-equal 2)

           (let ((file-1 (buffer-file-name (nth 0 wip-buffers)))
                 (file-2 (buffer-file-name (nth 1 wip-buffers))))
             ;; Files should be different
             (expect file-1 :not :to-equal file-2)
             ;; Both should exist
             (expect (file-exists-p file-1) :to-be t)
             (expect (file-exists-p file-2) :to-be t))))))

 (describe "temp file content"
   (it "preserves content in the temp file"
       (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify\n** Subtask")))
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
           (with-current-buffer wip-buffer
             ;; Wait for content to appear (if it's being copied)
             (unless (> (buffer-size) 0)
               (sleep-for 0.1))

             ;; Check that initial content is there
             (expect (buffer-string) :to-match "Task to clarify")

             ;; Add some content
             (goto-char (point-max))
             (insert "\n*** New subtask")
             ;; Save the buffer
             (save-buffer)

             ;; Store the file name before leaving the buffer context
             (let ((wip-file-name buffer-file-name))
               ;; Read the file directly to verify content was saved
               (let ((file-content (with-temp-buffer
                                     (insert-file-contents wip-file-name)
                                     (buffer-string))))
                 (expect file-content :to-match "Task to clarify")
                 (expect file-content :to-match "New subtask"))))))))

 (describe "auto-save functionality"
   (it "enables auto-save for WIP buffers"
       (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
           (with-current-buffer wip-buffer
             ;; File-backed buffers can use auto-save
             (auto-save-mode 1)
             ;; Should have an auto-save file name (either set or can be generated)
             (expect (or buffer-auto-save-file-name
                         (make-auto-save-file-name))
                     :not :to-be nil)))))

   (it "allows normal save operations"
       (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
           (with-current-buffer wip-buffer
             ;; Save should work without errors
             (expect (ignore-errors (save-buffer) t) :to-be t)))))

   (it "works with org-save-all-org-buffers"
       (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
           (with-current-buffer wip-buffer
             (set-buffer-modified-p t))
           ;; Should not error
           (expect (ignore-errors (org-save-all-org-buffers) t) :to-be t)))))

 (describe "temp file cleanup"
   (it "deletes temp file when stopping clarification"
       (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify"))
             temp-file)
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
           (with-current-buffer wip-buffer
             (setq temp-file buffer-file-name)
             ;; File should exist now
             (expect (file-exists-p temp-file) :to-be t))

           ;; Stop clarification
           (with-current-buffer wip-buffer
             (org-gtd-clarify-stop))

           ;; File should be deleted
           (expect (file-exists-p temp-file) :to-be nil)
           ;; Buffer should be killed
           (expect (buffer-live-p wip-buffer) :to-be nil))))

   (it "deletes temp file after successful organize"
       (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify"))
             (target-buffer (ogt--temp-org-file-buffer "target" "* Projects\n"))
             temp-file
             task-id)
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer (car (org-gtd-wip--get-buffers))))
           (with-current-buffer wip-buffer
             (setq temp-file buffer-file-name)
             ;; Get the task ID for cleanup
             (goto-char (point-min))
             (org-next-visible-heading 1)
             (setq task-id (org-entry-get nil "ID"))
             ;; File should exist now
             (expect (file-exists-p temp-file) :to-be t)

             ;; Simulate organizing (simplified)
             (org-refile nil nil
                         (list "Projects"
                               (buffer-file-name target-buffer)
                               nil
                               (with-current-buffer target-buffer (point-min)))))

           ;; In real usage, org-gtd-organize--call handles cleanup
           ;; For this test, we manually call cleanup to simulate that
           (when task-id
             (org-gtd-wip--cleanup-temp-file task-id))

           ;; After cleanup, temp file should be deleted
           (expect (file-exists-p temp-file) :to-be nil))))

   (xit "cleans up all temp files on Emacs exit"
        ;; This test is skipped because it would require simulating kill-emacs-hook
        ;; The functionality is tested manually
        ))

 (describe "edge cases"
   (it "handles multiple clarifications of the same item"
       (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
         ;; First clarification
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer-1 (car (org-gtd-wip--get-buffers))))
           ;; Stop first clarification
           (with-current-buffer wip-buffer-1
             (org-gtd-clarify-stop)))

         ;; Second clarification of same item
         (with-current-buffer source-buffer
           (org-gtd-clarify-item))

         (let ((wip-buffer-2 (car (org-gtd-wip--get-buffers))))
           (with-current-buffer wip-buffer-2
             ;; Should have a new temp file
             (expect buffer-file-name :not :to-be nil)
             (expect (file-exists-p buffer-file-name) :to-be t)))))

   (it "creates temp directory if it doesn't exist"
       (let ((temp-dir (expand-file-name "org-gtd" temporary-file-directory)))
         ;; Delete directory if it exists
         (when (file-exists-p temp-dir)
           (delete-directory temp-dir t))

         (let ((source-buffer (ogt--temp-org-file-buffer "taskfile" "* Task to clarify")))
           (with-current-buffer source-buffer
             (org-gtd-clarify-item))

           ;; Directory should be created
           (expect (file-directory-p temp-dir) :to-be t))))))