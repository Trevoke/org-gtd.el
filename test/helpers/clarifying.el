(defun ogt-clarify-as-single-action ()
  (let ((inhibit-message t))
    (execute-kbd-macro (kbd "C-c c s"))))

(defun ogt-clarify-as-quick-action ()
  (let ((inhibit-message t))
    (execute-kbd-macro (kbd "C-c c q"))))

(defun ogt-clarify-as-project ()
  (let ((inhibit-message t))
    (execute-kbd-macro (kbd "C-c c p"))))

(defun ogt-clarify-as-incubated-item (&optional date)
  (let ((inhibit-message t))
    (let* ((date (or date (calendar-current-date)))
           (year (nth 2 date))
           (month (nth 0 date))
           (day (nth 1 date)))
      (execute-kbd-macro (kbd "C-c c i %s-%s-%s RET")))))

(defun ogt-clarify-as-delegated-item (&optional to-whom date)
  (let ((inhibit-message t))
    (let* ((person (or to-whom "Someone"))
           (date (or date (calendar-current-date)))
           (year (nth 2 date))
           (month (nth 0 date))
           (day (nth 1 date)))
      (execute-kbd-macro (kbd (format "C-c c d %s RET %s-%s-%s RET" to-whom year month day))))))

(defun ogt-clarify-as-calendar-item (&optional date)
  (let ((inhibit-message t))
    (let* ((date (or date (calendar-current-date)))
           (year (nth 2 date))
           (month (nth 0 date))
           (day (nth 1 date)))
      (execute-kbd-macro (kbd (format "C-c c c %s-%s-%s RET" year month day))))))

(defun ogt-clarify-as-habit (repeater)
  (let ((inhibit-message t))
    (execute-kbd-macro (kbd (format "C-c c h %s RET" repeater)))))

(defun ogt-clarify-as-knowledge-item ()
  (let ((inhibit-message t))
    (execute-kbd-macro (kbd "C-c c k"))))


(defun ogt-clarify-as-trash-item ()
  (let ((inhibit-message t))
    (execute-kbd-macro (kbd "C-c c t"))))
