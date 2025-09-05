;; Load guard to prevent redundant loading
(unless (featurep 'org-gtd-test-helper-clarifying)

(defun ogt-clarify-as-single-action ()
  (let ((inhibit-message t))
    (org-gtd-single-action)))

(defun ogt-clarify-as-quick-action ()
  (let ((inhibit-message t))
    (org-gtd-quick-action)))

(defun ogt-clarify-as-project ()
  (let ((inhibit-message t))
    (org-gtd-project-new)))

(defun ogt-clarify-as-incubated-item (&optional date)
  (let ((inhibit-message t)
        (reminder-date (when date
                         (let* ((year (nth 2 date))
                                (month (nth 0 date))
                                (day (nth 1 date)))
                           (format "%04d-%02d-%02d" year month day)))))
    (org-gtd-incubate reminder-date)))

(defun ogt-clarify-as-delegated-item (&optional to-whom date)
  (let ((inhibit-message t)
        (person (or to-whom "Someone"))
        (checkin-date (when date
                        (let* ((year (nth 2 date))
                               (month (nth 0 date))
                               (day (nth 1 date)))
                          (format "%04d-%02d-%02d" year month day)))))
    (org-gtd-delegate person checkin-date)))

(defun ogt-clarify-as-calendar-item (&optional date)
  (let ((inhibit-message t)
        (appointment-date (when date
                            (let* ((year (nth 2 date))
                                   (month (nth 0 date))
                                   (day (nth 1 date)))
                              (format "%04d-%02d-%02d" year month day)))))
    (org-gtd-calendar appointment-date)))

(defun ogt-clarify-as-habit (repeater)
  (let ((inhibit-message t))
    (org-gtd-habit repeater)))

(defun ogt-clarify-as-knowledge-item ()
  (let ((inhibit-message t))
    (org-gtd-knowledge)))

(defun ogt-clarify-as-trash-item ()
  (let ((inhibit-message t))
    (org-gtd-trash)))

;; End load guard and provide feature
(provide 'org-gtd-test-helper-clarifying))
