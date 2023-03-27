;;; org-gtd-clarify.el --- Handle clarifying tasks -*- lexical-binding: t; coding: utf-8 -*-
;;
;; Copyright © 2019-2023 Aldric Giacomoni

;; Author: Aldric Giacomoni <trevoke@gmail.com>
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Set up emacs to helpfully clarify tasks so they can then be organized.
;;
;;; Code:

;;;###autoload
(defun org-gtd-clarify-item ()
  "Process item at point through org-gtd."
  (interactive)
  (let ((processing-buffer (org-gtd-wip--get-buffer))
        (window-config (current-window-configuration))
        (stuff-marker (point-marker)))
    (when (= (buffer-size processing-buffer) 0)
      (org-copy-subtree)
      (with-current-buffer processing-buffer
        (org-yank)))
    (display-buffer processing-buffer)
    (delete-other-windows (get-buffer-window processing-buffer))
    (setq-local org-gtd--window-config window-config
                org-gtd--stuff-marker stuff-marker
                org-gtd--wip-id (org-id-get)))

(provide 'org-gtd-clarify)
;;; org-gtd-clarify.el ends here
