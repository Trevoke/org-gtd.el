(require 'package)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; (use-package org)

;; (use-package org-edna)


(use-package org-gtd
               :quelpa (org-gtd :fetcher github-ssh :repo
               "trevoke/org-gtd.el" :branch "2.0.0"))

(show-paren-mode)

